CLASS zcl_sde_pivot DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_val,
             key TYPE string, "ALIAS~FIELD
             agg TYPE string, "SUM/COUNT/MIN/MAX/AVG
           END OF t_val,
           tt_vals TYPE STANDARD TABLE OF t_val WITH DEFAULT KEY,
           tt_keys TYPE STANDARD TABLE OF string WITH DEFAULT KEY,

           "one column of the grouped statement; the same list drives the SQL
           "and the transpose in zcl_sde_tools=>execute_pivot
           BEGIN OF t_sqlcol,
             key  TYPE string,      "ALIAS~FIELD, empty for COUNT( * )
             agg  TYPE string,      "SUM/COUNT/... , empty for a dimension
             comp TYPE string,      "component name = alias in the statement
             role TYPE c LENGTH 1,  "R row dimension, C column dimension, M measure
           END OF t_sqlcol,
           tt_sqlcols TYPE STANDARD TABLE OF t_sqlcol WITH DEFAULT KEY.

    METHODS:
      has_layout RETURNING VALUE(rv_has) TYPE abap_bool,
      has_columns RETURNING VALUE(rv_has) TYPE abap_bool,
      get_col_key RETURNING VALUE(rv_key) TYPE string,
      get_col_keys RETURNING VALUE(rt_keys) TYPE tt_keys,
      get_sql_columns RETURNING VALUE(rt_cols) TYPE tt_sqlcols,

      "HTML for the builder: the pivot cross - rows down the left, columns across
      "the top, measures in the body - plus the available field chips below it.
      "i_header is the tool bar of the canvas the panel is rendered into.
      render_panel IMPORTING it_fields      TYPE zif_sde_pivot_types=>tt_jfld
                             i_show_texts   TYPE abap_bool DEFAULT abap_false
                             i_header       TYPE string OPTIONAL
                   RETURNING VALUE(rv_html) TYPE string,

      "pv actions: pk_<key> pick an available field, pz_<z><i> pick a placed one,
      "pt_<z><i> put the picked field into that slot, px_<z><i> clear the slot,
      "sa_<i>_<AGG> aggregate of measure i, CLR clear everything
      handle_action IMPORTING i_act     TYPE string
                              it_fields TYPE zif_sde_pivot_types=>tt_jfld OPTIONAL,

      "drag&drop, posted as <target zone><slot>|<source zone><slot>|<key>;
      "source zone a is the available list, r/c/v are the three sections
      handle_drop IMPORTING i_move    TYPE string
                            it_fields TYPE zif_sde_pivot_types=>tt_jfld OPTIONAL,

      normalize_aggs IMPORTING it_fields TYPE zif_sde_pivot_types=>tt_jfld,

      "the statement behind the pivot: one group per row/column combination.
      "The matrix itself is built in ABAP - a dynamically specified SELECT
      "list cannot carry the CASE expressions a SQL-side matrix would need.
      build_select IMPORTING i_from        TYPE string
                             i_where       TYPE string
                             i_multi       TYPE abap_bool
                             i_rows        TYPE i
                   RETURNING VALUE(rv_sql) TYPE string,

      "the same statement as dynamic tokens: upper case, one line, no literals
      build_tokens IMPORTING i_multi  TYPE abap_bool
                   EXPORTING e_fields TYPE string
                             e_group  TYPE string,

      "column value -> component name part: A-Z, 0-9, '_' only
      sanitize IMPORTING i_txt          TYPE string
               RETURNING VALUE(rv_name) TYPE string.

  PRIVATE SECTION.
    CONSTANTS: c_min_slots TYPE i      VALUE 3,   "3 x 3 sections; a full one grows by one empty slot
               c_from_list TYPE string VALUE `a0`. "source of a chip that comes from the field list

    DATA: mt_rows    TYPE tt_keys, "group-by fields in order
          mt_cols    TYPE tt_keys, "column fields in order
          mt_vals    TYPE tt_vals, "aggregated fields
          m_pick     TYPE string,  "field picked for zone assignment
          m_pick_src TYPE string.  "where it was picked up: the field list or <zone><slot>

    METHODS:
      "slots: read one, take the content out of one, put a field into another
      slot_key IMPORTING i_zone        TYPE string
                         i_idx         TYPE i
               RETURNING VALUE(rv_key) TYPE string,
      pick_src RETURNING VALUE(rv_src) TYPE string,
      take IMPORTING i_zone TYPE string
                     i_idx  TYPE i
           EXPORTING e_key  TYPE string
                     e_agg  TYPE string,
      put IMPORTING i_zone    TYPE string
                    i_idx     TYPE i
                    i_key     TYPE string
                    i_agg     TYPE string OPTIONAL
                    it_fields TYPE zif_sde_pivot_types=>tt_jfld OPTIONAL,

      "rendering helpers
      zone_html IMPORTING i_zone         TYPE string
                          it_keys        TYPE tt_keys
                          it_fields      TYPE zif_sde_pivot_types=>tt_jfld
                          i_show_texts   TYPE abap_bool
                          i_vertical     TYPE abap_bool DEFAULT abap_false
                RETURNING VALUE(rv_html) TYPE string,
      vals_html IMPORTING it_fields      TYPE zif_sde_pivot_types=>tt_jfld
                          i_show_texts   TYPE abap_bool
                RETURNING VALUE(rv_html) TYPE string,
      free_slots IMPORTING i_zone         TYPE string
                           i_filled       TYPE i
                           i_vertical     TYPE abap_bool DEFAULT abap_false
                 RETURNING VALUE(rv_html) TYPE string,
      label_of IMPORTING i_key           TYPE string
                         it_fields       TYPE zif_sde_pivot_types=>tt_jfld
                         i_show_texts    TYPE abap_bool
               RETURNING VALUE(rv_label) TYPE string,
      is_used IMPORTING i_key        TYPE string
              RETURNING VALUE(rv_ok) TYPE abap_bool,

      qualify IMPORTING i_key         TYPE string
                        i_multi       TYPE abap_bool
              RETURNING VALUE(rv_sql) TYPE string,
      comp_name IMPORTING i_key          TYPE string
                          i_prefix       TYPE string OPTIONAL
                RETURNING VALUE(rv_name) TYPE string,
      is_numeric_field IMPORTING i_key        TYPE string
                                 it_fields    TYPE zif_sde_pivot_types=>tt_jfld
                       RETURNING VALUE(rv_ok) TYPE abap_bool,
      allowed_aggs IMPORTING i_key         TYPE string
                              it_fields    TYPE zif_sde_pivot_types=>tt_jfld
                    RETURNING VALUE(rt_aggs) TYPE tt_keys,
      default_agg IMPORTING i_key         TYPE string
                              it_fields    TYPE zif_sde_pivot_types=>tt_jfld
                    RETURNING VALUE(rv_agg) TYPE string.
ENDCLASS.



CLASS zcl_sde_pivot IMPLEMENTATION.

  METHOD has_layout.
    rv_has = boolc( mt_rows IS NOT INITIAL OR mt_vals IS NOT INITIAL OR mt_cols IS NOT INITIAL ).
  ENDMETHOD.

  METHOD has_columns.
    rv_has = boolc( mt_cols IS NOT INITIAL ).
  ENDMETHOD.

  METHOD get_col_key.
    rv_key = COND #( WHEN mt_cols IS NOT INITIAL THEN mt_cols[ 1 ] ).
  ENDMETHOD.

  METHOD get_col_keys.
    rt_keys = mt_cols.
  ENDMETHOD.

  METHOD qualify.
    "T0~CARRID -> t0~carrid (join) or carrid (single table)
    SPLIT i_key AT '~' INTO DATA(l_alias) DATA(l_field).
    rv_sql = COND #( WHEN i_multi = abap_true
                     THEN |{ to_lower( l_alias ) }~{ to_lower( l_field ) }|
                     ELSE to_lower( l_field ) ).
  ENDMETHOD.

  METHOD comp_name.
    "result component: [PREFIX_]ALIAS_FIELD
    DATA(l_name) = i_key.
    REPLACE '~' IN l_name WITH '_'.
    rv_name = COND #( WHEN i_prefix IS INITIAL THEN l_name ELSE |{ i_prefix }_{ l_name }| ).
    IF strlen( rv_name ) > 30.
      rv_name = rv_name+0(30).
    ENDIF.
    rv_name = to_upper( rv_name ).
  ENDMETHOD.

  METHOD sanitize.
    "column value -> component name part: A-Z, 0-9, '_' only
    rv_name = to_upper( i_txt ).
    DATA(l_len) = strlen( rv_name ).
    DATA(l_off) = 0.
    DATA(l_clean) = ``.
    WHILE l_off < l_len.
      DATA(l_ch) = rv_name+l_off(1).
      IF l_ch CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.
        l_clean = l_clean && l_ch.
      ELSE.
        l_clean = l_clean && '_'.
      ENDIF.
      l_off = l_off + 1.
    ENDWHILE.
    rv_name = l_clean.
    IF rv_name IS INITIAL OR rv_name CO '_'.
      rv_name = 'BLANK'.
    ENDIF.
    IF strlen( rv_name ) > 12.
      rv_name = rv_name+0(12).
    ENDIF.
  ENDMETHOD.

  METHOD is_numeric_field.
    SPLIT i_key AT '~' INTO DATA(l_alias) DATA(l_field).
    DATA(l_alias_up) = to_upper( l_alias ).
    DATA(l_field_up) = to_upper( l_field ).
    READ TABLE it_fields INTO DATA(ls_field) WITH KEY alias = l_alias_up fieldname = l_field_up.
    IF sy-subrc = 0.
      rv_ok = boolc( ls_field-inttype CA 'b8sIPFaSe' ).
    ENDIF.
  ENDMETHOD.

  METHOD allowed_aggs.
    rt_aggs = VALUE #( ( `COUNT` ) ).
    IF is_numeric_field( i_key = i_key it_fields = it_fields ) = abap_true.
      rt_aggs = VALUE #( ( `SUM` ) ( `COUNT` ) ( `MIN` ) ( `MAX` ) ( `AVG` ) ).
    ELSE.
      SPLIT i_key AT '~' INTO DATA(l_alias) DATA(l_field).
      DATA(l_alias_up) = to_upper( l_alias ).
      DATA(l_field_up) = to_upper( l_field ).
      READ TABLE it_fields INTO DATA(ls_field) WITH KEY alias = l_alias_up fieldname = l_field_up.
      IF sy-subrc = 0.
        CASE ls_field-datatype.
          WHEN 'CHAR' OR 'NUMC' OR 'DATS' OR 'TIMS' OR 'CLNT' OR 'CUKY' OR 'UNIT' OR 'LANG'.
            rt_aggs = VALUE #( ( `COUNT` ) ( `MIN` ) ( `MAX` ) ).
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD default_agg.
    rv_agg = COND #( WHEN is_numeric_field( i_key = i_key it_fields = it_fields ) = abap_true
                     THEN `SUM`
                     ELSE `COUNT` ).
  ENDMETHOD.

  METHOD label_of.
    "placed chip: the qualified key, or the field text when texts are switched on
    SPLIT i_key AT '~' INTO DATA(l_alias) DATA(l_field).
    DATA(l_alias_up) = to_upper( l_alias ).
    DATA(l_field_up) = to_upper( l_field ).
    READ TABLE it_fields INTO DATA(ls_fld) WITH KEY alias = l_alias_up fieldname = l_field_up.
    IF sy-subrc = 0 AND i_show_texts = abap_true AND ls_fld-ddtext IS NOT INITIAL.
      rv_label = escape( val = CONV string( ls_fld-ddtext ) format = cl_abap_format=>e_html_text ).
    ELSE.
      rv_label = to_lower( i_key ).
    ENDIF.
  ENDMETHOD.

  METHOD is_used.
    rv_ok = boolc( line_exists( mt_rows[ table_line = i_key ] )
                OR line_exists( mt_cols[ table_line = i_key ] )
                OR line_exists( mt_vals[ key = i_key ] ) ).
  ENDMETHOD.

  METHOD slot_key.
    "what sits in a slot, without touching it
    CHECK i_idx >= 1.
    CASE i_zone.
      WHEN `r`.
        CHECK i_idx <= lines( mt_rows ).
        rv_key = mt_rows[ i_idx ].
      WHEN `c`.
        CHECK i_idx <= lines( mt_cols ).
        rv_key = mt_cols[ i_idx ].
      WHEN `v`.
        CHECK i_idx <= lines( mt_vals ).
        rv_key = mt_vals[ i_idx ]-key.
    ENDCASE.
  ENDMETHOD.

  METHOD pick_src.
    "where the picked chip came from, in the notation of the drop protocol
    rv_src = COND #( WHEN m_pick_src IS INITIAL THEN c_from_list ELSE m_pick_src ).
  ENDMETHOD.

  METHOD take.
    "empty one slot and hand its content back (drag between sections)
    CLEAR: e_key, e_agg.
    CHECK i_idx >= 1.
    CASE i_zone.
      WHEN `r`.
        CHECK i_idx <= lines( mt_rows ).
        e_key = mt_rows[ i_idx ].
        DELETE mt_rows INDEX i_idx.
      WHEN `c`.
        CHECK i_idx <= lines( mt_cols ).
        e_key = mt_cols[ i_idx ].
        DELETE mt_cols INDEX i_idx.
      WHEN `v`.
        CHECK i_idx <= lines( mt_vals ).
        e_key = mt_vals[ i_idx ]-key.
        e_agg = mt_vals[ i_idx ]-agg.
        DELETE mt_vals INDEX i_idx.
    ENDCASE.
  ENDMETHOD.

  METHOD put.
    "insert before slot i_idx; a drop beyond the last filled slot appends
    FIELD-SYMBOLS <keys> TYPE tt_keys.

    CHECK i_key IS NOT INITIAL.
    DATA(l_idx) = i_idx.

    IF i_zone = `v`. "measures may hold the same field twice - one slot per aggregate
      DATA(ls_val) = VALUE t_val( key = i_key
                                  agg = COND #( WHEN i_agg IS NOT INITIAL
                                                THEN i_agg
                                                ELSE default_agg( i_key = i_key it_fields = it_fields ) ) ).
      "the same aggregate of the same field twice would collide in the result structure
      READ TABLE mt_vals TRANSPORTING NO FIELDS WITH KEY key = ls_val-key agg = ls_val-agg.
      IF sy-subrc = 0.
        DATA(l_dup) = sy-tabix.
        DELETE mt_vals INDEX l_dup.
        IF l_dup < l_idx.
          l_idx = l_idx - 1.
        ENDIF.
      ENDIF.
      IF l_idx >= 1 AND l_idx <= lines( mt_vals ).
        INSERT ls_val INTO mt_vals INDEX l_idx.
      ELSE.
        APPEND ls_val TO mt_vals.
      ENDIF.
      RETURN.
    ENDIF.

    CASE i_zone.
      WHEN `r`.
        ASSIGN mt_rows TO <keys>.
        DELETE mt_cols WHERE table_line = i_key. "a field is a row or a column, never both
      WHEN `c`.
        ASSIGN mt_cols TO <keys>.
        DELETE mt_rows WHERE table_line = i_key.
      WHEN OTHERS. RETURN.
    ENDCASE.

    "a dimension appears once per section: a second drop moves it, it does not double it
    READ TABLE <keys> TRANSPORTING NO FIELDS WITH KEY table_line = i_key.
    IF sy-subrc = 0.
      DATA(l_old) = sy-tabix.
      DELETE <keys> INDEX l_old.
      IF l_old < l_idx.
        l_idx = l_idx - 1.
      ENDIF.
    ENDIF.

    IF l_idx >= 1 AND l_idx <= lines( <keys> ).
      INSERT i_key INTO <keys> INDEX l_idx.
    ELSE.
      APPEND i_key TO <keys>.
    ENDIF.
  ENDMETHOD.

  METHOD handle_drop.
    "<target zone><slot>|<source zone><slot>|<key>
    SPLIT i_move AT '|' INTO DATA(l_tgt) DATA(l_src) DATA(l_key).
    CHECK strlen( l_tgt ) >= 2 AND l_key IS NOT INITIAL.

    DATA(l_tzone) = CONV string( l_tgt(1) ).
    DATA(l_tidx)  = CONV i( l_tgt+1 ).
    DATA(l_agg)   = ``.

    IF strlen( l_src ) >= 2 AND l_src(1) NE 'a'. "a placed chip is moved, not a new one
      DATA(l_szone) = CONV string( l_src(1) ).
      DATA(l_sidx)  = CONV i( l_src+1 ).
      IF l_szone = l_tzone AND l_sidx = l_tidx. "dropped where it already was
        CLEAR: m_pick, m_pick_src.
        RETURN.
      ENDIF.
      take( EXPORTING i_zone = l_szone i_idx = l_sidx
            IMPORTING e_key  = DATA(l_taken) e_agg = l_agg ).
      IF l_taken IS NOT INITIAL.
        l_key = l_taken. "the slot content wins over the key sent by the browser
      ENDIF.
      IF l_szone = l_tzone AND l_sidx < l_tidx. "the slots behind it moved up
        l_tidx = l_tidx - 1.
      ENDIF.
    ENDIF.

    put( i_zone = l_tzone i_idx = l_tidx i_key = l_key i_agg = l_agg it_fields = it_fields ).
    CLEAR: m_pick, m_pick_src.
  ENDMETHOD.

  METHOD handle_action.
    IF i_act = 'CLR'.
      CLEAR: mt_rows, mt_cols, mt_vals, m_pick, m_pick_src.
      RETURN.
    ENDIF.

    CHECK strlen( i_act ) > 3.
    DATA(l_rest) = substring( val = i_act off = 3 len = strlen( i_act ) - 3 ).

    CASE i_act(3).
      WHEN 'pk_'. "pick a field from the list (clicking it again cancels)
        IF m_pick = l_rest AND m_pick_src = c_from_list.
          CLEAR: m_pick, m_pick_src.
        ELSE.
          m_pick     = l_rest.
          m_pick_src = c_from_list.
        ENDIF.

      WHEN 'pz_'. "a chip in a slot: insert the picked field before it, or pick it up
        CHECK strlen( l_rest ) >= 2.
        IF m_pick IS NOT INITIAL AND m_pick_src NE l_rest.
          handle_drop( i_move    = |{ l_rest }\|{ pick_src( ) }\|{ m_pick }|
                       it_fields = it_fields ).
          RETURN.
        ENDIF.
        DATA(l_zkey) = slot_key( i_zone = CONV string( l_rest(1) ) i_idx = CONV i( l_rest+1 ) ).
        CHECK l_zkey IS NOT INITIAL.
        IF m_pick = l_zkey AND m_pick_src = l_rest. "clicking it again cancels
          CLEAR: m_pick, m_pick_src.
        ELSE.
          m_pick     = l_zkey.
          m_pick_src = l_rest.
        ENDIF.

      WHEN 'pt_'. "put the picked field into slot <zone><slot>
        CHECK strlen( l_rest ) >= 2 AND m_pick IS NOT INITIAL.
        handle_drop( i_move    = |{ l_rest }\|{ pick_src( ) }\|{ m_pick }|
                     it_fields = it_fields ).

      WHEN 'px_'. "clear slot <zone><slot>
        CHECK strlen( l_rest ) >= 2.
        take( EXPORTING i_zone = CONV string( l_rest(1) ) i_idx = CONV i( l_rest+1 ) ).
        CLEAR: m_pick, m_pick_src.

      WHEN 'sa_'. "sa_<idx>_<AGG>: aggregate of a measure
        SPLIT l_rest AT '_' INTO DATA(l_sidx) DATA(l_sagg).
        READ TABLE mt_vals ASSIGNING FIELD-SYMBOL(<val>) INDEX CONV i( l_sidx ).
        IF sy-subrc = 0 AND l_sagg IS NOT INITIAL.
          <val>-agg = l_sagg.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD normalize_aggs.
    LOOP AT mt_vals ASSIGNING FIELD-SYMBOL(<val>).
      DATA(lt_allowed) = allowed_aggs( i_key = <val>-key it_fields = it_fields ).
      IF NOT line_exists( lt_allowed[ table_line = <val>-agg ] ).
        <val>-agg = default_agg( i_key = <val>-key it_fields = it_fields ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD free_slots.
    "always one empty slot more than the section holds, never less than three
    DATA(l_slots) = nmax( val1 = c_min_slots val2 = i_filled + 1 ).
    DATA(l_cls) = COND string( WHEN i_vertical = abap_true THEN `slot vslot` ELSE `slot hslot` ).
    DATA(l_free) = l_slots - i_filled.

    "with a field picked every free slot is a target: mark them, the corner says which field
    DATA(l_txt) = COND string( WHEN m_pick IS NOT INITIAL THEN `+` ELSE `&#183;` ).
    IF m_pick IS NOT INITIAL.
      l_cls = |{ l_cls } ready|.
    ENDIF.

    DO l_free TIMES.
      DATA(l_idx) = i_filled + sy-index.
      rv_html = rv_html &&
        |<a class="{ l_cls }" href="SAPEVENT:pv?pt_{ i_zone }{ l_idx }"| &&
        | onmouseover="zo(event,this,'{ i_zone }{ l_idx }')" onclick="return cl(event)">| &&
        |{ l_txt }</a>|.
    ENDDO.
  ENDMETHOD.

  METHOD zone_html.
    "rows and columns: one chip per slot - drag it to another slot, cross removes it
    DATA(l_cls) = COND string( WHEN i_vertical = abap_true THEN `slot vslot fill` ELSE `slot hslot fill` ).

    LOOP AT it_keys INTO DATA(l_key).
      DATA(l_slot) = |{ i_zone }{ sy-tabix }|.
      DATA(l_pick) = COND string( WHEN m_pick = l_key AND m_pick_src = l_slot THEN ` pick` ).
      rv_html = rv_html &&
        |<span class="{ l_cls }{ l_pick }" onmouseover="zo(event,this,'{ l_slot }')">| &&
        |<a class="lbl" href="SAPEVENT:pv?pz_{ l_slot }"| &&
        | draggable="false" ondragstart="return false"| &&
        | onmousedown="return md(event,'{ l_key }','{ l_slot }')" onclick="return cl(event)"| &&
        | title="{ l_key } - drag into another slot">| &&
        |{ label_of( i_key = l_key it_fields = it_fields i_show_texts = i_show_texts ) }</a>| &&
        | <a class="rmx" href="SAPEVENT:pv?px_{ l_slot }" title="remove">&#10005;</a></span>|.
    ENDLOOP.

    rv_html = rv_html && free_slots( i_zone = i_zone i_filled = lines( it_keys ) i_vertical = i_vertical ).
  ENDMETHOD.

  METHOD vals_html.
    "measures: the aggregate is a listbox on the chip itself
    LOOP AT mt_vals INTO DATA(ls_val).
      DATA(l_idx) = sy-tabix.
      DATA(l_slot) = |v{ l_idx }|.
      DATA(l_pick) = COND string( WHEN m_pick = ls_val-key AND m_pick_src = l_slot THEN ` pick` ).
      DATA(lt_aggs) = allowed_aggs( i_key = ls_val-key it_fields = it_fields ).
      rv_html = rv_html &&
        |<span class="slot hslot fill vfill{ l_pick }" onmouseover="zo(event,this,'{ l_slot }')">| &&
        |<form style="display:inline" method="post" action="SAPEVENT:pvagg">| &&
        |<input type="hidden" name="idx" value="{ l_idx }">| &&
        |<select name="agg" onchange="this.form.submit()">|.
      LOOP AT lt_aggs INTO DATA(l_agg).
        rv_html = rv_html &&
          |<option{ COND string( WHEN ls_val-agg = l_agg THEN ' selected' ) }>{ l_agg }</option>|.
      ENDLOOP.
      rv_html = rv_html &&
        |</select> <a class="lbl" href="SAPEVENT:pv?pz_{ l_slot }"| &&
        | draggable="false" ondragstart="return false"| &&
        | onmousedown="return md(event,'{ ls_val-key }','{ l_slot }')" onclick="return cl(event)"| &&
        | title="{ ls_val-key } - drag into another slot">| &&
        |{ label_of( i_key = ls_val-key it_fields = it_fields i_show_texts = i_show_texts ) }</a>| &&
        | <a class="rmx" href="SAPEVENT:pv?px_{ l_slot }" title="remove">&#10005;</a></form></span>|.
    ENDLOOP.

    rv_html = rv_html && free_slots( i_zone = `v` i_filled = lines( mt_vals ) ).
  ENDMETHOD.

  METHOD render_panel.
    rv_html =
      `<html><head><meta charset="utf-8"><style>` &&
      `body{font-family:Arial,sans-serif;font-size:11px;margin:4px;background:#f6f8fa;}` &&
      "tool bar of the canvas, same look as in join mode
      `.base{display:inline-block;background:#2c5f8a;color:#fff;border-radius:6px;padding:6px 12px;margin:3px;font-weight:bold;}` &&
      `.card{display:inline-block;border:1px solid #aaa;border-radius:6px;padding:5px 10px;margin:3px;background:#fff;text-decoration:none;color:#000;}` &&
      `.card:hover{border-color:#2c5f8a;}` &&
      `.sel{background:#d3f2d3;border:2px solid #2e8b2e;font-weight:bold;}` &&
      "the cross: corner + columns on top, rows + measures below
      `table.pv{border-collapse:separate;border-spacing:3px;margin-top:2px;}` &&
      `table.pv td{vertical-align:top;padding:0;}` &&
      `td.side{width:210px;}` &&
      `.hdr{font-size:10px;font-weight:bold;color:#2c5f8a;margin:0 0 2px 3px;}` &&
      `.hdr i{font-weight:normal;font-style:normal;color:#999;}` &&
      `.zone{background:#fff;border:1px solid #dfe3e8;border-radius:5px;padding:2px;}` &&
      `.slot{border:1px dashed #c3c8ce;border-radius:4px;padding:2px 6px;margin:2px;` &&
      `background:#fbfcfd;color:#c0c4c8;text-decoration:none;white-space:nowrap;}` &&
      `.vslot{display:block;text-align:center;}` &&
      `.ready{color:#d2691e;border-color:#d2691e;font-weight:bold;}` &&
      `.hslot{display:inline-block;min-width:104px;text-align:center;}` &&
      `.fill{border-style:solid;border-color:#2e8b2e;background:#eef8ee;color:#000;text-align:left;}` &&
      `.vfill{border-color:#7a3db8;background:#f7f0ff;}` &&
      `.lbl{font-family:Consolas,monospace;text-decoration:none;color:#000;}` &&
      `.rmx{color:#a00;text-decoration:none;font-size:10px;}` &&
      `.pick{outline:2px dashed #d2691e;}` &&
      `.chip{display:inline-block;border:1px solid #bbb;border-radius:10px;padding:1px 8px;margin:2px;` &&
      `background:#fff;text-decoration:none;color:#000;}` &&
      `.chip:hover{border-color:#2c5f8a;}` &&
      `.used{color:#9aa0a6;border-style:dotted;}` &&
      `.c1{background:#d8ecff;border-color:#5797c9;}` &&
      `.c2{background:#dcf5d6;border-color:#67a95e;}` &&
      `.c3{background:#fff1c2;border-color:#c09a2b;}` &&
      `.c4{background:#f3ddff;border-color:#a678c2;}` &&
      `.c5{background:#ffdede;border-color:#c87373;}` &&
      `.c6{background:#d9f4ef;border-color:#58a99b;}` &&
      `.key{border-style:double;border-width:3px;}` &&
      `.avail{background:#fff;border:1px solid #dfe3e8;border-radius:5px;padding:3px;margin:3px;}` &&
      `.act{color:#2c5f8a;text-decoration:none;margin-right:6px;}` &&
      `.dir{color:#888;font-size:9px;}` &&
      `.hint{color:#d2691e;font-weight:bold;font-family:Consolas,monospace;}` &&
      `select{font-size:10px;}` &&
      `</style>` &&
      "drag a chip onto a slot: mouse events + a hidden form, as everywhere else
      `<script>var mk=null,ms=null,mt=null,mz=null,mv=false;` &&
      `function md(e,k,s){mk=k;ms=s;mv=false;return true;}` &&
      `function zo(e,el,t){if(!mk)return;mv=true;` &&
      `if(mt&&mt!==el){mt.style.boxShadow='';}mt=el;mz=t;` &&
      `el.style.boxShadow='0 0 0 2px #d2691e';}` &&
      `function cl(e){if(mv){mv=false;if(e.preventDefault)e.preventDefault();return false;}return true;}` &&
      `document.onmouseup=function(){if(!mk)return;` &&
      `var k=mk,s=ms,t=mz,ok=mv;mk=null;ms=null;mz=null;` &&
      `if(mt){mt.style.boxShadow='';mt=null;}` &&
      `if(ok&&t){document.getElementById('pmk').value=t+'|'+s+'|'+k;` &&
      `document.getElementById('pmf').submit();}};` &&
      `</script>` &&
      `</head><body onselectstart="return false">` &&
      `<form id="pmf" method="post" action="SAPEVENT:pvdrop" style="display:none">` &&
      `<input type="hidden" name="mv" id="pmk"></form>` &&
      i_header.

    "corner of the cross: title, clear, and what to do next
    DATA(l_corner) =
      `<div class="hdr">PIVOT&nbsp;&nbsp;<a class="act" href="SAPEVENT:pv?CLR">clear</a></div>`.
    IF m_pick IS NOT INITIAL.
      l_corner = l_corner &&
        |<div class="hint">{ to_lower( m_pick ) }</div>| &&
        `<div class="dir">click a slot to drop it there (click the chip again to cancel)</div>`.
    ELSE.
      l_corner = l_corner &&
        `<div class="dir">drag a field from the list below into a slot - or click the field,` &&
        ` then click the slot.<br>A section that fills up grows by one more slot.</div>`.
    ENDIF.

    rv_html = rv_html &&
      `<table class="pv"><tr>` &&
      |<td class="side">{ l_corner }</td>| &&
      `<td><div class="hdr">COLUMNS <i>- field values become result columns (max 50 combinations)</i></div>` &&
      `<div class="zone">` &&
      zone_html( i_zone = `c` it_keys = mt_cols it_fields = it_fields i_show_texts = i_show_texts ) &&
      `</div></td></tr><tr>` &&
      `<td class="side"><div class="hdr">ROWS <i>- grouped, one line each</i></div><div class="zone">` &&
      zone_html( i_zone = `r` it_keys = mt_rows it_fields = it_fields
                 i_show_texts = i_show_texts i_vertical = abap_true ) &&
      `</div></td>` &&
      `<td><div class="hdr">MEASURES <i>- aggregated per row and column</i></div><div class="zone">` &&
      vals_html( it_fields = it_fields i_show_texts = i_show_texts ) &&
      `</div></td></tr></table>`.

    "available fields: colored per table when the pivot runs on a join
    DATA lt_alias TYPE tt_keys.
    LOOP AT it_fields INTO DATA(ls_fld).
      DATA(l_alias) = condense( CONV string( ls_fld-alias ) ).
      IF NOT line_exists( lt_alias[ table_line = l_alias ] ).
        APPEND l_alias TO lt_alias.
      ENDIF.
    ENDLOOP.

    rv_html = rv_html && `<div class="avail">`.
    IF it_fields IS INITIAL.
      rv_html = rv_html && `<span class="dir">no fields selected - pick them in the join tool</span>`.
    ENDIF.
    LOOP AT it_fields INTO ls_fld.
      l_alias = condense( CONV string( ls_fld-alias ) ).
      DATA(l_key) = |{ l_alias }~{ ls_fld-fieldname }|.
      DATA(l_cls) = `chip`.
      IF lines( lt_alias ) > 1. "one color per table, as in the join panel
        READ TABLE lt_alias TRANSPORTING NO FIELDS WITH KEY table_line = l_alias.
        DATA(l_color) = ( sy-tabix - 1 ) MOD 6 + 1.
        l_cls = |{ l_cls } c{ l_color }|.
      ENDIF.
      IF ls_fld-keyflag = abap_true.
        l_cls = |{ l_cls } key|.
      ENDIF.
      IF is_used( l_key ) = abap_true. "already in a section
        l_cls = |{ l_cls } used|.
      ENDIF.
      IF m_pick = l_key AND m_pick_src = c_from_list.
        l_cls = |{ l_cls } pick|.
      ENDIF.
      DATA(l_label) = COND string(
        WHEN i_show_texts = abap_true AND ls_fld-ddtext IS NOT INITIAL
        THEN escape( val = CONV string( ls_fld-ddtext ) format = cl_abap_format=>e_html_text )
        ELSE |{ ls_fld-fieldname }| ).
      rv_html = rv_html &&
        |<a class="{ l_cls }" href="SAPEVENT:pv?pk_{ l_key }"| &&
        | draggable="false" ondragstart="return false"| &&
        | onmousedown="return md(event,'{ l_key }','{ c_from_list }')" onclick="return cl(event)"| &&
        | title="{ l_key } { escape( val = CONV string( ls_fld-ddtext ) format = cl_abap_format=>e_html_attr ) }">| &&
        |{ l_label }</a>|.
    ENDLOOP.

    rv_html = rv_html && `</div></body></html>`.
  ENDMETHOD.

  METHOD get_sql_columns.
    "the grouped statement column by column: dimensions first, measures last.
    "execute_pivot builds the result structure and the matrix from the same list
    LOOP AT mt_rows INTO DATA(l_key).
      APPEND VALUE #( key = l_key role = 'R' comp = comp_name( l_key ) ) TO rt_cols.
    ENDLOOP.
    LOOP AT mt_cols INTO l_key.
      APPEND VALUE #( key = l_key role = 'C' comp = comp_name( l_key ) ) TO rt_cols.
    ENDLOOP.
    LOOP AT mt_vals INTO DATA(ls_val).
      APPEND VALUE #( key  = ls_val-key
                      agg  = ls_val-agg
                      role = 'M'
                      comp = comp_name( i_key = ls_val-key i_prefix = ls_val-agg ) ) TO rt_cols.
    ENDLOOP.
    IF mt_vals IS INITIAL AND rt_cols IS NOT INITIAL. "no measure: count the rows per group
      APPEND VALUE #( agg = `COUNT` role = 'M' comp = `CNT_ROWS` ) TO rt_cols.
    ENDIF.

    "the component names are the aliases of the statement as well: keep them unique
    DATA lt_taken TYPE tt_keys.
    LOOP AT rt_cols ASSIGNING FIELD-SYMBOL(<col>).
      DATA(l_base) = <col>-comp.
      WHILE line_exists( lt_taken[ table_line = <col>-comp ] ).
        <col>-comp = |{ COND string( WHEN strlen( l_base ) > 28 THEN l_base+0(28) ELSE l_base ) }_{ sy-index }|.
      ENDWHILE.
      APPEND <col>-comp TO lt_taken.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_tokens.
    "same columns as build_select, but as the dynamic tokens of the SELECT
    CLEAR: e_fields, e_group.
    LOOP AT get_sql_columns( ) INTO DATA(ls_col).
      DATA(l_qual) = COND string( WHEN ls_col-key IS INITIAL
                                  THEN `*`
                                  ELSE to_upper( qualify( i_key = ls_col-key i_multi = i_multi ) ) ).
      IF e_fields IS NOT INITIAL.
        e_fields = |{ e_fields }, |.
      ENDIF.
      IF ls_col-role = 'M'.
        e_fields = |{ e_fields }{ ls_col-agg }( { l_qual } ) AS { ls_col-comp }|.
      ELSE.
        e_fields = |{ e_fields }{ l_qual } AS { ls_col-comp }|.
        IF e_group IS NOT INITIAL.
          e_group = |{ e_group }, |.
        ENDIF.
        e_group = |{ e_group }{ l_qual }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_select.
    DATA: l_fields TYPE string,
          l_group  TYPE string.
    DATA(l_nl) = cl_abap_char_utilities=>newline.

    CHECK has_layout( ) = abap_true.

    LOOP AT get_sql_columns( ) INTO DATA(ls_col).
      DATA(l_qual) = COND string( WHEN ls_col-key IS INITIAL
                                  THEN `*`
                                  ELSE qualify( i_key = ls_col-key i_multi = i_multi ) ).
      IF l_fields IS NOT INITIAL.
        l_fields = |{ l_fields },{ l_nl }       |.
      ENDIF.
      IF ls_col-role = 'M'.
        l_fields = |{ l_fields }{ ls_col-agg }( { l_qual } ) AS { to_lower( ls_col-comp ) }|.
      ELSE.
        l_fields = |{ l_fields }{ l_qual } AS { to_lower( ls_col-comp ) }|.
        IF l_group IS NOT INITIAL.
          l_group = |{ l_group }, |.
        ENDIF.
        l_group = |{ l_group }{ l_qual }|.
      ENDIF.
    ENDLOOP.

    rv_sql = |SELECT { l_fields }{ l_nl }  FROM { i_from }|.
    IF i_where IS NOT INITIAL.
      rv_sql = |{ rv_sql }{ l_nl } WHERE { i_where }|.
    ENDIF.
    IF l_group IS NOT INITIAL.
      rv_sql = |{ rv_sql }{ l_nl } GROUP BY { l_group }|.
    ENDIF.
    rv_sql = |{ rv_sql }{ l_nl } UP TO { i_rows } ROWS|.
  ENDMETHOD.

ENDCLASS.
