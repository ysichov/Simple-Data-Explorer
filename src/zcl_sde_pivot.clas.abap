CLASS zcl_sde_pivot DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_val,
             key TYPE string, "ALIAS~FIELD
             agg TYPE string, "SUM/COUNT/MIN/MAX/AVG
           END OF t_val,
           tt_vals TYPE STANDARD TABLE OF t_val WITH DEFAULT KEY,
           tt_keys TYPE STANDARD TABLE OF string WITH DEFAULT KEY,

           BEGIN OF t_colval,
             text    TYPE string, "display value
             literal TYPE string, "SQL literal ('USD' or 42), single-column compatibility
             cond    TYPE string, "SQL condition for multi-column buckets
           END OF t_colval,
           tt_colvals TYPE STANDARD TABLE OF t_colval WITH DEFAULT KEY.

    METHODS:
      has_layout RETURNING VALUE(rv_has) TYPE abap_bool,
      has_columns RETURNING VALUE(rv_has) TYPE abap_bool,
      get_col_key RETURNING VALUE(rv_key) TYPE string,
      get_col_keys RETURNING VALUE(rt_keys) TYPE tt_keys,

      "HTML for the fields panel: zones + available field chips (SAPEVENT:pv?...)
      render_panel IMPORTING it_fields      TYPE zcl_sde_tools=>tt_jfld
                             i_show_texts   TYPE abap_bool DEFAULT abap_false
                   RETURNING VALUE(rv_html) TYPE string,

      "pv actions: pk_<key> pick, tr rows, tc columns, ag_<AGG> values,
      "rr_<key>/rc_<key>/rv_<idx> remove, CLR clear all
      handle_action IMPORTING i_act TYPE string,

      "the whole pivot as ONE statement: CASE buckets per column value
      build_select IMPORTING i_from        TYPE string
                             i_where       TYPE string
                             i_multi       TYPE abap_bool
                             i_rows        TYPE i
                             it_col_vals   TYPE tt_colvals OPTIONAL
                   RETURNING VALUE(rv_sql) TYPE string.

  PRIVATE SECTION.
    DATA: mt_rows TYPE tt_keys, "group-by fields in order
          mt_cols TYPE tt_keys, "column fields in order
          mt_vals TYPE tt_vals, "aggregated fields
          m_pick  TYPE string.  "field picked for zone assignment

    METHODS:
      qualify IMPORTING i_key         TYPE string
                        i_multi       TYPE abap_bool
              RETURNING VALUE(rv_sql) TYPE string,
      comp_name IMPORTING i_key          TYPE string
                          i_prefix       TYPE string OPTIONAL
                RETURNING VALUE(rv_name) TYPE string,
      sanitize IMPORTING i_txt          TYPE string
               RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.



CLASS zcl_sde_pivot IMPLEMENTATION.

  METHOD has_layout.
    rv_has = boolc( mt_rows IS NOT INITIAL OR mt_vals IS NOT INITIAL OR mt_cols IS NOT INITIAL ).
  ENDMETHOD.

  METHOD has_columns.
    rv_has = boolc( mt_cols IS NOT INITIAL AND mt_vals IS NOT INITIAL ).
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

  METHOD handle_action.
    CASE i_act.
      WHEN 'CLR'.
        CLEAR: mt_rows, mt_cols, mt_vals, m_pick.
      WHEN 'tr'. "picked field -> rows
        IF m_pick IS NOT INITIAL AND NOT line_exists( mt_rows[ table_line = m_pick ] ).
          APPEND m_pick TO mt_rows.
        ENDIF.
        CLEAR m_pick.
      WHEN 'tc'. "picked field -> columns
        IF m_pick IS NOT INITIAL AND NOT line_exists( mt_cols[ table_line = m_pick ] ).
          APPEND m_pick TO mt_cols.
        ENDIF.
        CLEAR m_pick.
      WHEN OTHERS.
        "drag&drop: dr_/dc_/dv_<key> put the field straight into a zone
        IF strlen( i_act ) > 3 AND i_act(3) = 'dr_'.
          DATA(l_dkey) = substring( val = i_act off = 3 len = strlen( i_act ) - 3 ).
          IF NOT line_exists( mt_rows[ table_line = l_dkey ] ).
            APPEND l_dkey TO mt_rows.
          ENDIF.
          CLEAR m_pick.
          RETURN.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'dc_'.
          DATA(l_ckey) = substring( val = i_act off = 3 len = strlen( i_act ) - 3 ).
          IF NOT line_exists( mt_cols[ table_line = l_ckey ] ).
            APPEND l_ckey TO mt_cols.
          ENDIF.
          CLEAR m_pick.
          RETURN.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'dv_'.
          APPEND VALUE #( key = substring( val = i_act off = 3 len = strlen( i_act ) - 3 )
                          agg = 'SUM' ) TO mt_vals.
          CLEAR m_pick.
          RETURN.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'sa_'. "sa_<idx>_<AGG>: change aggregate
          SPLIT i_act+3 AT '_' INTO DATA(l_sidx) DATA(l_sagg).
          READ TABLE mt_vals ASSIGNING FIELD-SYMBOL(<val>) INDEX CONV i( l_sidx ).
          IF sy-subrc = 0 AND l_sagg IS NOT INITIAL.
            <val>-agg = l_sagg.
          ENDIF.
          RETURN.
        ENDIF.
        IF strlen( i_act ) > 3 AND i_act(3) = 'pk_'. "pick a field (or unpick)
          DATA(l_key) = substring( val = i_act off = 3 len = strlen( i_act ) - 3 ).
          m_pick = COND #( WHEN m_pick = l_key THEN `` ELSE l_key ).
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'ag_'. "picked field -> values with aggregate
          IF m_pick IS NOT INITIAL.
            APPEND VALUE #( key = m_pick agg = i_act+3 ) TO mt_vals.
          ENDIF.
          CLEAR m_pick.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'rr_'. "remove from rows
          DELETE mt_rows WHERE table_line = i_act+3.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'rc_'. "remove from columns
          DELETE mt_cols WHERE table_line = i_act+3.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'rv_'. "remove from values by index
          DATA(l_idx) = CONV i( i_act+3 ).
          DELETE mt_vals INDEX l_idx.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD render_panel.
    DATA(lt_aggs) = VALUE tt_keys( ( `SUM` ) ( `COUNT` ) ( `MIN` ) ( `MAX` ) ( `AVG` ) ).
    rv_html =
      `<html><head><meta charset="utf-8"><style>` &&
      `body{font-family:Arial,sans-serif;font-size:11px;margin:4px;background:#fff;}` &&
      `h4{margin:6px 0 2px 0;font-size:11px;color:#2c5f8a;}` &&
      `.chip{display:inline-block;border:1px solid #bbb;border-radius:10px;padding:1px 8px;margin:2px;text-decoration:none;color:#000;}` &&
      `.chip:hover{border-color:#2c5f8a;}` &&
      `.zone{background:#f6f8fa;border:1px dashed #bbb;border-radius:4px;padding:3px;margin:2px 0;}` &&
      `.zchip{display:inline-block;border:1px solid #2e8b2e;background:#eef8ee;border-radius:4px;` &&
      `padding:1px 6px;margin:2px;font-family:Consolas,monospace;text-decoration:none;color:#000;}` &&
      `.agg{color:#7a3db8;font-weight:bold;}` &&
      `.add{border-style:dashed;background:#fff8e6;border-color:#d2691e;color:#d2691e;font-weight:bold;}` &&
      `.pick{outline:2px dashed #d2691e;}` &&
      `.act{color:#2c5f8a;text-decoration:none;margin-right:6px;}` &&
      `.hint{color:#d2691e;font-weight:bold;}` &&
      `.dir{color:#888;font-size:9px;}` &&
      `.rmx{color:#a00;text-decoration:none;}` &&
      `select{font-size:10px;}` &&
      `</style>` &&
      "drag a field chip onto a zone (mouse events + hidden form, as elsewhere)
      `<script>var mk=null,mt=null,mz=null,mv=false;` &&
      `function md(e,k){mk=k;mv=false;return true;}` &&
      `function zo(e,el,z){if(!mk)return;mv=true;` &&
      `if(mt&&mt!==el){mt.style.boxShadow='';}mt=el;mz=z;` &&
      `el.style.boxShadow='0 0 0 2px #d2691e';}` &&
      `function cl(e){if(mv){mv=false;if(e.preventDefault)e.preventDefault();return false;}return true;}` &&
      `document.onmouseup=function(){if(!mk)return;` &&
      `var k=mk,z=mz,ok=mv;mk=null;mz=null;` &&
      `if(mt){mt.style.boxShadow='';mt=null;}` &&
      `if(ok&&z){document.getElementById('pmk').value=z+'_'+k;` &&
      `document.getElementById('pmf').submit();}};` &&
      `</script>` &&
      `</head><body onselectstart="return false">` &&
      `<form id="pmf" method="post" action="SAPEVENT:pvdrop" style="display:none">` &&
      `<input type="hidden" name="mv" id="pmk"></form>` &&
      `<h4>PIVOT&nbsp;&nbsp;<a class="act" href="SAPEVENT:pv?CLR">clear</a></h4>`.

    IF m_pick IS NOT INITIAL.
      rv_html = rv_html &&
        |<div class="hint">picked: { to_lower( m_pick ) } - now click a button inside the target zone | &&
        |(click the field again to cancel)</div>|.
    ELSE.
      rv_html = rv_html &&
        `<div class="dir">drag a field into a zone (or: click the field, then click inside the zone)</div>`.
    ENDIF.

    "zones: drop targets; when a field is picked the add-buttons appear inside as well
    DATA(l_pick_low) = to_lower( m_pick ).
    rv_html = rv_html && `<h4>Rows (group by)</h4><div class="zone" onmouseover="zo(event,this,'dr')">`.
    LOOP AT mt_rows INTO DATA(l_row).
      rv_html = rv_html &&
        |<a class="zchip" href="SAPEVENT:pv?rr_{ l_row }" title="remove">{ to_lower( l_row ) } &#10005;</a>|.
    ENDLOOP.
    IF m_pick IS NOT INITIAL.
      rv_html = rv_html &&
        |<a class="zchip add" href="SAPEVENT:pv?tr">+ { l_pick_low }</a>|.
    ELSEIF mt_rows IS INITIAL.
      rv_html = rv_html && `<span class="dir">group-by fields go here</span>`.
    ENDIF.

    rv_html = rv_html && `</div><h4>Columns (spread by values)</h4><div class="zone" onmouseover="zo(event,this,'dc')">`.
    LOOP AT mt_cols INTO DATA(l_col).
      rv_html = rv_html &&
        |<a class="zchip" href="SAPEVENT:pv?rc_{ l_col }" title="remove">{ to_lower( l_col ) } &#10005;</a>|.
    ENDLOOP.
    IF m_pick IS NOT INITIAL.
      rv_html = rv_html &&
        |<a class="zchip add" href="SAPEVENT:pv?tc">+ { l_pick_low }</a>|.
    ELSEIF mt_cols IS INITIAL.
      rv_html = rv_html && `<span class="dir">fields; their value combinations become CASE columns (max 50)</span>`.
    ENDIF.

    rv_html = rv_html && `</div><h4>Values (aggregates at the intersections)</h4><div class="zone" onmouseover="zo(event,this,'dv')">`.
    LOOP AT mt_vals INTO DATA(ls_val).
      DATA(l_vidx) = sy-tabix.
      rv_html = rv_html && |<span class="zchip">| &&
        |<form style="display:inline" method="post" action="SAPEVENT:pvagg">| &&
        |<input type="hidden" name="idx" value="{ l_vidx }">| &&
        |<select name="agg" onchange="this.form.submit()">|.
      LOOP AT lt_aggs INTO DATA(l_a).
        rv_html = rv_html &&
          |<option{ COND string( WHEN ls_val-agg = l_a THEN ' selected' ) }>{ l_a }</option>|.
      ENDLOOP.
      rv_html = rv_html &&
        |</select> { to_lower( ls_val-key ) }| &&
        | <a class="rmx" href="SAPEVENT:pv?rv_{ l_vidx }" title="remove">&#10005;</a></form></span>|.
    ENDLOOP.
    IF m_pick IS NOT INITIAL.
      rv_html = rv_html &&
        |<a class="zchip add" href="SAPEVENT:pv?ag_SUM">+ SUM( { l_pick_low } )</a>| &&
        |<a class="zchip add" href="SAPEVENT:pv?ag_COUNT">+ COUNT( { l_pick_low } )</a>| &&
        |<a class="zchip add" href="SAPEVENT:pv?ag_MIN">+ MIN( { l_pick_low } )</a>| &&
        |<a class="zchip add" href="SAPEVENT:pv?ag_MAX">+ MAX( { l_pick_low } )</a>| &&
        |<a class="zchip add" href="SAPEVENT:pv?ag_AVG">+ AVG( { l_pick_low } )</a>|.
    ELSEIF mt_vals IS INITIAL.
      rv_html = rv_html && `<span class="dir">aggregated fields go here</span>`.
    ENDIF.
    rv_html = rv_html && `</div><h4>Available fields</h4>`.

    "available fields as chips
    LOOP AT it_fields INTO DATA(ls_fld).
      DATA(l_key) = |{ condense( CONV string( ls_fld-alias ) ) }~{ ls_fld-fieldname }|.
      DATA(l_cls) = COND string( WHEN m_pick = l_key THEN 'chip pick' ELSE 'chip' ).
      DATA(l_label) = COND string(
        WHEN i_show_texts = abap_true AND ls_fld-ddtext IS NOT INITIAL
        THEN escape( val = ls_fld-ddtext format = cl_abap_format=>e_html_text )
        ELSE |{ ls_fld-fieldname }| ).
      rv_html = rv_html &&
        |<a class="{ l_cls }" href="SAPEVENT:pv?pk_{ l_key }"| &&
        | draggable="false" ondragstart="return false"| &&
        | onmousedown="return md(event,'{ l_key }')" onclick="return cl(event)"| &&
        | title="{ l_key } { escape( val = ls_fld-ddtext format = cl_abap_format=>e_html_attr ) }">| &&
        |{ l_label }</a>|.
    ENDLOOP.

    rv_html = rv_html && `</body></html>`.
  ENDMETHOD.

  METHOD build_select.
    DATA: l_fields TYPE string,
          l_group  TYPE string,
          lt_names TYPE tt_keys.
    DATA(l_nl) = cl_abap_char_utilities=>newline.

    CHECK has_layout( ) = abap_true.

    "row dimensions
    LOOP AT mt_rows INTO DATA(l_dim).
      DATA(l_qual) = qualify( i_key = l_dim i_multi = i_multi ).
      IF l_fields IS NOT INITIAL.
        l_fields = |{ l_fields },{ l_nl }       |.
        l_group  = |{ l_group }, |.
      ENDIF.
      l_fields = |{ l_fields }{ l_qual } AS { to_lower( comp_name( l_dim ) ) }|.
      l_group  = |{ l_group }{ l_qual }|.
    ENDLOOP.

    IF has_columns( ) = abap_true AND it_col_vals IS NOT INITIAL.
      "matrix: one CASE bucket per column value and aggregate
      LOOP AT it_col_vals INTO DATA(ls_cv).
        LOOP AT mt_vals INTO DATA(ls_val).
          DATA(l_fq) = qualify( i_key = ls_val-key i_multi = i_multi ).
          DATA(l_cond) = COND string( WHEN ls_cv-cond IS NOT INITIAL
                                      THEN ls_cv-cond
                                      ELSE |{ qualify( i_key = mt_cols[ 1 ] i_multi = i_multi ) } = { ls_cv-literal }| ).
          "component: AGG_FIELD_<value>, unique, <= 30
          DATA(l_sanval) = sanitize( ls_cv-text ).
          DATA(l_acomp) = comp_name( i_key = ls_val-key i_prefix = ls_val-agg ).
          DATA(l_base_len) = 29 - strlen( l_sanval ).
          DATA(l_name) = COND string( WHEN strlen( l_acomp ) > l_base_len
                                      THEN l_acomp+0(l_base_len) ELSE l_acomp ) && '_' && l_sanval.
          WHILE line_exists( lt_names[ table_line = l_name ] ).
            l_name = |{ COND string( WHEN strlen( l_name ) > 28 THEN l_name+0(28) ELSE l_name ) }_{ sy-index }|.
          ENDWHILE.
          APPEND l_name TO lt_names.
          IF l_fields IS NOT INITIAL.
            l_fields = |{ l_fields },{ l_nl }       |.
          ENDIF.
          "no ELSE: other buckets stay NULL and are ignored by the aggregate
          l_fields = |{ l_fields }{ ls_val-agg }( CASE WHEN { l_cond } | &&
                     |THEN { l_fq } END ) AS { to_lower( l_name ) }|.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      "no buckets: plain aggregates; column fields act as dimensions instead of disappearing
      LOOP AT mt_cols INTO DATA(l_col_dim).
        DATA(l_cdim) = qualify( i_key = l_col_dim i_multi = i_multi ).
        IF l_fields IS NOT INITIAL.
          l_fields = |{ l_fields },{ l_nl }       |.
          l_group  = |{ l_group }, |.
        ENDIF.
        l_fields = |{ l_fields }{ l_cdim } AS { to_lower( comp_name( l_col_dim ) ) }|.
        l_group  = |{ l_group }{ l_cdim }|.
      ENDLOOP.
      LOOP AT mt_vals INTO ls_val.
        IF l_fields IS NOT INITIAL.
          l_fields = |{ l_fields },{ l_nl }       |.
        ENDIF.
        l_fields = |{ l_fields }{ ls_val-agg }( { qualify( i_key = ls_val-key i_multi = i_multi ) } )| &&
                   | AS { to_lower( comp_name( i_key = ls_val-key i_prefix = ls_val-agg ) ) }|.
      ENDLOOP.
    ENDIF.

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
