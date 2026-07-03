CLASS zcl_sde_pivot DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_val,
             key TYPE string, "ALIAS~FIELD
             agg TYPE string, "SUM/COUNT/MIN/MAX/AVG
           END OF t_val,
           tt_vals TYPE STANDARD TABLE OF t_val WITH DEFAULT KEY,
           tt_keys TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    METHODS:
      has_layout RETURNING VALUE(rv_has) TYPE abap_bool,

      "HTML for the fields panel: zones + available field chips (SAPEVENT:pv?...)
      render_panel IMPORTING it_fields      TYPE zcl_sde_tools=>tt_jfld
                             i_show_texts   TYPE abap_bool DEFAULT abap_false
                   RETURNING VALUE(rv_html) TYPE string,

      "pv actions: pk_<key> pick, tr add to rows, ag_<AGG> add to values,
      "rr_<key> remove row, rv_<idx> remove value, CLR clear all
      handle_action IMPORTING i_act TYPE string,

      "GROUP BY statement over the current join configuration
      build_select IMPORTING i_from        TYPE string
                             i_where       TYPE string
                             i_multi       TYPE abap_bool
                             i_rows        TYPE i
                   RETURNING VALUE(rv_sql) TYPE string.

  PRIVATE SECTION.
    DATA: mt_rows TYPE tt_keys, "group-by fields in order
          mt_vals TYPE tt_vals, "aggregated fields
          m_pick  TYPE string.  "field picked for zone assignment

    METHODS:
      qualify IMPORTING i_key         TYPE string
                        i_multi       TYPE abap_bool
              RETURNING VALUE(rv_sql) TYPE string,
      comp_name IMPORTING i_key          TYPE string
                          i_prefix       TYPE string OPTIONAL
                RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.



CLASS zcl_sde_pivot IMPLEMENTATION.

  METHOD has_layout.
    rv_has = boolc( mt_rows IS NOT INITIAL OR mt_vals IS NOT INITIAL ).
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
  ENDMETHOD.

  METHOD handle_action.
    CASE i_act.
      WHEN 'CLR'.
        CLEAR: mt_rows, mt_vals, m_pick.
      WHEN 'tr'. "picked field -> rows
        IF m_pick IS NOT INITIAL AND NOT line_exists( mt_rows[ table_line = m_pick ] ).
          APPEND m_pick TO mt_rows.
        ENDIF.
        CLEAR m_pick.
      WHEN OTHERS.
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
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'rv_'. "remove from values by index
          DATA(l_idx) = CONV i( i_act+3 ).
          DELETE mt_vals INDEX l_idx.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD render_panel.
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
      `.pick{outline:2px dashed #d2691e;}` &&
      `.act{color:#2c5f8a;text-decoration:none;margin-right:6px;}` &&
      `.hint{color:#d2691e;font-weight:bold;}` &&
      `.dir{color:#888;font-size:9px;}` &&
      `</style></head><body>` &&
      `<h4>PIVOT (no columns yet: groups + totals)&nbsp;&nbsp;` &&
      `<a class="act" href="SAPEVENT:pv?CLR">clear</a></h4>`.

    IF m_pick IS NOT INITIAL.
      rv_html = rv_html &&
        |<div class="hint">{ m_pick }: add to&nbsp;| &&
        `<a class="act" href="SAPEVENT:pv?tr">Rows</a>` &&
        `<a class="act" href="SAPEVENT:pv?ag_SUM">SUM</a>` &&
        `<a class="act" href="SAPEVENT:pv?ag_COUNT">COUNT</a>` &&
        `<a class="act" href="SAPEVENT:pv?ag_MIN">MIN</a>` &&
        `<a class="act" href="SAPEVENT:pv?ag_MAX">MAX</a>` &&
        `<a class="act" href="SAPEVENT:pv?ag_AVG">AVG</a></div>|.
    ELSE.
      rv_html = rv_html &&
        `<div class="dir">click a field below, then choose the target zone</div>`.
    ENDIF.

    "zones
    rv_html = rv_html && `<h4>Rows (group by)</h4><div class="zone">`.
    LOOP AT mt_rows INTO DATA(l_row).
      rv_html = rv_html &&
        |<a class="zchip" href="SAPEVENT:pv?rr_{ l_row }" title="remove">{ to_lower( l_row ) } &#10005;</a>|.
    ENDLOOP.
    rv_html = rv_html && `</div><h4>Values (aggregates)</h4><div class="zone">`.
    LOOP AT mt_vals INTO DATA(ls_val).
      rv_html = rv_html &&
        |<a class="zchip" href="SAPEVENT:pv?rv_{ sy-tabix }" title="remove">| &&
        |<span class="agg">{ ls_val-agg }</span> { to_lower( ls_val-key ) } &#10005;</a>|.
    ENDLOOP.
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
        | title="{ l_key } { escape( val = ls_fld-ddtext format = cl_abap_format=>e_html_attr ) }">| &&
        |{ l_label }</a>|.
    ENDLOOP.

    rv_html = rv_html && `</body></html>`.
  ENDMETHOD.

  METHOD build_select.
    DATA: l_fields TYPE string,
          l_group  TYPE string.
    DATA(l_nl) = cl_abap_char_utilities=>newline.

    CHECK has_layout( ) = abap_true.

    LOOP AT mt_rows INTO DATA(l_row).
      DATA(l_qual) = qualify( i_key = l_row i_multi = i_multi ).
      IF l_fields IS NOT INITIAL.
        l_fields = |{ l_fields },{ l_nl }       |.
        l_group  = |{ l_group }, |.
      ENDIF.
      l_fields = |{ l_fields }{ l_qual } AS { to_lower( comp_name( l_row ) ) }|.
      l_group  = |{ l_group }{ l_qual }|.
    ENDLOOP.

    LOOP AT mt_vals INTO DATA(ls_val).
      IF l_fields IS NOT INITIAL.
        l_fields = |{ l_fields },{ l_nl }       |.
      ENDIF.
      l_fields = |{ l_fields }{ ls_val-agg }( { qualify( i_key = ls_val-key i_multi = i_multi ) } )| &&
                 | AS { to_lower( comp_name( i_key = ls_val-key i_prefix = ls_val-agg ) ) }|.
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
