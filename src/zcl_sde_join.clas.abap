CLASS zcl_sde_join DEFINITION PUBLIC INHERITING FROM zcl_sde_popup CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_pair,
             cand_field TYPE fieldname,
             base_field TYPE fieldname,
           END OF t_pair,
           tt_pairs TYPE STANDARD TABLE OF t_pair WITH DEFAULT KEY,

           BEGIN OF t_cand,
             tabname   TYPE tabname,
             ddtext    TYPE as4text,
             direction TYPE char1, "O-outgoing FK, I-incoming FK, T-text table, M-manual
             pairs     TYPE tt_pairs,
             selected  TYPE abap_bool,
             sel_order TYPE i,
           END OF t_cand,
           tt_cand TYPE STANDARD TABLE OF t_cand WITH DEFAULT KEY,

           BEGIN OF t_jtab,
             alias   TYPE char5,
             tabname TYPE tabname,
             ddtext  TYPE as4text,
             jtype   TYPE char10,          "INNER / LEFT OUTER
             cond    TYPE c LENGTH 255,    "ON condition, editable
           END OF t_jtab,
           tt_jtab TYPE STANDARD TABLE OF t_jtab WITH DEFAULT KEY,

           BEGIN OF t_jfld,
             sel       TYPE abap_bool,
             alias     TYPE char5,
             tabname   TYPE tabname,
             fieldname TYPE fieldname,
             keyflag   TYPE keyflag,
             ddtext    TYPE reptext,
           END OF t_jfld,
           tt_jfld TYPE STANDARD TABLE OF t_jfld WITH DEFAULT KEY.

    METHODS: constructor IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer.

  PRIVATE SECTION.
    DATA: mo_viewer    TYPE REF TO zcl_sde_table_viewer,
          m_tabname    TYPE tabname,
          mt_cand      TYPE tt_cand,
          mt_jtabs     TYPE tt_jtab,
          mt_jflds     TYPE tt_jfld,
          m_sel_count  TYPE i,
          mo_html      TYPE REF TO cl_gui_html_viewer,
          mo_low_split TYPE REF TO cl_gui_splitter_container,
          mo_flds_alv  TYPE REF TO cl_gui_alv_grid,
          mo_sql_text  TYPE REF TO cl_gui_textedit.

    METHODS:
      find_candidates,
      get_fieldlist IMPORTING i_tabname       TYPE tabname
                    RETURNING VALUE(rt_dfies) TYPE ddfields,
      render_html,
      show_html IMPORTING io_html TYPE REF TO cl_gui_html_viewer i_html TYPE string,
      add_candidate IMPORTING i_tabname TYPE tabname,
      toggle_candidate IMPORTING i_tabname TYPE tabname,
      rebuild_selection,
      move_table IMPORTING i_alias TYPE char5 i_dir TYPE i,
      apply_postdata IMPORTING it_postdata TYPE cnht_post_data_tab RETURNING VALUE(rv_act) TYPE string,
      create_sql_view,
      update_sql_view,
      create_flds_alv,
      set_marked_rows IMPORTING i_sel TYPE abap_bool,
      refresh_all,
      generate_select RETURNING VALUE(rv_sql) TYPE string,
      run_select,
      execute_sql IMPORTING i_sql TYPE string,
      upper_outside_quotes IMPORTING i_sql         TYPE string
                           RETURNING VALUE(rv_sql) TYPE string,

      on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
        IMPORTING action getdata postdata,
      on_flds_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      on_flds_ucomm FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_flds_dblclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING es_row_no.
ENDCLASS.



CLASS zcl_sde_join IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_viewer = io_viewer.
    m_tabname = io_viewer->m_tabname.

    mo_box = create( i_width = 1000 i_hight = 400 ).
    mo_box->set_caption( |Join builder: { m_tabname }| ).
    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_box
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.
    mo_splitter->set_row_mode( mode = mo_splitter->mode_relative ).
    mo_splitter->set_row_height( id = 1 height = 45 ).

    mo_splitter->get_container( EXPORTING row = 1 column = 1 RECEIVING container = DATA(lo_top) ).
    mo_splitter->get_container( EXPORTING row = 2 column = 1 RECEIVING container = DATA(lo_bottom) ).

    CREATE OBJECT mo_low_split ##FM_SUBRC_OK
      EXPORTING
        parent  = lo_bottom
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    CREATE OBJECT mo_html
      EXPORTING
        parent             = lo_top
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    IF sy-subrc = 0.
      mo_html->set_registered_events( VALUE #( ( eventid = cl_gui_html_viewer=>m_id_sapevent ) ) ).
      SET HANDLER on_sapevent FOR mo_html.
    ENDIF.

    find_candidates( ).
    rebuild_selection( ).
    create_sql_view( ).
    create_flds_alv( ).
    render_html( ).
    update_sql_view( ).
  ENDMETHOD.

  METHOD get_fieldlist.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = i_tabname
        langu          = sy-langu
      TABLES
        dfies_tab      = rt_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      CLEAR rt_dfies.
    ENDIF.
    DELETE rt_dfies WHERE datatype = 'CLNT' OR fieldname CP '.*'.
  ENDMETHOD.

  METHOD find_candidates.
    DATA: lt_dd08l TYPE TABLE OF dd08l,
          lt_keys  TYPE TABLE OF dd05p. "resolved field pairs incl. checkfield

    "Outgoing: foreign keys defined on the base table -> check tables
    SELECT * FROM dd08l INTO TABLE lt_dd08l
      WHERE tabname = m_tabname
        AND as4local = 'A'.                             "#EC CI_GENBUFF
    LOOP AT lt_dd08l INTO DATA(ls_fk) WHERE checktable IS NOT INITIAL
                                        AND checktable NE m_tabname
                                        AND checktable NE '*'.
      CLEAR lt_keys.
      CALL FUNCTION 'DD_FORKEY_GET'
        EXPORTING
          feldname  = ls_fk-fieldname
          tabname   = m_tabname
        TABLES
          forkeytab = lt_keys
        EXCEPTIONS
          OTHERS    = 4.
      CHECK sy-subrc < 2.
      READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = ls_fk-checktable.
      IF sy-subrc NE 0.
        APPEND VALUE #( tabname = ls_fk-checktable direction = 'O' ) TO mt_cand ASSIGNING <cand>.
      ENDIF.
      LOOP AT lt_keys INTO DATA(ls_key) WHERE fortable = m_tabname
                                          AND checkfield NE 'MANDT'
                                          AND checkfield IS NOT INITIAL.
        IF NOT line_exists( <cand>-pairs[ cand_field = ls_key-checkfield ] ).
          APPEND VALUE #( cand_field = ls_key-checkfield base_field = ls_key-forkey ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    "Incoming: tables having the base table as check table
    CLEAR lt_dd08l.
    SELECT * FROM dd08l INTO TABLE lt_dd08l UP TO 100 ROWS
      WHERE checktable = m_tabname
        AND as4local = 'A'.                             "#EC CI_GENBUFF
    LOOP AT lt_dd08l INTO ls_fk WHERE tabname NE m_tabname.
      IF NOT line_exists( mt_cand[ tabname = ls_fk-tabname ] ) AND lines( mt_cand ) >= 30.
        CONTINUE. "keep the canvas readable
      ENDIF.
      CLEAR lt_keys.
      CALL FUNCTION 'DD_FORKEY_GET'
        EXPORTING
          feldname  = ls_fk-fieldname
          tabname   = ls_fk-tabname
        TABLES
          forkeytab = lt_keys
        EXCEPTIONS
          OTHERS    = 4.
      CHECK sy-subrc < 2.
      READ TABLE mt_cand ASSIGNING <cand> WITH KEY tabname = ls_fk-tabname.
      IF sy-subrc NE 0.
        APPEND VALUE #( tabname = ls_fk-tabname direction = 'I' ) TO mt_cand ASSIGNING <cand>.
      ENDIF.
      LOOP AT lt_keys INTO ls_key WHERE fortable = ls_fk-tabname
                                    AND checkfield NE 'MANDT'
                                    AND checkfield IS NOT INITIAL.
        IF NOT line_exists( <cand>-pairs[ cand_field = ls_key-forkey ] ).
          APPEND VALUE #( cand_field = ls_key-forkey base_field = ls_key-checkfield ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    "Text table
    zcl_sde_ddic=>get_text_table( EXPORTING i_tname = m_tabname IMPORTING e_tab = DATA(l_texttab) ).
    IF l_texttab IS NOT INITIAL AND NOT line_exists( mt_cand[ tabname = l_texttab ] ).
      APPEND VALUE #( tabname = l_texttab direction = 'T' ) TO mt_cand ASSIGNING <cand>.
      DATA(lt_base_keys) = get_fieldlist( m_tabname ).
      LOOP AT get_fieldlist( l_texttab ) INTO DATA(ls_tf) WHERE keyflag = abap_true.
        IF line_exists( lt_base_keys[ fieldname = ls_tf-fieldname keyflag = abap_true ] ).
          APPEND VALUE #( cand_field = ls_tf-fieldname base_field = ls_tf-fieldname ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Descriptions
    IF mt_cand IS NOT INITIAL.
      SELECT tabname, ddtext FROM dd02t
        INTO TABLE @DATA(lt_texts)
        FOR ALL ENTRIES IN @mt_cand
        WHERE tabname = @mt_cand-tabname
          AND ddlanguage = @sy-langu.
      LOOP AT mt_cand ASSIGNING <cand>.
        READ TABLE lt_texts INTO DATA(ls_text) WITH KEY tabname = <cand>-tabname.
        IF sy-subrc = 0.
          <cand>-ddtext = ls_text-ddtext.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD add_candidate.
    DATA(l_tabname) = i_tabname.
    TRANSLATE l_tabname TO UPPER CASE.
    CONDENSE l_tabname.
    CHECK l_tabname IS NOT INITIAL AND l_tabname NE m_tabname.

    IF zcl_sde_sql=>exist_table( l_tabname ) NE 1 AND zcl_sde_sql=>exist_view( l_tabname ) NE 1.
      MESSAGE |Table { l_tabname } does not exist| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = l_tabname.
    IF sy-subrc NE 0.
      APPEND VALUE #( tabname = l_tabname direction = 'M' ) TO mt_cand ASSIGNING <cand>.
      SELECT SINGLE ddtext FROM dd02t INTO <cand>-ddtext
        WHERE tabname = l_tabname AND ddlanguage = sy-langu.
      "propose ON condition by matching key field names
      DATA(lt_base) = get_fieldlist( m_tabname ).
      LOOP AT get_fieldlist( l_tabname ) INTO DATA(ls_f) WHERE keyflag = abap_true.
        IF line_exists( lt_base[ fieldname = ls_f-fieldname ] ).
          APPEND VALUE #( cand_field = ls_f-fieldname base_field = ls_f-fieldname ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF <cand>-selected = abap_false.
      toggle_candidate( l_tabname ).
    ENDIF.
  ENDMETHOD.

  METHOD toggle_candidate.
    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = i_tabname.
    CHECK sy-subrc = 0.
    IF <cand>-selected = abap_true.
      <cand>-selected = abap_false.
      CLEAR <cand>-sel_order.
    ELSE.
      <cand>-selected = abap_true.
      ADD 1 TO m_sel_count.
      <cand>-sel_order = m_sel_count.
    ENDIF.
    rebuild_selection( ).
    refresh_all( ).
  ENDMETHOD.

  METHOD refresh_all.
    render_html( ).
    IF mo_flds_alv IS BOUND.
      Zcl_SDE_common=>refresh( mo_flds_alv ).
    ENDIF.
    update_sql_view( ).
  ENDMETHOD.

  METHOD rebuild_selection.
    DATA: lt_old_flds TYPE tt_jfld,
          lt_old_tabs TYPE tt_jtab,
          lt_sorted   TYPE tt_cand.

    lt_old_flds = mt_jflds.
    lt_old_tabs = mt_jtabs.
    CLEAR: mt_jtabs, mt_jflds.

    "base table always first, alias t0
    APPEND VALUE #( alias = 'T0' tabname = m_tabname ) TO mt_jtabs.
    LOOP AT get_fieldlist( m_tabname ) INTO DATA(ls_f).
      APPEND VALUE #( sel = ls_f-keyflag alias = 'T0' tabname = m_tabname
                      fieldname = ls_f-fieldname keyflag = ls_f-keyflag
                      ddtext = ls_f-fieldtext ) TO mt_jflds.
    ENDLOOP.

    lt_sorted = VALUE #( FOR wa IN mt_cand WHERE ( selected = abap_true ) ( wa ) ).
    SORT lt_sorted BY sel_order.

    LOOP AT lt_sorted INTO DATA(ls_cand).
      DATA(l_alias) = |T{ sy-tabix }|.
      APPEND INITIAL LINE TO mt_jtabs ASSIGNING FIELD-SYMBOL(<jtab>).
      <jtab>-alias   = l_alias.
      <jtab>-tabname = ls_cand-tabname.
      <jtab>-ddtext  = ls_cand-ddtext.
      <jtab>-jtype   = 'LEFT OUTER'.
      DATA l_cond TYPE string. "build in a string: char field would eat trailing blanks after AND
      CLEAR l_cond.
      LOOP AT ls_cand-pairs INTO DATA(ls_pair).
        IF l_cond IS NOT INITIAL.
          l_cond = |{ l_cond } AND |.
        ENDIF.
        l_cond = |{ l_cond }{ l_alias CASE = LOWER }~{ ls_pair-cand_field } = t0~{ ls_pair-base_field }|.
      ENDLOOP.
      <jtab>-cond = l_cond.
      "keep user's edits from previous rebuild (only while the alias is unchanged,
      "otherwise the edited condition would reference a wrong alias)
      READ TABLE lt_old_tabs INTO DATA(ls_old_tab) WITH KEY tabname = ls_cand-tabname.
      IF sy-subrc = 0.
        <jtab>-jtype = ls_old_tab-jtype.
        IF ls_old_tab-cond IS NOT INITIAL AND ls_old_tab-alias = <jtab>-alias.
          <jtab>-cond = ls_old_tab-cond.
        ENDIF.
      ENDIF.

      LOOP AT get_fieldlist( ls_cand-tabname ) INTO ls_f.
        APPEND VALUE #( alias = l_alias tabname = ls_cand-tabname
                        fieldname = ls_f-fieldname keyflag = ls_f-keyflag
                        ddtext = ls_f-fieldtext
                        sel = boolc( line_exists( ls_cand-pairs[ cand_field = ls_f-fieldname ] ) )
                      ) TO mt_jflds.
      ENDLOOP.
    ENDLOOP.

    "restore previous manual (de)selections
    LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>).
      READ TABLE lt_old_flds INTO DATA(ls_old) WITH KEY tabname = <fld>-tabname fieldname = <fld>-fieldname.
      IF sy-subrc = 0.
        <fld>-sel = ls_old-sel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD render_html.
    CHECK mo_html IS BOUND.

    DATA(l_html) =
      `<html><head><meta charset="utf-8"><style>` &&
      `body{font-family:Arial,sans-serif;font-size:11px;margin:4px;background:#f6f8fa;}` &&
      `.base{display:inline-block;background:#2c5f8a;color:#fff;border-radius:6px;padding:6px 12px;margin:3px;font-weight:bold;}` &&
      `.card{display:inline-block;border:1px solid #aaa;border-radius:6px;padding:5px 10px;margin:3px;background:#fff;text-decoration:none;color:#000;}` &&
      `.card:hover{border-color:#2c5f8a;}` &&
      `.sel{background:#d3f2d3;border:2px solid #2e8b2e;font-weight:bold;}` &&
      `.dir{color:#888;font-size:9px;}` &&
      `form{display:inline-block;margin:3px;}` &&
      `table.j{border-collapse:collapse;margin-top:6px;background:#fff;border:1px solid #ddd;border-radius:4px;}` &&
      `table.j td{padding:2px 5px;vertical-align:middle;white-space:nowrap;}` &&
      `.alias{font-weight:bold;color:#2c5f8a;}` &&
      `.btn{border:1px solid #bbb;background:#f2f2f2;border-radius:3px;cursor:pointer;font-size:10px;padding:0 4px;}` &&
      `input.cond{width:380px;font-family:Consolas,monospace;font-size:11px;}` &&
      `select{font-size:11px;}` &&
      `</style></head><body>` &&
      |<span class="base">{ m_tabname }</span> &#8646; |.

    LOOP AT mt_cand INTO DATA(ls_cand).
      DATA(l_class) = COND string( WHEN ls_cand-selected = abap_true THEN 'card sel' ELSE 'card' ).
      DATA(l_dir) = SWITCH string( ls_cand-direction
                      WHEN 'O' THEN '&#8594;'   "outgoing
                      WHEN 'I' THEN '&#8592;'   "incoming
                      WHEN 'T' THEN 'TXT'
                      ELSE '+' ).
      l_html = l_html &&
        |<a class="{ l_class }" href="SAPEVENT:toggle?{ ls_cand-tabname }">| &&
        |{ ls_cand-tabname } <span class="dir">{ l_dir } { escape( val = ls_cand-ddtext format = cl_abap_format=>e_html_text ) }</span></a>|.
    ENDLOOP.

    l_html = l_html &&
      `<form method="post" action="SAPEVENT:addtab">` &&
      `<input type="text" name="newtab" size="16" maxlength="30">` &&
      `<input type="submit" value="Add table"></form>`.

    "joined tables: order, join type, ON condition
    IF lines( mt_jtabs ) > 1.
      l_html = l_html && `<form method="post" action="SAPEVENT:tabs"><table class="j">`.
      LOOP AT mt_jtabs INTO DATA(ls_tab) FROM 2.
        DATA(l_inner) = COND string( WHEN ls_tab-jtype = 'INNER' THEN ' selected' ).
        DATA(l_left)  = COND string( WHEN ls_tab-jtype NE 'INNER' THEN ' selected' ).
        l_html = l_html &&
          |<tr><td><button class="btn" type="submit" name="act" value="up_{ ls_tab-alias }">&#9650;</button>| &&
          |<button class="btn" type="submit" name="act" value="dn_{ ls_tab-alias }">&#9660;</button>| &&
          |<button class="btn" type="submit" name="act" value="del_{ ls_tab-alias }">&#10005;</button></td>| &&
          |<td class="alias">{ ls_tab-alias }</td>| &&
          |<td><b>{ ls_tab-tabname }</b></td>| &&
          |<td><select name="jt_{ ls_tab-alias }">| &&
          |<option{ l_inner }>INNER</option><option{ l_left }>LEFT OUTER</option></select> JOIN ON</td>| &&
          |<td><input class="cond" type="text" name="on_{ ls_tab-alias }" value="{
             escape( val = CONV string( ls_tab-cond ) format = cl_abap_format=>e_html_attr ) }"></td></tr>|.
      ENDLOOP.
      l_html = l_html &&
        `</table><button class="btn" type="submit" name="act" value="apply">Apply</button></form>`.
    ENDIF.

    l_html = l_html && `</body></html>`.
    show_html( io_html = mo_html i_html = l_html ).
  ENDMETHOD.

  METHOD show_html.
    DATA: lt_html TYPE TABLE OF char255,
          l_url   TYPE char255.

    DATA(l_len) = strlen( i_html ).
    DATA(l_off) = 0.
    WHILE l_off < l_len.
      DATA(l_chunk) = nmin( val1 = 255 val2 = l_len - l_off ).
      APPEND i_html+l_off(l_chunk) TO lt_html.
      l_off = l_off + l_chunk.
    ENDWHILE.

    io_html->load_data(
      EXPORTING
        type         = 'text'
        subtype      = 'html'
      IMPORTING
        assigned_url = l_url
      CHANGING
        data_table   = lt_html
      EXCEPTIONS
        OTHERS       = 1 ).
    IF sy-subrc = 0.
      io_html->show_url( url = l_url ).
    ENDIF.
  ENDMETHOD.

  METHOD move_table.
    READ TABLE mt_jtabs INTO DATA(ls_tab) WITH KEY alias = i_alias.
    CHECK sy-subrc = 0.
    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = ls_tab-tabname.
    CHECK sy-subrc = 0 AND <cand>-selected = abap_true.

    "find the neighbour in selection order
    DATA(l_best) = 0.
    LOOP AT mt_cand ASSIGNING FIELD-SYMBOL(<other>) WHERE selected = abap_true AND tabname NE <cand>-tabname.
      IF ( i_dir < 0 AND <other>-sel_order < <cand>-sel_order AND ( l_best = 0 OR <other>-sel_order > l_best ) )
      OR ( i_dir > 0 AND <other>-sel_order > <cand>-sel_order AND ( l_best = 0 OR <other>-sel_order < l_best ) ).
        l_best = <other>-sel_order.
      ENDIF.
    ENDLOOP.
    CHECK l_best NE 0.
    READ TABLE mt_cand ASSIGNING <other> WITH KEY sel_order = l_best selected = abap_true.
    CHECK sy-subrc = 0.
    DATA(l_tmp) = <cand>-sel_order.
    <cand>-sel_order  = <other>-sel_order.
    <other>-sel_order = l_tmp.
  ENDMETHOD.

  METHOD apply_postdata.
    "form fields: act=..., jt_Tn=..., on_Tn=... (url-encoded)
    DATA(l_post) = concat_lines_of( table = it_postdata ).
    SPLIT l_post AT '&' INTO TABLE DATA(lt_pairs).
    LOOP AT lt_pairs INTO DATA(l_pair).
      SPLIT l_pair AT '=' INTO DATA(l_name) DATA(l_value).
      REPLACE ALL OCCURRENCES OF '+' IN l_value WITH ` `.
      l_value = cl_http_utility=>unescape_url( l_value ).
      IF l_name = 'act'.
        rv_act = l_value.
      ELSEIF l_name CP 'jt_*'.
        READ TABLE mt_jtabs ASSIGNING FIELD-SYMBOL(<jtab>) WITH KEY alias = l_name+3.
        IF sy-subrc = 0.
          <jtab>-jtype = l_value.
        ENDIF.
      ELSEIF l_name CP 'on_*'.
        READ TABLE mt_jtabs ASSIGNING <jtab> WITH KEY alias = l_name+3.
        IF sy-subrc = 0.
          <jtab>-cond = l_value.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_sapevent.
    CASE action.
      WHEN 'toggle'.
        toggle_candidate( CONV #( getdata ) ).
      WHEN 'addtab'.
        DATA(l_post) = concat_lines_of( table = postdata ).
        SPLIT l_post AT '=' INTO DATA(l_dummy) DATA(l_newtab).
        REPLACE ALL OCCURRENCES OF '+' IN l_newtab WITH ` `.
        l_newtab = cl_http_utility=>unescape_url( l_newtab ).
        add_candidate( CONV #( l_newtab ) ).
      WHEN 'tabs'.
        DATA(l_act) = apply_postdata( postdata ). "stores jtype/cond edits into mt_jtabs
        IF strlen( l_act ) > 3.
          CASE l_act(3).
            WHEN 'up_'.
              move_table( i_alias = CONV #( l_act+3 ) i_dir = -1 ).
              rebuild_selection( ).
            WHEN 'dn_'.
              move_table( i_alias = CONV #( l_act+3 ) i_dir = 1 ).
              rebuild_selection( ).
            WHEN 'del'. "del_Tn
              READ TABLE mt_jtabs INTO DATA(ls_tab) WITH KEY alias = l_act+4.
              IF sy-subrc = 0.
                toggle_candidate( ls_tab-tabname ). "deselects + rebuilds + refreshes
                RETURN.
              ENDIF.
          ENDCASE.
        ENDIF.
        refresh_all( ).
    ENDCASE.
  ENDMETHOD.

  METHOD create_sql_view.
    mo_low_split->set_column_width( id = 1 width = 45 ).
    mo_low_split->get_container( EXPORTING row = 1 column = 1 RECEIVING container = DATA(lo_cont) ).

    CREATE OBJECT mo_sql_text
      EXPORTING
        parent = lo_cont
      EXCEPTIONS
        OTHERS = 1.
  ENDMETHOD.

  METHOD update_sql_view.
    CHECK mo_sql_text IS BOUND.
    mo_sql_text->set_textstream( generate_select( ) ).
  ENDMETHOD.

  METHOD create_flds_alv.
    DATA: ls_layout TYPE lvc_s_layo.

    mo_low_split->get_container( EXPORTING row = 1 column = 2 RECEIVING container = DATA(lo_cont) ).
    mo_flds_alv = NEW #( i_parent = lo_cont ).

    DATA(lt_fcat) = VALUE lvc_t_fcat(
      ( fieldname = 'SEL'       coltext = 'Sel'   outputlen = 3 checkbox = 'X' )
      ( fieldname = 'ALIAS'     coltext = 'Alias' outputlen = 5 )
      ( fieldname = 'TABNAME'   coltext = 'Table' outputlen = 12 )
      ( fieldname = 'FIELDNAME' coltext = 'Field' outputlen = 15 )
      ( fieldname = 'KEYFLAG'   coltext = 'Key'   outputlen = 3 checkbox = 'X' )
      ( fieldname = 'DDTEXT'    coltext = 'Description' outputlen = 30 ) ).

    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'A'. "multi-row selection with Ctrl/Shift
    SET HANDLER on_flds_toolbar  FOR mo_flds_alv.
    SET HANDLER on_flds_ucomm    FOR mo_flds_alv.
    SET HANDLER on_flds_dblclick FOR mo_flds_alv.

    mo_flds_alv->set_table_for_first_display(
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = mt_jflds
        it_fieldcatalog = lt_fcat
      EXCEPTIONS
        OTHERS          = 1 ).
    mo_flds_alv->set_toolbar_interactive( ).
  ENDMETHOD.

  METHOD on_flds_toolbar.
    e_object->mt_toolbar = VALUE ttb_button(
      ( function = 'MARK'    icon = icon_okay   quickinfo = 'Mark selected rows (Ctrl/Shift+click rows first)' text = 'Mark' )
      ( function = 'UNMARK'  icon = icon_cancel quickinfo = 'Unmark selected rows' text = 'Unmark' )
      ( butn_type = 3 )
      ( function = 'SEL_ALL' icon = icon_select_all   quickinfo = 'Select all fields' )
      ( function = 'DESEL'   icon = icon_deselect_all quickinfo = 'Deselect all fields' )
      ( function = 'KEYS'    icon = icon_foreign_key  quickinfo = 'Select key fields only' )
      ( butn_type = 3 )
      ( function = 'RUN'     icon = icon_execute_object quickinfo = 'Run the SELECT shown on the left' text = 'Run' ) ).
  ENDMETHOD.

  METHOD set_marked_rows.
    mo_flds_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).
    LOOP AT lt_rows INTO DATA(ls_row).
      READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) INDEX ls_row-index.
      IF sy-subrc = 0.
        <fld>-sel = i_sel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_flds_ucomm.
    CASE e_ucomm.
      WHEN 'MARK'.
        set_marked_rows( abap_true ).
      WHEN 'UNMARK'.
        set_marked_rows( abap_false ).
      WHEN 'SEL_ALL'.
        LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>).
          <fld>-sel = abap_true.
        ENDLOOP.
      WHEN 'DESEL'.
        LOOP AT mt_jflds ASSIGNING <fld>.
          <fld>-sel = abap_false.
        ENDLOOP.
      WHEN 'KEYS'.
        LOOP AT mt_jflds ASSIGNING <fld>.
          <fld>-sel = <fld>-keyflag.
        ENDLOOP.
      WHEN 'RUN'. "run whatever is in the SQL view (incl. manual edits)
        run_select( ).
        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
    Zcl_SDE_common=>refresh( mo_flds_alv ).
    update_sql_view( ).
  ENDMETHOD.

  METHOD on_flds_dblclick. "double click toggles a single field
    READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.
    <fld>-sel = boolc( <fld>-sel = abap_false ).
    Zcl_SDE_common=>refresh( mo_flds_alv ).
    update_sql_view( ).
  ENDMETHOD.

  METHOD generate_select.
    DATA: l_fields TYPE string,
          l_from   TYPE string.

    LOOP AT mt_jflds INTO DATA(ls_fld) WHERE sel = abap_true.
      IF l_fields IS NOT INITIAL.
        l_fields = |{ l_fields },{ cl_abap_char_utilities=>newline }       |.
      ENDIF.
      l_fields = |{ l_fields }{ ls_fld-alias CASE = LOWER }~{ ls_fld-fieldname CASE = LOWER } AS { ls_fld-alias CASE = LOWER }_{ ls_fld-fieldname CASE = LOWER }|.
    ENDLOOP.
    IF l_fields IS INITIAL.
      l_fields = 't0~*'.
    ENDIF.

    LOOP AT mt_jtabs INTO DATA(ls_tab).
      IF sy-tabix = 1.
        l_from = |{ ls_tab-tabname CASE = LOWER } AS t0|.
      ELSE.
        l_from = |{ l_from }{ cl_abap_char_utilities=>newline }  { ls_tab-jtype } JOIN { ls_tab-tabname CASE = LOWER } AS { ls_tab-alias CASE = LOWER } ON { ls_tab-cond }|.
      ENDIF.
    ENDLOOP.

    rv_sql = |SELECT { l_fields }{ cl_abap_char_utilities=>newline }| &&
             |  FROM { l_from }|.

    "inherit WHERE from the base window selections, qualified with t0~
    DATA(l_where) = mo_viewer->get_where( ).
    IF l_where IS NOT INITIAL.
      LOOP AT get_fieldlist( m_tabname ) INTO DATA(ls_f).
        REPLACE ALL OCCURRENCES OF REGEX |\\b{ ls_f-fieldname }\\b| IN l_where WITH |t0~{ ls_f-fieldname }|.
      ENDLOOP.
      rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } WHERE { l_where }|.
    ENDIF.

    DATA(l_rows) = COND i( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 ).
    rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } UP TO { l_rows } ROWS|.
  ENDMETHOD.

  METHOD run_select.
    DATA l_sql TYPE string.
    mo_sql_text->get_textstream( IMPORTING text = l_sql ).
    cl_gui_cfw=>flush( ).
    CHECK l_sql IS NOT INITIAL.
    execute_sql( l_sql ).
  ENDMETHOD.

  METHOD execute_sql.
    DATA: l_sql    TYPE string,
          l_rows   TYPE i,
          lt_comp  TYPE abap_component_tab,
          lr_table TYPE REF TO data.
    FIELD-SYMBOLS: <result> TYPE STANDARD TABLE.

    l_sql = i_sql.
    CHECK l_sql IS NOT INITIAL.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf   IN l_sql WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_sql WITH ` `.
    CONDENSE l_sql.
    DATA(l_upper) = to_upper( l_sql ).

    "cut out UP TO n ROWS
    l_rows = COND i( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 ).
    FIND REGEX 'UP\s+TO\s+(\d+)\s+ROWS' IN l_upper
      MATCH OFFSET DATA(l_off) MATCH LENGTH DATA(l_len) SUBMATCHES DATA(l_rows_c).
    IF sy-subrc = 0.
      l_rows = l_rows_c.
      REPLACE SECTION OFFSET l_off LENGTH l_len OF l_sql   WITH ``.
      REPLACE SECTION OFFSET l_off LENGTH l_len OF l_upper WITH ``.
    ENDIF.

    "split into SELECT <fields> FROM <from> [WHERE <where>]
    FIND REGEX '^\s*SELECT\s' IN l_upper MATCH LENGTH DATA(l_sel_len).
    IF sy-subrc NE 0.
      MESSAGE 'Statement must start with SELECT' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    FIND REGEX '\sFROM\s' IN l_upper MATCH OFFSET DATA(l_from_off) MATCH LENGTH DATA(l_from_len).
    IF sy-subrc NE 0.
      MESSAGE 'FROM clause not found' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(l_fields) = condense( substring( val = l_sql off = l_sel_len len = l_from_off - l_sel_len ) ).
    DATA(l_rest_off) = l_from_off + l_from_len.
    DATA(l_where) = ``.
    FIND REGEX '\sWHERE\s' IN l_upper MATCH OFFSET DATA(l_where_off) MATCH LENGTH DATA(l_where_len).
    IF sy-subrc = 0.
      DATA(l_from) = condense( substring( val = l_sql off = l_rest_off len = l_where_off - l_rest_off ) ).
      l_where = condense( substring( val = l_sql off = l_where_off + l_where_len len = strlen( l_sql ) - l_where_off - l_where_len ) ).
    ELSE.
      l_from = condense( substring( val = l_sql off = l_rest_off len = strlen( l_sql ) - l_rest_off ) ).
    ENDIF.

    "alias -> table map from the FROM clause
    TYPES: BEGIN OF t_alias, alias TYPE string, tabname TYPE tabname, END OF t_alias.
    DATA lt_alias TYPE TABLE OF t_alias.
    FIND ALL OCCURRENCES OF REGEX '(\w+)\s+AS\s+(\w+)' IN to_upper( l_from ) RESULTS DATA(lt_matches).
    LOOP AT lt_matches INTO DATA(ls_match).
      DATA(l_tab)   = to_upper( substring( val = l_from off = ls_match-submatches[ 1 ]-offset len = ls_match-submatches[ 1 ]-length ) ).
      DATA(l_alias) = to_upper( substring( val = l_from off = ls_match-submatches[ 2 ]-offset len = ls_match-submatches[ 2 ]-length ) ).
      APPEND VALUE #( alias = l_alias tabname = l_tab ) TO lt_alias.
    ENDLOOP.
    IF lt_alias IS INITIAL.
      "no aliases: single table select
      APPEND VALUE #( alias = '' tabname = to_upper( segment( val = condense( l_from ) index = 1 sep = ` ` ) ) ) TO lt_alias.
    ENDIF.

    "build result structure from the field list
    SPLIT l_fields AT ',' INTO TABLE DATA(lt_fld_str).
    LOOP AT lt_fld_str INTO DATA(l_fld_str).
      CONDENSE l_fld_str.
      CHECK l_fld_str IS NOT INITIAL.
      DATA(l_fld_up) = to_upper( l_fld_str ).
      DATA: l_alias2 TYPE string, l_field TYPE string, l_name TYPE string.
      CLEAR: l_alias2, l_field, l_name.
      FIND REGEX '^(?:(\w+)~)?(\w+)(?:\s+AS\s+(\w+))?$' IN l_fld_up
        SUBMATCHES l_alias2 l_field l_name.
      IF sy-subrc NE 0.
        MESSAGE |Cannot parse field: { l_fld_str }| TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      IF l_name IS INITIAL.
        l_name = COND #( WHEN l_alias2 IS INITIAL THEN l_field ELSE |{ l_alias2 }_{ l_field }| ).
      ENDIF.
      IF strlen( l_name ) > 30.
        l_name = l_name+0(30).
      ENDIF.

      READ TABLE lt_alias INTO DATA(ls_alias) WITH KEY alias = l_alias2.
      IF sy-subrc NE 0.
        READ TABLE lt_alias INTO ls_alias INDEX 1.
      ENDIF.

      TRY.
          DATA(lo_type) = CAST cl_abap_datadescr(
            cl_abap_typedescr=>describe_by_name( |{ ls_alias-tabname }-{ l_field }| ) ).
        CATCH cx_root.
          MESSAGE |Unknown field { ls_alias-tabname }-{ l_field }| TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.

      "keep names unique
      WHILE line_exists( lt_comp[ name = l_name ] ).
        l_name = |{ COND string( WHEN strlen( l_name ) > 28 THEN l_name+0(28) ELSE l_name ) }_{ sy-index }|.
      ENDWHILE.
      APPEND VALUE #( name = l_name type = lo_type ) TO lt_comp.
    ENDLOOP.

    IF lt_comp IS INITIAL.
      MESSAGE 'No fields to select' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    "dynamic Open SQL tokens must be uppercase (keep quoted literals as-is)
    l_fields = to_upper( l_fields ).
    l_from   = to_upper( l_from ).
    l_where  = upper_outside_quotes( l_where ).

    TRY.
        DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
        DATA(lo_tab) = cl_abap_tabledescr=>create(
                         p_line_type  = lo_struct
                         p_table_kind = cl_abap_tabledescr=>tablekind_std
                         p_unique     = abap_false ).
        CREATE DATA lr_table TYPE HANDLE lo_tab.
        ASSIGN lr_table->* TO <result>.

        "new strict-mode Open SQL: dynamic tokens with AS aliases are supported here
        SELECT (l_fields)
          FROM (l_from)
          WHERE (l_where)
          INTO CORRESPONDING FIELDS OF TABLE @<result>
          UP TO @l_rows ROWS.

      CATCH cx_root INTO DATA(lx).
        MESSAGE lx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    zcl_sde_appl=>open_int_table(
      it_ref  = lr_table
      iv_name = |JOIN { m_tabname } ({ lines( <result> ) })| ).
  ENDMETHOD.

  METHOD upper_outside_quotes.
    "segments alternate: outside / inside single quotes
    SPLIT i_sql AT '''' INTO TABLE DATA(lt_parts).
    LOOP AT lt_parts ASSIGNING FIELD-SYMBOL(<part>).
      IF sy-tabix MOD 2 = 1. "outside quotes
        <part> = to_upper( <part> ).
      ENDIF.
    ENDLOOP.
    rv_sql = concat_lines_of( table = lt_parts sep = '''' ).
  ENDMETHOD.

ENDCLASS.
