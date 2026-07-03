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
             parent    TYPE tabname, "table whose FKs discovered this one; pairs link to it
             alias     TYPE char5,   "assigned at first selection, never renumbered
             checked   TYPE abap_bool, "emptiness check already done
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
             pos       TYPE i, "order in the SELECT list (only when sel = X)
             alias     TYPE char5,
             tabname   TYPE tabname,
             fieldname TYPE fieldname,
             keyflag   TYPE keyflag,
             ddtext    TYPE reptext,
           END OF t_jfld,
           tt_jfld TYPE STANDARD TABLE OF t_jfld WITH DEFAULT KEY.

    METHODS: constructor IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer.

protected section.
  PRIVATE SECTION.
    DATA: mo_viewer    TYPE REF TO zcl_sde_table_viewer,
          m_tabname    TYPE tabname,
          mt_cand      TYPE tt_cand,
          mt_jtabs     TYPE tt_jtab,
          mt_jflds     TYPE tt_jfld,
          m_sel_count  TYPE i,
          m_alias_count TYPE i, "grows only: aliases are never reused
          m_base_pos   TYPE i VALUE 1, "position of the base table in the join order (1 = FROM)
          mo_html      TYPE REF TO cl_gui_html_viewer,
          mo_low_split TYPE REF TO cl_gui_splitter_container,
          mo_flds_html TYPE REF TO cl_gui_html_viewer,
          mo_sql_html  TYPE REF TO cl_gui_html_viewer,
          m_ready      TYPE abap_bool,                   "constructor finished: changes go live to the viewer
          m_pick       TYPE string,                     "click-to-move: picked field key or table alias
          m_show_texts TYPE abap_bool,                   "field chips: texts instead of tech names
          m_fld_lang   TYPE spras.                       "language of the field texts

    METHODS:
      find_candidates,
      discover_for IMPORTING i_tabname TYPE tabname,
      get_fieldlist IMPORTING i_tabname       TYPE tabname
                    RETURNING VALUE(rt_dfies) TYPE ddfields,
      render_html,
      render_flds,
      show_html IMPORTING io_html TYPE REF TO cl_gui_html_viewer i_html TYPE string,
      add_candidate IMPORTING i_tabname TYPE tabname,
      toggle_candidate IMPORTING i_tabname TYPE tabname,
      rebuild_selection,
      normalize_pos,
      move_table IMPORTING i_alias TYPE char5 i_dir TYPE i,
      move_table_before IMPORTING i_from TYPE string i_to TYPE string,
      move_field_before IMPORTING i_from TYPE string i_to TYPE string,
      move_alias_fields_before IMPORTING i_from TYPE string i_to TYPE string,
      handle_fld_action IMPORTING i_act TYPE string,
      reload_field_texts,
      apply_postdata IMPORTING it_postdata TYPE cnht_post_data_tab RETURNING VALUE(rv_act) TYPE string,
      create_sql_view,
      update_sql_view,
      refresh_all,
      generate_select RETURNING VALUE(rv_sql) TYPE string,
      build_where RETURNING VALUE(rv_where) TYPE string,
      execute_sql IMPORTING i_sql TYPE string i_quiet TYPE abap_bool DEFAULT abap_false,
      viewer_alive RETURNING VALUE(rv_alive) TYPE abap_bool,
      on_viewer_sel FOR EVENT selection_done OF zcl_sde_sel_opt,
      upper_outside_quotes IMPORTING i_sql         TYPE string
                           RETURNING VALUE(rv_sql) TYPE string,

      on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
        IMPORTING action getdata postdata.
ENDCLASS.



CLASS ZCL_SDE_JOIN IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_viewer = io_viewer.
    m_tabname = io_viewer->m_tabname.
    m_fld_lang = sy-langu.

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

    mo_low_split->set_column_width( id = 1 width = 45 ).
    mo_low_split->get_container( EXPORTING row = 1 column = 2 RECEIVING container = DATA(lo_right) ).
    CREATE OBJECT mo_flds_html
      EXPORTING
        parent = lo_right
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc = 0.
      mo_flds_html->set_registered_events( VALUE #( ( eventid = cl_gui_html_viewer=>m_id_sapevent ) ) ).
      SET HANDLER on_sapevent FOR mo_flds_html.
    ENDIF.

    find_candidates( ).
    rebuild_selection( ).
    create_sql_view( ).
    refresh_all( ).
    m_ready = abap_true. "from now on every change is applied to the original window
    update_sql_view( ).

    "react on filter changes in the original window: SQL text + data follow
    IF mo_viewer->mo_sel IS BOUND.
      SET HANDLER on_viewer_sel FOR mo_viewer->mo_sel.
    ENDIF.
  ENDMETHOD.

  METHOD on_viewer_sel.
    update_sql_view( ).
  ENDMETHOD.


  METHOD get_fieldlist.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = i_tabname
        langu          = COND spras( WHEN m_fld_lang IS NOT INITIAL THEN m_fld_lang ELSE sy-langu )
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
    discover_for( m_tabname ).
  ENDMETHOD.


  METHOD discover_for.
    "adds the FK neighbours of i_tabname to the canvas; their ON conditions
    "will reference i_tabname (the parent), so the graph can be walked deeper
    DATA: lt_dd08l TYPE TABLE OF dd08l,
          lt_keys  TYPE TABLE OF dd05p. "resolved field pairs incl. checkfield

    "Outgoing: foreign keys defined on the parent -> check tables
    SELECT * FROM dd08l INTO TABLE lt_dd08l
      WHERE tabname = i_tabname
        AND as4local = 'A'.                             "#EC CI_GENBUFF
    LOOP AT lt_dd08l INTO DATA(ls_fk) WHERE checktable IS NOT INITIAL
                                        AND checktable NE i_tabname
                                        AND checktable NE m_tabname
                                        AND checktable NE '*'.
      IF line_exists( mt_cand[ tabname = ls_fk-checktable ] ).
        CONTINUE. "already on the canvas (keep the first parent)
      ENDIF.
      CLEAR lt_keys.
      CALL FUNCTION 'DD_FORKEY_GET'
        EXPORTING
          feldname  = ls_fk-fieldname
          tabname   = i_tabname
        TABLES
          forkeytab = lt_keys
        EXCEPTIONS
          OTHERS    = 4.
      CHECK sy-subrc < 2.
      APPEND VALUE #( tabname = ls_fk-checktable direction = 'O' parent = i_tabname )
        TO mt_cand ASSIGNING FIELD-SYMBOL(<cand>).
      LOOP AT lt_keys INTO DATA(ls_key) WHERE fortable = i_tabname
                                          AND checkfield NE 'MANDT'
                                          AND checkfield IS NOT INITIAL.
        IF NOT line_exists( <cand>-pairs[ cand_field = ls_key-checkfield ] ).
          APPEND VALUE #( cand_field = ls_key-checkfield base_field = ls_key-forkey ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    "Incoming: tables having the parent as check table
    CLEAR lt_dd08l.
    SELECT * FROM dd08l INTO TABLE lt_dd08l UP TO 100 ROWS
      WHERE checktable = i_tabname
        AND as4local = 'A'.                             "#EC CI_GENBUFF
    LOOP AT lt_dd08l INTO ls_fk WHERE tabname NE i_tabname
                                  AND tabname NE m_tabname.
      IF line_exists( mt_cand[ tabname = ls_fk-tabname ] ).
        CONTINUE.
      ENDIF.
      IF lines( mt_cand ) >= 60. "keep the canvas readable
        CONTINUE.
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
      APPEND VALUE #( tabname = ls_fk-tabname direction = 'I' parent = i_tabname )
        TO mt_cand ASSIGNING <cand>.
      LOOP AT lt_keys INTO ls_key WHERE fortable = ls_fk-tabname
                                    AND checkfield NE 'MANDT'
                                    AND checkfield IS NOT INITIAL.
        IF NOT line_exists( <cand>-pairs[ cand_field = ls_key-forkey ] ).
          APPEND VALUE #( cand_field = ls_key-forkey base_field = ls_key-checkfield ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    "Text table of the parent
    zcl_sde_ddic=>get_text_table( EXPORTING i_tname = i_tabname IMPORTING e_tab = DATA(l_texttab) ).
    IF l_texttab IS NOT INITIAL AND l_texttab NE m_tabname
       AND NOT line_exists( mt_cand[ tabname = l_texttab ] ).
      APPEND VALUE #( tabname = l_texttab direction = 'T' parent = i_tabname ) TO mt_cand ASSIGNING <cand>.
      DATA(lt_parent_keys) = get_fieldlist( i_tabname ).
      LOOP AT get_fieldlist( l_texttab ) INTO DATA(ls_tf) WHERE keyflag = abap_true.
        IF line_exists( lt_parent_keys[ fieldname = ls_tf-fieldname keyflag = abap_true ] ).
          APPEND VALUE #( cand_field = ls_tf-fieldname base_field = ls_tf-fieldname ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Keep only real database tables (drop structures and generated maintenance views)
    IF mt_cand IS NOT INITIAL.
      SELECT tabname FROM dd02l
        INTO TABLE @DATA(lt_real)
        FOR ALL ENTRIES IN @mt_cand
        WHERE tabname = @mt_cand-tabname
          AND tabclass IN ('TRANSP','POOL','CLUSTER').
      LOOP AT mt_cand ASSIGNING <cand>.
        IF NOT line_exists( lt_real[ table_line = <cand>-tabname ] ).
          DELETE mt_cand.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Drop tables without any data - no point in offering them for a join
    LOOP AT mt_cand ASSIGNING <cand> WHERE checked = abap_false.
      <cand>-checked = abap_true.
      DATA l_has_data TYPE abap_bool.
      CLEAR l_has_data.
      TRY.
          DATA(l_dyntab) = <cand>-tabname.
          SELECT SINGLE @abap_true FROM (l_dyntab) INTO @l_has_data.
        CATCH cx_root.                                  "#EC NO_HANDLER
      ENDTRY.
      IF l_has_data = abap_false.
        DELETE mt_cand.
      ENDIF.
    ENDLOOP.

    "Descriptions
    IF mt_cand IS NOT INITIAL.
      SELECT tabname, ddtext FROM dd02t
        INTO TABLE @DATA(lt_texts)
        FOR ALL ENTRIES IN @mt_cand
        WHERE tabname = @mt_cand-tabname
          AND ddlanguage = @sy-langu.
      LOOP AT mt_cand ASSIGNING <cand> WHERE ddtext IS INITIAL.
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
      "manually added: no emptiness check, the user asked for this table explicitly
      APPEND VALUE #( tabname = l_tabname direction = 'M' parent = m_tabname checked = abap_true ) TO mt_cand ASSIGNING <cand>.
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
      "cascade: tables joined via this one lose their anchor - deselect them too
      DATA lt_gone TYPE TABLE OF tabname.
      APPEND <cand>-tabname TO lt_gone.
      DATA(l_i) = 0.
      WHILE l_i < lines( lt_gone ).
        l_i = l_i + 1.
        DATA(l_parent) = lt_gone[ l_i ].
        LOOP AT mt_cand ASSIGNING FIELD-SYMBOL(<child>) WHERE selected = abap_true AND parent = l_parent.
          <child>-selected = abap_false.
          CLEAR <child>-sel_order.
          APPEND <child>-tabname TO lt_gone.
        ENDLOOP.
      ENDWHILE.
    ELSE.
      <cand>-selected = abap_true.
      ADD 1 TO m_sel_count.
      <cand>-sel_order = m_sel_count.
      IF <cand>-alias IS INITIAL. "permanent alias: survives arbitrary deselection of other joins
        ADD 1 TO m_alias_count.
        <cand>-alias = |T{ m_alias_count }|.
      ENDIF.
      discover_for( i_tabname ). "expand the canvas with the neighbours of this table
    ENDIF.
    rebuild_selection( ).
    refresh_all( ).
  ENDMETHOD.


  METHOD refresh_all.
    render_html( ).
    render_flds( ).
    update_sql_view( ).
  ENDMETHOD.


  METHOD rebuild_selection.
    DATA: lt_old_flds TYPE tt_jfld,
          lt_old_tabs TYPE tt_jtab,
          lt_sorted   TYPE tt_cand.

    lt_old_flds = mt_jflds.
    lt_old_tabs = mt_jtabs.
    CLEAR: mt_jtabs, mt_jflds.

    "base table fields always listed first
    LOOP AT get_fieldlist( m_tabname ) INTO DATA(ls_f).
      APPEND VALUE #( sel = abap_true alias = 'T0' tabname = m_tabname "the window opened with all fields
                      fieldname = ls_f-fieldname keyflag = ls_f-keyflag
                      ddtext = ls_f-fieldtext ) TO mt_jflds.
    ENDLOOP.

    lt_sorted = VALUE #( FOR wa IN mt_cand WHERE ( selected = abap_true ) ( wa ) ).
    SORT lt_sorted BY sel_order.

    "candidates keep their permanent alias regardless of join order or removals
    LOOP AT lt_sorted INTO DATA(ls_cand).
      DATA(l_alias) = condense( CONV string( ls_cand-alias ) ).
      APPEND INITIAL LINE TO mt_jtabs ASSIGNING FIELD-SYMBOL(<jtab>).
      <jtab>-alias   = l_alias.
      <jtab>-tabname = ls_cand-tabname.
      <jtab>-ddtext  = ls_cand-ddtext.
      <jtab>-jtype   = 'LEFT OUTER'.
      "the ON condition links to the parent's alias (t0 for the base, tN for a chained table)
      DATA(l_parent_alias) = `t0`.
      DATA(l_parent_missing) = abap_false.
      IF ls_cand-parent IS NOT INITIAL AND ls_cand-parent NE m_tabname.
        READ TABLE mt_jtabs INTO DATA(ls_parent_tab) WITH KEY tabname = ls_cand-parent.
        IF sy-subrc = 0.
          l_parent_alias = to_lower( condense( CONV string( ls_parent_tab-alias ) ) ).
        ELSE.
          l_parent_missing = abap_true. "parent left the join: anchor to the base table instead
        ENDIF.
      ENDIF.

      DATA(lt_pairs) = ls_cand-pairs.
      IF l_parent_missing = abap_true.
        DATA(lt_base_flds) = get_fieldlist( m_tabname ).
        "keep only pairs whose right side exists in the base table
        LOOP AT lt_pairs INTO DATA(ls_chk).
          IF NOT line_exists( lt_base_flds[ fieldname = ls_chk-base_field ] ).
            DELETE lt_pairs.
          ENDIF.
        ENDLOOP.
        IF lt_pairs IS INITIAL. "no overlap: propose matching key field names
          LOOP AT get_fieldlist( ls_cand-tabname ) INTO DATA(ls_kf) WHERE keyflag = abap_true.
            IF line_exists( lt_base_flds[ fieldname = ls_kf-fieldname ] ).
              APPEND VALUE #( cand_field = ls_kf-fieldname base_field = ls_kf-fieldname ) TO lt_pairs.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      DATA l_cond TYPE string. "build in a string: char field would eat trailing blanks after AND
      CLEAR l_cond.
      LOOP AT lt_pairs INTO DATA(ls_pair).
        IF l_cond IS NOT INITIAL.
          l_cond = |{ l_cond } AND |.
        ENDIF.
        l_cond = |{ l_cond }{ l_alias CASE = LOWER }~{ ls_pair-cand_field } = { l_parent_alias }~{ ls_pair-base_field }|.
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
        "default for a joined table: key fields only, skip names already in the SELECT
        DATA(l_dupe) = boolc( line_exists( mt_jflds[ fieldname = ls_f-fieldname sel = abap_true ] ) ).
        APPEND VALUE #( alias = l_alias tabname = ls_cand-tabname
                        fieldname = ls_f-fieldname keyflag = ls_f-keyflag
                        ddtext = ls_f-fieldtext
                        sel = boolc( ls_f-keyflag = abap_true AND l_dupe = abap_false )
                      ) TO mt_jflds.
      ENDLOOP.
    ENDLOOP.

    "insert the base table at its chosen position in the join order.
    "star topology: every ON references t0, so the base must be the 1st or 2nd
    "table - otherwise the DB sees t0 before it is joined ("table T0 unknown")
    IF m_base_pos < 1.
      m_base_pos = 1.
    ELSEIF m_base_pos > 2.
      m_base_pos = 2.
    ENDIF.
    IF m_base_pos > lines( mt_jtabs ) + 1.
      m_base_pos = lines( mt_jtabs ) + 1.
    ENDIF.
    DATA(ls_base) = VALUE t_jtab( alias = 'T0' tabname = m_tabname jtype = 'LEFT OUTER' ).
    READ TABLE lt_old_tabs INTO DATA(ls_old_base) WITH KEY tabname = m_tabname.
    IF sy-subrc = 0 AND ls_old_base-jtype IS NOT INITIAL.
      ls_base-jtype = ls_old_base-jtype.
    ENDIF.
    INSERT ls_base INTO mt_jtabs INDEX m_base_pos.

    "if the base is not the FROM table, the first table's link condition moves to the base row
    IF m_base_pos > 1.
      READ TABLE mt_jtabs ASSIGNING FIELD-SYMBOL(<first>) INDEX 1.
      READ TABLE mt_jtabs ASSIGNING FIELD-SYMBOL(<base>) INDEX m_base_pos.
      IF <base>-cond IS INITIAL.
        <base>-cond = <first>-cond.
      ENDIF.
      CLEAR <first>-cond. "FROM table has no ON

      "old base ON edits survive
      READ TABLE lt_old_tabs INTO ls_old_base WITH KEY tabname = m_tabname.
      IF sy-subrc = 0 AND ls_old_base-cond IS NOT INITIAL.
        <base>-cond = ls_old_base-cond.
      ENDIF.
    ENDIF.

    "restore previous manual (de)selections and the SELECT order
    LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>).
      READ TABLE lt_old_flds INTO DATA(ls_old) WITH KEY tabname = <fld>-tabname fieldname = <fld>-fieldname.
      IF sy-subrc = 0.
        <fld>-sel = ls_old-sel.
        <fld>-pos = ls_old-pos.
      ENDIF.
    ENDLOOP.
    normalize_pos( ).
  ENDMETHOD.


  METHOD normalize_pos.
    DATA l_max TYPE i.
    LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) WHERE sel = abap_true AND pos > 0.
      IF <fld>-pos > l_max.
        l_max = <fld>-pos.
      ENDIF.
    ENDLOOP.
    LOOP AT mt_jflds ASSIGNING <fld> WHERE sel = abap_true AND pos = 0.
      l_max = l_max + 1.
      <fld>-pos = l_max.
    ENDLOOP.
    LOOP AT mt_jflds ASSIGNING <fld> WHERE sel = abap_false AND pos NE 0.
      CLEAR <fld>-pos.
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
      `.grip{cursor:move;color:#888;font-weight:bold;}` &&
      `input.cond{width:380px;font-family:Consolas,monospace;font-size:11px;}` &&
      `select{font-size:11px;}` &&
      `.tabhdr{margin:6px 0 2px 0;font-weight:bold;color:#2c5f8a;}` &&
      `.chip{display:inline-block;border:1px solid #bbb;border-radius:10px;padding:1px 8px;margin:2px;text-decoration:none;color:#000;}` &&
      `.chip:hover{border-color:#2c5f8a;}` &&
      `.on{font-weight:bold;}` &&
      `.off{background:#fff!important;}` &&
      `.c1{background:#d8ecff;border-color:#5797c9;}` &&
      `.c2{background:#dcf5d6;border-color:#67a95e;}` &&
      `.c3{background:#fff1c2;border-color:#c09a2b;}` &&
      `.c4{background:#f3ddff;border-color:#a678c2;}` &&
      `.c5{background:#ffdede;border-color:#c87373;}` &&
      `.c6{background:#d9f4ef;border-color:#58a99b;}` &&
      `.key{border-style:double;border-width:3px;}` &&
      `.tblpill{display:inline-block;border:1px solid #888;border-radius:4px;padding:1px 8px;margin:2px;font-weight:bold;}` &&
      `.act{color:#2c5f8a;text-decoration:none;margin-right:6px;}` &&
      `.paint{outline:2px solid #2e8b2e;}` &&
      `</style>` &&
      `<script>var dk=null,lt=null;` &&
      `function ds(e,k){dk=k;try{e.dataTransfer.effectAllowed='move';` &&
      `e.dataTransfer.setData('Text',k);e.dataTransfer.setData('text',k);}catch(x){}return true;}` &&
      `function uh(){}` &&
      `function ov(e,el){if(e.preventDefault)e.preventDefault();` &&
      `try{e.dataTransfer.dropEffect='move';}catch(x){}return false;}` &&
      `function dp(e,k,p){if(e.preventDefault)e.preventDefault();uh();` &&
      `if(dk&&dk!=k){k=String(k).replace('#','%23');` &&
      `window.location.href='SAPEVENT:'+p+'?'+dk+'__'+k;}dk=null;return false;}` &&
      `document.ondragend=function(){uh();};` &&
      "lasso selection: hold the left mouse button and sweep over the chips
      `var pt=false,pks={},pn=0,painted=false,pm='1';` &&
      `function pd(e,el,k){e=e||window.event;pt=true;pks={};pn=0;painted=false;` &&
      `pm=(' '+el.className+' ').indexOf(' on ')>=0?'0':'1';pa(el,k);` &&
      `if(e.preventDefault)e.preventDefault();e.returnValue=false;return false;}` &&
      `function pv(e,el,k){if(pt&&!pks[k]){pa(el,k);if(pn>1)painted=true;}}` &&
      `function pa(el,k){pks[k]=1;pn++;el.className+=' paint';}` &&
      `function pc(e){if(painted){if(e.preventDefault)e.preventDefault();return false;}return true;}` &&
      `document.onmouseup=function(){if(!pt)return;pt=false;` &&
      `if(pn>1){var a=[];for(var k in pks)a.push(k);` &&
      `var f=document.getElementById('pf');f.action='SAPEVENT:fsel'+pm;` &&
      `document.getElementById('pk').value=a.join(';');f.submit();}};` &&
      `</script>` &&
      `</head><body>` &&
      `<form id="pf" method="post" action="SAPEVENT:fsel" style="display:none">` &&
      `<input type="hidden" name="keys" id="pk"></form>` &&
      |<span class="base">{ m_tabname }</span> &#8646; |.

    LOOP AT mt_cand INTO DATA(ls_cand).
      DATA(l_class) = COND string( WHEN ls_cand-selected = abap_true THEN 'card sel' ELSE 'card' ).
      DATA(l_dir) = SWITCH string( ls_cand-direction
                      WHEN 'O' THEN '&#8594;'   "outgoing
                      WHEN 'I' THEN '&#8592;'   "incoming
                      WHEN 'T' THEN 'TXT'
                      ELSE '+' ).
      DATA(l_via) = COND string( WHEN ls_cand-parent IS NOT INITIAL AND ls_cand-parent NE m_tabname
                                 THEN | via { ls_cand-parent }| ).
      l_html = l_html &&
        |<a class="{ l_class }" href="SAPEVENT:toggle?{ ls_cand-tabname }">| &&
        |{ ls_cand-tabname } <span class="dir">{ l_dir } { escape( val = ls_cand-ddtext format = cl_abap_format=>e_html_text ) }{ l_via }</span></a>|.
    ENDLOOP.

    l_html = l_html &&
      `<form method="post" action="SAPEVENT:addtab">` &&
      `<input type="text" name="newtab" size="16" maxlength="30">` &&
      `<input type="submit" value="Add table"></form>`.

    "joined tables: order (the first one is FROM), join type, ON condition
    IF lines( mt_jtabs ) > 1.
      l_html = l_html && `<form method="post" action="SAPEVENT:tabs"><table class="j">`.
      LOOP AT mt_jtabs INTO DATA(ls_tab).
        DATA(l_idx) = sy-tabix.
        DATA(l_key) = condense( CONV string( ls_tab-alias ) ).
        DATA(l_del) = COND string( WHEN ls_tab-tabname NE m_tabname
          THEN |<button class="btn" type="submit" name="act" value="del_{ l_key }">&#10005;</button>| ).
        l_html = l_html &&
          |<tr draggable="true" ondragstart="ds(event,'{ l_key }')" ondragover="return ov(event,this)"| &&
          | ondrop="return dp(event,'{ l_key }','tmv')">| &&
          |<td><span class="grip">&#8801;</span> | &&
          |<button class="btn" type="submit" name="act" value="up_{ l_key }">&#9650;</button>| &&
          |<button class="btn" type="submit" name="act" value="dn_{ l_key }">&#9660;</button>{ l_del }</td>| &&
          |<td class="alias">{ l_key }</td>| &&
          |<td><b>{ ls_tab-tabname }</b></td>|.
        IF l_idx = 1.
          l_html = l_html && |<td>FROM</td><td></td></tr>|.
        ELSE.
          DATA(l_inner) = COND string( WHEN ls_tab-jtype = 'INNER' THEN ' selected' ).
          DATA(l_left)  = COND string( WHEN ls_tab-jtype NE 'INNER' THEN ' selected' ).
          l_html = l_html &&
            |<td><select name="jt_{ l_key }" onchange="this.form.submit()">| &&
            |<option{ l_inner }>INNER</option><option{ l_left }>LEFT OUTER</option></select> JOIN ON</td>| &&
            |<td><input class="cond" type="text" name="on_{ l_key }" onchange="this.form.submit()" value="{
               escape( val = CONV string( ls_tab-cond ) format = cl_abap_format=>e_html_attr ) }"></td></tr>|.
        ENDIF.
      ENDLOOP.
      l_html = l_html &&
        `</table><button class="btn" type="submit" name="act" value="apply">Apply</button></form>`.
    ENDIF.

    l_html = l_html &&
      `<div class="tabhdr">FIELDS&nbsp;&nbsp;` &&
      `<a class="act" href="SAPEVENT:fld?ALL">select all</a>` &&
      `<a class="act" href="SAPEVENT:fld?NONE">clear</a>` &&
      `<a class="act" href="SAPEVENT:fld?KEYS">keys</a>` &&
      `<select onchange="window.location.href='SAPEVENT:fld?lng_'+this.value">` &&
      |<option value="TECH"{ COND string( WHEN m_show_texts = abap_false THEN ' selected' ) }>Technical name</option>|.
    LOOP AT zcl_sde_appl=>mt_lang INTO DATA(ls_top_lang).
      DATA(l_top_lang_sel) = COND string( WHEN m_show_texts = abap_true AND m_fld_lang = ls_top_lang-spras THEN ' selected' ).
      l_html = l_html &&
        |<option value="{ ls_top_lang-spras }"{ l_top_lang_sel }>{ escape( val = ls_top_lang-sptxt format = cl_abap_format=>e_html_text ) }</option>|.
    ENDLOOP.
    l_html = l_html && `</select></div>`.

    "fields grouped by table: click toggles SELECT membership, paint selects many
    LOOP AT mt_jtabs INTO DATA(ls_pick_tab).
      DATA(l_pick_alias) = condense( CONV string( ls_pick_tab-alias ) ).
      DATA(l_pick_color_idx) = sy-tabix - 1.
      l_pick_color_idx = l_pick_color_idx MOD 6.
      l_pick_color_idx = l_pick_color_idx + 1.
      DATA(l_pick_color) = |c{ l_pick_color_idx }|.
      l_html = l_html &&
        |<div class="tabhdr"><span class="tblpill { l_pick_color }">{ l_pick_alias }</span>| &&
        |{ ls_pick_tab-tabname }&nbsp;&nbsp;| &&
        |<a class="act" href="SAPEVENT:fld?all_{ l_pick_alias }">all</a>| &&
        |<a class="act" href="SAPEVENT:fld?non_{ l_pick_alias }">none</a>| &&
        |<a class="act" href="SAPEVENT:fld?key_{ l_pick_alias }">keys</a></div>|.
      LOOP AT mt_jflds INTO DATA(ls_pick_fld) WHERE alias = ls_pick_tab-alias.
        DATA(l_pick_cls) = COND string( WHEN ls_pick_fld-sel = abap_true THEN 'chip on' ELSE 'chip off' ).
        IF ls_pick_fld-keyflag = abap_true.
          l_pick_cls = l_pick_cls && ' key'.
        ENDIF.
        l_pick_cls = |{ l_pick_cls } { l_pick_color }|.
        DATA(l_pick_fkey) = |{ l_pick_alias }~{ ls_pick_fld-fieldname }|.
        DATA(l_pick_label) = COND string(
          WHEN m_show_texts = abap_true AND ls_pick_fld-ddtext IS NOT INITIAL
          THEN escape( val = ls_pick_fld-ddtext format = cl_abap_format=>e_html_text )
          ELSE |{ ls_pick_fld-fieldname }| ).
        l_html = l_html &&
          |<a class="{ l_pick_cls }" href="SAPEVENT:fld?tg_{ l_pick_fkey }"| &&
          | draggable="false" ondragstart="return false"| &&
          | onmousedown="return pd(event,this,'{ l_pick_fkey }')"| &&
          | onmouseover="pv(event,this,'{ l_pick_fkey }')"| &&
          | onselectstart="return false"| &&
          | onclick="return pc(event)" title="{ l_pick_fkey } { escape( val = ls_pick_fld-ddtext format = cl_abap_format=>e_html_attr ) }">| &&
          |{ l_pick_label }</a>|.
      ENDLOOP.
    ENDLOOP.

    l_html = l_html && `</body></html>`.
    show_html( io_html = mo_html i_html = l_html ).
  ENDMETHOD.


  METHOD render_flds.
    DATA lt_sel TYPE tt_jfld.

    CHECK mo_flds_html IS BOUND.

    DATA(l_html) =
      `<html><head><meta charset="utf-8"><style>` &&
      `body{font-family:Arial,sans-serif;font-size:11px;margin:4px;background:#fff;}` &&
      `.tabhdr{margin:6px 0 2px 0;font-weight:bold;color:#2c5f8a;}` &&
      `.chip{display:inline-block;border:1px solid #bbb;border-radius:10px;padding:1px 8px;margin:2px;text-decoration:none;color:#000;}` &&
      `.chip:hover{border-color:#2c5f8a;}` &&
      `.on{font-weight:bold;cursor:pointer;}` &&
      `.off{background:#fff!important;}` &&
      `.c1{background:#d8ecff;border-color:#5797c9;}` &&
      `.c2{background:#dcf5d6;border-color:#67a95e;}` &&
      `.c3{background:#fff1c2;border-color:#c09a2b;}` &&
      `.c4{background:#f3ddff;border-color:#a678c2;}` &&
      `.c5{background:#ffdede;border-color:#c87373;}` &&
      `.c6{background:#d9f4ef;border-color:#58a99b;}` &&
      `.key{border-style:double;border-width:3px;}` &&
      `.rm{color:#a00;text-decoration:none;margin-left:4px;}` &&
      `.tblpill{display:inline-block;border:1px solid #888;border-radius:4px;padding:1px 8px;margin:2px;font-weight:bold;text-decoration:none;color:#000;}` &&
      `.zone{display:inline-block;border:1px dashed #bbb;border-radius:10px;color:#999;padding:1px 8px;margin:2px;text-decoration:none;}` &&
      `.dir{color:#888;font-size:9px;}` &&
      `.act{color:#2c5f8a;text-decoration:none;margin-right:6px;}` &&
      `.paint{outline:2px solid #2e8b2e;}` &&
      `.pick{outline:2px dashed #d2691e;}` &&
      `.hint{color:#d2691e;font-weight:bold;}` &&
      `</style>` &&
      "drag with the mouse (hold and sweep, drop before the marked element);
      "a plain click still works as pick/insert
      `<script>var mk=null,mp=null,mt=null,mtk=null,mv=false;` &&
      `function md(e,k,p){mk=k;mp=p;mv=false;return true;}` &&
      `function mo(e,el,k,p){if(!mk||p!=mp||k==mk)return;mv=true;` &&
      `if(mt&&mt!==el){mt.style.boxShadow='';}mt=el;mtk=k;` &&
      `el.style.boxShadow='-4px 0 0 0 #d2691e';}` &&
      `function cl(e){if(mv){mv=false;if(e.preventDefault)e.preventDefault();return false;}return true;}` &&
      `document.onmouseup=function(){if(!mk)return;` &&
      `var k=mk,p=mp,t=mtk,ok=mv;mk=null;mp=null;mtk=null;` &&
      `if(mt){mt.style.boxShadow='';mt=null;}` &&
      `if(ok&&t){var f=document.getElementById('mf');f.action='SAPEVENT:'+p;` &&
      `document.getElementById('mv').value=k+'__'+t;f.submit();}};` &&
      `</script>` &&
      `</head><body onselectstart="return false">` &&
      `<form id="mf" method="post" action="SAPEVENT:fmv" style="display:none">` &&
      `<input type="hidden" name="mv" id="mv"></form>`.

    IF m_pick IS NOT INITIAL.
      l_html = l_html &&
        |<div class="hint">moving <b>{ m_pick }</b> - click the element to insert it before | &&
        |(same element = cancel)</div>|.
    ENDIF.

    "selected fields in SELECT order: color shows the real source table
    lt_sel = VALUE #( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
    SORT lt_sel BY pos.
    l_html = l_html && `<div class="tabhdr">SELECT&nbsp;&nbsp;`.
    LOOP AT mt_jtabs INTO DATA(ls_order_tab).
      DATA(l_ord_alias) = condense( CONV string( ls_order_tab-alias ) ).
      DATA(l_ord_color_idx) = sy-tabix - 1.
      l_ord_color_idx = l_ord_color_idx MOD 6.
      l_ord_color_idx = l_ord_color_idx + 1.
      DATA(l_ord_color) = |c{ l_ord_color_idx }|.
      DATA(l_ord_pick) = COND string( WHEN m_pick = l_ord_alias THEN ' pick' ).
      l_html = l_html &&
        |<a class="tblpill { l_ord_color }{ l_ord_pick }" href="SAPEVENT:fpick?{ l_ord_alias }"| &&
        | draggable="false" ondragstart="return false"| &&
        | onmousedown="return md(event,'{ l_ord_alias }','fgmv')"| &&
        | onmouseover="mo(event,this,'{ l_ord_alias }','fgmv')" onclick="return cl(event)">| &&
        |{ l_ord_alias }</a>|.
    ENDLOOP.
    l_html = l_html &&
      `<a class="zone" href="SAPEVENT:fpick?END_T"` &&
      ` onmouseover="mo(event,this,'END_T','fgmv')" onclick="return cl(event)">&#8677; tables end</a></div>`.

    LOOP AT lt_sel INTO DATA(ls_sel).
      DATA(l_alias) = condense( CONV string( ls_sel-alias ) ).
      READ TABLE mt_jtabs TRANSPORTING NO FIELDS WITH KEY alias = ls_sel-alias.
      DATA(l_color_idx) = sy-tabix - 1.
      l_color_idx = l_color_idx MOD 6.
      l_color_idx = l_color_idx + 1.
      DATA(l_color) = |c{ l_color_idx }|.
      DATA(l_fkey) = |{ l_alias }~{ ls_sel-fieldname }|.
      DATA(l_cls) = |chip on { l_color }|.
      IF ls_sel-keyflag = abap_true.
        l_cls = l_cls && ' key'.
      ENDIF.
      DATA(l_label) = COND string(
        WHEN m_show_texts = abap_true AND ls_sel-ddtext IS NOT INITIAL
        THEN escape( val = ls_sel-ddtext format = cl_abap_format=>e_html_text )
        ELSE |{ ls_sel-fieldname }| ).
      IF m_pick = l_fkey.
        l_cls = l_cls && ' pick'.
      ENDIF.
      l_html = l_html &&
        |<a class="{ l_cls }" href="SAPEVENT:fpick?{ l_fkey }"| &&
        | draggable="false" ondragstart="return false"| &&
        | onmousedown="return md(event,'{ l_fkey }','fmv')"| &&
        | onmouseover="mo(event,this,'{ l_fkey }','fmv')" onclick="return cl(event)"| &&
        | title="{ l_fkey } { escape( val = ls_sel-ddtext format = cl_abap_format=>e_html_attr ) }">| &&
        |{ l_label }</a>|.
    ENDLOOP.
    l_html = l_html &&
      `<a class="zone" href="SAPEVENT:fpick?END_F"` &&
      ` onmouseover="mo(event,this,'END_F','fmv')" onclick="return cl(event)">&#8677; end</a>`.

    l_html = l_html && `</body></html>`.
    show_html( io_html = mo_flds_html i_html = l_html ).
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
    "visual swap in the join order; the base table takes part via m_base_pos
    READ TABLE mt_jtabs INTO DATA(ls_tab) WITH KEY alias = i_alias.
    CHECK sy-subrc = 0.
    DATA(l_idx) = sy-tabix.
    DATA(l_other_idx) = l_idx + i_dir.
    CHECK l_other_idx >= 1 AND l_other_idx <= lines( mt_jtabs ).
    READ TABLE mt_jtabs INTO DATA(ls_other) INDEX l_other_idx.

    IF ls_tab-tabname = m_tabname. "moving the base itself
      m_base_pos = m_base_pos + i_dir.
      RETURN.
    ENDIF.
    IF ls_other-tabname = m_tabname. "candidate crosses the base
      m_base_pos = m_base_pos - i_dir.
      RETURN.
    ENDIF.

    "two candidates: swap their selection order
    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = ls_tab-tabname.
    CHECK sy-subrc = 0.
    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<other>) WITH KEY tabname = ls_other-tabname.
    CHECK sy-subrc = 0.
    DATA(l_tmp) = <cand>-sel_order.
    <cand>-sel_order  = <other>-sel_order.
    <other>-sel_order = l_tmp.
  ENDMETHOD.


  METHOD move_table_before.
    "drag&drop: place table i_from before i_to ('#END' = last position)
    DATA lt_order TYPE TABLE OF tabname.

    LOOP AT mt_jtabs INTO DATA(ls_tab).
      IF condense( CONV string( ls_tab-alias ) ) = i_from.
        DATA(l_from_tab) = ls_tab-tabname.
      ELSE.
        APPEND ls_tab-tabname TO lt_order.
      ENDIF.
    ENDLOOP.
    CHECK l_from_tab IS NOT INITIAL.

    IF i_to = '#END'.
      APPEND l_from_tab TO lt_order.
    ELSE.
      READ TABLE mt_jtabs INTO ls_tab WITH KEY alias = i_to.
      CHECK sy-subrc = 0.
      READ TABLE lt_order TRANSPORTING NO FIELDS WITH KEY table_line = ls_tab-tabname.
      IF sy-subrc = 0.
        INSERT l_from_tab INTO lt_order INDEX sy-tabix.
      ELSE.
        APPEND l_from_tab TO lt_order.
      ENDIF.
    ENDIF.

    "write the new order back: base position + candidate selection order
    DATA(l_ord) = 0.
    LOOP AT lt_order INTO DATA(l_tabname).
      IF l_tabname = m_tabname.
        m_base_pos = sy-tabix.
        CONTINUE.
      ENDIF.
      ADD 1 TO l_ord.
      READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = l_tabname.
      IF sy-subrc = 0.
        <cand>-sel_order = l_ord.
      ENDIF.
    ENDLOOP.
    m_sel_count = l_ord.

    rebuild_selection( ).
  ENDMETHOD.


  METHOD move_field_before.
    "drag&drop: place field i_from (ALIAS~FIELD) before i_to ('#END' = last)
    DATA lt_keys TYPE TABLE OF string.

    DATA(lt_sel) = VALUE tt_jfld( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
    SORT lt_sel BY pos.
    LOOP AT lt_sel INTO DATA(ls_sel).
      DATA(l_key) = |{ condense( CONV string( ls_sel-alias ) ) }~{ ls_sel-fieldname }|.
      IF l_key NE i_from.
        APPEND l_key TO lt_keys.
      ENDIF.
    ENDLOOP.

    IF i_to = '#END'.
      APPEND i_from TO lt_keys.
    ELSE.
      READ TABLE lt_keys TRANSPORTING NO FIELDS WITH KEY table_line = i_to.
      IF sy-subrc = 0.
        INSERT i_from INTO lt_keys INDEX sy-tabix.
      ELSE.
        APPEND i_from TO lt_keys.
      ENDIF.
    ENDIF.

    LOOP AT lt_keys INTO l_key.
      DATA(l_pos) = sy-tabix. "READ TABLE below overwrites sy-tabix!
      SPLIT l_key AT '~' INTO DATA(l_alias) DATA(l_field).
      READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) WITH KEY alias = l_alias fieldname = l_field.
      IF sy-subrc = 0.
        <fld>-pos = l_pos.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD move_alias_fields_before.
    "drag&drop: place all selected fields of alias i_from before alias i_to
    DATA: lt_keys TYPE TABLE OF string,
          lt_move TYPE TABLE OF string,
          l_split_alias TYPE string,
          l_split_field TYPE string.

    DATA(lt_sel) = VALUE tt_jfld( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
    SORT lt_sel BY pos.
    LOOP AT lt_sel INTO DATA(ls_sel).
      DATA(l_alias) = condense( CONV string( ls_sel-alias ) ).
      DATA(l_key) = |{ l_alias }~{ ls_sel-fieldname }|.
      IF l_alias = i_from.
        APPEND l_key TO lt_move.
      ELSE.
        APPEND l_key TO lt_keys.
      ENDIF.
    ENDLOOP.
    CHECK lt_move IS NOT INITIAL.

    IF i_to = '#END'.
      APPEND LINES OF lt_move TO lt_keys.
    ELSE.
      DATA(l_inserted) = abap_false.
      LOOP AT lt_keys INTO l_key.
        SPLIT l_key AT '~' INTO l_split_alias l_split_field.
        IF l_split_alias = i_to.
          INSERT LINES OF lt_move INTO lt_keys INDEX sy-tabix.
          l_inserted = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF l_inserted = abap_false.
        APPEND LINES OF lt_move TO lt_keys.
      ENDIF.
    ENDIF.

    LOOP AT lt_keys INTO l_key.
      DATA(l_pos) = sy-tabix. "READ TABLE below overwrites sy-tabix!
      SPLIT l_key AT '~' INTO l_split_alias l_split_field.
      READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) WITH KEY alias = l_split_alias fieldname = l_split_field.
      IF sy-subrc = 0.
        <fld>-pos = l_pos.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_fld_action.
    CASE i_act.
      WHEN 'ALL'.
        LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>).
          <fld>-sel = abap_true.
        ENDLOOP.
      WHEN 'NONE'.
        LOOP AT mt_jflds ASSIGNING <fld>.
          <fld>-sel = abap_false.
        ENDLOOP.
      WHEN 'KEYS'.
        LOOP AT mt_jflds ASSIGNING <fld>.
          <fld>-sel = <fld>-keyflag.
          CLEAR <fld>-pos.
        ENDLOOP.
      WHEN OTHERS.
        IF strlen( i_act ) > 4 AND i_act(4) = 'lng_'. "field name variant: lng_TECH / lng_<spras>
          IF i_act+4 = 'TECH'.
            m_show_texts = abap_false.
          ELSE.
            m_show_texts = abap_true.
            m_fld_lang = i_act+4.
            reload_field_texts( ).
          ENDIF.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'tg_'. "toggle single field: tg_ALIAS~FIELD
          SPLIT i_act+3 AT '~' INTO DATA(l_alias) DATA(l_field).
          READ TABLE mt_jflds ASSIGNING <fld> WITH KEY alias = l_alias fieldname = l_field.
          IF sy-subrc = 0.
            <fld>-sel = boolc( <fld>-sel = abap_false ).
            CLEAR <fld>-pos.
          ENDIF.
        ELSEIF strlen( i_act ) > 4.
          DATA(l_arg) = CONV char5( i_act+4 ).
          CASE i_act(4).
            WHEN 'all_'.
              LOOP AT mt_jflds ASSIGNING <fld> WHERE alias = l_arg.
                <fld>-sel = abap_true.
              ENDLOOP.
            WHEN 'non_'.
              LOOP AT mt_jflds ASSIGNING <fld> WHERE alias = l_arg.
                <fld>-sel = abap_false.
              ENDLOOP.
            WHEN 'key_'.
              LOOP AT mt_jflds ASSIGNING <fld> WHERE alias = l_arg.
                <fld>-sel = <fld>-keyflag.
              ENDLOOP.
          ENDCASE.
        ENDIF.
    ENDCASE.
    normalize_pos( ).
    render_html( ).
    render_flds( ).
    update_sql_view( ).
  ENDMETHOD.


  METHOD reload_field_texts.
    LOOP AT mt_jtabs INTO DATA(ls_tab).
      LOOP AT get_fieldlist( ls_tab-tabname ) INTO DATA(ls_f). "uses m_fld_lang
        READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>)
          WITH KEY alias = ls_tab-alias fieldname = ls_f-fieldname.
        IF sy-subrc = 0.
          <fld>-ddtext = ls_f-fieldtext.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
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
      WHEN 'tmv'. "drag&drop of tables: FROM__TO
        SPLIT getdata AT '__' INTO DATA(l_from) DATA(l_to).
        IF l_from IS NOT INITIAL AND l_to IS NOT INITIAL.
          move_table_before( i_from = l_from i_to = l_to ).
          refresh_all( ).
        ENDIF.
      WHEN 'fmv' OR 'fgmv'. "mouse drag posted as mv=FROM__TO (TO may be END_F/END_T)
        l_post = concat_lines_of( table = postdata ).
        SPLIT l_post AT '=' INTO l_dummy DATA(l_move).
        REPLACE ALL OCCURRENCES OF '+' IN l_move WITH ` `.
        l_move = cl_http_utility=>unescape_url( l_move ).
        SPLIT l_move AT '__' INTO l_from l_to.
        IF l_from IS NOT INITIAL AND l_to IS NOT INITIAL.
          IF l_to = 'END_F' OR l_to = 'END_T'.
            l_to = '#END'.
          ENDIF.
          IF action = 'fmv'.
            move_field_before( i_from = l_from i_to = l_to ).
          ELSE.
            move_alias_fields_before( i_from = l_from i_to = l_to ).
          ENDIF.
          CLEAR m_pick.
          render_flds( ).
          update_sql_view( ).
        ENDIF.
      WHEN 'fpick'. "click-to-move: 1st click picks, 2nd click inserts before the target
        DATA(l_key) = CONV string( getdata ).
        IF m_pick IS INITIAL.
          IF l_key NE 'END_F' AND l_key NE 'END_T'. "end zones cannot be picked up
            m_pick = l_key.
          ENDIF.
          render_flds( ).
        ELSEIF m_pick = l_key. "cancel
          CLEAR m_pick.
          render_flds( ).
        ELSE.
          DATA(l_pick_is_fld) = boolc( m_pick CS '~' ).
          DATA(l_key_is_fld)  = boolc( l_key CS '~' OR l_key = 'END_F' ).
          IF l_pick_is_fld NE l_key_is_fld. "field onto table pill etc: re-pick instead
            m_pick = COND #( WHEN l_key NE 'END_F' AND l_key NE 'END_T' THEN l_key ).
            render_flds( ).
            RETURN.
          ENDIF.
          IF l_pick_is_fld = abap_true.
            move_field_before( i_from = m_pick
                               i_to   = COND #( WHEN l_key = 'END_F' THEN '#END' ELSE l_key ) ).
          ELSE.
            move_alias_fields_before( i_from = m_pick
                                      i_to   = COND #( WHEN l_key = 'END_T' THEN '#END' ELSE l_key ) ).
          ENDIF.
          CLEAR m_pick.
          render_flds( ).
          update_sql_view( ).
        ENDIF.
      WHEN 'fld'.
        handle_fld_action( CONV #( getdata ) ).
      WHEN 'fsel' OR 'fsel0' OR 'fsel1'. "lasso: select/unselect painted fields (keys=T0~F1;T1~F2;...)
        DATA(l_lasso_sel) = boolc( action NE 'fsel0' ).
        l_post = concat_lines_of( table = postdata ).
        SPLIT l_post AT '=' INTO l_dummy DATA(l_keys).
        l_keys = cl_http_utility=>unescape_url( l_keys ).
        SPLIT l_keys AT ';' INTO TABLE DATA(lt_keys).
        LOOP AT lt_keys INTO DATA(l_fkey).
          SPLIT l_fkey AT '~' INTO DATA(l_alias) DATA(l_field).
          READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) WITH KEY alias = l_alias fieldname = l_field.
          IF sy-subrc = 0.
            <fld>-sel = l_lasso_sel.
          ENDIF.
        ENDLOOP.
        normalize_pos( ).
        render_html( ).
        render_flds( ).
        update_sql_view( ).
    ENDCASE.
  ENDMETHOD.


  METHOD create_sql_view.
    mo_low_split->get_container( EXPORTING row = 1 column = 1 RECEIVING container = DATA(lo_cont) ).

    CREATE OBJECT mo_sql_html
      EXPORTING
        parent = lo_cont
      EXCEPTIONS
        OTHERS = 1.
  ENDMETHOD.


  METHOD update_sql_view.
    CHECK mo_sql_html IS BOUND.
    DATA(l_sql) = generate_select( ).
    DATA(l_sql_html) = escape( val = l_sql format = cl_abap_format=>e_html_text ).
    REPLACE ALL OCCURRENCES OF REGEX '\bSELECT\b' IN l_sql_html WITH '<span class="kw">SELECT</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bFROM\b' IN l_sql_html WITH '<span class="kw">FROM</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bWHERE\b' IN l_sql_html WITH '<span class="kw">WHERE</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bJOIN\b' IN l_sql_html WITH '<span class="kw">JOIN</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bLEFT\b' IN l_sql_html WITH '<span class="kw">LEFT</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bOUTER\b' IN l_sql_html WITH '<span class="kw">OUTER</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bINNER\b' IN l_sql_html WITH '<span class="kw">INNER</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bON\b' IN l_sql_html WITH '<span class="kw">ON</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bAS\b' IN l_sql_html WITH '<span class="kw2">AS</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bAND\b' IN l_sql_html WITH '<span class="kw2">AND</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bUP TO\b' IN l_sql_html WITH '<span class="kw">UP TO</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bROWS\b' IN l_sql_html WITH '<span class="kw">ROWS</span>'.

    DATA(l_html) =
      `<html><head><meta charset="utf-8"><style>` &&
      `body{margin:0;background:#f6f8fa;font-family:Consolas,monospace;font-size:12px;}` &&
      `pre{margin:6px;white-space:pre-wrap;}` &&
      `.kw{color:#0033cc;font-weight:bold;}` &&
      `.kw2{color:#7a3db8;font-weight:bold;}` &&
      `</style></head><body><pre>` && l_sql_html && `</pre></body></html>`.
    show_html( io_html = mo_sql_html i_html = l_html ).

    "apply every change directly to the original window
    IF m_ready = abap_true AND viewer_alive( ) = abap_true.
      execute_sql( i_sql = l_sql ).
    ENDIF.
  ENDMETHOD.


  METHOD viewer_alive.
    rv_alive = boolc( mo_viewer IS BOUND AND line_exists( zcl_sde_appl=>mt_obj[ alv_viewer = mo_viewer ] ) ).
  ENDMETHOD.


  METHOD generate_select.
    DATA: l_fields TYPE string,
          l_from   TYPE string,
          lt_sel   TYPE tt_jfld.

    "single table: plain field names, so the window looks exactly like the original one
    DATA(l_multi) = boolc( lines( mt_jtabs ) > 1 ).

    lt_sel = VALUE #( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
    SORT lt_sel BY pos.
    LOOP AT lt_sel INTO DATA(ls_fld).
      IF l_fields IS NOT INITIAL.
        l_fields = |{ l_fields },{ cl_abap_char_utilities=>newline }       |.
      ENDIF.
      IF l_multi = abap_true.
        l_fields = |{ l_fields }{ ls_fld-alias CASE = LOWER }~{ ls_fld-fieldname CASE = LOWER } AS { ls_fld-alias CASE = LOWER }_{ ls_fld-fieldname CASE = LOWER }|.
      ELSE.
        l_fields = |{ l_fields }{ ls_fld-fieldname CASE = LOWER }|.
      ENDIF.
    ENDLOOP.
    IF l_fields IS INITIAL.
      l_fields = COND #( WHEN l_multi = abap_true THEN 't0~*' ELSE '*' ).
    ENDIF.

    LOOP AT mt_jtabs INTO DATA(ls_tab).
      IF sy-tabix = 1.
        IF l_multi = abap_true.
          l_from = |{ ls_tab-tabname CASE = LOWER } AS { ls_tab-alias CASE = LOWER }|.
        ELSE.
          l_from = |{ ls_tab-tabname CASE = LOWER }|.
        ENDIF.
      ELSE.
        l_from = |{ l_from }{ cl_abap_char_utilities=>newline }  { ls_tab-jtype } JOIN { ls_tab-tabname CASE = LOWER } AS { ls_tab-alias CASE = LOWER } ON { ls_tab-cond }|.
      ENDIF.
    ENDLOOP.

    rv_sql = |SELECT { l_fields }{ cl_abap_char_utilities=>newline }| &&
             |  FROM { l_from }|.

    "WHERE built from the panel ranges with the CURRENT aliases;
    "filters of removed joins are silently skipped
    DATA(l_where) = build_where( ).
    IF l_where IS NOT INITIAL.
      rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } WHERE { l_where }|.
    ENDIF.

    DATA(l_rows) = COND i( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 ).
    rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } UP TO { l_rows } ROWS|.
  ENDMETHOD.


  METHOD build_where.
    DATA: lt_where TYPE rsds_twhere,
          lt_range TYPE rsds_trange.

    CHECK mo_viewer->mo_sel IS BOUND.
    DATA(l_multi) = boolc( lines( mt_jtabs ) > 1 ).

    APPEND INITIAL LINE TO lt_range ASSIGNING FIELD-SYMBOL(<tabl>).
    <tabl>-tablename = m_tabname.
    LOOP AT mo_viewer->mo_sel->mt_sel_tab INTO DATA(ls_tab) WHERE range IS NOT INITIAL.
      "panel label (CARRID or T1_CONNID) -> alias + field
      DATA(l_label) = condense( CONV string( ls_tab-field_label ) ).
      DATA(l_alias) = `T0`.
      DATA(l_field) = l_label.
      FIND REGEX '^T\d+_' IN l_label MATCH LENGTH DATA(l_len).
      IF sy-subrc = 0.
        DATA(l_alias_len) = l_len - 1. "without the trailing '_'
        l_alias = l_label+0(l_alias_len).
        l_field = l_label+l_len.
      ENDIF.
      "only fields of tables currently in the join; stale filters are dropped
      READ TABLE mt_jflds TRANSPORTING NO FIELDS WITH KEY alias = l_alias fieldname = l_field.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      APPEND INITIAL LINE TO <tabl>-frange_t ASSIGNING FIELD-SYMBOL(<t_range>).
      <t_range>-fieldname = COND #( WHEN l_multi = abap_true
                                    THEN |{ to_lower( l_alias ) }~{ to_lower( l_field ) }|
                                    ELSE to_lower( l_field ) ).
      <t_range>-selopt_t  = ls_tab-range.
    ENDLOOP.
    CHECK <tabl>-frange_t IS NOT INITIAL.

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = lt_range
      IMPORTING
        where_clauses = lt_where.

    LOOP AT lt_where INTO DATA(ls_where) WHERE tablename = m_tabname.
      LOOP AT ls_where-where_tab INTO DATA(l_line).
        CONDENSE l_line-line.
        rv_where = |{ rv_where } { l_line-line }|.
      ENDLOOP.
    ENDLOOP.
    CONDENSE rv_where.
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
      IF i_quiet = abap_false.
        MESSAGE 'Statement must start with SELECT' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      RETURN.
    ENDIF.
    FIND REGEX '\sFROM\s' IN l_upper MATCH OFFSET DATA(l_from_off) MATCH LENGTH DATA(l_from_len).
    IF sy-subrc NE 0.
      IF i_quiet = abap_false.
        MESSAGE 'FROM clause not found' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
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
        IF i_quiet = abap_false.
          MESSAGE |Cannot parse field: { l_fld_str }| TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
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
          IF i_quiet = abap_false.
            MESSAGE |Unknown field { ls_alias-tabname }-{ l_field }| TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
          RETURN.
      ENDTRY.

      "keep names unique
      WHILE line_exists( lt_comp[ name = l_name ] ).
        l_name = |{ COND string( WHEN strlen( l_name ) > 28 THEN l_name+0(28) ELSE l_name ) }_{ sy-index }|.
      ENDWHILE.
      APPEND VALUE #( name = l_name type = lo_type ) TO lt_comp.
    ENDLOOP.

    IF lt_comp IS INITIAL.
      IF i_quiet = abap_false.
        MESSAGE 'No fields to select' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
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
        IF i_quiet = abap_false.
          MESSAGE lx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        RETURN.
    ENDTRY.

    DATA(l_view_name) = |JOIN { m_tabname } ({ lines( <result> ) })|.
    mo_viewer->rebind( ir_tab = lr_table i_name = l_view_name i_generic = abap_true ).
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
