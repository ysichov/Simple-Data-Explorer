CLASS zcl_sde_tools DEFINITION PUBLIC INHERITING FROM zcl_sde_popup CREATE PUBLIC.
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

           tt_jfld TYPE zif_sde_pivot_types=>tt_jfld. "shared with zcl_sde_pivot, see zif_sde_pivot_types

    METHODS: constructor IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer
                                   io_parent TYPE REF TO cl_gui_container OPTIONAL, "docked mode: build inside this container
      "called from the toolbars (dynamically, the viewer holds us as REF TO object)
      save_layout_dialog,
      load_layout_dialog,
      sort_by IMPORTING it_cols TYPE lvc_t_fnam i_desc TYPE abap_bool DEFAULT abap_false.

protected section.
  PRIVATE SECTION.
    TYPES: BEGIN OF t_domain_cache,
             domname TYPE domname,
             value   TYPE string,
             text    TYPE string,
           END OF t_domain_cache.

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
          m_mode       TYPE char1,                      "active tool: ' ' none, J join, P pivot
          mo_pivot     TYPE REF TO zcl_sde_pivot,
          m_pick       TYPE string,                     "click-to-move: picked field key or table alias
          m_sql_edit   TYPE abap_bool,                  "SQL panel in manual edit mode
          m_sql_manual TYPE string,                     "manually edited statement
          m_order      TYPE string,                      "ORDER BY set from the sort buttons
          m_loading    TYPE abap_bool,                   "a layout is being applied: do not re-cache filters
          m_show_texts TYPE abap_bool,                   "field chips: texts instead of tech names
          m_fld_lang   TYPE spras,                       "language of the field texts
          m_layout     TYPE char1,                       "splitter geometry currently applied (' ' or P)
          mt_where_sel TYPE TABLE OF zcl_sde_appl=>selection_display_s, "ranges before pivot rebind
          mt_domain_cache TYPE TABLE OF t_domain_cache, "fixed-value texts already looked up
          mt_pivot_sort TYPE abap_sortorder_tab. "sort of the matrix - the grouped SQL has no column to sort by

    METHODS:
      find_candidates,
      discover_for IMPORTING i_tabname TYPE tabname,
      get_fieldlist IMPORTING i_tabname       TYPE tabname
                    RETURNING VALUE(rt_dfies) TYPE ddfields,
      render_html,
      render_flds,
      apply_layout,
      pivot_src_fields RETURNING VALUE(rt_fields) TYPE tt_jfld,
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
      build_on_picker IMPORTING i_alias        TYPE string
                                it_prev        TYPE string_table
                                i_input_id     TYPE string
                      RETURNING VALUE(rv_html) TYPE string,
      reload_field_texts,
      fill_sel_extras,
      drop_stale_order,
      cfg_dir RETURNING VALUE(rv_dir) TYPE string,
      cfg_name IMPORTING i_name        TYPE string
               RETURNING VALUE(rv_file) TYPE string,
      save_config IMPORTING i_name TYPE string,
      load_config IMPORTING i_name TYPE string,
      open_layout_for IMPORTING i_base TYPE tabname
                                i_file TYPE string,
      apply_postdata IMPORTING it_postdata TYPE cnht_post_data_tab RETURNING VALUE(rv_act) TYPE string,
      create_sql_view,
      update_sql_view,
      refresh_all,
      generate_select RETURNING VALUE(rv_sql) TYPE string,
      execute_pivot,
      key_type IMPORTING i_key          TYPE string
                         i_agg          TYPE string OPTIONAL
               RETURNING VALUE(ro_type) TYPE REF TO cl_abap_datadescr,
      value_text IMPORTING i_row          TYPE any
                           it_comps       TYPE string_table
                 RETURNING VALUE(rv_text) TYPE string,
      value_label IMPORTING i_row          TYPE any
                            it_comps       TYPE string_table
                            it_cols        TYPE zcl_sde_pivot=>tt_sqlcols
                  RETURNING VALUE(rv_text) TYPE string,
      field_label IMPORTING i_key          TYPE string
                            i_raw          TYPE string
                  RETURNING VALUE(rv_text) TYPE string,
      field_header IMPORTING i_key          TYPE string
                   RETURNING VALUE(rv_text) TYPE string,
      domain_text IMPORTING i_domname      TYPE domname
                            i_value        TYPE string
                  RETURNING VALUE(rv_text) TYPE string,
      sync_sel_panel,
      build_from RETURNING VALUE(rv_from) TYPE string,
      is_multi RETURNING VALUE(rv_multi) TYPE abap_bool,
      cache_where_selection,
      build_where RETURNING VALUE(rv_where) TYPE string,
      execute_sql IMPORTING i_sql TYPE string i_quiet TYPE abap_bool DEFAULT abap_false,
      fcat_entry IMPORTING i_fieldname   TYPE lvc_fname
                           io_type       TYPE REF TO cl_abap_datadescr
                           i_text        TYPE string
                 RETURNING VALUE(rs_cat) TYPE lvc_s_fcat,
      viewer_alive RETURNING VALUE(rv_alive) TYPE abap_bool,
      on_viewer_sel FOR EVENT selection_done OF zcl_sde_sel_opt,
      upper_outside_quotes IMPORTING i_sql         TYPE string
                           RETURNING VALUE(rv_sql) TYPE string,
      format_sql IMPORTING i_sql         TYPE string
                 RETURNING VALUE(rv_sql) TYPE string,

      on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
        IMPORTING action getdata postdata.
ENDCLASS.



CLASS ZCL_SDE_TOOLS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_viewer = io_viewer.
    m_tabname = io_viewer->m_tabname.
    m_fld_lang = sy-langu.

    DATA lo_parent TYPE REF TO cl_gui_container.
    IF io_parent IS BOUND. "docked below the data area of the viewer window
      lo_parent = io_parent.
    ELSE. "standalone popup
      mo_box = create( i_width = 1000 i_hight = 400 ).
      mo_box->set_caption( |Tools: { m_tabname }| ).
      SET HANDLER on_box_close FOR mo_box.
      lo_parent = mo_box.
    ENDIF.

    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = lo_parent
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.
    mo_splitter->set_row_mode( mode = mo_splitter->mode_relative ).
    mo_splitter->set_row_height( id = 1 height = 22 ).

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
    cache_where_selection( ).
    refresh_all( ).
    m_ready = abap_true. "from now on every change is applied to the original window
    update_sql_view( ).

    "react on filter changes in the original window: SQL text + data follow
    IF mo_viewer->mo_sel IS BOUND.
      SET HANDLER on_viewer_sel FOR mo_viewer->mo_sel.
    ENDIF.
  ENDMETHOD.

  METHOD on_viewer_sel.
    cache_where_selection( ).
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
    apply_layout( ).
    render_html( ).
    render_flds( ).
    update_sql_view( ).
  ENDMETHOD.


  METHOD apply_layout.
    "pivot mode: the builder needs the canvas, and the SELECT list it would push
    "aside belongs to the join - so the statement takes the whole lower half
    DATA(l_want) = COND char1( WHEN m_mode = 'P' THEN 'P' ELSE space ).
    CHECK l_want NE m_layout AND mo_splitter IS BOUND AND mo_low_split IS BOUND.
    m_layout = l_want.

    IF l_want = 'P'.
      mo_splitter->set_row_height( id = 1 height = 62 ).
      mo_low_split->set_column_width( id = 1 width = 100 ).
    ELSE.
      mo_splitter->set_row_height( id = 1 height = 22 ).
      mo_low_split->set_column_width( id = 1 width = 45 ).
    ENDIF.
  ENDMETHOD.


  METHOD pivot_src_fields.
    "the pivot source is what the join selected, not every field of every table
    rt_fields = VALUE #( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
    SORT rt_fields BY pos.
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
                      ddtext = ls_f-fieldtext datatype = ls_f-datatype inttype = ls_f-inttype
                      domname = ls_f-domname ) TO mt_jflds.
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

      IF lt_pairs IS INITIAL.
        "no foreign key at all: an empty ON dumps the statement right away, so
        "link on the first field name the two tables have in common - keys first
        DATA(l_anchor) = COND tabname( WHEN l_parent_missing = abap_true OR ls_cand-parent IS INITIAL
                                       THEN m_tabname ELSE ls_cand-parent ).
        DATA(lt_anchor_flds) = get_fieldlist( l_anchor ).
        DATA(lt_cand_flds)   = get_fieldlist( ls_cand-tabname ).
        LOOP AT lt_cand_flds INTO DATA(ls_cf) WHERE keyflag = abap_true.
          IF line_exists( lt_anchor_flds[ fieldname = ls_cf-fieldname ] ).
            APPEND VALUE #( cand_field = ls_cf-fieldname base_field = ls_cf-fieldname ) TO lt_pairs.
          ENDIF.
        ENDLOOP.
        IF lt_pairs IS INITIAL. "the other way round: a key of the anchor table
          LOOP AT lt_cand_flds INTO ls_cf WHERE fieldname NE 'MANDT'.
            IF line_exists( lt_anchor_flds[ fieldname = ls_cf-fieldname keyflag = abap_true ] ).
              APPEND VALUE #( cand_field = ls_cf-fieldname base_field = ls_cf-fieldname ) TO lt_pairs.
            ENDIF.
          ENDLOOP.
        ENDIF.
        "no key matches at all: leave it empty rather than invent a link on a
        "random common column - the panel asks for the condition instead
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
                        ddtext = ls_f-fieldtext datatype = ls_f-datatype inttype = ls_f-inttype
                        domname = ls_f-domname
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

    "the tool bar looks the same in every mode: base table + the two tools
    DATA(l_head) =
      |<span class="base">{ m_tabname }</span>| &&
      |<a class="card{ COND string( WHEN m_mode = 'J' THEN ' sel' ) }" href="SAPEVENT:mode?J">&#128279; Join</a>| &&
      |<a class="card{ COND string( WHEN m_mode = 'P' THEN ' sel' ) }" href="SAPEVENT:mode?P">&#8862; Pivot table</a>|.

    IF m_mode = 'P' AND mo_pivot IS BOUND. "the pivot builder gets the whole canvas
      DATA(lt_pvsrc) = pivot_src_fields( ).
      mo_pivot->normalize_aggs( lt_pvsrc ).
      show_html( io_html = mo_html
                 i_html  = mo_pivot->render_panel( it_fields    = lt_pvsrc
                                                   i_show_texts = m_show_texts
                                                   i_header     = l_head ) ).
      RETURN.
    ENDIF.

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
      `.onp{margin-top:2px;font-size:10px;}` &&
      `.onp select{max-width:170px;}` &&
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
      "ON condition helpers: append a ready pair / a hand-picked pair, or clear
      `function oadd(id,t){if(!t)return;var i=document.getElementById(id);` &&
      `var v=i.value.replace(/^\s+|\s+$/g,'');` &&
      `i.value=v?v+' AND '+t:t;i.form.submit();}` &&
      "picking a ready pair replaces the condition - repeating the choice must
      "not pile up another AND term on top of the previous one
      `function oq(s,id){var t=s.value;s.selectedIndex=0;if(!t)return;` &&
      `var i=document.getElementById(id);i.value=t;i.form.submit();}` &&
      `function op(id,a,b){var l=document.getElementById(a),r=document.getElementById(b);` &&
      `if(!l.value||!r.value)return;oadd(id,l.value+' = '+r.value);}` &&
      `function ocl(id){var i=document.getElementById(id);i.value='';i.form.submit();}` &&
      `</script>` &&
      `</head><body>` &&
      `<form id="pf" method="post" action="SAPEVENT:fsel" style="display:none">` &&
      `<input type="hidden" name="keys" id="pk"></form>` &&
      l_head.

    IF m_mode = 'J'. "join tool: candidate canvas + join configuration
    l_html = l_html && ` &#8646; `.
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

    "saving/loading the layout lives on the selection panel toolbar, not here

    "joined tables: order (the first one is FROM), join type, ON condition
    IF lines( mt_jtabs ) > 1.
      l_html = l_html && `<form method="post" action="SAPEVENT:tabs"><table class="j">`.
      DATA lt_prev TYPE string_table. "aliases rendered so far (join order)
      CLEAR lt_prev.
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
          DATA(l_oid) = |oi_{ l_key }|. "id of this row's ON input, used by the helper controls
          l_html = l_html &&
            |<td><select name="jt_{ l_key }" onchange="this.form.submit()">| &&
            |<option{ l_inner }>INNER</option><option{ l_left }>LEFT OUTER</option></select> JOIN ON</td>| &&
            |<td><input class="cond" type="text" id="{ l_oid }" name="on_{ l_key }" onchange="this.form.submit()" value="{
               escape( val = CONV string( ls_tab-cond ) format = cl_abap_format=>e_html_attr ) }">| &&
            build_on_picker( i_alias = l_key it_prev = lt_prev i_input_id = l_oid ) &&
            |</td></tr>|.
        ENDIF.
        APPEND l_key TO lt_prev. "only tables already joined may appear on the right side of ON
      ENDLOOP.
      l_html = l_html &&
        `</table><button class="btn" type="submit" name="act" value="apply">Apply</button></form>`.
    ENDIF.
    ENDIF. "m_mode = 'J'

    IF m_mode NE 'P'. "in pivot mode the field zones live in the right panel
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
    ENDIF. "m_mode NE 'P'

    l_html = l_html && `</body></html>`.
    show_html( io_html = mo_html i_html = l_html ).
  ENDMETHOD.


  METHOD render_flds.
    DATA lt_sel TYPE tt_jfld.

    CHECK mo_flds_html IS BOUND.

    IF m_mode = 'P'. "the SELECT list belongs to the join: collapsed while pivoting
      show_html( io_html = mo_flds_html
                 i_html  = `<html><head><meta charset="utf-8"></head>` &&
                           `<body style="font-family:Arial;font-size:11px;color:#888;margin:4px">` &&
                           `The SELECT list is edited in the join tool.</body></html>` ).
      RETURN.
    ENDIF.

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


  METHOD build_on_picker.
    "helper controls under an ON condition: ready-made same-name pairs and a
    "free field-to-field picker, so the condition never has to be typed by hand
    DATA: l_same TYPE string, "options of the "same name" listbox
          l_left TYPE string, "fields of the joined table
          l_right TYPE string. "fields of the tables joined before it

    CHECK it_prev IS NOT INITIAL.
    DATA(l_lo) = to_lower( i_alias ).

    LOOP AT mt_jflds INTO DATA(ls_fld) WHERE alias = i_alias.
      DATA(l_qual) = |{ l_lo }~{ to_lower( ls_fld-fieldname ) }|.
      l_left = l_left && |<option value="{ l_qual }">{ ls_fld-fieldname }</option>|.
      "the same field name in an already joined table is the usual link
      LOOP AT it_prev INTO DATA(l_prev).
        READ TABLE mt_jflds TRANSPORTING NO FIELDS
          WITH KEY alias = l_prev fieldname = ls_fld-fieldname.
        CHECK sy-subrc = 0.
        DATA(l_pair) = |{ l_qual } = { to_lower( l_prev ) }~{ to_lower( ls_fld-fieldname ) }|.
        DATA(l_mark) = COND string( WHEN ls_fld-keyflag = abap_true THEN ` (key)` ).
        l_same = l_same && |<option value="{ l_pair }">{ l_pair }{ l_mark }</option>|.
      ENDLOOP.
    ENDLOOP.

    LOOP AT it_prev INTO l_prev.
      l_right = l_right && |<optgroup label="{ l_prev }">|.
      LOOP AT mt_jflds INTO ls_fld WHERE alias = l_prev.
        l_right = l_right &&
          |<option value="{ to_lower( l_prev ) }~{ to_lower( ls_fld-fieldname ) }">{ ls_fld-fieldname }</option>|.
      ENDLOOP.
      l_right = l_right && `</optgroup>`.
    ENDLOOP.

    rv_html = |<div class="onp">|.
    IF l_same IS NOT INITIAL.
      rv_html = rv_html &&
        |<select onchange="oq(this,'{ i_input_id }')">| &&
        |<option value="">&#43; same name&hellip;</option>{ l_same }</select> |.
    ENDIF.
    "empty first entry: the listboxes build a new pair, they do not mirror the
    "condition above - without it the browser shows the first field as "selected"
    rv_html = rv_html &&
      |<select id="ol_{ i_alias }"><option value="">&#8212; field &#8212;</option>{ l_left }</select> = | &&
      |<select id="or_{ i_alias }"><option value="">&#8212; field &#8212;</option>{ l_right }</select> | &&
      |<button class="btn" type="button" onclick="op('{ i_input_id }','ol_{ i_alias }','or_{ i_alias }')">&#43; add</button> | &&
      |<a class="act" href="#" onclick="ocl('{ i_input_id }');return false;">clear</a></div>|.
  ENDMETHOD.


  METHOD cfg_dir.
    "folder from the selection screen; empty falls back to a fixed temp folder
    rv_dir = condense( zcl_sde_appl=>gv_path ).
    IF rv_dir IS INITIAL.
      rv_dir = 'C:\temp\sde\'.
    ENDIF.
    IF substring( val = rv_dir off = strlen( rv_dir ) - 1 len = 1 ) NE '\'.
      rv_dir = |{ rv_dir }\\|.
    ENDIF.
  ENDMETHOD.


  METHOD drop_stale_order.
    "terms of tables that left the join would fail as "table T2 is unknown"
    CHECK m_order IS NOT INITIAL.
    DATA(l_kept) = ``.
    SPLIT m_order AT ',' INTO TABLE DATA(lt_terms).
    LOOP AT lt_terms INTO DATA(l_term).
      DATA(l_trim) = condense( l_term ).
      CHECK l_trim IS NOT INITIAL.
      FIND REGEX '^(\w+)~' IN l_trim SUBMATCHES DATA(l_alias).
      IF sy-subrc = 0 AND NOT line_exists( mt_jtabs[ alias = to_upper( l_alias ) ] ).
        CONTINUE.
      ENDIF.
      IF l_kept IS NOT INITIAL.
        l_kept = |{ l_kept }, |.
      ENDIF.
      l_kept = |{ l_kept }{ l_trim }|.
    ENDLOOP.
    m_order = l_kept.
  ENDMETHOD.


  METHOD sort_by.
    "the sort buttons of the data grid drive an explicit order instead of a
    "frontend sort: sorting only the fetched rows would sort the wrong 500 rows.
    "For the plain join this becomes ORDER BY text, parsed back out of the
    "displayed statement by execute_sql. The pivot's grouped statement has no
    "SQL column that corresponds to a matrix column, so the transposed result
    "is sorted directly instead - the grid already shows its real fieldnames.
    IF m_mode = 'P'.
      CLEAR mt_pivot_sort.
      LOOP AT it_cols INTO DATA(l_pcol).
        DATA(l_pname) = condense( CONV string( l_pcol ) ).
        CHECK l_pname IS NOT INITIAL.
        APPEND VALUE #( name = to_upper( l_pname ) descending = i_desc ) TO mt_pivot_sort.
      ENDLOOP.
      update_sql_view( ).
      RETURN.
    ENDIF.

    CLEAR m_order.
    DATA(l_dir) = COND string( WHEN i_desc = abap_true THEN ` DESCENDING` ELSE `` ).
    DATA(l_multi) = is_multi( ).

    LOOP AT it_cols INTO DATA(l_col).
      DATA(l_name) = condense( CONV string( l_col ) ).
      CHECK l_name IS NOT INITIAL.
      "grid column T1_STATUS belongs to alias T1, field STATUS
      DATA(l_sql_fld) = to_lower( l_name ).
      IF l_multi = abap_true.
        FIND REGEX '^(T\d+)_(.+)$' IN l_name SUBMATCHES DATA(l_alias) DATA(l_field).
        IF sy-subrc = 0 AND line_exists( mt_jflds[ alias = l_alias fieldname = l_field ] ).
          l_sql_fld = |{ to_lower( l_alias ) }~{ to_lower( l_field ) }|.
        ENDIF.
      ENDIF.
      IF m_order IS NOT INITIAL.
        m_order = |{ m_order }, |.
      ENDIF.
      m_order = |{ m_order }{ l_sql_fld }{ l_dir }|.
    ENDLOOP.

    update_sql_view( ).
  ENDMETHOD.


  METHOD save_layout_dialog.
    DATA: l_file   TYPE string,
          l_path   TYPE string,
          l_full   TYPE string,
          l_action TYPE i.

    DATA(l_def) = condense( CONV string( m_tabname ) ).
    REPLACE ALL OCCURRENCES OF '/' IN l_def WITH '_'.
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING  window_title         = 'Save join layout'
                 default_extension    = 'sdj'
                 default_file_name    = |{ l_def }.sdj|
                 initial_directory    = cfg_dir( )
                 file_filter          = 'Join layouts (*.sdj)|*.sdj|All files (*.*)|*.*|'
      CHANGING   filename             = l_file
                 path                 = l_path
                 fullpath             = l_full
                 user_action          = l_action
      EXCEPTIONS OTHERS               = 1 ).
    CHECK sy-subrc = 0 AND l_action = cl_gui_frontend_services=>action_ok AND l_full IS NOT INITIAL.
    save_config( l_full ).
  ENDMETHOD.


  METHOD load_layout_dialog.
    DATA: lt_files  TYPE filetable,
          l_rc      TYPE i,
          l_action  TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING  window_title            = 'Load join layout'
                 default_extension       = 'sdj'
                 initial_directory       = cfg_dir( )
                 file_filter             = 'Join layouts (*.sdj)|*.sdj|All files (*.*)|*.*|'
                 multiselection          = abap_false
      CHANGING   file_table              = lt_files
                 rc                      = l_rc
                 user_action             = l_action
      EXCEPTIONS OTHERS                  = 1 ).
    CHECK sy-subrc = 0 AND l_action = cl_gui_frontend_services=>action_ok AND lt_files IS NOT INITIAL.
    load_config( CONV string( lt_files[ 1 ]-filename ) ).
  ENDMETHOD.


  METHOD cfg_name.
    "a layout name is a plain file name: namespace slashes would break the path
    DATA(l_name) = condense( i_name ).
    IF l_name IS INITIAL.
      l_name = m_tabname.
    ENDIF.
    IF l_name CA '\:'. "the file dialogs hand over a full path - take it as it is
      rv_file = l_name.
      RETURN.
    ENDIF.
    REPLACE ALL OCCURRENCES OF REGEX '[\\/:*?"<>|]' IN l_name WITH '_'.
    IF NOT l_name CP '*.sdj'.
      l_name = |{ l_name }.sdj|.
    ENDIF.
    rv_file = |{ cfg_dir( ) }{ l_name }|.
  ENDMETHOD.


  METHOD save_config.
    "tab separated text: readable in any editor, trivial to parse back
    DATA: lt_lines TYPE string_table,
          l_tab    TYPE c LENGTH 1.

    l_tab = cl_abap_char_utilities=>horizontal_tab.
    APPEND |SDE-JOIN{ l_tab }1| TO lt_lines.
    APPEND |BASE{ l_tab }{ m_tabname }| TO lt_lines.
    APPEND |BASEPOS{ l_tab }{ m_base_pos }| TO lt_lines.
    APPEND |MODE{ l_tab }{ m_mode }| TO lt_lines.
    IF m_order IS NOT INITIAL.
      APPEND |ORD{ l_tab }{ m_order }| TO lt_lines.
    ENDIF.
    LOOP AT mt_jtabs INTO DATA(ls_tab).
      APPEND |TAB{ l_tab }{ ls_tab-alias }{ l_tab }{ ls_tab-tabname }{ l_tab }{ ls_tab-jtype }{ l_tab }{ ls_tab-cond }|
        TO lt_lines.
    ENDLOOP.
    LOOP AT mt_jflds INTO DATA(ls_fld) WHERE sel = abap_true.
      APPEND |FLD{ l_tab }{ ls_fld-alias }{ l_tab }{ ls_fld-fieldname }{ l_tab }{ ls_fld-pos }| TO lt_lines.
    ENDLOOP.
    "the pivot cross: rows, columns, measures with their aggregate, matrix sort.
    "Written whatever the active tool is - switching to Join and back must not
    "cost the layout, and an old reader simply ignores the unknown line types
    IF mo_pivot IS BOUND.
      LOOP AT mo_pivot->get_rows( ) INTO DATA(l_prow).
        APPEND |PVR{ l_tab }{ l_prow }| TO lt_lines.
      ENDLOOP.
      LOOP AT mo_pivot->get_col_keys( ) INTO DATA(l_pcol).
        APPEND |PVC{ l_tab }{ l_pcol }| TO lt_lines.
      ENDLOOP.
      LOOP AT mo_pivot->get_vals( ) INTO DATA(ls_pval).
        APPEND |PVV{ l_tab }{ ls_pval-key }{ l_tab }{ ls_pval-agg }| TO lt_lines.
      ENDLOOP.
      LOOP AT mt_pivot_sort INTO DATA(ls_psort).
        APPEND |PVS{ l_tab }{ ls_psort-name }{ l_tab }{ ls_psort-descending }| TO lt_lines.
      ENDLOOP.
    ENDIF.
    "filters: the row values plus every line of a multi-range
    cache_where_selection( ). "pull what is currently typed in the selection panel
    LOOP AT mt_where_sel INTO DATA(ls_sel).
      CHECK ls_sel-low IS NOT INITIAL OR ls_sel-high IS NOT INITIAL
         OR ls_sel-sign IS NOT INITIAL OR ls_sel-range IS NOT INITIAL.
      APPEND |SEL{ l_tab }{ ls_sel-field_label }{ l_tab }{ ls_sel-sign }{ l_tab }{ ls_sel-opti }{
                 l_tab }{ ls_sel-low }{ l_tab }{ ls_sel-high }| TO lt_lines.
      LOOP AT ls_sel-range INTO DATA(ls_range).
        APPEND |SELR{ l_tab }{ ls_sel-field_label }{ l_tab }{ ls_range-sign }{ l_tab }{ ls_range-opti }{
                   l_tab }{ ls_range-low }{ l_tab }{ ls_range-high }| TO lt_lines.
      ENDLOOP.
    ENDLOOP.

    IF m_sql_edit = abap_true AND m_sql_manual IS NOT INITIAL.
      "one line: newlines would break the line-based format
      DATA(l_flat) = m_sql_manual.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf   IN l_flat WITH ` `.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_flat WITH ` `.
      APPEND |SQL{ l_tab }{ l_flat }| TO lt_lines.
    ENDIF.

    DATA(l_file) = cfg_name( i_name ).
    cl_gui_frontend_services=>gui_download(
      EXPORTING  filename = l_file
                 filetype = 'ASC'
      CHANGING   data_tab = lt_lines
      EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc = 0.
      MESSAGE |Layout saved: { l_file }| TYPE 'S'.
    ELSE.
      MESSAGE |Cannot write { l_file }| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD open_layout_for.
    "a saved layout always belongs to one table; loading it for a different
    "table means opening a window for THAT table, the same way every other
    "table switch in this app works (foreign-key double-click, "open" by name)
    IF zcl_sde_sql=>exist_table( i_base ) NE 1 AND zcl_sde_sql=>exist_view( i_base ) NE 1.
      MESSAGE |{ i_base } no longer exists - cannot open the layout's table| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = i_base.
    <obj>-alv_viewer->open_tools( i_visible = abap_true ).
    CHECK <obj>-alv_viewer->mo_tools IS BOUND.
    "mo_tools is REF TO object (the viewer must not depend on zcl_sde_tools at
    "activation) - the new window's own load_config now sees a matching BASE
    CALL METHOD <obj>-alv_viewer->mo_tools->('LOAD_CONFIG') EXPORTING i_name = i_file.
  ENDMETHOD.


  METHOD load_config.
    DATA: lt_lines TYPE string_table,
          lt_part  TYPE string_table.

    DATA(l_file) = cfg_name( i_name ).
    cl_gui_frontend_services=>gui_upload(
      EXPORTING  filename = l_file
                 filetype = 'ASC'
      CHANGING   data_tab = lt_lines
      EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc NE 0 OR lt_lines IS INITIAL.
      MESSAGE |Cannot read { l_file }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    "first pass: header check - a layout only fits the table it was built on
    TYPES: BEGIN OF t_saved_tab,
             alias   TYPE char5,
             tabname TYPE tabname,
             jtype   TYPE char10,
             cond    TYPE c LENGTH 255,
           END OF t_saved_tab,
           BEGIN OF t_saved_fld,
             alias     TYPE char5,
             fieldname TYPE fieldname,
             pos       TYPE i,
           END OF t_saved_fld.
    DATA: lt_stabs TYPE TABLE OF t_saved_tab,
          lt_sflds TYPE TABLE OF t_saved_fld,
          lt_wsel  TYPE TABLE OF zcl_sde_appl=>selection_display_s,
          lt_prows TYPE zcl_sde_pivot=>tt_keys,
          lt_pcols TYPE zcl_sde_pivot=>tt_keys,
          lt_pvals TYPE zcl_sde_pivot=>tt_vals,
          lt_psort TYPE abap_sortorder_tab,
          l_base   TYPE tabname,
          l_pos    TYPE i,
          l_mode   TYPE char1,
          l_sql    TYPE string.

    m_loading = abap_true. "every refresh from here on must not touch the filter cache
    CLEAR m_order. "a file without ORD means: no sorting
    LOOP AT lt_lines INTO DATA(l_line).
      CLEAR lt_part.
      SPLIT l_line AT cl_abap_char_utilities=>horizontal_tab INTO TABLE lt_part.
      CHECK lines( lt_part ) >= 2.
      CASE lt_part[ 1 ].
        WHEN 'BASE'.    l_base = lt_part[ 2 ].
        WHEN 'BASEPOS'. l_pos  = lt_part[ 2 ].
        WHEN 'MODE'.    l_mode = lt_part[ 2 ].
        WHEN 'SQL'.     l_sql  = lt_part[ 2 ].
        WHEN 'ORD'.     m_order = lt_part[ 2 ].
        WHEN 'TAB'.
          CHECK lines( lt_part ) >= 3.
          APPEND VALUE #( alias   = lt_part[ 2 ]
                          tabname = lt_part[ 3 ]
                          jtype   = COND #( WHEN lines( lt_part ) >= 4 THEN lt_part[ 4 ] )
                          cond    = COND #( WHEN lines( lt_part ) >= 5 THEN lt_part[ 5 ] ) ) TO lt_stabs.
        WHEN 'FLD'.
          CHECK lines( lt_part ) >= 4.
          APPEND VALUE #( alias = lt_part[ 2 ] fieldname = lt_part[ 3 ] pos = lt_part[ 4 ] ) TO lt_sflds.
        WHEN 'PVR'. APPEND lt_part[ 2 ] TO lt_prows. "pivot cross: row dimension
        WHEN 'PVC'. APPEND lt_part[ 2 ] TO lt_pcols. "column dimension
        WHEN 'PVV'. "measure: field and its aggregate
          CHECK lines( lt_part ) >= 3.
          APPEND VALUE #( key = lt_part[ 2 ] agg = lt_part[ 3 ] ) TO lt_pvals.
        WHEN 'PVS'. "sort of the matrix
          APPEND VALUE #( name       = lt_part[ 2 ]
                          descending = COND #( WHEN lines( lt_part ) >= 3 THEN lt_part[ 3 ] ) ) TO lt_psort.
        WHEN 'SEL' OR 'SELR'. "filter of one field / one line of a multi-range
          "an empty HIGH loses its trailing tab on the way through the file,
          "so everything past the field name is optional
          CHECK lines( lt_part ) >= 4.
          DATA(l_f_sign) = CONV string( lt_part[ 3 ] ).
          DATA(l_f_opti) = CONV string( lt_part[ 4 ] ).
          DATA(l_f_low)  = COND string( WHEN lines( lt_part ) >= 5 THEN lt_part[ 5 ] ).
          DATA(l_f_high) = COND string( WHEN lines( lt_part ) >= 6 THEN lt_part[ 6 ] ).
          READ TABLE lt_wsel ASSIGNING FIELD-SYMBOL(<wsel>) WITH KEY field_label = lt_part[ 2 ].
          IF sy-subrc NE 0.
            APPEND INITIAL LINE TO lt_wsel ASSIGNING <wsel>.
            <wsel>-field_label = lt_part[ 2 ].
          ENDIF.
          IF lt_part[ 1 ] = 'SEL'.
            <wsel>-sign = l_f_sign.
            <wsel>-opti = l_f_opti.
            <wsel>-low  = l_f_low.
            <wsel>-high = l_f_high.
          ELSE.
            APPEND VALUE #( sign = l_f_sign
                            opti = l_f_opti
                            low  = l_f_low
                            high = l_f_high ) TO <wsel>-range.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    IF l_base NE m_tabname.
      CLEAR m_loading.
      "wrong window for this layout: open the right one instead of refusing -
      "one window per table is how this app always switches tables (see the
      "foreign-key double-click in zcl_sde_plugins=>run_dictionary_key)
      open_layout_for( i_base = l_base i_file = l_file ).
      RETURN.
    ENDIF.

    "start from an empty join: aliases are handed out again in the saved order
    LOOP AT mt_cand ASSIGNING FIELD-SYMBOL(<cand>).
      CLEAR: <cand>-selected, <cand>-sel_order, <cand>-alias.
    ENDLOOP.
    CLEAR: m_sel_count, m_alias_count.

    LOOP AT lt_stabs INTO DATA(ls_stab) WHERE tabname NE m_tabname.
      add_candidate( ls_stab-tabname ). "selects it and rebuilds the join
      READ TABLE mt_cand ASSIGNING <cand> WITH KEY tabname = ls_stab-tabname.
      IF sy-subrc = 0.
        <cand>-alias = ls_stab-alias. "keep the alias the saved ON conditions refer to
      ENDIF.
    ENDLOOP.
    "aliases were overwritten: continue numbering above the highest one in use
    LOOP AT mt_cand INTO DATA(ls_cnt) WHERE alias IS NOT INITIAL.
      DATA(l_num) = CONV i( ls_cnt-alias+1 ).
      IF l_num > m_alias_count.
        m_alias_count = l_num.
      ENDIF.
    ENDLOOP.

    m_base_pos = COND #( WHEN l_pos > 0 THEN l_pos ELSE 1 ).
    rebuild_selection( ).

    LOOP AT mt_jtabs ASSIGNING FIELD-SYMBOL(<jtab>).
      READ TABLE lt_stabs INTO ls_stab WITH KEY alias = <jtab>-alias.
      CHECK sy-subrc = 0.
      IF ls_stab-jtype IS NOT INITIAL.
        <jtab>-jtype = ls_stab-jtype.
      ENDIF.
      <jtab>-cond = ls_stab-cond.
    ENDLOOP.

    LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>).
      CLEAR: <fld>-sel, <fld>-pos.
    ENDLOOP.
    LOOP AT lt_sflds INTO DATA(ls_sfld).
      READ TABLE mt_jflds ASSIGNING <fld>
        WITH KEY alias = ls_sfld-alias fieldname = ls_sfld-fieldname.
      CHECK sy-subrc = 0.
      <fld>-sel = abap_true.
      <fld>-pos = ls_sfld-pos.
    ENDLOOP.
    normalize_pos( ). "pos drives the SELECT order, mt_jflds keeps its per-table grouping

    "the pivot cross, if the file carries one
    IF lt_prows IS NOT INITIAL OR lt_pcols IS NOT INITIAL OR lt_pvals IS NOT INITIAL.
      IF mo_pivot IS NOT BOUND.
        mo_pivot = NEW #( ).
      ENDIF.
      mo_pivot->set_layout( it_rows = lt_prows it_cols = lt_pcols it_vals = lt_pvals ).
      mt_pivot_sort = lt_psort.
    ENDIF.

    IF l_mode IS NOT INITIAL.
      m_mode = l_mode.
      IF m_mode = 'P' AND mo_pivot IS NOT BOUND. "file from before the pivot was saved
        mo_pivot = NEW #( ).
      ENDIF.
    ENDIF.
    IF l_sql IS NOT INITIAL.
      m_sql_manual = l_sql.
    ENDIF.

    "build_where only looks at RANGE, so a row without SELR lines needs one
    LOOP AT lt_wsel ASSIGNING <wsel> WHERE range IS INITIAL AND sign IS NOT INITIAL.
      APPEND VALUE #( sign = <wsel>-sign opti = <wsel>-opti
                      low  = <wsel>-low  high = <wsel>-high ) TO <wsel>-range.
    ENDLOOP.

    "filters: into the cache AND straight into the panel rows - a refresh of the
    "panel during the rebuild would otherwise re-cache them away as empty
    mt_where_sel = lt_wsel.
    IF mo_viewer->mo_sel IS BOUND.
      LOOP AT mo_viewer->mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<panel>).
        DATA(l_lbl) = condense( CONV string( <panel>-field_label ) ).
        READ TABLE lt_wsel INTO DATA(ls_wsel) WITH KEY field_label = <panel>-field_label.
        IF sy-subrc NE 0. "the base table is listed as CARRID or as T0_CARRID
          DATA l_wsel_alt TYPE lvc_fname.
          FIND REGEX '^T\d+_' IN l_lbl MATCH LENGTH DATA(l_wsel_pfx).
          IF sy-subrc = 0.
            l_wsel_alt = l_lbl+l_wsel_pfx.
          ELSE.
            l_wsel_alt = |T0_{ l_lbl }|.
          ENDIF.
          READ TABLE lt_wsel INTO ls_wsel WITH KEY field_label = l_wsel_alt.
        ENDIF.
        CHECK sy-subrc = 0.
        <panel>-low   = ls_wsel-low.
        <panel>-high  = ls_wsel-high.
        <panel>-sign  = ls_wsel-sign.
        <panel>-opti  = ls_wsel-opti.
        <panel>-range = ls_wsel-range.
        mo_viewer->mo_sel->update_sel_row( CHANGING c_sel_row = <panel> ).
      ENDLOOP.
      Zcl_SDE_common=>refresh( mo_viewer->mo_sel->mo_sel_alv ).
    ENDIF.

    refresh_all( ).
    CLEAR m_loading. "from now on panel edits are cached again as usual
    MESSAGE |Layout loaded: { l_file }| TYPE 'S'.
  ENDMETHOD.


  METHOD fill_sel_extras.
    "every field of every joined table, in the label form the selection panel
    "uses (T1_CONNID). Fields already in the SELECT are skipped there, so the
    "panel keeps them once and only gains the ones left out of the field list.
    CHECK mo_viewer->mo_sel IS BOUND.
    CLEAR mo_viewer->mo_sel->mt_extra_flds.
    DATA(l_multi) = is_multi( ).

    LOOP AT mt_jtabs INTO DATA(ls_tab).
      DATA(l_alias) = condense( CONV string( ls_tab-alias ) ).
      LOOP AT get_fieldlist( ls_tab-tabname ) INTO DATA(ls_f).
        CHECK ls_f-fieldname NE 'MANDT'.
        APPEND VALUE #(
          field_label = COND lvc_fname( WHEN l_multi = abap_true
                                        THEN |{ l_alias }_{ ls_f-fieldname }|
                                        ELSE ls_f-fieldname )
          name        = ls_f-fieldtext
          int_type    = ls_f-inttype
          datatype    = ls_f-datatype
          element     = ls_f-rollname
          domain      = ls_f-domname
          length      = ls_f-outputlen
          style       = COND #( WHEN ls_f-keyflag = abap_true
                                THEN VALUE #( ( fieldname = 'FIELD_LABEL' style = '00000020' ) ) )
        ) TO mo_viewer->mo_sel->mt_extra_flds.
      ENDLOOP.
    ENDLOOP.
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
          "a term glued to the previous one (..._idAND t4~...) would not parse
          REPLACE ALL OCCURRENCES OF REGEX '(\w)AND\s' IN l_value WITH '$1 AND '.
          <jtab>-cond = condense( l_value ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_sapevent.
    CASE action.
      WHEN 'mode'. "tool selection: J join, P pivot (click again = off)
        IF m_mode = getdata.
          CLEAR m_mode.
        ELSE.
          m_mode = getdata.
          IF m_mode = 'P' AND mo_pivot IS NOT BOUND.
            mo_pivot = NEW #( ).
          ENDIF.
        ENDIF.
        refresh_all( ).
      WHEN 'pv'. "pivot slot actions
        IF mo_pivot IS BOUND.
          mo_pivot->handle_action( i_act = CONV #( getdata ) it_fields = pivot_src_fields( ) ).
          render_html( ).
          update_sql_view( ).
        ENDIF.
      WHEN 'pvdrop'. "chip dropped on a slot: mv=<target><slot>|<source><slot>|<key>
        IF mo_pivot IS BOUND.
          DATA(l_drop_post) = concat_lines_of( table = postdata ).
          SPLIT l_drop_post AT '=' INTO DATA(l_drop_name) DATA(l_drop).
          REPLACE ALL OCCURRENCES OF '+' IN l_drop WITH ` `.
          l_drop = cl_http_utility=>unescape_url( l_drop ).
          IF l_drop IS NOT INITIAL.
            mo_pivot->handle_drop( i_move = l_drop it_fields = pivot_src_fields( ) ).
            render_html( ).
            update_sql_view( ).
          ENDIF.
        ENDIF.
      WHEN 'pvagg'. "aggregate listbox on a Values chip: idx=n&agg=SUM
        IF mo_pivot IS BOUND.
          DATA: l_agg_idx TYPE string, l_agg_new TYPE string.
          CLEAR: l_agg_idx, l_agg_new.
          DATA(l_agg_post) = concat_lines_of( table = postdata ).
          SPLIT l_agg_post AT '&' INTO TABLE DATA(lt_agg_pairs).
          LOOP AT lt_agg_pairs INTO DATA(l_agg_pair).
            SPLIT l_agg_pair AT '=' INTO DATA(l_agg_name) DATA(l_agg_val).
            CASE l_agg_name.
              WHEN 'idx'. l_agg_idx = l_agg_val.
              WHEN 'agg'. l_agg_new = l_agg_val.
            ENDCASE.
          ENDLOOP.
          IF l_agg_idx IS NOT INITIAL AND l_agg_new IS NOT INITIAL.
            mo_pivot->handle_action( i_act     = |sa_{ l_agg_idx }_{ l_agg_new }|
                                     it_fields = pivot_src_fields( ) ).
            render_html( ).
            update_sql_view( ).
          ENDIF.
        ENDIF.
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
      WHEN 'sqledit'. "toggle manual SQL editing
        IF getdata = 'ON'.
          m_sql_edit = abap_true.
          m_sql_manual = generate_select( ).
        ELSE.
          CLEAR: m_sql_edit, m_sql_manual.
        ENDIF.
        update_sql_view( ).
      WHEN 'sqlrun'. "posted from the textarea form: sql=...&act=run/back
        DATA(l_act_sql) = ``.
        DATA(l_sql_val) = ``.
        l_post = concat_lines_of( table = postdata ).
        SPLIT l_post AT '&' INTO TABLE DATA(lt_sql_pairs).
        LOOP AT lt_sql_pairs INTO DATA(l_sql_pair).
          SPLIT l_sql_pair AT '=' INTO DATA(l_sql_name) DATA(l_sql_value).
          REPLACE ALL OCCURRENCES OF '+' IN l_sql_value WITH ` `.
          l_sql_value = cl_http_utility=>unescape_url( l_sql_value ).
          CASE l_sql_name.
            WHEN 'sql'.
              l_sql_val = l_sql_value.
            WHEN 'act'.
              l_act_sql = l_sql_value.
          ENDCASE.
        ENDLOOP.
        IF l_act_sql = 'back'.
          CLEAR: m_sql_edit, m_sql_manual.
          update_sql_view( ). "back to auto-generated statement and auto-apply
        ELSE.
          m_sql_manual = format_sql( l_sql_val ). "the form transport eats line breaks
          IF m_sql_manual IS NOT INITIAL.
            execute_sql( i_sql = m_sql_manual ). "errors shown in the status bar
          ENDIF.
          update_sql_view( ). "stay in edit mode with the manual text
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
    IF mo_sql_html IS BOUND. "the edit link and the textarea form fire SAPEVENTs
      mo_sql_html->set_registered_events( VALUE #( ( eventid = cl_gui_html_viewer=>m_id_sapevent ) ) ).
      SET HANDLER on_sapevent FOR mo_sql_html.
    ENDIF.
  ENDMETHOD.


  METHOD update_sql_view.
    CHECK mo_sql_html IS BOUND.
    DATA l_sql TYPE string.
    IF m_mode = 'P' AND mo_pivot IS BOUND AND mo_pivot->has_layout( ) = abap_true.
      mo_pivot->normalize_aggs( pivot_src_fields( ) ).
      l_sql = mo_pivot->build_select( i_from  = build_from( )
                                      i_where = build_where( )
                                      i_multi = is_multi( )
                                      i_rows  = COND #( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 ) ).
    ELSE.
      l_sql = generate_select( ).
    ENDIF.

    IF m_sql_edit = abap_true. "manual mode: textarea instead of the highlighted view
      DATA(l_edit_sql) = COND #( WHEN m_sql_manual IS NOT INITIAL THEN m_sql_manual ELSE l_sql ).
      DATA(l_edit_html) =
        `<html><head><meta charset="utf-8"><style>` &&
        `body{margin:0;background:#fff;font-family:Consolas,monospace;font-size:12px;}` &&
        `textarea{width:98%;height:220px;font-family:Consolas,monospace;font-size:12px;border:1px solid #ccc;}` &&
        `.btn{border:1px solid #2e8b2e;background:#2e8b2e;color:#fff;border-radius:3px;padding:2px 12px;cursor:pointer;}` &&
        `.btn2{border:1px solid #bbb;background:#f2f2f2;border-radius:3px;padding:2px 12px;cursor:pointer;}` &&
        `</style></head><body>` &&
        `<form method="post" action="SAPEVENT:sqlrun">` &&
        |<textarea name="sql">{ escape( val = l_edit_sql format = cl_abap_format=>e_html_text ) }</textarea><br>| &&
        `<button class="btn" type="submit" name="act" value="run">&#9654; Run</button> ` &&
        `<button class="btn2" type="submit" name="act" value="back">Back to auto</button>` &&
        `</form></body></html>`.
      show_html( io_html = mo_sql_html i_html = l_edit_html ).
      RETURN. "no auto-execution while editing
    ENDIF.

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
    REPLACE ALL OCCURRENCES OF REGEX '\bORDER BY\b' IN l_sql_html WITH '<span class="kw">ORDER BY</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bDESCENDING\b' IN l_sql_html WITH '<span class="kw2">DESCENDING</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bUP TO\b' IN l_sql_html WITH '<span class="kw">UP TO</span>'.
    REPLACE ALL OCCURRENCES OF REGEX '\bROWS\b' IN l_sql_html WITH '<span class="kw">ROWS</span>'.

    DATA(l_html) =
      `<html><head><meta charset="utf-8"><style>` &&
      `body{margin:0;background:#f6f8fa;font-family:Consolas,monospace;font-size:12px;}` &&
      `pre{margin:6px;white-space:pre-wrap;}` &&
      `.kw{color:#0033cc;font-weight:bold;}` &&
      `.kw2{color:#7a3db8;font-weight:bold;}` &&
      `.edit{color:#2c5f8a;text-decoration:none;margin:6px;display:inline-block;}` &&
      `</style></head><body>` &&
      `<a class="edit" href="SAPEVENT:sqledit?ON">&#9998; edit</a>` &&
      `<pre>` && l_sql_html && `</pre></body></html>`.
    show_html( io_html = mo_sql_html i_html = l_html ).

    "a join without ON cannot be executed - say so instead of letting the DB fail
    LOOP AT mt_jtabs INTO DATA(ls_chk_tab) FROM 2 WHERE cond IS INITIAL.
      MESSAGE |Set the ON condition for { ls_chk_tab-alias } { ls_chk_tab-tabname }| TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDLOOP.

    "apply every change directly to the original window
    IF m_ready = abap_true AND viewer_alive( ) = abap_true.
      IF m_mode = 'P' AND mo_pivot IS BOUND AND mo_pivot->has_layout( ) = abap_true.
        execute_pivot( ). "the statement groups; the matrix (or the plain grouped rows,
                          "when nothing is spread into columns) is shaped in ABAP
      ELSE.
        execute_sql( i_sql = l_sql ).
      ENDIF.
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

    l_from = build_from( ).

    rv_sql = |SELECT { l_fields }{ cl_abap_char_utilities=>newline }| &&
             |  FROM { l_from }|.

    "WHERE built from the panel ranges with the CURRENT aliases;
    "filters of removed joins are silently skipped
    DATA(l_where) = build_where( ).
    IF l_where IS NOT INITIAL.
      rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } WHERE { l_where }|.
    ENDIF.

    drop_stale_order( ). "a removed table must not stay behind in ORDER BY
    IF m_order IS NOT INITIAL.
      rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } ORDER BY { m_order }|.
    ENDIF.

    DATA(l_rows) = COND i( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 ).
    rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } UP TO { l_rows } ROWS|.
  ENDMETHOD.


  METHOD is_multi.
    rv_multi = boolc( lines( mt_jtabs ) > 1 ).
  ENDMETHOD.


  METHOD cache_where_selection.
    CHECK mo_viewer->mo_sel IS BOUND.
    "while a layout is applied the panel is still empty: caching it now would
    "delete the very filters that were just read from the file
    CHECK m_loading = abap_false.

    DATA lt_valid TYPE TABLE OF lvc_fname.
    LOOP AT mo_viewer->mo_sel->mt_sel_tab INTO DATA(ls_current).
      DATA(l_current_label) = condense( CONV string( ls_current-field_label ) ).
      DATA l_current_alt TYPE lvc_fname.
      FIND REGEX '^T\d+_' IN l_current_label MATCH LENGTH DATA(l_current_pfx_len).
      IF sy-subrc = 0.
        l_current_alt = l_current_label+l_current_pfx_len.
      ELSE.
        l_current_alt = |T0_{ l_current_label }|.
      ENDIF.
      APPEND ls_current-field_label TO lt_valid.
      APPEND l_current_alt TO lt_valid.
    ENDLOOP.

    DATA(l_cache_idx) = lines( mt_where_sel ).
    WHILE l_cache_idx > 0.
      READ TABLE mt_where_sel INDEX l_cache_idx INTO DATA(ls_cached).
      IF NOT line_exists( lt_valid[ table_line = ls_cached-field_label ] ).
        DELETE mt_where_sel INDEX l_cache_idx.
      ENDIF.
      l_cache_idx = l_cache_idx - 1.
    ENDWHILE.

    LOOP AT mo_viewer->mo_sel->mt_sel_tab INTO DATA(ls_sel).
      DATA(l_label) = condense( CONV string( ls_sel-field_label ) ).
      DATA l_alt TYPE lvc_fname.
      FIND REGEX '^T\d+_' IN l_label MATCH LENGTH DATA(l_pfx_len).
      IF sy-subrc = 0.
        l_alt = l_label+l_pfx_len.
      ELSE.
        l_alt = |T0_{ l_label }|.
      ENDIF.

      IF ls_sel-range IS INITIAL
         AND ls_sel-low IS INITIAL
         AND ls_sel-high IS INITIAL
         AND ls_sel-sign IS INITIAL
         AND ls_sel-opti IS INITIAL.
        DELETE mt_where_sel WHERE field_label = ls_sel-field_label
                               OR field_label = l_alt.
        CONTINUE.
      ENDIF.

      READ TABLE mt_where_sel ASSIGNING FIELD-SYMBOL(<saved>)
        WITH KEY field_label = ls_sel-field_label.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_sel TO <saved>.
      ELSE.
        APPEND ls_sel TO mt_where_sel.
      ENDIF.
      DELETE mt_where_sel WHERE field_label = l_alt.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_from.
    DATA(l_multi) = is_multi( ).
    LOOP AT mt_jtabs INTO DATA(ls_tab).
      IF sy-tabix = 1.
        IF l_multi = abap_true.
          rv_from = |{ ls_tab-tabname CASE = LOWER } AS { ls_tab-alias CASE = LOWER }|.
        ELSE.
          rv_from = |{ ls_tab-tabname CASE = LOWER }|.
        ENDIF.
      ELSE.
        rv_from = |{ rv_from }{ cl_abap_char_utilities=>newline }  { ls_tab-jtype } JOIN { ls_tab-tabname CASE = LOWER } AS { ls_tab-alias CASE = LOWER } ON { ls_tab-cond }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_where.
    DATA: lt_where TYPE rsds_twhere,
          lt_range TYPE rsds_trange,
          lt_sel   TYPE TABLE OF zcl_sde_appl=>selection_display_s.

    DATA(l_multi) = boolc( lines( mt_jtabs ) > 1 ).

    "the cache alone is enough: filters loaded from a file exist before the
    "selection panel is even created (it is built on demand, SEL_ON)
    lt_sel = mt_where_sel.
    IF lt_sel IS INITIAL.
      CHECK mo_viewer->mo_sel IS BOUND.
      lt_sel = mo_viewer->mo_sel->mt_sel_tab.
    ENDIF.

    APPEND INITIAL LINE TO lt_range ASSIGNING FIELD-SYMBOL(<tabl>).
    <tabl>-tablename = m_tabname.
    LOOP AT lt_sel INTO DATA(ls_tab) WHERE range IS NOT INITIAL.
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

    "cut out ORDER BY <o> (the last clause before UP TO, so it goes first)
    DATA(l_order) = ``.
    FIND REGEX '\sORDER\s+BY\s' IN l_upper MATCH OFFSET DATA(l_ord_off) MATCH LENGTH DATA(l_ord_len).
    IF sy-subrc = 0.
      DATA(l_ord_val_off) = l_ord_off + l_ord_len.
      l_order = condense( substring( val = l_sql off = l_ord_val_off len = strlen( l_sql ) - l_ord_val_off ) ).
      l_sql   = l_sql+0(l_ord_off).
      l_upper = l_upper+0(l_ord_off).
    ENDIF.

    "cut out GROUP BY <g> (comes after WHERE in the generated statement)
    DATA(l_group) = ``.
    FIND REGEX '\sGROUP\s+BY\s' IN l_upper MATCH OFFSET DATA(l_grp_off) MATCH LENGTH DATA(l_grp_len).
    IF sy-subrc = 0.
      DATA(l_grp_val_off) = l_grp_off + l_grp_len.
      l_group = condense( substring( val = l_sql off = l_grp_val_off len = strlen( l_sql ) - l_grp_val_off ) ).
      l_sql   = l_sql+0(l_grp_off).
      l_upper = l_upper+0(l_grp_off).
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
    "[\w/] - table name may carry a namespace prefix, e.g. /CLIN/RGR_TCG
    FIND ALL OCCURRENCES OF REGEX '([\w/]+)\s+AS\s+(\w+)' IN to_upper( l_from ) RESULTS DATA(lt_matches).
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
    DATA lt_cat TYPE lvc_t_fcat. "per-item headers (pivot CASE buckets)
    SPLIT l_fields AT ',' INTO TABLE DATA(lt_fld_str).
    LOOP AT lt_fld_str INTO DATA(l_fld_str).
      CONDENSE l_fld_str.
      CHECK l_fld_str IS NOT INITIAL.
      DATA(l_fld_up) = to_upper( l_fld_str ).
      DATA: l_agg TYPE string, l_alias2 TYPE string, l_field TYPE string,
            l_name TYPE string, l_head TYPE string.
      CLEAR: l_agg, l_alias2, l_field, l_name, l_head.
      FIND REGEX '^(?:(SUM|COUNT|MIN|MAX|AVG)\s*\(\s*)?(?:(\w+)~)?(\w+|\*)\s*\)?(?:\s+AS\s+(\w+))?$' IN l_fld_up
        SUBMATCHES l_agg l_alias2 l_field l_name.
      IF sy-subrc NE 0.
        "complex expression, e.g. SUM( CASE WHEN t1~curr = 'USD' THEN t0~price END ) AS name
        FIND REGEX '^(SUM|COUNT|MIN|MAX|AVG)\s*\(' IN l_fld_up SUBMATCHES l_agg.
        FIND REGEX '\sAS\s+(\w+)\s*$' IN l_fld_up SUBMATCHES l_name.
        FIND REGEX 'THEN\s+(?:(\w+)~)?(\w+)' IN l_fld_up SUBMATCHES l_alias2 l_field.
        IF l_field IS INITIAL.
          FIND REGEX '(\w+)~(\w+)' IN l_fld_up SUBMATCHES l_alias2 l_field.
        ENDIF.
        IF l_agg IS INITIAL OR l_name IS INITIAL OR l_field IS INITIAL.
          IF i_quiet = abap_false.
            MESSAGE |Cannot parse field: { l_fld_str } (expressions need AS name)| TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
          RETURN.
        ENDIF.
        "column header from the WHEN literal of the original (case-preserving) text
        DATA: l_lit TYPE string, l_litq TYPE string,
              l_cond_alias TYPE string, l_cond_field TYPE string,
              l_head_value TYPE string,
              l_head_subrc TYPE sy-subrc.
        CLEAR: l_lit, l_litq, l_cond_alias, l_cond_field, l_head_value.
        FIND REGEX 'WHEN\s+(?:(\w+)~)?(\w+)\s*=\s*(?:''([^'']*)''|(\S+))\s+(?:AND|THEN)' IN l_fld_str
          IGNORING CASE SUBMATCHES l_cond_alias l_cond_field l_litq l_lit.
        l_head_subrc = sy-subrc.
        l_head_value = COND #( WHEN l_head_subrc NE 0 THEN l_name
                               WHEN l_litq IS NOT INITIAL THEN l_litq
                               WHEN l_lit IS NOT INITIAL AND l_lit NE `''` THEN l_lit
                               ELSE `(empty)` ). "matched an empty '' literal
        l_head = COND #( WHEN l_head_subrc NE 0
                           OR l_cond_field IS INITIAL
                           OR ( strlen( l_head_value ) > 2 AND l_head_value NE `(empty)` )
                         THEN l_head_value
                         ELSE |{ l_cond_field }: { l_head_value }| ).
      ENDIF.
      IF l_name IS INITIAL.
        l_name = COND #( WHEN l_alias2 IS INITIAL THEN l_field ELSE |{ l_alias2 }_{ l_field }| ).
        IF l_agg IS NOT INITIAL.
          l_name = |{ l_agg }_{ COND #( WHEN l_field = '*' THEN 'ROWS' ELSE l_name ) }|.
        ENDIF.
      ENDIF.
      IF strlen( l_name ) > 30.
        l_name = l_name+0(30).
      ENDIF.

      READ TABLE lt_alias INTO DATA(ls_alias) WITH KEY alias = l_alias2.
      IF sy-subrc NE 0.
        READ TABLE lt_alias INTO ls_alias INDEX 1.
      ENDIF.

      DATA lo_type TYPE REF TO cl_abap_datadescr.
      IF l_agg = 'COUNT'. "COUNT( field ) / COUNT( * ): integer result
        lo_type = cl_abap_elemdescr=>get_int8( ).
      ELSEIF l_agg = 'AVG'. "AVG returns a decimal regardless of the field type
        lo_type = cl_abap_elemdescr=>get_p( p_length = 16 p_decimals = 3 ).
      ELSE. "plain field or SUM/MIN/MAX: the field's own type
        "describe_by_name raises a CLASSIC exception - a TRY/CATCH would dump here
        DATA lo_td TYPE REF TO cl_abap_typedescr.
        cl_abap_typedescr=>describe_by_name(
          EXPORTING  p_name         = |{ ls_alias-tabname }-{ l_field }|
          RECEIVING  p_descr_ref    = lo_td
          EXCEPTIONS type_not_found = 1 OTHERS = 2 ).
        IF sy-subrc NE 0 OR lo_td IS NOT BOUND.
          IF i_quiet = abap_false.
            MESSAGE |Unknown field { ls_alias-tabname }-{ l_field }| TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
          RETURN.
        ENDIF.
        TRY.
            lo_type = CAST cl_abap_datadescr( lo_td ).
          CATCH cx_sy_move_cast_error.
            IF i_quiet = abap_false.
              MESSAGE |{ ls_alias-tabname }-{ l_field } is not a data type| TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
            RETURN.
        ENDTRY.
      ENDIF.

      "keep names unique
      WHILE line_exists( lt_comp[ name = l_name ] ).
        l_name = |{ COND string( WHEN strlen( l_name ) > 28 THEN l_name+0(28) ELSE l_name ) }_{ sy-index }|.
      ENDWHILE.
      APPEND VALUE #( name = l_name type = lo_type ) TO lt_comp.
      APPEND fcat_entry( i_fieldname = CONV #( l_name ) io_type = lo_type i_text = l_head ) TO lt_cat.
    ENDLOOP.

    IF lt_comp IS INITIAL.
      IF i_quiet = abap_false.
        MESSAGE 'No fields to select' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      RETURN.
    ENDIF.

    "dynamic Open SQL tokens must be uppercase (CASE literals keep their case)
    l_fields = upper_outside_quotes( l_fields ).
    l_from   = to_upper( l_from ).
    l_where  = upper_outside_quotes( l_where ).
    l_group  = to_upper( l_group ).
    l_order  = to_upper( l_order ).

    TRY.
        DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
        DATA(lo_tab) = cl_abap_tabledescr=>create(
                         p_line_type  = lo_struct
                         p_table_kind = cl_abap_tabledescr=>tablekind_std
                         p_unique     = abap_false ).
        CREATE DATA lr_table TYPE HANDLE lo_tab.
        ASSIGN lr_table->* TO <result>.

        "new strict-mode Open SQL: dynamic tokens with AS aliases are supported here
        IF l_group IS INITIAL AND l_order IS INITIAL.
          SELECT (l_fields)
            FROM (l_from)
            WHERE (l_where)
            INTO CORRESPONDING FIELDS OF TABLE @<result>
            UP TO @l_rows ROWS.
        ELSEIF l_group IS INITIAL.
          SELECT (l_fields)
            FROM (l_from)
            WHERE (l_where)
            ORDER BY (l_order)
            INTO CORRESPONDING FIELDS OF TABLE @<result>
            UP TO @l_rows ROWS.
        ELSEIF l_order IS INITIAL.
          SELECT (l_fields)
            FROM (l_from)
            WHERE (l_where)
            GROUP BY (l_group)
            INTO CORRESPONDING FIELDS OF TABLE @<result>
            UP TO @l_rows ROWS.
        ELSE.
          SELECT (l_fields)
            FROM (l_from)
            WHERE (l_where)
            GROUP BY (l_group)
            ORDER BY (l_order)
            INTO CORRESPONDING FIELDS OF TABLE @<result>
            UP TO @l_rows ROWS.
        ENDIF.

      CATCH cx_root INTO DATA(lx).
        IF i_quiet = abap_false.
          MESSAGE lx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        RETURN.
    ENDTRY.

    DATA(l_view_name) = COND #( WHEN l_group IS INITIAL
                                THEN |JOIN { m_tabname } ({ lines( <result> ) })|
                                ELSE |PIVOT { m_tabname } ({ lines( <result> ) })| ).
    mo_viewer->rebind( ir_tab = lr_table i_name = l_view_name i_generic = abap_true
                       it_catalog = lt_cat ).
    sync_sel_panel( ).
  ENDMETHOD.


  METHOD sync_sel_panel.
    "the filters of the panel follow the fields the statement produced
    CHECK mo_viewer IS BOUND.
    IF mo_viewer->mo_sel IS BOUND.
      fill_sel_extras( ). "unselected join fields stay filterable behind the toolbar toggle
      mo_viewer->mo_sel->update_sel_tab( ).
      LOOP AT mo_viewer->mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<sync_sel>).
        DATA(l_sync_label) = condense( CONV string( <sync_sel>-field_label ) ).
        READ TABLE mt_where_sel INTO DATA(ls_sync_saved)
          WITH KEY field_label = <sync_sel>-field_label.
        IF sy-subrc NE 0
           OR ( ls_sync_saved-range IS INITIAL
                AND ls_sync_saved-low IS INITIAL
                AND ls_sync_saved-high IS INITIAL
                AND ls_sync_saved-sign IS INITIAL
                AND ls_sync_saved-opti IS INITIAL ).
          DATA l_sync_alt TYPE lvc_fname.
          FIND REGEX '^T\d+_' IN l_sync_label MATCH LENGTH DATA(l_sync_pfx_len).
          IF sy-subrc = 0.
            l_sync_alt = l_sync_label+l_sync_pfx_len.
          ELSE.
            l_sync_alt = |T0_{ l_sync_label }|.
          ENDIF.
          READ TABLE mt_where_sel INTO ls_sync_saved WITH KEY field_label = l_sync_alt.
        ENDIF.
        IF sy-subrc = 0.
          <sync_sel>-low         = ls_sync_saved-low.
          <sync_sel>-high        = ls_sync_saved-high.
          <sync_sel>-sign        = ls_sync_saved-sign.
          <sync_sel>-opti        = ls_sync_saved-opti.
          <sync_sel>-range       = ls_sync_saved-range.
          <sync_sel>-option_icon = ls_sync_saved-option_icon.
          <sync_sel>-more_icon   = ls_sync_saved-more_icon.
          mo_viewer->mo_sel->update_sel_row( CHANGING c_sel_row = <sync_sel> ).
        ENDIF.
      ENDLOOP.
      Zcl_SDE_common=>refresh( mo_viewer->mo_sel->mo_sel_alv ).
    ENDIF.
  ENDMETHOD.


  METHOD key_type.
    "type of one column of the grouped statement
    IF i_agg = 'COUNT'. "COUNT( field ) / COUNT( * ): integer result
      ro_type = cl_abap_elemdescr=>get_int8( ).
      RETURN.
    ELSEIF i_agg = 'AVG'. "AVG returns a decimal regardless of the field type
      ro_type = cl_abap_elemdescr=>get_p( p_length = 16 p_decimals = 3 ).
      RETURN.
    ENDIF.

    SPLIT i_key AT '~' INTO DATA(l_alias) DATA(l_field).
    DATA(l_alias_up) = to_upper( l_alias ).
    DATA(l_field_up) = to_upper( l_field ).
    READ TABLE mt_jtabs INTO DATA(ls_tab) WITH KEY alias = l_alias_up.
    CHECK sy-subrc = 0.

    "describe_by_name raises a CLASSIC exception - a TRY/CATCH would dump here
    DATA lo_td TYPE REF TO cl_abap_typedescr.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING  p_name         = |{ ls_tab-tabname }-{ l_field_up }|
      RECEIVING  p_descr_ref    = lo_td
      EXCEPTIONS type_not_found = 1 OTHERS = 2 ).
    CHECK sy-subrc = 0 AND lo_td IS BOUND.
    TRY.
        ro_type = CAST cl_abap_datadescr( lo_td ).
      CATCH cx_sy_move_cast_error.
        CLEAR ro_type.
    ENDTRY.
  ENDMETHOD.


  METHOD value_text.
    "the values of a few components as one text: C/X, (empty) for a blank one
    FIELD-SYMBOLS <v> TYPE any.
    DATA l_txt TYPE string.

    LOOP AT it_comps INTO DATA(l_comp).
      ASSIGN COMPONENT l_comp OF STRUCTURE i_row TO <v>.
      CHECK sy-subrc = 0.
      l_txt = <v>. "plain assignment: NUMC/DATS/decimals keep their raw form
      CONDENSE l_txt.
      IF rv_text IS NOT INITIAL.
        rv_text = |{ rv_text }/|.
      ENDIF.
      rv_text = |{ rv_text }{ COND string( WHEN l_txt IS INITIAL THEN `(empty)` ELSE l_txt ) }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD domain_text.
    "fixed-value text of one domain value, cached across the cells of one pivot
    READ TABLE mt_domain_cache INTO DATA(ls_cached) WITH KEY domname = i_domname value = i_value.
    IF sy-subrc = 0.
      rv_text = ls_cached-text.
      RETURN.
    ENDIF.

    SELECT SINGLE ddtext FROM dd07t INTO rv_text
      WHERE domname     = i_domname
        AND ddlanguage  = m_fld_lang
        AND as4local    = 'A'
        AND domvalue_l  = i_value.
    IF sy-subrc NE 0.
      CLEAR rv_text.
    ENDIF.

    APPEND VALUE #( domname = i_domname value = i_value text = rv_text ) TO mt_domain_cache.
  ENDMETHOD.


  METHOD field_label.
    "display text of one field's value: the domain's fixed-value text when the
    "field has one, the field's own label for an 'X' the domain cannot explain
    "(typical for a FLAG data element - checked/unchecked has no useful code),
    "the raw value otherwise
    rv_text = COND #( WHEN i_raw IS INITIAL THEN `(empty)` ELSE i_raw ).
    CHECK i_key IS NOT INITIAL.

    SPLIT i_key AT '~' INTO DATA(l_alias) DATA(l_field).
    READ TABLE mt_jflds INTO DATA(ls_fld)
      WITH KEY alias = to_upper( l_alias ) fieldname = to_upper( l_field ).
    CHECK sy-subrc = 0.

    DATA(l_dtext) = COND string( WHEN ls_fld-domname IS NOT INITIAL
                                 THEN domain_text( i_domname = ls_fld-domname i_value = i_raw )
                                 ELSE `` ).
    IF l_dtext IS NOT INITIAL.
      rv_text = l_dtext.
    ELSEIF i_raw = 'X'. "no domain text for the checked value: name the field instead
      rv_text = COND #( WHEN ls_fld-ddtext IS NOT INITIAL
                        THEN CONV string( ls_fld-ddtext ) ELSE ls_fld-fieldname ).
    ENDIF.
  ENDMETHOD.


  METHOD field_header.
    "the field's own label: DDIC text if there is one, otherwise the technical name
    CHECK i_key IS NOT INITIAL.
    SPLIT i_key AT '~' INTO DATA(l_alias) DATA(l_field).
    READ TABLE mt_jflds INTO DATA(ls_fld)
      WITH KEY alias = to_upper( l_alias ) fieldname = to_upper( l_field ).
    CHECK sy-subrc = 0.
    rv_text = COND #( WHEN ls_fld-ddtext IS NOT INITIAL THEN CONV string( ls_fld-ddtext ) ELSE ls_fld-fieldname ).
  ENDMETHOD.


  METHOD value_label.
    "display text of a few components, one field_label per component - used
    "for headers only. value_text is still the matching key, so distinct
    "database values are never merged into one bucket by a label collision.
    FIELD-SYMBOLS <v> TYPE any.
    DATA l_txt TYPE string.

    LOOP AT it_comps INTO DATA(l_comp).
      ASSIGN COMPONENT l_comp OF STRUCTURE i_row TO <v>.
      CHECK sy-subrc = 0.
      l_txt = <v>.
      CONDENSE l_txt.

      READ TABLE it_cols INTO DATA(ls_src) WITH KEY comp = l_comp.
      DATA(l_part) = field_label( i_key = COND #( WHEN sy-subrc = 0 THEN ls_src-key ELSE `` ) i_raw = l_txt ).

      IF rv_text IS NOT INITIAL.
        rv_text = |{ rv_text } / |.
      ENDIF.
      rv_text = |{ rv_text }{ l_part }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD execute_pivot.
    "The matrix is spread here, not by the database: a dynamically specified
    "SELECT list cannot carry CASE expressions. The statement groups by the
    "dimensions, so every line of its result is one cell of the matrix.
    CONSTANTS c_max_cols TYPE i VALUE 50. "distinct column value combinations

    TYPES: BEGIN OF t_cell,
             bucket TYPE string, "column value combination (raw, matches value_text)
             meas   TYPE string, "component of the measure in the grouped result
             comp   TYPE string, "component in the matrix
           END OF t_cell,
           BEGIN OF t_bktinfo,
             key   TYPE string, "raw combination - the matching key, never shown
             label TYPE string, "domain/field-aware text - the column header
           END OF t_bktinfo,
           BEGIN OF t_rmeta,
             comp      TYPE string,
             key       TYPE string,     "ALIAS~FIELD behind the component, for field_label
             use_label TYPE abap_bool,  "at least one occurring value has a text label
             header    TYPE string,
           END OF t_rmeta.

    FIELD-SYMBOLS: <narrow> TYPE STANDARD TABLE,
                   <wide>   TYPE STANDARD TABLE,
                   <nrow>   TYPE any,
                   <wrow>   TYPE any,
                   <val>    TYPE any,
                   <cell>   TYPE any.

    DATA: lt_ncomp  TYPE abap_component_tab,
          lt_wcomp  TYPE abap_component_tab,
          lt_cat    TYPE lvc_t_fcat,
          lt_rdim   TYPE string_table, "row dimension components
          lt_cdim   TYPE string_table, "column dimension components
          lt_meas   TYPE zcl_sde_pivot=>tt_sqlcols,
          lt_bktinfo TYPE TABLE OF t_bktinfo,
          lt_rmeta  TYPE TABLE OF t_rmeta,
          lt_taken  TYPE string_table,
          lt_cell   TYPE TABLE OF t_cell,
          lt_sort   TYPE abap_sortorder_tab,
          lr_narrow TYPE REF TO data,
          lr_wide   TYPE REF TO data.

    CHECK mo_pivot IS BOUND AND viewer_alive( ) = abap_true.
    DATA(lt_cols) = mo_pivot->get_sql_columns( ).
    CHECK lt_cols IS NOT INITIAL.

    "structure of the grouped statement: one component per column, same names
    LOOP AT lt_cols INTO DATA(ls_col).
      DATA(lo_type) = key_type( i_key = ls_col-key i_agg = ls_col-agg ).
      IF lo_type IS NOT BOUND.
        MESSAGE |Cannot determine the type of { ls_col-key }| TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      APPEND VALUE #( name = ls_col-comp type = lo_type ) TO lt_ncomp.
      CASE ls_col-role.
        WHEN 'R'.    APPEND ls_col-comp TO lt_rdim.
        WHEN 'C'.    APPEND ls_col-comp TO lt_cdim.
        WHEN OTHERS. APPEND ls_col TO lt_meas.
      ENDCASE.
    ENDLOOP.
    "without column fields nothing is spread - the grouped result is the result,
    "just with domain-aware row dimensions instead of raw codes (see below)

    DATA l_fields TYPE string.
    DATA l_group  TYPE string.
    mo_pivot->build_tokens( EXPORTING i_multi  = is_multi( )
                            IMPORTING e_fields = l_fields
                                      e_group  = l_group ).
    DATA(l_from) = to_upper( build_from( ) ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_from WITH ` `.
    DATA(l_where) = upper_outside_quotes( build_where( ) ).
    DATA(l_rows)  = COND i( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 ).

    TRY.
        DATA(lo_nline) = cl_abap_structdescr=>create( lt_ncomp ).
        DATA(lo_ntab)  = cl_abap_tabledescr=>create( p_line_type  = lo_nline
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                     p_unique     = abap_false ).
        CREATE DATA lr_narrow TYPE HANDLE lo_ntab.
        ASSIGN lr_narrow->* TO <narrow>.

        IF l_group IS INITIAL.
          SELECT (l_fields)
            FROM (l_from)
            WHERE (l_where)
            INTO CORRESPONDING FIELDS OF TABLE @<narrow>
            UP TO @l_rows ROWS.
        ELSE.
          SELECT (l_fields)
            FROM (l_from)
            WHERE (l_where)
            GROUP BY (l_group)
            INTO CORRESPONDING FIELDS OF TABLE @<narrow>
            UP TO @l_rows ROWS.
        ENDIF.
      CATCH cx_root INTO DATA(lx).
        MESSAGE lx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    "sorted by the dimensions, the transpose walks the result once
    LOOP AT lt_rdim INTO DATA(l_comp).
      APPEND VALUE #( name = l_comp ) TO lt_sort.
    ENDLOOP.
    LOOP AT lt_cdim INTO l_comp.
      APPEND VALUE #( name = l_comp ) TO lt_sort.
    ENDLOOP.
    SORT <narrow> BY (lt_sort).

    "the distinct column value combinations become the columns of the matrix;
    "the label (domain text, or the field name behind an unexplained 'X') is
    "computed once per combination, from whichever row is the first to show it.
    "Nothing to spread without a column field - the grouped rows stand as they are.
    IF lt_cdim IS NOT INITIAL.
      LOOP AT <narrow> ASSIGNING <nrow>.
        DATA(l_key) = value_text( i_row = <nrow> it_comps = lt_cdim ).
        IF NOT line_exists( lt_bktinfo[ key = l_key ] ).
          APPEND VALUE #( key   = l_key
                          label = value_label( i_row = <nrow> it_comps = lt_cdim it_cols = lt_cols ) ) TO lt_bktinfo.
        ENDIF.
      ENDLOOP.
      SORT lt_bktinfo BY key.
      IF lines( lt_bktinfo ) > c_max_cols.
        DATA(l_cut) = c_max_cols + 1.
        DELETE lt_bktinfo FROM l_cut.
        MESSAGE |Only the first { c_max_cols } column values are shown| TYPE 'S' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.

    "matrix structure: the row dimensions, then one column per bucket and measure.
    "A row dimension with domain-aware text (or an unexplained 'X') gets that
    "text in the cell instead of the raw code - the whole column turns into a
    "plain string then, since a fixed-value field is exactly the case where its
    "DDIC type would render it as a checkbox or a bare code, not the intended text.
    LOOP AT lt_rdim INTO l_comp.
      READ TABLE lt_cols INTO DATA(ls_rcol) WITH KEY comp = l_comp role = 'R'.
      DATA(l_rkey)  = COND string( WHEN sy-subrc = 0 THEN ls_rcol-key ELSE `` ).
      DATA(l_ruse)  = abap_false.
      DATA(l_rhead) = to_lower( l_comp ).

      IF l_rkey IS NOT INITIAL.
        DATA(l_rhead2) = field_header( l_rkey ).
        IF l_rhead2 IS NOT INITIAL.
          l_rhead = l_rhead2.
          "does any value actually occurring in the result get a text label?
          LOOP AT <narrow> ASSIGNING <nrow>.
            ASSIGN COMPONENT l_comp OF STRUCTURE <nrow> TO <val>.
            CHECK sy-subrc = 0.
            DATA(l_rraw) = CONV string( <val> ).
            CONDENSE l_rraw.
            IF field_label( i_key = l_rkey i_raw = l_rraw ) NE COND string( WHEN l_rraw IS INITIAL THEN `(empty)` ELSE l_rraw ).
              l_ruse = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      APPEND VALUE #( comp = l_comp key = l_rkey use_label = l_ruse header = l_rhead ) TO lt_rmeta.
    ENDLOOP.

    LOOP AT lt_rmeta INTO DATA(ls_rmeta).
      IF ls_rmeta-use_label = abap_true.
        DATA(lo_rtype) = CAST cl_abap_datadescr( cl_abap_elemdescr=>get_string( ) ).
        APPEND VALUE #( name = ls_rmeta-comp type = lo_rtype ) TO lt_wcomp.
        APPEND fcat_entry( i_fieldname = CONV #( ls_rmeta-comp ) io_type = lo_rtype i_text = ls_rmeta-header ) TO lt_cat.
      ELSE.
        DATA(lo_dtype) = lt_ncomp[ name = ls_rmeta-comp ]-type.
        APPEND VALUE #( name = ls_rmeta-comp type = lo_dtype ) TO lt_wcomp.
        APPEND fcat_entry( i_fieldname = CONV #( ls_rmeta-comp ) io_type = lo_dtype i_text = `` ) TO lt_cat.
      ENDIF.
      APPEND ls_rmeta-comp TO lt_taken.
    ENDLOOP.

    IF lt_cdim IS NOT INITIAL.
      LOOP AT lt_bktinfo INTO DATA(ls_bkt).
        LOOP AT lt_meas INTO DATA(ls_meas).
          "component: AGG_FIELD_<value>, unique, <= 30 - built from the raw key,
          "the label may contain spaces or run long
          DATA(l_san)  = mo_pivot->sanitize( ls_bkt-key ).
          DATA(l_base) = 29 - strlen( l_san ).
          DATA(l_name) = COND string( WHEN strlen( ls_meas-comp ) > l_base
                                      THEN ls_meas-comp+0(l_base) ELSE ls_meas-comp ) && `_` && l_san.
          WHILE line_exists( lt_taken[ table_line = l_name ] ).
            l_name = |{ COND string( WHEN strlen( l_name ) > 28 THEN l_name+0(28) ELSE l_name ) }_{ sy-index }|.
          ENDWHILE.
          APPEND l_name TO lt_taken.

          DATA(lo_mtype) = lt_ncomp[ name = ls_meas-comp ]-type.
          APPEND VALUE #( name = l_name type = lo_mtype ) TO lt_wcomp.
          APPEND VALUE #( bucket = ls_bkt-key meas = ls_meas-comp comp = l_name ) TO lt_cell.
          "header: the column label, and the measure as well when there is more than one
          DATA(l_head) = COND string( WHEN lines( lt_meas ) = 1
                                      THEN ls_bkt-label
                                      ELSE |{ ls_bkt-label } { to_lower( ls_meas-comp ) }| ).
          APPEND fcat_entry( i_fieldname = CONV #( l_name ) io_type = lo_mtype i_text = l_head ) TO lt_cat.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      "no columns placed: the measures keep their own name, straight from the
      "grouped result - nothing to spread, nothing to disambiguate
      LOOP AT lt_meas INTO DATA(ls_flatmeas).
        DATA(lo_ftype) = lt_ncomp[ name = ls_flatmeas-comp ]-type.
        APPEND VALUE #( name = ls_flatmeas-comp type = lo_ftype ) TO lt_wcomp.
        DATA(l_fhead) = COND string( WHEN ls_flatmeas-key IS INITIAL THEN `Count`
                                     ELSE field_header( ls_flatmeas-key ) ).
        APPEND fcat_entry( i_fieldname = CONV #( ls_flatmeas-comp ) io_type = lo_ftype i_text = l_fhead ) TO lt_cat.
      ENDLOOP.
    ENDIF.

    TRY.
        DATA(lo_wline) = cl_abap_structdescr=>create( lt_wcomp ).
        DATA(lo_wtab)  = cl_abap_tabledescr=>create( p_line_type  = lo_wline
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                     p_unique     = abap_false ).
        CREATE DATA lr_wide TYPE HANDLE lo_wtab.
      CATCH cx_root INTO lx.
        MESSAGE lx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    ASSIGN lr_wide->* TO <wide>.

    "one line per row dimension combination, one cell per bucket
    DATA(l_prev) = ``.
    LOOP AT <narrow> ASSIGNING <nrow>.
      DATA(l_idx)  = sy-tabix. "value_text below reads tables of its own
      DATA(l_dims) = value_text( i_row = <nrow> it_comps = lt_rdim ).
      IF l_idx = 1 OR l_dims NE l_prev.
        APPEND INITIAL LINE TO <wide> ASSIGNING <wrow>.
        LOOP AT lt_rmeta INTO ls_rmeta.
          ASSIGN COMPONENT ls_rmeta-comp OF STRUCTURE <nrow> TO <val>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT ls_rmeta-comp OF STRUCTURE <wrow> TO <cell>.
          CHECK sy-subrc = 0.
          IF ls_rmeta-use_label = abap_true.
            DATA(l_cellraw) = CONV string( <val> ).
            CONDENSE l_cellraw.
            <cell> = field_label( i_key = ls_rmeta-key i_raw = l_cellraw ).
          ELSE.
            <cell> = <val>.
          ENDIF.
        ENDLOOP.
        l_prev = l_dims.
      ENDIF.

      IF lt_cdim IS NOT INITIAL.
        DATA(l_bkt) = value_text( i_row = <nrow> it_comps = lt_cdim ).
        LOOP AT lt_cell INTO DATA(ls_cell) WHERE bucket = l_bkt.
          ASSIGN COMPONENT ls_cell-meas OF STRUCTURE <nrow> TO <val>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT ls_cell-comp OF STRUCTURE <wrow> TO <cell>.
          IF sy-subrc = 0.
            <cell> = <val>.
          ENDIF.
        ENDLOOP.
      ELSE.
        "no columns placed: the measures are already the row's own components
        LOOP AT lt_meas INTO DATA(ls_flatval).
          ASSIGN COMPONENT ls_flatval-comp OF STRUCTURE <nrow> TO <val>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT ls_flatval-comp OF STRUCTURE <wrow> TO <cell>.
          IF sy-subrc = 0.
            <cell> = <val>.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    "the sort buttons of the grid: stale column names (from a layout that has
    "since changed) are dropped rather than raising an error
    DATA lt_valid_sort TYPE abap_sortorder_tab.
    LOOP AT mt_pivot_sort INTO DATA(ls_psort).
      IF line_exists( lt_wcomp[ name = ls_psort-name ] ).
        APPEND ls_psort TO lt_valid_sort.
      ENDIF.
    ENDLOOP.
    IF lt_valid_sort IS NOT INITIAL.
      SORT <wide> BY (lt_valid_sort).
    ENDIF.

    mo_viewer->rebind( ir_tab     = lr_wide
                       i_name     = |PIVOT { m_tabname } ({ lines( <wide> ) })|
                       i_generic  = abap_true
                       it_catalog = lt_cat ).
    sync_sel_panel( ).
  ENDMETHOD.


  METHOD fcat_entry.
    rs_cat-fieldname = i_fieldname.
    TRY.
        DATA(lo_elem) = CAST cl_abap_elemdescr( io_type ).
        IF lo_elem->is_ddic_type( ) = abap_true.
          DATA(ls_dfies) = lo_elem->get_ddic_field( ).
          MOVE-CORRESPONDING ls_dfies TO rs_cat.
          rs_cat-fieldname = i_fieldname.
          CLEAR: rs_cat-tabname, rs_cat-key.
        ELSE.
          rs_cat-inttype  = lo_elem->type_kind.
          rs_cat-intlen   = lo_elem->length.
          rs_cat-decimals = lo_elem->decimals.
        ENDIF.
      CATCH cx_root.                                    "#EC NO_HANDLER
    ENDTRY.
    IF i_text IS NOT INITIAL. "explicit header (pivot bucket value)
      rs_cat-reptext   = i_text.
      rs_cat-scrtext_l = i_text.
      rs_cat-scrtext_m = i_text.
      rs_cat-scrtext_s = i_text.
    ELSEIF rs_cat-reptext IS INITIAL AND rs_cat-scrtext_l IS INITIAL.
      rs_cat-reptext = rs_cat-scrtext_l = rs_cat-scrtext_m = rs_cat-scrtext_s = i_fieldname.
    ENDIF.
  ENDMETHOD.


  METHOD format_sql.
    DATA(l_nl) = cl_abap_char_utilities=>newline.

    "normalize whitespace outside quoted literals, keep literals intact
    SPLIT i_sql AT '''' INTO TABLE DATA(lt_parts).
    DATA(l_n) = lines( lt_parts ).
    LOOP AT lt_parts INTO DATA(l_part).
      DATA(l_i) = sy-tabix.
      IF l_i MOD 2 = 1. "outside quotes
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf   IN l_part WITH ` `.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_part WITH ` `.
        CONDENSE l_part.
        IF l_i > 1.
          rv_sql = |{ rv_sql } |. "space after a closing quote
        ENDIF.
        rv_sql = rv_sql && l_part.
        IF l_i < l_n.
          rv_sql = |{ rv_sql } '|. "space before an opening quote
        ENDIF.
      ELSE. "literal
        rv_sql = rv_sql && l_part && `'`.
      ENDIF.
    ENDLOOP.

    "layout: field list with line breaks, keywords on own lines
    FIND REGEX '\sFROM\s' IN rv_sql IGNORING CASE MATCH OFFSET DATA(l_off).
    IF sy-subrc = 0.
      DATA(l_head) = rv_sql+0(l_off).
      DATA(l_tail_off) = l_off + 1.
      DATA(l_tail) = rv_sql+l_tail_off. "starts with FROM ...
      REPLACE ALL OCCURRENCES OF `,` IN l_head WITH |,{ l_nl }       |.
      REPLACE ALL OCCURRENCES OF REGEX '\s+(LEFT\s+OUTER\s+JOIN|INNER\s+JOIN)\s+'
        IN l_tail WITH |{ l_nl }  $1 | IGNORING CASE.
      REPLACE FIRST OCCURRENCE OF REGEX '\s+WHERE\s+' IN l_tail WITH |{ l_nl } WHERE | IGNORING CASE.
      REPLACE FIRST OCCURRENCE OF REGEX '\s+GROUP\s+BY\s+' IN l_tail WITH |{ l_nl } GROUP BY | IGNORING CASE.
      REPLACE FIRST OCCURRENCE OF REGEX '\s+ORDER\s+BY\s+' IN l_tail WITH |{ l_nl } ORDER BY | IGNORING CASE.
      REPLACE FIRST OCCURRENCE OF REGEX '\s+UP\s+TO\s+' IN l_tail WITH |{ l_nl } UP TO | IGNORING CASE.
      rv_sql = |{ l_head }{ l_nl }  { l_tail }|.
    ENDIF.
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
