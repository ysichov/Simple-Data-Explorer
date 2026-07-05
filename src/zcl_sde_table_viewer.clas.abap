CLASS zcl_sde_table_viewer DEFINITION PUBLIC INHERITING FROM zcl_sde_popup CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_column_emitter,
             column  TYPE lvc_fname,
             emitter TYPE REF TO zcl_sde_transmitter,
           END OF t_column_emitter.

    DATA: m_lang             TYPE ddlanguage,
          m_is_sql           TYPE boolean,
          m_is_view          TYPE boolean,
          m_is_cds           TYPE boolean,
          m_tabname          TYPE tabname,
          m_texttabname      TYPE tabname,
          m_count            TYPE i,
          mo_alv             TYPE REF TO cl_gui_alv_grid,
          mo_sel             TYPE REF TO zcl_sde_sel_opt,
          mr_table           TYPE REF TO data,
          mr_text_table      TYPE REF TO data,
          mo_sel_parent      TYPE REF TO cl_gui_container,
          mo_alv_parent      TYPE REF TO cl_gui_container,
          mt_alv_catalog     TYPE lvc_t_fcat,
          mt_text_components TYPE abap_component_tab,
          mo_column_emitters TYPE TABLE OF t_column_emitter,
          mo_sel_width       TYPE i,
          mo_outer_splitter  TYPE REF TO cl_gui_splitter_container, "data area / docked tools
          mo_tools_parent    TYPE REF TO cl_gui_container,
          mo_tools           TYPE REF TO object, "zcl_sde_tools (untyped to avoid a cycle at activation)
          m_tools_visible    TYPE abap_bool,
          m_visible,
          m_std_tbar         TYPE x,
          m_show_empty,
          m_join_active      TYPE abap_bool.

    METHODS:
      constructor IMPORTING i_tname           TYPE any OPTIONAL
                            ir_tab            TYPE REF TO data OPTIONAL
                            i_additional_name TYPE string OPTIONAL
                            i_is_view         TYPE boolean OPTIONAL
                            i_is_cds          TYPE boolean OPTIONAL,
      get_where RETURNING VALUE(c_where) TYPE string,
      rebind IMPORTING ir_tab     TYPE REF TO data
                       i_name     TYPE string
                       i_generic  TYPE abap_bool DEFAULT abap_false "replace displayed data (new structure allowed)
                       it_catalog TYPE lvc_t_fcat OPTIONAL,         "ready-made catalog (pivot headers)
      refresh_table FOR EVENT selection_done OF zcl_sde_sel_opt.

protected section.
  PRIVATE SECTION.
    METHODS:
      create_popup,
      create_alv,
      create_sel_alv,
      set_header,
      read_text_table,
      update_texts,
      get_field_info IMPORTING i_tab TYPE tabname,
      create_field_cat IMPORTING i_tname           TYPE tabname
                       RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
      create_generic_field_cat RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data,
      on_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid IMPORTING e_object,
      handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
      handle_menu_button  FOR EVENT menu_button OF cl_gui_alv_grid IMPORTING e_object e_ucomm,
      before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
      on_table_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.
ENDCLASS.



CLASS ZCL_SDE_TABLE_VIEWER IMPLEMENTATION.


  METHOD constructor.

    DATA: ls_comp         TYPE abap_componentdescr,
          lt_comp_notab   TYPE abap_component_tab,
          lt_comp_tab2str TYPE abap_component_tab,
          lt_comp_str     TYPE abap_component_tab,
          lv_s            TYPE string,
          lv_data         TYPE REF TO data.

    DATA: l_notab   TYPE REF TO data,
          l_tab2str TYPE REF TO data.

    DATA: handle_notab   TYPE REF TO cl_abap_structdescr,
          handle_tab2str TYPE REF TO cl_abap_structdescr,
          lo_new_tab     TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <notab>   TYPE STANDARD TABLE,
                   <tab2str> TYPE STANDARD TABLE,
                   <any_tab> TYPE ANY TABLE,
                   <temptab> TYPE ANY TABLE.


    super->constructor( i_additional_name = i_additional_name ).
    m_lang = sy-langu.
    mo_sel_width = 0.
    m_tabname = i_tname.
    create_popup( ).

    IF i_is_view = abap_true.
      m_is_view = 'X'.
    ENDIF.

    IF i_is_cds = abap_true.
      m_is_cds = 'X'.
    ENDIF.

    IF m_tabname IS NOT INITIAL. "generic tables (joins, cluster refs) have no DDIC name
      zcl_sde_ddic=>get_text_table( EXPORTING i_tname = m_tabname IMPORTING e_tab = m_texttabname ).
      IF m_texttabname IS NOT INITIAL.
        get_field_info( m_texttabname ).
      ENDIF.
      get_field_info( m_tabname ).
    ENDIF.
    IF ir_tab IS NOT BOUND.
      zcl_sde_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table  ).
      IF m_is_view IS INITIAL.
        m_is_sql = abap_true.
      ENDIF.
    ELSE.

      FIELD-SYMBOLS:<any> TYPE any.
      ASSIGN ir_tab->* TO <any>.
      DATA lo_tabl  TYPE REF TO cl_abap_tabledescr.
      DATA lo_struc TYPE REF TO cl_abap_structdescr.
      lo_tabl ?= cl_abap_typedescr=>describe_by_data( <any> ).
      TRY.
          lo_struc ?= lo_tabl->get_table_line_type( ).
          ASSIGN ir_tab->* TO <any_tab>.
          TRY.

              LOOP AT lo_struc->components INTO DATA(comp) WHERE type_kind NE 'l' AND type_kind NE 'r'. "no ref

                IF comp-type_kind NE 'h'.
                  ls_comp-name = comp-name.
                  ls_comp-type ?= lo_struc->get_component_type( comp-name ).
                  APPEND ls_comp TO lt_comp_notab.
                  APPEND ls_comp TO lt_comp_tab2str.
                ELSE.
                  ls_comp-name = comp-name.
                  ls_comp-type ?= cl_abap_typedescr=>describe_by_data( lv_s ).
                  APPEND ls_comp TO lt_comp_tab2str.
                  APPEND ls_comp TO lt_comp_str.

                  ls_comp-name = comp-name && '_REF'.
                  ls_comp-type ?= cl_abap_typedescr=>describe_by_data( lv_data ).
                  APPEND ls_comp TO lt_comp_tab2str.
                ENDIF.
              ENDLOOP.
            CATCH cx_sy_move_cast_error.
          ENDTRY.

          TRY.
              handle_notab  = cl_abap_structdescr=>create( lt_comp_notab ).
              handle_tab2str  = cl_abap_structdescr=>create( lt_comp_tab2str ).

              lo_new_tab = cl_abap_tabledescr=>create(
                              p_line_type  = handle_notab
                              p_table_kind = cl_abap_tabledescr=>tablekind_std
                              p_unique     = abap_false ).

              CREATE DATA l_notab TYPE HANDLE lo_new_tab.

              lo_new_tab = cl_abap_tabledescr=>create(
                              p_line_type  = handle_tab2str
                              p_table_kind = cl_abap_tabledescr=>tablekind_std
                              p_unique     = abap_false ).

              CREATE DATA l_tab2str TYPE HANDLE lo_new_tab.

              ASSIGN l_notab->* TO <notab>.
              MOVE-CORRESPONDING <any_tab> TO <notab>.
              ASSIGN l_tab2str->* TO <tab2str>.
              MOVE-CORRESPONDING <notab> TO <tab2str>.

              LOOP AT <any_tab> ASSIGNING FIELD-SYMBOL(<old_struc>).
                READ TABLE <tab2str> ASSIGNING FIELD-SYMBOL(<new_struc>) INDEX sy-tabix.
                LOOP AT lt_comp_str INTO ls_comp.
                  ASSIGN COMPONENT ls_comp-name OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<field>).
                  ASSIGN COMPONENT ls_comp-name OF STRUCTURE <old_struc> TO <temptab>.
                  <field> = | { icon_view_table } [{ lines( <temptab> ) }] |.
                  ASSIGN COMPONENT ls_comp-name  OF STRUCTURE <old_struc> TO <field>.
                  ASSIGN COMPONENT |{ ls_comp-name }_REF| OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<ref>).
                  GET REFERENCE OF <field> INTO <ref>.
                ENDLOOP.
              ENDLOOP.

              GET REFERENCE OF <tab2str> INTO mr_table.
            CATCH cx_root.
              mr_table = ir_tab.
          ENDTRY.
        CATCH cx_sy_move_cast_error.  "no structure
          ls_comp-name = 'FIELD'.
          ls_comp-type ?= cl_abap_typedescr=>describe_by_data( lv_s ).
          APPEND ls_comp TO lt_comp_tab2str.

          handle_tab2str  = cl_abap_structdescr=>create( lt_comp_tab2str ).
          lo_new_tab = cl_abap_tabledescr=>create(
                               p_line_type  = handle_tab2str
                               p_table_kind = cl_abap_tabledescr=>tablekind_std
                               p_unique     = abap_false ).

          CREATE DATA l_tab2str TYPE HANDLE lo_new_tab.
          ASSIGN l_tab2str->* TO <tab2str>.
          ASSIGN ir_tab->* TO <any_tab>.

          LOOP AT <any_tab> ASSIGNING <old_struc>.
            APPEND INITIAL LINE TO <tab2str> ASSIGNING <new_struc>.
            ASSIGN COMPONENT 'FIELD' OF STRUCTURE <new_struc> TO <field>.
            <field> = <old_struc>.
          ENDLOOP.
          GET REFERENCE OF <tab2str> INTO mr_table.
      ENDTRY.
    ENDIF.
    create_alv( ).
    create_sel_alv( ).
    mo_alv->set_focus( mo_alv ).
  ENDMETHOD.


  METHOD create_popup.
    mo_box = create( i_width = 800 i_hight = 150 ).

    "outer split: data area on top, docked tools area below (collapsed until needed)
    CREATE OBJECT mo_outer_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_box
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.
    mo_outer_splitter->set_row_mode( mode = mo_outer_splitter->mode_relative ).
    mo_outer_splitter->set_row_height( id = 1 height = 100 ).
    mo_outer_splitter->set_row_sash( id    = 1
                                     type  = cl_gui_splitter_container=>type_sashvisible
                                     value = cl_gui_splitter_container=>false ).
    mo_outer_splitter->set_row_sash( id    = 1
                                     type  = cl_gui_splitter_container=>type_movable
                                     value = cl_gui_splitter_container=>false ).
    mo_outer_splitter->get_container( EXPORTING row = 1 column = 1 RECEIVING container = DATA(lo_main) ).
    mo_outer_splitter->get_container( EXPORTING row = 2 column = 1 RECEIVING container = mo_tools_parent ).

    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = lo_main
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->set_column_mode(  mode = mo_splitter->mode_absolute ).
    mo_splitter->set_column_width( id = 1 width = mo_sel_width ).

    CALL METHOD:
     mo_splitter->get_container(  EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_sel_parent ),

      mo_splitter->get_container
       EXPORTING
        row       = 1
        column    = 2
       RECEIVING
        container = mo_alv_parent.

    SET HANDLER on_box_close FOR mo_box.
  ENDMETHOD.


  METHOD create_alv.
    DATA: ls_layout TYPE lvc_s_layo,
          effect    TYPE i,
          lt_f4     TYPE lvc_t_f4.
    FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE.

    mo_alv = NEW #( i_parent = mo_alv_parent ).
    mt_alv_catalog = create_field_cat( m_tabname ).
    ASSIGN mr_table->* TO <f_tab>.
    IF m_tabname IS NOT INITIAL.
      IF m_is_view  = abap_true.
        CALL FUNCTION 'VIEW_GET_DATA'
          EXPORTING
            view_name = m_tabname
          TABLES
            data      = <f_tab>.
        m_count = lines( <f_tab> ).
      ELSE.
        read_text_table( ).
        zcl_sde_sql=>read_any_table( EXPORTING i_tabname = m_tabname i_where = get_where( ) i_row_count = zcl_sde_appl=>gv_rows
                             CHANGING cr_tab =  mr_table c_count = m_count ).
        update_texts( ).

      ENDIF.
    ENDIF.
    set_header( ).
    ls_layout-col_opt = abap_true.
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode = 'D'.
    CREATE OBJECT zcl_sde_appl=>c_dragdropalv.
    effect = cl_dragdrop=>move + cl_dragdrop=>copy.

    CALL METHOD zcl_sde_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line' ##NO_TEXT
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD zcl_sde_appl=>c_dragdropalv->get_handle IMPORTING handle = DATA(handle_alv).
    ls_layout-s_dragdrop-grid_ddid = handle_alv.
    SET HANDLER   before_user_command
                  handle_user_command
                  handle_menu_button
                  handle_tab_toolbar
                  handle_doubleclick
                  zcl_sde_dragdrop=>drag
                  on_menu_request
                  on_f4 FOR mo_alv.

    CALL METHOD mo_alv->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        i_default       = abap_true
        is_layout       = ls_layout
      CHANGING
        it_fieldcatalog = mt_alv_catalog
        it_outtab       = <f_tab>.

    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mt_alv_catalog ).
    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      CLEAR <catalog>-key.
      DATA(ls_f4) = VALUE  lvc_s_f4( register   = abap_true chngeafter = abap_true fieldname  = <catalog>-fieldname ).
      INSERT ls_f4 INTO TABLE lt_f4.
    ENDLOOP.
    mo_alv->register_f4_for_fields( it_f4 = lt_f4 ).
    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).

    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<cat>) WHERE scrtext_l IS INITIAL.
      Zcl_SDE_common=>translate_field(  CHANGING c_fld = <cat> ).
    ENDLOOP.

    mo_alv->set_frontend_fieldcatalog( EXPORTING  it_fieldcatalog = mt_alv_catalog ).
    me->handle_user_command( EXPORTING e_ucomm = 'HIDE' ).
    "me->handle_user_command( EXPORTING e_ucomm = 'TECH' ).
    mo_alv->set_toolbar_interactive( ).
  ENDMETHOD.


  METHOD create_sel_alv.
    IF mo_sel IS INITIAL.
      mo_sel     = NEW #( io_viewer = me io_container = mo_sel_parent ).
      SET HANDLER refresh_table FOR mo_sel.
    ELSE.
      mo_sel->update_sel_tab( ).
    ENDIF.
  ENDMETHOD.


  METHOD read_text_table.
    FIELD-SYMBOLS: <f_tab> TYPE ANY TABLE.
    zcl_sde_ddic=>get_text_table( EXPORTING i_tname =  m_tabname IMPORTING e_tab = DATA(l_tab) ).
    CHECK l_tab IS NOT INITIAL.
    zcl_sde_rtti=>create_table_by_name( EXPORTING i_tname = l_tab CHANGING c_table = mr_text_table ).
    ASSIGN mr_text_table->* TO <f_tab>.
    SELECT * FROM (l_tab) INTO TABLE <f_tab> ORDER BY PRIMARY KEY.
  ENDMETHOD.


  METHOD set_header.
    DATA: lv_text       TYPE as4text,
          lv_header     TYPE text200.

    IF zcl_sde_appl=>gv_vname IS INITIAL.
      SELECT SINGLE ddtext INTO lv_text
        FROM dd02t
       WHERE tabname = m_tabname
         AND ddlanguage = m_lang.
    ELSE.
      SELECT SINGLE ddtext INTO lv_text
         FROM dd25t
        WHERE viewname = m_tabname
          AND ddlanguage = m_lang.
    ENDIF.

    lv_header = |Simple Data Explorer v.2 SelecTor - { m_tabname } - { lv_text } ({ m_count }) { m_additional_name }|.
    mo_box->set_caption(  lv_header  ).
  ENDMETHOD.


  METHOD on_f4.
    FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <tab>.
    READ TABLE <tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<f4_row>).
    GET REFERENCE OF <f4_row> INTO zcl_sde_appl=>gr_current_row.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST' ##FM_SUBRC_OK
      EXPORTING
        tabname           = m_tabname
        fieldname         = e_fieldname
        callback_program  = sy-repid
        callback_form     = 'CALLBACK_F4_TAB' "callback_method - doesn't work for local class
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
  ENDMETHOD.


  METHOD on_menu_request.
    DATA: l_smenu TYPE REF TO cl_ctmenu.

    mo_alv->get_current_cell( IMPORTING
        es_row_id = DATA(ls_row)
        es_col_id = DATA(ls_col) ).

    e_object->add_separator( ).
    CREATE OBJECT l_smenu.

    SELECT SINGLE dbtab INTO @DATA(l_dbtab)
      FROM t777d
      WHERE dbtab = @m_tabname.

    IF sy-subrc = 0.
      CALL METHOD l_smenu->add_function
        EXPORTING
          fcode = 'DETAIL' ##NO_TEXT
          text  = 'Show object' ##NO_TEXT.

      IF l_dbtab+0(2) = 'PA'.
        CALL METHOD l_smenu->add_function
          EXPORTING
            fcode = 'PY' ##NO_TEXT
            text  = 'PaYroll Clusters' ##NO_TEXT.
      ENDIF.
    ENDIF.

    CALL METHOD e_object->add_submenu
      EXPORTING
        menu = l_smenu
        text = 'Data Driven Jumps'.
  ENDMETHOD.


  METHOD handle_tab_toolbar.
    IF m_visible IS INITIAL.
      DATA(lt_toolbar) = VALUE ttb_button(
       ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options' butn_type = 0 )
       ( butn_type = 3 ) ).
    ENDIF.

    lt_toolbar = VALUE ttb_button( BASE lt_toolbar
     ( function = 'REFRESH'  icon = icon_refresh quickinfo = 'Refresh' butn_type = 0 )
     ( function = 'LANGUAGE' icon = icon_foreign_trade quickinfo = 'Languages' butn_type = 2 )
     ( function = 'OPTIONS'  icon = icon_list          quickinfo = 'Empty columns options'   butn_type = 2 )
     ( function = 'TABLES'   icon = icon_net_graphic   quickinfo = 'Table links'   butn_type = 0 )
     ( function = 'TOOLS'    icon = icon_tools quickinfo = 'Tools: join, pivot' butn_type = 0 )
     ( function = 'TBAR' icon = COND #( WHEN m_std_tbar IS INITIAL THEN icon_column_right ELSE icon_column_left )
        quickinfo = COND #( WHEN m_std_tbar IS INITIAL THEN 'Show standard ALV function'  ELSE 'Hide standard ALV function') )
     ( butn_type = 3 ) ).

    IF m_std_tbar IS INITIAL.
      e_object->mt_toolbar =  lt_toolbar.
    ELSE.
      e_object->mt_toolbar =  lt_toolbar = VALUE ttb_button( BASE lt_toolbar ( LINES OF e_object->mt_toolbar ) ).
    ENDIF.
  ENDMETHOD.


  METHOD get_field_info.
    DATA: lv_clause      TYPE string,
          lr_struc       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          lt_field_info  TYPE TABLE OF dfies,
          l_fname        TYPE fieldname,
          l_tname        TYPE tabname,
          ls_tf          LIKE LINE OF Zcl_SDE_common=>mt_tabfields,
          dref           TYPE REF TO data,
          l_x            TYPE xstring.

    CREATE DATA lr_struc TYPE (i_tab).
    lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_struc ).
    it_tabdescr[] = lr_table_descr->components[].

    DATA(l_exist) = zcl_sde_sql=>exist_table( i_tab ).
    IF  l_exist = 1.
      SELECT  COUNT( * ) FROM (i_tab).
      DATA(l_count) = sy-dbcnt.
    ENDIF.

    LOOP AT it_tabdescr INTO DATA(ls) WHERE name NE 'MANDT' AND name NE 'CLIENT'.
      IF NOT line_exists( Zcl_SDE_common=>mt_tabfields[ tabname = i_tab fieldname = ls-name ] ).
        l_tname = i_tab.
        l_fname = ls-name.

        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = l_tname
            fieldname      = l_fname
            langu          = sy-langu
          TABLES
            dfies_tab      = lt_field_info
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.

        CHECK sy-subrc = 0.
        CLEAR ls_tf.
        MOVE-CORRESPONDING lt_field_info[ 1 ] TO ls_tf.

        "check empty field
        IF l_exist = 1 AND l_count < 10000.
          IF ls_tf-rollname IS NOT INITIAL.
            CREATE DATA dref TYPE (ls_tf-rollname).
            ASSIGN dref->* TO FIELD-SYMBOL(<field>).
            lv_clause = |{ ls_tf-fieldname } NE ''|.
            SELECT SINGLE (ls_tf-fieldname) INTO @<field>
              FROM (i_tab)
             WHERE (lv_clause).
            IF sy-subrc NE 0.
              ls_tf-empty = abap_true.
            ENDIF.
          ELSEIF ls_tf-datatype = 'RAWSTRING'.
            lv_clause = |{ ls_tf-fieldname } NE ''|.
            SELECT SINGLE (ls_tf-fieldname) INTO @l_x
              FROM (i_tab)
             WHERE (lv_clause).
            IF sy-subrc NE 0.
              ls_tf-empty = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
        INSERT ls_tf INTO TABLE Zcl_SDE_common=>mt_tabfields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_field_cat.
    DATA: lr_struc       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          l_replace      TYPE string,
          l_texttab      TYPE tabname,
          lo_str         TYPE REF TO cl_abap_structdescr.

    IF i_tname IS INITIAL. "dynamically built table (join result etc.): catalog from RTTI
      et_catalog = create_generic_field_cat( ).
      RETURN.
    ENDIF.

    zcl_sde_rtti=>create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_t_comp = mt_text_components e_handle = lo_str ).
    CREATE DATA lr_struc TYPE HANDLE lo_str.
    lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_struc ).
    it_tabdescr[] = lr_table_descr->components[].
    zcl_sde_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = l_texttab ).
    l_replace = l_texttab && '_'.

    LOOP AT it_tabdescr INTO DATA(ls) WHERE name NE 'MANDT' AND name NE 'CLIENT'.
      DATA(l_ind) = sy-tabix.
      APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      <catalog>-col_pos = l_ind.
      READ TABLE Zcl_SDE_common=>mt_tabfields INTO DATA(ls_tf) WITH KEY tabname = i_tname fieldname = ls-name.

      <catalog>-style = Zcl_SDE_common=>c_white.
      MOVE-CORRESPONDING ls_tf TO <catalog>.
      <catalog>-f4availabl = abap_true.
      IF ls_tf-is_text = abap_true.
        <catalog>-style = Zcl_SDE_common=>c_grey.
      ENDIF.

      IF ls_tf-checktable IS NOT INITIAL.
        <catalog>-style = Zcl_SDE_common=>c_blue.
      ENDIF.

      IF line_exists( zcl_sde_plugins=>mt_field_links[ tab = i_tname field = ls_tf-fieldname ] ).
        <catalog>-style = Zcl_SDE_common=>c_green.
      ENDIF.

      IF line_exists( zcl_sde_plugins=>mt_el_links[ element = ls_tf-rollname ] ).
        <catalog>-style = Zcl_SDE_common=>c_green.
      ENDIF.

      IF ls_tf-keyflag = abap_true.
        <catalog>-style = <catalog>-style BIT-OR Zcl_SDE_common=>c_bold.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD rebind.
    DATA ls_layout TYPE lvc_s_layo.
    FIELD-SYMBOLS <f_tab> TYPE STANDARD TABLE.

    CHECK mo_alv IS BOUND.
    mr_table = ir_tab.
    m_additional_name = i_name.
    ASSIGN mr_table->* TO <f_tab>.
    m_count = lines( <f_tab> ).
    IF it_catalog IS NOT INITIAL. "caller provides the headers (pivot columns)
      mt_alv_catalog = it_catalog.
      m_join_active = abap_true.
    ELSEIF i_generic = abap_true.
      mt_alv_catalog = create_generic_field_cat( ).
      m_join_active = abap_true.
    ELSE.
      mt_alv_catalog = create_field_cat( m_tabname ).
      CLEAR m_join_active.
    ENDIF.

    ls_layout-col_opt    = abap_true.
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'D'.
    mo_alv->set_table_for_first_display(
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = <f_tab>
        it_fieldcatalog = mt_alv_catalog
      EXCEPTIONS
        OTHERS          = 1 ).
    set_header( ).
    Zcl_SDE_common=>refresh( mo_alv ).

    "the select-options panel must follow the new field list
    IF mo_sel IS BOUND.
      mo_sel->update_sel_tab( ).
      Zcl_SDE_common=>refresh( mo_sel->mo_sel_alv ).
    ENDIF.
  ENDMETHOD.


  METHOD create_generic_field_cat.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    CHECK mr_table IS BOUND.
    ASSIGN mr_table->* TO <tab>.
    DATA(lo_struc) = CAST cl_abap_structdescr(
      CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <tab> ) )->get_table_line_type( ) ).

    LOOP AT lo_struc->components INTO DATA(ls_comp) WHERE type_kind NE cl_abap_typedescr=>typekind_table
                                                      AND type_kind NE cl_abap_typedescr=>typekind_dref
                                                      AND type_kind NE cl_abap_typedescr=>typekind_oref.
      APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      <catalog>-col_pos   = sy-tabix.
      <catalog>-fieldname = ls_comp-name.
      <catalog>-style     = Zcl_SDE_common=>c_white.
      TRY.
          DATA(lo_elem) = CAST cl_abap_elemdescr( lo_struc->get_component_type( ls_comp-name ) ).
          IF lo_elem->is_ddic_type( ) = abap_true.
            DATA(ls_dfies) = lo_elem->get_ddic_field( ).
            MOVE-CORRESPONDING ls_dfies TO <catalog>.
            <catalog>-fieldname = ls_comp-name. "keep the component name, not the element's
            CLEAR: <catalog>-tabname, <catalog>-key.
          ELSE.
            <catalog>-inttype  = lo_elem->type_kind.
            <catalog>-intlen   = lo_elem->length.
            <catalog>-decimals = lo_elem->decimals.
          ENDIF.
        CATCH cx_root.                                  "#EC NO_HANDLER
      ENDTRY.
      IF <catalog>-reptext IS INITIAL AND <catalog>-scrtext_l IS INITIAL.
        <catalog>-reptext = <catalog>-scrtext_l = <catalog>-scrtext_m = <catalog>-scrtext_s = ls_comp-name.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_menu_button.
    CALL METHOD cl_gui_cfw=>flush.
    IF e_ucomm = 'LANGUAGE'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TECH'
          text  = 'Technical name'. "Teхническое имя
      LOOP AT zcl_sde_appl=>mt_lang INTO DATA(ls_lang).
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = CONV #( ls_lang-spras )
            text  = CONV #( ls_lang-sptxt ).
      ENDLOOP.
    ELSEIF e_ucomm = 'OPTIONS'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'HIDE'
          text  = 'Hide empty columns'. "Спрятать пустые столбцы
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'SHOW'
          text  = 'Show empty columns'. "Отобразить пустые столбцы
    ENDIF.
  ENDMETHOD.


  METHOD handle_doubleclick.
    DATA: lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone    TYPE REF TO data.

    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD TABLE.
    CHECK es_row_no-row_id IS NOT INITIAL.
    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    zcl_sde_plugins=>link( EXPORTING i_str = <tab> i_column = e_column io_viewer = me ).

    ASSIGN COMPONENT |{ e_column-fieldname }_REF| OF STRUCTURE <tab> TO FIELD-SYMBOL(<ref>).
    IF sy-subrc = 0.
      zcl_sde_appl=>open_int_table( EXPORTING iv_name = CONV #( e_column-fieldname ) it_ref = <ref> ).
    ENDIF.
  ENDMETHOD.


  METHOD before_user_command.
    CASE e_ucomm.
      WHEN '&INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
    ENDCASE.
  ENDMETHOD.


  METHOD on_table_close.
    DATA: lv_tabix LIKE sy-tabix.
    sender->free( ).

    "Free Memory
    LOOP AT zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
      IF <obj>-alv_viewer->mo_box = sender.
        lv_tabix = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.
      FREE <obj>-alv_viewer->mr_table.
      FREE <obj>-alv_viewer->mo_alv.

      "shutdown receivers.
      IF <obj>-alv_viewer->mo_sel IS NOT INITIAL.
        LOOP AT <obj>-alv_viewer->mo_sel->mt_sel_tab INTO DATA(l_sel).
          IF l_sel-receiver IS BOUND.
            l_sel-receiver->shut_down( ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      FREE <obj>-alv_viewer.
      DELETE zcl_sde_appl=>mt_obj INDEX lv_tabix.
    ENDIF.
  ENDMETHOD.


  METHOD handle_user_command.
    DATA:
      it_fields     TYPE lvc_t_fcat,
      lv_clause(45),
      lv_sel_width  TYPE i.

    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD  TABLE.

    ASSIGN mr_table->* TO <f_tab>.
    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = it_fields[] ).
    IF e_ucomm = 'SEL_ON' AND m_visible IS INITIAL.
      create_sel_alv( ).
      m_visible = abap_true.
      IF mo_sel_width = 0.
        lv_sel_width = 500.
      ELSE.
        lv_sel_width = mo_sel_width.
      ENDIF.

      mo_splitter->set_column_width( EXPORTING id    = 1 width = lv_sel_width ).
      mo_alv->set_toolbar_interactive( ).
      RETURN.
    ELSEIF e_ucomm = 'PY'.
      READ TABLE <f_tab> INDEX Zcl_SDE_common=>get_selected( mo_alv ) ASSIGNING FIELD-SYMBOL(<f_line>).
      zcl_sde_plugins=>run_hrpy_rgdir( <f_line> ).
    ELSEIF e_ucomm = 'DETAIL'.
      IF m_tabname+0(2) = 'PA'.
        zcl_sde_plugins=>run_pa20( me ).
      ELSEIF m_tabname+0(3) = 'HRP'.
        zcl_sde_plugins=>run_pp01( me ).
      ENDIF.
    ELSEIF e_ucomm = 'REFRESH'.
      "CHECK get_where( ) IS NOT INITIAL.
      mo_sel->raise_selection_done( ).
      IF zcl_sde_sql=>exist_table( m_tabname ) = 1.
        m_is_sql = 'X'.
      ENDIF.
    ELSEIF e_ucomm = 'TBAR'.
      m_std_tbar = BIT-NOT  m_std_tbar.
    ELSEIF e_ucomm = 'TOOLS'. "dock the tools area below the data
      IF m_tabname IS INITIAL OR ( zcl_sde_sql=>exist_table( m_tabname ) NE 1 AND zcl_sde_sql=>exist_view( m_tabname ) NE 1 ).
        MESSAGE 'Tools need a database table or view' TYPE 'S' DISPLAY LIKE 'E'.
      ELSEIF mo_tools IS NOT BOUND.
        mo_box->set_height( height = 600 ). "make room
        mo_outer_splitter->set_row_mode( mode = mo_outer_splitter->mode_absolute ).
        mo_outer_splitter->set_row_height( id = 1 height = 155 ).
        mo_outer_splitter->set_row_sash( id    = 1
                                         type  = cl_gui_splitter_container=>type_sashvisible
                                         value = cl_gui_splitter_container=>true ).
        mo_outer_splitter->set_row_sash( id    = 1
                                         type  = cl_gui_splitter_container=>type_movable
                                         value = cl_gui_splitter_container=>true ).
        mo_tools = NEW zcl_sde_tools( io_viewer = me io_parent = mo_tools_parent ).
        m_tools_visible = abap_true.
      ELSE. "toggle
        m_tools_visible = boolc( m_tools_visible = abap_false ).
        IF m_tools_visible = abap_true.
          mo_outer_splitter->set_row_mode( mode = mo_outer_splitter->mode_absolute ).
          mo_outer_splitter->set_row_height( id = 1 height = 155 ).
        ELSE.
          mo_outer_splitter->set_row_mode( mode = mo_outer_splitter->mode_relative ).
          mo_outer_splitter->set_row_height( id = 1 height = 100 ).
        ENDIF.
        mo_outer_splitter->set_row_sash( id    = 1
                                         type  = cl_gui_splitter_container=>type_sashvisible
                                         value = COND #( WHEN m_tools_visible = abap_true
                                                          THEN cl_gui_splitter_container=>true
                                                          ELSE cl_gui_splitter_container=>false ) ).
        mo_outer_splitter->set_row_sash( id    = 1
                                         type  = cl_gui_splitter_container=>type_movable
                                         value = COND #( WHEN m_tools_visible = abap_true
                                                          THEN cl_gui_splitter_container=>true
                                                          ELSE cl_gui_splitter_container=>false ) ).
      ENDIF.
      RETURN.
    ELSEIF e_ucomm = 'TABLES'.
      DATA(lt_obj) = VALUE sdg1_obj( (  obj_name = m_tabname type = 'TABL' )  ).
      CALL FUNCTION 'REPOSITORY_STRUCTURE_GRAPH'
        EXPORTING
          type    = 'TABL'
        TABLES
          objects = lt_obj.
      SET TITLEBAR 'SDE'.
      RETURN.
    ELSE.
      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<fields>) WHERE domname NE 'MANDT'.
        <fields>-col_pos = sy-tabix.
        CASE e_ucomm.
          WHEN 'HIDE'. "hide select options
            IF <fields>-tabname = m_tabname AND lines( <f_tab> ) > 0.
              CLEAR m_show_empty.
              lv_clause = |{ <fields>-fieldname } IS NOT INITIAL|.
              LOOP AT <f_tab> ASSIGNING <f_line>  WHERE (lv_clause).
                EXIT.
              ENDLOOP.
              IF sy-subrc NE 0.
                <fields>-no_out = abap_true.
              ENDIF.
            ENDIF.
          WHEN 'SHOW'.
            m_show_empty = abap_true.
            <fields>-no_out = ' '.
          WHEN 'UPDATE'.
            lv_clause = |{ <fields>-fieldname } IS NOT INITIAL|.
            LOOP AT <f_tab> ASSIGNING <f_line>  WHERE (lv_clause).
              EXIT.
            ENDLOOP.
            IF sy-subrc = 0.
              <fields>-no_out = ''.
            ENDIF.
          WHEN 'TECH'. "technical field name
            <fields>-scrtext_l = <fields>-scrtext_m = <fields>-scrtext_s =  <fields>-reptext = <fields>-fieldname.
          WHEN OTHERS. "header names translation
            IF line_exists( zcl_sde_appl=>mt_lang[ spras = e_ucomm ] ).
              Zcl_SDE_common=>translate_field( EXPORTING i_lang = CONV #( e_ucomm ) CHANGING c_fld = <fields> ).
              IF mo_sel IS BOUND.
                READ TABLE mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) WITH KEY field_label = <fields>-fieldname.
                IF sy-subrc = 0.
                  <sel>-name = <fields>-scrtext_l.
                  IF <sel>-name IS INITIAL.
                    <sel>-name = <fields>-reptext.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF line_exists( zcl_sde_appl=>mt_lang[ spras = e_ucomm ] ).
      m_lang = e_ucomm.
      set_header( ).
      update_texts( ).
      mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang  ).
    ENDIF.

    CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].
    Zcl_SDE_common=>refresh( mo_alv ).
    IF mo_sel IS BOUND.
      IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
        mo_sel->update_sel_tab( ).
      ENDIF.
      Zcl_SDE_common=>refresh( mo_sel->mo_sel_alv ).
    ENDIF.
  ENDMETHOD.                           "handle_user_command


  METHOD get_where."dynamic where clause
    DATA: lt_where TYPE rsds_twhere,
          lt_range TYPE rsds_trange.

    IF  mo_sel IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_range ASSIGNING FIELD-SYMBOL(<tabl>).
      <tabl>-tablename = m_tabname.
      LOOP AT mo_sel->mt_sel_tab INTO DATA(ls_tab) WHERE range IS NOT INITIAL.
        APPEND INITIAL LINE TO <tabl>-frange_t ASSIGNING FIELD-SYMBOL(<t_range>).
        IF sy-subrc = 0.
          <t_range>-fieldname = ls_tab-field_label.
          <t_range>-selopt_t  = ls_tab-range.
        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
        EXPORTING
          field_ranges  = lt_range
        IMPORTING
          where_clauses = lt_where.

      LOOP AT lt_where INTO DATA(ls_where) WHERE tablename = m_tabname.
        LOOP AT ls_where-where_tab INTO DATA(l_where).
          CONDENSE l_where-line.
          c_where = |{ c_where } { l_where-line }|.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD refresh_table.
    DATA: ls_row    TYPE zcl_sde_appl=>t_sel_row,
          lt_filter TYPE lvc_t_filt.
    CHECK m_join_active = abap_false.
    IF m_is_sql = abap_true.
      DATA(l_where) = get_where( ).
      zcl_sde_sql=>read_any_table( EXPORTING i_tabname = m_tabname i_where = l_where i_row_count = zcl_sde_appl=>gv_rows CHANGING cr_tab =  mr_table c_count = m_count ).
      IF l_where IS INITIAL.
        CLEAR m_is_sql.
      ENDIF.
    ELSE.
      CLEAR lt_filter.
    ENDIF.
    update_texts( ).
    set_header( ).

    LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      ENDIF.
      IF m_is_sql = abap_false.
        LOOP AT <sel>-range INTO DATA(l_range).
          APPEND VALUE #( fieldname = <sel>-field_label
                                low = l_range-low
                               high = l_range-high
                               sign = l_range-sign
                             option = l_range-opti ) TO lt_filter.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    CALL METHOD mo_alv->set_filter_criteria
      EXPORTING
        it_filter = lt_filter.

    Zcl_SDE_common=>refresh( mo_sel->mo_sel_alv ).
    Zcl_SDE_common=>refresh( mo_alv ).
    mo_sel->mo_viewer->handle_user_command( 'UPDATE' ).
    mo_sel->mo_viewer->handle_user_command( 'HIDE' ).
    LOOP AT mo_column_emitters INTO DATA(l_emit).
      l_emit-emitter->emit_col( l_emit-column ).
    ENDLOOP.
  ENDMETHOD.


  METHOD update_texts.
    DATA: l_text_field TYPE fieldname,
          l_replace    TYPE string,
          lv_clause    TYPE string.

    FIELD-SYMBOLS: <f_tab> TYPE ANY TABLE.
    FIELD-SYMBOLS: <text_tab> TYPE  STANDARD TABLE,
                   <check>    TYPE any.

    CHECK m_texttabname IS NOT INITIAL.

    "text fields
    ASSIGN mr_text_table->* TO <text_tab>.
    READ TABLE <text_tab> INDEX 1 ASSIGNING FIELD-SYMBOL(<text_dummy>).
    CHECK sy-subrc = 0.

    READ TABLE Zcl_SDE_common=>mt_tabfields WITH KEY tabname = m_texttabname domname = 'SPRAS' INTO DATA(l_texttabfield).
    IF sy-subrc = 0.
      DATA(l_lang) = l_texttabfield-fieldname.
    ENDIF.

    l_replace = m_texttabname && '_'.
    ASSIGN mr_table->* TO <f_tab>.

    LOOP AT <f_tab> ASSIGNING FIELD-SYMBOL(<str>).
      CLEAR lv_clause.
      LOOP AT Zcl_SDE_common=>mt_tabfields INTO l_texttabfield WHERE tabname = m_texttabname AND keyflag = abap_true.
        UNASSIGN <check>.
        ASSIGN COMPONENT l_texttabfield-fieldname OF STRUCTURE <str> TO <check>.
        IF sy-subrc NE 0.
          READ TABLE Zcl_SDE_common=>mt_tabfields WITH KEY tabname = m_texttabname fieldname  = l_texttabfield-fieldname INTO DATA(l_texttab).
          READ TABLE Zcl_SDE_common=>mt_tabfields WITH KEY tabname = m_tabname domname  = l_texttab-domname INTO DATA(l_maintab).
          ASSIGN COMPONENT l_maintab-fieldname OF STRUCTURE <str> TO <check>.
          CLEAR l_maintab.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF lv_clause IS INITIAL.
          lv_clause = |{ l_texttabfield-fieldname } = '{ <check> }'|.
        ELSE.
          lv_clause = |{ lv_clause } AND { l_texttabfield-fieldname } = '{ <check> }'|.
        ENDIF.
      ENDLOOP.

      IF l_lang IS NOT INITIAL.
        ASSIGN COMPONENT l_lang OF STRUCTURE <text_dummy> TO FIELD-SYMBOL(<dummy>).
        IF sy-subrc = 0.
          lv_clause = |{ lv_clause } AND { l_lang } = '{ m_lang }'|.
        ENDIF.
      ENDIF.

      LOOP AT <text_tab> ASSIGNING FIELD-SYMBOL(<text_str>)  WHERE (lv_clause).
        EXIT.
      ENDLOOP.
      CHECK sy-subrc = 0.

      LOOP AT mt_text_components INTO DATA(ls_comp).
        l_text_field = ls_comp-name.
        REPLACE l_replace IN l_text_field WITH ''.
        ASSIGN COMPONENT ls_comp-name OF STRUCTURE <str> TO FIELD-SYMBOL(<to>).
        ASSIGN COMPONENT l_text_field OF STRUCTURE <text_str> TO FIELD-SYMBOL(<from>).
        <to> = <from>.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
