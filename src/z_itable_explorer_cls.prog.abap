
CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.
CLASS lcl_rtti_tree DEFINITION DEFERRED.
CLASS lcl_window DEFINITION DEFERRED.
CLASS lcl_table_viewer DEFINITION DEFERRED.

CLASS lcl_box_handler DEFINITION."for memory clearing
  PUBLIC SECTION.
    METHODS: on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.
ENDCLASS.


CLASS lcl_appl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF sign_option_icon_s,
             sign          TYPE tvarv_sign,
             option        TYPE tvarv_opti,
             icon_name(64) TYPE c,
             icon          TYPE aqadh_type_of_icon,
           END OF sign_option_icon_s,

           BEGIN OF var_table,
             step    TYPE i,
             stack   TYPE i,
             first   TYPE xfeld,
             leaf    TYPE string,
             name    TYPE string,
             short   TYPE string,
             key     TYPE salv_de_node_key,
             cl_leaf TYPE int4,
             ref     TYPE REF TO data,
             tree    TYPE REF TO lcl_rtti_tree,
             class   TYPE string,
           END OF var_table,

           BEGIN OF var_table_h,
             step    TYPE i,
             leaf    TYPE string,
             name    TYPE string,
             short   TYPE string,
             cl_leaf TYPE int4,
             ref     TYPE REF TO data,
             tree    TYPE REF TO lcl_rtti_tree,
           END OF var_table_h,


           BEGIN OF var_table_temp,
             step  TYPE i,
             stack TYPE i,
             first TYPE xfeld,
             leaf  TYPE string,
             name  TYPE string,
             short TYPE string,
             class TYPE string,
           END OF var_table_temp,

           BEGIN OF t_obj,
             name       TYPE string,
             alv_viewer TYPE REF TO lcl_table_viewer,
           END OF t_obj,

           BEGIN OF t_classes_types,
             name TYPE string,
             full TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_types,

           BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang  ,
           BEGIN OF t_stack,
             stackpointer TYPE tpda_stack_pointer,
             stacklevel   TYPE tpda_stack_level,
             program      TYPE tpda_program,
             include      TYPE tpda_include,
             line         TYPE tpda_sc_line,
             eventtype    TYPE tpda_event_type,
             eventname    TYPE tpda_event,
           END OF t_stack,

           BEGIN OF t_step_counter,
             step       TYPE i,
             stacklevel TYPE tpda_stack_level,
             program    TYPE tpda_program,
             include    TYPE tpda_include,
             line       TYPE tpda_sc_line,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
           END OF t_step_counter.

    CLASS-DATA: m_option_icons     TYPE TABLE OF sign_option_icon_s,
                mt_lang            TYPE TABLE OF t_lang,
                mt_obj             TYPE TABLE OF t_obj, "main object table
                m_ctrl_box_handler TYPE REF TO lcl_box_handler,
                c_dragdropalv      TYPE REF TO cl_dragdrop.

    CLASS-METHODS:
      init_icons_table,
      init_lang,
      open_int_table IMPORTING it_tab  TYPE ANY TABLE OPTIONAL
                               it_ref  TYPE REF TO data OPTIONAL
                               iv_name TYPE string
                               iv_dummy type xfeld OPTIONAL
                               iv_show  type xfeld OPTIONAL
                               i_tname TYPE string OPTIONAL.
ENDCLASS.

CLASS lcl_popup DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA m_counter              TYPE i.
    DATA: m_additional_name      TYPE string,
          mo_box                 TYPE REF TO cl_gui_dialogbox_container,
          mo_splitter            TYPE REF TO cl_gui_splitter_container,
          mo_variables_container TYPE REF TO cl_gui_container.

    METHODS: constructor IMPORTING i_additional_name TYPE string OPTIONAL,
      create IMPORTING i_width       TYPE i
                       i_hight       TYPE i
                       i_name        TYPE text100 OPTIONAL
             RETURNING VALUE(ro_box) TYPE REF TO cl_gui_dialogbox_container,

      on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.
ENDCLASS.

CLASS lcl_popup IMPLEMENTATION.

  METHOD constructor.
    m_additional_name = i_additional_name.
  ENDMETHOD.

  METHOD create.
    DATA: l_top  TYPE i,
          l_left TYPE i.

    ADD 1 TO m_counter.
    l_top  = l_left = 5 + 5 * ( m_counter DIV 5 ) +  ( m_counter MOD 5 ) * 50.

    CREATE OBJECT ro_box
      EXPORTING
        width                       = i_width
        height                      = i_hight
        top                         = l_top
        left                        = l_left
        caption                     = i_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD on_box_close.
    sender->free( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_ddic DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_text_table IMPORTING i_tname TYPE tabname
                                  EXPORTING e_tab   TYPE tabname.
ENDCLASS.

CLASS lcl_ddic IMPLEMENTATION.
  METHOD get_text_table.
    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname   = i_tname
      IMPORTING
        texttable = e_tab.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_dd_data DEFINITION."drag&drop data
  PUBLIC  SECTION.
    DATA: m_row    TYPE i,
          m_column TYPE lvc_s_col.
ENDCLASS.

CLASS lcl_dragdrop DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      drag FOR EVENT ondrag OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row e_column ,
      drop FOR EVENT ondrop OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row.
ENDCLASS.

CLASS lcl_sql DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      read_any_table IMPORTING i_tabname   TYPE tabname
                               i_where     TYPE string
                               i_row_count TYPE i OPTIONAL
                     CHANGING  cr_tab      TYPE REF TO data
                               c_count     TYPE i,
      exist_table IMPORTING i_tab TYPE tabname RETURNING VALUE(e_subrc) LIKE sy-subrc,
      exist_view  IMPORTING i_tab TYPE tabname RETURNING VALUE(e_subrc) LIKE sy-subrc,
      exist_cds   IMPORTING i_tab TYPE tabname RETURNING VALUE(e_subrc) LIKE sy-subrc  .
ENDCLASS.

CLASS lcl_sql IMPLEMENTATION.
  METHOD read_any_table.
    FIELD-SYMBOLS: <f_tab> TYPE ANY TABLE.

    ASSIGN cr_tab->* TO <f_tab>.
    c_count = lines( <f_tab> ).
    CHECK lcl_sql=>exist_table( i_tabname ) = 1.
    IF i_where IS NOT INITIAL.
      TRY.
          SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF  TABLE <f_tab> WHERE (i_where) ORDER BY PRIMARY KEY
           .
        CATCH cx_sy_dynamic_osql_semantics.             "#EC NO_HANDLER
        CATCH cx_sy_dynamic_osql_syntax.                "#EC NO_HANDLER
        CATCH cx_sy_conversion_no_number.               "#EC NO_HANDLER
      ENDTRY.
    ELSE.
      IF i_row_count IS NOT SUPPLIED.
        SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF TABLE <f_tab> ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF TABLE <f_tab> UP TO i_row_count ROWS ORDER BY PRIMARY KEY..
      ENDIF.
    ENDIF.
    c_count = sy-dbcnt.
  ENDMETHOD.

  METHOD exist_table.
    SELECT COUNT( * ) FROM dd02l
     WHERE tabname = i_tab
       AND ( tabclass = 'TRANSP' OR tabclass = 'CLUSTER' ).
    e_subrc = sy-dbcnt.
  ENDMETHOD.

  METHOD exist_view.
    SELECT COUNT( * ) FROM dd02l
     WHERE tabname = i_tab
       AND tabclass = 'VIEW'.
    e_subrc = sy-dbcnt.
  ENDMETHOD.

  METHOD exist_cds.
    SELECT COUNT( * ) FROM dd02l
     WHERE tabname = i_tab
       AND tabclass = 'VIEW'
       AND applclass = 'SDGV'.
    e_subrc = sy-dbcnt.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_alv_common DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: c_white(4) TYPE x VALUE '00000001', "white background
               c_grey(4)  TYPE x VALUE '00000003', "gray background
               c_green(4) TYPE x VALUE '00000216', "green +underline
               c_blue(4)  TYPE x VALUE '00000209', " blue font +underline
               c_bold(4)  TYPE x VALUE '00000020'.

    TYPES: BEGIN OF t_tabfields.
             INCLUDE TYPE   dfies.
             TYPES: empty   TYPE xfeld,
             is_text TYPE xfeld,
           END OF t_tabfields.

    CLASS-DATA: mt_tabfields TYPE HASHED TABLE OF t_tabfields WITH UNIQUE KEY tabname fieldname.


    CLASS-METHODS:
      refresh IMPORTING i_obj TYPE REF TO cl_gui_alv_grid i_layout TYPE lvc_s_layo OPTIONAL i_soft TYPE char01 OPTIONAL,
      translate_field IMPORTING i_lang TYPE ddlanguage OPTIONAL CHANGING c_fld TYPE lvc_s_fcat,
      get_selected IMPORTING i_obj TYPE REF TO cl_gui_alv_grid RETURNING VALUE(e_index) TYPE i.
ENDCLASS.

CLASS lcl_alv_common IMPLEMENTATION.
  METHOD refresh.
    DATA l_stable TYPE lvc_s_stbl.
    l_stable = 'XX'.
    IF i_layout IS SUPPLIED.
      i_obj->set_frontend_layout( i_layout ) .
    ENDIF.
    i_obj->refresh_table_display( EXPORTING is_stable = l_stable i_soft_refresh = i_soft  ).
  ENDMETHOD.

  METHOD translate_field.
    DATA: lt_field_info TYPE TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = c_fld-tabname
        fieldname      = c_fld-fieldname
        langu          = i_lang
      TABLES
        dfies_tab      = lt_field_info
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      READ TABLE lt_field_info INDEX 1 INTO DATA(l_info).
      IF l_info-scrtext_l IS INITIAL AND l_info-scrtext_m IS INITIAL AND l_info-scrtext_s IS INITIAL.
        IF l_info-fieldtext IS NOT INITIAL.
          MOVE l_info-fieldtext TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ELSE.
          MOVE l_info-fieldname TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ENDIF.
      ELSE.
        c_fld-scrtext_l = l_info-scrtext_l.
        c_fld-scrtext_m = l_info-scrtext_m.
        c_fld-scrtext_s = l_info-scrtext_s.
        IF l_info-reptext IS NOT INITIAL.
          c_fld-reptext   = l_info-reptext.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_selected.
    i_obj->get_selected_cells( IMPORTING et_cell = DATA(lt_sel_cells) ).
    IF lines( lt_sel_cells ) > 0.
      e_index = lt_sel_cells[ 1 ]-row_id.
    ELSE.
      i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).
      IF lines( lt_sel_rows ) > 0.
        e_index = lt_sel_rows[ 1 ]-index.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_rtti DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_table_by_name IMPORTING i_tname TYPE tabname
                           CHANGING  c_table TYPE REF TO data,

      create_struc_handle IMPORTING i_tname  TYPE tabname
                          EXPORTING e_t_comp TYPE abap_component_tab
                                    e_handle TYPE REF TO cl_abap_structdescr.
ENDCLASS.

CLASS lcl_rtti_tree DEFINITION FINAL INHERITING FROM lcl_popup.
  PUBLIC SECTION.

    TYPES: BEGIN OF t_classes_leaf,
             name TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_leaf.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
           END OF ts_table.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: main_node_key   TYPE salv_de_node_key,
          m_leaf          TYPE string,
          m_variable      TYPE REF TO data,
          m_object        TYPE REF TO object,
          m_hide          TYPE x,
          m_globals       TYPE x,
          m_class_data    TYPE x,
          m_ldb           TYPE x,
          m_changed       TYPE x,
          m_locals_key    TYPE salv_de_node_key,
          m_globals_key   TYPE salv_de_node_key,
          m_class_key     TYPE salv_de_node_key,
          m_ldb_key       TYPE salv_de_node_key,
          m_debug_key     TYPE salv_de_node_key,
          m_icon          TYPE salv_de_tree_image,
          mt_vars         TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_state        TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_classes_leaf TYPE TABLE OF t_classes_leaf,
          m_new_node      TYPE salv_de_node_key,
          m_no_refresh    TYPE xfeld,
          m_prg_info      TYPE tpda_scr_prg_info,
          tree            TYPE REF TO cl_salv_tree.

    METHODS constructor IMPORTING i_header   TYPE clike DEFAULT 'View'.
    METHODS create_popup.

    METHODS add_variable
      IMPORTING
        iv_root_name TYPE string
        iv_full_name TYPE string OPTIONAL
        iv_key       TYPE salv_de_node_key OPTIONAL
        i_icon       TYPE salv_de_tree_image OPTIONAL
        i_cl_leaf    TYPE int4 OPTIONAL
      CHANGING
        io_var       TYPE any  .

    METHODS clear.

    METHODS add_buttons.
    METHODS add_node
      IMPORTING
        iv_name TYPE string
        iv_icon TYPE salv_de_tree_image OPTIONAL.

    METHODS add_obj_var
      IMPORTING
                iv_name       TYPE lvc_value
                iv_full       TYPE string OPTIONAL
                iv_value      TYPE string OPTIONAL
                iv_key        TYPE salv_de_node_key OPTIONAL
                iv_icon       TYPE  salv_de_tree_image OPTIONAL
      RETURNING VALUE(er_key) TYPE salv_de_node_key.

    METHODS delete_node IMPORTING iv_key TYPE salv_de_node_key.
    METHODS display.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_kind,
                 struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                 table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                 elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                 class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                 intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                 ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
               END OF c_kind.

    DATA: tree_table TYPE tt_table.

    METHODS traverse
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                iv_name           TYPE clike
                iv_fullname       TYPE string OPTIONAL
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
                i_cl_leaf         TYPE int4 OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_struct
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                iv_name           TYPE clike
                iv_fullname       TYPE string OPTIONAL
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
                i_cl_leaf         TYPE int4 OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_elem
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                iv_name           TYPE clike
                iv_fullname       TYPE string OPTIONAL
                iv_value          TYPE any OPTIONAL
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
                i_cl_leaf         TYPE int4 OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_table
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                iv_name           TYPE clike
                iv_fullname       TYPE string OPTIONAL
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
                i_cl_leaf         TYPE int4 OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS: hndl_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key,
      hndl_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function,
      check_change.
ENDCLASS.

CLASS lcl_window DEFINITION INHERITING FROM lcl_popup.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
           END OF ts_table.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: m_history              TYPE x,
          m_visualization        TYPE x,
          m_zcode                TYPE x,
          m_prg                  TYPE tpda_scr_prg_info,
          m_debug_button         LIKE sy-ucomm,
          m_show_step            TYPE xfeld,
          m_update_tree          TYPE xfeld,
          mo_splitter_code       TYPE REF TO cl_gui_splitter_container,
          mo_splitter_var        TYPE REF TO cl_gui_splitter_container,
          mo_toolbar_container   TYPE REF TO cl_gui_container,
          mo_importing_container TYPE REF TO cl_gui_container,
          mo_locals_container    TYPE REF TO cl_gui_container,
          mo_exporting_container TYPE REF TO cl_gui_container,
          mo_code_container      TYPE REF TO cl_gui_container,
          mo_editor_container    TYPE REF TO cl_gui_container,
          mo_stack_container     TYPE REF TO cl_gui_container,
          mo_code_viewer         TYPE REF TO cl_gui_abapedit,
          mt_stack               TYPE TABLE OF lcl_appl=>t_stack,
          mo_toolbar             TYPE REF TO cl_gui_toolbar,
          mo_salv_stack          TYPE REF TO cl_salv_table,
          mt_tree_imp            TYPE tt_table,
          mt_tree_loc            TYPE tt_table,
          mt_tree_exp            TYPE tt_table,
          mo_tree_imp            TYPE REF TO cl_salv_tree,
          mo_tree_loc            TYPE REF TO cl_salv_tree,
          mo_tree_exp            TYPE REF TO cl_salv_tree,
          mt_breaks              TYPE tpda_bp_persistent_it.

    METHODS: constructor IMPORTING i_additional_name TYPE string OPTIONAL,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      set_program IMPORTING iv_program TYPE program.

    METHODS set_program_line IMPORTING iv_line LIKE sy-index.
    METHODS create_code_viewer.
    METHODS show_stack.
ENDCLASS.


CLASS lcl_types DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF selection_display_s,
        ind         TYPE i,
        field_label TYPE lvc_fname,
        int_type(1),
        inherited   TYPE aqadh_type_of_icon,
        emitter     TYPE aqadh_type_of_icon,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
        name        TYPE reptext,
        element     TYPE text60,
        domain      TYPE text60,
        datatype    TYPE string,
        length      TYPE i,
        transmitter TYPE REF TO lcl_data_transmitter,
        receiver    TYPE REF TO lcl_data_receiver,
        color       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
      END OF selection_display_s,

      BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row.

    CLASS-DATA: mt_sel TYPE TABLE OF lcl_types=>selection_display_s.
ENDCLASS.


CLASS lcl_window IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    m_history = '01'.
    m_zcode = '01'.

    mo_box = create( i_name = 'SDDE Simple Debugger Data Explorer beta v. 0.2' i_width = 1200 i_hight = 400 ).
    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_box
        rows    = 3
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->get_container(
     EXPORTING
       row       = 2
       column    = 1
     RECEIVING
       container = mo_code_container ).

    mo_splitter->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_toolbar_container ).

    mo_splitter->set_row_height( id = 1  height = '3' ).

    mo_splitter->set_row_sash( id    = 1 type  = 0 value = 0 ).

    mo_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_variables_container ).

    CREATE OBJECT mo_splitter_code ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_code_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_code->get_container(
         EXPORTING
           row       = 1
           column    = 1
         RECEIVING
           container = mo_editor_container ).

    mo_splitter_code->get_container(
         EXPORTING
           row       = 1
           column    = 2
         RECEIVING
           container = mo_stack_container ).

    mo_splitter_code->set_column_width( EXPORTING id = 1 width = '67' ).

    CREATE OBJECT mo_splitter_var ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_variables_container
        rows    = 1
        columns = 3
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_var->set_column_width( EXPORTING id = 1 width = '25' ).
    mo_splitter_var->set_column_width( EXPORTING id = 2 width = '50' ).
    mo_splitter_var->set_column_width( EXPORTING id = 3 width = '25' ).

    mo_splitter_var->get_container(
             EXPORTING
               row       = 1
               column    = 1
             RECEIVING
               container = mo_importing_container ).

    mo_splitter_var->get_container(
         EXPORTING
           row       = 1
           column    = 2
         RECEIVING
           container = mo_locals_container ).

    mo_splitter_var->get_container(
             EXPORTING
               row       = 1
               column    = 3
             RECEIVING
               container = mo_exporting_container ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_toolbar EXPORTING parent = mo_toolbar_container.
    mo_toolbar->set_visible( 'X' ).
    add_toolbar_buttons( ).
    create_code_viewer( ).

  ENDMETHOD.

  METHOD add_toolbar_buttons.
    DATA: lt_button TYPE ttb_button,
          ls_button LIKE LINE OF lt_button,
          lt_events TYPE cntl_simple_events,
          ls_events LIKE LINE OF lt_events.

    CLEAR ls_button.
    ls_button-function = 'HIST'.
    ls_button-icon = CONV #( icon_graduate ).
    ls_button-quickinfo = 'History On'.
    ls_button-text = 'History On'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'VIS'.
    ls_button-icon = CONV #( icon_flight ).
    ls_button-quickinfo = 'Visualization Off'.
    ls_button-text = 'Visualization Off'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'CODE'.
    ls_button-icon = CONV #( icon_customer_warehouse ).
    ls_button-quickinfo = 'Only Z'.
    ls_button-text = 'Only Z'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-butn_type = 3.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'F5'.
    ls_button-icon = CONV #( icon_debugger_step_into ).
    ls_button-quickinfo = 'Step into'.
    ls_button-text = 'Step into'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'F6BEG'.
    ls_button-icon = CONV #( icon_release ).
    ls_button-quickinfo = 'Start of block'.
    ls_button-text = 'Start of block'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'F7END'.
    ls_button-icon = CONV #( icon_outgoing_org_unit ).
    ls_button-quickinfo = 'End of block'.
    ls_button-text = 'End of block '.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'F6'.
    ls_button-icon = CONV #( icon_debugger_step_over ).
    ls_button-quickinfo = 'Step over'.
    ls_button-text = 'Step over'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'F7'.
    ls_button-icon = CONV #( icon_debugger_step_out ).
    ls_button-quickinfo = 'Step out'.
    ls_button-text = 'Step out'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'F8'.
    ls_button-icon = CONV #( icon_debugger_continue ).
    ls_button-quickinfo = 'Continue'.
    ls_button-text = 'Continue'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-butn_type = 3.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'BACK'.
    ls_button-icon = CONV #( icon_column_left ).
    ls_button-quickinfo = 'Step Back'.
    ls_button-text = 'Step Back'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function = 'FORW'.
    ls_button-icon = CONV #( icon_column_right ).
    ls_button-quickinfo = 'Step forward'.
    ls_button-text = 'Forward'.
    ls_button-butn_type = 0.
    APPEND ls_button TO lt_button.

    mo_toolbar->add_button_group( lt_button ).

* Register events
    ls_events-eventid = cl_gui_toolbar=>m_id_function_selected.
    ls_events-appl_event = space.
    APPEND ls_events TO lt_events.

    mo_toolbar->set_registered_events( events = lt_events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.
  ENDMETHOD.

  METHOD set_program.
    DATA gr_scan TYPE REF TO cl_ci_scan.
    DATA(gr_source) = cl_ci_source_include=>create( p_name = iv_program ).

    CREATE OBJECT gr_scan EXPORTING p_include = gr_source .
    mo_code_viewer->set_text( table = gr_source->lines  ).
  ENDMETHOD.

  METHOD set_program_line.
    TYPES: lntab TYPE STANDARD TABLE OF i.
    DATA lt_lines TYPE lntab.

    APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<line>).
    <line> = iv_line.
    mo_code_viewer->set_marker( EXPORTING marker_number = 7  marker_lines = lt_lines ).

    CLEAR lt_lines.

    LOOP AT mt_breaks INTO DATA(ls_break) WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
      <line> = ls_break-linesrc.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 9  marker_lines = lt_lines ).
    mo_code_viewer->select_lines( EXPORTING from_line = iv_line to_line = iv_line ).
  ENDMETHOD.

  METHOD create_code_viewer.
    CHECK mo_code_viewer IS INITIAL.

    CREATE OBJECT mo_code_viewer
      EXPORTING
        parent           = mo_editor_container
        max_number_chars = 100.

    mo_code_viewer->init_completer( ).
    mo_code_viewer->upload_properties(
           EXCEPTIONS
             dp_error_create  = 1
             dp_error_general = 2
             dp_error_send    = 3
             OTHERS           = 4 ).

    mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
    mo_code_viewer->create_document( ).
    mo_code_viewer->set_readonly_mode( 1 ).
  ENDMETHOD.

  METHOD show_stack.
    IF mo_salv_stack IS INITIAL.
      cl_salv_table=>factory(
  EXPORTING
    r_container = mo_stack_container
  IMPORTING
  r_salv_table = mo_salv_stack
  CHANGING
  t_table = mt_stack ).

      DATA:  lo_column  TYPE REF TO cl_salv_column.

      DATA(lo_columns) = mo_salv_stack->get_columns( ).
      lo_columns->set_optimize( 'X' ).

      lo_column ?= lo_columns->get_column( 'STACKPOINTER' ).
      lo_column->set_output_length( '5' ).

      lo_column ?= lo_columns->get_column( 'STACKLEVEL' ).
      lo_column->set_output_length( '5' ).
      mo_salv_stack->display( ).
    ELSE.
      mo_salv_stack->refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD hnd_toolbar.
    CONSTANTS: c_mask TYPE x VALUE '01'.
    "m_debug_button = fcode.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_sel_opt DEFINITION DEFERRED.

CLASS lcl_rtti IMPLEMENTATION.
  METHOD create_struc_handle.
    cl_abap_typedescr=>describe_by_name( EXPORTING p_name          = i_tname
                                         RECEIVING p_descr_ref     = DATA(lo_descr)
                                         EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 0.
      e_handle ?= lo_descr.
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD create_table_by_name.
    DATA: lo_new_tab  TYPE REF TO cl_abap_tabledescr,
          lo_new_type TYPE REF TO cl_abap_structdescr.

    create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_handle = lo_new_type ).
    lo_new_tab = cl_abap_tabledescr=>create(
                    p_line_type  = lo_new_type
                    p_table_kind = cl_abap_tabledescr=>tablekind_std
                    p_unique     = abap_false ).
    CREATE DATA c_table TYPE HANDLE lo_new_tab.  "Create a New table type
  ENDMETHOD.
ENDCLASS.

CLASS lcl_data_transmitter DEFINITION.
  PUBLIC SECTION.
    EVENTS: data_changed EXPORTING VALUE(e_row) TYPE lcl_types=>t_sel_row,
             col_changed EXPORTING VALUE(e_column) TYPE lvc_fname.
    METHODS: emit IMPORTING e_row TYPE lcl_types=>t_sel_row,
      emit_col IMPORTING e_column TYPE lvc_fname.
ENDCLASS.

CLASS lcl_data_transmitter IMPLEMENTATION.
  METHOD  emit.
    RAISE EVENT data_changed EXPORTING e_row = e_row.
  ENDMETHOD.

  METHOD emit_col.
    RAISE EVENT col_changed EXPORTING e_column = e_column.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_data_receiver DEFINITION.
  PUBLIC SECTION.
    DATA: mo_transmitter TYPE REF TO lcl_data_transmitter,
          lo_tab_from    TYPE REF TO lcl_table_viewer,
          lo_sel_to      TYPE REF TO lcl_sel_opt,
          m_from_field   TYPE lvc_fname,
          m_to_field     TYPE lvc_fname.
    METHODS: constructor
      IMPORTING io_transmitter TYPE REF TO lcl_data_transmitter OPTIONAL
                io_tab_from    TYPE REF TO lcl_table_viewer OPTIONAL
                io_sel_to      TYPE REF TO lcl_sel_opt OPTIONAL
                i_from_field   TYPE lvc_fname OPTIONAL
                i_to_field     TYPE lvc_fname OPTIONAL,
      shut_down,
      update FOR EVENT data_changed OF lcl_data_transmitter IMPORTING e_row,
      update_col FOR EVENT col_changed OF lcl_data_transmitter IMPORTING e_column,
      on_grid_button_click
          FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no.
ENDCLASS.

CLASS lcl_sel_opt DEFINITION.
  PUBLIC SECTION.
    DATA: mo_viewer  TYPE REF TO lcl_table_viewer,
          mo_sel_alv TYPE REF TO cl_gui_alv_grid,
          mt_fcat    TYPE lvc_t_fcat,
          mt_sel_tab TYPE TABLE OF lcl_types=>selection_display_s,
          ms_layout  TYPE lvc_s_layo.

    EVENTS: selection_done.
    METHODS:
      constructor IMPORTING io_viewer TYPE REF TO lcl_table_viewer io_container TYPE REF TO cl_gui_container,
      raise_selection_done,
      update_sel_tab,
      set_value IMPORTING  i_field TYPE any i_low TYPE any OPTIONAL i_high TYPE any OPTIONAL i_clear TYPE xfeld DEFAULT abap_true ,
      update_sel_row CHANGING c_sel_row TYPE lcl_types=>selection_display_s.

  PRIVATE SECTION.
    METHODS:
      init_fcat IMPORTING i_dd_handle TYPE i,
      handle_sel_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname es_row_no er_event_data,
      on_grid_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no,
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING  er_data_changed,
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
      handle_context_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid IMPORTING e_object.
ENDCLASS.

CLASS lcl_table_viewer DEFINITION INHERITING FROM lcl_popup.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_column_emitter,
             column  TYPE lvc_fname,
             emitter TYPE REF TO lcl_data_transmitter,
           END OF t_column_emitter,
           BEGIN OF t_elem,
             field TYPE fieldname,
             elem  TYPE ddobjname,
           END OF t_elem.

    DATA: m_lang             TYPE ddlanguage,
          m_tabname          TYPE tabname,
          m_texttabname      TYPE tabname,
          m_count            TYPE i,
          mo_alv             TYPE REF TO cl_gui_alv_grid,
          mo_sel             TYPE REF TO lcl_sel_opt,
          mr_table           TYPE REF TO data,
          mr_text_table      TYPE REF TO data,
          mo_sel_parent      TYPE REF TO cl_gui_container,
          mo_alv_parent      TYPE REF TO cl_gui_container,
          mt_alv_catalog     TYPE lvc_t_fcat,
          mt_text_components TYPE abap_component_tab,

          mt_fields          TYPE TABLE OF t_elem,
          mo_column_emitters TYPE TABLE OF t_column_emitter,
          mo_sel_width       TYPE i,
          m_visible,
          m_std_tbar         TYPE x,
          m_show_empty       TYPE i.

    METHODS:
      constructor IMPORTING i_tname           TYPE any OPTIONAL
                            i_additional_name TYPE string OPTIONAL
                            ir_tab            TYPE REF TO data OPTIONAL,
      refresh_table FOR EVENT selection_done OF lcl_sel_opt.

  PRIVATE SECTION.
    METHODS:
      create_popup,
      create_alv,
      create_sel_alv,
      set_header,
      read_text_table,
      update_texts,
      get_where RETURNING VALUE(c_where) TYPE string,

      create_field_cat IMPORTING i_tname           TYPE tabname
                       RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
      translate_field IMPORTING i_lang TYPE ddlanguage CHANGING c_fld TYPE lvc_s_fcat,
      handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
      before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      get_field_info IMPORTING i_tab TYPE tabname,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no.
ENDCLASS.

CLASS lcl_text_viewer DEFINITION FINAL INHERITING FROM lcl_popup.
  PUBLIC SECTION.
    DATA: mo_text     TYPE REF TO cl_gui_textedit.
    METHODS: constructor IMPORTING ir_str TYPE REF TO data.
ENDCLASS.

CLASS lcl_text_viewer IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_box = create( i_name = 'text' i_width = 200 i_hight = 100 ).
    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_box
        rows    = 1
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->get_container(
     EXPORTING
       row       = 1
       column    = 1
     RECEIVING
       container = mo_variables_container ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_text
      EXPORTING
        parent                 = mo_variables_container
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      on_box_close( mo_box ).
    ENDIF.

    mo_text->set_readonly_mode( ).

    FIELD-SYMBOLS <str> TYPE string.
    ASSIGN ir_str->* TO <str>.
    DATA lt_string TYPE TABLE OF char255.
    WHILE strlen( <str> ) > 255.
      APPEND <str>+0(255) TO lt_string.
      SHIFT <str> LEFT BY 255 PLACES.
    ENDWHILE.
    APPEND <str> TO lt_string.
    mo_text->set_text_as_r3table( lt_string ).
    CALL METHOD cl_gui_cfw=>flush.
    mo_text->set_focus( mo_box ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_plugins DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_field_links,
             tab        TYPE tablename,
             field      TYPE fieldname,
             method(30),
             rtab       TYPE tablename,
             rfield     TYPE fieldname,
             const      TYPE string, "aqadh_range_value,
           END OF t_field_links,
           BEGIN OF t_el_links,
             element TYPE tablename,
             rtab    TYPE tablename,
             rfield  TYPE fieldname,
             plugin  TYPE tcode,
           END OF t_el_links.

    CLASS-DATA: mt_field_links TYPE  TABLE OF t_field_links,
                mt_el_links    TYPE  TABLE OF t_el_links,
                mr_cluster     TYPE REF TO data.
    CLASS-METHODS: init,
      link IMPORTING i_str     TYPE any
                     io_viewer TYPE REF TO lcl_table_viewer
                     i_column  TYPE any,
      run_pa20 IMPORTING io_viewer TYPE REF TO lcl_table_viewer,
      run_pp01 IMPORTING io_viewer TYPE REF TO lcl_table_viewer,
      run_text IMPORTING io_viewer TYPE REF TO lcl_table_viewer,
      run_subty IMPORTING io_viewer TYPE REF TO lcl_table_viewer,
      run_py_cluster IMPORTING io_viewer TYPE REF TO lcl_table_viewer,
      run_dictionary_key IMPORTING i_str     TYPE any
                                   io_viewer TYPE REF TO lcl_table_viewer
                                   i_column  TYPE any,
      run_data_element IMPORTING i_str          TYPE any
                                 io_viewer      TYPE REF TO lcl_table_viewer
                                 i_column       TYPE any
                       RETURNING VALUE(is_done) TYPE xfeld,
      run_field_2_field IMPORTING i_str          TYPE any
                                  io_viewer      TYPE REF TO lcl_table_viewer
                                  i_column       TYPE any
                        RETURNING VALUE(is_done) TYPE xfeld,
      run_field_2_plugin IMPORTING "i_str          TYPE any
                                   io_viewer      TYPE REF TO lcl_table_viewer
                                   i_column       TYPE any
                         RETURNING VALUE(is_done) TYPE xfeld,

      run_hrp1001_adatanr IMPORTING i_str          TYPE any
                                    io_viewer      TYPE REF TO lcl_table_viewer
                                    i_column       TYPE any
                          RETURNING VALUE(is_done) TYPE xfeld,
      run_hrpy_rgdir IMPORTING i_str          TYPE any.
ENDCLASS.

CLASS lcl_plugins IMPLEMENTATION.
  METHOD init.
    "data elements links
    mt_el_links = VALUE #(
      ( element = 'PERSNO'   plugin = 'PA20' )
      ( element = 'HROBJID'  plugin = 'PP01' )
      ( element = 'LGART'    rtab = 'T512W'   rfield = 'LGART' )
      ( element = 'ITXEX'    plugin = 'SHOW_TEXT' )
      ( element = 'SUBTY'    plugin = 'SUBTY' ) ).

    "field to field links
    mt_field_links = VALUE #(
      ( tab = 'PA0001'     field = 'PLANS' rtab = 'HRP1000' rfield = 'OTYPE' const = 'S' )
      ( tab = 'PA0001'     field = 'PLANS' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'PA0001'     field = 'ORGEH' rtab = 'HRP1000' rfield = 'OTYPE' const = 'O' )
      ( tab = 'PA0001'     field = 'ORGEH' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'PA0001'     field = 'STELL' rtab = 'HRP1000' rfield = 'OBJID' const = 'C' )
      ( tab = 'PA0001'     field = 'STELL' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'HRP1001'    field = 'SCLAS' rfield = 'OTYPE' )
      ( tab = 'HRP1001'    field = 'SOBID' rfield = 'OBJID' )
      ( tab = 'HRP1002'    field = 'TABNR' rtab = 'HRT1002'  rfield = 'TABNR' )
      ( tab = 'HRP1035'    field = 'TABNR' rtab = 'HRT1035'  rfield = 'TABNR' )
      ( tab = 'HRP1222'    field = 'TABNR' rtab = 'HRT1222'  rfield = 'TABNR' )
      ( tab = 'PA2006'     field = 'QUONR' rtab = 'PTQUODED' rfield = 'QUONR' )
      ( tab = 'PTQUODED'   field = 'QUONR' rtab = 'PA2006'   rfield = 'QUONR' )
      ( tab = 'PTQUODED'   field = 'DOCNR' rtab = 'PA2001'   rfield = 'DOCNR' )
      ( tab = 'HRPY_RGDIR' field = 'SEQNR' method = 'RUN_PY_CLUSTER' ) ).
  ENDMETHOD.

  METHOD link.
    CHECK run_field_2_field( EXPORTING io_viewer = io_viewer i_column = i_column i_str = i_str ) = abap_false.
    CHECK run_field_2_plugin( EXPORTING io_viewer = io_viewer i_column = i_column ) = abap_false.

    CHECK run_data_element(  EXPORTING io_viewer = io_viewer i_column = i_column i_str = i_str ) = abap_false.
    CHECK run_hrp1001_adatanr( EXPORTING io_viewer = io_viewer i_column = i_column i_str = i_str ) = abap_false.
    run_dictionary_key( EXPORTING io_viewer = io_viewer i_column = i_column i_str = i_str ).

    LOOP AT lcl_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS NOT INITIAL.
      CASE ls_link-method.
        WHEN 'RUN_PY_CLUSTER'.
          ASSIGN COMPONENT 'PERNR' OF STRUCTURE i_str TO FIELD-SYMBOL(<pernr>).
          ASSIGN COMPONENT ls_link-field OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_pa20.
    DATA: l_infty    TYPE infty,
          l_temp(10) TYPE c.

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = lcl_alv_common=>get_selected( io_viewer->mo_alv ).

    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<tab>).

    SELECT SINGLE infty INTO l_infty FROM t777d WHERE dbtab = io_viewer->m_tabname.
    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <tab> TO FIELD-SYMBOL(<field>).
    l_temp = <field>.
    SET PARAMETER ID 'PER' FIELD l_temp.
    SET PARAMETER ID 'FCD' FIELD 'DIS'.
    SET PARAMETER ID 'ITP' FIELD l_infty.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <tab> TO <field>.
    l_temp = <field>.
    SET PARAMETER ID 'SUB' FIELD l_temp.
    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <tab> TO <field>.
    l_temp = <field>.
    SET PARAMETER ID 'BEG' FIELD <field>.
    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <tab> TO <field>.
    l_temp = <field>.
    SET PARAMETER ID 'END' FIELD <field>.
    CALL TRANSACTION 'PA20' AND SKIP FIRST SCREEN.
  ENDMETHOD.

  METHOD run_pp01.
    DATA: it_bdcdata    TYPE TABLE OF  bdcdata,
          save_plvar(2),
          save_otype(2),
          save_objid(8),
          l_infty(4),
          l_subty(4),
          l_temp(10).

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = lcl_alv_common=>get_selected( io_viewer->mo_alv ).

    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<tab>).

    SELECT SINGLE infty INTO l_infty FROM t777d WHERE dbtab = io_viewer->m_tabname.

    GET PARAMETER ID 'POP' FIELD save_plvar.
    GET PARAMETER ID 'POT' FIELD save_otype.
    GET PARAMETER ID 'PON' FIELD save_objid.

    ASSIGN COMPONENT 'PLVAR' OF STRUCTURE <tab> TO FIELD-SYMBOL(<plvar>).
    SET PARAMETER ID 'POP' FIELD <plvar>.                   "RITPP01

    ASSIGN COMPONENT 'OBJID' OF STRUCTURE <tab> TO FIELD-SYMBOL(<field>).
    l_temp = <field>.
    SET PARAMETER ID 'PON' FIELD l_temp.

    ASSIGN COMPONENT 'OTYPE' OF STRUCTURE <tab> TO <field>.
    l_temp = <field>.
    SET PARAMETER ID 'POT' FIELD l_temp.

    ASSIGN COMPONENT 'ISTAT' OF STRUCTURE <tab> TO FIELD-SYMBOL(<istat>).
    l_temp = <istat>.

    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <tab> TO <field>.
    IF sy-subrc = 0.
      l_subty = <field>.
    ELSE.
      CLEAR l_subty.
    ENDIF.

    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <tab> TO FIELD-SYMBOL(<date>).
    SET PARAMETER ID 'BEG' FIELD <date>.

    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <tab> TO <date>.
    SET PARAMETER ID 'END' FIELD <date>.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <tab> TO <field>.

    it_bdcdata = VALUE #(
      ( program = 'SAPMH5A0' dynpro = '1000' dynbegin = abap_true )
      ( fnam = 'PPHDR-INFTY' fval = l_infty )
      ( fnam = 'PPHDR-SUBTY' fval = l_subty )
      ( fnam = 'PPHDR-ISTAT' fval = l_temp )
      ( fnam = 'BDC_OKCODE' fval = 'DISP' )
      ).
    CALL TRANSACTION 'PP02' USING it_bdcdata MODE 'E'.
  ENDMETHOD.

  METHOD run_text.
    "NEW lcl_text_viewer( io_viewer ).
  ENDMETHOD.

  METHOD run_hrpy_rgdir.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = 'HRPY_RGDIR'.
    ASSIGN COMPONENT 'PERNR' OF STRUCTURE i_str TO FIELD-SYMBOL(<pernr>).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'PERNR' i_low = <pernr>  ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.

  METHOD run_hrp1001_adatanr.
    IF i_column = 'ADATANR' AND io_viewer->m_tabname = 'HRP1001'.
      ASSIGN COMPONENT 'ADATANR' OF STRUCTURE i_str TO FIELD-SYMBOL(<datanr>).
      ASSIGN COMPONENT 'RELAT' OF STRUCTURE i_str TO FIELD-SYMBOL(<relat>).
      "SELECT SINGLE pasub INTO @DATA(lv_struc) FROM t77ar WHERE relat = @<relat>.
      "SELECT SINGLE dbtab INTO @DATA(lv_dbtab) FROM t77ad WHERE pasub = @lv_struc.

*      IF sy-subrc = 0.
*        APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
*        CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = lv_dbtab.
*        <obj>-alv_viewer->mo_sel->set_value( i_field = 'ADATANR' i_low = <datanr>  ).
*        <obj>-alv_viewer->mo_sel->raise_selection_done( ).
*        is_done = abap_true.
*      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD run_subty.
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = lcl_alv_common=>get_selected( io_viewer->mo_alv ).
    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<str>).

    SELECT SINGLE stypt, namst INTO @DATA(l_result)   FROM t777d WHERE dbtab = @io_viewer->m_tabname.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <str> TO FIELD-SYMBOL(<subty>).
    DATA(l_infty) = io_viewer->m_tabname+2(4).
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_result-stypt.
    <obj>-alv_viewer->mo_sel->set_value( i_field = l_result-namst i_low = <subty> ).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'SUBTY' i_low = <subty> ).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'INFTY' i_low = l_infty ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.

  METHOD run_py_cluster.
    DATA: lo_handle TYPE REF TO cl_abap_complexdescr,
          l_name    TYPE string.
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.

    DATA(l_row) = lcl_alv_common=>get_selected( io_viewer->mo_alv ).
    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<str>).

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <str> TO FIELD-SYMBOL(<pernr>).
    ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <str> TO FIELD-SYMBOL(<seqnr>).


    FIELD-SYMBOLS: <cluster> TYPE any.

    lo_handle ?= cl_abap_typedescr=>describe_by_name( 'PAYRU_RESULT' ).
    CREATE DATA mr_cluster TYPE HANDLE lo_handle.
    ASSIGN mr_cluster->* TO <cluster>.

    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        employeenumber               = <pernr>
        sequencenumber               = <seqnr>
      CHANGING
        payroll_result               = <cluster>
      EXCEPTIONS
        illegal_isocode_or_clusterid = 1
        error_generating_import      = 2
        import_mismatch_error        = 3
        subpool_dir_full             = 4
        no_read_authority            = 5
        no_record_found              = 6
        versions_do_not_match        = 7
        error_reading_archive        = 8
        error_reading_relid          = 9
        OTHERS                       = 10.


    DATA lo_tree TYPE REF TO lcl_rtti_tree.
    CREATE OBJECT lo_tree.

    l_name = |{ <pernr> } Seqnr { <seqnr> } |.

    lo_tree->add_variable( EXPORTING iv_root_name = l_name
                                        iv_full_name = l_name
                                        CHANGING io_var =  <cluster> ).

    lo_tree->display( ).

  ENDMETHOD.

  METHOD run_field_2_plugin.
    LOOP AT lcl_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS NOT INITIAL.
      CASE ls_link-method.
        WHEN 'RUN_PY_CLUSTER'.
          run_py_cluster( io_viewer ).
      ENDCASE.
      is_done = 'X'.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_field_2_field.
    DATA: lo_viewer TYPE REF TO lcl_table_viewer.
    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    LOOP AT lcl_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS INITIAL.
      ASSIGN COMPONENT ls_link-field OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
      IF lo_viewer IS INITIAL.
        IF ls_link-rtab IS INITIAL.
          lo_viewer = io_viewer.
        ELSE.
          APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
          CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = ls_link-rtab.
          lo_viewer = <obj>-alv_viewer.
        ENDIF.
      ENDIF.
      lo_viewer->mo_sel->set_value( i_field = ls_link-rfield i_low = COND aqadh_type_of_icon( WHEN ls_link-const IS INITIAL THEN <field> ELSE ls_link-const ) ).
    ENDLOOP.
    IF sy-subrc = 0.
      lo_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = io_viewer->m_lang  ).
      lo_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      lo_viewer->mo_sel->raise_selection_done( ).
      is_done = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD run_data_element.
    DATA: lo_viewer TYPE REF TO lcl_table_viewer.

    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    READ TABLE lcl_alv_common=>mt_tabfields WITH KEY tabname = io_viewer->m_tabname fieldname = i_column INTO DATA(l_field).
    LOOP AT lcl_plugins=>mt_el_links INTO DATA(l_el_link) WHERE element = l_field-rollname AND plugin NE '' ." 'PA20' .
      CASE l_el_link-plugin.
        WHEN 'PA20'.
          lcl_plugins=>run_pa20( io_viewer ).
        WHEN 'PP01'.
          lcl_plugins=>run_pp01( io_viewer ).
        WHEN 'SHOW_TEXT'.
          lcl_plugins=>run_text( io_viewer ).
        WHEN 'SUBTY'.
          lcl_plugins=>run_subty( io_viewer ).
      ENDCASE.
      is_done = abap_true.
    ENDLOOP.
    IF is_done = abap_true.
      RETURN.
    ENDIF.

    LOOP AT lcl_plugins=>mt_el_links INTO l_el_link WHERE element = l_field-rollname .
      IF lo_viewer IS INITIAL.
        APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
        CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_el_link-rtab.
        lo_viewer = <obj>-alv_viewer.
      ENDIF.
      ASSIGN COMPONENT i_column OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
      lo_viewer->mo_sel->set_value( i_field = l_el_link-rfield i_low = <field>  ).
    ENDLOOP.
    IF sy-subrc = 0.
      lo_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = io_viewer->m_lang  ).
      lo_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      lo_viewer->mo_sel->raise_selection_done( ).
      is_done = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD run_dictionary_key.
    DATA: lt_keys TYPE TABLE OF dd05p.

    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    READ TABLE lcl_alv_common=>mt_tabfields INTO DATA(field) WITH KEY tabname = io_viewer->m_tabname fieldname = i_column .
    ASSIGN COMPONENT i_column OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
    CHECK <field> IS NOT INITIAL.
    CALL FUNCTION 'DD_FORKEY_GET'
      EXPORTING
        feldname  = CONV fieldname( i_column )
        tabname   = io_viewer->m_tabname
      TABLES
        forkeytab = lt_keys
      EXCEPTIONS
        not_equal = 1
        not_found = 2
        not_valid = 3
        OTHERS    = 4.

    IF sy-subrc < 2.
      APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
      CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = field-checktable.
      LOOP AT lt_keys INTO DATA(l_keys).
        ASSIGN COMPONENT l_keys-forkey OF STRUCTURE i_str TO <field>.
        CHECK sy-subrc = 0.
        <obj>-alv_viewer->mo_sel->set_value( i_field = l_keys-checkfield i_low = <field>  ).
      ENDLOOP.
      <obj>-alv_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = io_viewer->m_lang  ).
      <obj>-alv_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      <obj>-alv_viewer->mo_sel->raise_selection_done( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_data_receiver IMPLEMENTATION.
  METHOD constructor.
    lo_sel_to = io_sel_to.
    m_from_field =  i_from_field.
    m_to_field =  i_to_field.
    lo_tab_from = io_tab_from.
    mo_transmitter = io_transmitter.

    IF mo_transmitter IS NOT INITIAL.
      IF lo_tab_from IS INITIAL.
        SET HANDLER me->update FOR io_transmitter.
      ELSE.
        SET HANDLER me->update_col FOR io_transmitter.
      ENDIF.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES.
    ENDIF.
  ENDMETHOD.

  METHOD shut_down.
    IF mo_transmitter IS NOT INITIAL.
      SET HANDLER me->update FOR mo_transmitter  ACTIVATION space.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES  ACTIVATION space.
    ENDIF.
    CLEAR lo_sel_to.
  ENDMETHOD.

  METHOD on_grid_button_click.
    FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE.

    CHECK m_from_field = es_col_id-fieldname.
    ASSIGN lo_tab_from->mr_table->* TO <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <tab> TO  FIELD-SYMBOL(<f_field>).
    CHECK lo_sel_to IS NOT INITIAL.
    lo_sel_to->set_value( i_field = m_to_field i_low = <f_field>  ).
    lo_sel_to->raise_selection_done( ).
  ENDMETHOD.

  METHOD  update.
    DATA: l_updated.

    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    IF <to>-range[] = e_row-range[].
      l_updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    MOVE-CORRESPONDING e_row TO <to>.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = e_row ).
    ENDIF.
    lo_sel_to->raise_selection_done( ).
  ENDMETHOD.

  METHOD update_col.
    DATA: l_updated,
          lt_sel_row   TYPE lcl_types=>t_sel_row.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE any.

    CHECK lo_sel_to IS NOT INITIAL.
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    DATA(lt_old_range) = <to>-range.
    CLEAR: <to>-sign, <to>-opti, <to>-low, <to>-high, <to>-range.
    ASSIGN lo_tab_from->mr_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT e_column OF STRUCTURE <row> TO <field>.
      IF line_exists( <to>-range[ low = <field> ] ).
        APPEND VALUE #( sign = 'I' opti = 'EQ' low = <field> ) TO <to>-range.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0." empty column
      APPEND VALUE #( sign = 'I' opti = 'EQ' low = '' ) TO <to>-range.
    ENDIF.

    LOOP AT <to>-range ASSIGNING FIELD-SYMBOL(<sel>).
      <to>-low = <sel>-low.
      lo_sel_to->update_sel_row( CHANGING c_sel_row = <to> ).
      EXIT.
    ENDLOOP.

    MOVE-CORRESPONDING <to> TO lt_sel_row.
    IF <to>-range = lt_old_range.
      l_updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = lt_sel_row ).
      lo_sel_to->raise_selection_done( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_box_handler IMPLEMENTATION.
  METHOD on_box_close.
    DATA: lv_tabix LIKE sy-tabix.
    sender->free( ).

    "Free Memory
    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
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
      IF lv_tabix NE 0.
        DELETE lcl_appl=>mt_obj INDEX lv_tabix.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_BOX_CLOSE
ENDCLASS.               "lcl_box_handler

CLASS lcl_table_viewer IMPLEMENTATION.
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

    IF ir_tab IS NOT BOUND.
      lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table  ).
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
              LOOP AT lo_struc->components INTO DATA(comp).

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

    get_field_info( m_tabname ).

    create_alv( ).
    create_sel_alv( ).
    mo_sel->mo_viewer->handle_user_command( 'SHOW' ).
    mo_alv->set_focus( mo_alv ).
  ENDMETHOD.

  METHOD create_popup.
    mo_box = create( i_width = 800 i_hight = 150 ).

    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_box
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

    IF lcl_appl=>m_ctrl_box_handler IS INITIAL.
      lcl_appl=>m_ctrl_box_handler = NEW #( ).
    ENDIF.
    SET HANDLER lcl_appl=>m_ctrl_box_handler->on_box_close FOR mo_box.
  ENDMETHOD.

  METHOD create_alv.
    DATA: ls_layout TYPE lvc_s_layo,
          effect    TYPE i,
          lt_f4     TYPE lvc_t_f4.

    FIELD-SYMBOLS: <f_tab>   TYPE table.

    mo_alv = NEW #( i_parent = mo_alv_parent ).
    mt_alv_catalog = create_field_cat( m_tabname ).

    IF mt_alv_catalog IS INITIAL.
      RETURN. "todo show tables without structure
    ENDIF.

    ASSIGN mr_table->* TO <f_tab>.
    IF m_tabname IS NOT INITIAL.
      read_text_table( ).
      lcl_sql=>read_any_table( EXPORTING i_tabname = m_tabname i_where = get_where( ) i_row_count = 100
                           CHANGING cr_tab =  mr_table c_count = m_count ).
      update_texts( ).


    ENDIF.
    set_header( ).
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode = 'D'.
    CREATE OBJECT lcl_appl=>c_dragdropalv.
    effect = cl_dragdrop=>move + cl_dragdrop=>copy.

    CALL METHOD lcl_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line' ##NO_TEXT
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD lcl_appl=>c_dragdropalv->get_handle IMPORTING handle = DATA(handle_alv).
    ls_layout-s_dragdrop-grid_ddid = handle_alv.

    SET HANDLER   before_user_command
                  handle_user_command
                  handle_tab_toolbar
                  handle_doubleclick
                  lcl_dragdrop=>drag
                  FOR mo_alv.

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
      lcl_alv_common=>translate_field(  CHANGING c_fld = <cat> ).
    ENDLOOP.

    mo_alv->set_frontend_fieldcatalog( EXPORTING  it_fieldcatalog = mt_alv_catalog ).
    me->handle_user_command( EXPORTING e_ucomm = 'TECH' ).
    me->handle_user_command( EXPORTING e_ucomm = 'SHOW' ).
    mo_alv->set_toolbar_interactive( ).
  ENDMETHOD.

  METHOD translate_field.
    DATA: l_dd04 TYPE dd04v.

    READ TABLE mt_fields INTO DATA(l_field) WITH KEY field = c_fld-fieldname.
    CHECK l_field-elem IS NOT INITIAL.
    CLEAR l_dd04.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = CONV ddobjname( l_field-elem )
        langu         = i_lang
      IMPORTING
        dd04v_wa      = l_dd04
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc = 0.
      IF l_dd04-reptext IS NOT INITIAL.
        MOVE-CORRESPONDING l_dd04 TO c_fld.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_sel_alv.
    IF mo_sel IS INITIAL.
      mo_sel     = NEW #( io_viewer = me io_container = mo_sel_parent ).
      SET HANDLER refresh_table FOR mo_sel.
    ELSE.
      mo_sel->update_sel_tab( ).
    ENDIF.
  ENDMETHOD.

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

METHOD read_text_table.
    FIELD-SYMBOLS: <f_tab> TYPE ANY TABLE.
    lcl_ddic=>get_text_table( EXPORTING i_tname =  m_tabname IMPORTING e_tab = DATA(l_tab) ).
    CHECK l_tab IS NOT INITIAL.
    lcl_rtti=>create_table_by_name( EXPORTING i_tname = l_tab CHANGING c_table = mr_text_table ).
    ASSIGN mr_text_table->* TO <f_tab>.
    SELECT * FROM (l_tab) INTO TABLE <f_tab> ORDER BY PRIMARY KEY.
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

    READ TABLE lcl_alv_common=>mt_tabfields WITH KEY tabname = m_texttabname domname = 'SPRAS' INTO DATA(l_texttabfield).
    IF sy-subrc = 0.
      DATA(l_lang) = l_texttabfield-fieldname.
    ENDIF.

    l_replace = m_texttabname && '_'.
    ASSIGN mr_table->* TO <f_tab>.

    LOOP AT <f_tab> ASSIGNING FIELD-SYMBOL(<str>).
      CLEAR lv_clause.
      LOOP AT lcl_alv_common=>mt_tabfields INTO l_texttabfield WHERE tabname = m_texttabname AND keyflag = abap_true.
        UNASSIGN <check>.
        ASSIGN COMPONENT l_texttabfield-fieldname OF STRUCTURE <str> TO <check>.
        IF sy-subrc NE 0.
          READ TABLE lcl_alv_common=>mt_tabfields WITH KEY tabname = m_texttabname fieldname  = l_texttabfield-fieldname INTO DATA(l_texttab).
          READ TABLE lcl_alv_common=>mt_tabfields WITH KEY tabname = m_tabname domname  = l_texttab-domname INTO DATA(l_maintab).
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


  METHOD set_header.
    DATA: lv_text       TYPE as4text,
          lv_header(80) TYPE c.

    SELECT SINGLE ddtext INTO lv_text
      FROM dd02t
     WHERE tabname = m_tabname
       AND ddlanguage = m_lang.

    lv_header = |{ m_tabname } - { lv_text } { m_additional_name }|.
    mo_box->set_caption( lv_header ).
  ENDMETHOD.

  METHOD handle_tab_toolbar.
    IF m_visible IS INITIAL.
      DATA(lt_toolbar) = VALUE ttb_button(
       ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options'  butn_type = 0 )
       ( butn_type = 3 ) ).
    ENDIF.
    APPEND VALUE #( function = 'TECH' icon = icon_wd_caption quickinfo = 'Tech names'  butn_type = 0 ) TO lt_toolbar.

    LOOP AT lcl_appl=>mt_lang INTO DATA(lang).
      IF sy-tabix > 10.
        EXIT.
      ENDIF.
      APPEND VALUE #( function = lang-spras icon = icon_foreign_trade quickinfo = lang-sptxt butn_type = 0 text = lang-sptxt ) TO lt_toolbar.
    ENDLOOP.

    lt_toolbar = VALUE ttb_button( BASE lt_toolbar
     ( function = 'SHOW'  icon = icon_list  quickinfo = 'Show empty columns'   butn_type = 0  )
     ( function = 'TBAR' icon = COND #( WHEN m_std_tbar IS INITIAL THEN icon_column_right ELSE icon_column_left )
        quickinfo = COND #( WHEN m_std_tbar IS INITIAL THEN 'Show standard ALV function'  ELSE 'Hide standard ALV function') )
     ( butn_type = 3 ) ).

    IF m_std_tbar IS INITIAL.
      e_object->mt_toolbar =  lt_toolbar.
    ELSE.
      e_object->mt_toolbar =  lt_toolbar = VALUE ttb_button( BASE lt_toolbar ( LINES OF e_object->mt_toolbar ) ).
    ENDIF.
  ENDMETHOD.

  METHOD create_field_cat.
    DATA: lr_field       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          lr_data_descr  TYPE REF TO cl_abap_datadescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          l_replace      TYPE string,
          l_texttab      TYPE tabname,
          lr_temp        TYPE REF TO data,
          l_name         TYPE string,
          l_dd04         TYPE dd04v.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <struc> TYPE any,
                   <field> TYPE any.

    ASSIGN mr_table->* TO <tab>.
    CREATE DATA lr_temp LIKE LINE OF <tab>.
    ASSIGN lr_temp->* TO <struc>.

    TRY.
        lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_temp ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    it_tabdescr[] = lr_table_descr->components[].
    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = l_texttab ).
    l_replace = l_texttab && '_'.

    LOOP AT it_tabdescr INTO DATA(ls)
       WHERE type_kind NE 'h'
         AND type_kind NE 'l'.
      DATA(l_ind) = sy-tabix.

      ASSIGN COMPONENT ls-name OF STRUCTURE <struc> TO <field>.
      GET REFERENCE OF <field> INTO lr_field.
      lr_data_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_field ).
      l_name = lr_data_descr->absolute_name.
      REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_name WITH ''.
      APPEND VALUE #( field = ls-name elem = l_name ) TO mt_fields.

      CLEAR l_dd04.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = CONV ddobjname( l_name )
          langu         = m_lang
        IMPORTING
          dd04v_wa      = l_dd04
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.



      APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).

      READ TABLE lcl_alv_common=>mt_tabfields INTO DATA(ls_tf) WITH KEY tabname = i_tname fieldname = ls-name.

      <catalog>-style = lcl_alv_common=>c_white.
      MOVE-CORRESPONDING ls_tf TO <catalog>.
      <catalog>-f4availabl = abap_true.
      IF ls_tf-is_text = abap_true.
        <catalog>-style = lcl_alv_common=>c_grey.
      ENDIF.

      IF ls_tf-checktable IS NOT INITIAL.
        <catalog>-style = lcl_alv_common=>c_blue.
      ENDIF.

      IF line_exists( lcl_plugins=>mt_field_links[ tab = i_tname field = ls_tf-fieldname ] ).
        <catalog>-style = lcl_alv_common=>c_green.
      ENDIF.

      IF line_exists( lcl_plugins=>mt_el_links[ element = ls_tf-rollname ] ).
        <catalog>-style = lcl_alv_common=>c_green.
      ENDIF.

      IF ls_tf-keyflag = abap_true.
        <catalog>-style = <catalog>-style BIT-OR lcl_alv_common=>c_bold.
      ENDIF.

      <catalog>-col_pos = l_ind.
      <catalog>-fieldname = ls-name.
      <catalog>-f4availabl = abap_true.

      IF l_dd04 IS INITIAL.
        <catalog>-scrtext_s = <catalog>-scrtext_m = <catalog>-scrtext_l = <catalog>-reptext = <catalog>-fieldname = ls-name.
      ELSE.
        MOVE-CORRESPONDING l_dd04 TO <catalog>.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.

  METHOD handle_doubleclick.
    DATA: lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone    TYPE REF TO data.
    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD TABLE.

    CHECK es_row_no-row_id IS NOT INITIAL.
    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).

    ASSIGN COMPONENT |{ e_column-fieldname }_REF| OF STRUCTURE <tab> TO FIELD-SYMBOL(<ref>).
    IF sy-subrc = 0.
      lcl_appl=>open_int_table( EXPORTING iv_name = CONV #( e_column-fieldname ) it_ref = <ref> ).
      RETURN.
    ELSE.
*      TRY.
*          lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ 1 ]-{ e_column-fieldname }| ).
*          table_clone = lo_table_descr->elem_clone( ).
*          lcl_appl=>open_int_table( EXPORTING iv_name = |{ m_additional_name }[ 1 ]-{ e_column-fieldname }| it_ref = table_clone ).
*          return.
*        CATCH cx_sy_move_cast_error.
*      ENDTRY.
    ENDIF.


    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING <tab>.
    lcl_plugins=>link( EXPORTING i_str = <tab> i_column = e_column io_viewer = me ).

  ENDMETHOD.

  METHOD get_field_info.
    DATA: lv_clause      TYPE string,
          lr_struc       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          lt_field_info  TYPE TABLE OF dfies,
          l_fname        TYPE fieldname,
          l_tname        TYPE tabname,
          ls_tf          LIKE LINE OF lcl_alv_common=>mt_tabfields,
          dref           TYPE REF TO data,
          l_x            TYPE xstring.

    CHECK i_tab IS NOT INITIAL.
    CREATE DATA lr_struc TYPE (i_tab).
    lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_struc ).
    it_tabdescr[] = lr_table_descr->components[].

    DATA(l_exist) = lcl_sql=>exist_table( i_tab ).
    IF  l_exist = 1.
      SELECT  COUNT( * ) FROM (i_tab).
      DATA(l_count) = sy-dbcnt.
    ENDIF.

    LOOP AT it_tabdescr INTO DATA(ls) WHERE name NE 'MANDT' AND name NE 'CLIENT'.
      IF NOT line_exists( lcl_alv_common=>mt_tabfields[ tabname = i_tab fieldname = ls-name ] ).
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
        INSERT ls_tf INTO TABLE lcl_alv_common=>mt_tabfields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD before_user_command.
    CASE e_ucomm.
      WHEN '&INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_user_command.
    DATA: it_fields     TYPE lvc_t_fcat,
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
    ELSEIF e_ucomm = 'TBAR'.
      m_std_tbar = BIT-NOT  m_std_tbar.
    ELSE.
      IF e_ucomm = 'SHOW'.
        IF m_show_empty IS INITIAL.
          m_show_empty = 1.
        ELSE.
          CLEAR m_show_empty.
        ENDIF.
      ENDIF.

      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<fields>) WHERE domname NE 'MANDT'.
        <fields>-col_pos = sy-tabix.
        CASE e_ucomm.
          WHEN 'SHOW'.
            IF m_show_empty = abap_false.
              <fields>-no_out = ' '.
            ELSE.
              lv_clause = |{ <fields>-fieldname } IS NOT INITIAL|.
              LOOP AT <f_tab> ASSIGNING FIELD-SYMBOL(<f_line>)  WHERE (lv_clause).
                EXIT.
              ENDLOOP.
              IF sy-subrc NE 0.
                <fields>-no_out = abap_true.
              ENDIF.
            ENDIF.
          WHEN 'TECH'. "technical field name
            <fields>-scrtext_l = <fields>-scrtext_m = <fields>-scrtext_s =  <fields>-reptext = <fields>-fieldname.
          WHEN OTHERS. "header names translation
            IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
              translate_field( EXPORTING i_lang = CONV #( e_ucomm )  CHANGING c_fld = <fields> ).
              IF mo_sel IS BOUND.
                READ TABLE mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) WITH KEY field_label = <fields>-fieldname.
                IF sy-subrc = 0.
                  IF <fields>-scrtext_l IS NOT INITIAL.
                    <sel>-name = <fields>-scrtext_l.
                  ENDIF.
                  IF <sel>-name IS INITIAL.
                    IF <fields>-reptext IS NOT INITIAL.
                      <sel>-name = <fields>-reptext.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
      m_lang = e_ucomm.
      set_header( ).
      mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang  ).
    ENDIF.

    CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].
    lcl_alv_common=>refresh( mo_alv ).
    IF mo_sel IS BOUND.
      IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
        mo_sel->update_sel_tab( ).
      ENDIF.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
    ENDIF.
  ENDMETHOD.                           "handle_user_command

  METHOD refresh_table.
    DATA: ls_row    TYPE lcl_types=>t_sel_row,
          lt_filter TYPE lvc_t_filt.

    CLEAR lt_filter.
    set_header( ).

    LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      ENDIF.
      LOOP AT <sel>-range INTO DATA(l_range).
        APPEND VALUE #( fieldname = <sel>-field_label
                              low = l_range-low
                             high = l_range-high
                             sign = l_range-sign
                           option = l_range-opti ) TO lt_filter.
      ENDLOOP.
    ENDLOOP.

    IF mo_sel->mt_sel_tab IS NOT INITIAL.
      CALL METHOD mo_alv->set_filter_criteria
        EXPORTING
          it_filter = lt_filter.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
      lcl_alv_common=>refresh( mo_alv ).
      mo_sel->mo_viewer->handle_user_command( 'SHOW' ).
      LOOP AT mo_column_emitters INTO DATA(l_emit).
        l_emit-emitter->emit_col( l_emit-column ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sel_opt IMPLEMENTATION.
  METHOD constructor.
    DATA: effect     TYPE i,
          handle_alv TYPE i.

    mo_viewer = io_viewer.
    mo_sel_alv = NEW #( i_parent = io_container ).
    update_sel_tab( ).
    CREATE OBJECT lcl_appl=>c_dragdropalv.
    effect =  cl_dragdrop=>copy. " + cl_dragdrop=>move.

    CALL METHOD lcl_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line'
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD lcl_appl=>c_dragdropalv->get_handle IMPORTING handle = handle_alv.
    ms_layout-s_dragdrop-col_ddid = handle_alv.
    init_fcat( handle_alv ).
    ms_layout-cwidth_opt = abap_true.
    ms_layout-col_opt = abap_true.
    ms_layout-ctab_fname = 'COLOR'.
    ms_layout-stylefname = 'STYLE'.

    "fields for F4 event handling
    DATA(gt_f4) = VALUE  lvc_t_f4( register   = abap_true chngeafter = abap_true
                             ( fieldname  = 'LOW'  )
                             ( fieldname  = 'HIGH'  ) ).

    mo_sel_alv->register_f4_for_fields( it_f4 = gt_f4 ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    SET HANDLER handle_user_command
                handle_sel_toolbar
                lcl_dragdrop=>drag
                lcl_dragdrop=>drop
                on_data_changed
                on_data_changed_finished
                on_grid_button_click
                handle_context_menu_request
                handle_doubleclick
                on_f4 FOR mo_sel_alv.

    CALL METHOD mo_sel_alv->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        i_default       = abap_true
        is_layout       = ms_layout
      CHANGING
        it_outtab       = mt_sel_tab[]
        it_fieldcatalog = mt_fcat.

    mo_sel_alv->set_toolbar_interactive( ).
  ENDMETHOD.

  METHOD init_fcat.
    mt_fcat = VALUE #(
     ( fieldname = 'IND'         coltext = '№'  outputlen = 3 style = '00000003' )
     ( fieldname = 'FIELD_LABEL' coltext = 'Label'  outputlen = 30 dragdropid = i_dd_handle )
     ( fieldname = 'SIGN'        coltext = 'SIGN'   tech = abap_true )
     ( fieldname = 'OPTI'        coltext = 'Option' tech = abap_true )
     ( fieldname = 'OPTION_ICON' coltext = 'Option' icon = abap_true outputlen = 4 style = cl_gui_alv_grid=>mc_style_button )
     ( fieldname = 'LOW'         coltext = 'From data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4 col_opt = abap_true  )
     ( fieldname = 'HIGH'        coltext = 'To data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4  col_opt = abap_true )
     ( fieldname = 'MORE_ICON'   coltext = 'Range' icon = abap_true  style = cl_gui_alv_grid=>mc_style_button  )
     ( fieldname = 'RANGE'   tech = abap_true  )
     ( fieldname = 'INHERITED'   coltext = 'Inh.' icon = abap_true outputlen = 4 seltext = 'Inherited' style = '00000003')
     ( fieldname = 'EMITTER'    coltext = 'Emit.' icon = abap_true outputlen = 4 seltext = 'Emitter' style = '00000003')
     ( fieldname = 'NAME' coltext = 'Field name'  outputlen = 60 style = '00000003')
     ( fieldname = 'ELEMENT' coltext = 'Data element'  outputlen = 15 style = '00000209' )
     ( fieldname = 'DOMAIN'  coltext = 'Domain'  outputlen = 15 style = '00000209' )
     ( fieldname = 'DATATYPE' coltext = 'Type'  outputlen = 5 style = '00000003')
     ( fieldname = 'LENGTH' coltext = 'Length'  outputlen = 5 style = '00000003')
     ( fieldname = 'TRANSMITTER'   tech = abap_true  )
     ( fieldname = 'RECEIVER'    tech = abap_true  )
     ( fieldname = 'COLOR'    tech = abap_true  ) ).
  ENDMETHOD.

  METHOD raise_selection_done.
    DATA: ls_row TYPE lcl_types=>t_sel_row.
    lcl_alv_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    LOOP AT mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD update_sel_tab.
    IF mt_sel_tab[] IS NOT INITIAL.
      DATA(lt_copy) = mt_sel_tab.
    ENDIF.
    CLEAR mt_sel_tab[].
    mo_viewer->mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mo_viewer->mt_alv_catalog ).
    LOOP AT mo_viewer->mt_alv_catalog INTO DATA(l_catalog) WHERE domname NE 'MANDT'.
      DATA(lv_ind) = sy-tabix.
      APPEND INITIAL LINE TO mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel_tab>).
      READ TABLE lt_copy INTO DATA(ls_copy) WITH KEY field_label = l_catalog-fieldname.

      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_copy TO <sel_tab>.
      ELSE.
        <sel_tab>-option_icon = icon_led_inactive.
        <sel_tab>-more_icon = icon_enter_more.
      ENDIF.

      <sel_tab>-ind = lv_ind.
      <sel_tab>-field_label = l_catalog-fieldname.
      <sel_tab>-int_type = l_catalog-inttype.
      <sel_tab>-element = l_catalog-rollname.
      <sel_tab>-domain =  l_catalog-domname.
      <sel_tab>-datatype = l_catalog-datatype.
      <sel_tab>-length = l_catalog-outputlen.
      lcl_alv_common=>translate_field( EXPORTING i_lang = mo_viewer->m_lang CHANGING c_fld = l_catalog ).
      <sel_tab>-name = l_catalog-scrtext_l.
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_sel_toolbar.
    e_object->mt_toolbar[] = VALUE #( butn_type = 0 disabled = ''
     ( function = 'SEL_OFF' icon = icon_arrow_right    quickinfo = 'Hide' )
     ( function = 'SEL_CLEAR' icon = icon_delete_row    quickinfo = 'Clear Select-Options' ) ).
  ENDMETHOD.

  METHOD set_value.
    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = i_field.
    CHECK sy-subrc = 0.
    IF i_low IS SUPPLIED.
      IF i_clear IS INITIAL.
        APPEND VALUE #( sign = 'I' opti = 'EQ' low = i_low high = i_high ) TO <to>-range.
      ELSE.
        CLEAR:  <to>-opti, <to>-sign,<to>-range.
        IF i_low IS SUPPLIED.
          <to>-low = i_low.
        ENDIF.
        IF i_high IS SUPPLIED.
          <to>-high = i_high.
        ENDIF.
        update_sel_row( CHANGING c_sel_row = <to> ).
      ENDIF.
    ELSE.
      CLEAR:  <to>-opti, <to>-sign.
      <to>-high = i_high.
      update_sel_row( CHANGING c_sel_row = <to> ).
    ENDIF.
    IF <to>-transmitter IS BOUND.
      DATA: ls_row TYPE lcl_types=>t_sel_row.
      MOVE-CORRESPONDING <to> TO ls_row.
      <to>-transmitter->emit( EXPORTING e_row = ls_row ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_doubleclick.
    DATA: it_bdcdata TYPE TABLE OF  bdcdata.

    CHECK es_row_no-row_id IS NOT INITIAL.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id INTO DATA(l_sel).
    APPEND VALUE #( program = 'SAPLSD_ENTRY' dynpro = '1000' dynbegin = abap_true ) TO it_bdcdata.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WB_DISPLAY' ) TO it_bdcdata.

    IF e_column = 'ELEMENT'.
      SET PARAMETER ID 'DTYP' FIELD l_sel-element.
      APPEND VALUE #( fnam = 'RSRD1-DDTYPE' fval = abap_true ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSEIF e_column = 'DOMAIN'.
      SET PARAMETER ID 'DOM' FIELD l_sel-domain.
      APPEND VALUE #( fnam = 'RSRD1-DOMA' fval = abap_true ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSE.
      CALL FUNCTION 'DOCU_CALL'
        EXPORTING
          id                = 'DE'
          langu             = mo_viewer->m_lang
          object            = l_sel-element
          typ               = 'E'
          displ             = abap_true
          displ_mode        = 3
          use_sec_langu     = abap_true
          display_shorttext = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD update_sel_row. "select patterns rules
    IF c_sel_row-high IS INITIAL AND c_sel_row-opti = 'BT'.
      CLEAR c_sel_row-opti.
    ENDIF.

    IF c_sel_row-low IS NOT INITIAL AND c_sel_row-opti IS INITIAL.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'EQ'.
    ENDIF.

    IF c_sel_row-high IS NOT INITIAL AND c_sel_row-opti NE 'NB' .
      c_sel_row-opti = 'BT'.
    ENDIF.

    IF c_sel_row-sign IS INITIAL AND c_sel_row-opti IS INITIAL.
      CLEAR: c_sel_row-low, c_sel_row-low.
    ENDIF.

    IF c_sel_row-low CA  '*%+&'.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'CP'.
    ENDIF.

    IF c_sel_row-opti IS NOT INITIAL AND c_sel_row-sign IS INITIAL.
      c_sel_row-sign = 'I'.
    ENDIF.

    TRY.
        c_sel_row-option_icon = lcl_appl=>m_option_icons[ sign = c_sel_row-sign option = c_sel_row-opti ]-icon_name.
      CATCH cx_sy_itab_line_not_found.                  "#EC NO_HANDLER
    ENDTRY.

    IF c_sel_row-sign IS NOT INITIAL.
      READ TABLE c_sel_row-range ASSIGNING FIELD-SYMBOL(<range>) INDEX 1.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO c_sel_row-range ASSIGNING <range>.
      ENDIF.
      MOVE-CORRESPONDING c_sel_row TO <range>.
      IF c_sel_row-opti NE 'BT' AND c_sel_row-opti NE 'NB' .
        CLEAR c_sel_row-high.
      ENDIF.
      IF c_sel_row-int_type = 'D' OR c_sel_row-int_type = 'T' .
        DO 2 TIMES.
          ASSIGN COMPONENT  COND string( WHEN sy-index = 1 THEN 'LOW' ELSE 'HIGH'  ) OF STRUCTURE <range> TO FIELD-SYMBOL(<field>).
          IF <field> IS INITIAL.
            CONTINUE.
          ENDIF.

          IF c_sel_row-int_type = 'D'.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL' ##FM_SUBRC_OK
              EXPORTING
                date_external            = <field>
              IMPORTING
                date_internal            = <field>
              EXCEPTIONS
                date_external_is_invalid = 1
                OTHERS                   = 2.
          ELSE.
            REPLACE ALL OCCURRENCES OF ':' IN <field> WITH ''.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
    c_sel_row-more_icon = COND #( WHEN c_sel_row-range IS INITIAL THEN icon_enter_more    ELSE icon_display_more  ).

    IF c_sel_row-receiver IS BOUND AND c_sel_row-inherited IS INITIAL.
      c_sel_row-inherited = icon_businav_value_chain.
    ENDIF.
  ENDMETHOD.

  METHOD on_f4.
    DATA: return_tab TYPE STANDARD TABLE OF ddshretval,
          lt_objec   TYPE TABLE OF objec,
          ls_objec   TYPE objec,
          l_otype    TYPE otype,
          l_plvar    TYPE plvar,
          l_multiple TYPE xfeld,
          l_clear    TYPE xfeld.

    IF e_fieldname = 'LOW'.
      l_multiple = abap_true.
    ENDIF.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
    DATA(l_fname) =  <sel>-field_label.

    lcl_types=>mt_sel[] = mt_sel_tab[].
    IF <sel>-element = 'HROBJID'.
      READ TABLE mt_sel_tab INTO DATA(l_sel) WITH KEY field_label = 'OTYPE'.
      l_otype = l_sel-low.
      READ TABLE mt_sel_tab INTO l_sel WITH KEY field_label = 'PLVAR'.
      IF sy-subrc = 0 AND l_sel-low IS NOT INITIAL.
        l_plvar = l_sel-low.
      ELSE.
        CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
          IMPORTING
            act_plvar       = l_plvar
          EXCEPTIONS
            no_active_plvar = 1
            OTHERS          = 2.
      ENDIF.
    ELSEIF <sel>-element = 'PERSNO'.
      l_otype = 'P'.
    ENDIF.

    IF l_otype IS NOT INITIAL.
      CALL FUNCTION 'RH_OBJID_REQUEST'
        EXPORTING
          plvar            = l_plvar
          otype            = l_otype
          seark_begda      = sy-datum
          seark_endda      = sy-datum
          dynpro_repid     = sy-repid
          dynpro_dynnr     = sy-dynnr
          set_mode         = l_multiple
        IMPORTING
          sel_object       = ls_objec
        TABLES
          sel_hrobject_tab = lt_objec
        EXCEPTIONS
          OTHERS           = 6.
      IF sy-subrc = 0.
        l_clear = abap_true.
        LOOP AT lt_objec INTO ls_objec.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = ls_objec-objid i_clear = l_clear ).
            CLEAR l_clear.
          ELSE.
            set_value( EXPORTING i_field = <sel>-field_label i_high = ls_objec-objid i_clear = l_clear ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.

      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
        EXPORTING
          tabname           = mo_viewer->m_tabname
          fieldname         = l_fname
          callback_program  = sy-repid
          callback_form     = 'CALLBACK_F4_SEL' "callback_method - doesn't work for local class
          multiple_choice   = l_multiple
        TABLES
          return_tab        = return_tab
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          no_values_found   = 4
          OTHERS            = 5.

      IF sy-subrc = 0 AND lines( return_tab ) > 0.
        ASSIGN er_event_data->m_data->* TO FIELD-SYMBOL(<itab>).
        CLEAR <sel>-range.
        l_clear = abap_true.
        LOOP AT return_tab ASSIGNING FIELD-SYMBOL(<ret>) WHERE fieldname = l_fname.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = <ret>-fieldval i_clear = l_clear ).
            CLEAR l_clear.
          ELSE.
            set_value( EXPORTING i_field = <sel>-field_label i_high = <ret>-fieldval ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    er_event_data->m_event_handled = abap_true.
    raise_selection_done( ).
  ENDMETHOD.

  METHOD on_grid_button_click.
    DATA:
      l_tabfield TYPE rstabfield,
      ls_opt     TYPE rsoptions VALUE 'XXXXXXXXXX',
      lv_sign    TYPE raldb_sign,
      lv_option  TYPE raldb_opti.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    CASE es_col_id.
      WHEN 'OPTION_ICON'. "edit select logical expression type
        CALL FUNCTION 'SELECT_OPTION_OPTIONS'
          EXPORTING
            selctext     = 'nnnn'
            option_list  = ls_opt
          IMPORTING
            sign         = lv_sign
            option       = lv_option
          EXCEPTIONS
            delete_line  = 1
            not_executed = 2
            illegal_sign = 3
            OTHERS       = 4.
        IF sy-subrc = 0.
          <tab>-sign = lv_sign.
          <tab>-opti = lv_option.
        ELSEIF sy-subrc = 1.
          CLEAR: <tab>-low, <tab>-high,<tab>-sign, <tab>-opti, <tab>-range.
        ENDIF.
      WHEN 'MORE_ICON'. "edit ranges
        l_tabfield-tablename = mo_viewer->m_tabname.
        l_tabfield-fieldname = <tab>-field_label.

        CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
          EXPORTING
            title             = 'title'
            text              = 'text'
            tab_and_field     = l_tabfield
          TABLES
            range             = <tab>-range
          EXCEPTIONS
            no_range_tab      = 1
            cancelled         = 2
            internal_error    = 3
            invalid_fieldname = 4
            OTHERS            = 5.
        IF sy-subrc = 0.
          READ TABLE <tab>-range INDEX 1 INTO DATA(l_range).
          MOVE-CORRESPONDING l_range TO <tab>.
          IF <tab>-opti NE 'BT'.
            CLEAR <tab>-high.
          ENDIF.
        ENDIF.
    ENDCASE.
    update_sel_row( CHANGING c_sel_row = <tab> ).
    RAISE EVENT selection_done.
  ENDMETHOD.

  METHOD on_data_changed.
    DATA: l_start TYPE i,
          lv_time TYPE sy-uzeit.

    FIELD-SYMBOLS: <field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_cells>).
      READ TABLE mt_sel_tab INDEX <ls_cells>-row_id ASSIGNING FIELD-SYMBOL(<tab>).
      ASSIGN COMPONENT <ls_cells>-fieldname OF STRUCTURE <tab> TO <field>.
      READ TABLE mo_viewer->mt_alv_catalog WITH KEY fieldname = <tab>-field_label INTO DATA(l_cat).

      IF <field> IS NOT INITIAL AND <ls_cells>-value IS INITIAL.
        READ TABLE <tab>-range INTO DATA(l_second) INDEX 2.
        IF sy-subrc = 0.
          IF ( <ls_cells>-fieldname = 'LOW' AND <tab>-high IS INITIAL ) OR  ( <ls_cells>-fieldname = 'HIGH' AND <tab>-low IS INITIAL  ).
            DELETE <tab>-range INDEX 1.
          ELSE.
            CLEAR l_second.
          ENDIF.
        ENDIF.
      ENDIF.

      IF l_cat-convexit = 'ALPHA' AND NOT  <ls_cells>-value CA '+*'.
        <ls_cells>-value = |{ <ls_cells>-value ALPHA = IN }|.
        l_start = 128 - l_cat-dd_outlen.
        <ls_cells>-value = <ls_cells>-value+l_start(l_cat-dd_outlen).
      ENDIF.

      IF <ls_cells>-value IS NOT INITIAL.
        IF <tab>-int_type = 'D'.
          DATA: lv_date TYPE sy-datum.
          CALL FUNCTION 'CONVERT_DATE_INPUT'
            EXPORTING
              input                     = <ls_cells>-value
              plausibility_check        = abap_true
            IMPORTING
              output                    = lv_date
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.

          IF sy-subrc = 0.
            <ls_cells>-value = |{ lv_date DATE = USER }|.
          ENDIF.
        ELSEIF <tab>-int_type = 'T'.
          CALL FUNCTION 'CONVERT_TIME_INPUT'
            EXPORTING
              input                     = <ls_cells>-value
            IMPORTING
              output                    = lv_time
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.
          <ls_cells>-value = lv_time+0(2) && ':' && lv_time+2(2) && ':' && lv_time+4(2).
        ENDIF.
      ENDIF.
    ENDLOOP.
    CHECK sy-subrc = 0.

    IF l_second IS INITIAL.
      <field> = <ls_cells>-value.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = <ls_cells>-fieldname i_value = <ls_cells>-value ).
    ELSE.
      <tab>-low = l_second-low.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'LOW' i_value = l_second-low ).
      IF l_second-high CO '0 '.
        CLEAR l_second-high.
      ENDIF.
      <tab>-high = l_second-high.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'HIGH' i_value = l_second-high ).

      <tab>-opti = l_second-opti.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'OPTI' i_value = l_second-opti ).
      <tab>-sign = l_second-sign.
      er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'SIGN' i_value = l_second-sign ).
    ENDIF.

    update_sel_row( CHANGING c_sel_row = <tab> ).
    lcl_alv_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout  ).
    raise_selection_done( ).
  ENDMETHOD.

  METHOD on_data_changed_finished.
    CHECK e_modified IS NOT INITIAL.
    RAISE EVENT selection_done.
  ENDMETHOD.

  METHOD handle_context_menu_request.
    DATA: ls_func TYPE ui_func,
          lt_func TYPE ui_functions.

    DATA(l_index) = lcl_alv_common=>get_selected( mo_sel_alv ).

    IF l_index IS NOT INITIAL.
      READ TABLE mt_sel_tab INTO DATA(l_sel) INDEX l_index.
    ENDIF.

    e_object->get_functions( IMPORTING fcodes = DATA(lt_fcodes) ). "Inactivate all standard functions

    LOOP AT lt_fcodes INTO DATA(ls_fcode) WHERE fcode NE '&OPTIMIZE'.
      ls_func = ls_fcode-fcode.
      APPEND ls_func TO lt_func.
    ENDLOOP.

    e_object->hide_functions( lt_func ).
    e_object->add_separator( ).

    IF l_sel-range[]  IS NOT INITIAL OR l_index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'SEL_CLEAR'
          text  = 'Clear Select-Options'.
    ENDIF.

    IF l_sel-receiver IS NOT INITIAL OR l_index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DELR'
          text  = 'Delete receiver'.
    ENDIF.
  ENDMETHOD.

  METHOD handle_user_command.
    DATA: lv_sel_width TYPE i.

    IF e_ucomm = 'SEL_OFF'. "Hide select-options alv
      mo_viewer->m_visible = ''.

      lv_sel_width = 0.
      CALL METHOD mo_viewer->mo_splitter->get_column_width ##FM_SUBRC_OK
        EXPORTING
          id                = 1
        IMPORTING
          result            = mo_viewer->mo_sel_width
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.

      CALL METHOD mo_viewer->mo_splitter->set_column_width
        EXPORTING
          id    = 1
          width = lv_sel_width.
      mo_viewer->mo_alv->set_toolbar_interactive( ).
      RETURN.
    ENDIF.

    IF e_ucomm = 'SEL_CLEAR' OR e_ucomm = 'DELR'. "clear all selections
      mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).

      LOOP AT lt_sel_rows INTO DATA(l_row).
        READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX l_row-index.
        IF e_ucomm = 'SEL_CLEAR'.
          CLEAR : <sel>-low, <sel>-high, <sel>-sign, <sel>-opti, <sel>-range.
        ELSEIF e_ucomm = 'DELR'.
          IF <sel>-receiver IS NOT INITIAL.
            <sel>-receiver->shut_down( ).
            FREE <sel>-receiver.
            CLEAR <sel>-receiver.
            CLEAR <sel>-inherited.
          ENDIF.
        ENDIF.
        update_sel_row( CHANGING c_sel_row = <sel> ).
      ENDLOOP.
      RAISE EVENT selection_done.
    ENDIF.

    lcl_alv_common=>refresh( mo_viewer->mo_alv ).
    RAISE EVENT selection_done.
  ENDMETHOD.                           "handle_user_command
ENDCLASS.

CLASS lcl_appl IMPLEMENTATION.
  METHOD init_icons_table.
    m_option_icons = VALUE #(
     ( sign = space option = space  icon_name = icon_led_inactive )
     ( sign = 'I'   option = 'EQ'   icon_name = icon_equal_green )
     ( sign = 'I'   option = 'NE'   icon_name = icon_not_equal_green )
     ( sign = 'I'   option = 'LT'   icon_name = icon_less_green )
     ( sign = 'I'   option = 'LE'   icon_name = icon_less_equal_green )
     ( sign = 'I'   option = 'GT'   icon_name = icon_greater_green )
     ( sign = 'I'   option = 'GE'   icon_name = icon_greater_equal_green )
     ( sign = 'I'   option = 'CP'   icon_name = icon_pattern_include_green )
     ( sign = 'I'   option = 'NP'   icon_name = icon_pattern_exclude_green )
     ( sign = 'I'   option = 'BT'   icon_name = icon_interval_include_green )
     ( sign = 'I'   option = 'NB'   icon_name = icon_interval_exclude_green )
     ( sign = 'E'   option = 'EQ'   icon_name = icon_equal_red )
     ( sign = 'E'   option = 'NE'   icon_name = icon_not_equal_red )
     ( sign = 'E'   option = 'LT'   icon_name = icon_less_red )
     ( sign = 'E'   option = 'LE'   icon_name = icon_less_equal_red )
     ( sign = 'E'   option = 'GT'   icon_name = icon_greater_red )
     ( sign = 'E'   option = 'GE'   icon_name = icon_greater_equal_red )
     ( sign = 'E'   option = 'CP'   icon_name = icon_pattern_include_red )
     ( sign = 'E'   option = 'NP'   icon_name = icon_pattern_exclude_red )
     ( sign = 'E'   option = 'BT'   icon_name = icon_interval_include_red )
     ( sign = 'E'   option = 'NB'   icon_name = icon_interval_exclude_red ) ).
  ENDMETHOD.

  METHOD init_lang.
    SELECT c~spras t~sptxt INTO CORRESPONDING FIELDS OF TABLE mt_lang
      FROM t002c AS c
      INNER JOIN t002t AS t
      ON c~spras = t~sprsl
      WHERE t~spras = sy-langu
      ORDER BY c~ladatum DESCENDING c~lauzeit DESCENDING.
  ENDMETHOD.

  METHOD open_int_table.

    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND.
      r_tab = it_ref.
    ELSE.
      GET REFERENCE OF it_tab INTO r_tab.
    ENDIF.

    IF iv_dummy is INITIAL.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW #(  i_additional_name = iv_name ir_tab = r_tab ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
    ELSE.
     CALL FUNCTION 'Z_DUMMY_SCREEN'  EXPORTING it_tab = it_tab iv_name = iv_name show = iv_show .
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_rtti_tree IMPLEMENTATION .
  METHOD constructor.

    super->constructor( ).
    create_popup( ).
    "IF i_type = 'L'.
    cl_salv_tree=>factory(
         EXPORTING r_container = mo_box
         IMPORTING r_salv_tree = tree
         CHANGING t_table = tree_table ).
    "ENDIF.

    DATA(lo_setting) =  tree->get_tree_settings( ).
    lo_setting->set_hierarchy_header( i_header ).
    lo_setting->set_hierarchy_size( 30 ).
    lo_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

    DATA(lo_columns) = tree->get_columns( ).
    "lo_columns->set_optimize( abap_true ).
    lo_columns->get_column( 'VALUE' )->set_short_text( 'Value' ).
    lo_columns->get_column( 'VALUE' )->set_output_length( 40 ).

*    lo_columns->get_column( 'FULLNAME' )->set_short_text( 'Full name' ).
*    lo_columns->get_column( 'FULLNAME' )->set_output_length( 40 ).

    lo_columns->get_column( 'FULLNAME' )->set_visible( '' ).

    lo_columns->get_column( 'TYPENAME' )->set_short_text( 'Type' ).
    lo_columns->get_column( 'TYPENAME' )->set_output_length( 20 ).

    add_buttons( ).

    tree->get_nodes( )->expand_all( ).
    DATA(lo_event) = tree->get_event( ) .
    SET HANDLER hndl_double_click
                hndl_user_command FOR lo_event.

    tree->display( ).
  ENDMETHOD.

  METHOD create_popup.
    mo_box = create( i_width = 500 i_hight = 300 ).

    mo_box->set_caption( CONV #( 'TEST' ) ).

    IF lcl_appl=>m_ctrl_box_handler IS INITIAL.
      lcl_appl=>m_ctrl_box_handler = NEW #( ).
    ENDIF.
    SET HANDLER lcl_appl=>m_ctrl_box_handler->on_box_close FOR mo_box.
  ENDMETHOD.

  METHOD add_buttons.
    DATA(lo_functions) = tree->get_functions( ).
    lo_functions->set_all( ).

*    lo_functions->add_function(
*       name     = 'INITIALS'
*       icon     = CONV #( icon_start_viewer )
*       text     = 'Initials'
*       tooltip  = 'Show/hide initial values'
*       position = if_salv_c_function_position=>right_of_salv_functions ).

  ENDMETHOD.

  METHOD clear.

    tree->get_nodes( )->delete_all( ).

    CLEAR: m_globals_key,
           m_locals_key,
           m_ldb_key,
           m_debug_key,
           m_class_key,
           mt_vars,
           mt_classes_leaf,
           m_no_refresh.
  ENDMETHOD.


  METHOD add_node.

    main_node_key =
          tree->get_nodes( )->add_node(
            related_node   = ''
            collapsed_icon = iv_icon
            expanded_icon = iv_icon
            relationship   = if_salv_c_node_relation=>last_child
            row_style = if_salv_c_tree_style=>emphasized_a
            text           = CONV #( iv_name )
            folder         = abap_true
          )->get_key( ).

    CASE iv_name.
      WHEN 'Locals'.
        m_locals_key = main_node_key.
      WHEN 'Globals'.
        m_globals_key = main_node_key.
      WHEN 'LDB'.
        m_ldb_key = main_node_key.
      WHEN 'Class-data global variables'.
        m_class_key = main_node_key.
      WHEN 'Debug point'.
        m_debug_key = main_node_key.
    ENDCASE.
  ENDMETHOD.



  METHOD add_obj_var.

    DATA: lv_icon TYPE salv_de_tree_image,
          ls_tree TYPE ts_table,
          l_key   TYPE salv_de_node_key.

    IF iv_icon IS NOT SUPPLIED.
      lv_icon = icon_oo_class.
    ELSE.
      lv_icon = iv_icon.
    ENDIF.

    IF iv_full IS SUPPLIED.
      ls_tree-fullname = iv_full.
    ENDIF.

    IF iv_value IS SUPPLIED.
      ls_tree-value = iv_value.
    ENDIF.

    IF iv_key IS SUPPLIED.
      l_key = iv_key.
    ELSE.
      l_key = main_node_key.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = main_node_key.
    ENDIF.

    er_key = tree->get_nodes( )->add_node(
      related_node   = l_key
      relationship   = if_salv_c_node_relation=>last_child
      collapsed_icon = lv_icon
      expanded_icon  = lv_icon
      data_row       = ls_tree
      text           = iv_name
      folder         = abap_false
    )->get_key( ).
  ENDMETHOD.

  METHOD delete_node.
    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(l_node) =  lo_nodes->get_node( iv_key ).
    IF sy-subrc = 0.
      l_node->delete( ).
    ENDIF.
  ENDMETHOD.

  METHOD display.

    DATA(lo_columns) = tree->get_columns( ).
    lo_columns->get_column( 'KIND' )->set_visible( abap_false ).
    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(lt_nodes) =  lo_nodes->get_all_nodes( ).

    "expanding only first level nodes.
    DATA lt_sub TYPE salv_t_nodes.
    LOOP AT lt_nodes INTO DATA(l_node).
      READ TABLE lt_sub WITH KEY node = l_node-node TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        TRY.
            l_node-node->expand( ).
            lt_sub = l_node-node->get_subtree( ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.
    ENDLOOP.

    tree->display( ).
  ENDMETHOD.

  METHOD hndl_user_command.
    CONSTANTS: c_mask TYPE x VALUE '01'.

    "lcl_appl=>open_int_table( iv_name = 'Steps'   it_tab =  mo_debugger->mt_steps ).


  ENDMETHOD.

  METHOD hndl_double_click.
    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(l_node) =  lo_nodes->get_node( node_key ).
    DATA r_row TYPE REF TO data.
    DATA r_ref TYPE REF TO data.

    r_row = l_node->get_data_row( ).
    ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT 'REF' OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
    ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
    ASSIGN COMPONENT 'FULLNAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<fullname>).

    CASE <kind>.
      WHEN cl_abap_datadescr=>typekind_table.
        lcl_appl=>open_int_table( iv_name = <fullname> it_ref = <ref> ).
      WHEN cl_abap_datadescr=>typekind_string.
        NEW lcl_text_viewer( <ref> ).
    ENDCASE.
  ENDMETHOD.

  METHOD add_variable.
    DATA: lr_new   TYPE REF TO data,
          lr_struc TYPE REF TO data,
          l_name   TYPE string,
          l_key    TYPE salv_de_node_key,
          l_rel    TYPE salv_de_node_relation.

    FIELD-SYMBOLS: <new>      TYPE any,
                   <tab_from> TYPE ANY TABLE,
                   <tab_to>   TYPE STANDARD TABLE.

    IF i_icon IS SUPPLIED.
      m_icon = i_icon.
    ELSE.
      CLEAR m_icon.
    ENDIF.

    DATA l_full_name TYPE string.
    IF iv_full_name IS SUPPLIED.
      l_full_name = iv_full_name.
    ELSE.
      l_full_name = iv_root_name.
    ENDIF.

    l_name = iv_root_name.
    DESCRIBE FIELD io_var TYPE DATA(lv_type).
    IF lv_type NE cl_abap_typedescr=>typekind_table.
      CREATE DATA lr_new LIKE io_var.
      ASSIGN lr_new->*  TO <new>.
      <new> = io_var.
    ELSE.
      ASSIGN io_var TO <tab_from>.
      CREATE DATA lr_struc LIKE LINE OF <tab_from>.
      ASSIGN lr_struc->* TO FIELD-SYMBOL(<ls_record>).
      CREATE DATA lr_new LIKE STANDARD TABLE OF <ls_record>.
      ASSIGN lr_new->* TO <tab_to>.
      <tab_to> = <tab_from>.
    ENDIF.
    m_variable = lr_new.

    DATA td TYPE sydes_desc.
    DESCRIBE FIELD io_var INTO td.
    IF td-types[ 1 ]-type = 'r'.
      m_object = io_var.
    ENDIF.

    IF iv_key IS SUPPLIED AND iv_key IS NOT INITIAL.
      l_key = iv_key.
    ELSE.
      l_key = main_node_key.
    ENDIF.

    l_rel = if_salv_c_node_relation=>last_child.
    ASSIGN m_variable->* TO FIELD-SYMBOL(<new_value>).
    READ TABLE mt_vars WITH KEY name = l_full_name INTO DATA(l_var).

    IF sy-subrc = 0.
      DATA(lo_nodes) = tree->get_nodes( ).
      DATA(l_node) =  lo_nodes->get_node( l_var-key ).
      DATA r_row TYPE REF TO data.
      DATA r_ref TYPE REF TO data.

      IF sy-subrc = 0.
        TRY.
            r_row = l_node->get_data_row( ).
            ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
            ASSIGN COMPONENT 'REF' OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
            ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
            r_ref = <ref>.
            ASSIGN r_ref->* TO FIELD-SYMBOL(<old_value>).


          CATCH cx_root.
        ENDTRY.
      ENDIF.
    ELSE.


      IF lv_type NE cl_abap_typedescr=>typekind_table.
        IF <new> IS INITIAL AND m_hide IS NOT INITIAL.
          RETURN.
        ENDIF.
      ELSE.
        IF <tab_to> IS INITIAL AND m_hide IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA(l_root_key) = traverse(
     io_type_descr = cl_abap_typedescr=>describe_by_data_ref( m_variable )
     iv_parent_key = l_key
     iv_rel  = l_rel
     iv_name = l_name
     iv_fullname = l_full_name
     ir_up = m_variable
     iv_parent_name = l_name
     i_cl_leaf = i_cl_leaf ).

    READ TABLE mt_vars WITH KEY name = l_full_name TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
      <vars>-leaf = m_leaf.
      <vars>-name = l_full_name.
      <vars>-key = l_root_key.
      <vars>-ref = m_variable.
      <vars>-cl_leaf = i_cl_leaf.
      <vars>-tree = me.
    ENDIF.

    IF l_rel = if_salv_c_node_relation=>next_sibling.
      IF <kind> NE 'v' AND <kind> NE 'u'.
        IF l_node IS NOT INITIAL.
          l_node->delete( ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD traverse.
    CASE io_type_descr->kind.
      WHEN c_kind-struct.
        e_root_key = traverse_struct( io_type_descr = io_type_descr
                                      iv_parent_key = iv_parent_key
                                      iv_rel  = iv_rel
                                      iv_name = iv_name
                                      iv_fullname = iv_fullname
                                      i_cl_leaf = i_cl_leaf
                                      ir_up = ir_up iv_parent_name = iv_parent_name ).

      WHEN c_kind-table.
        e_root_key = traverse_table( io_type_descr = io_type_descr
                                     iv_parent_key = iv_parent_key
                                     iv_rel  = iv_rel
                                     iv_name = iv_name
                                     iv_fullname = iv_fullname
                                     ir_up = ir_up
                                     i_cl_leaf = i_cl_leaf
                                      iv_parent_name = iv_parent_name ).
      WHEN c_kind-elem.
        e_root_key = traverse_elem( io_type_descr = io_type_descr
                                    iv_parent_key = iv_parent_key
                                    iv_rel  = iv_rel
                                    iv_name = iv_name
                                    iv_fullname = iv_fullname
                                    ir_up = ir_up
                                    i_cl_leaf = i_cl_leaf
                                    iv_parent_name = iv_parent_name ).
    ENDCASE.
  ENDMETHOD.

  METHOD traverse_struct.
    DATA: lt_component    TYPE abap_component_tab,
          ls_component    LIKE LINE OF lt_component,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          ls_tree         TYPE ts_table,
          lv_text         TYPE lvc_value,
          lv_node_key     TYPE salv_de_node_key,
          lv_icon         TYPE salv_de_tree_image.

    lo_struct_descr ?= io_type_descr.
    ls_tree-ref =  ir_up.
    ls_tree-typename = lo_struct_descr->absolute_name.
    REPLACE FIRST OCCURRENCE OF '\TYPE=' IN ls_tree-typename+0(6) WITH ''.
    IF ls_tree-typename+0(1) = '%'.
      ls_tree-typename = |{ lo_struct_descr->type_kind }({ lo_struct_descr->length / 2 })|.
    ENDIF.

    ls_tree-kind = lo_struct_descr->type_kind.

    IF m_icon IS INITIAL.
      lv_icon = icon_structure.
    ELSE.
      lv_icon = m_icon.
    ENDIF.

    IF iv_name IS NOT INITIAL.
      lv_text = iv_name.
    ELSE.
      lv_text = ls_tree-typename.
    ENDIF.

    ls_tree-fullname = iv_fullname.
    IF lv_text IS NOT INITIAL.

      READ TABLE mt_vars WITH KEY name = iv_fullname INTO DATA(l_var).
      IF sy-subrc NE 0.
        e_root_key = m_new_node = lv_node_key =
          tree->get_nodes( )->add_node(
            related_node   = iv_parent_key
            relationship   = iv_rel
            collapsed_icon = lv_icon
            expanded_icon  = lv_icon
            data_row       = ls_tree
            text           = lv_text
            folder         = abap_true
          )->get_key( ).
      ELSE.
        lv_node_key = l_var-key.
      ENDIF.
    ENDIF.

    lt_component = lo_struct_descr->get_components( ).
    LOOP AT lt_component INTO ls_component. "WHERE name IS NOT INITIAL.
      DATA: lr_new_struc TYPE REF TO data.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<up>).
      IF ls_component-name IS INITIAL.
        lr_new_struc = ir_up.
      ELSE.
        ASSIGN COMPONENT ls_component-name OF STRUCTURE <up> TO FIELD-SYMBOL(<new>).
        GET REFERENCE OF <new> INTO lr_new_struc.
      ENDIF.

      traverse(
        io_type_descr = ls_component-type
        iv_parent_key = lv_node_key
        iv_rel  = if_salv_c_node_relation=>last_child
        iv_name = ls_component-name
        iv_fullname = |{ iv_fullname }-{ ls_component-name }|
        ir_up = lr_new_struc
        iv_parent_name = |{ iv_parent_name }-{ ls_component-name }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD traverse_elem.
    DATA: lo_elem_descr TYPE REF TO cl_abap_elemdescr,
          ls_tree       TYPE ts_table,
          lv_text       TYPE lvc_value,
          lv_icon       TYPE salv_de_tree_image,
          lv_node_key   TYPE salv_de_node_key,
          l_key         TYPE salv_de_node_key,
          l_rel         TYPE salv_de_node_relation.

    lo_elem_descr ?= io_type_descr.
    ls_tree-ref = ir_up.

    ls_tree-typename = lo_elem_descr->absolute_name.
    REPLACE FIRST OCCURRENCE OF '\TYPE=' IN ls_tree-typename WITH ''.
    IF ls_tree-typename+0(1) = '%'.
      ls_tree-typename = |{ lo_elem_descr->type_kind }({ lo_elem_descr->length / 2 })|.
    ENDIF.

    ls_tree-kind = lo_elem_descr->type_kind.

    ASSIGN ir_up->* TO FIELD-SYMBOL(<new_value>).
    IF iv_value IS SUPPLIED.
      ls_tree-value = iv_value.
    ELSE.
      IF <new_value> IS NOT INITIAL.
        ls_tree-value = <new_value>.
      ENDIF.
    ENDIF.

    CASE lo_elem_descr->type_kind.
      WHEN 'D'.
        lv_icon = icon_date.
      WHEN 'T'.
        lv_icon = icon_bw_time_sap.
      WHEN 'C'.
        lv_icon = icon_wd_input_field.
      WHEN 'P'.
        lv_icon = icon_increase_decimal.
      WHEN 'g'.
        lv_icon = icon_text_act.
      WHEN 'N' OR 'I'.
        lv_icon = icon_pm_order.

      WHEN OTHERS.
        lv_icon = icon_element.
    ENDCASE.

    lv_text = iv_name.

    IF ls_tree-value IS INITIAL AND m_hide IS NOT INITIAL.
      RETURN.
    ENDIF.

    ls_tree-fullname = iv_fullname.

    IF iv_parent_key IS NOT INITIAL.
      l_key = iv_parent_key.
    ELSE.
      l_key = main_node_key.
    ENDIF.

    l_rel = iv_rel.
    READ TABLE mt_vars WITH KEY name = iv_fullname INTO DATA(l_var).
    IF sy-subrc = 0.

      DATA(lo_nodes) = tree->get_nodes( ).
      DATA(l_node) =  lo_nodes->get_node( l_var-key ).
      DATA r_row TYPE REF TO data.
      DATA r_ref TYPE REF TO data.

      TRY.
          r_row = l_node->get_data_row( ).
          ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
          ASSIGN COMPONENT 'REF' OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
          ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
          r_ref = <ref>.
          ASSIGN r_ref->* TO FIELD-SYMBOL(<old_value>).
          IF <old_value> NE <new_value>.
            l_key = l_var-key.
            l_rel = if_salv_c_node_relation=>next_sibling.
            IF <kind> NE 'v' AND <kind> NE 'u'.
              DELETE mt_vars WHERE name = iv_fullname.
            ENDIF.
          ELSE.
            IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
              IF <kind> NE 'v' AND <kind> NE 'u'.
                DELETE mt_vars WHERE name = iv_fullname.
                l_node->delete( ).
              ENDIF.
            ENDIF.


          ENDIF.

          IF <new_value> IS INITIAL AND m_hide IS NOT INITIAL.
            DELETE mt_vars WHERE name = iv_fullname.
            l_node->delete( ).
            RETURN.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ELSE.


      IF <new_value> IS INITIAL AND m_hide IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    e_root_key = tree->get_nodes( )->add_node(
       related_node   = l_key
       relationship   = l_rel
       data_row       = ls_tree
       collapsed_icon = lv_icon
       expanded_icon  = lv_icon
       text           = lv_text
       folder         = abap_false )->get_key( ).


    IF l_rel = if_salv_c_node_relation=>next_sibling AND l_node IS NOT INITIAL.
      l_node->delete( ).
    ENDIF.
  ENDMETHOD.

  METHOD check_change.
  ENDMETHOD.

  METHOD traverse_table.
    DATA: lo_table_descr TYPE REF TO cl_abap_tabledescr,
          ls_tree        TYPE ts_table,
          lv_text        TYPE lvc_value,
          lv_node_key    TYPE salv_de_node_key,
          lv_icon        TYPE salv_de_tree_image,
          l_key          TYPE salv_de_node_key,
          l_rel          TYPE salv_de_node_relation.

    FIELD-SYMBOLS: <tab> TYPE ANY TABLE.

    ASSIGN ir_up->* TO <tab>.
    DATA(lines) = lines( <tab> ).
    ls_tree-ref = ir_up.

    lo_table_descr ?= io_type_descr.

    ls_tree-fullname = |{ iv_name } ({ lines })|.
    ls_tree-kind = lo_table_descr->type_kind.
    ls_tree-typename = replace(  val = lo_table_descr->absolute_name sub = '\TYPE=' with = ''   ).
    lv_icon = icon_view_table.

    IF iv_name IS NOT INITIAL.
      lv_text = ls_tree-fullname.
    ELSE.
      lv_text = ls_tree-typename.
    ENDIF.

    l_rel = iv_rel.
    ASSIGN ir_up->* TO FIELD-SYMBOL(<new_value>).
    READ TABLE mt_vars WITH KEY name = iv_fullname INTO DATA(l_var).
    IF sy-subrc = 0.

      DATA(lo_nodes) = tree->get_nodes( ).
      DATA(l_node) =  lo_nodes->get_node( l_var-key ).
      DATA r_row TYPE REF TO data.
      DATA r_ref TYPE REF TO data.

      TRY.
          r_row = l_node->get_data_row( ).
          ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
          ASSIGN COMPONENT 'REF' OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
          ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
          r_ref = <ref>.
          ASSIGN r_ref->* TO FIELD-SYMBOL(<old_value>).
          IF <old_value> NE <new_value>.
            l_key = l_var-key.
            l_rel = if_salv_c_node_relation=>next_sibling.
            IF <kind> NE 'v' AND <kind> NE 'u'.
              DELETE mt_vars WHERE name = iv_fullname.
            ENDIF.
          ELSE.
            IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
              IF <kind> NE 'v' AND <kind> NE 'u'.
                DELETE mt_vars WHERE name = iv_fullname.
                l_node->delete( ).
              ENDIF.
            ENDIF.

          ENDIF.

          IF <new_value> IS INITIAL. "AND m_hide IS NOT INITIAL.
            DELETE mt_vars WHERE name = iv_fullname.
            l_node->delete( ).
            RETURN.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ELSE.



    ENDIF.

    IF lines > 0. "OR  m_hide IS INITIAL.
      READ TABLE mt_vars WITH KEY name = iv_parent_name TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.

        ls_tree-fullname = iv_fullname.
        e_root_key =
          tree->get_nodes( )->add_node(
            related_node   = iv_parent_key
            relationship   = iv_rel
            collapsed_icon = lv_icon
            expanded_icon  = lv_icon
            data_row       = ls_tree
            text           = lv_text
            folder         = abap_true
          )->get_key( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_dragdrop IMPLEMENTATION.
  METHOD drag.
    DATA(dataobj) = NEW lcl_dd_data( ).
    dataobj->m_row = e_row-index.
    dataobj->m_column = e_column.
    e_dragdropobj->object = dataobj.
  ENDMETHOD.

  METHOD drop."It should be refactored someday...
    DATA: ls_row          TYPE lcl_types=>t_sel_row,
          lv_set_receiver.

    LOOP AT lcl_appl=>mt_obj INTO DATA(lo).
      "to
      IF lo-alv_viewer->mo_sel IS BOUND.
        IF e_dragdropobj->droptargetctrl = lo-alv_viewer->mo_sel->mo_sel_alv.
          DATA(lo_to) = lo-alv_viewer->mo_sel.
        ENDIF.
      ENDIF.

      "from tab
      IF lo-alv_viewer->mo_alv = e_dragdropobj->dragsourcectrl.
        DATA(lo_from_tab) = lo-alv_viewer.
        CONTINUE.
      ENDIF.

      IF e_dragdropobj->dragsourcectrl = lo-alv_viewer->mo_sel->mo_sel_alv.
        DATA(lo_from_sel) = lo-alv_viewer->mo_sel.
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_cells( IMPORTING et_cell = DATA(lt_sel_cells) ).
      ENDIF.
    ENDLOOP.

    IF lo_from_tab IS BOUND." tab to select
      FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE,
                     <f_field> TYPE any.
      lo_from_tab->mo_alv->get_selected_cells( IMPORTING et_cell = lt_sel_cells  ).
      lo_from_tab->mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(lt_sel_col)  ).

      LOOP AT lt_sel_col INTO DATA(l_col).
        TRY.
            lo_from_tab->mt_alv_catalog[ fieldname = l_col-fieldname ]-style = cl_gui_alv_grid=>mc_style_button.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        READ TABLE lo_from_tab->mo_column_emitters WITH KEY column = l_col ASSIGNING FIELD-SYMBOL(<emitter>).
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO lo_from_tab->mo_column_emitters ASSIGNING <emitter>.
          <emitter>-column = l_col.
          <emitter>-emitter = NEW #( ).
        ENDIF.
      ENDLOOP.

      IF sy-subrc = 0.
        lv_set_receiver = abap_true.
        CALL METHOD lo_from_tab->mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = lo_from_tab->mt_alv_catalog.
      ENDIF.

      TRY.
          ASSIGN lo_from_tab->mr_table->* TO <f_tab>.
          READ TABLE lo_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to_tab>) INDEX e_row.
          LOOP AT lt_sel_cells INTO DATA(l_cell).
            IF sy-tabix = 1.
              DATA(l_colname) = l_cell-col_id-fieldname.
            ENDIF.
            READ TABLE <f_tab> INDEX l_cell-row_id ASSIGNING FIELD-SYMBOL(<f_str>).
            ASSIGN COMPONENT l_colname OF STRUCTURE <f_str> TO <f_field>.
            IF sy-subrc = 0.
              IF lv_set_receiver IS NOT INITIAL.
                IF <to_tab>-receiver IS BOUND.
                  <to_tab>-receiver->shut_down( ).
                ENDIF.
                CREATE OBJECT <to_tab>-receiver
                  EXPORTING
                    io_transmitter = <emitter>-emitter
                    i_from_field   = CONV #( lt_sel_cells[ 1 ]-col_id )
                    i_to_field     = <to_tab>-field_label
                    io_sel_to      = lo_to
                    io_tab_from    = lo_from_tab.
                SET HANDLER <to_tab>-receiver->on_grid_button_click FOR lo_from_tab->mo_alv.
              ENDIF.

              IF <to_tab>-range IS INITIAL.
                <to_tab>-low = <f_field>.
              ENDIF.
              IF NOT line_exists( <to_tab>-range[ low = <f_field> ] ).
                APPEND VALUE #( sign = 'I' opti = 'EQ' low = <f_field>  ) TO <to_tab>-range.
              ENDIF.
            ENDIF.
          ENDLOOP.
          lo_to->update_sel_row( CHANGING c_sel_row = <to_tab> ).
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
      ENDTRY.
    ENDIF.

    "select to select
    IF lo_from_sel NE lo_to.
      IF lt_sel_rows[] IS INITIAL.
        DELETE lt_sel_cells WHERE col_id NE 'FIELD_LABEL'.
        LOOP AT lt_sel_cells INTO DATA(l_sel).
          APPEND INITIAL LINE TO lt_sel_rows ASSIGNING FIELD-SYMBOL(<row>).
          <row>-index = l_sel-row_id-index.
        ENDLOOP.
      ENDIF.

      LOOP AT lt_sel_rows ASSIGNING <row>.
        READ TABLE lo_from_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<from_tab>) INDEX <row>-index.
        IF lines( lt_sel_rows ) = 1.
          READ TABLE lo_to->mt_sel_tab ASSIGNING <to_tab> INDEX e_row.
        ELSE.
          READ TABLE lo_to->mt_sel_tab ASSIGNING <to_tab> WITH KEY field_label = <from_tab>-field_label.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING <from_tab> TO ls_row.
        MOVE-CORRESPONDING ls_row TO <to_tab>.
        <from_tab>-emitter = icon_workflow_external_event.
        <to_tab>-inherited = icon_businav_value_chain.
        IF <from_tab>-transmitter IS INITIAL.
          CREATE OBJECT <from_tab>-transmitter.
        ENDIF.
        IF <to_tab>-receiver IS NOT INITIAL.
          <to_tab>-receiver->shut_down( ). "receiver clearing
          FREE <to_tab>-receiver.
        ENDIF.
        CREATE OBJECT <to_tab>-receiver
          EXPORTING
            io_transmitter = <from_tab>-transmitter
            io_sel_to      = lo_to
            i_to_field     = <to_tab>-field_label.
      ENDLOOP.
    ENDIF.

    DATA(lo_alv) = CAST cl_gui_alv_grid( e_dragdropobj->dragsourcectrl ).
    lcl_alv_common=>refresh( EXPORTING i_obj = lo_alv ).

    lo_alv ?= e_dragdropobj->droptargetctrl.
    lo_to->raise_selection_done( ).
  ENDMETHOD.
ENDCLASS.
