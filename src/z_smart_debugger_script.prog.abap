*<SCRIPT:PERSISTENT>
"REPORT  SMART_DEBUGGER_SCRIPT.

*<SCRIPT:HEADER>
*<SCRIPTNAME>Z_SMART_DEBUGGER_SCRIPT</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SINGLE_RUN>X</SINGLE_RUN>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*<SCRIPT:PERSISTENT>

*<SCRIPT:HEADER>
*<SCRIPTNAME>Z_SMART_DEBUGGER_TEST</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>

"REPORT text.
*  & Smart  Debugger (Project ARIADNA - Advanced Reverse Ingeneering Abap Debugger with New Analytycs )
*  & Multi-windows program for viewing all objects and data structures in debug
*  &---------------------------------------------------------------------*
*  & version: beta 0.9.600
*  & Git https://github.com/ysichov/SDDE
*  & RU description - https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/
*  & EN description - https://github.com/ysichov/SDDE/wiki

*  & Written by Yurii Sychov
*  & e-mail:   ysichov@gmail.com
*  & blog:     https://ysychov.wordpress.com/blog/
*  & LinkedIn: https://www.linkedin.com/in/ysychov/
*  &---------------------------------------------------------------------*

*  & External resources
*  & https://github.com/WegnerDan/abapMermaid
*  & https://github.com/ysichov/abapMermaid - should be used this fork with scroll enabled
*  & https://gist.github.com/AtomKrieg/7f4ec2e2f49b82def162e85904b7e25b - data object visualizer

*  & Inspired by
*  & https://habr.com/ru/articles/504908/
*  & https://github.com/larshp/ABAP-Object-Visualizer - Abap Object Visualizer
*  & https://github.com/ysichov/SDE_abapgit - Simple Data Explorer

CLASS lcl_ai DEFINITION DEFERRED.
CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.
CLASS lcl_rtti_tree DEFINITION DEFERRED.
CLASS lcl_ace_window DEFINITION DEFERRED.
CLASS lcl_table_viewer DEFINITION DEFERRED.
CLASS lcl_mermaid DEFINITION DEFERRED.
CLASS lcl_appl DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF selection_display,
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
      END OF selection_display,
      BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string, "aqadh_range_value,
        high        TYPE string, "aqadh_range_value,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row.


    TYPES: BEGIN OF sign_option_icon_s,
             sign          TYPE tvarv_sign,
             option        TYPE tvarv_opti,
             icon_name(64) TYPE c,
             icon          TYPE aqadh_type_of_icon,
           END OF sign_option_icon_s,

           BEGIN OF var_table,
             step          TYPE i,
             stack         TYPE i,
             program(40)   TYPE c,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             first         TYPE xfeld,
             is_appear     TYPE xfeld,
             del           TYPE xfeld,
             leaf          TYPE string,
             name(1000)               ,
             path          TYPE string,
             short         TYPE string,
             key           TYPE salv_de_node_key,
             parent        TYPE string,
             cl_leaf       TYPE int4,
             ref           TYPE REF TO data,
             type          TYPE string,
             instance      TYPE string,
             objname       TYPE string,
             done          TYPE xfeld,
           END OF var_table,

           variables TYPE STANDARD TABLE OF var_table WITH NON-UNIQUE DEFAULT KEY,

           BEGIN OF var_table_temp,
             step          TYPE i,
             stack         TYPE i,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             name          TYPE string,
             value         TYPE string,
             first         TYPE xfeld,
             is_appear     TYPE xfeld,
             del           TYPE xfeld,
             program(40)   TYPE c,
             leaf          TYPE string,
             path          TYPE string,
             type          TYPE string,
             instance      TYPE string,
             objname       TYPE string,
             ref           TYPE REF TO data,
           END OF var_table_temp,

           BEGIN OF var_table_h,
             step          TYPE i,
             program(40)   TYPE c,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             leaf          TYPE string,
             name          TYPE string,
             path          TYPE string,
             parent        TYPE string,
             short         TYPE string,
             cl_leaf       TYPE int4,  "?
             ref           TYPE REF TO data,
             tree          TYPE REF TO lcl_rtti_tree,
             time          LIKE sy-uname,
           END OF var_table_h,

           BEGIN OF t_obj,
             name       TYPE string,
             alv_viewer TYPE REF TO lcl_table_viewer,
           END OF t_obj,

           BEGIN OF t_popup,
             parent TYPE REF TO cl_gui_dialogbox_container,
             child  TYPE REF TO cl_gui_dialogbox_container,
           END OF t_popup,

           BEGIN OF t_classes_types,
             name TYPE string,
             full TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_types,

           BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang,

           BEGIN OF t_stack,
             step       TYPE i,
             "stackpointer TYPE tpda_stack_pointer,
             stacklevel TYPE tpda_stack_level,
             line       TYPE tpda_sc_line,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             program    TYPE tpda_program,
             include    TYPE tpda_include,
           END OF t_stack,

           BEGIN OF t_step_counter,
             step       TYPE i,
             stacklevel TYPE tpda_stack_level,
             line       TYPE tpda_sc_line,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             first      TYPE xfeld,
             last       TYPE xfeld,
             program    TYPE tpda_program,
             include    TYPE tpda_include,
             time       LIKE sy-uzeit,
           END OF t_step_counter,
           tt_steps TYPE STANDARD TABLE OF t_step_counter WITH EMPTY KEY.

    CLASS-DATA: m_option_icons    TYPE TABLE OF sign_option_icon_s,
                mt_lang           TYPE TABLE OF t_lang,
                mt_obj            TYPE TABLE OF t_obj, "main object table
                mt_popups         TYPE TABLE OF t_popup, "dependents popups
                c_dragdropalv     TYPE REF TO cl_dragdrop,
                is_mermaid_active TYPE xfeld.

    CLASS-DATA: mt_sel TYPE TABLE OF selection_display.


    CLASS-METHODS:
      init_icons_table,
      init_lang,
      check_mermaid,
      open_int_table IMPORTING it_tab    TYPE ANY TABLE OPTIONAL
                               it_ref    TYPE REF TO data OPTIONAL
                               i_name    TYPE string
                               io_window TYPE REF TO lcl_ace_window.

ENDCLASS.

CLASS lcl_popup DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA m_counter              TYPE i.
    DATA: m_additional_name      TYPE string,
          mo_box                 TYPE REF TO cl_gui_dialogbox_container,
          mo_splitter            TYPE REF TO cl_gui_splitter_container,
          mo_splitter_imp_exp    TYPE REF TO cl_gui_splitter_container,
          mo_variables_container TYPE REF TO cl_gui_container,
          mo_tables_container    TYPE REF TO cl_gui_container.

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

    DATA: top  TYPE i,
          left TYPE i.

    ADD 1 TO m_counter.
    top  = left = 1 + 2 * ( m_counter DIV 5 ) +  ( m_counter MOD 5 ) * 10.

    CREATE OBJECT ro_box
      EXPORTING
        width                       = i_width
        height                      = i_hight
        top                         = top
        left                        = left
        caption                     = i_name
        lifetime                    = 2
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
    LOOP AT lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>) WHERE parent = sender .
      <popup>-child->free( ).
      CLEAR <popup>-child.
    ENDLOOP.
    IF sy-subrc <> 0.
      DELETE  lcl_appl=>mt_popups WHERE child = sender.
    ENDIF.
    DELETE lcl_appl=>mt_popups WHERE child IS INITIAL.
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

CLASS lcl_alv_common DEFINITION.

  PUBLIC SECTION.
    CONSTANTS: c_white(4) TYPE x VALUE '00000001'. "white background

    CLASS-METHODS:
      refresh IMPORTING i_obj TYPE REF TO cl_gui_alv_grid i_layout TYPE lvc_s_layo OPTIONAL i_soft TYPE char01 OPTIONAL,
      translate_field IMPORTING i_lang TYPE ddlanguage OPTIONAL CHANGING c_fld TYPE lvc_s_fcat,
      get_selected IMPORTING i_obj TYPE REF TO cl_gui_alv_grid RETURNING VALUE(e_index) TYPE i.

ENDCLASS.

CLASS lcl_alv_common IMPLEMENTATION.

  METHOD refresh.

    DATA stable TYPE lvc_s_stbl.
    stable = 'XX'.
    IF i_layout IS SUPPLIED.
      i_obj->set_frontend_layout( i_layout ).
    ENDIF.
    i_obj->refresh_table_display( EXPORTING is_stable = stable i_soft_refresh = i_soft ).

  ENDMETHOD.

  METHOD translate_field.

    DATA: field_info TYPE TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = c_fld-tabname
        fieldname      = c_fld-fieldname
        langu          = i_lang
      TABLES
        dfies_tab      = field_info
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      READ TABLE field_info INDEX 1 INTO DATA(info).
      IF info-scrtext_l IS INITIAL AND info-scrtext_m IS INITIAL AND info-scrtext_s IS INITIAL.
        IF info-fieldtext IS NOT INITIAL.
          MOVE info-fieldtext TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ELSE.
          MOVE info-fieldname TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ENDIF.
      ELSE.
        c_fld-scrtext_l = info-scrtext_l.
        c_fld-scrtext_m = info-scrtext_m.
        c_fld-scrtext_s = info-scrtext_s.
        IF info-reptext IS NOT INITIAL.
          c_fld-reptext   = info-reptext.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_selected.

    i_obj->get_selected_cells( IMPORTING et_cell = DATA(sel_cells) ).
    IF lines( sel_cells ) > 0.
      e_index = sel_cells[ 1 ]-row_id.
    ELSE.
      i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
      IF lines( sel_rows ) > 0.
        e_index = sel_rows[ 1 ]-index.
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

CLASS lcl_debugger_script DEFINITION DEFERRED.

CLASS lcl_source_parser DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: parse_tokens IMPORTING i_program TYPE program io_debugger TYPE REF TO lcl_debugger_script.

ENDCLASS.

CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_obj,
             name TYPE string,
             obj  TYPE string,
           END OF t_obj,

           BEGIN OF t_sel_var,
             name   TYPE string,
             is_sel TYPE xfeld,
             refval TYPE REF TO data,
           END OF t_sel_var.

    DATA: mt_obj            TYPE TABLE OF t_obj,
          mt_compo          TYPE TABLE OF scompo,
          mt_locals         TYPE tpda_scr_locals_it,
          mt_globals        TYPE tpda_scr_globals_it,
          mt_ret_exp        TYPE tpda_scr_locals_it,
          m_counter         TYPE i,
          mt_steps          TYPE  TABLE OF lcl_appl=>t_step_counter, "source code steps
          mt_var_step       TYPE  TABLE OF lcl_appl=>var_table_h,
          m_step            TYPE i,
          m_is_find         TYPE xfeld,
          m_stop_stack      TYPE i,
          m_debug           TYPE x,
          m_refresh         TYPE xfeld, "to refactor
          m_update          TYPE xfeld,
          is_step           TYPE xfeld,
          ms_stack_prev     TYPE   lcl_appl=>t_stack,
          ms_stack          TYPE   lcl_appl=>t_stack,
          is_history        TYPE xfeld,
          m_hist_step       TYPE i,
          m_step_delta      TYPE i,
          mt_vars_hist_view TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_vars_hist      TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_state          TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mv_recurse        TYPE i,
          mt_classes_types  TYPE TABLE OF lcl_appl=>t_classes_types,
          mo_window         TYPE REF TO lcl_ace_window,
          mv_f7_stop        TYPE xfeld,
          m_f6_level        TYPE i,
          m_target_stack    TYPE i,
          mo_tree_imp       TYPE REF TO lcl_rtti_tree,
          mo_tree_local     TYPE REF TO lcl_rtti_tree,
          mo_tree_exp       TYPE REF TO lcl_rtti_tree,
          mt_selected_var   TYPE TABLE OF t_sel_var,
          mv_stack_changed  TYPE xfeld,
          m_variable        TYPE REF TO data,
          mt_new_string     TYPE TABLE OF  string,
          m_quick           TYPE tpda_scr_quick_info,
          mr_statements     TYPE RANGE OF string.

    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION,

      run_script,
      run_script_hist IMPORTING i_step  TYPE i OPTIONAL
                      EXPORTING es_stop TYPE xfeld
                      ,
      show_variables CHANGING it_var TYPE lcl_appl=>variables RETURNING VALUE(stop) TYPE xfeld,
      set_selected_vars,
      save_hist IMPORTING
                  i_name              TYPE clike
                  i_fullname          TYPE string
                  i_type              TYPE string
                  i_cl_leaf           TYPE int4
                  i_parent_calculated TYPE string
                  ir_up               TYPE any OPTIONAL
                  i_instance          TYPE string OPTIONAL,

      f5 RETURNING VALUE(stop) TYPE xfeld,
      f6 RETURNING VALUE(stop) TYPE xfeld,
      f7 RETURNING VALUE(stop) TYPE xfeld,
      f8 RETURNING VALUE(stop) TYPE xfeld,
      make_step,
      hndl_script_buttons IMPORTING i_stack_changed TYPE xfeld
                          RETURNING VALUE(stop)     TYPE xfeld,
      get_obj_index IMPORTING i_name TYPE any RETURNING VALUE(e_index) TYPE string,
      create_reference         IMPORTING i_name            TYPE string
                                         i_type            TYPE string
                                         i_shortname       TYPE string OPTIONAL
                                         i_quick           TYPE tpda_scr_quick_info
                                         i_parent          TYPE string OPTIONAL
                               RETURNING VALUE(e_root_key) TYPE salv_de_node_key,
      show_step.

  PRIVATE SECTION.

    CONSTANTS: BEGIN OF c_kind,
                 struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                 table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                 elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                 class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                 intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                 ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
               END OF c_kind.

    METHODS: transfer_variable IMPORTING i_name              TYPE string
                                         i_type              TYPE string
                                         i_shortname         TYPE string OPTIONAL
                                         i_value             TYPE string OPTIONAL
                                         i_parent_calculated TYPE string OPTIONAL
                                         i_cl_leaf           TYPE int4 OPTIONAL
                                         i_instance          TYPE string OPTIONAL,

      create_simple_var IMPORTING i_name        TYPE string
                        RETURNING VALUE(er_var) TYPE REF TO data,

      create_simple_string IMPORTING i_name          TYPE string
                           RETURNING VALUE(e_string) TYPE string,

      create_struc         IMPORTING  i_name          TYPE string
                           RETURNING  VALUE(er_struc) TYPE REF TO data
                           EXCEPTIONS type_not_found,

      create_struc2         IMPORTING i_name      TYPE string
                                      i_shortname TYPE string OPTIONAL,

      get_class_name   IMPORTING i_name        TYPE string
                       RETURNING VALUE(e_name) TYPE string,

      get_deep_struc       IMPORTING i_name TYPE string
                                     r_obj  TYPE REF TO data,

      get_table  IMPORTING i_name TYPE string
                 CHANGING  c_obj  TYPE REF TO data,

      read_class_globals.

    METHODS traverse
      IMPORTING
        io_type_descr       TYPE REF TO cl_abap_typedescr
        i_name              TYPE clike
        i_fullname          TYPE string OPTIONAL
        i_type              TYPE string
        ir_up               TYPE REF TO data OPTIONAL
        i_parent_calculated TYPE string OPTIONAL
        i_struc_name        TYPE string OPTIONAL
        i_instance          TYPE string OPTIONAL
        i_cl_leaf           TYPE int4
        i_ref               TYPE xfeld OPTIONAL
        i_suffix            TYPE string OPTIONAL.

    METHODS traverse_struct
      IMPORTING io_type_descr       TYPE REF TO cl_abap_typedescr
                i_name              TYPE clike
                i_fullname          TYPE string OPTIONAL
                i_type              TYPE string
                i_cl_leaf           TYPE int4
                ir_up               TYPE  REF TO data OPTIONAL
                i_parent_calculated TYPE string OPTIONAL
                i_struc_name        TYPE string OPTIONAL
                i_instance          TYPE string OPTIONAL
                i_suffix            TYPE string OPTIONAL.

    METHODS traverse_elem
      IMPORTING
        i_name              TYPE clike
        i_fullname          TYPE string OPTIONAL
        i_type              TYPE string
        i_value             TYPE any OPTIONAL
        ir_up               TYPE  REF TO data OPTIONAL
        i_parent_calculated TYPE string OPTIONAL
        i_cl_leaf           TYPE int4
        i_instance          TYPE string OPTIONAL.

ENDCLASS.

CLASS lcl_mermaid DEFINITION INHERITING FROM lcl_popup FRIENDS  lcl_debugger_script.

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_if,
             if_ind      TYPE i,
             end_ind     TYPE i,
             before_else TYPE i,
           END OF ts_if,
           tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY.


    DATA: mo_debugger     TYPE REF TO lcl_debugger_script,
          mo_mm_container TYPE REF TO cl_gui_container,
          mo_mm_toolbar   TYPE REF TO cl_gui_container,
          mo_toolbar      TYPE REF TO cl_gui_toolbar,
          mo_diagram      TYPE REF TO object,
          mv_type         TYPE string,
          ms_if           TYPE ts_if,
          mt_if           TYPE tt_if,
          mv_step         TYPE i,
          mt_steps        TYPE lcl_appl=>tt_steps.

    METHODS: constructor IMPORTING io_debugger TYPE REF TO lcl_debugger_script
                                   i_type      TYPE string,

      steps_flow IMPORTING i_direction TYPE ui_func OPTIONAL,
      magic_search IMPORTING i_direction TYPE ui_func OPTIONAL,
      code_execution_scanner,
      parse_call IMPORTING i_program TYPE program i_index TYPE i i_stack TYPE i i_event TYPE string,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      open_mermaid IMPORTING i_mm_string TYPE string.

ENDCLASS.

CLASS lcl_rtti_tree DEFINITION FINAL. " INHERITING FROM lcl_popup.

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
             path     TYPE string,
             instance TYPE string,
           END OF ts_table.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: main_node_key   TYPE salv_de_node_key,
          m_refresh       TYPE xfeld,
          m_leaf          TYPE string,
          m_hide          TYPE x,
          m_clear         TYPE flag,
          m_locals        TYPE x,
          m_globals       TYPE x,
          m_syst          TYPE x,
          m_class_data    TYPE x,
          m_ldb           TYPE x,
          m_locals_key    TYPE salv_de_node_key,
          m_globals_key   TYPE salv_de_node_key,
          m_class_key     TYPE salv_de_node_key,
          m_syst_key      TYPE salv_de_node_key,
          m_ldb_key       TYPE salv_de_node_key,
          m_icon          TYPE salv_de_tree_image,
          mt_vars         TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_classes_leaf TYPE TABLE OF t_classes_leaf,
          m_prg_info      TYPE tpda_scr_prg_info,
          mo_debugger     TYPE REF TO lcl_debugger_script,
          m_tree          TYPE REF TO cl_salv_tree.

    METHODS constructor IMPORTING i_header   TYPE clike DEFAULT 'View'
                                  i_type     TYPE xfeld OPTIONAL
                                  i_cont     TYPE REF TO cl_gui_container OPTIONAL
                                  i_debugger TYPE REF TO lcl_debugger_script OPTIONAL.

    METHODS del_variable IMPORTING  i_full_name TYPE string i_state TYPE xfeld OPTIONAL.

    METHODS clear.

    METHODS add_buttons IMPORTING i_type TYPE xfeld.
    METHODS add_node
      IMPORTING
        i_name TYPE string
        i_icon TYPE salv_de_tree_image OPTIONAL.

    METHODS add_obj_nodes
      IMPORTING
                is_var            TYPE lcl_appl=>var_table
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS delete_node IMPORTING i_key TYPE salv_de_node_key.
    METHODS display IMPORTING io_debugger TYPE REF TO lcl_debugger_script OPTIONAL.

    METHODS traverse
      IMPORTING
                io_type_descr       TYPE REF TO cl_abap_typedescr
                i_parent_key        TYPE salv_de_node_key
                i_rel               TYPE salv_de_node_relation
                is_var              TYPE lcl_appl=>var_table
                ir_up               TYPE REF TO data OPTIONAL
                i_parent_calculated TYPE string OPTIONAL
                i_struc_name        TYPE string OPTIONAL
      RETURNING VALUE(e_root_key)   TYPE salv_de_node_key.

    METHODS traverse_struct
      IMPORTING
                io_type_descr       TYPE REF TO cl_abap_typedescr
                i_parent_key        TYPE salv_de_node_key
                i_rel               TYPE salv_de_node_relation
                is_var              TYPE lcl_appl=>var_table
                ir_up               TYPE REF TO data OPTIONAL
                i_parent_calculated TYPE string OPTIONAL
                i_struc_name        TYPE string OPTIONAL
      RETURNING VALUE(e_root_key)   TYPE salv_de_node_key.

    METHODS traverse_elem
      IMPORTING
                io_type_descr       TYPE REF TO cl_abap_typedescr
                i_parent_key        TYPE salv_de_node_key
                i_rel               TYPE salv_de_node_relation
                is_var              TYPE lcl_appl=>var_table
                i_value             TYPE any OPTIONAL
                i_parent_calculated TYPE string OPTIONAL
      RETURNING VALUE(e_root_key)   TYPE salv_de_node_key.

    METHODS traverse_obj
      IMPORTING
                i_parent_key        TYPE salv_de_node_key
                i_rel               TYPE salv_de_node_relation
                is_var              TYPE lcl_appl=>var_table
                i_value             TYPE any OPTIONAL
                ir_up               TYPE REF TO data OPTIONAL
                i_parent_calculated TYPE string OPTIONAL
      RETURNING VALUE(e_root_key)   TYPE salv_de_node_key.

    METHODS traverse_table
      IMPORTING
                io_type_descr       TYPE REF TO cl_abap_typedescr
                i_parent_key        TYPE salv_de_node_key
                i_rel               TYPE salv_de_node_relation
                is_var              TYPE lcl_appl=>var_table
                ir_up               TYPE REF TO data OPTIONAL
                i_parent_calculated TYPE string OPTIONAL
      RETURNING VALUE(e_root_key)   TYPE salv_de_node_key.

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


    METHODS: hndl_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key,
      hndl_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.

ENDCLASS.

CLASS lcl_ai_api DEFINITION.

  PUBLIC SECTION.

    METHODS  call_openai  IMPORTING i_prompt TYPE string RETURNING VALUE(answer) TYPE string.
  PRIVATE SECTION.
    DATA: mv_api_key TYPE string.

    METHODS: build_request
      IMPORTING
        i_prompt  TYPE string
      EXPORTING
        e_payload TYPE string ,
      send_request
        IMPORTING
          i_payload  TYPE string
        EXPORTING
          e_response TYPE string,
      output
        IMPORTING
                  i_prompt      TYPE string
                  i_content     TYPE string
        RETURNING VALUE(answer) TYPE string.

ENDCLASS.

CLASS lcl_ai_api IMPLEMENTATION.

  METHOD call_openai.
    DATA: prompt   TYPE string,
          payload  TYPE string,
          response TYPE string.

    "Build payload
    CALL METHOD build_request
      EXPORTING
        i_prompt  = i_prompt
      IMPORTING
        e_payload = payload.

    CALL METHOD me->send_request
      EXPORTING
        i_payload  = payload
      IMPORTING
        e_response = response.

    answer = output(
      EXPORTING
        i_prompt  = i_prompt
        i_content = response ).

  ENDMETHOD.

  METHOD build_request.

    DATA: payload TYPE string.

    payload = |{ '{ "model": "mistral-tiny", "messages": [{ "role": "user", "content": "' && i_prompt &&  '" }], "max_tokens": 1000 } ' }|.

    e_payload = payload.
  ENDMETHOD.

  METHOD send_request.

    DATA: o_http_client TYPE REF TO if_http_client,
          response_body TYPE string,
          header        TYPE string.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = 'Z_LM' "SM59 local config
      IMPORTING
        client                   = o_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 13.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

    mv_api_key = 'lmstudio'. "any name for local LLMs or secret key for external
    "set request header
    o_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    o_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { mv_api_key }| ).

    o_http_client->request->set_method('POST').

    "set payload
    o_http_client->request->set_cdata( i_payload ).

    CALL METHOD o_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc = 0.
      CALL METHOD o_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.
      "Get response
      IF sy-subrc <> 0.
        response_body = o_http_client->response->get_data( ).
        e_response = response_body.
      ELSE.
        response_body = o_http_client->response->get_data( ).
        IF response_body IS NOT INITIAL.
          e_response = response_body.
        ELSE.
          e_response = 'Call was succeesful, but got no response'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD output.

    DATA: text(1000) TYPE c,
          string     TYPE string,
          content    TYPE string,
          reasoning  TYPE string.

    TYPES: BEGIN OF lty_s_message,
             role              TYPE string,
             content           TYPE string,
             reasoning_content TYPE string,
           END           OF lty_s_message,
           lty_t_message TYPE STANDARD TABLE OF lty_s_message WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF lty_s_choice,
             index         TYPE string,
             message       TYPE lty_s_message,
             logprobs      TYPE string,
             finish_reason TYPE string,
           END      OF lty_s_choice,
           BEGIN OF lty_s_base_chatgpt_res,
             id      TYPE string,
             object  TYPE string,
             created TYPE string,
             model   TYPE string,
             choices TYPE TABLE OF lty_s_choice WITH NON-UNIQUE DEFAULT KEY,
           END OF lty_s_base_chatgpt_res.

    DATA response TYPE lty_s_base_chatgpt_res.

    DATA: binary TYPE xstring.

    DATA: o_x2c TYPE REF TO cl_abap_conv_in_ce.
    o_x2c = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    binary = i_content.
    o_x2c->convert( EXPORTING input = binary
                     IMPORTING data  = string ).

    /ui2/cl_json=>deserialize( EXPORTING json = string CHANGING data = response ).

    IF  response-choices IS NOT INITIAL.
      content = response-choices[ 1 ]-message-content.
      reasoning = response-choices[ 1 ]-message-reasoning_content.
    ELSE.
      content = string.
    ENDIF.

    answer = content.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ai DEFINITION INHERITING FROM lcl_popup.

  PUBLIC SECTION.
    DATA: mo_ai_box               TYPE REF TO cl_gui_dialogbox_container,
          mo_ai_splitter          TYPE REF TO cl_gui_splitter_container,
          mo_ai_toolbar_container TYPE REF TO cl_gui_container,
          mo_ai_toolbar           TYPE REF TO cl_gui_toolbar,
          mo_prompt_container     TYPE REF TO cl_gui_container,
          mo_answer_container     TYPE REF TO cl_gui_container,
          mo_prompt_text          TYPE REF TO cl_gui_textedit,
          mo_answer_text          TYPE REF TO cl_gui_textedit,
          mv_prompt               TYPE string,
          mv_answer               TYPE string.

    METHODS: constructor IMPORTING io_source TYPE REF TO cl_ci_source_include
                                   io_parent TYPE REF TO cl_gui_dialogbox_container,
      add_ai_toolbar_buttons,
      hnd_ai_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode.

ENDCLASS.

CLASS lcl_ai IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_ai_box = create( i_name = 'SDDE Simple Debugger Data Explorer beta v. 0.9' i_width = 1400 i_hight = 400 ).
    CREATE OBJECT mo_ai_splitter
      EXPORTING
        parent  = mo_ai_box
        rows    = 3
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    "save new popup ref
    APPEND INITIAL LINE TO lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
    <popup>-parent = io_parent.
    <popup>-child = mo_ai_box.

    SET HANDLER on_box_close FOR mo_ai_box.

    mo_ai_splitter->get_container(
         EXPORTING
           row       = 1
           column    = 1
         RECEIVING
           container = mo_ai_toolbar_container ).

    mo_ai_splitter->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_prompt_container ).

    mo_ai_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_answer_container  ).

    mo_ai_splitter->set_row_height( id = 1 height = '3' ).

    mo_ai_splitter->set_row_sash( id    = 1
                                  type  = 0
                                  value = 0 ).


    SET HANDLER on_box_close FOR mo_ai_box.


    CREATE OBJECT mo_prompt_text
      EXPORTING
        parent                 = mo_prompt_container
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

    CREATE OBJECT mo_answer_text
      EXPORTING
        parent                 = mo_answer_container
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

    mo_answer_text->set_readonly_mode( ).

    CREATE OBJECT mo_ai_toolbar EXPORTING parent = mo_ai_toolbar_container.
    add_ai_toolbar_buttons( ).
    mo_ai_toolbar->set_visible( 'X' ).

    "set prompt
    DATA string TYPE TABLE OF char255.

    APPEND INITIAL LINE TO string ASSIGNING FIELD-SYMBOL(<str>).
    <str> = 'Explain please the meaning of this ABAP code'.
    mv_prompt = <str>.
    APPEND INITIAL LINE TO string ASSIGNING <str>.


    LOOP AT io_source->lines INTO DATA(line).
      APPEND INITIAL LINE TO string ASSIGNING <str>.
      <str> = line.
      mv_prompt = mv_prompt && <str>.
    ENDLOOP.

    mo_prompt_text->set_text_as_r3table( string ).
    cl_gui_control=>set_focus( mo_ai_box ).

  ENDMETHOD.

  METHOD add_ai_toolbar_buttons.

    DATA: button TYPE ttb_button,
          events TYPE cntl_simple_events,
          event  LIKE LINE OF events.

    button  = VALUE #(
     ( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ).

    mo_ai_toolbar->add_button_group( button ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_ai_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_ai_toolbar FOR mo_ai_toolbar.

  ENDMETHOD.

  METHOD hnd_ai_toolbar.

    DATA: prompt TYPE string.

    CASE fcode.

      WHEN 'AI'.

        cl_gui_cfw=>flush( ).
        DATA(o_ai) = NEW lcl_ai_api( ).

        DATA text TYPE TABLE OF char255.
        CALL METHOD mo_prompt_text->get_text_as_stream
          IMPORTING
            text = text.
        CLEAR mv_prompt.
        LOOP AT text INTO DATA(line).
          CONCATENATE mv_prompt line
                      "cl_abap_char_utilities=>newline
                 INTO mv_prompt.
        ENDLOOP.

        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN mv_prompt WITH ''.
        REPLACE ALL OCCURRENCES OF '#' IN mv_prompt WITH ''.
        REPLACE ALL OCCURRENCES OF REGEX '[[:cntrl:]]' IN mv_prompt WITH ' '.

        mv_answer = o_ai->call_openai( mv_prompt ).
        mo_answer_text->set_textstream( mv_answer ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ace_window DEFINITION INHERITING FROM lcl_popup .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
           END OF ts_table,

           BEGIN OF ts_calls,
             event TYPE string,
             type  TYPE string,
             name  TYPE string,
             outer TYPE string,
             inner TYPE string,
           END OF ts_calls,
           tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer,

           BEGIN OF ts_calls_line,
             class     TYPE string,
             eventtype TYPE string,
             eventname TYPE string,
             index     TYPE i,
           END OF ts_calls_line,
           tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY,

           BEGIN OF ts_kword,
             index     TYPE i,
             line      TYPE i,
             name      TYPE string,
             from      TYPE i,
             to        TYPE i,
             tt_calls  TYPE tt_calls,
             to_evtype TYPE string,
             to_evname TYPE string,
           END OF ts_kword,

           BEGIN OF calculated_var ,
             line TYPE i,
             name TYPE string,
           END OF calculated_var ,

           BEGIN OF composed_vars,
             line TYPE i,
             name TYPE string,
           END OF composed_vars,

           tt_kword      TYPE STANDARD TABLE OF ts_kword WITH EMPTY KEY,
           tt_calculated TYPE STANDARD TABLE OF calculated_var  WITH EMPTY KEY,
           tt_composed   TYPE STANDARD TABLE OF composed_vars WITH EMPTY KEY,

           BEGIN OF ts_params,
             class     TYPE string,
             event     TYPE string,
             name      TYPE string,
             param     TYPE string,
             type      TYPE char1,
             preferred TYPE char1,
           END OF ts_params,
           tt_params TYPE STANDARD TABLE OF ts_params WITH EMPTY KEY,

           BEGIN OF ts_int_tabs,
             eventtype TYPE string,
             eventname TYPE string,
             name      TYPE string,
             type      TYPE string,
           END OF ts_int_tabs,
           tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY,

           BEGIN OF ts_progs,
             include       TYPE program,
             source        TYPE REF TO cl_ci_source_include,
             scan          TYPE REF TO cl_ci_scan,
             t_keytokens   TYPE tt_kword,
             t_calculated  TYPE tt_calculated,
             t_composed    TYPE tt_composed,
             t_params      TYPE tt_params,
             tt_tabs       TYPE tt_tabs,
             tt_calls_line TYPE tt_calls_line,
           END OF ts_progs,

           BEGIN OF ts_locals,
             program    TYPE tpda_program,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             loc_fill   TYPE xfeld,
             locals_tab TYPE tpda_scr_locals_it,
             mt_fs      TYPE tpda_scr_locals_it,
           END OF ts_locals,

           BEGIN OF ts_globals,
             program     TYPE tpda_program,
             glob_fill   TYPE xfeld,
             globals_tab TYPE tpda_scr_globals_it,
             mt_fs       TYPE tpda_scr_locals_it,
           END OF ts_globals,

           BEGIN OF ts_watch,
             program TYPE string,
             line    TYPE i,
           END OF ts_watch,
           tt_watch TYPE STANDARD  TABLE OF ts_watch WITH EMPTY KEY,

           BEGIN OF ts_bpoint,
             program TYPE string,
             include TYPE string,
             line    TYPE i,
             type    TYPE char1,
             del     TYPE char1,
           END OF ts_bpoint,
           tt_bpoints TYPE STANDARD TABLE OF ts_bpoint WITH EMPTY KEY.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: m_version              TYPE x, " 0 - alpha, 01 - beta
          m_history              TYPE x,
          m_visualization        TYPE x,
          m_varhist              TYPE x,
          m_zcode                TYPE x,
          m_direction            TYPE x,
          m_prg                  TYPE tpda_scr_prg_info,
          m_debug_button         LIKE sy-ucomm,
          m_show_step            TYPE xfeld,
          mt_bpoints             TYPE tt_bpoints,
          mo_debugger            TYPE REF TO lcl_debugger_script,
          mo_splitter_code       TYPE REF TO cl_gui_splitter_container,
          mo_splitter_var        TYPE REF TO cl_gui_splitter_container,
          mo_splitter_steps      TYPE REF TO cl_gui_splitter_container,
          mo_toolbar_container   TYPE REF TO cl_gui_container,
          mo_importing_container TYPE REF TO cl_gui_container,
          mo_locals_container    TYPE REF TO cl_gui_container,
          mo_exporting_container TYPE REF TO cl_gui_container,
          mo_code_container      TYPE REF TO cl_gui_container,
          mo_imp_exp_container   TYPE REF TO cl_gui_container,
          mo_editor_container    TYPE REF TO cl_gui_container,
          mo_steps_container     TYPE REF TO cl_gui_container,
          mo_stack_container     TYPE REF TO cl_gui_container,
          mo_hist_container      TYPE REF TO cl_gui_container,
          mo_code_viewer         TYPE REF TO cl_gui_abapedit,
          mt_stack               TYPE TABLE OF lcl_appl=>t_stack,
          mo_toolbar             TYPE REF TO cl_gui_toolbar,
          mo_salv_stack          TYPE REF TO cl_salv_table,
          mo_salv_steps          TYPE REF TO cl_salv_table,
          mo_salv_hist           TYPE REF TO cl_salv_table,
          mt_breaks              TYPE tpda_bp_persistent_it,
          mt_watch               TYPE tt_watch,
          mt_coverage            TYPE tt_watch,
          m_hist_depth           TYPE i,
          m_start_stack          TYPE i,
          mt_source              TYPE STANDARD  TABLE OF ts_progs,
          mt_params              TYPE STANDARD  TABLE OF ts_params,
          mt_locals_set          TYPE STANDARD TABLE OF ts_locals,
          mt_globals_set         TYPE STANDARD TABLE OF ts_globals.

    METHODS: constructor IMPORTING i_debugger TYPE REF TO lcl_debugger_script i_additional_name TYPE string OPTIONAL,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      on_stack_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      set_program IMPORTING i_program TYPE program,
      show_coverage,

      on_editor_double_click  FOR EVENT dblclick OF cl_gui_abapedit IMPORTING sender,
      on_editor_border_click  FOR EVENT border_click OF cl_gui_abapedit IMPORTING line cntrl_pressed_set shift_pressed_set,

      set_program_line IMPORTING i_line LIKE sy-index OPTIONAL,
      create_code_viewer,
      show_stack.

ENDCLASS.

CLASS lcl_debugger_script IMPLEMENTATION.

  METHOD prologue.
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.

    CONSTANTS: c_mask TYPE x VALUE '01'.

    is_step = abap_on.
    lcl_appl=>check_mermaid( ).
    lcl_appl=>init_lang( ).
    lcl_appl=>init_icons_table( ).

    mo_window = NEW lcl_ace_window( me ).

    mo_tree_imp = NEW lcl_rtti_tree( i_header   = 'Importing parameters'
                                     i_type     = 'I'
                                     i_cont     = mo_window->mo_importing_container
                                     i_debugger = me ).

    mo_tree_local = NEW lcl_rtti_tree( i_header   = 'Variables'
                                       i_type     = 'L'
                                       i_cont     = mo_window->mo_locals_container
                                       i_debugger = me ).

    mo_tree_exp = NEW lcl_rtti_tree( i_header   = 'Exporting & Returning parameters'
                                     i_type     = 'E'
                                     i_cont     = mo_window->mo_exporting_container
                                     i_debugger = me ).

    mo_tree_local->m_locals = mo_tree_local->m_locals BIT-XOR c_mask.

  ENDMETHOD.

  METHOD create_simple_var.

    DATA: lr_symbsimple TYPE REF TO tpda_sys_symbsimple,
          o_elem        TYPE REF TO cl_abap_elemdescr.

    CALL METHOD cl_tpda_script_data_descr=>get_quick_info
      EXPORTING
        p_var_name   = i_name
      RECEIVING
        p_symb_quick = DATA(quick).

    ASSIGN quick-quickdata TO FIELD-SYMBOL(<lv_value>).
    lr_symbsimple ?= <lv_value>.
    ASSIGN lr_symbsimple->* TO FIELD-SYMBOL(<simple>).

    CALL METHOD cl_abap_complexdescr=>describe_by_name
      EXPORTING
        p_name         = quick-abstypename
      RECEIVING
        p_descr_ref    = DATA(o_type)
      EXCEPTIONS
        type_not_found = 1.

    o_elem ?= o_type.
    CREATE DATA er_var TYPE HANDLE o_elem.
    ASSIGN er_var->* TO FIELD-SYMBOL(<new_elem>).
    <new_elem> = <simple>-valstring.

  ENDMETHOD.

  METHOD create_simple_string.

    DATA: lr_string TYPE REF TO tpda_sys_symbstring,
          lr_struc  TYPE REF TO data,
          o_elem    TYPE REF TO cl_abap_elemdescr,
          depth     TYPE i VALUE 0.

    FIELD-SYMBOLS: <string>   TYPE tpda_sys_symbstring,
                   <lv_value> TYPE any.

    e_string = 'Unknown'.  " Default value

    TRY.
        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = i_name
          RECEIVING
            p_symb_quick = DATA(quick).

        ASSIGN quick-quickdata TO <lv_value>.

        " Handle only direct strings, not references
        IF quick-typid = 'g'. " Regular string
          lr_string ?= <lv_value>.
          ASSIGN lr_string->* TO <string>.
          e_string = <string>-valstring.
        ELSE.
          " For references and other types, just return a descriptive text
          e_string = |[{ quick-typid }:{ i_name }]|.
        ENDIF.

      CATCH cx_root.
        e_string = |Error: { i_name }|.
    ENDTRY.

  ENDMETHOD.

  METHOD create_struc.

    DATA: o_new_type    TYPE REF TO cl_abap_structdescr,
          comp_descr    TYPE abap_componentdescr,
          components    TYPE abap_component_tab,
          lr_symbsimple TYPE REF TO tpda_sys_symbsimple,
          o_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          comp_full     TYPE  tpda_scr_struct_comp_it,
          comp_it       TYPE tpda_script_struc_componentsit.

    FIELD-SYMBOLS: <lv_value> TYPE any,
                   <simple>   TYPE tpda_sys_symbsimple.

    CLEAR er_struc.
    CALL METHOD cl_tpda_script_data_descr=>get_quick_info
      EXPORTING
        p_var_name   = i_name
      RECEIVING
        p_symb_quick = DATA(quick).

    o_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    o_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

    lcl_rtti=>create_struc_handle( EXPORTING i_tname = CONV #( replace( val = quick-abstypename sub = '/TYPE=' with = '' ) ) IMPORTING e_handle = o_new_type ).
    IF o_new_type IS NOT INITIAL.
      CREATE DATA er_struc TYPE HANDLE o_new_type.
    ELSE.

      LOOP AT comp_full INTO DATA(comp).
        comp_descr-name = comp-compname.

        ASSIGN comp-symbquick-quickdata TO <lv_value>.
        lr_symbsimple ?= <lv_value>.
        ASSIGN lr_symbsimple->* TO <simple>.

        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = |{ i_name }-{ comp-compname }|
          RECEIVING
            p_symb_quick = DATA(quick_sub).

        CALL METHOD cl_abap_complexdescr=>describe_by_name
          EXPORTING
            p_name         = quick_sub-abstypename
          RECEIVING
            p_descr_ref    = DATA(o_type)
          EXCEPTIONS
            type_not_found = 1.

        IF sy-subrc = 0.
          comp_descr-type ?= o_type.
          APPEND comp_descr TO components.
        ENDIF.
      ENDLOOP.
      o_new_type  = cl_abap_structdescr=>create( components ).
      CREATE DATA er_struc TYPE HANDLE o_new_type.
    ENDIF.

    ASSIGN er_struc->* TO FIELD-SYMBOL(<new_struc>).

    LOOP AT comp_full INTO comp.
      ASSIGN comp-symbquick-quickdata TO <lv_value>.
      lr_symbsimple ?= <lv_value>.
      ASSIGN COMPONENT comp-compname OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<new>).
      <new> = lr_symbsimple->valstring.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_deep_struc.

    DATA: lr_struc      TYPE REF TO data,
          o_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          comp_full     TYPE  tpda_scr_struct_comp_it,
          comp_it       TYPE tpda_script_struc_componentsit,
          lr_symbsimple TYPE REF TO tpda_sys_symbsimple,
          lr_symbstring TYPE REF TO tpda_sys_symbstring,
          lr_symbstruc  TYPE REF TO tpda_sys_symbstruct,
          r_data        TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value> TYPE any.

    ASSIGN r_obj->* TO FIELD-SYMBOL(<new_deep>).
    o_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    o_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

    LOOP AT comp_full INTO DATA(comp).
      CASE comp-symbquick-metatype.
        WHEN cl_tpda_script_data_descr=>mt_simple.
          ASSIGN comp-symbquick-quickdata TO <lv_value>.
          lr_symbsimple ?= <lv_value>.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO FIELD-SYMBOL(<new>).
          <new> = lr_symbsimple->valstring.

        WHEN cl_tpda_script_data_descr=>mt_string.
          ASSIGN comp-symbquick-quickdata TO <lv_value>.
          lr_symbstring ?= <lv_value>.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO <new>.
          <new> = lr_symbstring->valstring.

        WHEN cl_tpda_script_data_descr=>mt_struct.
          ASSIGN comp-symbquick-quickdata TO <lv_value>.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO <new>.
          GET REFERENCE OF <new> INTO lr_struc.
          lr_symbstruc ?= <lv_value>.
          get_deep_struc( EXPORTING i_name = |{ i_name }-{ comp-compname }|
                                    r_obj  = lr_struc ).

        WHEN cl_tpda_script_data_descr=>mt_tab.
          FIELD-SYMBOLS: <new_table> TYPE ANY TABLE.
          ASSIGN COMPONENT comp-compname OF STRUCTURE <new_deep> TO <new_table>.
          GET REFERENCE OF <new_table> INTO r_data.
          get_table( EXPORTING i_name = |{ i_name }-{ comp-compname }|
                     CHANGING  c_obj  = r_data ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_table. "construct deep tables

    DATA: r_data        TYPE REF TO data,
          o_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone   TYPE REF TO data,
          o_tabl        TYPE REF TO cl_abap_tabledescr,
          o_struc       TYPE REF TO cl_abap_structdescr,
          r_struc       TYPE REF TO data.

    FIELD-SYMBOLS: <f>         TYPE ANY TABLE,
                   <new_table> TYPE ANY TABLE.

    ASSIGN c_obj->* TO <new_table>.
    o_tabl ?= cl_abap_typedescr=>describe_by_data( <new_table> ).
    o_struc ?= o_tabl->get_table_line_type( ).

    CREATE DATA r_data TYPE HANDLE o_struc.
    ASSIGN r_data->* TO FIELD-SYMBOL(<new_line>).

    o_table_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    TRY.
        table_clone = o_table_descr->elem_clone( ).
        ASSIGN table_clone->* TO <f>.
        DATA: count TYPE i.

        LOOP AT <f> ASSIGNING FIELD-SYMBOL(<fs>).
          count = sy-tabix.
          CLEAR <new_line>.

          DO.
            ASSIGN COMPONENT sy-index OF STRUCTURE <fs> TO FIELD-SYMBOL(<from>).
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.

            ASSIGN COMPONENT sy-index OF STRUCTURE <new_line> TO FIELD-SYMBOL(<to>).
            DESCRIBE FIELD <from> TYPE DATA(from_type).
            DESCRIBE FIELD   <to> TYPE DATA(to_type).

            IF from_type = to_type.
              <to> = <from>.
            ELSEIF to_type = 'h'.
              o_struc ?= cl_abap_typedescr=>describe_by_data( <fs> ).
              READ TABLE o_struc->components INDEX sy-index INTO DATA(comp_descr).
              GET REFERENCE OF <to> INTO r_data.
              get_table( EXPORTING i_name = |{ i_name }[ { count } ]-{ comp_descr-name }|
                         CHANGING  c_obj  = r_data ).
            ENDIF.
          ENDDO.
          INSERT <new_line> INTO TABLE <new_table>.
        ENDLOOP.
      CATCH cx_root.
        DATA(cnt) = o_table_descr->linecnt( ).
        DO cnt TIMES.
          r_struc = create_struc( i_name = |{ i_name }[{ sy-index }]| ).
          ASSIGN r_struc->* TO <new_line>.
          INSERT <new_line> INTO TABLE <new_table>.
        ENDDO.
    ENDTRY.

  ENDMETHOD.

  METHOD transfer_variable.

    DATA: lr_struc      TYPE REF TO data,
          o_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone   TYPE REF TO data,
          name          TYPE string,
          full_name     TYPE string,
          o_deep_handle TYPE REF TO cl_abap_datadescr,
          deep_ref      TYPE REF TO cl_abap_typedescr,
          o_tabl        TYPE REF TO cl_abap_tabledescr,
          o_struc       TYPE REF TO cl_abap_structdescr,
          r_header      TYPE REF TO data,
          r_elem        TYPE REF TO data.

    DATA: len TYPE i.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    full_name = i_name.

    IF i_name NE '{A:initial}'.
      TRY.
          CALL METHOD cl_tpda_script_data_descr=>get_quick_info
            EXPORTING
              p_var_name   = i_name
            RECEIVING
              p_symb_quick = m_quick.
        CATCH cx_tpda_varname .

          mo_tree_local->del_variable( EXPORTING i_full_name = i_name i_state = 'X' ).
          RETURN.
      ENDTRY.
    ELSE.
      m_quick-typid = 'g'.
    ENDIF.

    IF i_shortname IS NOT INITIAL.
      name = i_shortname.
    ELSE.
      name = i_name.
    ENDIF.

    TRY.
        IF i_name NE '{A:initial}'.
          ASSIGN m_quick-quickdata->* TO <lv_value>.
        ENDIF.

        IF m_quick-typid = 'h'."internal table
          READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO DATA(source).
          READ TABLE source-tt_tabs WITH KEY name = i_name INTO DATA(var).


          o_table_descr ?= cl_tpda_script_data_descr=>factory( i_name ).

*          DATA(comp_tpda) = o_table_descr->components( ).
*
*          DATA: comp TYPE abap_component_tab,
*                comp_descr TYPE abap_componentdescr.
*
*          LOOP AT comp_tpda INTO DATA(comp_descr_tpda).
*            REPLACE ALL OCCURRENCES OF '\TYPE-POOL=ABAP\TYPE=' IN comp_descr_tpda-abstypename WITH ''.
*            REPLACE ALL OCCURRENCES OF '\TYPE=' IN comp_descr_tpda-abstypename WITH ''.
*            REPLACE ALL OCCURRENCES OF '\TYPE-POOL=' IN comp_descr_tpda-abstypename WITH ''.
*
*            IF comp_descr_tpda-abstypename+0(3) = '%_T' OR
*              comp_descr_tpda-abstypename+0(11) = '\INTERFACE=' OR
*              comp_descr_tpda-abstypename+0(7) = '\CLASS='.
*              DATA(old_generation) = abap_true.
*              EXIT.
*            ENDIF.
*
*            DATA(o_descr) = cl_abap_typedescr=>describe_by_name( comp_descr_tpda-abstypename ).
*            IF o_descr IS INSTANCE OF cl_abap_elemdescr.
*              DATA(o_elem) = CAST cl_abap_elemdescr( o_descr ).
*              CLEAR comp_descr.
*              comp_descr-name = comp_descr_tpda-compname.
*              comp_descr-type = o_elem.
*              APPEND comp_descr TO comp.
*            ENDIF.
*          ENDLOOP.
*
*          old_generation = abap_true.
*          IF old_generation IS INITIAL.
*            "--- создаём структуру на основе component_tab
*            DATA(o_struct) = cl_abap_structdescr=>create( comp ).
*
*            "--- создаём таблицу этой структуры
*            DATA(o_table)  = cl_abap_tabledescr=>create( o_struct ).
*
*            "--- создаём реальный объект таблицы
*            DATA lr_table TYPE REF TO data.
*            CREATE DATA lr_table TYPE HANDLE o_table.
*
*            ASSIGN lr_table->* TO FIELD-SYMBOL(<lt_dyn>).
*          ENDIF.

          table_clone = o_table_descr->elem_clone( ).


          "ASSIGN table_clone->* TO FIELD-SYMBOL(<f>).

*          IF old_generation IS INITIAL.
*            MOVE-CORRESPONDING <f> TO <lt_dyn>.
*          ELSE.
          ASSIGN table_clone->* TO FIELD-SYMBOL(<lt_dyn>).
*          ENDIF.

          "check header area
          DATA td       TYPE sydes_desc.
          DESCRIBE FIELD <lt_dyn> INTO td.

          READ TABLE td-names INTO DATA(names) INDEX 1.
          IF sy-subrc = 0.
            TRY.
                CALL METHOD cl_tpda_script_data_descr=>get_quick_info
                  EXPORTING
                    p_var_name   = |{ i_name }-{ names-name }|
                  RECEIVING
                    p_symb_quick = DATA(quick).

                o_tabl ?= cl_abap_typedescr=>describe_by_data( <lt_dyn> ).

                o_struc ?= o_tabl->get_table_line_type( ).
                CREATE DATA r_header TYPE HANDLE o_struc.
                ASSIGN r_header->* TO FIELD-SYMBOL(<header>).

                traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( r_header )
                          i_name        = name
                          i_fullname    = i_name
                          i_type        = i_type
                          i_parent_calculated = i_parent_calculated
                          i_instance     = i_instance
                          i_cl_leaf      = i_cl_leaf
                          ir_up          = r_header ).

                name = name && '[]'.
                full_name = i_name && '[]'.
              CATCH cx_tpda_varname .
            ENDTRY.
          ENDIF.
          GET REFERENCE OF <lt_dyn> INTO lr_struc.

          traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                    i_name        = name
                    i_fullname    = full_name
                    i_type        = i_type
                    i_instance     = i_instance
                    i_parent_calculated = i_parent_calculated
                    i_cl_leaf      = i_cl_leaf
                    ir_up          = lr_struc ).

        ELSEIF m_quick-typid = 'l'. "data ref

          DATA: info TYPE tpda_scr_quick_info.

          FIELD-SYMBOLS: <symbdatref> TYPE tpda_sys_symbdatref.
          info = cl_tpda_script_data_descr=>get_quick_info( i_name ).
          ASSIGN info-quickdata->* TO <symbdatref>.

          " Check if the referenced object exists
          IF <symbdatref>-instancename IS NOT INITIAL AND
             <symbdatref>-instancename <> '{R:initial}' AND
             <symbdatref>-instancename <> '{A:initial}'.

            TRY." Try to get info about the referenced object
                DATA(ref_info) = cl_tpda_script_data_descr=>get_quick_info( <symbdatref>-instancename  ).

                " Handle string references specially
                IF ref_info-typid = 'g'. "string
                  " Create a string variable directly
                  APPEND INITIAL LINE TO mt_new_string ASSIGNING FIELD-SYMBOL(<m_string_ref>).
                  <m_string_ref> = create_simple_string( <symbdatref>-instancename ).
                  GET REFERENCE OF <m_string_ref> INTO m_variable.

                  traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( m_variable )
                            i_name        = name
                            i_type        = i_type
                            i_fullname    = i_name
                            i_parent_calculated = i_parent_calculated
                            i_instance     = i_instance
                            i_cl_leaf      = i_cl_leaf
                            ir_up          = m_variable ).
                ELSE.
                  " Handle other reference types as before
                  transfer_variable( EXPORTING i_name =  <symbdatref>-instancename
                                               i_type = i_type
                                               i_shortname = i_name
                                               i_parent_calculated = i_parent_calculated
                                               i_cl_leaf = i_cl_leaf
                                               i_instance = <symbdatref>-instancename ).
                ENDIF.
              CATCH cx_tpda_varname.
                " Handle error - show as unresolved reference
                APPEND INITIAL LINE TO mt_new_string ASSIGNING <m_string_ref>.
                <m_string_ref> = |Unresolved reference: { <symbdatref>-instancename }|.
                GET REFERENCE OF <m_string_ref> INTO m_variable.

                traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( m_variable )
                          i_name        = name
                          i_type        = i_type
                          i_fullname    = i_name
                          i_parent_calculated = i_parent_calculated
                          i_instance     = i_instance
                          i_cl_leaf      = i_cl_leaf
                          ir_up          = m_variable ).
            ENDTRY.
          ENDIF.

        ELSEIF m_quick-typid = 'r'. "reference
          FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
          ASSIGN m_quick-quickdata->* TO <symobjref>.

          save_hist( EXPORTING i_fullname    = i_name
                               i_name        = i_shortname
                               i_parent_calculated = i_parent_calculated
                               i_type        = i_type
                               i_cl_leaf     = i_cl_leaf
                               i_instance     = <symobjref>-instancename ).

          create_reference( EXPORTING i_name      = name
                                      i_type      = i_type
                                      i_shortname = name
                                      i_parent    = i_parent_calculated
                                      i_quick     = m_quick ).

        ELSEIF m_quick-typid = 'v' OR m_quick-typid = 'u'."deep structure or structure

          CALL METHOD cl_abap_complexdescr=>describe_by_name
            EXPORTING
              p_name         = m_quick-abstypename
            RECEIVING
              p_descr_ref    = deep_ref
            EXCEPTIONS
              type_not_found = 1.

          IF sy-subrc = 0.
            o_deep_handle ?= deep_ref.
            CREATE DATA lr_struc TYPE HANDLE o_deep_handle.
            get_deep_struc( EXPORTING i_name = i_name r_obj = lr_struc ).
            ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_deep>).

            traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                      i_name        = name
                      i_fullname    = i_name
                      i_type        = i_type
                      i_parent_calculated = i_parent_calculated
                      i_instance     = i_instance
                      i_cl_leaf      = i_cl_leaf
                      ir_up          = lr_struc ).
          ELSE.
            create_struc2( EXPORTING i_name = i_name i_shortname = name ).
          ENDIF.

        ELSEIF m_quick-typid = 'g'."string
          APPEND INITIAL LINE TO mt_new_string ASSIGNING FIELD-SYMBOL(<m_string>).
          IF i_name NE '{A:initial}'.
            <m_string> = create_simple_string( i_name ).
          ELSE.
            <m_string> = '{A:initial}'.
          ENDIF.
          GET REFERENCE OF <m_string> INTO m_variable.
          traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( m_variable )
                    i_name        = name
                    i_type        = i_type
                    i_fullname    = i_name
                    i_parent_calculated = i_parent_calculated
                    i_instance     = i_instance
                    i_cl_leaf      = i_cl_leaf
                    ir_up          = m_variable ).
        ELSE.
          lr_struc = create_simple_var( i_name ).
          ASSIGN lr_struc->* TO FIELD-SYMBOL(<new_elem>).

          traverse( io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( lr_struc )
                    i_name        = name
                    i_fullname    = i_name
                    i_type        = i_type
                    i_parent_calculated = i_parent_calculated
                    ir_up          = lr_struc
                    i_cl_leaf      = i_cl_leaf
                    i_instance     = i_instance ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_class_name.

    DATA: o_object TYPE REF TO cl_tpda_script_objectdescr,
          o_descr  TYPE REF TO cl_tpda_script_data_descr.

    FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.

    TRY.
        CALL METHOD cl_tpda_script_data_descr=>get_quick_info
          EXPORTING
            p_var_name   = i_name
          RECEIVING
            p_symb_quick = DATA(quick).

        ASSIGN quick-quickdata->* TO <symobjref>.
        IF <symobjref>-instancename <> '{O:initial}'.

          o_descr = cl_tpda_script_data_descr=>factory( <symobjref>-instancename ).
          o_object ?= o_descr.

          e_name = o_object->classname( ).
        ENDIF.
      CATCH cx_tpda_varname .
    ENDTRY.

  ENDMETHOD.

  METHOD create_reference.

    DATA: obj        LIKE LINE OF mt_obj,
          lr_struc   TYPE REF TO data,
          o_object   TYPE REF TO cl_tpda_script_objectdescr,
          o_descr    TYPE REF TO cl_tpda_script_data_descr,
          attributes TYPE tpda_script_object_attribut_it.

    FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
    ASSIGN i_quick-quickdata->* TO <symobjref>.
    IF <symobjref>-instancename <> '{O:initial}'.

      obj-name = i_name.
      obj-obj = <symobjref>-instancename.
      COLLECT obj INTO mt_obj.

      TRY.
          o_descr = cl_tpda_script_data_descr=>factory( <symobjref>-instancename ).
          o_object ?= o_descr.

          attributes = o_object->attributes( ).
          DELETE attributes WHERE instantiation = 1.
          SORT attributes BY acckind name.

          DATA(name) = o_object->classname( ).
          DATA(obj_ind) =  get_obj_index( <symobjref>-instancename ).

          READ TABLE mt_classes_types WITH KEY full = obj_ind TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            LOOP AT attributes ASSIGNING FIELD-SYMBOL(<attribute>).
              AT NEW acckind.
                APPEND INITIAL LINE TO mt_classes_types ASSIGNING FIELD-SYMBOL(<cl_type>).
                <cl_type>-name = i_name.
                <cl_type>-full = obj_ind.
                <cl_type>-type = <attribute>-acckind.
              ENDAT.
            ENDLOOP.
          ENDIF.

          DATA: parent TYPE string.
          IF i_parent IS NOT INITIAL.
            parent = |{ i_parent }-{ i_name }|.
          ELSE.
            parent = i_name.
          ENDIF.

          LOOP AT attributes ASSIGNING <attribute>.

            transfer_variable( EXPORTING i_name        = |{ <symobjref>-instancename  }-{ <attribute>-name }|
                                         i_shortname   = <attribute>-name
                                         i_type       = i_type
                                         i_instance    = <symobjref>-instancename
                                         i_cl_leaf     = <attribute>-acckind
                                         i_parent_calculated = parent ).

            READ TABLE mt_state WITH KEY path = |{ parent  }-{ <attribute>-name }| ASSIGNING FIELD-SYMBOL(<state>).
            IF sy-subrc = 0.
              <state>-cl_leaf = <attribute>-acckind.
            ENDIF.
          ENDLOOP.
        CATCH cx_tpda_varname.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD create_struc2.

    DATA: o_struc_descr TYPE REF TO cl_tpda_script_structdescr,
          components    TYPE abap_component_tab,
          comp_full     TYPE  tpda_scr_struct_comp_it,
          comp_descr    TYPE abap_componentdescr,
          comp_it       TYPE tpda_script_struc_componentsit,
          structdescr   TYPE REF TO cl_abap_structdescr,
          r_data        TYPE REF TO data.

    FIELD-SYMBOLS: <str> TYPE any.

    o_struc_descr ?= cl_tpda_script_data_descr=>factory( i_name ).
    o_struc_descr->components( IMPORTING p_components_it = comp_it p_components_full_it = comp_full ).

    LOOP AT comp_it INTO DATA(comp).
      comp_descr-name = comp-compname.
      IF comp-typid = 'u'.
        r_data = create_struc( EXPORTING i_name = |{ comp-longname }| ).
      ELSE.
        r_data = create_simple_var( EXPORTING i_name = |{ comp-longname }| ).
      ENDIF.
      ASSIGN r_data->* TO FIELD-SYMBOL(<item>).

      CALL METHOD cl_abap_complexdescr=>describe_by_data
        EXPORTING
          p_data      = <item>
        RECEIVING
          p_descr_ref = DATA(o_type).

      comp_descr-type ?= o_type.
      APPEND comp_descr TO components.
    ENDLOOP.

    structdescr = cl_abap_structdescr=>create( components ).
    CREATE DATA r_data TYPE HANDLE structdescr.
    ASSIGN r_data->* TO <str>.

    get_deep_struc( EXPORTING i_name = i_name r_obj = r_data ).
    ASSIGN r_data->* TO FIELD-SYMBOL(<new_deep>).

  ENDMETHOD.

  METHOD script.

    run_script( ).
    show_step( ).
    me->break( ).

  ENDMETHOD.

  METHOD run_script_hist.

    DATA: vars_history LIKE mt_vars_hist_view,
          hist_step    TYPE i,
          old_step     TYPE i.

    CLEAR mv_recurse.
    is_history = abap_true.
    IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
    IF i_step IS NOT INITIAL.
      hist_step = i_step.
      READ TABLE mt_steps WITH KEY step = i_step INTO DATA(steps).

    ELSE.
      IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack IS INITIAL.
        READ TABLE mt_steps INTO steps INDEX m_hist_step.
        m_target_stack = steps-stacklevel.
      ENDIF.

      IF mo_window->m_direction IS NOT INITIAL AND m_hist_step = 1 AND mo_window->m_debug_button IS NOT INITIAL.
        es_stop = abap_true.
      ENDIF.

      IF mo_window->m_direction IS INITIAL AND m_hist_step = m_step AND mo_window->m_debug_button IS NOT INITIAL.
        es_stop = abap_true.
      ENDIF.

      old_step = m_hist_step.
      IF mo_window->m_direction IS NOT INITIAL AND m_hist_step > 1 AND mo_window->m_debug_button IS NOT INITIAL.
        SUBTRACT 1 FROM m_hist_step.
      ENDIF.

      IF mo_window->m_direction IS INITIAL AND m_hist_step < m_step AND mo_window->m_debug_button IS NOT INITIAL.
        ADD 1  TO m_hist_step.
      ENDIF.

      hist_step = m_hist_step.

      READ TABLE mt_steps INTO steps WITH KEY step =  m_hist_step.
      READ TABLE mt_steps INTO DATA(step_old) WITH KEY step =  old_step.

      IF steps-stacklevel <> step_old-stacklevel.
        m_refresh = abap_true.
      ENDIF.

      mo_window->set_program( steps-include ).
      mo_window->set_program_line( steps-line ).

      IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack =  steps-stacklevel.
        CLEAR m_target_stack.
        es_stop = abap_true.
      ENDIF.

      READ TABLE mo_window->mt_stack INTO DATA(stack) INDEX 1.

      MOVE-CORRESPONDING stack TO mo_window->m_prg.

      IF mo_window->m_debug_button = 'F6' AND m_stop_stack IS INITIAL.
        m_stop_stack = stack-stacklevel.
      ENDIF.

    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      es_stop = abap_true.
    ENDIF.

    IF mo_window->m_debug_button = 'F6' AND m_stop_stack = stack-stacklevel.

      es_stop = abap_true.
      CLEAR m_stop_stack.
    ENDIF.

    IF ( mo_window->m_debug_button = 'F6BEG' AND steps-first = abap_true AND m_target_stack = stack-stacklevel ) OR
       ( mo_window->m_debug_button = 'F6END' AND steps-last = abap_true  AND m_target_stack = stack-stacklevel ).
      CLEAR m_target_stack.
      es_stop = abap_true.
    ENDIF.

    IF i_step IS INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = steps-include linesrc = steps-line INTO DATA(break).
      IF sy-subrc = 0.

        es_stop = abap_true.
      ENDIF.
    ENDIF.

    IF  i_step IS NOT INITIAL.
      es_stop = abap_true.
    ENDIF.

    IF es_stop = abap_true.

      "history state find refactoring
      DATA(vars_hist) = mt_vars_hist.
      SORT vars_hist BY step ASCENDING first DESCENDING.

      CLEAR vars_history.

      LOOP AT mt_steps INTO DATA(hist_steps) WHERE step <= hist_step.
        IF hist_steps-stacklevel < steps-stacklevel.
          DELETE vars_history WHERE leaf = 'LOCAL'.
        ENDIF.
        LOOP AT vars_hist INTO DATA(hist) WHERE step = hist_steps-step.
          IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
          IF  mo_tree_local->m_globals IS INITIAL AND hist-leaf = 'GLOBAL' OR hist-program <> steps-program.
            CONTINUE.
          ENDIF.
          IF  mo_tree_local->m_class_data IS INITIAL AND hist-leaf = 'CLASS'.
            CONTINUE.
          ENDIF.
          IF ( hist-leaf = 'LOCAL' OR hist-leaf = 'IMP' OR hist-leaf = 'EXP' ) AND hist-stack <> steps-stacklevel.
            CONTINUE.
          ENDIF.

          IF hist-step = hist_step AND hist-first IS INITIAL.
            CONTINUE.
          ENDIF.

          IF hist-del IS INITIAL.
            READ TABLE vars_history WITH KEY name = hist-name ASSIGNING FIELD-SYMBOL(<hist>).
            IF sy-subrc = 0.
              <hist> = hist.
              CLEAR <hist>-done.
            ELSE.
              "check initial.
              IF mo_tree_local->m_hide IS NOT INITIAL.
                ASSIGN hist-ref->* TO FIELD-SYMBOL(<new>).
                IF <new> IS NOT INITIAL.
                  APPEND INITIAL LINE TO vars_history ASSIGNING <hist>.
                  <hist> = hist.
                  CLEAR <hist>-done.
                ENDIF.
              ELSE.
                APPEND INITIAL LINE TO vars_history ASSIGNING <hist>.
                <hist> = hist.
                CLEAR <hist>-done.
              ENDIF.
            ENDIF.
          ELSE.
            IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
            mo_tree_local->clear( ).
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      IF m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

      SORT vars_history BY name.

      IF step_old-stacklevel <> steps-stacklevel OR m_refresh = abap_true.
        mo_tree_local->clear( ).
        mo_tree_exp->clear( ).
        mo_tree_imp->clear( ).
      ENDIF.


      IF vars_history IS NOT INITIAL.
        show_variables( CHANGING it_var = vars_history ).

        set_selected_vars( ).
        CLEAR m_refresh.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD run_script.

    DATA: type TYPE string.
    ADD 1 TO m_counter.
    TRY.
        cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info = mo_window->m_prg ).
        DATA(stack) = cl_tpda_script_abapdescr=>get_abap_stack( ).
        READ TABLE mo_window->mt_stack INDEX 1 INTO ms_stack_prev.

        MOVE-CORRESPONDING stack TO mo_window->mt_stack.
        READ TABLE mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack>).
        <stack>-step = m_step.
        ms_stack = <stack>.

        CALL METHOD cl_tpda_script_bp_services=>get_all_bps RECEIVING p_bps_it = mo_window->mt_breaks.

        IF is_step = abap_true.
          ADD 1 TO m_step.
          m_hist_step = m_step.
          GET TIME.
          "add missed ELSE/ENDIF/ENDCASE
          IF m_step > 2.
            READ TABLE mt_steps INDEX m_step - 1 INTO DATA(one_step).

            IF ms_stack-line > one_step-line.
              READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO DATA(source).
              READ TABLE source-t_keytokens WITH KEY line = ms_stack-line INTO DATA(key).
              READ TABLE source-t_keytokens INDEX sy-tabix - 1 INTO key.
              READ TABLE source-t_keytokens WITH KEY line = one_step-line INTO DATA(key_prev).

              IF key_prev-name <> 'DO' AND key_prev-name <> 'LOOP' AND key_prev-name <> 'WHILE'.

                IF key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ENDCASE'.

                  APPEND INITIAL LINE TO mt_steps ASSIGNING FIELD-SYMBOL(<step>).
                  MOVE-CORRESPONDING ms_stack TO <step>.
                  <step>-line = key-line.
                  <step>-step = m_step.
                  ADD 1 TO m_step.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          APPEND INITIAL LINE TO mt_steps ASSIGNING <step>.
          MOVE-CORRESPONDING ms_stack TO <step>.
          <step>-time = sy-uzeit.
          <step>-step = m_step.
          CLEAR is_step.
          IF mv_stack_changed = abap_true AND  ms_stack_prev-stacklevel < ms_stack-stacklevel.
            <step>-first = abap_true.
          ENDIF.
          IF mo_window->m_prg-flag_eoev = abap_true.
            <step>-last = abap_true.
          ENDIF.
        ENDIF.

        IF mo_window->m_prg-program NE mo_tree_local->m_prg_info-program OR
          mo_window->m_prg-event-eventname NE mo_tree_local->m_prg_info-event-eventname OR
          mo_window->m_prg-event-eventtype NE mo_tree_local->m_prg_info-event-eventtype.

          CLEAR: m_step_delta,
                 mt_ret_exp,
                 mt_obj,
                 mt_ret_exp.

          mv_stack_changed = abap_true.

          DATA: step TYPE i.
          step = m_step - 1.
          IF mo_window->m_varhist IS NOT INITIAL.
            mo_tree_local->clear( ).
            mo_tree_exp->clear( ).
            mo_tree_imp->clear( ).
            IF ms_stack_prev-program <> ms_stack-program.
              CLEAR mt_state.
            ELSE.
              DELETE mt_state WHERE leaf NE 'GLOBAL'. "AND leaf NE 'SYST'.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR mv_stack_changed.
          m_step_delta = 1.
        ENDIF.
      CATCH cx_tpda_src_info.
    ENDTRY.

    mo_tree_local->m_prg_info = mo_window->m_prg.

    IF mo_window->m_version IS INITIAL.
      DATA: optimize TYPE xfeld.
      READ TABLE mo_window->mt_source WITH KEY include = ms_stack_prev-include INTO source.
      IF sy-subrc = 0.
        READ TABLE source-t_keytokens WITH KEY line = ms_stack_prev-line INTO DATA(oper).
        IF mv_stack_changed IS INITIAL.
          IF oper-name = 'COMPUTE' OR oper-name = 'SELECT' OR oper-name = 'CLEAR' OR  oper-name = 'LOOP' OR oper-name = 'SORT'
             OR oper-name = 'DELETE' OR oper-name = 'READ' OR  oper-name = 'CONCATENATE' OR oper-name = 'CONDENSE'
             OR oper-name = 'APPEND' OR oper-name = 'MODIFY' OR  oper-name = 'CREATE' OR oper-name = 'SHIFT'
             OR oper-name = 'ASSIGN' OR oper-name = 'UNASSIGN' OR oper-name = 'TRANSLATE' OR  oper-name = 'REPLACE'
             OR  oper-name = 'ADD' OR  oper-name = 'SUBTRACT'.
            optimize = abap_true.

            IF oper-name = 'UNASSIGN'.
              mo_tree_local->clear( ).
              mo_tree_exp->clear( ).
              mo_tree_imp->clear( ).
              DELETE mt_state WHERE leaf NE 'GLOBAL'.
            ENDIF.

          ENDIF.
        ENDIF.
      ELSE.
        lcl_source_parser=>parse_tokens( i_program = mo_window->m_prg-include io_debugger = me ).
        READ TABLE mo_window->mt_source WITH KEY include = ms_stack-include INTO source.
      ENDIF.
    ENDIF.

    IF mo_window->m_varhist IS NOT INITIAL.
      IF mo_tree_local->m_globals IS NOT INITIAL AND mo_tree_local->m_ldb IS NOT INITIAL.
        DATA: name(40),
              inc       TYPE TABLE OF  d010inc.
        name = abap_source->program( ).

        CALL FUNCTION 'RS_PROGRAM_INDEX'
          EXPORTING
            pg_name      = name
          TABLES
            compo        = mt_compo
            inc          = inc
          EXCEPTIONS
            syntax_error = 1
            OTHERS       = 2.
      ENDIF.

      IF mv_stack_changed = abap_true.
        IF mo_tree_local->m_locals IS NOT INITIAL.
          READ TABLE mo_window->mt_locals_set WITH KEY program = ms_stack-program
                                                       eventname = ms_stack-eventname
                                                       eventtype = ms_stack-eventtype
             INTO DATA(local_set).

          IF sy-subrc = 0 AND local_set-loc_fill = abap_true.
            mt_locals = local_set-locals_tab.
          ELSE.
            CALL METHOD cl_tpda_script_data_descr=>locals RECEIVING p_locals_it = mt_locals.

            IF ms_stack-eventtype = 'METHOD'.
              APPEND INITIAL LINE TO mt_locals ASSIGNING FIELD-SYMBOL(<loc>).
              <loc>-name = 'ME'.
            ENDIF.
            IF ms_stack-eventtype = 'FUNCTION'.
              DATA: fname              TYPE rs38l_fnam,
                    exception_list     TYPE TABLE OF  rsexc,
                    export_parameter   TYPE TABLE OF  rsexp,
                    import_parameter   TYPE TABLE OF  rsimp,
                    changing_parameter TYPE TABLE OF    rscha,
                    tables_parameter   TYPE TABLE OF    rstbl.

              fname = ms_stack-eventname.
              CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
                EXPORTING
                  funcname           = fname
                TABLES
                  exception_list     = exception_list
                  export_parameter   = export_parameter
                  import_parameter   = import_parameter
                  changing_parameter = changing_parameter
                  tables_parameter   = tables_parameter
                EXCEPTIONS
                  error_message      = 1
                  function_not_found = 2
                  invalid_name       = 3
                  OTHERS             = 4.
              IF sy-subrc = 0.
                LOOP AT export_parameter INTO DATA(exp).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = exp-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
                LOOP AT import_parameter INTO DATA(imp).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = imp-parameter.
                  <loc>-parkind = 1.
                ENDLOOP.
                LOOP AT changing_parameter INTO DATA(change).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = change-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
                LOOP AT tables_parameter INTO DATA(table).
                  APPEND INITIAL LINE TO mt_locals ASSIGNING <loc>.
                  <loc>-name = table-parameter.
                  <loc>-parkind = 2.
                ENDLOOP.
              ENDIF.
            ENDIF.

            IF mo_window->m_prg-event-eventtype = 'FORM'.
              LOOP AT source-t_params INTO DATA(params) WHERE name = mo_window->m_prg-event-eventname AND class IS INITIAL.
                READ TABLE mt_locals WITH KEY name = params-param ASSIGNING FIELD-SYMBOL(<local>).
                IF sy-subrc = 0.
                  IF params-type = 'I'.
                    <local>-parkind = '1'.
                  ELSEIF params-type = 'E'.
                    <local>-parkind = '2'.
                  ENDIF.
                ENDIF.
              ENDLOOP.
              "get_form_parameters( i_prg = mo_window->m_prg i_form = ms_stack-eventname ).
            ENDIF.

            SORT mt_locals.

            local_set-program = ms_stack-program.
            local_set-eventname = ms_stack-eventname.
            local_set-eventtype = ms_stack-eventtype.
            local_set-loc_fill = abap_true.
            local_set-locals_tab = mt_locals.
            APPEND local_set TO mo_window->mt_locals_set.
          ENDIF.
        ENDIF.

        IF ( mo_tree_local->m_globals IS NOT INITIAL OR  mo_tree_local->m_ldb IS NOT INITIAL ) AND ms_stack_prev-program <> ms_stack-program.

          READ TABLE mo_window->mt_globals_set WITH KEY program = ms_stack-program INTO DATA(global_set).

          IF sy-subrc = 0 AND global_set-glob_fill = abap_true.
            mt_globals = global_set-globals_tab.
          ELSE.

            CALL METHOD cl_tpda_script_data_descr=>globals RECEIVING p_globals_it = mt_globals.
            SORT mt_globals.
            IF mo_tree_local->m_globals IS NOT INITIAL AND  mo_tree_local->m_ldb IS NOT INITIAL.
              LOOP AT mt_globals ASSIGNING FIELD-SYMBOL(<global>).
                READ TABLE mt_compo WITH KEY name = <global>-name TRANSPORTING NO FIELDS.
                IF sy-subrc NE 0.
                  <global>-parisval = 'L'.
                ENDIF.
              ENDLOOP.
            ENDIF.
            global_set-program = ms_stack-program.
            global_set-globals_tab = mt_globals.
            global_set-glob_fill = abap_true.
            APPEND global_set TO mo_window->mt_globals_set.
          ENDIF.
        ENDIF.

        IF mo_tree_local->m_class_data IS NOT INITIAL.
          read_class_globals( ).
        ENDIF.

      ENDIF.

      DATA: lr_names TYPE RANGE OF string,
            temp     TYPE char30.

      DATA(globals) = mt_globals.
      DATA(locals) = mt_locals.

      IF optimize = abap_true AND m_update IS INITIAL.

        LOOP AT source-t_calculated INTO DATA(param) WHERE line = ms_stack_prev-line.
          temp = param-name.
          lr_names = VALUE #( BASE lr_names ( sign = 'I' option = 'EQ' low = temp ) ).
        ENDLOOP.

        DELETE lr_names WHERE low = 'CORRESPONDING'. "to refactor
        IF sy-subrc = 0.
          DELETE globals WHERE name NOT IN lr_names.
          DELETE locals WHERE name NOT IN lr_names.
        ENDIF.

      ENDIF.

      IF mo_tree_local->m_locals IS NOT INITIAL.
        LOOP AT locals INTO DATA(local).

          CASE local-parkind.
            WHEN 0.
              type = 'LOCAL'.
            WHEN 1.
              type = 'IMP'.
            WHEN OTHERS.
              type = 'EXP'.
          ENDCASE.

          transfer_variable( EXPORTING i_name = local-name i_type = type ).
        ENDLOOP.

        READ TABLE mo_window->mt_locals_set
         WITH KEY program = ms_stack-program eventtype = ms_stack-eventtype eventname = ms_stack-eventname
         INTO DATA(locals_set).
        LOOP AT locals_set-mt_fs INTO DATA(fs).
          transfer_variable( EXPORTING i_name = fs-name i_type = 'LOCAL' ).
        ENDLOOP.
      ENDIF.

      IF mo_tree_local->m_globals IS NOT INITIAL.

        LOOP AT globals INTO DATA(global)  WHERE parisval NE 'L'.
          transfer_variable( EXPORTING i_name = global-name i_type = 'GLOBAL' ).
        ENDLOOP.
        READ TABLE mo_window->mt_globals_set WITH KEY program = ms_stack-program INTO DATA(globals_set).
        LOOP AT globals_set-mt_fs INTO fs.
          transfer_variable( EXPORTING i_name = fs-name i_type = 'GLOBAL' ).
        ENDLOOP.

      ENDIF.
    ENDIF.

    IF mo_tree_local->m_syst IS NOT INITIAL.
      transfer_variable( EXPORTING i_name = 'SYST' i_type = 'SYST' ).
    ELSE.
      DELETE mo_tree_local->mt_vars WHERE leaf = 'SYST'.
      DELETE mt_state WHERE leaf = 'SYST'.
    ENDIF.

    IF mo_tree_local->m_ldb IS NOT INITIAL.
      LOOP AT globals INTO global WHERE parisval = 'L'.
        transfer_variable( EXPORTING i_name = global-name i_type = 'LDB' ).
      ENDLOOP.
    ENDIF.

    LOOP AT mt_state ASSIGNING FIELD-SYMBOL(<state>).
      CLEAR <state>-done.
    ENDLOOP.

    "check dependents variables.
    IF mt_selected_var IS NOT INITIAL.
      READ TABLE source-t_keytokens WITH KEY line = ms_stack_prev-line INTO DATA(keyword).
      LOOP AT keyword-tt_calls INTO DATA(call) WHERE event = 'FORM' AND name =  ms_stack-eventname.
        READ TABLE mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO mt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
          <selected>-name = call-inner.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR: mo_window->m_show_step.
    mo_tree_imp->m_prg_info = mo_window->m_prg.

  ENDMETHOD.

  METHOD show_variables.

    FIELD-SYMBOLS: <hist> TYPE any,
                   <new>  TYPE any.

    DATA: rel     TYPE salv_de_node_relation,
          key     TYPE salv_de_node_key,
          o_tree  TYPE REF TO  lcl_rtti_tree,
          is_skip TYPE xfeld.
    ADD 1 TO mv_recurse.
    IF mo_tree_local->m_clear = abap_true.
      mo_tree_local->clear( ).
      CLEAR mo_tree_local->m_clear.
    ENDIF.

    READ TABLE it_var WITH KEY del = abap_true TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      mo_tree_local->clear( ).
    ENDIF.

    mo_tree_imp->m_leaf =  'IMP'.
    mo_tree_exp->m_leaf =  'EXP'.

    IF mo_tree_local->m_locals_key IS NOT INITIAL AND mo_tree_local->m_locals IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_locals_key ).
      CLEAR mo_tree_local->m_locals_key.

      DELETE mo_tree_local->mt_vars WHERE leaf = 'LOCAL'.
      DELETE mt_state WHERE leaf = 'LOCAL'.
    ENDIF.

    IF mo_tree_local->m_globals_key IS NOT INITIAL AND mo_tree_local->m_globals IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_globals_key ).
      CLEAR mo_tree_local->m_globals_key.
      DELETE mo_tree_local->mt_vars WHERE leaf = 'GLOBAL'. "OR leaf = 'SYST'.
      DELETE mt_state WHERE leaf = 'GLOBAL'. "OR leaf = 'SYST'.
    ENDIF.

    IF mo_tree_local->m_class_key IS NOT INITIAL AND mo_tree_local->m_class_data IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_class_key ).
      DELETE mo_tree_local->mt_vars WHERE leaf = 'CLASS'.
      DELETE mt_state WHERE leaf = 'CLASS'.
    ENDIF.

    IF mo_tree_local->m_syst IS INITIAL.
      READ TABLE mo_tree_local->mt_vars WITH KEY name = 'SYST' INTO DATA(var).
      IF sy-subrc = 0.
        mo_tree_local->delete_node( var-key ).
        DELETE mo_tree_local->mt_vars WHERE leaf =  'SYST'.
        DELETE mt_state WHERE leaf = 'SYST'.
      ENDIF.
    ENDIF.

    IF mo_tree_local->m_ldb_key IS NOT INITIAL AND mo_tree_local->m_ldb IS INITIAL.
      mo_tree_local->delete_node( mo_tree_local->m_ldb_key ).
      CLEAR mo_tree_local->m_ldb_key.
      DELETE mo_tree_local->mt_vars WHERE leaf = 'LDB'.
      DELETE mt_state WHERE leaf = 'LDB'.
    ENDIF.

    rel = if_salv_c_node_relation=>last_child.

    LOOP AT it_var ASSIGNING FIELD-SYMBOL(<var>) WHERE done = abap_false.

      CASE <var>-leaf.

        WHEN 'LOCAL'.
          IF mo_tree_local->m_locals IS NOT INITIAL.
            mo_tree_local->m_leaf =  'LOCAL'.
            IF mo_tree_local->m_locals_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'Locals' i_icon = CONV #( icon_life_events ) ).
            ELSE.
              mo_tree_local->main_node_key = mo_tree_local->m_locals_key.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.

        WHEN 'GLOBAL'.
          IF mo_tree_local->m_globals IS NOT INITIAL.
            mo_tree_local->m_leaf =  'GLOBAL'.
            IF mo_tree_local->m_globals_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'Globals' i_icon = CONV #( icon_life_events ) ).
            ELSE.
              mo_tree_local->main_node_key = mo_tree_local->m_globals_key.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.

        WHEN 'LDB'.
          IF mo_tree_local->m_ldb IS NOT INITIAL.
            mo_tree_local->m_leaf =  'LDB'.
            IF mo_tree_local->m_ldb_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'LDB' i_icon = CONV #( icon_life_events ) ).
            ELSE.
              mo_tree_local->main_node_key = mo_tree_local->m_ldb_key.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.

        WHEN 'SYST'.
          IF mo_tree_local->m_syst IS NOT INITIAL.
            mo_tree_local->m_leaf =  'SYST'.
            IF mo_tree_local->m_syst_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'System variables' i_icon = CONV #( icon_life_events ) ).
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
        WHEN 'CLASS'.
          IF mo_tree_local->m_class_data IS NOT INITIAL.
            mo_tree_local->m_leaf =  'CLASS'.
            IF mo_tree_local->m_class_key IS INITIAL.
              mo_tree_local->add_node( i_name = 'Class-data global variables' i_icon = CONV #( icon_life_events ) ).
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
      ENDCASE.

      READ TABLE mt_selected_var WITH KEY name = <var>-name ASSIGNING FIELD-SYMBOL(<sel>).
      IF sy-subrc = 0.

        IF <sel>-refval IS BOUND.
          ASSIGN <sel>-refval->* TO <hist>.
          ASSIGN <var>-ref->* TO <new>.

          IF <new> <> <hist>.
            <sel>-refval = <var>-ref.
            stop = abap_true.
          ENDIF.
        ELSE.
          <sel>-refval = <var>-ref.
        ENDIF.
      ENDIF.

      CASE <var>-leaf.
        WHEN 'IMP'.
          o_tree = mo_tree_imp.
        WHEN 'EXP'.
          o_tree = mo_tree_exp.
        WHEN OTHERS.
          o_tree = mo_tree_local.
      ENDCASE.

      IF <var>-parent IS NOT INITIAL.
        READ TABLE o_tree->mt_vars WITH KEY path = <var>-parent TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <var>-done = abap_true.
        ELSE.
          IF o_tree->m_hide IS INITIAL.

            is_skip = abap_true.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.
        <var>-done = abap_true.
      ENDIF.

      READ TABLE o_tree->mt_vars WITH KEY path = <var>-parent INTO var.
      IF sy-subrc = 0.
        key = var-key.
      ELSE.
        key = o_tree->main_node_key.
      ENDIF.

      IF <var>-ref IS NOT INITIAL.
        o_tree->traverse(
          io_type_descr  = cl_abap_typedescr=>describe_by_data_ref( <var>-ref )
          i_parent_key  = key
          i_rel         = rel
          is_var         = <var>
          ir_up          = <var>-ref
          i_parent_calculated = CONV #( <var>-name ) ).
      ELSE.
        o_tree->traverse_obj(
          i_parent_key  = key
          i_rel         = rel
          is_var         = <var>
          ir_up          = <var>-ref
          i_parent_calculated = CONV #( <var>-name ) ).
      ENDIF.

    ENDLOOP.

    IF is_skip = abap_true.
      CLEAR is_skip.
      IF mv_recurse < 5.
        show_variables( CHANGING it_var = it_var ).
      ENDIF.
      set_selected_vars( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_selected_vars.

    DATA(nodes) = mo_tree_local->m_tree->get_nodes( )->get_all_nodes( ).
    LOOP AT nodes INTO DATA(node).
      DATA(name) = node-node->get_text( ).
      READ TABLE mt_selected_var WITH KEY name = name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        node-node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD hndl_script_buttons.

    IF m_is_find = abap_true.
      stop = abap_true.
      CLEAR m_is_find.
      RETURN.
    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      stop = abap_true.

    ELSEIF mo_window->m_debug_button = 'F6'.
      IF m_f6_level IS NOT INITIAL AND m_f6_level = ms_stack-stacklevel OR mo_window->m_history IS INITIAL.
        CLEAR m_f6_level.
        stop = abap_true.
      ENDIF.

    ELSEIF mo_window->m_debug_button = 'F6END'.
      IF mo_window->m_prg-flag_eoev IS NOT INITIAL AND m_target_stack = ms_stack-stacklevel.
        stop = abap_true.
      ENDIF.
    ELSEIF mo_window->m_debug_button = 'F7'.

      IF m_target_stack = ms_stack-stacklevel.
        CLEAR m_target_stack.
        stop = abap_true.
      ENDIF.

    ELSEIF mo_window->m_debug_button IS NOT INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = mo_window->m_prg-include linesrc = mo_window->m_prg-line INTO DATA(gs_break).
      IF sy-subrc = 0.
        stop = abap_true.
      ELSE.

        IF mo_window->m_debug_button = 'F6BEG' AND m_target_stack = ms_stack-stacklevel.
          stop = abap_true.
        ELSE.
          IF mo_window->m_history IS NOT INITIAL.
            IF ms_stack-stacklevel = mo_window->m_hist_depth +  mo_window->m_start_stack.
              "f6( )."to refactor
            ELSE.
              "f5( )."to refactor
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      stop = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD end. "dummy method
  ENDMETHOD.

  METHOD f5.

    READ TABLE mo_window->mt_stack INTO DATA(stack) INDEX 1.

    IF mo_window->m_debug_button NE 'F5' AND mo_window->m_zcode IS NOT INITIAL.
      IF stack-program+0(1) NE 'Z' AND stack-program+0(5) NE 'SAPLZ' AND m_f6_level <> stack-stacklevel.
        f7( ).
        RETURN.
      ENDIF.
    ENDIF.

*    IF ( mo_window->m_debug_button = 'F6BEG' OR mo_window->m_debug_button = 'F6END' ) AND m_target_stack IS INITIAL.
*      m_target_stack = stack-stacklevel.
*    ENDIF.

    IF mo_window->m_debug_button = 'F7' AND m_target_stack IS INITIAL.
      m_target_stack = stack-stacklevel - 1.
    ENDIF.

    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_into.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.
    "step out and not save history for standard code if it is swithed off
    IF  mo_window->m_zcode IS NOT INITIAL.

      cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info = mo_window->m_prg ).
      DO.
        IF   mo_window->m_prg-program+0(1) <> 'Z' AND mo_window->m_prg-program+0(5) <> 'SAPLZ' .
          f7( ).
        ELSE.
          EXIT.
        ENDIF.
        cl_tpda_script_abapdescr=>get_abap_src_info( IMPORTING p_prg_info = mo_window->m_prg ).
      ENDDO.
    ENDIF.

    IF mv_f7_stop = abap_true.
      CLEAR m_counter.
      stop = abap_true.
      m_is_find = abap_true.
    ENDIF.
    IF m_counter >= 50000."very deep history - to stop
      CLEAR m_counter.
      stop = abap_true.
    ENDIF.

    IF m_counter MOD 10000 = 0.
      show_step( ).
    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      stop = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD f6.

    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_over.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.

  ENDMETHOD.

  METHOD f7.

    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_step_out.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.

  ENDMETHOD.

  METHOD f8.

    TRY.
        CALL METHOD debugger_controller->debug_step
          EXPORTING
            p_command = cl_tpda_script_debugger_ctrl=>debug_continue.
        is_step = abap_true.
      CATCH cx_tpda_scr_rtctrl_status .
      CATCH cx_tpda_scr_rtctrl .
    ENDTRY.

  ENDMETHOD.

  METHOD make_step.

    DATA: stop TYPE xfeld.

    READ TABLE mo_window->mt_stack INDEX 1 INTO DATA(stack).
    IF mo_window->m_debug_button = 'F6' AND mo_window->m_history IS NOT INITIAL.
      m_f6_level = stack-stacklevel.
    ENDIF.

    WHILE stop IS INITIAL.

      CASE mo_window->m_debug_button.

        WHEN 'F5' OR 'F6END' OR 'F6BEG'.
          stop = f5( ).
        WHEN 'F6'.
          IF mo_window->m_history IS INITIAL.
            stop = f6( ).
          ELSE.
            stop = f5( ).
          ENDIF.

        WHEN 'F7'.
          IF mo_window->m_history IS INITIAL.
            stop = f7( ).
          ELSE.
            stop = f5( ).
          ENDIF.

        WHEN 'F8'.
          IF mo_window->m_history IS INITIAL.
            stop = f8( ).
          ELSE.

            IF stack-stacklevel = mo_window->m_start_stack + mo_window->m_hist_depth.
              stop = f6( ).
            ELSE.
              stop = f5( ).
            ENDIF.
          ENDIF.

      ENDCASE.
      run_script( ).
      stop = hndl_script_buttons( mv_stack_changed ).
      READ TABLE mo_window->mt_stack INDEX 1 INTO stack.

    ENDWHILE.
    show_step( ).
    me->break( ).

  ENDMETHOD.

  METHOD get_obj_index.

    FIND FIRST OCCURRENCE OF '*' IN i_name MATCH OFFSET DATA(offset).
    e_index =  i_name+0(offset).

  ENDMETHOD.

  METHOD show_step.

    show_variables( CHANGING it_var = mt_state ).
    set_selected_vars( ).
    mo_window->set_program( mo_window->m_prg-include ).
    mo_window->set_program_line( mo_window->m_prg-line ).
    mo_window->show_stack( ).
    mo_tree_imp->display( ).
    mo_tree_local->display( ).
    mo_tree_exp->display( ).
    IF mo_window->m_debug_button NE 'F5'.
      mo_window->m_show_step = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD read_class_globals.

    DATA: compo_tmp TYPE TABLE OF scompo,
          class     TYPE seu_name.

    mo_tree_local->m_leaf = 'Class-data global variables'.
    IF mo_tree_local->m_class_data IS NOT INITIAL.

      "global classes
      CALL METHOD cl_tpda_script_abapdescr=>get_loaded_programs
        IMPORTING
          p_progs_it = DATA(progs).

      DELETE progs WHERE sys = abap_true.

      LOOP AT progs INTO DATA(prog) WHERE name+30(2) = 'CP' AND ( name+0(1) = 'Z' OR name+0(1) = 'Y' ) .

        CLEAR prog-name+30(2).
        REPLACE ALL OCCURRENCES OF '=' IN prog-name WITH ''.

        DATA refc TYPE REF TO cl_abap_objectdescr.
        CALL METHOD cl_abap_classdescr=>describe_by_name
          EXPORTING
            p_name         = prog-name
          RECEIVING
            p_descr_ref    = DATA(ref)
          EXCEPTIONS
            type_not_found = 1
            OTHERS         = 2.

        refc ?= ref.

        READ TABLE mt_obj WITH KEY name = class TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
          <obj>-name = class.
        ENDIF.

        save_hist( EXPORTING i_fullname    = CONV #( prog-name )
                             i_name        = CONV #( prog-name )
                             i_parent_calculated = ''
                             i_type        = 'CLASS'
                             i_cl_leaf     = 0
                             i_instance     = CONV #( prog-name ) ).


        LOOP AT refc->attributes INTO DATA(atr).
          transfer_variable( EXPORTING i_name =  CONV #( |{ prog-name }=>{ atr-name }| )
                             i_shortname = CONV #( atr-name )
                             i_parent_calculated = CONV #( prog-name )
                              i_type = 'CLASS' ).
        ENDLOOP.
      ENDLOOP.

    ENDIF.

    compo_tmp = mt_compo.
    DELETE compo_tmp WHERE  type NE 'D'.

  ENDMETHOD.

  METHOD save_hist.

    DATA: add        TYPE xfeld,
          add_hist   TYPE xfeld,
          name2(100),
          full_name  TYPE string.

    CHECK m_hist_step = m_step AND mo_window->m_direction IS INITIAL.
    IF ir_up IS SUPPLIED.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<ir_up>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    IF i_instance IS INITIAL.
      full_name = i_fullname.
    ELSE.
      IF i_parent_calculated IS INITIAL.
        IF i_name IS NOT INITIAL.
          full_name = i_name.
        ELSE.
          full_name = i_fullname.
        ENDIF.
      ELSE.
        IF i_fullname+0(3) = '{O:'.
          full_name = i_fullname.
        ELSE.
          full_name =  |{ i_parent_calculated }-{ i_name }|.
        ENDIF.
      ENDIF.
    ENDIF.
    IF i_instance IS INITIAL.
      READ TABLE mt_state
           WITH KEY name = full_name
                    program = mo_window->mt_stack[ 1 ]-program
            ASSIGNING FIELD-SYMBOL(<state>).
    ELSE.
      READ TABLE mt_state
          WITH KEY name = full_name
                   program = mo_window->mt_stack[ 1 ]-program
                   instance = i_instance
           ASSIGNING <state>.
    ENDIF.

    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mt_state ASSIGNING <state>.
      <state>-stack = mo_window->mt_stack[ 1 ]-stacklevel.
      <state>-step  = m_step - m_step_delta.
      <state>-program   = mo_window->m_prg-program.
      <state>-eventtype = mo_window->m_prg-eventtype.
      <state>-eventname = mo_window->m_prg-eventname.
      <state>-name = full_name.

      IF i_name IS NOT INITIAL.
        <state>-short = i_name.
      ELSE.
        <state>-short = i_fullname.
      ENDIF.

      <state>-leaf = i_type.
      <state>-is_appear = abap_true.
      <state>-parent = i_parent_calculated.
      <state>-instance = i_instance.

      IF i_parent_calculated IS NOT INITIAL.
        IF i_name IS NOT INITIAL.
          <state>-path =  |{ i_parent_calculated }-{ i_name }|.
        ELSE.
          <state>-path =  |{ i_parent_calculated }-{ i_fullname }|.
        ENDIF.
      ELSE.
        IF i_instance IS INITIAL.
          <state>-path = i_fullname.
        ELSE.
          IF i_parent_calculated IS INITIAL.
            IF i_name IS NOT INITIAL.
              <state>-path = i_name.
            ELSE.
              <state>-path = i_fullname.
            ENDIF.
          ELSE.

            IF i_name IS NOT INITIAL.
              <state>-path =  |{ i_parent_calculated }-{ i_name }|.
            ELSE.
              <state>-path =  |{ i_parent_calculated }-{ i_fullname }|.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF m_hist_step > 1.
      <state>-step = m_hist_step - 1."m_step - m_step_delta.
    ELSE.
      <state>-step = m_hist_step.
    ENDIF.
    <state>-instance = i_instance.

    IF ir_up IS SUPPLIED.
      <state>-ref = ir_up.

      DATA(o_elem) = cl_abap_typedescr=>describe_by_data_ref( <state>-ref ).

      name2 = i_fullname.

      IF name2+0(2) NE '{O'.

        IF <state>-leaf NE 'GLOBAL' AND <state>-leaf NE 'CLASS'.
          READ TABLE mt_vars_hist_view
           WITH KEY stack = <state>-stack
                    name = i_fullname
                    eventtype = <state>-eventtype
                    eventname = <state>-eventname
                    INTO DATA(hist).
        ELSE.
          READ TABLE mt_vars_hist_view
           WITH KEY name = i_fullname
                    INTO hist.
        ENDIF.

        IF sy-subrc NE 0.
          add_hist = add = abap_on.
        ELSE.
          ASSIGN hist-ref->* TO FIELD-SYMBOL(<hist>).

          IF <hist> NE <ir_up>.
            add_hist = add = abap_on.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE mt_vars_hist_view WITH KEY name = <state>-name INTO hist.
        IF sy-subrc = 0.
          ASSIGN hist-ref->* TO <hist>.
          IF <hist> NE <ir_up>.
            add_hist = add = abap_on.
          ENDIF.
        ELSE.
          add_hist = add = abap_on.
        ENDIF.

      ENDIF.
      IF mv_stack_changed = abap_true.
        add = abap_on.
      ENDIF.

      o_elem = cl_abap_typedescr=>describe_by_data_ref( <state>-ref ).
      IF <state>-type IS INITIAL.
        <state>-type = o_elem->absolute_name.
      ENDIF.

      IF add = abap_on.

        CLEAR <state>-first.

        IF  ms_stack_prev-stacklevel IS INITIAL OR
         ms_stack-stacklevel > ms_stack_prev-stacklevel.
          <state>-first = 'X'.
        ENDIF.

        <state>-cl_leaf = i_cl_leaf.
        INSERT <state> INTO mt_vars_hist_view INDEX 1.

        IF  add_hist = abap_true.
          INSERT <state> INTO mt_vars_hist INDEX 1.
          READ TABLE mt_selected_var WITH KEY name = <state>-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            m_is_find = abap_true.
          ENDIF.
        ENDIF.

      ENDIF.
    ELSE. "main node without data
      IF m_hist_step > 1.
        <state>-step = m_hist_step - 1.
      ELSE.
        <state>-step = m_hist_step.
      ENDIF.

      READ TABLE mt_vars_hist WITH KEY name = <state>-name INTO DATA(var_hist).
      IF sy-subrc = 0.
        IF <state>-instance <> var_hist-instance.
          <state>-del = abap_true.
          INSERT <state> INTO mt_vars_hist INDEX 1.
        ENDIF.
      ELSE.
        <state>-first = 'X'.
        INSERT <state> INTO mt_vars_hist INDEX 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD traverse.

    "create new data
    DATA: lr_new   TYPE REF TO data,
          lr_struc TYPE REF TO data.

    FIELD-SYMBOLS: <new>      TYPE any,
                   <tab_from> TYPE ANY TABLE,
                   <tab_to>   TYPE STANDARD TABLE,
                   <ir_up>    TYPE any.
    ASSIGN ir_up->* TO <ir_up>.
    DESCRIBE FIELD ir_up TYPE DATA(type).
    IF type NE cl_abap_typedescr=>typekind_table.
      CREATE DATA lr_new LIKE <ir_up>.
      ASSIGN lr_new->*  TO <new>.
      ASSIGN ir_up->* TO <new>.
      GET REFERENCE OF <new> INTO lr_new.
    ELSE.
      ASSIGN ir_up->* TO <tab_from>.
      CREATE DATA lr_struc LIKE LINE OF <tab_from>.
      ASSIGN lr_struc->* TO FIELD-SYMBOL(<record>).
      CREATE DATA lr_new LIKE STANDARD TABLE OF <record>.
      ASSIGN lr_new->* TO <tab_to>.
      <tab_to> = <tab_from>.
    ENDIF.
    GET REFERENCE OF <new> INTO m_variable.

    DATA td TYPE sydes_desc.
    DESCRIBE FIELD ir_up INTO td.

    m_variable = lr_new.

    CASE io_type_descr->kind.
      WHEN c_kind-struct.
        IF i_struc_name IS SUPPLIED.
          traverse_struct( io_type_descr  = io_type_descr
                           i_name        = i_name
                           i_fullname    = i_fullname
                           i_type        = i_type
                           ir_up          = ir_up
                           i_parent_calculated = i_parent_calculated
                           i_instance     = i_instance
                           i_cl_leaf      = i_cl_leaf
                           i_struc_name  = i_struc_name
                           i_suffix       = i_suffix ).
        ELSE.
          traverse_struct( io_type_descr  = io_type_descr
                           i_name        = i_name
                           i_fullname    = i_fullname
                           i_type        = i_type
                           ir_up          = ir_up
                           i_instance     = i_instance
                           i_cl_leaf      = i_cl_leaf
                           i_parent_calculated = i_parent_calculated ).
        ENDIF.

      WHEN c_kind-elem.
        traverse_elem( i_name        = i_name
                       i_fullname    = i_fullname
                       i_type        = i_type
                       ir_up          = ir_up
                       i_instance     = i_instance
                       i_cl_leaf      = i_cl_leaf
                       i_parent_calculated = i_parent_calculated ).

      WHEN c_kind-table.
        traverse_elem( i_name        = i_name
                       i_fullname    = i_fullname
                       i_type        = i_type
                       ir_up          = ir_up
                       i_instance     = i_instance
                       i_cl_leaf      = i_cl_leaf
                       i_parent_calculated = i_parent_calculated ).
    ENDCASE.

  ENDMETHOD.

  METHOD traverse_struct.

    DATA: component       TYPE abap_component_tab,
          comp_descronent LIKE LINE OF component,
          o_struct_descr  TYPE REF TO cl_abap_structdescr,
          string          TYPE string,
          parent          TYPE string.

    o_struct_descr ?= io_type_descr.

    IF  ( i_struc_name IS SUPPLIED AND i_struc_name IS NOT INITIAL ) OR i_struc_name IS NOT SUPPLIED.
      IF i_name IS NOT INITIAL.
        save_hist( EXPORTING ir_up          = ir_up
                             i_fullname    = i_fullname
                             i_name        = i_name
                             i_type        = i_type
                             i_parent_calculated = i_parent_calculated
                             i_cl_leaf     = i_cl_leaf
                             i_instance     = i_instance ).

      ENDIF.
    ENDIF.

    component = o_struct_descr->get_components( ).

    LOOP AT component INTO comp_descronent.
      IF comp_descronent-name IS INITIAL AND comp_descronent-suffix IS NOT INITIAL.
        DATA(suffix) =  comp_descronent-suffix.
      ENDIF.

      IF i_suffix IS NOT INITIAL.
        comp_descronent-name = comp_descronent-name && i_suffix.
      ENDIF.
      DATA: lr_new_struc TYPE REF TO data.
      ASSIGN ir_up->* TO FIELD-SYMBOL(<up>).
      IF comp_descronent-name IS INITIAL.
        lr_new_struc = ir_up.
      ELSE.
        ASSIGN COMPONENT comp_descronent-name OF STRUCTURE <up> TO FIELD-SYMBOL(<new>).
        GET REFERENCE OF <new> INTO lr_new_struc.
      ENDIF.

      IF comp_descronent-name IS NOT INITIAL.
        string = |{ i_fullname }-{ comp_descronent-name }|.
      ELSE.
        string = i_fullname.
      ENDIF.

      TRY.
          CALL METHOD cl_tpda_script_data_descr=>get_quick_info
            EXPORTING
              p_var_name   = string
            RECEIVING
              p_symb_quick = DATA(quick).
        CATCH cx_tpda_varname .
      ENDTRY.

      IF quick-typid = 'r'.
        DATA: lr_variable TYPE REF TO data. "need to refaktor
        lr_variable = m_variable.

        FIELD-SYMBOLS: <symobjref> TYPE tpda_sys_symbobjref.
        ASSIGN quick-quickdata->* TO <symobjref>.

        save_hist( EXPORTING i_fullname    = string
                             i_name        = comp_descronent-name
                             i_parent_calculated = i_fullname
                             i_type        = i_type
                             i_cl_leaf     = i_cl_leaf
                             i_instance     = <symobjref>-instancename ).

        create_reference( EXPORTING i_name      = string
                                    i_type      = i_type
                                    i_shortname = comp_descronent-name
                                    i_quick     = quick ).

        m_variable = lr_variable.
      ELSE.
        IF i_name IS NOT INITIAL.
          IF i_parent_calculated IS NOT INITIAL.
            parent = |{ i_parent_calculated }-{ i_name }|.
          ELSE.
            parent = i_name.
          ENDIF.
        ELSE.
          parent = i_parent_calculated.
        ENDIF.
        traverse( io_type_descr  = comp_descronent-type
                  i_name        = comp_descronent-name
                  i_fullname    = string
                  i_type        = i_type
                  ir_up          = lr_new_struc
                  i_parent_calculated = parent
                  i_struc_name  = comp_descronent-name
                  i_cl_leaf      = i_cl_leaf
                  i_instance     = i_instance
                  i_suffix       = suffix ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD traverse_elem.

    save_hist( EXPORTING ir_up          = ir_up
                         i_fullname    = i_fullname
                         i_name        = i_name
                         i_parent_calculated = i_parent_calculated
                         i_type        = i_type
                         i_cl_leaf     = i_cl_leaf
                         i_instance     = i_instance ).

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION


CLASS lcl_ace_window IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_debugger = i_debugger.
    m_history = m_varhist =  m_zcode  = '01'.
    m_hist_depth = 9.

    mo_box = create( i_name = 'SDDE Simple Debugger Data Explorer beta v. 0.9' i_width = 1400 i_hight = 400 ).
    SET HANDLER on_box_close FOR mo_box.
    CREATE OBJECT mo_splitter
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

    mo_splitter->set_row_height( id = 1 height = '3' ).
    mo_splitter->set_row_height( id = 2 height = '70' ).

    mo_splitter->set_row_sash( id    = 1
                               type  = 0
                               value = 0 ).

    mo_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_tables_container ).

    mo_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_tables_container ).

    CREATE OBJECT mo_splitter_code
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
        container = mo_variables_container ).

    mo_splitter_code->set_column_width( EXPORTING id = 1 width = '60' ).

    CREATE OBJECT mo_splitter_var
      EXPORTING
        parent  = mo_variables_container
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_var->set_row_height( id = 1 height = '66' ).

    mo_splitter_var->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_locals_container ).

    mo_splitter_var->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_imp_exp_container ).

    CREATE OBJECT mo_splitter_imp_exp
      EXPORTING
        parent  = mo_imp_exp_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_imp_exp->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_importing_container ).

    mo_splitter_imp_exp->get_container(
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = mo_exporting_container ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_toolbar EXPORTING parent = mo_toolbar_container.
    add_toolbar_buttons( ).
    mo_toolbar->set_visible( 'X' ).
    create_code_viewer( ).

  ENDMETHOD.

  METHOD add_toolbar_buttons.

    DATA: button TYPE ttb_button,
          events TYPE cntl_simple_events,
          event  LIKE LINE OF events.

    button  = VALUE #(
     "( function = 'VIS'  icon = CONV #( icon_flight ) quickinfo = 'Visualization switch' text = 'Visualization OFF' )
     "( function = 'HIST' icon = CONV #( icon_graduate ) quickinfo = 'Stack History switch' text = 'History On' )
     "( function = 'VARHIST' icon = CONV #( icon_graduate ) quickinfo = 'Variables History switch' text = 'Vars History On' )
     "( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' )
     ( function = 'F5' icon = CONV #( icon_debugger_step_into ) quickinfo = 'Step into' text = 'Step into' )
     ( function = 'F6' icon = CONV #( icon_debugger_step_over ) quickinfo = 'Step over' text = 'Step over' )
     ( function = 'F7' icon = CONV #( icon_debugger_step_out ) quickinfo = 'Step out' text = 'Step out' )
     ( function = 'F8' icon = CONV #( icon_debugger_continue ) quickinfo = 'to the next Breakpoint' text = 'Continue' )
     ( function = 'DIRECTION' icon = CONV #( icon_column_right ) quickinfo = 'Forward' text = 'Forward' )
     ( butn_type = 3  )
     ( function = 'DEPTH' icon = CONV #( icon_next_hierarchy_level ) quickinfo = 'History depth level' text = |Depth { m_hist_depth }| )
     ( function = 'CODE' icon = CONV #( icon_customer_warehouse ) quickinfo = 'Only Z' text = 'Only Z' )
     ( function = 'CLEARVAR' icon = CONV #( icon_select_detail ) quickinfo = 'Clear all selected variables' text = 'Clear vars' )
     ( butn_type = 3  )
     ( COND #( WHEN lcl_appl=>is_mermaid_active = abap_true
      THEN VALUE #( function = 'DIAGRAM' icon = CONV #( icon_workflow_process ) quickinfo = ' Calls Flow' text = 'Diagram' ) ) )
     ( function = 'SMART' icon = CONV #( icon_wizard ) quickinfo = 'Calculations sequence' text = 'Calculations Flow' )
     ( function = 'COVERAGE' icon = CONV #( icon_wizard ) quickinfo = 'Coverage ' text = 'Coverage' )
     ( butn_type = 3  )
     ( function = 'STEPS' icon = CONV #( icon_next_step ) quickinfo = 'Steps table' text = 'Steps' )
     ( function = 'HISTORY' icon = CONV #( icon_history ) quickinfo = 'History table' text = 'History' )
     ( butn_type = 3  )
     ( function = 'ENGINE' icon = CONV #( icon_graduate ) quickinfo = 'Faster version but can skip some changes' text = 'Alpha' )
     ( function = 'DEBUG' icon = CONV #( icon_tools ) quickinfo = 'Debug' text = 'Debug' )
     ( function = 'INFO' icon = CONV #( icon_bw_gis ) quickinfo = 'Documentation' text = '' )
                    ).

    mo_toolbar->add_button_group( button ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  ENDMETHOD.

  METHOD set_program.

    lcl_source_parser=>parse_tokens( i_program = i_program io_debugger = mo_debugger ).
    READ TABLE mt_source WITH KEY include = i_program INTO DATA(source).
    IF sy-subrc = 0.
      mo_code_viewer->set_text( table = source-source->lines ).
    ENDIF.

  ENDMETHOD.

  METHOD set_program_line.

    TYPES: lntab TYPE STANDARD TABLE OF i.
    DATA lines TYPE lntab.

    mo_code_viewer->remove_all_marker( 2 ).
    mo_code_viewer->remove_all_marker( 4 ).

*    "session breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING
        main_program         = mo_debugger->mo_window->m_prg-include
      IMPORTING
        breakpoints_complete = DATA(points)
      EXCEPTIONS
        c_call_error         = 1
        generate             = 2
        wrong_parameters     = 3
        OTHERS               = 4.

    LOOP AT points INTO DATA(point). "WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
      <line> = point-line.

      APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
      MOVE-CORRESPONDING point TO <point>.
      <point>-type = 'S'.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).

*    "exernal breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING
        main_program         = mo_debugger->mo_window->m_prg-include
        flag_other_session   = abap_true
      IMPORTING
        breakpoints_complete = points
      EXCEPTIONS
        c_call_error         = 1
        generate             = 2
        wrong_parameters     = 3
        OTHERS               = 4.

    "blue arrow - current line
    APPEND INITIAL LINE TO lines ASSIGNING <line>.
    <line> = i_line.
    mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).

    CLEAR lines.

    LOOP AT points INTO point. "WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lines ASSIGNING <line>.
      <line> = point-line.

      APPEND INITIAL LINE TO mt_bpoints ASSIGNING <point>.
      MOVE-CORRESPONDING point TO <point>.
      <point>-type = 'E'.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).

    "watchpoints or coverage
    CLEAR lines.
    LOOP AT mt_watch INTO DATA(watch).
      APPEND INITIAL LINE TO lines ASSIGNING <line>.
      <line> = watch-line.
    ENDLOOP.

    "coverage
    LOOP AT mt_coverage INTO DATA(coverage).
      APPEND INITIAL LINE TO lines ASSIGNING <line>.
      <line> = coverage-line.
    ENDLOOP.

    mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).

    IF i_line IS NOT INITIAL.
      mo_code_viewer->select_lines( EXPORTING from_line = i_line to_line = i_line ).
    ENDIF.

    mo_code_viewer->clear_line_markers( 'S' ).
    mo_code_viewer->draw( ).

  ENDMETHOD.

  METHOD create_code_viewer.

    DATA: events TYPE cntl_simple_events,
          event  TYPE cntl_simple_event.

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

    "DATA(o_handler) = NEW lcl_event_handler( mo_debugger ).

    event-eventid    = cl_gui_textedit=>event_double_click.
    APPEND event TO events.

    mo_code_viewer->set_registered_events( events ).
    mo_code_viewer->register_event_border_click( ).
    mo_code_viewer->register_event_break_changed( ).

    SET HANDLER on_editor_double_click FOR mo_code_viewer.
    SET HANDLER on_editor_border_click FOR mo_code_viewer.

    mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
    mo_code_viewer->create_document( ).
    mo_code_viewer->set_readonly_mode( 1 ).

  ENDMETHOD.

  METHOD show_stack.
    IF mo_salv_stack IS INITIAL.

      cl_salv_table=>factory(
        EXPORTING
          r_container  = mo_tables_container
        IMPORTING
          r_salv_table = mo_salv_stack
        CHANGING
          t_table      = mt_stack ).

      DATA:  o_column  TYPE REF TO cl_salv_column.

      DATA(o_columns) = mo_salv_stack->get_columns( ).
      "o_columns->set_optimize( 'X' ).

      o_column ?= o_columns->get_column( 'STEP' ).
      o_column->set_output_length( '3' ).
      o_column->set_short_text( 'STEP' ).

      "o_column ?= o_columns->get_column( 'STACKPOINTER' ).
      "o_column->set_output_length( '5' ).

      o_column ?= o_columns->get_column( 'STACKLEVEL' ).
      o_column->set_output_length( '5' ).

      o_column ?= o_columns->get_column( 'PROGRAM' ).
      o_column->set_output_length( '30' ).

      o_column ?= o_columns->get_column( 'INCLUDE' ).
      o_column->set_output_length( '40' ).

      o_column ?= o_columns->get_column( 'EVENTTYPE' ).
      o_column->set_output_length( '20' ).

      o_column ?= o_columns->get_column( 'EVENTNAME' ).
      o_column->set_output_length( '50' ).

      DATA(o_event) =  mo_salv_stack->get_event( ).

      SET HANDLER on_stack_double_click FOR o_event.

      mo_salv_stack->display( ).
    ELSE.
      mo_salv_stack->refresh( ).
    ENDIF.

  ENDMETHOD.

  METHOD show_coverage.

    CLEAR: mt_watch, mt_coverage,mt_stack.
    LOOP AT mo_debugger->mt_steps INTO DATA(step).

      READ TABLE mt_stack WITH KEY include = step-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
        MOVE-CORRESPONDING step TO <stack>.
      ENDIF.

      IF step-include <> mo_debugger->mo_window->m_prg-include.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO mt_coverage ASSIGNING FIELD-SYMBOL(<coverage>).
      <coverage>-line = step-line.
    ENDLOOP.

    SORT mt_coverage.
    DELETE ADJACENT DUPLICATES FROM mt_coverage.

  ENDMETHOD.


  METHOD on_stack_double_click.

    READ TABLE mo_debugger->mo_window->mt_stack INDEX row INTO DATA(stack).
    "only for coverage stack selection should work.

    CHECK mo_debugger->mo_window->mt_coverage IS NOT INITIAL.

    "check if we have recorded steps for choosen stack level
    READ TABLE  mo_debugger->mt_steps WITH KEY program = stack-program include = stack-include TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING stack TO mo_debugger->mo_window->m_prg.
    MOVE-CORRESPONDING stack TO mo_debugger->ms_stack.

    show_coverage( ).
    mo_debugger->show_step( ).

  ENDMETHOD.

  METHOD on_editor_double_click.
    sender->get_selection_pos( IMPORTING from_line = DATA(fr_line) from_pos = DATA(fr_pos) to_line = DATA(to_line) to_pos = DATA(to_pos) ).

  ENDMETHOD.

  METHOD on_editor_border_click.

    DATA: type    TYPE char1.

    IF cntrl_pressed_set IS INITIAL.
      type = 'S'.
    ELSE.
      type = 'E'.
    ENDIF.

    LOOP AT mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = line.
      type = <point>-type.

      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING
          index        = line
          mainprog     = m_prg-program
          program      = m_prg-include
          bp_type      = type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

      IF sy-subrc = 0.
        <point>-del = abap_true.
      ENDIF.
    ENDLOOP.

    IF sy-subrc <> 0. "create
      CALL FUNCTION 'RS_SET_BREAKPOINT'
        EXPORTING
          index        = line
          program      = m_prg-include
          mainprogram  = m_prg-program
          bp_type      = type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

    ENDIF.
    DELETE mt_bpoints WHERE del IS NOT INITIAL.
    set_program_line( ).
  ENDMETHOD.


  METHOD hnd_toolbar.

    CONSTANTS: c_mask TYPE x VALUE '01'.
    FIELD-SYMBOLS: <any> TYPE any.
    m_debug_button = fcode.
    READ TABLE mt_stack INDEX 1 INTO DATA(stack).
    CASE fcode.

      WHEN 'AI'.

        READ TABLE mo_debugger->mo_window->mt_source INDEX 1 INTO DATA(source).
        NEW lcl_ai( io_source = source-source io_parent =  mo_debugger->mo_window->mo_box ).

      WHEN 'ENGINE'.
        m_version = m_version BIT-XOR c_mask.
        IF m_version IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'ENGINE'  text = 'Alpha' quickinfo = 'Faster version' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'ENGINE'  text = 'Beta' quickinfo = 'Slower version' ).
        ENDIF.
      WHEN 'VIS'.
        m_visualization = m_visualization BIT-XOR c_mask.
        IF m_visualization IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'VIS' icon = CONV #( icon_flight ) text = 'Visualization OFF' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'VIS' icon = CONV #( icon_car ) text = 'Visualization ON' ).
        ENDIF.

      WHEN 'DIRECTION'.
        m_direction = m_direction BIT-XOR c_mask.
        IF m_direction IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'DIRECTION' icon = CONV #( icon_column_right ) text = 'Forward' quickinfo = 'Forward' ).
          mo_toolbar->set_button_info( EXPORTING fcode = 'F5' text = 'Step into' quickinfo = 'Step into' ).
          mo_toolbar->set_button_info( EXPORTING fcode = 'F8' text = 'Continue' quickinfo = 'to the next Breakpoint' ).
          mo_toolbar->set_button_visible( visible = abap_true fcode = 'F6' ).
          mo_toolbar->set_button_visible( visible = abap_true fcode = 'F7' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'DIRECTION' icon = CONV #( icon_column_left ) text = 'Backward' quickinfo = 'Backward' ).
          mo_toolbar->set_button_info( EXPORTING fcode = 'F5' text = 'Step back' quickinfo = 'Step back' ).
          mo_toolbar->set_button_info( EXPORTING fcode = 'F8' text = 'to the previous stop condition' ).
          mo_toolbar->set_button_visible( visible = abap_false fcode = 'F6' ).
          mo_toolbar->set_button_visible( visible = abap_false fcode = 'F7' ).

        ENDIF.

      WHEN 'DEPTH'.
        IF m_hist_depth < 9.
          ADD 1 TO m_hist_depth.
        ELSE.
          CLEAR m_hist_depth.
        ENDIF.
        mo_toolbar->set_button_info( EXPORTING fcode = 'DEPTH' text = |Depth { m_hist_depth }| ).

      WHEN 'HIST'.
        m_history = m_history BIT-XOR c_mask.
        IF m_history IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'HIST' icon = CONV #( icon_red_xcircle ) text = 'History OFF' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'HIST' icon = CONV #( icon_graduate ) text = 'History ON' ).
        ENDIF.

      WHEN 'VARHIST'.
        m_varhist = m_varhist BIT-XOR c_mask.
        IF m_varhist IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode =  'VARHIST' icon = CONV #( icon_red_xcircle ) text = 'Vars History OFF' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode =  'VARHIST' icon = CONV #( icon_graduate ) text = 'Vars History ON' ).
        ENDIF.

      WHEN 'DIAGRAM'.
        DATA(o_mermaid) = NEW lcl_mermaid( io_debugger = mo_debugger i_type =  'DIAG' ).

      WHEN 'SMART'.
        o_mermaid = NEW lcl_mermaid( io_debugger = mo_debugger i_type =  'SMART' ).
        mo_debugger->show_step( ).

      WHEN 'COVERAGE'.
        show_coverage( ).
        mo_debugger->show_step( ).

      WHEN 'CODE'.
        m_zcode = m_zcode BIT-XOR c_mask.
        IF m_zcode IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Z & Standard' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Only Z code' ).
        ENDIF.

      WHEN 'CLEARVAR'.
        CLEAR: mo_debugger->mt_selected_var.

        DATA(nodes) = mo_debugger->mo_tree_local->m_tree->get_nodes( )->get_all_nodes( ).
        LOOP AT nodes INTO DATA(node).
          node-node->set_row_style( if_salv_c_tree_style=>default ).
        ENDLOOP.
        mo_debugger->run_script_hist( mo_debugger->m_hist_step ).
        mo_debugger->mo_tree_local->display( ).
        RETURN.
      WHEN 'DEBUG'."activate break_points
        mo_debugger->m_debug = mo_debugger->m_debug BIT-XOR c_mask.

      WHEN 'INFO'.
        DATA(url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = url.

        url = 'https://github.com/ysichov/Smart-Debugger'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = url.


      WHEN 'STEPS'.

        lcl_appl=>open_int_table( i_name = 'Steps' it_tab = mo_debugger->mt_steps io_window = mo_debugger->mo_window ).

      WHEN 'HISTORY'.
        DATA: vars_hist TYPE TABLE OF lcl_appl=>var_table_temp.
        LOOP AT  mo_debugger->mt_vars_hist INTO DATA(vars).
          APPEND INITIAL LINE TO vars_hist ASSIGNING FIELD-SYMBOL(<hist>).
          MOVE-CORRESPONDING vars TO <hist>.

          IF vars-ref IS BOUND.
            DATA(o_descr) = cl_abap_typedescr=>describe_by_data_ref( vars-ref ).

            IF o_descr->type_kind = cl_abap_typedescr=>typekind_table.
              <hist>-value = 'Table'.
            ELSEIF o_descr->type_kind = cl_abap_typedescr=>typekind_struct1."structure
              <hist>-value = 'Structure'.
            ELSEIF o_descr->type_kind = cl_abap_typedescr=>typekind_struct2."deep structure
              <hist>-value = 'Deep Structure'.
            ELSE.
              ASSIGN vars-ref->* TO <any>.
              IF sy-subrc = 0.
                <hist>-value = <any>.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        lcl_appl=>open_int_table( i_name = |mt_vars_hist - History({ lines( vars_hist ) })| it_tab = vars_hist io_window = mo_debugger->mo_window ).

    ENDCASE.

    IF m_direction IS INITIAL AND mo_debugger->m_hist_step = mo_debugger->m_step.
      IF fcode = 'F8'.
        m_start_stack = stack-stacklevel.

      ENDIF.
      CASE fcode.
        WHEN 'F5' OR 'F6' OR 'F6END' OR 'F6BEG' OR 'F7' OR 'F8'.

          IF fcode = 'F7'.
            mo_debugger->m_target_stack = stack-stacklevel - 1.
          ENDIF.

          mo_debugger->make_step( ).
      ENDCASE.

    ELSE.
      CASE fcode.

        WHEN 'F5' OR 'F6' OR 'F7' OR 'F8' OR 'F6BEG' OR 'F6END'.
          DO.
            mo_debugger->run_script_hist( IMPORTING es_stop = DATA(stop) ).

            IF stop = abap_true.
              READ TABLE  mo_debugger->mt_steps INTO DATA(step) INDEX mo_debugger->m_hist_step.
              set_program( step-include ).
              set_program_line( step-line ).
              mo_debugger->mo_tree_imp->display( ).
              mo_debugger->mo_tree_local->display( ).
              mo_debugger->mo_tree_exp->display( ).
              RETURN.
            ENDIF.
          ENDDO.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_sel_opt DEFINITION DEFERRED.

CLASS lcl_rtti IMPLEMENTATION.

  METHOD create_struc_handle.
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = i_tname
                                         RECEIVING  p_descr_ref    = DATA(o_descr)
                                         EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 0.
      e_handle ?= o_descr.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD create_table_by_name.

    DATA: o_new_tab  TYPE REF TO cl_abap_tabledescr,
          o_new_type TYPE REF TO cl_abap_structdescr.

    create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_handle = o_new_type ).
    o_new_tab = cl_abap_tabledescr=>create(
      p_line_type  = o_new_type
      p_table_kind = cl_abap_tabledescr=>tablekind_std
      p_unique     = abap_false ).
    CREATE DATA c_table TYPE HANDLE o_new_tab.  "Create a New table type
  ENDMETHOD.

ENDCLASS.

CLASS lcl_data_transmitter DEFINITION.

  PUBLIC SECTION.
    EVENTS: data_changed EXPORTING VALUE(e_row) TYPE lcl_appl=>t_sel_row,
      col_changed EXPORTING VALUE(e_column) TYPE lvc_fname.
    METHODS: emit IMPORTING e_row TYPE lcl_appl=>t_sel_row,
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
          o_tab_from     TYPE REF TO lcl_table_viewer,
          o_sel_to       TYPE REF TO lcl_sel_opt,
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
    DATA: mo_debugger TYPE REF TO lcl_table_viewer,
          mo_sel_alv  TYPE REF TO cl_gui_alv_grid,
          mt_fcat     TYPE lvc_t_fcat,
          mt_sel_tab  TYPE TABLE OF lcl_appl=>selection_display,
          ms_layout   TYPE lvc_s_layo.

    EVENTS: selection_done.
    METHODS:
      constructor IMPORTING io_viewer TYPE REF TO lcl_table_viewer io_container TYPE REF TO cl_gui_container,
      raise_selection_done,
      update_sel_tab,
      set_value IMPORTING  i_field TYPE any i_low TYPE any OPTIONAL i_high TYPE any OPTIONAL i_clear TYPE xfeld DEFAULT abap_true ,
      update_sel_row CHANGING c_sel_row TYPE lcl_appl=>selection_display.

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
          mo_alv             TYPE REF TO cl_gui_alv_grid,
          mo_sel             TYPE REF TO lcl_sel_opt,
          mr_table           TYPE REF TO data,
          mo_sel_parent      TYPE REF TO cl_gui_container,
          mo_alv_parent      TYPE REF TO cl_gui_container,
          mt_alv_catalog     TYPE lvc_t_fcat,
          mt_fields          TYPE TABLE OF t_elem,
          mo_column_emitters TYPE TABLE OF t_column_emitter,
          mo_sel_width       TYPE i,
          m_visible,
          m_std_tbar         TYPE x,
          m_show_empty       TYPE i,
          mo_window          TYPE REF TO lcl_ace_window.

    METHODS:
      constructor IMPORTING i_tname           TYPE any OPTIONAL
                            i_additional_name TYPE string OPTIONAL
                            ir_tab            TYPE REF TO data OPTIONAL
                            io_window         TYPE REF TO lcl_ace_window,
      refresh_table FOR EVENT selection_done OF lcl_sel_opt.

  PRIVATE SECTION.
    METHODS:
      create_popup,
      create_alv,
      create_sel_alv,
      set_header,
      create_field_cat IMPORTING i_tname           TYPE tabname
                       RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
      translate_field IMPORTING i_lang TYPE ddlanguage CHANGING c_fld TYPE lvc_s_fcat,
      handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
      before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
      on_table_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.

ENDCLASS.

CLASS lcl_text_viewer DEFINITION FINAL INHERITING FROM lcl_popup.

  PUBLIC SECTION.
    DATA: mo_text     TYPE REF TO cl_gui_textedit.
    METHODS: constructor IMPORTING ir_str TYPE REF TO data.
ENDCLASS.

CLASS lcl_text_viewer IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_box = create( i_name = 'text' i_width = 700 i_hight = 200 ).
    CREATE OBJECT mo_splitter
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
    DATA string TYPE TABLE OF char255.

    WHILE strlen( <str> ) > 255.
      APPEND <str>+0(255) TO string.
      SHIFT <str> LEFT BY 255 PLACES.
    ENDWHILE.

    APPEND <str> TO string.
    mo_text->set_text_as_r3table( string ).
    CALL METHOD cl_gui_cfw=>flush.
    mo_text->set_focus( mo_box ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_data_receiver IMPLEMENTATION.

  METHOD constructor.

    o_sel_to = io_sel_to.
    m_from_field =  i_from_field.
    m_to_field =  i_to_field.
    o_tab_from = io_tab_from.
    mo_transmitter = io_transmitter.

    IF mo_transmitter IS NOT INITIAL.
      IF o_tab_from IS INITIAL.
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
    CLEAR o_sel_to.

  ENDMETHOD.

  METHOD on_grid_button_click.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    CHECK m_from_field = es_col_id-fieldname.
    ASSIGN o_tab_from->mr_table->* TO <table>.
    READ TABLE <table> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <row> TO  FIELD-SYMBOL(<field>).
    CHECK o_sel_to IS NOT INITIAL.
    o_sel_to->set_value( i_field = m_to_field i_low = <field> ).
    o_sel_to->raise_selection_done( ).

  ENDMETHOD.

  METHOD  update.

    DATA: updated.

    READ TABLE o_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    IF <to>-range[] = e_row-range[].
      updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    MOVE-CORRESPONDING e_row TO <to>.
    IF <to>-transmitter IS BOUND AND updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = e_row ).
    ENDIF.
    o_sel_to->raise_selection_done( ).

  ENDMETHOD.

  METHOD update_col.

    DATA: updated,
          sel_row   TYPE lcl_appl=>t_sel_row.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE any.

    CHECK o_sel_to IS NOT INITIAL.
    READ TABLE o_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    DATA(old_range) = <to>-range.
    CLEAR: <to>-sign, <to>-opti, <to>-low, <to>-high, <to>-range.
    ASSIGN o_tab_from->mr_table->* TO <tab>.

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
      o_sel_to->update_sel_row( CHANGING c_sel_row = <to> ).
      EXIT.
    ENDLOOP.

    MOVE-CORRESPONDING <to> TO sel_row.
    IF <to>-range = old_range.
      updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    IF <to>-transmitter IS BOUND AND updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = sel_row ).
      o_sel_to->raise_selection_done( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_table_viewer IMPLEMENTATION.

  METHOD constructor.

    DATA: comp_descr   TYPE abap_componentdescr,
          comp_notab   TYPE abap_component_tab,
          comp_tab2str TYPE abap_component_tab,
          comp_str     TYPE abap_component_tab,
          s            TYPE string,
          data         TYPE REF TO data.

    DATA: notab   TYPE REF TO data,
          tab2str TYPE REF TO data.

    DATA: handle_notab   TYPE REF TO cl_abap_structdescr,
          handle_tab2str TYPE REF TO cl_abap_structdescr,
          o_new_tab      TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <notab>   TYPE STANDARD TABLE,
                   <tab2str> TYPE STANDARD TABLE,
                   <any_tab> TYPE ANY TABLE,
                   <temptab> TYPE ANY TABLE.

    super->constructor( i_additional_name = i_additional_name ).
    mo_window = io_window.
    m_lang = sy-langu.
    mo_sel_width = 0.
    m_tabname = i_tname.
    create_popup( ).

    IF ir_tab IS NOT BOUND.
      lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table ).
    ELSE.
      FIELD-SYMBOLS:<any> TYPE any.
      ASSIGN ir_tab->* TO <any>.
      DATA o_tabl  TYPE REF TO cl_abap_tabledescr.
      DATA o_struc TYPE REF TO cl_abap_structdescr.
      o_tabl ?= cl_abap_typedescr=>describe_by_data( <any> ).
      TRY.
          o_struc ?= o_tabl->get_table_line_type( ).
          ASSIGN ir_tab->* TO <any_tab>.
          TRY.
              LOOP AT o_struc->components INTO DATA(comp).

                IF comp-type_kind NE 'h'.
                  comp_descr-name = comp-name.
                  comp_descr-type ?= o_struc->get_component_type( comp-name ).
                  APPEND comp_descr TO comp_notab.
                  APPEND comp_descr TO comp_tab2str.
                ELSE.
                  comp_descr-name = comp-name.
                  comp_descr-type ?= cl_abap_typedescr=>describe_by_data( s ).
                  APPEND comp_descr TO comp_tab2str.
                  APPEND comp_descr TO comp_str.

                  comp_descr-name = comp-name && '_REF'.
                  comp_descr-type ?= cl_abap_typedescr=>describe_by_data( data ).
                  APPEND comp_descr TO comp_tab2str.
                ENDIF.
              ENDLOOP.
            CATCH cx_sy_move_cast_error.
          ENDTRY.

          TRY.
              handle_notab  = cl_abap_structdescr=>create( comp_notab ).
              handle_tab2str  = cl_abap_structdescr=>create( comp_tab2str ).

              o_new_tab = cl_abap_tabledescr=>create(
                p_line_type  = handle_notab
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false ).

              CREATE DATA notab TYPE HANDLE o_new_tab.

              o_new_tab = cl_abap_tabledescr=>create(
                p_line_type  = handle_tab2str
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false ).

              CREATE DATA tab2str TYPE HANDLE o_new_tab.

              ASSIGN notab->* TO <notab>.
              MOVE-CORRESPONDING <any_tab> TO <notab>.
              ASSIGN tab2str->* TO <tab2str>.
              MOVE-CORRESPONDING <notab> TO <tab2str>.

              LOOP AT <any_tab> ASSIGNING FIELD-SYMBOL(<old_struc>).
                READ TABLE <tab2str> ASSIGNING FIELD-SYMBOL(<new_struc>) INDEX sy-tabix.
                LOOP AT comp_str INTO comp_descr.
                  ASSIGN COMPONENT comp_descr-name OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<field>).
                  ASSIGN COMPONENT comp_descr-name OF STRUCTURE <old_struc> TO <temptab>.
                  <field> = | { icon_view_table } [{ lines( <temptab> ) }] |.
                  ASSIGN COMPONENT comp_descr-name  OF STRUCTURE <old_struc> TO <field>.
                  ASSIGN COMPONENT |{ comp_descr-name }_REF| OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<ref>).
                  GET REFERENCE OF <field> INTO <ref>.
                ENDLOOP.
              ENDLOOP.

              GET REFERENCE OF <tab2str> INTO mr_table.
            CATCH cx_root.
              mr_table = ir_tab.
          ENDTRY.
        CATCH cx_sy_move_cast_error.  "no structure
          comp_descr-name = 'FIELD'.
          comp_descr-type ?= cl_abap_typedescr=>describe_by_data( s ).
          APPEND comp_descr TO comp_tab2str.

          handle_tab2str  = cl_abap_structdescr=>create( comp_tab2str ).
          o_new_tab = cl_abap_tabledescr=>create(
            p_line_type  = handle_tab2str
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_unique     = abap_false ).

          CREATE DATA tab2str TYPE HANDLE o_new_tab.
          ASSIGN tab2str->* TO <tab2str>.
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
    "save new popup ref
    APPEND INITIAL LINE TO lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
    <popup>-parent = mo_window->mo_box.
    <popup>-child = mo_box.

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_box
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->set_column_mode( mode = mo_splitter->mode_absolute ).
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

    DATA: layout TYPE lvc_s_layo,
          effect TYPE i,
          f4s    TYPE lvc_t_f4.

    FIELD-SYMBOLS: <table>   TYPE table.

    mo_alv = NEW #( i_parent = mo_alv_parent ).
    mt_alv_catalog = create_field_cat( m_tabname ).

    IF mt_alv_catalog IS INITIAL.
      RETURN. "todo show tables without structure
    ENDIF.

    ASSIGN mr_table->* TO <table>.
    set_header( ).
    layout-cwidth_opt = abap_true.
    layout-sel_mode = 'D'.
    CREATE OBJECT lcl_appl=>c_dragdropalv.
    effect = cl_dragdrop=>move + cl_dragdrop=>copy.

    CALL METHOD lcl_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line' ##NO_TEXT
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD lcl_appl=>c_dragdropalv->get_handle IMPORTING handle = DATA(handle_alv).
    layout-s_dragdrop-grid_ddid = handle_alv.

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
        is_layout       = layout
      CHANGING
        it_fieldcatalog = mt_alv_catalog
        it_outtab       = <table>.

    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mt_alv_catalog ).
    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      CLEAR <catalog>-key.
      DATA(f4) = VALUE lvc_s_f4( register = abap_true chngeafter = abap_true fieldname = <catalog>-fieldname ).
      INSERT f4 INTO TABLE f4s.
    ENDLOOP.

    mo_alv->register_f4_for_fields( it_f4 = f4s ).
    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).

    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<cat>) WHERE scrtext_l IS INITIAL.
      lcl_alv_common=>translate_field( CHANGING c_fld = <cat> ).
    ENDLOOP.

    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).
    me->handle_user_command( EXPORTING e_ucomm = 'TECH' ).
    me->handle_user_command( EXPORTING e_ucomm = 'SHOW' ).
    mo_alv->set_toolbar_interactive( ).

  ENDMETHOD.

  METHOD translate_field.

    DATA: dd04 TYPE dd04v.

    READ TABLE mt_fields INTO DATA(field) WITH KEY field = c_fld-fieldname.
    CHECK field-elem IS NOT INITIAL.
    CLEAR dd04.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = CONV ddobjname( field-elem )
        langu         = i_lang
      IMPORTING
        dd04v_wa      = dd04
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc = 0.
      IF dd04-reptext IS NOT INITIAL.
        MOVE-CORRESPONDING dd04 TO c_fld.
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

  METHOD set_header.

    DATA: text       TYPE as4text,
          header(80) TYPE c.

    SELECT SINGLE ddtext INTO text
      FROM dd02t
     WHERE tabname = m_tabname
       AND ddlanguage = m_lang.

    header = |{ m_tabname } - { text } { m_additional_name }|.
    mo_box->set_caption( header ).

  ENDMETHOD.

  METHOD handle_tab_toolbar.

    IF m_visible IS INITIAL.
      DATA(toolbar) = VALUE ttb_button(
       ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options'  butn_type = 0 )
       ( butn_type = 3 ) ).
    ENDIF.

    APPEND VALUE #( function = 'TECH' icon = icon_wd_caption quickinfo = 'Tech names'  butn_type = 0 ) TO toolbar.

    LOOP AT lcl_appl=>mt_lang INTO DATA(lang).
      IF sy-tabix > 10.
        EXIT.
      ENDIF.
      APPEND VALUE #( function = lang-spras icon = icon_foreign_trade quickinfo = lang-sptxt butn_type = 0 text = lang-sptxt ) TO toolbar.
    ENDLOOP.

    toolbar = VALUE ttb_button( BASE toolbar
     ( function = 'SHOW'  icon = icon_list  quickinfo = 'Show empty columns'   butn_type = 0  )
     ( function = 'TBAR' icon = COND #( WHEN m_std_tbar IS INITIAL THEN icon_column_right ELSE icon_column_left )
        quickinfo = COND #( WHEN m_std_tbar IS INITIAL THEN 'Show standard ALV function'  ELSE 'Hide standard ALV function') )
     ( butn_type = 3 ) ).

    IF m_std_tbar IS INITIAL.
      e_object->mt_toolbar =  toolbar.
    ELSE.
      e_object->mt_toolbar =  toolbar = VALUE ttb_button( BASE toolbar ( LINES OF e_object->mt_toolbar ) ).
    ENDIF.

  ENDMETHOD.

  METHOD create_field_cat.

    DATA: lr_field       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          lr_data_descr  TYPE REF TO cl_abap_datadescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          texttab        TYPE tabname,
          lr_temp        TYPE REF TO data,
          name           TYPE string,
          dd04           TYPE dd04v.

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
    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = texttab ).

    LOOP AT it_tabdescr INTO DATA(ls)
       WHERE type_kind NE 'h'
         AND type_kind NE 'l'.
      DATA(ind) = sy-tabix.

      ASSIGN COMPONENT ls-name OF STRUCTURE <struc> TO <field>.
      GET REFERENCE OF <field> INTO lr_field.
      lr_data_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_field ).
      name = lr_data_descr->absolute_name.
      REPLACE ALL OCCURRENCES OF '\TYPE=' IN name WITH ''.
      APPEND VALUE #( field = ls-name elem = name ) TO mt_fields.

      CLEAR dd04.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = CONV ddobjname( name )
          langu         = m_lang
        IMPORTING
          dd04v_wa      = dd04
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).

      <catalog>-col_pos = ind.
      <catalog>-style = lcl_alv_common=>c_white.
      <catalog>-fieldname = ls-name.
      <catalog>-f4availabl = abap_true.

      IF dd04 IS INITIAL.
        <catalog>-scrtext_s = <catalog>-scrtext_m = <catalog>-scrtext_l = <catalog>-reptext = <catalog>-fieldname = ls-name.
      ELSE.
        MOVE-CORRESPONDING dd04 TO <catalog>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_doubleclick.

    DATA: o_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          table_clone   TYPE REF TO data.
    FIELD-SYMBOLS: <table>  TYPE STANDARD TABLE.

    CHECK es_row_no-row_id IS NOT INITIAL.
    ASSIGN mr_table->* TO  <table>.
    READ TABLE <table> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT e_column-fieldname  OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).

    CASE e_column-fieldname.
      WHEN 'VALUE'.
        IF sy-subrc = 0.
          IF <val> = 'Table'.
            ASSIGN COMPONENT 'REF'  OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
            lcl_appl=>open_int_table( EXPORTING i_name = CONV #( e_column-fieldname ) it_ref = <ref> io_window = mo_window ).
          ENDIF.
        ELSE.
          TRY.
              o_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
              table_clone = o_table_descr->elem_clone( ).
              lcl_appl=>open_int_table( EXPORTING i_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
            CATCH cx_sy_move_cast_error.
          ENDTRY.
        ENDIF.
      WHEN 'STEP'.
        MOVE-CORRESPONDING <row> TO mo_window->m_prg.
        MOVE-CORRESPONDING <row> TO mo_window->mo_debugger->ms_stack.

        mo_window->show_coverage( ).
        mo_window->mo_debugger->show_step( ).
      WHEN OTHERS. "check if it is an embedded table.
        TRY.
            o_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
            table_clone = o_table_descr->elem_clone( ).
            lcl_appl=>open_int_table( EXPORTING i_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
          CATCH cx_sy_move_cast_error.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD on_table_close.
    DATA: tabix LIKE sy-tabix.
    sender->free( ).

    "Free Memory
    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
      IF <obj>-alv_viewer->mo_box = sender.
        tabix = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.
      FREE <obj>-alv_viewer->mr_table.
      FREE <obj>-alv_viewer->mo_alv.

      "shutdown receivers.
      IF <obj>-alv_viewer->mo_sel IS NOT INITIAL.
        LOOP AT <obj>-alv_viewer->mo_sel->mt_sel_tab INTO DATA(sel).
          IF sel-receiver IS BOUND.
            sel-receiver->shut_down( ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      FREE <obj>-alv_viewer.
      IF tabix NE 0.
        DELETE lcl_appl=>mt_obj INDEX tabix.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD before_user_command.

    CASE e_ucomm.
      WHEN '&INFO'.
        DATA(url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = url.
    ENDCASE.

  ENDMETHOD.

  METHOD handle_user_command.

    DATA: it_fields  TYPE lvc_t_fcat,
          clause(45),
          sel_width  TYPE i.

    FIELD-SYMBOLS: <table>  TYPE STANDARD  TABLE.
    ASSIGN mr_table->* TO <table>.
    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = it_fields[] ).
    IF e_ucomm = 'SEL_ON' AND m_visible IS INITIAL.
      create_sel_alv( ).
      m_visible = abap_true.
      IF mo_sel_width = 0.
        sel_width = 500.
      ELSE.
        sel_width = mo_sel_width.
      ENDIF.

      mo_splitter->set_column_width( EXPORTING id = 1 width = sel_width ).
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
              clause = |{ <fields>-fieldname } IS NOT INITIAL|.
              LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>)  WHERE (clause).
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
      mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang ).
    ENDIF.

    CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].

    lcl_alv_common=>refresh( mo_alv ).
    IF mo_sel IS BOUND.
      IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
        mo_sel->update_sel_tab( ).
      ENDIF.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
      mo_sel->mo_sel_alv->refresh_table_display(  ).
    ENDIF.

  ENDMETHOD.                           "handle_user_command


  METHOD refresh_table.

    DATA: row    TYPE lcl_appl=>t_sel_row,
          filter TYPE lvc_t_filt.

    CLEAR filter.
    set_header( ).

    LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO row.
        <sel>-transmitter->emit( e_row = row ).
      ENDIF.
      LOOP AT <sel>-range INTO DATA(range).
        APPEND VALUE #( fieldname = <sel>-field_label
                              low = range-low
                             high = range-high
                             sign = range-sign
                           option = range-opti ) TO filter.
      ENDLOOP.
    ENDLOOP.

    IF mo_sel->mt_sel_tab IS NOT INITIAL.
      CALL METHOD mo_alv->set_filter_criteria
        EXPORTING
          it_filter = filter.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
      lcl_alv_common=>refresh( mo_alv ).
      mo_sel->mo_debugger->handle_user_command( 'SHOW' ).
      LOOP AT mo_column_emitters INTO DATA(emit).
        emit-emitter->emit_col( emit-column ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sel_opt IMPLEMENTATION.
  METHOD constructor.
    DATA: effect     TYPE i,
          handle_alv TYPE i.

    mo_debugger = io_viewer.
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

    DATA: row TYPE lcl_appl=>t_sel_row.

    lcl_alv_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    LOOP AT mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO row.
        <sel>-transmitter->emit( e_row = row ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD update_sel_tab.

    IF mt_sel_tab[] IS NOT INITIAL.
      DATA(sel_tab_copy) = mt_sel_tab.
    ENDIF.
    CLEAR mt_sel_tab[].
    mo_debugger->mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mo_debugger->mt_alv_catalog ).
    LOOP AT mo_debugger->mt_alv_catalog INTO DATA(catalog) WHERE domname NE 'MANDT'.
      DATA(ind) = sy-tabix.
      APPEND INITIAL LINE TO mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel_tab>).
      READ TABLE sel_tab_copy INTO DATA(copy) WITH KEY field_label = catalog-fieldname.

      IF sy-subrc = 0.
        MOVE-CORRESPONDING copy TO <sel_tab>.
      ELSE.
        <sel_tab>-option_icon = icon_led_inactive.
        <sel_tab>-more_icon = icon_enter_more.
      ENDIF.

      <sel_tab>-ind = ind.
      <sel_tab>-field_label = catalog-fieldname.
      <sel_tab>-int_type = catalog-inttype.
      <sel_tab>-element = catalog-rollname.
      <sel_tab>-domain =  catalog-domname.
      <sel_tab>-datatype = catalog-datatype.
      <sel_tab>-length = catalog-outputlen.
      lcl_alv_common=>translate_field( EXPORTING i_lang = mo_debugger->m_lang CHANGING c_fld = catalog ).
      <sel_tab>-name = catalog-scrtext_l.
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
      DATA: row TYPE lcl_appl=>t_sel_row.
      MOVE-CORRESPONDING <to> TO row.
      <to>-transmitter->emit( EXPORTING e_row = row ).
    ENDIF.

  ENDMETHOD.

  METHOD handle_doubleclick.

    DATA: it_bdcdata TYPE TABLE OF  bdcdata.

    CHECK es_row_no-row_id IS NOT INITIAL.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id INTO DATA(sel).
    APPEND VALUE #( program = 'SAPLSD_ENTRY' dynpro = '1000' dynbegin = abap_true ) TO it_bdcdata.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WB_DISPLAY' ) TO it_bdcdata.

    IF e_column = 'ELEMENT'.
      SET PARAMETER ID 'DTYP' FIELD sel-element.
      APPEND VALUE #( fnam = 'RSRD1-DDTYPE' fval = abap_true ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSEIF e_column = 'DOMAIN'.
      SET PARAMETER ID 'DOM' FIELD sel-domain.
      APPEND VALUE #( fnam = 'RSRD1-DOMA' fval = abap_true ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSE.
      CALL FUNCTION 'DOCU_CALL'
        EXPORTING
          id                = 'DE'
          langu             = mo_debugger->m_lang
          object            = sel-element
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

    IF c_sel_row-low CA  '*%+&' AND c_sel_row-opti <> 'NP'.
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
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
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
          objects    TYPE TABLE OF objec,
          objec      TYPE objec,
          otype      TYPE otype,
          plvar      TYPE plvar,
          multiple   TYPE xfeld,
          clear      TYPE xfeld.

    IF e_fieldname = 'LOW'.
      multiple = abap_true.
    ENDIF.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
    DATA(fname) =  <sel>-field_label.

    lcl_appl=>mt_sel[] = mt_sel_tab[].
    IF <sel>-element = 'HROBJID'.
      READ TABLE mt_sel_tab INTO DATA(sel) WITH KEY field_label = 'OTYPE'.
      otype = sel-low.
      READ TABLE mt_sel_tab INTO sel WITH KEY field_label = 'PLVAR'.
      IF sy-subrc = 0 AND sel-low IS NOT INITIAL.
        plvar = sel-low.
      ELSE.
        CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
          IMPORTING
            act_plvar       = plvar
          EXCEPTIONS
            no_active_plvar = 1
            OTHERS          = 2.
      ENDIF.
    ELSEIF <sel>-element = 'PERSNO'.
      otype = 'P'.
    ENDIF.

    IF otype IS NOT INITIAL.
      CALL FUNCTION 'RH_OBJID_REQUEST'
        EXPORTING
          plvar            = plvar
          otype            = otype
          seark_begda      = sy-datum
          seark_endda      = sy-datum
          dynpro_repid     = sy-repid
          dynpro_dynnr     = sy-dynnr
          set_mode         = multiple
        IMPORTING
          sel_object       = objec
        TABLES
          sel_hrobject_tab = objects
        EXCEPTIONS
          OTHERS           = 6.
      IF sy-subrc = 0.
        clear = abap_true.
        LOOP AT objects INTO objec.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = objec-objid i_clear = clear ).
            CLEAR clear.
          ELSE.
            set_value( EXPORTING i_field = <sel>-field_label i_high = objec-objid i_clear = clear ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.

      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
        EXPORTING
          tabname           = mo_debugger->m_tabname
          fieldname         = fname
          callback_program  = sy-repid
          callback_form     = 'CALLBACK_F4_SEL' "callback_method - doesn't work for local class
          multiple_choice   = multiple
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
        clear = abap_true.
        LOOP AT return_tab ASSIGNING FIELD-SYMBOL(<ret>) WHERE fieldname = fname.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = <ret>-fieldval i_clear = clear ).
            CLEAR clear.
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

    DATA: tabfield TYPE rstabfield,
          opt      TYPE rsoptions VALUE 'XXXXXXXXXX',
          sign     TYPE raldb_sign,
          option   TYPE raldb_opti.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    CASE es_col_id.
      WHEN 'OPTION_ICON'. "edit select logical expression type
        CALL FUNCTION 'SELECT_OPTION_OPTIONS'
          EXPORTING
            selctext     = 'nnnn'
            option_list  = opt
          IMPORTING
            sign         = sign
            option       = option
          EXCEPTIONS
            delete_line  = 1
            not_executed = 2
            illegal_sign = 3
            OTHERS       = 4.
        IF sy-subrc = 0.
          <tab>-sign = sign.
          <tab>-opti = option.
        ELSEIF sy-subrc = 1.
          CLEAR: <tab>-low, <tab>-high,<tab>-sign, <tab>-opti, <tab>-range.
        ENDIF.
      WHEN 'MORE_ICON'. "edit ranges
        tabfield-tablename = mo_debugger->m_tabname.
        tabfield-fieldname = <tab>-field_label.

        CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
          EXPORTING
            title             = 'title'
            text              = 'text'
            tab_and_field     = tabfield
          TABLES
            range             = <tab>-range
          EXCEPTIONS
            no_range_tab      = 1
            cancelled         = 2
            internal_error    = 3
            invalid_fieldname = 4
            OTHERS            = 5.
        IF sy-subrc = 0.
          READ TABLE <tab>-range INDEX 1 INTO DATA(range).
          MOVE-CORRESPONDING range TO <tab>.
          IF <tab>-opti NE 'BT'.
            CLEAR <tab>-high.
          ENDIF.
        ENDIF.
    ENDCASE.
    update_sel_row( CHANGING c_sel_row = <tab> ).
    RAISE EVENT selection_done.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: start TYPE i,
          time  TYPE sy-uzeit.

    FIELD-SYMBOLS: <field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<cells>).
      READ TABLE mt_sel_tab INDEX <cells>-row_id ASSIGNING FIELD-SYMBOL(<tab>).
      ASSIGN COMPONENT <cells>-fieldname OF STRUCTURE <tab> TO <field>.
      READ TABLE mo_debugger->mt_alv_catalog WITH KEY fieldname = <tab>-field_label INTO DATA(cat).

      IF <field> IS NOT INITIAL AND <cells>-value IS INITIAL.
        READ TABLE <tab>-range INTO DATA(second) INDEX 2.
        IF sy-subrc = 0.
          IF ( <cells>-fieldname = 'LOW' AND <tab>-high IS INITIAL ) OR  ( <cells>-fieldname = 'HIGH' AND <tab>-low IS INITIAL  ).
            DELETE <tab>-range INDEX 1.
          ELSE.
            CLEAR second.
          ENDIF.
        ENDIF.
      ENDIF.

      IF cat-convexit = 'ALPHA' AND NOT  <cells>-value CA '+*'.
        <cells>-value = |{ <cells>-value ALPHA = IN }|.
        start = 128 - cat-dd_outlen.
        <cells>-value = <cells>-value+start(cat-dd_outlen).
      ENDIF.

      IF <cells>-value IS NOT INITIAL.
        IF <tab>-int_type = 'D'.
          DATA: date TYPE sy-datum.
          CALL FUNCTION 'CONVERT_DATE_INPUT'
            EXPORTING
              input                     = <cells>-value
              plausibility_check        = abap_true
            IMPORTING
              output                    = date
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.

          IF sy-subrc = 0.
            <cells>-value = |{ date DATE = USER }|.
          ENDIF.
        ELSEIF <tab>-int_type = 'T'.
          CALL FUNCTION 'CONVERT_TIME_INPUT'
            EXPORTING
              input                     = <cells>-value
            IMPORTING
              output                    = time
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.
          <cells>-value = time+0(2) && ':' && time+2(2) && ':' && time+4(2).
        ENDIF.
      ENDIF.
    ENDLOOP.
    CHECK sy-subrc = 0.

    IF second IS INITIAL.
      <field> = <cells>-value.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = <cells>-fieldname i_value = <cells>-value ).
    ELSE.
      <tab>-low = second-low.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = 'LOW' i_value = second-low ).
      IF second-high CO '0 '.
        CLEAR second-high.
      ENDIF.
      <tab>-high = second-high.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = 'HIGH' i_value = second-high ).

      <tab>-opti = second-opti.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = 'OPTI' i_value = second-opti ).
      <tab>-sign = second-sign.
      er_data_changed->modify_cell( EXPORTING i_row_id = <cells>-row_id i_fieldname = 'SIGN' i_value = second-sign ).
    ENDIF.

    update_sel_row( CHANGING c_sel_row = <tab> ).
    lcl_alv_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout ).
    raise_selection_done( ).

  ENDMETHOD.

  METHOD on_data_changed_finished.

    CHECK e_modified IS NOT INITIAL.
    RAISE EVENT selection_done.

  ENDMETHOD.

  METHOD handle_context_menu_request.

    DATA: func  TYPE ui_func,
          funcs TYPE ui_functions.

    DATA(index) = lcl_alv_common=>get_selected( mo_sel_alv ).

    IF index IS NOT INITIAL.
      READ TABLE mt_sel_tab INTO DATA(sel) INDEX index.
    ENDIF.

    e_object->get_functions( IMPORTING fcodes = DATA(fcodes) ). "Inactivate all standard functions

    LOOP AT fcodes INTO DATA(fcode) WHERE fcode NE '&OPTIMIZE'.
      func = fcode-fcode.
      APPEND func TO funcs.
    ENDLOOP.

    e_object->hide_functions( funcs ).
    e_object->add_separator( ).

    IF sel-range[]  IS NOT INITIAL OR index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'SEL_CLEAR'
          text  = 'Clear Select-Options'.
    ENDIF.

    IF sel-receiver IS NOT INITIAL OR index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DELR'
          text  = 'Delete receiver'.
    ENDIF.

  ENDMETHOD.

  METHOD handle_user_command.

    DATA: sel_width TYPE i.

    IF e_ucomm = 'SEL_OFF'. "Hide select-options alv

      mo_debugger->m_visible = ''.

      sel_width = 0.
      CALL METHOD mo_debugger->mo_splitter->get_column_width
        EXPORTING
          id                = 1
        IMPORTING
          result            = mo_debugger->mo_sel_width
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.

      CALL METHOD mo_debugger->mo_splitter->set_column_width
        EXPORTING
          id    = 1
          width = sel_width.
      mo_debugger->mo_alv->set_toolbar_interactive( ).
      RETURN.
    ENDIF.

    IF e_ucomm = 'SEL_CLEAR' OR e_ucomm = 'DELR'. "clear all selections
      mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).

      LOOP AT sel_rows INTO DATA(row).
        READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX row-index.
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

    lcl_alv_common=>refresh( mo_debugger->mo_alv ).
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
*    SELECT c~spras t~sptxt INTO CORRESPONDING FIELDS OF TABLE mt_lang
*      FROM t002c AS c
*      INNER JOIN t002t AS t
*      ON c~spras = t~sprsl
*      WHERE t~spras = sy-langu
*      ORDER BY c~ladatum DESCENDING c~lauzeit DESCENDING.
  ENDMETHOD.

  METHOD check_mermaid.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.

    IF sy-subrc = 0.
      is_mermaid_active = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD open_int_table.

    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND.
      r_tab = it_ref.
    ELSE.
      GET REFERENCE OF it_tab INTO r_tab.
    ENDIF.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW #(  i_additional_name = i_name ir_tab = r_tab io_window = io_window ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_rtti_tree IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    mo_debugger = i_debugger.

    cl_salv_tree=>factory(
      EXPORTING
        r_container = i_cont
      IMPORTING
        r_salv_tree = m_tree
      CHANGING
        t_table     = tree_table ).

    DATA(o_setting) =  m_tree->get_tree_settings( ).
    o_setting->set_hierarchy_header( i_header ).
    o_setting->set_hierarchy_size( 30 ).
    o_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

    DATA(o_columns) = m_tree->get_columns( ).
    o_columns->set_optimize( abap_true ).

    o_columns->get_column( 'VALUE' )->set_short_text( 'Value' ).
    o_columns->get_column( 'INSTANCE' )->set_short_text( 'Instance' ).
    o_columns->get_column( 'FULLNAME' )->set_visible( abap_false ).
    o_columns->get_column( 'PATH' )->set_visible( abap_false ).
    o_columns->get_column( 'TYPENAME' )->set_short_text( 'Type' ).
    o_columns->get_column( 'TYPENAME' )->set_medium_text( 'Absolute Type' ).

    add_buttons( i_type ).

    DATA(o_event) = m_tree->get_event( ) .
    SET HANDLER hndl_double_click
                hndl_user_command FOR o_event.

    m_globals = '01'.
    m_tree->display( ).

  ENDMETHOD.

  METHOD add_buttons.

    DATA(o_functions) = m_tree->get_functions( ).
    o_functions->set_all( ).

    o_functions->set_group_layout( abap_false ).
    o_functions->set_group_aggregation( abap_false ).
    o_functions->set_group_print( abap_false ).

    CHECK mo_debugger IS NOT INITIAL AND i_type = 'L'.

    o_functions->add_function(
      name     = 'INITIALS'
      icon     = CONV #( icon_start_viewer )
      text     = 'Initials'
      tooltip  = 'Show/hide initial values'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'LOCALS'
      icon     = CONV #( icon_foreign_trade )
      text     = 'Locals'
      tooltip  = 'Show/hide locals variables'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'GLOBALS'
      icon     = CONV #( icon_foreign_trade )
      text     = 'Globals'
      tooltip  = 'Show/hide global variables'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'SYST'
      icon     = CONV #( icon_foreign_trade )
      text     = 'SYST'
      tooltip  = 'Show/hide SY sructure'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'CLASS_DATA'
      icon     = CONV #( icon_oo_class_attribute )
      text     = 'CLASS-DATA'
      tooltip  = 'Show/hide Class-Data variables (global)'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'LDB'
      icon     = CONV #( icon_biw_report_view )
      text     = 'LDB'
      tooltip  = 'Show/hide Local Data Base variables (global)'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    o_functions->add_function(
      name     = 'REFRESH'
      icon     = CONV #( icon_refresh )
      text     = ''
      tooltip  = 'Refresh'
      position = if_salv_c_function_position=>left_of_salv_functions ).

  ENDMETHOD.

  METHOD clear.

    m_tree->get_nodes( )->delete_all( ).

    CLEAR: m_globals_key,
           m_locals_key,
           m_syst_key,
           m_ldb_key,
           m_class_key,
           mt_vars,
           mt_classes_leaf.

  ENDMETHOD.

  METHOD traverse.

    ASSIGN ir_up->* TO FIELD-SYMBOL(<new>).
    IF <new> IS INITIAL AND m_hide IS NOT INITIAL.
      me->del_variable( CONV #( is_var-name )  ).
      RETURN.
    ENDIF.

    CASE io_type_descr->kind.
      WHEN c_kind-struct.
        IF i_struc_name IS SUPPLIED.
          e_root_key = traverse_struct( io_type_descr  = io_type_descr
                                        i_parent_key  = i_parent_key
                                        i_rel         = i_rel
                                        is_var         = is_var
                                        ir_up          = ir_up
                                        i_parent_calculated = i_parent_calculated
                                        i_struc_name  = i_struc_name ).
        ELSE.
          e_root_key = traverse_struct( io_type_descr  = io_type_descr
                                        i_parent_key  = i_parent_key
                                        i_rel         = i_rel
                                        is_var         = is_var
                                        ir_up          = ir_up
                                        i_parent_calculated = i_parent_calculated ).
        ENDIF.

      WHEN c_kind-table.
        e_root_key = traverse_table( io_type_descr  = io_type_descr
                                     i_parent_key  = i_parent_key
                                     i_rel         = i_rel
                                     is_var         = is_var
                                     ir_up          = ir_up
                                     i_parent_calculated = i_parent_calculated ).
      WHEN c_kind-elem.
        e_root_key = traverse_elem( io_type_descr  = io_type_descr
                                    i_parent_key  = i_parent_key
                                    i_rel         = i_rel
                                    is_var         = is_var
                                    i_parent_calculated = i_parent_calculated ).

    ENDCASE.

  ENDMETHOD.

  METHOD traverse_struct.

    DATA: component      TYPE abap_component_tab,
          o_struct_descr TYPE REF TO cl_abap_structdescr,
          tree           TYPE ts_table,
          text           TYPE lvc_value,
          key            TYPE salv_de_node_key,
          rel            TYPE salv_de_node_relation,
          icon           TYPE salv_de_tree_image.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    rel = i_rel.
    o_struct_descr ?= io_type_descr.
    tree-ref =  ir_up.
    IF is_var-instance NE '{A:initial}'.
      "ls_tree-typename = o_struct_descr->absolute_name.
      "REPLACE FIRST OCCURRENCE OF '\TYPE=' IN tree-typename+0(6) WITH ''.
      DATA: split TYPE TABLE OF string.
      SPLIT o_struct_descr->absolute_name AT '\TYPE=' INTO TABLE split.
      tree-typename = split[ lines( split ) ].

      IF tree-typename+0(1) = '%'.
        tree-typename = |{ o_struct_descr->type_kind }({ o_struct_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    tree-kind = o_struct_descr->type_kind.

    IF m_icon IS INITIAL.
      icon = icon_structure.
    ELSE.
      icon = m_icon.
    ENDIF.

    text = is_var-short.
    tree-fullname = is_var-name.
    tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        key = leaf-key.
      ENDIF.
    ELSE.
      key = i_parent_key.
    ENDIF.

    IF key IS INITIAL.
      key = i_parent_key.
      rel = i_rel.
    ENDIF.

    IF  ( i_struc_name IS SUPPLIED AND i_struc_name IS NOT INITIAL ) OR i_struc_name IS NOT SUPPLIED.
      IF text IS NOT INITIAL.

        DATA(nodes) = m_tree->get_nodes( )->get_all_nodes( ).
        LOOP AT nodes INTO DATA(node).
          DATA(lr_row) = node-node->get_data_row( ).
          FIELD-SYMBOLS <row> TYPE ts_table.
          ASSIGN lr_row->* TO <row>.
          IF <row>-fullname = is_var-name.
            DATA(o_node) = node-node.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF o_node IS NOT INITIAL.
          READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(var).
          IF sy-subrc = 0.
            IF o_node IS NOT INITIAL.
              TRY.
                  FIELD-SYMBOLS: <old_value> TYPE any.
                  ASSIGN var-ref->* TO <old_value>.
                  IF sy-subrc = 0.
                    IF is_var-type = var-type.
                      RETURN.
                    ELSE.
                      key = var-key.
                      rel = if_salv_c_node_relation=>next_sibling.
                      DELETE mt_vars WHERE name = is_var-name.
                    ENDIF.
                  ENDIF.
                CATCH cx_root.
                  DELETE mt_vars WHERE name = is_var-name.
              ENDTRY.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      e_root_key = m_tree->get_nodes( )->add_node(
             related_node   = key
             relationship   = rel
             data_row       = tree
             collapsed_icon = icon
             expanded_icon  = icon
             text           = text
             folder         = abap_false )->get_key( ).

      APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
      <vars>-key = e_root_key.
      <vars>-stack = mo_debugger->mo_window->mt_stack[ 1 ]-stacklevel.
      <vars>-step  = mo_debugger->m_step - mo_debugger->m_step_delta.
      <vars>-program   = mo_debugger->mo_window->m_prg-program.
      <vars>-eventtype = mo_debugger->mo_window->m_prg-eventtype.
      <vars>-eventname = mo_debugger->mo_window->m_prg-eventname.
      <vars>-leaf  = m_leaf.
      <vars>-name  = is_var-name.
      <vars>-short = is_var-short.
      <vars>-ref  = ir_up.
      <vars>-cl_leaf = is_var-cl_leaf.
      <vars>-path = is_var-path.
      <vars>-type = o_struct_descr->absolute_name.

    ENDIF.

    IF rel = if_salv_c_node_relation=>next_sibling AND o_node IS NOT INITIAL.
      IF o_node IS NOT INITIAL.

        o_node->delete( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD traverse_elem.

    DATA: o_elem_descr TYPE REF TO cl_abap_elemdescr,
          tree         TYPE ts_table,
          text         TYPE lvc_value,
          icon         TYPE salv_de_tree_image,
          key          TYPE salv_de_node_key,
          rel          TYPE salv_de_node_relation.

    o_elem_descr ?= io_type_descr.
    tree-ref = is_var-ref.
    rel = i_rel.

    IF is_var-instance NE '{A:initial}'.
      tree-typename = o_elem_descr->absolute_name.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN tree-typename WITH ''.
      IF tree-typename+0(1) = '%'.
        tree-typename = |{ o_elem_descr->type_kind }({ o_elem_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    tree-kind = o_elem_descr->type_kind.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    IF i_value IS SUPPLIED.
      tree-value = i_value.
    ELSE.
      IF <new_value> IS NOT INITIAL.
        tree-value = <new_value>.
      ENDIF.
    ENDIF.

    CASE o_elem_descr->type_kind.
      WHEN 'D'.
        icon = icon_date.
      WHEN 'T'.
        icon = icon_bw_time_sap.
      WHEN 'C'.
        icon = icon_wd_input_field.
      WHEN 'P'.
        icon = icon_increase_decimal.
      WHEN 'g'.
        icon = icon_text_act.
      WHEN 'N' OR 'I'.
        icon = icon_pm_order.
      WHEN OTHERS.
        icon = icon_element.
    ENDCASE.

    text = is_var-short.
    tree-fullname = is_var-name."is_var-path.
    tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        key = leaf-key.
      ENDIF.
    ELSE.
      key = i_parent_key.
    ENDIF.

    IF key IS INITIAL.
      key = i_parent_key.
      rel = i_rel.
    ENDIF.

    DATA(nodes) = m_tree->get_nodes( )->get_all_nodes( ).
    LOOP AT nodes INTO DATA(node).
      DATA(name) = node-node->get_text( ).
      DATA(lr_row) = node-node->get_data_row( ).
      FIELD-SYMBOLS <row> TYPE ts_table.
      ASSIGN lr_row->* TO <row>.
      IF <row>-fullname = is_var-name.
        DATA(o_node) = node-node.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

    IF o_node IS NOT INITIAL.
      READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(var).
      IF sy-subrc = 0.
        TRY.
            FIELD-SYMBOLS: <old_value> TYPE any.
            ASSIGN var-ref->* TO <old_value>.
            IF sy-subrc = 0.
              IF is_var-type = var-type.
                IF <old_value> NE <new_value>.
                  key = var-key.
                  rel = if_salv_c_node_relation=>next_sibling.
                  DELETE mt_vars WHERE name = is_var-name.
                ELSE.
                  IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
                  ELSE.
                    RETURN.
                  ENDIF.
                ENDIF.
              ELSE.
                key = var-key.
                rel = if_salv_c_node_relation=>next_sibling.
                DELETE mt_vars WHERE name = is_var-name.
              ENDIF.
            ENDIF.
          CATCH cx_root.
            DELETE mt_vars WHERE name = is_var-name.
        ENDTRY.
      ENDIF.
    ENDIF.

    DATA(o_nodes) = m_tree->get_nodes( ).

    TRY.
        CALL METHOD o_nodes->add_node
          EXPORTING
            related_node   = key
            relationship   = rel
            data_row       = tree
            collapsed_icon = icon
            expanded_icon  = icon
            text           = text
            folder         = abap_false
          RECEIVING
            node           = o_node.

        IF sy-subrc = 0.
          e_root_key = o_node->get_key( ).

          APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
          <vars>-stack = mo_debugger->mo_window->mt_stack[ 1 ]-stacklevel.
          <vars>-step = mo_debugger->m_step - mo_debugger->m_step_delta.
          <vars>-program = mo_debugger->mo_window->m_prg-program.
          <vars>-eventtype = mo_debugger->mo_window->m_prg-eventtype.
          <vars>-eventname = mo_debugger->mo_window->m_prg-eventname.
          <vars>-leaf = m_leaf.
          <vars>-name = is_var-name.
          <vars>-short = is_var-short.
          <vars>-key = e_root_key.
          <vars>-ref = is_var-ref.
          <vars>-cl_leaf = is_var-cl_leaf.
          <vars>-type = o_elem_descr->absolute_name.
          <vars>-path = is_var-path.

          IF rel = if_salv_c_node_relation=>next_sibling AND o_node IS NOT INITIAL.
            IF o_node IS NOT INITIAL.
              o_node->delete( ).
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.

  METHOD traverse_obj.

    DATA: tree TYPE ts_table,
          text TYPE lvc_value,
          icon TYPE salv_de_tree_image,
          key  TYPE salv_de_node_key,
          rel  TYPE salv_de_node_relation.

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(var).
    IF mo_debugger->m_debug IS NOT INITIAL.BREAK-POINT.ENDIF.
    IF sy-subrc = 0.
      DATA(o_nodes) = m_tree->get_nodes( ).
      DATA(o_node) =  o_nodes->get_node( var-key ).

      IF var-ref = ir_up.
        RETURN.
      ENDIF.

    ELSE.
      rel = i_rel.
    ENDIF.

    icon = icon_oo_object.
    text = is_var-short.
    tree-fullname = is_var-name.
    tree-path = is_var-path.

    DATA(string) = is_var-instance.
    DATA: split TYPE TABLE OF string.

    IF is_var-instance IS NOT INITIAL.
      string = is_var-instance.
      REPLACE ALL OCCURRENCES OF REGEX '[*{}]' IN string WITH ''.
      REPLACE ALL OCCURRENCES OF '\CLASS' IN string WITH ''.
      REPLACE ALL OCCURRENCES OF '\PROGRAM' IN string WITH ''.
      SPLIT string AT '=' INTO TABLE split.
      tree-instance = |{ split[ lines( split ) ] }({ split[ 1 ] })|.
    ENDIF.
    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        key = leaf-key.
      ENDIF.
    ENDIF.

    IF key IS INITIAL.
      key = i_parent_key.
      rel = i_rel.
    ENDIF.

    e_root_key = m_tree->get_nodes( )->add_node(
     related_node   = key
     relationship   = rel
     data_row       = tree
     collapsed_icon = icon
     expanded_icon  = icon
     text           = text
     folder         = abap_false )->get_key( ).

    APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
    <vars>-stack = mo_debugger->mo_window->mt_stack[ 1 ]-stacklevel.
    <vars>-step = mo_debugger->m_step - mo_debugger->m_step_delta.
    <vars>-program = mo_debugger->mo_window->m_prg-program.
    <vars>-eventtype = mo_debugger->mo_window->m_prg-eventtype.
    <vars>-eventname = mo_debugger->mo_window->m_prg-eventname.
    <vars>-leaf = m_leaf.
    <vars>-name = is_var-name.
    <vars>-short = is_var-short.
    <vars>-key = e_root_key.
    <vars>-cl_leaf = is_var-cl_leaf.
    <vars>-path = is_var-path.

    IF o_node IS NOT INITIAL.
      o_node->delete( ).
    ENDIF.

  ENDMETHOD.

  METHOD traverse_table.

    DATA: o_table_descr TYPE REF TO cl_abap_tabledescr,
          tree          TYPE ts_table,
          text          TYPE lvc_value,
          icon          TYPE salv_de_tree_image,
          key           TYPE salv_de_node_key,
          rel           TYPE salv_de_node_relation.

    FIELD-SYMBOLS: <tab> TYPE ANY TABLE.

    ASSIGN ir_up->* TO <tab>.
    DATA(lines) = lines( <tab> ).
    tree-ref = ir_up.
    key = i_parent_key.

    o_table_descr ?= io_type_descr.

    tree-fullname = |{ is_var-short } ({ lines })|.
    tree-kind = o_table_descr->type_kind.
    IF is_var-instance NE '{A:initial}'.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = mo_debugger->ms_stack-include INTO DATA(source).
      READ TABLE source-tt_tabs WITH KEY name = is_var-short INTO DATA(tab).
      IF sy-subrc <> 0.
        DATA: split TYPE TABLE OF string.
        SPLIT o_table_descr->absolute_name AT '\TYPE=' INTO TABLE split.
        tree-typename = split[ lines( split ) ].
      ELSE.
        tree-typename = tab-type.
      ENDIF.
    ENDIF.
    icon = icon_view_table.

    IF is_var-name IS NOT INITIAL.
      text = tree-fullname.
    ELSE.
      text = tree-typename.
    ENDIF.

    rel = i_rel.
    ASSIGN ir_up->* TO FIELD-SYMBOL(<new_value>).
    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(var).
    DATA(nodes) = m_tree->get_nodes( )->get_all_nodes( ).
    LOOP AT nodes INTO DATA(node).
      DATA(lr_row) = node-node->get_data_row( ).
      FIELD-SYMBOLS <row> TYPE ts_table.
      ASSIGN lr_row->* TO <row>.
      IF <row>-fullname = is_var-name.
        DATA(o_node) = node-node.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF o_node IS NOT INITIAL.
      TRY.
          FIELD-SYMBOLS: <old_value> TYPE any.
          ASSIGN var-ref->* TO <old_value>.
          IF sy-subrc = 0.
            IF <old_value> NE <new_value>.
              key = var-key.
              rel = if_salv_c_node_relation=>next_sibling.
              DELETE mt_vars WHERE name = is_var-name.
            ELSE.
              IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
                me->del_variable( CONV #( is_var-name )  ).
              ENDIF.
            ENDIF.
          ENDIF.

          IF <new_value> IS INITIAL AND m_hide IS NOT INITIAL.
            me->del_variable( CONV #( is_var-name ) ).
            RETURN.
          ENDIF.
        CATCH cx_root.
          me->del_variable( CONV #( is_var-name )  ).
      ENDTRY.
    ELSE.

      IF <new_value> IS INITIAL AND m_hide IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        key = leaf-key.
      ENDIF.
    ELSE.
      key = i_parent_key.
    ENDIF.

    READ TABLE mt_vars WITH KEY name = i_parent_calculated TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.

      tree-fullname = is_var-name.
      e_root_key =
        m_tree->get_nodes( )->add_node(
          related_node   = key
          relationship   = i_rel
          collapsed_icon = icon
          expanded_icon  = icon
          data_row       = tree
          text           = text
          folder         = abap_true
        )->get_key( ).

      APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
      <vars>-stack = mo_debugger->mo_window->mt_stack[ 1 ]-stacklevel.
      <vars>-leaf = m_leaf.
      <vars>-name = is_var-name.
      <vars>-program = mo_debugger->mo_window->m_prg-program.
      <vars>-eventtype = mo_debugger->mo_window->m_prg-eventtype.
      <vars>-eventname = mo_debugger->mo_window->m_prg-eventname.
      <vars>-short = is_var-short.
      <vars>-key = e_root_key.
      <vars>-ref = ir_up.
      <vars>-step = mo_debugger->m_step - mo_debugger->m_step_delta.
      <vars>-cl_leaf = is_var-cl_leaf.
      <vars>-path = is_var-path.

      IF rel = if_salv_c_node_relation=>next_sibling AND o_node IS NOT INITIAL.
        IF o_node IS NOT INITIAL.
          o_node->delete( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD add_node.

    main_node_key =
          m_tree->get_nodes( )->add_node(
            related_node   = ''
            collapsed_icon = i_icon
            expanded_icon = i_icon
            relationship   = if_salv_c_node_relation=>last_child
            row_style = if_salv_c_tree_style=>intensified
            text           = CONV #( i_name )
            folder         = abap_true
          )->get_key( ).

    CASE i_name.
      WHEN 'Locals'.
        m_locals_key = main_node_key.
      WHEN 'Globals'.
        m_globals_key = main_node_key.
      WHEN 'LDB'.
        m_ldb_key = main_node_key.

      WHEN 'Class-data global variables'.
        m_class_key = main_node_key.

      WHEN 'System variables'.
        m_syst_key = main_node_key.
    ENDCASE.

  ENDMETHOD.

  METHOD add_obj_nodes.

    DATA match TYPE match_result_tab.
    FIND ALL OCCURRENCES OF  '-' IN is_var-name RESULTS match. "Only first level of instance should be here
    IF lines( match ) > 1.
      RETURN.
    ENDIF.

    DATA text TYPE lvc_value.
    DATA node_key TYPE salv_de_node_key.
    DATA icon TYPE salv_de_tree_image.

    CASE is_var-cl_leaf.
      WHEN 1.
        icon = icon_led_green.
        text = 'Public'.
      WHEN 2.
        icon = icon_led_red.
        text = 'Private'.
      WHEN 3.
        icon = icon_led_yellow.
        text = 'Protected'.
    ENDCASE.

    READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf ASSIGNING FIELD-SYMBOL(<class>).
    IF sy-subrc NE 0.

      READ TABLE mt_vars WITH KEY path = is_var-parent INTO DATA(var).
      node_key =
        m_tree->get_nodes( )->add_node(
          related_node   = var-key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = icon
          expanded_icon  = icon
          text           = text
          folder         = abap_true
        )->get_key( ).

      APPEND INITIAL LINE TO mt_classes_leaf ASSIGNING <class>.
      <class>-name = is_var-parent.
      <class>-key = node_key.
      <class>-type = is_var-cl_leaf.
    ENDIF.

  ENDMETHOD.

  METHOD delete_node.

    DATA(o_nodes) = m_tree->get_nodes( ).
    DATA(o_node) =  o_nodes->get_node( i_key ).
    IF o_node IS NOT INITIAL.
      o_node->delete( ).

    ENDIF.

  ENDMETHOD.

  METHOD display.

    DATA(o_columns) = m_tree->get_columns( ).
    o_columns->get_column( 'KIND' )->set_visible( abap_false ).

    DATA(o_nodes) = m_tree->get_nodes( ).
    DATA(nodes) =  o_nodes->get_all_nodes( ).


    DATA sub TYPE salv_t_nodes.
    LOOP AT nodes INTO DATA(node).
      READ TABLE sub WITH KEY node = node-node TRANSPORTING NO FIELDS. "expanding only first level nodes.
      IF sy-subrc NE 0.
        TRY.
            node-node->expand( ).
            sub = node-node->get_subtree( ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.
    ENDLOOP.
    m_tree->display( ).

  ENDMETHOD.

  METHOD hndl_user_command.

    CONSTANTS: c_mask TYPE x VALUE '01'.

    CASE e_salv_function.

      WHEN 'REFRESH'."
        m_refresh = abap_true.
        mo_debugger->run_script_hist( mo_debugger->m_hist_step ).
        mo_debugger->mo_tree_local->display( ).
        RETURN.

      WHEN 'INITIALS'."Show/hide empty variables
        m_hide = m_hide BIT-XOR c_mask.
        m_clear = abap_true.

      WHEN 'LOCALS'."Show/hide locals variables
        m_locals = m_locals BIT-XOR c_mask.
        m_refresh = abap_true.
      WHEN 'GLOBALS'."Show/hide global variables
        m_globals = m_globals BIT-XOR c_mask.
        m_refresh = abap_true.
      WHEN 'SYST'."Show/hide sy structure
        m_syst = m_syst BIT-XOR c_mask.
        m_refresh = abap_true.
      WHEN 'CLASS_DATA'."Show/hide CLASS-DATA variables (globals)
        m_class_data = m_class_data BIT-XOR c_mask.

      WHEN 'LDB'."Show/hide LDB variables (globals)
        m_ldb = m_ldb BIT-XOR c_mask.
    ENDCASE.

    m_refresh = abap_true.
    mo_debugger->mo_tree_local->clear( ).
    mo_debugger->mo_tree_exp->clear( ).
    mo_debugger->mo_tree_imp->clear( ).

    "mo_debugger->run_script_hist( mo_debugger->m_hist_step ).
    "mo_debugger->mo_tree_local->display( ).

    "RETURN.

    mo_debugger->m_update = abap_true.

    mo_debugger->mo_tree_local->display( ).

    CLEAR mo_debugger->mo_window->m_debug_button.
    IF mo_debugger->m_hist_step = mo_debugger->m_step.
      CLEAR mo_debugger->is_history.
    ENDIF.
    IF e_salv_function NE 'TEST'.

      IF mo_debugger->is_history = abap_true.

        mo_debugger->run_script_hist( ).
      ELSE.
        mo_debugger->run_script( ).
        mo_debugger->hndl_script_buttons( mo_debugger->mv_stack_changed ).
      ENDIF.
      mo_debugger->show_step( ).
    ENDIF.

  ENDMETHOD.

  METHOD hndl_double_click.

    DATA(o_nodes) = m_tree->get_nodes( ).
    DATA(o_node) =  o_nodes->get_node( node_key ).
    DATA r_row TYPE REF TO data.

    r_row = o_node->get_data_row( ).
    ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT 'REF' OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
    ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
    ASSIGN COMPONENT 'FULLNAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<fullname>).
    ASSIGN COMPONENT 'PATH' OF STRUCTURE <row> TO FIELD-SYMBOL(<path>).

    IF <fullname> IS NOT INITIAL.
      READ TABLE mo_debugger->mt_selected_var WITH KEY name =  <fullname> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE mo_debugger->mt_selected_var WHERE name = <fullname>.
        o_node->set_row_style( if_salv_c_tree_style=>default ).
      ELSE.
        o_node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
        APPEND INITIAL LINE TO mo_debugger->mt_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
        <sel>-name = <fullname>.
        <sel>-is_sel = abap_true.
      ENDIF.

      CASE <kind>.
        WHEN cl_abap_datadescr=>typekind_table.
          lcl_appl=>open_int_table( i_name = <fullname> it_ref = <ref> io_window = mo_debugger->mo_window ).
        WHEN cl_abap_datadescr=>typekind_string.
          NEW lcl_text_viewer( <ref> ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD del_variable.

    IF mo_debugger->m_debug IS NOT INITIAL. BREAK-POINT. ENDIF.
    DATA(vars_hist) = mo_debugger->mt_vars_hist.
    SORT vars_hist BY step DESCENDING.
    LOOP AT vars_hist INTO DATA(hist) WHERE name = i_full_name.
      IF hist-del IS INITIAL.
        CLEAR: hist-ref, hist-first.
        hist-del = abap_true.
        hist-step = mo_debugger->m_hist_step - 1.
        INSERT hist INTO mo_debugger->mt_vars_hist INDEX 1.
      ENDIF.
    ENDLOOP.

    DATA(o_nodes) = m_tree->get_nodes( ).
    READ TABLE mo_debugger->mt_state WITH KEY name = i_full_name ASSIGNING FIELD-SYMBOL(<var>).
    IF sy-subrc = 0.

      TRY.
          DATA(o_node) =  o_nodes->get_node( <var>-key ).
        CATCH cx_salv_msg.
      ENDTRY.

      DELETE mt_vars WHERE name = i_full_name.
      DELETE mt_classes_leaf WHERE name = i_full_name.
      IF i_state = abap_true.
        DELETE mo_debugger->mt_state WHERE name = i_full_name.
      ENDIF.

      DATA(nam) = i_full_name && '-'.
      DELETE mt_vars WHERE name CS nam.
      DELETE mt_classes_leaf WHERE name  CS nam.
      IF i_state = abap_true.
        DELETE mo_debugger->mt_state WHERE name CS nam.
      ENDIF.
      TRY.
          IF o_node IS NOT INITIAL.
            o_node->delete( ).
          ENDIF.
        CATCH cx_salv_msg.
      ENDTRY.
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

    DATA: row          TYPE lcl_appl=>t_sel_row,
          set_receiver.

    LOOP AT lcl_appl=>mt_obj INTO DATA(lo).
      "to
      IF lo-alv_viewer->mo_sel IS BOUND.
        IF e_dragdropobj->droptargetctrl = lo-alv_viewer->mo_sel->mo_sel_alv.
          DATA(o_to) = lo-alv_viewer->mo_sel.
        ENDIF.
      ENDIF.

      "from tab
      IF lo-alv_viewer->mo_alv = e_dragdropobj->dragsourcectrl.
        DATA(o_from_tab) = lo-alv_viewer.
        CONTINUE.
      ENDIF.

      IF e_dragdropobj->dragsourcectrl = lo-alv_viewer->mo_sel->mo_sel_alv.
        DATA(o_from_sel) = lo-alv_viewer->mo_sel.
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_cells( IMPORTING et_cell = DATA(sel_cells) ).
      ENDIF.
    ENDLOOP.

    IF o_from_tab IS BOUND." tab to select
      FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                     <field> TYPE any.
      o_from_tab->mo_alv->get_selected_cells( IMPORTING et_cell = sel_cells ).
      o_from_tab->mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(sel_col) ).

      LOOP AT sel_col INTO DATA(col).
        TRY.
            o_from_tab->mt_alv_catalog[ fieldname = col-fieldname ]-style = cl_gui_alv_grid=>mc_style_button.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        READ TABLE o_from_tab->mo_column_emitters WITH KEY column = col ASSIGNING FIELD-SYMBOL(<emitter>).
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO o_from_tab->mo_column_emitters ASSIGNING <emitter>.
          <emitter>-column = col.
          <emitter>-emitter = NEW #( ).
        ENDIF.
      ENDLOOP.

      IF sy-subrc = 0.
        set_receiver = abap_true.
        CALL METHOD o_from_tab->mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = o_from_tab->mt_alv_catalog.
      ENDIF.

      TRY.
          ASSIGN o_from_tab->mr_table->* TO <table>.
          READ TABLE o_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to_tab>) INDEX e_row.
          LOOP AT sel_cells INTO DATA(cell).
            IF sy-tabix = 1.
              DATA(colname) = cell-col_id-fieldname.
            ENDIF.
            READ TABLE <table> INDEX cell-row_id ASSIGNING FIELD-SYMBOL(<str>).
            ASSIGN COMPONENT colname OF STRUCTURE <str> TO <field>.
            IF sy-subrc = 0.
              IF set_receiver IS NOT INITIAL.
                IF <to_tab>-receiver IS BOUND.
                  <to_tab>-receiver->shut_down( ).
                ENDIF.
                CREATE OBJECT <to_tab>-receiver
                  EXPORTING
                    io_transmitter = <emitter>-emitter
                    i_from_field   = CONV #( sel_cells[ 1 ]-col_id )
                    i_to_field     = <to_tab>-field_label
                    io_sel_to      = o_to
                    io_tab_from    = o_from_tab.
                SET HANDLER <to_tab>-receiver->on_grid_button_click FOR o_from_tab->mo_alv.
              ENDIF.

              IF <to_tab>-range IS INITIAL.
                <to_tab>-low = <field>.
              ENDIF.
              IF NOT line_exists( <to_tab>-range[ low = <field> ] ).
                APPEND VALUE #( sign = 'I' opti = 'EQ' low = <field>  ) TO <to_tab>-range.
              ENDIF.
            ENDIF.
          ENDLOOP.
          o_to->update_sel_row( CHANGING c_sel_row = <to_tab> ).
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
      ENDTRY.
    ENDIF.

    "select to select
    IF o_from_sel NE o_to.
      IF sel_rows[] IS INITIAL.
        DELETE sel_cells WHERE col_id NE 'FIELD_LABEL'.
        LOOP AT sel_cells INTO DATA(sel).
          APPEND INITIAL LINE TO sel_rows ASSIGNING FIELD-SYMBOL(<row>).
          <row>-index = sel-row_id-index.
        ENDLOOP.
      ENDIF.

      LOOP AT sel_rows ASSIGNING <row>.
        READ TABLE o_from_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<from_tab>) INDEX <row>-index.
        IF lines( sel_rows ) = 1.
          READ TABLE o_to->mt_sel_tab ASSIGNING <to_tab> INDEX e_row.
        ELSE.
          READ TABLE o_to->mt_sel_tab ASSIGNING <to_tab> WITH KEY field_label = <from_tab>-field_label.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING <from_tab> TO row.
        MOVE-CORRESPONDING row TO <to_tab>.
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
            io_sel_to      = o_to
            i_to_field     = <to_tab>-field_label.
      ENDLOOP.
    ENDIF.

    DATA(o_alv) = CAST cl_gui_alv_grid( e_dragdropobj->dragsourcectrl ).
    lcl_alv_common=>refresh( EXPORTING i_obj = o_alv ).

    o_alv ?= e_dragdropobj->droptargetctrl.
    o_to->raise_selection_done( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_source_parser IMPLEMENTATION.

  METHOD parse_tokens.

    DATA: lr_scan         TYPE REF TO cl_ci_scan,
          prev            TYPE string,
          change          TYPE string,
          split           TYPE TABLE OF string,
          o_scan          TYPE REF TO cl_ci_scan,
          o_statement     TYPE REF TO if_ci_kzn_statement_iterator,
          o_procedure     TYPE REF TO if_ci_kzn_statement_iterator,
          token           TYPE lcl_ace_window=>ts_kword,
          calculated_var  TYPE lcl_ace_window=>calculated_var,
          composed_var    TYPE lcl_ace_window=>composed_vars,
          tokens          TYPE lcl_ace_window=>tt_kword,
          calculated_vars TYPE  lcl_ace_window=>tt_calculated,
          composed        TYPE lcl_ace_window=>tt_composed,
          call            TYPE lcl_ace_window=>ts_calls,
          call_line       TYPE lcl_ace_window=>ts_calls_line,
          int_table       TYPE lcl_ace_window=>ts_int_tabs,
          int_tables      TYPE lcl_ace_window=>tt_tabs,
          eventtype       TYPE string,
          eventname       TYPE string,
          param           TYPE lcl_ace_window=>ts_params,
          par             TYPE char1,
          type            TYPE char1,
          class           TYPE xfeld,
          cl_name         TYPE string,
          preferred       TYPE xfeld.

    "CLEAR mv_step.

    READ TABLE io_debugger->mo_window->mt_source WITH KEY include = i_program INTO DATA(source).
    IF sy-subrc <> 0.

      source-source = cl_ci_source_include=>create( p_name = i_program ).
      o_scan = NEW cl_ci_scan( p_include = source-source ).

      source-include = i_program.

      o_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = o_scan ).
      o_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = o_scan ).

      TRY.
          o_statement->next( ).
        CATCH cx_scan_iterator_reached_end.
          EXIT.
      ENDTRY.

      DATA(kw) = o_statement->get_keyword( ).

      DATA(word) = o_statement->get_token( offset = 2 ).

      o_procedure->statement_index = o_statement->statement_index.
      o_procedure->statement_type = o_statement->statement_type.

      DATA(max) = lines( o_scan->statements ).
      DO.
        CLEAR token-tt_calls.
        TRY.
            o_procedure->next( ).
          CATCH cx_scan_iterator_reached_end.
        ENDTRY.

        kw = o_procedure->get_keyword( ).

        token-name = kw.
        token-index = o_procedure->statement_index.
        READ TABLE o_scan->statements INDEX o_procedure->statement_index INTO DATA(statement).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        READ TABLE o_scan->tokens INDEX statement-from INTO DATA(scan_token).
        token-line = calculated_var-line = composed_var-line = scan_token-row.

        DATA new TYPE xfeld.

        IF kw = 'CLASS'.
          class = abap_true.
        ENDIF.

        IF kw = 'FORM' OR kw = 'METHOD' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
          int_table-eventtype = eventtype = param-event =  kw.

          CLEAR eventname.
          IF kw = 'FORM'.
            CLEAR: class, param-class.
          ELSE.
            int_table-eventtype = eventtype = param-event =  'METHOD'.
          ENDIF.
        ENDIF.

        IF kw = 'ENDFORM' OR kw = 'ENDMETHOD'.
          CLEAR: eventtype, eventname, int_table.
          IF param-param IS INITIAL. "No params - save empty row if no params
            READ TABLE source-t_params WITH KEY event = param-event name = param-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              CLEAR param-type.
              APPEND param TO source-t_params.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR prev.
        IF kw = 'ASSIGN' OR kw = 'ADD' OR kw = 'SUBTRACT' .
          DATA(count) = 0.
        ENDIF.
        CLEAR: new, token-to_evname, token-to_evtype .


        WHILE 1 = 1.
          IF kw IS INITIAL.
            EXIT.
          ENDIF.
          CLEAR change.
          word = o_procedure->get_token( offset = sy-index ).

          IF ( word CS '(' AND ( NOT word CS ')' ) ) OR word CS '->' OR word CS '=>'."can be method call
            call-name = word.
            call-event = 'METHOD'.
            REPLACE ALL OCCURRENCES OF '(' IN call-name WITH ''.
            FIND FIRST OCCURRENCE OF '->' IN  call-name.
            IF sy-subrc = 0.
              SPLIT call-name  AT '->' INTO TABLE split.
              call-name = split[ 2 ].
            ENDIF.

            FIND FIRST OCCURRENCE OF '=>' IN  call-name.
            IF sy-subrc = 0.
              SPLIT call-name  AT '=>' INTO TABLE split.
              call-name = split[ 2 ].
            ENDIF.
            token-to_evname = call-name.
            token-to_evtype = call-event = 'METHOD'.
            IF new = abap_true.
              call-name =  token-to_evname = 'CONSTRUCTOR'.
            ENDIF.
          ENDIF.

          IF sy-index = 1 AND token-name = word.
            CONTINUE.
          ENDIF.

          IF sy-index = 2 AND ( kw = 'DATA' OR kw = 'PARAMETERS' ).
            "WRITE: 'var =', token.
            int_table-name = word.
          ENDIF.

          IF sy-index = 2 AND kw = 'PERFORM'.
            token-to_evname = call-name = word.
            token-to_evtype = call-event = 'FORM'.
          ENDIF.

          IF sy-index = 2 AND class = abap_true AND param-class IS INITIAL.
            call_line-class = param-class = word.
          ENDIF.

          IF sy-index = 2 AND eventtype IS NOT INITIAL AND eventname IS INITIAL.
            int_table-eventname = eventname = param-name = word.

            MOVE-CORRESPONDING int_table TO call_line.
            call_line-index = o_procedure->statement_index + 1.
            "methods in definition should be overwrited by Implementation section
            READ TABLE source-tt_calls_line WITH KEY eventname = call_line-eventname eventtype = call_line-eventtype ASSIGNING FIELD-SYMBOL(<call_line>).
            IF sy-subrc = 0.
              <call_line> = call_line.
            ELSE.
              APPEND call_line TO source-tt_calls_line.
            ENDIF.

          ENDIF.

          IF word = ''.
            CLEAR call.
            CASE kw.
              WHEN 'COMPUTE'.
                IF  NOT prev CO '0123456789.+-/* '.
                  composed_var-name = prev.
                  APPEND  composed_var TO composed.
                ENDIF.
              WHEN 'CLEAR' OR 'SORT' OR 'CONDENSE'."no logic
              WHEN 'FORM'.
                IF param-name IS NOT INITIAL.
                  APPEND param TO source-t_params.
                  CLEAR param.
                ENDIF.
            ENDCASE.
            EXIT.
          ENDIF.

          IF word = 'USING' OR word = 'IMPORTING'.
            param-type = 'I'.
            CLEAR: type, par.
          ELSEIF word = 'CHANGING' OR word = 'EXPORTING' OR word = 'RETURNING'.

            IF param-param IS NOT INITIAL.
              APPEND param TO source-t_params.
              CLEAR: type, par, param-param.
            ENDIF.

            param-type = 'E'.
            CLEAR: type, par.
          ELSEIF word = 'OPTIONAL' OR word = 'PREFERRED'.
            CONTINUE.
          ELSEIF word = 'PARAMETER'.
            preferred = abap_true.
            CONTINUE.
          ENDIF.

          IF preferred = abap_true.
            READ TABLE source-t_params WITH KEY event = 'METHOD' name = param-name param = word ASSIGNING FIELD-SYMBOL(<param>).
            IF sy-subrc = 0.
              <param>-preferred = abap_true.
            ENDIF.

            CLEAR preferred.
            CONTINUE.
          ENDIF.

          IF word <> 'CHANGING' AND word <> 'EXPORTING' AND word <> 'RETURNING' AND word <> 'IMPORTING' AND word <> 'USING'.
            IF kw = 'FORM' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
              IF par = abap_true AND type IS INITIAL AND word NE 'TYPE'.

                APPEND param TO source-t_params.
                CLEAR: par, param-param.
              ENDIF.

              IF par IS INITIAL AND sy-index > 3.
                param-param = word.
                par = abap_true.
                CONTINUE.
              ENDIF.
              IF par = abap_true AND type IS INITIAL AND word = 'TYPE'.
                type = abap_true.
                CONTINUE.
              ENDIF.
              IF par = abap_true AND type = abap_true.

                APPEND param TO source-t_params.
                CLEAR: type, par, param-param.
              ENDIF.
            ENDIF.
          ENDIF.

          DATA temp TYPE char30.
          temp = word.

          IF temp+0(5) = 'DATA('.
            SHIFT temp LEFT BY 5 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN temp WITH ''.
          ENDIF.

          IF temp+0(6) = '@DATA('.
            SHIFT temp LEFT BY 6 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN temp WITH ''.
          ENDIF.

          IF temp+0(13) = 'FIELD-SYMBOL('.
            SHIFT temp LEFT BY 13 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN temp WITH ''.
          ENDIF.

          IF word = 'NEW'.
            new = abap_true.
          ENDIF.

          FIND FIRST OCCURRENCE OF '->' IN word.
          IF sy-subrc = 0.
            CLEAR new.
          ENDIF.

          CASE kw.
            WHEN 'DATA' OR 'PARAMETERS'.
              IF (  prev = 'OF' ) AND temp <> 'TABLE' AND temp <> 'OF'.
                int_table-type = temp.
                APPEND int_table TO int_tables.
              ENDIF.

            WHEN 'COMPUTE'.
              IF temp CA '=' AND new IS INITIAL..
                change = prev.
              ENDIF.

              IF ( prev = '=' OR prev CA '+-/*' ) AND temp <> 'NEW'.
                IF NOT temp  CA '()' .
                  IF NOT temp  CO '0123456789. '.
                    composed_var-name = temp.
                    APPEND  composed_var TO composed.
                    IF call IS NOT INITIAL.
                      call-outer = temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'PERFORM' .

              IF  temp = 'USING' OR temp = 'CHANGING' .
                CLEAR prev.
              ENDIF.

              IF  prev = 'USING' OR prev = 'CHANGING' .

                IF NOT temp  CA '()' .
                  IF NOT temp  CO '0123456789. '.
                    call-outer = temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.
                    change = temp.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'CREATE' OR 'CALL'.
              DATA: import TYPE xfeld,
                    export.

              IF prev = 'FUNCTION' AND kw = 'CALL'.
                token-to_evtype =   call-event = 'FUNCTION'.
                token-to_evname =  call-name = word.
                REPLACE ALL OCCURRENCES OF '''' IN  token-to_evname WITH ''.
              ENDIF.

              IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
                export = abap_true.
                CLEAR import.
                CONTINUE.

              ELSEIF word = 'IMPORTING'.
                import = abap_true.
                CLEAR export.
                CONTINUE.

              ENDIF.

              IF prev = 'OBJECT'.
                "WRITE : 'value', temp.
*          CONTINUE.
              ENDIF.

              IF  prev = '='.
                IF NOT temp  CA '()'.
                  IF NOT temp  CO '0123456789. '.
                    IF import = abap_true.
                      call-outer = temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      calculated_var-name = temp.
                      APPEND  calculated_var TO calculated_vars.
                    ELSEIF export = abap_true.
                      call-outer = temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      composed_var-name = temp.
                      APPEND  composed_var TO composed.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
                IF NOT temp  CO '0123456789. ' AND temp <> '=' AND ( import = abap_true OR export = abap_true ).
                  call-inner = temp.
                ENDIF.
              ENDIF.

            WHEN 'CLEAR' OR 'SORT'.
              change = temp.
            WHEN  'CONDENSE'.

              IF temp <> 'NO-GAPS'.
                change = temp.
              ENDIF.
            WHEN 'ASSIGN' OR 'UNASSIGN'.
              ADD 1 TO count.
              IF count <> 2.
                change = temp.
              ENDIF.
            WHEN 'ADD' OR 'SUBTRACT'.
              ADD 1 TO count.
              IF count = 1.
                IF  NOT temp CO '0123456789.() '.
                  composed_var-name = temp.
                  APPEND  composed_var TO composed.
                ENDIF.
              ENDIF.
              IF count = 3.
                change = temp.
              ENDIF.
            WHEN 'READ'.
              IF prev =  'INTO' OR prev =  'ASSIGNING'.
                change = temp.
              ENDIF.

            WHEN 'SELECT'.
              IF  ( prev =  'INTO' OR prev =  '(' ) AND ( temp <> 'TABLE' AND temp <> '('  AND temp <> ')' AND  temp <> ',' ).
                change = temp.
              ENDIF.

            WHEN OTHERS.

          ENDCASE.
          IF call-event = 'METHOD'.
            IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
              export = abap_true.
              CLEAR import.
              CONTINUE.

            ELSEIF word = 'IMPORTING'.
              import = abap_true.
              CLEAR export.
              CONTINUE.
            ENDIF.

            IF  temp = 'USING' OR temp = 'CHANGING' .
              CLEAR prev.
            ENDIF.

            IF  prev = 'USING' OR prev = 'CHANGING' .

              IF NOT temp  CA '()' .
                IF NOT temp  CO '0123456789. '.
                  call-outer = temp.
                  READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND call TO token-tt_calls.
                  ENDIF.
                  change = temp.
                ENDIF.
              ENDIF.
            ENDIF.

            IF  prev = '='.
              IF NOT temp  CA '()'.
                IF NOT temp  CO '0123456789. '.
                  IF import = abap_true.
                    call-outer = temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.

                    calculated_var-name = temp.
                    APPEND  calculated_var TO calculated_vars.
                  ELSEIF export = abap_true.
                    call-outer = temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.
                    composed_var-name = temp.
                    APPEND  composed_var TO composed.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              IF NOT temp  CO '0123456789. ' AND temp <> '=' AND ( import = abap_true OR export = abap_true ).
                call-inner = temp.
              ENDIF.
            ENDIF.

          ENDIF.

          IF temp = '(' .
            prev = temp.
            CONTINUE.
          ENDIF.

          IF  NOT temp  CA '()'.
            IF temp <> 'TABLE' AND temp <> 'NEW'  AND prev <> '('.
              IF  kw <> 'PERFORM'.
                prev = temp.
              ELSEIF word = 'USING' OR word = 'CHANGING'.
                prev = temp.
              ENDIF.
            ENDIF.
          ENDIF.

          IF change IS NOT INITIAL.
            calculated_var-name = change.
            APPEND calculated_var TO calculated_vars.

            IF change+0(1) = '<'.

              SPLIT change AT '-' INTO TABLE split.
              change = split[ 1 ].
              IF eventtype IS INITIAL. "Global fs
                READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = i_program ASSIGNING FIELD-SYMBOL(<globals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
                  <globals_set>-program = i_program.
                ENDIF.
                READ TABLE  <globals_set>-mt_fs WITH KEY name = change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO  <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
                  <gl_fs>-name = change.
                ENDIF.

              ELSE."local fs
                READ TABLE io_debugger->mo_window->mt_locals_set
                 WITH KEY program = i_program eventtype = eventtype eventname = eventname
                 ASSIGNING FIELD-SYMBOL(<locals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
                  <locals_set>-program = i_program.
                  <locals_set>-eventname = eventname.
                  <locals_set>-eventtype = eventtype.
                ENDIF.
                READ TABLE <locals_set>-mt_fs WITH KEY name = change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
                  <loc_fs>-name = change.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDWHILE.
        token-from = statement-from.
        token-to = statement-to.
        APPEND token TO tokens.
        IF o_procedure->statement_index = max.
          EXIT.
        ENDIF.

      ENDDO.

      "Fill keyword links for perform

      LOOP AT tokens ASSIGNING FIELD-SYMBOL(<s_token>) WHERE tt_calls IS NOT INITIAL.

        READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
        DATA(index) = 0.
        LOOP AT source-t_params INTO param WHERE event = call-event AND name = call-name .
          ADD 1 TO index.
          READ TABLE <s_token>-tt_calls INDEX index ASSIGNING FIELD-SYMBOL(<call>).
          IF sy-subrc = 0.
            <call>-inner = param-param.
            IF param-type = 'I'.
              <call>-type = '>'.
            ELSE.
              <call>-type = '<'.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      "clear value(var) to var.
      LOOP AT source-t_params ASSIGNING <param>.
        REPLACE ALL OCCURRENCES OF 'VALUE(' IN <param>-param WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN <param>-param WITH ''.
      ENDLOOP.

      source-scan = o_scan.
      source-t_keytokens = tokens.
      source-t_calculated = calculated_vars.
      source-t_composed = composed.
      source-tt_tabs = int_tables.
      APPEND source TO io_debugger->mo_window->mt_source.

    ENDIF.

  ENDMETHOD.


ENDCLASS.

CLASS lcl_mermaid IMPLEMENTATION.

  METHOD constructor.

    DATA text TYPE text100.

    super->constructor( ).

    mo_debugger = io_debugger.
    mv_type = i_type.

    CHECK lcl_appl=>is_mermaid_active = abap_true.

    CASE mv_type.
      WHEN 'DIAG'.
        text = 'Calls flow'.
      WHEN 'SMART'.
        text = 'Calculations sequence'.
    ENDCASE.

    IF mo_box IS INITIAL.
      mo_box = create( i_name = text i_width = 1000 i_hight = 300 ).
      "save new popup ref
      APPEND INITIAL LINE TO lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
      <popup>-parent = mo_debugger->mo_window->mo_box.
      <popup>-child = mo_box.

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 2
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->get_container(
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_mm_container ).

      mo_splitter->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_mm_toolbar ).

      mo_splitter->set_row_height( id = 1 height = '3' ).
      mo_splitter->set_row_height( id = 2 height = '70' ).

      mo_splitter->set_row_sash( id    = 1
                                 type  = 0
                                 value = 0 ).

      CREATE OBJECT mo_toolbar EXPORTING parent = mo_mm_toolbar.
      add_toolbar_buttons( ).
      mo_toolbar->set_visible( 'X' ).
    ENDIF.
    CASE mv_type.
      WHEN 'DIAG'.
        steps_flow( ).
      WHEN 'SMART'.
        magic_search( ).
    ENDCASE.

  ENDMETHOD.

  METHOD steps_flow.

    TYPES: BEGIN OF lty_entity,
             event TYPE string,
             name  TYPE string,
           END OF lty_entity,
           BEGIN OF t_ind,
             from TYPE i,
             to   TYPE i,
           END OF t_ind  .

    DATA: mm_string TYPE string,
          name      TYPE string,
          entities  TYPE TABLE OF lty_entity,
          entity    TYPE lty_entity,
*          ind1      TYPE i,
*          ind2      TYPE i,
          parts     TYPE TABLE OF string,
          step      LIKE LINE OF mo_debugger->mt_steps,
          ind       TYPE t_ind,
          indexes   TYPE TABLE OF t_ind.


    DATA(copy) = mo_debugger->mt_steps.

    LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
      IF <copy>-eventtype = 'METHOD'.
        SPLIT <copy>-program AT '=' INTO TABLE parts.
        <copy>-eventname = entity-name = |"{ parts[ 1 ] }->{ <copy>-eventname }"|.
        entity-event = <copy>-eventtype.

      ELSEIF <copy>-eventtype = 'FUNCTION'.
        <copy>-eventname = entity-name = |"{ <copy>-eventtype }:{ <copy>-eventname }"|.
      ELSE.
        <copy>-eventname = entity-name = |"{ <copy>-program }:{ <copy>-eventname }"|.
      ENDIF.

      COLLECT entity INTO entities.
    ENDLOOP.

    CLEAR step.

    IF i_direction IS INITIAL.
      mm_string = |graph TD\n |.
    ELSE.
      mm_string = |graph { i_direction }\n |.
    ENDIF.

    LOOP AT copy INTO DATA(step2).
      IF step IS INITIAL.
        step = step2.
        CONTINUE.
      ENDIF.
      IF step2-stacklevel > step-stacklevel.

        READ TABLE entities WITH KEY name = step-eventname TRANSPORTING NO FIELDS.
        ind-from = sy-tabix.
        READ TABLE entities WITH KEY name = step2-eventname TRANSPORTING NO FIELDS.
        ind-to = sy-tabix.
        READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          mm_string = |{ mm_string }{ ind-from }({ step-eventname }) --> { ind-to }({ step2-eventname })\n|.
          APPEND ind TO indexes.
        ENDIF.
      ENDIF.
      step = step2.
    ENDLOOP.

    open_mermaid( mm_string ).

  ENDMETHOD.

  METHOD magic_search.

    DATA: add         TYPE xfeld,
          mm_string   TYPE string,
          sub         TYPE string,
          form        TYPE string,
          direction   TYPE string,
          box_s       TYPE string,
          box_e       TYPE string,
          ind2        TYPE i,
          start       TYPE i,
          end         TYPE i,
          bool        TYPE string,
          block_first TYPE i,
          els_before  TYPE i.

    TYPES: BEGIN OF ts_line,
             cond       TYPE string,
             include    TYPE string,
             line       TYPE i,
             ind        TYPE i,
             event      TYPE string,
             stack      TYPE i,
             code       TYPE string,
             arrow      TYPE string,
             subname    TYPE string,
             del        TYPE flag,
             els_before TYPE i,
             els_after  TYPE i,
           END OF ts_line.

    DATA: line      TYPE ts_line,
          lines     TYPE STANDARD TABLE OF ts_line,
          pre_stack TYPE ts_line,
          opened    TYPE i.

    CLEAR mo_debugger->mo_window->mt_watch.

    IF lines( mo_debugger->mt_steps ) > 1.
      DATA(steps) = mo_debugger->mt_steps.
    ELSE.

      code_execution_scanner( ).
      steps = mt_steps.
    ENDIF.

    DATA: yes TYPE xfeld.
    LOOP AT steps INTO DATA(step).
      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = step-include INTO DATA(source).
      READ TABLE source-t_keytokens WITH KEY line = step-line INTO DATA(keyword).
      LOOP AT keyword-tt_calls INTO DATA(call).

        READ TABLE mo_debugger->mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          yes = abap_true.
        ENDIF.

        READ TABLE mo_debugger->mt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          yes = abap_true.
        ENDIF.
      ENDLOOP.
      IF yes = abap_true.
        LOOP AT keyword-tt_calls INTO call.
          READ TABLE mo_debugger->mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
            <selected>-name = call-outer.
          ENDIF.

          READ TABLE mo_debugger->mt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
            <selected>-name = call-inner.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    "deleting empty cycles.
    DATA: prev    LIKE LINE OF steps,
          pre_key TYPE string.

    READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = step-include INTO source.

    LOOP AT steps ASSIGNING FIELD-SYMBOL(<step>).
      DATA(ind) = sy-tabix.
      READ TABLE source-t_keytokens WITH KEY line = <step>-line INTO DATA(key).
      IF prev IS NOT INITIAL.
        IF ( key-name = 'ENDDO' OR key-name = 'ENDWHILE' OR key-name = 'ENDLOOP' OR key-name = 'ENDIF' )  AND
           ( pre_key = 'DO' OR pre_key = 'LOOP'  OR pre_key = 'WHILE'  OR pre_key = 'IF' ).
          <step>-first = 'D'."to delete
          READ TABLE steps INDEX ind - 1 ASSIGNING FIELD-SYMBOL(<step_prev>).
          <step_prev>-first = 'D'.
        ENDIF.
      ENDIF.
      prev = <step>.
      pre_key = key-name.
    ENDLOOP.

    DELETE steps WHERE first = 'D'.

    SORT steps BY step DESCENDING.

    "collecting dependents variables
    LOOP AT steps INTO step.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = step-include INTO source.

      LOOP AT source-t_calculated INTO DATA(calculated_var) WHERE line = step-line.
        READ TABLE mo_debugger->mt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
*          APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
*          <selected>-name = calculated_var-name.

          LOOP AT source-t_composed INTO DATA(composed_var) WHERE line = step-line.
            READ TABLE mo_debugger->mt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
              <selected>-name = composed_var-name.
            ENDIF.
          ENDLOOP.
        ENDIF.
        "adding returning values
*        LOOP AT source-t_params INTO DATA(param).
*          READ TABLE mo_debugger->mt_selected_var WITH KEY name = param-param TRANSPORTING NO FIELDS.
*          IF sy-subrc <> 0.
*            APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
*            <selected>-name = param-param.
*          ENDIF.
*        ENDLOOP.
      ENDLOOP.

      READ TABLE source-t_keytokens WITH KEY line = step-line INTO keyword.
      LOOP AT keyword-tt_calls INTO call.

        READ TABLE mo_debugger->mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
          <selected>-name = call-inner.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
    SORT mo_debugger->mt_selected_var.
    DELETE ADJACENT DUPLICATES FROM mo_debugger->mt_selected_var.

    "collecting watchpoints
    CLEAR mo_debugger->mo_window->mt_coverage.

    LOOP AT  steps INTO step.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = step-include INTO source.
      READ TABLE source-t_keytokens WITH KEY line = step-line INTO key.

      CLEAR line-cond.
      IF key-name = 'IF' OR key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ELSEIF' OR
         key-name = 'CASE' OR key-name = 'WHEN' OR key-name = 'ENDCASE' OR
          key-name = 'DO' OR key-name = 'ENDDO'  OR key-name = 'LOOP'  OR key-name = 'ENDLOOP' OR key-name = 'WHILE' OR key-name = 'ENDWHILE'.
        APPEND INITIAL LINE TO mo_debugger->mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).

        <watch>-program = step-program.
        <watch>-line = line-line = step-line.

        INSERT line INTO lines INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-cond = key-name.
        <line>-event = step-eventname.
        <line>-stack = step-stacklevel.
        <line>-include = step-include.
      ENDIF.
      CLEAR ind.
      LOOP AT  source-t_calculated INTO calculated_var WHERE line = step-line.
        ADD 1 TO ind.
        LOOP AT source-t_composed INTO composed_var WHERE line = step-line.
          READ TABLE mo_debugger->mt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO  mo_debugger->mt_selected_var ASSIGNING <selected>.
            <selected>-name = composed_var-name.
          ENDIF.
        ENDLOOP.

        READ TABLE mo_debugger->mt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.

          APPEND INITIAL LINE TO mo_debugger->mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = step-program.
          <watch>-line = line-line = step-line.

          "should be commented for Smart debugger
*          LOOP AT lines ASSIGNING <line> WHERE line = line-line AND event = step-eventname AND stack = step-stacklevel .
*            <line>-del = abap_true.
*          ENDLOOP.
          IF ind = 1.
            line-event = step-eventname.
            line-stack = step-stacklevel.
            line-include = step-include.
            INSERT line INTO lines INDEX 1.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    DELETE lines WHERE del = abap_true.

    "getting code texts and calls params
    LOOP AT lines ASSIGNING <line>.
      ind = sy-tabix.

      READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = <line>-include INTO source.
      READ TABLE source-t_keytokens WITH KEY line = <line>-line INTO keyword.
      LOOP AT source-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
        IF token-str = 'USING' OR token-str = 'EXPORTING' OR token-str = 'IMPORTING' OR token-str = 'CHANGING'.
          EXIT.
        ENDIF.
        IF <line>-code IS INITIAL.
          <line>-code = token-str.
        ELSE.
          <line>-code = |{  <line>-code } { token-str }|.
        ENDIF.
      ENDLOOP.



      IF keyword-to_evname IS NOT INITIAL.
        SORT keyword-tt_calls BY outer.
        DELETE ADJACENT DUPLICATES FROM keyword-tt_calls.
        LOOP AT keyword-tt_calls INTO call.
          IF sy-tabix <> 1.
            <line>-arrow = |{ <line>-arrow }, |.
          ENDIF.
          <line>-arrow  = |{ <line>-arrow  } { call-outer } { call-type } { call-inner }|.
          <line>-subname = call-name.
          REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
          REPLACE ALL OCCURRENCES OF '"' IN  <line>-code WITH ''.
        ENDLOOP.
      ENDIF.
      REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
    ENDLOOP.

    "check subform execution steps existance and if/case structures build

    DATA: if_depth   TYPE i,
          when_count TYPE i.
    LOOP AT lines ASSIGNING <line> WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP' .
      <line>-ind = sy-tabix.

      FIELD-SYMBOLS: <if> TYPE ts_if.
      IF <line>-cond = 'IF' OR  <line>-cond = 'CASE'.
        ADD 1 TO if_depth.
        CLEAR when_count.
        APPEND INITIAL LINE TO mt_if  ASSIGNING <if>.
        <if>-if_ind = <line>-ind.

      ENDIF.

      IF <line>-cond = 'ENDIF' OR <line>-cond = 'ENDCASE'.
        <if>-end_ind = <line>-ind.
        SUBTRACT 1 FROM if_depth.
        LOOP AT mt_if  ASSIGNING <if> WHERE end_ind = 0.
        ENDLOOP.
      ENDIF.

      IF <line>-cond = 'WHEN'.
        ADD 1 TO when_count.
      ENDIF.

      IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.

        <line>-els_before = els_before.
        <line>-els_after = <line>-ind.
        DATA(counter) = <line>-ind + 1.
        DO.
          READ TABLE lines INDEX counter INTO line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after = counter.
            EXIT.
          ELSE.
            ADD 1 TO counter.

          ENDIF.
        ENDDO.
        IF when_count = 1. "to refactor
*          <if>-if_ind = els_before.
*          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond = 'WHEN'.

        <line>-els_before = els_before.
        <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE lines INDEX counter INTO line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF line-cond = 'WHEN'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after = counter.
            EXIT.
          ELSE.
            ADD 1 TO counter.

          ENDIF.
        ENDDO.
        IF when_count = 1.
          <if>-if_ind = els_before.
          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        els_before = <line>-ind.
      ELSE.
        CLEAR   els_before.
      ENDIF.

      READ TABLE lines WITH KEY event = <line>-subname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR <line>-arrow.
      ENDIF.
    ENDLOOP.

    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.

    IF lines( lines ) > 0.
      IF lines[ lines( lines ) ]-arrow IS NOT INITIAL.
        CLEAR lines[ lines( lines ) ]-arrow .
      ENDIF.
    ENDIF.

    "creating mermaid code
    CHECK lines IS NOT INITIAL.

    IF i_direction IS INITIAL.
      IF lines( lines ) < 100.
        direction = 'LR'.
      ELSE.
        direction = 'TB'.
      ENDIF.
    ELSE.
      direction = i_direction.
    ENDIF.

    mm_string = |graph { direction }\n |.

    LOOP AT lines INTO line WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND  cond <> 'WHEN'.

      ind = sy-tabix.

      IF line-cond IS INITIAL.
        box_s = '('.
        box_e = ')'.
      ELSE.
        box_s = '{'.
        box_e = '}'.
      ENDIF.

      IF pre_stack IS INITIAL.
        pre_stack = line.
      ENDIF.

      IF ( pre_stack-stack > line-stack OR pre_stack-event <> line-event ) AND opened > 0 AND sub IS INITIAL.
        IF pre_stack-stack = line-stack AND pre_stack-event <> line-event.
          DATA(times) = 1.
        ELSE.
          times = pre_stack-stack - line-stack.
        ENDIF.

        DO times TIMES.
          mm_string = |{ mm_string } end\n|.
          SUBTRACT 1 FROM opened.
          IF opened = 0.
            EXIT.
          ENDIF.
        ENDDO.

      ENDIF.
      DATA: name TYPE string.
      IF    line-cond = 'LOOP' OR line-cond = 'DO' OR line-cond = 'WHILE' OR line-arrow IS NOT INITIAL .
        IF line-arrow IS NOT INITIAL.
          mm_string = |{ mm_string }{ ind }{ box_s }"{ line-code }"{ box_e }\n|.
          pre_stack = line.

        ENDIF.

        "IF strlen( line-code ) > 50.
        "lv_name = line-code+0(50).
        "ELSE.
        name = line-code.
        "ENDIF.
        REPLACE ALL OCCURRENCES OF `PERFORM` IN name WITH `FORM` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN name WITH `FUNCTION` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL METHOD` IN name WITH `METHOD` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `<` IN name WITH `` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF '>' IN name WITH `` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `-` IN name WITH `~` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF ` ` IN name WITH `&nbsp;` IN CHARACTER MODE.

        mm_string = |{ mm_string } subgraph S{ ind }["{ name }"]\n  direction { direction }\n|.
        ADD 1 TO opened.
        start = ind.
        CONTINUE.
      ENDIF.

      IF line-cond = 'ENDLOOP' OR line-cond = 'ENDDO' OR line-cond = 'ENDWHILE'.
        SUBTRACT 1 FROM opened.
        mm_string = |{ mm_string } end\n|.
        CONTINUE.
      ENDIF.

      mm_string = |{ mm_string }{ ind }{ box_s }"{ line-code }"{ box_e }\n|.
      pre_stack = line.

    ENDLOOP.

    DO opened TIMES.
      mm_string = |{ mm_string } end\n|.
      SUBTRACT 1 FROM opened.
    ENDDO.

    DATA: if_ind TYPE i.
    CLEAR pre_stack.
    LOOP AT lines INTO line WHERE cond <> 'LOOP' AND cond <> 'DO' AND cond <> 'WHILE' AND cond <> 'ENDLOOP' AND cond <> 'ENDDO' AND cond <> 'ENDWHILE'.

      IF line-cond = 'IF' OR line-cond = 'CASE' .
        ADD 1 TO if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.


      IF pre_stack IS INITIAL.
        IF line-cond = 'WHEN' OR line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
          pre_stack = lines[ <if>-if_ind ].
        ELSE.
          pre_stack = line.

          IF line-arrow IS NOT INITIAL.
            sub = '|"' && line-arrow && '"|'.
          ELSE.
            CLEAR sub.
          ENDIF.

          CONTINUE.
        ENDIF.

      ENDIF.

      IF line-cond = 'ELSE' OR line-cond = 'ELSEIF' OR line-cond = 'WHEN'.
        bool = '|' && line-code && '|'.
        IF line-els_after IS NOT INITIAL.
          mm_string = |{ mm_string }{ ms_if-if_ind }-->{ bool }{ line-els_after }\n|.
          DATA(diff) = ms_if-end_ind - line-els_after.
          DATA(last_els) = line-els_after.
          IF line-cond <> 'WHEN' AND line-cond <> 'ELSEIF'  AND  diff > 1 AND line-els_after <> ms_if-end_ind.
            mm_string = |{ mm_string }{  line-els_after }-->{ ms_if-end_ind }\n|.
          ENDIF.
        ELSE.
          mm_string = |{ mm_string }{ ms_if-if_ind }-->{ bool }{ ms_if-end_ind }\n|.
        ENDIF.

        IF line-els_before IS NOT INITIAL AND line-els_before <> ms_if-if_ind.
          mm_string = |{ mm_string }{ line-els_before }-->{ ms_if-end_ind }\n|.
        ENDIF.

        IF lines[ line-ind + 1 ]-cond <> 'ENDIF' AND lines[ line-ind + 1 ]-cond <> 'ENDCASE'.
          CLEAR pre_stack.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF   pre_stack-cond NE 'ELSE' AND pre_stack-cond NE 'ELSEIF' AND pre_stack-cond NE 'WHEN' AND NOT ( last_els = line-ind ).

        mm_string = |{ mm_string }{ pre_stack-ind }-->{ sub }{ line-ind }\n|.

        IF line-arrow IS NOT INITIAL.
          sub = '|"' && line-arrow && '"|'.
        ELSE.
          CLEAR sub.
        ENDIF.

      ENDIF.

      pre_stack = line.

      IF line-cond = 'ENDIF' OR line-cond = 'ENDCASE'.
        DELETE mt_if INDEX if_ind.
        SUBTRACT 1 FROM if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.

    ENDLOOP.

    open_mermaid( mm_string ).
  ENDMETHOD.

  METHOD parse_call.
    DATA: statement TYPE i,
          stack     TYPE i.

    stack = i_stack + 1.
    statement = i_index.
    READ TABLE mo_debugger->mo_window->mt_source WITH KEY include = i_program INTO DATA(source).
    DO.
      READ TABLE source-t_keytokens WITH KEY index =  statement INTO DATA(key).
      IF key-name = 'DATA'.
        ADD 1 TO statement.
        CONTINUE.
      ENDIF.
      ADD 1 TO mv_step.
      APPEND INITIAL LINE TO mt_steps ASSIGNING FIELD-SYMBOL(<step>).

      <step>-step = mv_step.
      <step>-line = key-line.
      <step>-eventname = i_event.
      <step>-eventtype = 'FORM'.
      <step>-stacklevel = stack.
      <step>-program = i_program.
      <step>-include = i_program.

      IF key-to_evname IS NOT INITIAL.
        READ TABLE source-tt_calls_line WITH KEY eventname = key-to_evname eventtype = key-to_evtype INTO DATA(call_line).

        parse_call( EXPORTING i_index = call_line-index
                                                 i_event = call_line-eventname
                                                 i_program = i_program
                                                 i_stack   = stack
                                                  ).

      ENDIF.

      IF key-name = 'ENDFORM' OR key-name = 'ENDMETHOD'.
        RETURN.
      ENDIF.

      ADD 1 TO statement.
    ENDDO.
  ENDMETHOD.


  METHOD code_execution_scanner.
    "code execution scanner
    DATA: max       TYPE i,
          step      TYPE i,
          call_line TYPE lcl_ace_window=>ts_calls_line.

    READ TABLE mo_debugger->mo_window->mt_source INDEX 1 INTO DATA(source).

    DATA: structure LIKE source-scan->structures.

    READ TABLE source-scan->structures WITH KEY type = 'E' TRANSPORTING  NO FIELDS.
    IF sy-subrc = 0.
      structure = source-scan->structures.
      DELETE structure WHERE type <> 'E'.
      SORT structure BY stmnt_type ASCENDING.
    ELSE.
      CLEAR max.
      LOOP AT source-scan->structures INTO DATA(str) WHERE type <> 'P' AND type <> 'C' .
        IF max < str-stmnt_to.
          max = str-stmnt_to.
          APPEND str TO structure.
        ENDIF.
      ENDLOOP.
    ENDIF.


    DATA: event     TYPE string,
          stack     TYPE i VALUE 1,
          statement TYPE i.

    LOOP AT structure INTO str.

      READ TABLE source-t_keytokens WITH KEY index =  statement INTO DATA(key).

      IF str-type = 'E'.
        statement = str-stmnt_from + 1.
        event = key-name.
      ELSE.
        statement = str-stmnt_from.
      ENDIF.

      WHILE statement <= str-stmnt_to.
        READ TABLE source-t_keytokens WITH KEY index =  statement INTO key.

        IF key-name = 'DATA' OR key-name = 'CONSTANTS' OR sy-subrc <> 0.
          ADD 1 TO statement.
          CONTINUE.
        ENDIF.
        ADD 1 TO step.
        APPEND INITIAL LINE TO mt_steps ASSIGNING FIELD-SYMBOL(<step>).

        <step>-step = step.
        <step>-line = key-line.
        <step>-eventname = event.
        <step>-eventtype = 'EVENT'.
        <step>-stacklevel = stack.
        <step>-program = source-include.
        <step>-include = source-include.

        IF key-to_evname IS NOT INITIAL.
          READ TABLE source-tt_calls_line WITH KEY eventname = key-to_evname eventtype = key-to_evtype INTO call_line.

          parse_call( EXPORTING i_index = call_line-index
                                           i_event = call_line-eventname
                                           i_program = source-include
                                           i_stack   = stack
                                            ).
        ENDIF.

        ADD 1 TO statement.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_toolbar_buttons.

    DATA: button TYPE ttb_button,
          events TYPE cntl_simple_events,
          event  LIKE LINE OF events.

    button  = VALUE #(
     ( function = 'TD' icon = CONV #( icon_view_expand_vertical ) quickinfo = 'Vertical' text = '' )
     ( function = 'LR' icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
     ( butn_type = 3  )
     ( function = 'TEXT' icon = CONV #( icon_wd_caption ) quickinfo = 'Mermaid Diagram text' text = '' )
                    ).

    mo_toolbar->add_button_group( button ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  ENDMETHOD.

  METHOD hnd_toolbar.


    IF fcode = 'TEXT'.
      DATA: mm_string TYPE string,
            ref       TYPE REF TO data.
      mm_string = mo_diagram->('GET_SOURCE_CODE_STRING').
      GET REFERENCE OF mm_string INTO ref.
      NEW lcl_text_viewer( ref ).

      RETURN.
    ENDIF.

    CASE mv_type.
      WHEN 'DIAG'.
        steps_flow( fcode ).
      WHEN 'SMART'.
        magic_search( fcode ).

    ENDCASE.

  ENDMETHOD.

  METHOD open_mermaid.

    CHECK lcl_appl=>is_mermaid_active = abap_true.

    TRY.
        IF mo_diagram IS INITIAL.
          CREATE OBJECT mo_diagram TYPE ('ZCL_WD_GUI_MERMAID_JS_DIAGRAM') EXPORTING parent = mo_mm_container.
        ENDIF.
        CALL METHOD mo_diagram->('SET_SOURCE_CODE_STRING') EXPORTING source_code = i_mm_string.
        CALL METHOD mo_diagram->('DISPLAY').

      CATCH cx_root INTO DATA(error).
        MESSAGE error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
