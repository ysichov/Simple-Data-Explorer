*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Simple Data Explorer v.2 SelecTor
*&---------------------------------------------------------------------*
*& version: beta 2.0.0.77
*& GIT:            https://github.com/ysichov/SDE/blob/master/SDE_750.abap - here may be most actual version
*& AbapGit         https://github.com/ysichov/SDE_abapgit
*& RU description  https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/
*& EN description  https://blogs.sap.com/2020/03/22/simple-data-explorer/

*& Multi-windows program for viewing tables, views, salary clusters, CDS and some links between them
*& Written by Yurii Sychov
*& e-mail:   ysichov@gmail.com
*& blog:     https://ysychov.wordpress.com/blog/
*& LinkedIn: https://www.linkedin.com/in/ysychov/

*&---------------------------------------------------------------------*
*& External resources
*& https://github.com/bizhuka/eui - ALV listboxes

REPORT z_sde_standalone.

*------------REPORT EVENTS--------------------
INTERFACE zif_sde_pivot_types DEFERRED.
CLASS zcl_sde_transmitter DEFINITION DEFERRED.
CLASS zcl_sde_tools DEFINITION DEFERRED.
CLASS zcl_sde_text_viewer DEFINITION DEFERRED.
CLASS zcl_sde_table_viewer DEFINITION DEFERRED.
CLASS zcl_sde_sql DEFINITION DEFERRED.
CLASS zcl_sde_sel_opt DEFINITION DEFERRED.
CLASS zcl_sde_rtti DEFINITION DEFERRED.
CLASS zcl_sde_receiver DEFINITION DEFERRED.
CLASS zcl_sde_py_cluster_viewer DEFINITION DEFERRED.
CLASS zcl_sde_popup DEFINITION DEFERRED.
CLASS zcl_sde_plugins DEFINITION DEFERRED.
CLASS zcl_sde_pivot DEFINITION DEFERRED.
CLASS zcl_sde_dragdrop DEFINITION DEFERRED.
CLASS zcl_sde_dd_data DEFINITION DEFERRED.
CLASS zcl_sde_ddic DEFINITION DEFERRED.
CLASS zcl_sde_common DEFINITION DEFERRED.
CLASS zcl_sde_appl DEFINITION DEFERRED.
INTERFACE zif_sde_pivot_types.

  "Shared types between zcl_sde_tools (join field list) and zcl_sde_pivot
  "(pivot column value list) - each class needs the other's type, so a
  "direct cross-reference would create a circular type dependency that
  "cannot be resolved when both classes are flattened into one standalone
  "program (see generate_standalone.sh).

  TYPES: BEGIN OF t_jfld,
           sel       TYPE abap_bool,
           pos       TYPE i, "order in the SELECT list (only when sel = X)
           alias     TYPE char5,
           tabname   TYPE tabname,
           fieldname TYPE fieldname,
           keyflag   TYPE keyflag,
           ddtext    TYPE reptext,
           datatype  TYPE c LENGTH 30,
           inttype   TYPE c LENGTH 1,
         END OF t_jfld,
         tt_jfld TYPE STANDARD TABLE OF t_jfld WITH DEFAULT KEY,

         BEGIN OF t_colval,
           text    TYPE string, "display value
           literal TYPE string, "SQL literal ('USD' or 42), single-column compatibility
           cond    TYPE string, "SQL condition for multi-column buckets
         END OF t_colval,
         tt_colvals TYPE STANDARD TABLE OF t_colval WITH DEFAULT KEY.

ENDINTERFACE.

CLASS zcl_sde_appl DEFINITION CREATE PUBLIC.
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
        transmitter TYPE REF TO zcl_sde_transmitter,
        receiver    TYPE REF TO zcl_sde_receiver,
        color       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
        drop_down   TYPE int4,
      END OF selection_display_s,
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

           BEGIN OF t_obj,
             alv_viewer TYPE REF TO zcl_sde_table_viewer,
           END OF t_obj,

           BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang  .

    CLASS-DATA: m_option_icons TYPE TABLE OF sign_option_icon_s,
                mt_lang        TYPE TABLE OF t_lang,
                mt_obj         TYPE TABLE OF t_obj, "main object table
                c_dragdropalv  TYPE REF TO cl_dragdrop.

    CLASS-DATA: mt_sel TYPE TABLE OF selection_display_s.

    "Mirrors of the report's selection-screen state, kept in sync by Z_SDE_STANDALONE
    "so that global classes (which cannot see report-local data) can read them.
    CLASS-DATA: gv_rows         TYPE i,
                gv_vname        TYPE tabname,
                gr_current_row TYPE REF TO data.

    CLASS-METHODS:
      init_icons_table,
      init_lang,
      suppress_run_button,
      open_int_table IMPORTING it_tab  TYPE ANY TABLE OPTIONAL
                               it_ref  TYPE REF TO data OPTIONAL
                               iv_name TYPE string,
      exit.
ENDCLASS.
CLASS zcl_sde_common DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS: c_white(4) TYPE x VALUE '00000001', "white background
               c_grey(4)  TYPE x VALUE '00000003', "gray background
               c_green(4) TYPE x VALUE '00000216', "green +underline
               c_blue(4)  TYPE x VALUE '00000209', " blue font +underline
               c_bold(4)  TYPE x VALUE '00000020'.
    TYPES: BEGIN OF t_tabfields.
             INCLUDE TYPE   dfies.
    TYPES:   empty   TYPE boolean,
             is_text TYPE boolean,
           END OF t_tabfields.

    CLASS-DATA: mt_tabfields TYPE HASHED TABLE OF t_tabfields WITH UNIQUE KEY tabname fieldname.

    CLASS-METHODS:
      refresh IMPORTING i_obj TYPE REF TO cl_gui_alv_grid i_layout TYPE lvc_s_layo OPTIONAL i_soft TYPE char01 OPTIONAL,
      translate_field IMPORTING i_lang TYPE ddlanguage OPTIONAL CHANGING c_fld TYPE lvc_s_fcat,
      get_selected IMPORTING i_obj TYPE REF TO cl_gui_alv_grid RETURNING VALUE(e_index) TYPE i.

ENDCLASS.
CLASS zcl_sde_ddic DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS: get_text_table IMPORTING i_tname TYPE tabname
                                  EXPORTING e_tab   TYPE tabname.
ENDCLASS.
CLASS zcl_sde_dd_data DEFINITION CREATE PUBLIC. "drag&drop data
  PUBLIC  SECTION.
    DATA: m_row    TYPE i,
          m_column TYPE lvc_s_col.
ENDCLASS.
CLASS zcl_sde_dragdrop DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS:
      drag FOR EVENT ondrag OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row e_column ,
      drop FOR EVENT ondrop OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row.
ENDCLASS.
CLASS zcl_sde_pivot DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_val,
             key TYPE string, "ALIAS~FIELD
             agg TYPE string, "SUM/COUNT/MIN/MAX/AVG
           END OF t_val,
           tt_vals TYPE STANDARD TABLE OF t_val WITH DEFAULT KEY,
           tt_keys TYPE STANDARD TABLE OF string WITH DEFAULT KEY,

           tt_colvals TYPE zif_sde_pivot_types=>tt_colvals. "shared with zcl_sde_tools, see zif_sde_pivot_types

    METHODS:
      has_layout RETURNING VALUE(rv_has) TYPE abap_bool,
      has_columns RETURNING VALUE(rv_has) TYPE abap_bool,
      get_col_key RETURNING VALUE(rv_key) TYPE string,
      get_col_keys RETURNING VALUE(rt_keys) TYPE tt_keys,

      "HTML for the fields panel: zones + available field chips (SAPEVENT:pv?...)
      render_panel IMPORTING it_fields      TYPE zif_sde_pivot_types=>tt_jfld
                             i_show_texts   TYPE abap_bool DEFAULT abap_false
                   RETURNING VALUE(rv_html) TYPE string,

      "pv actions: pk_<key> pick, tr rows, tc columns, ag_<AGG> values,
      "rr_<key>/rc_<key>/rv_<idx> remove, CLR clear all
      handle_action IMPORTING i_act TYPE string,
      normalize_aggs IMPORTING it_fields TYPE zif_sde_pivot_types=>tt_jfld,

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
CLASS zcl_sde_plugins DEFINITION CREATE PUBLIC.
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
                mt_el_links    TYPE  TABLE OF t_el_links.
    CLASS-METHODS: init,
      link IMPORTING i_str     TYPE any
                     io_viewer TYPE REF TO zcl_sde_table_viewer
                     i_column  TYPE any,
      run_pa20 IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_pp01 IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_text IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_subty IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_py_cluster IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_dictionary_key IMPORTING i_str     TYPE any
                                   io_viewer TYPE REF TO zcl_sde_table_viewer
                                   i_column  TYPE any,
      run_data_element IMPORTING i_str          TYPE any
                                 io_viewer      TYPE REF TO zcl_sde_table_viewer
                                 i_column       TYPE any
                       RETURNING VALUE(is_done) TYPE boolean,
      run_field_2_field IMPORTING i_str          TYPE any
                                  io_viewer      TYPE REF TO zcl_sde_table_viewer
                                  i_column       TYPE any
                        RETURNING VALUE(is_done) TYPE boolean,
      run_field_2_plugin IMPORTING "i_str          TYPE any
                                   io_viewer      TYPE REF TO zcl_sde_table_viewer
                                   i_column       TYPE any
                         RETURNING VALUE(is_done) TYPE boolean,

      run_hrp1001_adatanr IMPORTING i_str          TYPE any
                                    io_viewer      TYPE REF TO zcl_sde_table_viewer
                                    i_column       TYPE any
                          RETURNING VALUE(is_done) TYPE boolean,
      run_hrpy_rgdir IMPORTING i_str          TYPE any.
ENDCLASS.
CLASS zcl_sde_popup DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-DATA m_counter TYPE i.
    DATA: mo_box            TYPE REF TO cl_gui_dialogbox_container,
          mo_splitter       TYPE REF TO cl_gui_splitter_container,
          mo_parent         TYPE REF TO cl_gui_container,
          m_additional_name TYPE string.

    METHODS: constructor IMPORTING i_additional_name TYPE string OPTIONAL,
      create IMPORTING i_width       TYPE i
                       i_hight       TYPE i
                       i_name        TYPE text100 OPTIONAL
             RETURNING VALUE(ro_box) TYPE REF TO cl_gui_dialogbox_container,

      on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.
ENDCLASS.
CLASS zcl_sde_py_cluster_viewer DEFINITION INHERITING FROM zcl_sde_popup CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_hier,
        anynode   TYPE string,
        anyparent TYPE string,
        key       TYPE salv_de_node_key, "internal tree key
        name      TYPE string,
        tab_ref   TYPE REF TO data,
        type(1),
      END OF ts_hier,
      tt_hier TYPE TABLE OF ts_hier,
      BEGIN OF t_children,
        item TYPE REF TO zcl_sde_table_viewer,
      END OF t_children.

    DATA: mt_hier     TYPE tt_hier, " Tree hierarchy
          mo_nodes    TYPE REF TO cl_salv_nodes,
          mo_node     TYPE REF TO cl_salv_node,
          mo_events   TYPE REF TO cl_salv_events_tree,
          mt_empty    TYPE tt_hier,
          mr_cluster  TYPE REF TO data, "payru_result,
          m_pernr(8)  TYPE n,
          m_seqnr(5)  TYPE n,
          mt_children TYPE TABLE OF t_children.

    DATA :  mo_tree  TYPE REF TO cl_salv_tree.
    METHODS: constructor IMPORTING i_pernr TYPE any i_seqnr TYPE any,
      show_tree,
      on_box_close  REDEFINITION .

  PRIVATE SECTION.
    METHODS: init_alv_tree,
      create_tree,
      create_hierarchy,
      read_cluster,
      hndl_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key.
ENDCLASS.
CLASS zcl_sde_transmitter DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    EVENTS: data_changed EXPORTING VALUE(e_row) TYPE zcl_sde_appl=>t_sel_row,
      col_changed EXPORTING VALUE(e_column) TYPE lvc_fname.
    METHODS: emit IMPORTING e_row TYPE zcl_sde_appl=>t_sel_row,
      emit_col IMPORTING e_column TYPE lvc_fname.
ENDCLASS.
CLASS zcl_sde_receiver DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    DATA: mo_transmitter TYPE REF TO zcl_sde_transmitter,
          lo_tab_from    TYPE REF TO zcl_sde_table_viewer,
          lo_sel_to      TYPE REF TO zcl_sde_sel_opt,
          m_from_field   TYPE lvc_fname,
          m_to_field     TYPE lvc_fname.
    METHODS: constructor
      IMPORTING io_transmitter TYPE REF TO zcl_sde_transmitter OPTIONAL
                io_tab_from    TYPE REF TO zcl_sde_table_viewer OPTIONAL
                io_sel_to      TYPE REF TO zcl_sde_sel_opt OPTIONAL
                i_from_field   TYPE lvc_fname OPTIONAL
                i_to_field     TYPE lvc_fname OPTIONAL,
      shut_down,
      update FOR EVENT data_changed OF zcl_sde_transmitter IMPORTING e_row,
      update_col FOR EVENT col_changed OF zcl_sde_transmitter IMPORTING e_column,
      on_grid_button_click
        FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no.
ENDCLASS.
CLASS zcl_sde_rtti DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_ui_type,
        " Simple UI types
        char     TYPE string VALUE 'char',
        numc     TYPE string VALUE 'numc',
        numeric  TYPE string VALUE 'numeric',
        boolean  TYPE string VALUE 'boolean',
        date     TYPE string VALUE 'date',
        time     TYPE string VALUE 'time',
        datetime TYPE string VALUE 'datetime',
        " Complext UI types
        string   TYPE string VALUE 'string',
        range    TYPE string VALUE 'range',
        table    TYPE string VALUE 'table',
      END OF mc_ui_type .

    TYPES: BEGIN OF ts_field_desc,
             name(61),
             sys_type     TYPE abap_typekind, " SYSTEM
             ui_type      TYPE string, " Only for KIND = P
             length       TYPE i,             " Only for KIND = P
             decimals     TYPE i,             " Only for KIND = P
             " For editing in ALV
             rollname(61),
             label        TYPE dfies-fieldtext,
             " Table description
             table_kind   TYPE abap_tablekind,
             unique       TYPE abap_bool,
             key          TYPE abap_keydescr_tab,
             key_defkind  TYPE abap_keydefkind,
             sub_fdesc    TYPE string,
           END OF ts_field_desc .

    TYPES:
      tt_field_desc TYPE HASHED TABLE OF ts_field_desc WITH UNIQUE KEY name .
    TYPES:
      tt_unique_type TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line .

    CLASS-METHODS:
      create_table_by_name IMPORTING i_tname TYPE tabname
                           CHANGING  c_table TYPE REF TO data,

      create_struc_handle IMPORTING i_tname  TYPE tabname
                          EXPORTING e_t_comp TYPE abap_component_tab
                                    e_handle TYPE REF TO cl_abap_structdescr,

      create_structure IMPORTING io_range         TYPE REF TO cl_abap_datadescr OPTIONAL
                                 iv_sub_fdesc     TYPE string OPTIONAL
                                 it_field_desc    TYPE tt_field_desc OPTIONAL
                       RETURNING VALUE(ro_struct) TYPE REF TO cl_abap_structdescr,

      find_table_fieldname
        IMPORTING
          ir_unique_type TYPE REF TO tt_unique_type
        CHANGING
          cv_rollname    TYPE csequence
          cv_label       TYPE csequence OPTIONAL,

      get_field_desc
       IMPORTING
        iv_field_name TYPE csequence OPTIONAL
        iv_data TYPE any OPTIONAL
        is_sh_field TYPE dfies OPTIONAL
        ir_unique_type TYPE REF TO tt_unique_type OPTIONAL
        RETURNING VALUE(rs_field_desc) TYPE ts_field_desc,

      create_type_descr IMPORTING iv_rollname    TYPE csequence OPTIONAL
                                  is_field_desc  TYPE ts_field_desc OPTIONAL
                                  VALUE(ir_type) TYPE REF TO data OPTIONAL
                        RETURNING
                                  VALUE(ro_type) TYPE REF TO cl_abap_datadescr,

      find_drop_down IMPORTING io_grid TYPE REF TO cl_gui_alv_grid
                      CHANGING cs_fieldcat TYPE lvc_s_fcat cv_drdn_hndl TYPE i,

      is_list_box IMPORTING iv_tabname TYPE dfies-tabname iv_fieldname  TYPE dfies-fieldname
                  EXPORTING ev_list_box  TYPE abap_bool es_sh_desc  TYPE shlp_descr.
ENDCLASS.
CLASS zcl_sde_sel_opt DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    DATA: mo_viewer  TYPE REF TO zcl_sde_table_viewer,
          mo_sel_alv TYPE REF TO cl_gui_alv_grid,
          mt_fcat    TYPE lvc_t_fcat,
          mt_sel_tab TYPE TABLE OF zcl_sde_appl=>selection_display_s,
          ms_layout  TYPE lvc_s_layo.

    EVENTS: selection_done.
    METHODS:
      constructor IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer io_container TYPE REF TO cl_gui_container,
      raise_selection_done,
      update_sel_tab,
      set_value IMPORTING  i_field TYPE any i_low TYPE any OPTIONAL i_high TYPE any OPTIONAL i_clear TYPE boolean DEFAULT abap_true ,
      update_sel_row CHANGING c_sel_row TYPE zcl_sde_appl=>selection_display_s.

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
CLASS zcl_sde_sql DEFINITION CREATE PUBLIC.
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
CLASS zcl_sde_table_viewer DEFINITION INHERITING FROM zcl_sde_popup CREATE PUBLIC.
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
CLASS zcl_sde_text_viewer DEFINITION FINAL INHERITING FROM zcl_sde_popup CREATE PUBLIC.
  PUBLIC SECTION.
    DATA: mo_text     TYPE REF TO cl_gui_textedit.

    METHODS: constructor IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      load_text  IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer.

ENDCLASS.
CLASS zcl_sde_tools DEFINITION INHERITING FROM zcl_sde_popup CREATE PUBLIC.
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
                                   io_parent TYPE REF TO cl_gui_container OPTIONAL. "docked mode: build inside this container

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
          m_mode       TYPE char1,                      "active tool: ' ' none, J join, P pivot
          mo_pivot     TYPE REF TO zcl_sde_pivot,
          m_pick       TYPE string,                     "click-to-move: picked field key or table alias
          m_sql_edit   TYPE abap_bool,                  "SQL panel in manual edit mode
          m_sql_manual TYPE string,                     "manually edited statement
          m_show_texts TYPE abap_bool,                   "field chips: texts instead of tech names
          m_fld_lang   TYPE spras,                       "language of the field texts
          mt_where_sel TYPE TABLE OF zcl_sde_appl=>selection_display_s. "ranges before pivot rebind

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
      get_pivot_col_values RETURNING VALUE(rt_vals) TYPE zif_sde_pivot_types=>tt_colvals,
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
CLASS zcl_sde_transmitter IMPLEMENTATION.
  METHOD  emit.
    RAISE EVENT data_changed EXPORTING e_row = e_row.
  ENDMETHOD.

  METHOD emit_col.
    RAISE EVENT col_changed EXPORTING e_column = e_column.
  ENDMETHOD.
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
                      ddtext = ls_f-fieldtext datatype = ls_f-datatype inttype = ls_f-inttype ) TO mt_jflds.
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
                        ddtext = ls_f-fieldtext datatype = ls_f-datatype inttype = ls_f-inttype
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
      |<span class="base">{ m_tabname }</span>| &&
      |<a class="card{ COND string( WHEN m_mode = 'J' THEN ' sel' ) }" href="SAPEVENT:mode?J">&#128279; Join</a>| &&
      |<a class="card{ COND string( WHEN m_mode = 'P' THEN ' sel' ) }" href="SAPEVENT:mode?P">&#8862; Pivot table</a>|.

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

    IF m_mode = 'P' AND mo_pivot IS BOUND. "pivot zones instead of the SELECT list
      "the pivot source is what the join selected, not every field of every table
      DATA(lt_src) = VALUE tt_jfld( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
      SORT lt_src BY pos.
      mo_pivot->normalize_aggs( lt_src ).
      show_html( io_html = mo_flds_html
                 i_html  = mo_pivot->render_panel( it_fields = lt_src i_show_texts = m_show_texts ) ).
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
      WHEN 'pv'. "pivot zone actions
        IF mo_pivot IS BOUND.
          mo_pivot->handle_action( CONV #( getdata ) ).
          render_flds( ).
          update_sql_view( ).
        ENDIF.
      WHEN 'pvdrop'. "field dragged onto a zone: mv=dr_/dc_/dv_<key>
        IF mo_pivot IS BOUND.
          DATA(l_drop_post) = concat_lines_of( table = postdata ).
          SPLIT l_drop_post AT '=' INTO DATA(l_drop_name) DATA(l_drop).
          REPLACE ALL OCCURRENCES OF '+' IN l_drop WITH ` `.
          l_drop = cl_http_utility=>unescape_url( l_drop ).
          IF l_drop IS NOT INITIAL.
            mo_pivot->handle_action( l_drop ).
            render_flds( ).
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
            mo_pivot->handle_action( |sa_{ l_agg_idx }_{ l_agg_new }| ).
            render_flds( ).
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
      DATA(lt_src) = VALUE tt_jfld( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
      SORT lt_src BY pos.
      mo_pivot->normalize_aggs( lt_src ).
      l_sql = mo_pivot->build_select( i_from      = build_from( )
                                      i_where     = build_where( )
                                      i_multi     = is_multi( )
                                      i_rows      = COND #( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 )
                                      it_col_vals = get_pivot_col_values( ) ).
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

    l_from = build_from( ).

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
  METHOD is_multi.
    rv_multi = boolc( lines( mt_jtabs ) > 1 ).
  ENDMETHOD.
  METHOD cache_where_selection.
    CHECK mo_viewer->mo_sel IS BOUND.

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
  METHOD get_pivot_col_values.
    TYPES: BEGIN OF t_colmeta,
             key   TYPE string,
             sql   TYPE string,
             comp  TYPE string,
             field TYPE fieldname,
           END OF t_colmeta.

    FIELD-SYMBOLS: <vals> TYPE STANDARD TABLE,
                   <row>  TYPE any,
                   <v>    TYPE any.

    DATA: lt_cols TYPE STANDARD TABLE OF string,
          lt_meta TYPE STANDARD TABLE OF t_colmeta,
          lt_comp TYPE abap_component_tab,
          lr_vals TYPE REF TO data.

    CHECK mo_pivot IS BOUND AND mo_pivot->has_columns( ) = abap_true.
    DATA(lt_keys) = mo_pivot->get_col_keys( ).
    CHECK lt_keys IS NOT INITIAL.

    DATA(l_col_idx) = 0.
    LOOP AT lt_keys INTO DATA(l_key).
      SPLIT l_key AT '~' INTO DATA(l_alias) DATA(l_field).
      DATA(l_alias_up) = to_upper( l_alias ).
      DATA(l_field_up) = to_upper( l_field ).
      READ TABLE mt_jtabs INTO DATA(ls_tab) WITH KEY alias = l_alias_up.
      CHECK sy-subrc = 0.

      ADD 1 TO l_col_idx.
      DATA(l_comp) = |C{ l_col_idx }|.
      DATA(l_col) = COND string( WHEN is_multi( ) = abap_true
                                 THEN |{ l_alias_up }~{ l_field_up }|
                                 ELSE |{ l_field_up }| ).

      DATA lo_td TYPE REF TO cl_abap_typedescr.
      DATA lo_type TYPE REF TO cl_abap_datadescr.
      cl_abap_typedescr=>describe_by_name(
        EXPORTING  p_name         = |{ ls_tab-tabname }-{ l_field_up }|
        RECEIVING  p_descr_ref    = lo_td
        EXCEPTIONS type_not_found = 1 OTHERS = 2 ).
      IF sy-subrc NE 0 OR lo_td IS NOT BOUND.
        RETURN.
      ENDIF.
      TRY.
          lo_type = CAST cl_abap_datadescr( lo_td ).
        CATCH cx_sy_move_cast_error.
          RETURN.
      ENDTRY.

      APPEND l_col TO lt_cols.
      APPEND VALUE #( name = l_comp type = lo_type ) TO lt_comp.
      APPEND VALUE #( key = l_key sql = l_col comp = l_comp field = l_field_up ) TO lt_meta.
    ENDLOOP.

    CHECK lt_comp IS NOT INITIAL.

    TRY.
        DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
        DATA(lo_tab) = cl_abap_tabledescr=>create(
                         p_line_type  = lo_struct
                         p_table_kind = cl_abap_tabledescr=>tablekind_std
                         p_unique     = abap_false ).
        CREATE DATA lr_vals TYPE HANDLE lo_tab.
      CATCH cx_root.
        RETURN.
    ENDTRY.
    ASSIGN lr_vals->* TO <vals>.

    DATA(l_from) = to_upper( build_from( ) ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_from WITH ` `.
    DATA(l_where) = upper_outside_quotes( build_where( ) ).

    TRY.
        IF l_where IS INITIAL.
          SELECT DISTINCT (lt_cols)
            FROM (l_from)
            INTO TABLE @<vals>
            UP TO 50 ROWS.
        ELSE.
          SELECT DISTINCT (lt_cols)
            FROM (l_from)
            WHERE (l_where)
            INTO TABLE @<vals>
            UP TO 50 ROWS.
        ENDIF.
      CATCH cx_root.
        RETURN.
    ENDTRY.
    SORT <vals>.

    "SQL literal: always a quoted character literal - ABAP SQL converts it to the
    "column type itself, while unquoted decimals ('185.00') are a syntax error
    LOOP AT <vals> ASSIGNING <row>.
      DATA: l_text TYPE string,
            l_cond TYPE string,
            l_lit  TYPE string.
      CLEAR: l_text, l_cond.

      LOOP AT lt_meta INTO DATA(ls_meta).
        ASSIGN COMPONENT ls_meta-comp OF STRUCTURE <row> TO <v>.
        CHECK sy-subrc = 0.

        DATA l_txt TYPE string.
        l_txt = <v>. "plain assignment: NUMC/DATS/decimals keep their raw form
        CONDENSE l_txt.
        l_lit = l_txt.
        REPLACE ALL OCCURRENCES OF `'` IN l_lit WITH `''`.
        l_lit = COND #( WHEN l_lit IS INITIAL THEN `' '` ELSE |'{ l_lit }'| ).

        IF l_text IS NOT INITIAL.
          l_text = |{ l_text }/|.
          l_cond = |{ l_cond } AND |.
        ENDIF.
        l_text = |{ l_text }{ COND string( WHEN l_txt IS INITIAL THEN '(empty)' ELSE l_txt ) }|.
        IF l_txt IS INITIAL.
          l_cond = |{ l_cond }( { ls_meta-sql } = { l_lit } OR { ls_meta-sql } IS NULL )|.
        ELSE.
          l_cond = |{ l_cond }{ ls_meta-sql } = { l_lit }|.
        ENDIF.
      ENDLOOP.

      IF NOT line_exists( rt_vals[ text = l_text ] ).
        APPEND VALUE #( text    = l_text
                        literal = COND #( WHEN lines( lt_meta ) = 1 THEN l_lit )
                        cond    = l_cond ) TO rt_vals.
      ENDIF.
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

    CHECK mo_viewer->mo_sel IS BOUND.
    DATA(l_multi) = boolc( lines( mt_jtabs ) > 1 ).

    lt_sel = mt_where_sel.
    IF lt_sel IS INITIAL.
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

    TRY.
        DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
        DATA(lo_tab) = cl_abap_tabledescr=>create(
                         p_line_type  = lo_struct
                         p_table_kind = cl_abap_tabledescr=>tablekind_std
                         p_unique     = abap_false ).
        CREATE DATA lr_table TYPE HANDLE lo_tab.
        ASSIGN lr_table->* TO <result>.

        "new strict-mode Open SQL: dynamic tokens with AS aliases are supported here
        IF l_group IS INITIAL.
          SELECT (l_fields)
            FROM (l_from)
            WHERE (l_where)
            INTO CORRESPONDING FIELDS OF TABLE @<result>
            UP TO @l_rows ROWS.
        ELSE.
          SELECT (l_fields)
            FROM (l_from)
            WHERE (l_where)
            GROUP BY (l_group)
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
    IF mo_viewer->mo_sel IS BOUND.
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

CLASS zcl_sde_text_viewer IMPLEMENTATION.
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
       container = mo_parent ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_text
      EXPORTING
        parent                 = mo_parent
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

    load_text( io_viewer ).
  ENDMETHOD.

  METHOD load_text."only for HR systems
*    DATA: lr_pskey TYPE REF TO data,
*          lr_text  TYPE REF TO data.
*
*    FIELD-SYMBOLS: <text_tab> TYPE STANDARD TABLE,
*                   <pskey>    TYPE any.
*    CREATE DATA lr_text TYPE ('HRPAD_TEXT_TAB'). "HANDLE lo_handle.
*    ASSIGN lr_text->* TO <text_tab>.
*
*    CREATE DATA lr_pskey TYPE ('PSKEY'). "HANDLE lo_handle.
*    ASSIGN lr_pskey->* TO <pskey>.
*
*    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
*    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).
*    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
*    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<row>).
*    MOVE-CORRESPONDING <row> TO <pskey>.
*    ASSIGN COMPONENT 'INFTY' OF STRUCTURE <pskey> TO FIELD-SYMBOL(<field>).
*    <field> = io_viewer->m_tabname+2(4).
*
*    TRY.
*        CALL METHOD cl_hrpa_text_cluster=>read
*          EXPORTING
*            tclas         = 'A'
*            pskey         = <pskey>
*            no_auth_check = abap_true
*          IMPORTING
*            text_tab      = <text_tab>.
*      CATCH cx_hrpa_missing_authorization .
*      CATCH cx_hrpa_violated_assertion .
*    ENDTRY.
*
*    mo_text->set_text_as_r3table( <text_tab> ).
*    CALL METHOD cl_gui_cfw=>flush.
*    mo_text->set_focus( mo_box ).
  ENDMETHOD.

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

CLASS zcl_sde_sql IMPLEMENTATION.
  METHOD read_any_table.
    FIELD-SYMBOLS: <f_tab> TYPE ANY TABLE.

    ASSIGN cr_tab->* TO <f_tab>.
    c_count = lines( <f_tab> ).
    CHECK zcl_sde_sql=>exist_table( i_tabname ) = 1.
    IF i_where IS NOT INITIAL.
      TRY.
          SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF TABLE <f_tab>  UP TO i_row_count ROWS  WHERE (i_where)  ORDER BY PRIMARY KEY.

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

CLASS zcl_sde_sel_opt IMPLEMENTATION.
  METHOD constructor.
    DATA: effect     TYPE i,
          handle_alv TYPE i.

    mo_viewer = io_viewer.
    mo_sel_alv = NEW #( i_parent = io_container ).
    CREATE OBJECT zcl_sde_appl=>c_dragdropalv.
    effect =  cl_dragdrop=>copy. " + cl_dragdrop=>move.

    CALL METHOD zcl_sde_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line'
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD zcl_sde_appl=>c_dragdropalv->get_handle IMPORTING handle = handle_alv.
    ms_layout-s_dragdrop-col_ddid = handle_alv.
    init_fcat( handle_alv ).
    update_sel_tab( ).

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
                zcl_sde_dragdrop=>drag
                zcl_sde_dragdrop=>drop
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
     ( fieldname = 'LOW'         coltext = 'From data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4 col_opt = abap_true drdn_field = 'DROP_DOWN' )
     ( fieldname = 'HIGH'        coltext = 'To data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4  col_opt = abap_true drdn_field = 'DROP_DOWN' )
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
    Zcl_SDE_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    DATA: ls_row TYPE zcl_sde_appl=>t_sel_row.
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
      READ TABLE Zcl_SDE_common=>mt_tabfields WITH KEY tabname = l_catalog-tabname fieldname = l_catalog-fieldname  INTO DATA(l_tfield).
      IF l_tfield-empty = '' OR mo_viewer->m_show_empty IS NOT INITIAL OR mo_viewer->m_count = 0.
        APPEND INITIAL LINE TO mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel_tab>).
        READ TABLE lt_copy INTO DATA(ls_copy) WITH KEY field_label = l_catalog-fieldname.
        IF sy-subrc NE 0.
          "the join builder renames base fields CARRID <-> T0_CARRID: keep the entered ranges
          DATA l_alt TYPE lvc_fname.
          FIND REGEX '^T\d+_' IN l_catalog-fieldname MATCH LENGTH DATA(l_pfx_len).
          IF sy-subrc = 0.
            l_alt = l_catalog-fieldname+l_pfx_len.
          ELSE.
            l_alt = |T0_{ l_catalog-fieldname }|.
          ENDIF.
          READ TABLE lt_copy INTO ls_copy WITH KEY field_label = l_alt.
        ENDIF.
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

        Zcl_SDE_common=>translate_field( EXPORTING i_lang = mo_viewer->m_lang CHANGING c_fld = l_catalog ).
        <sel_tab>-name = l_catalog-scrtext_l.

        IF l_tfield-keyflag = abap_true.
          <sel_tab>-style = VALUE #( ( fieldname = 'FIELD_LABEL' style = '00000020' ) ).
        ENDIF.
        IF l_tfield-empty = abap_true.
          <sel_tab>-color = VALUE #( ( fname = 'FIELD_LABEL' color-col = 2 color-int = 0 color-inv = 1 ) ).
        ELSE.
          <sel_tab>-color = VALUE #( ( fname = 'FIELD_LABEL' color-col = 4 color-int = 0 color-inv = 1 ) ).
        ENDIF.

        DATA: lv_drop_new TYPE i,
              lv_drop     TYPE i.
        l_catalog-ref_table = mo_viewer->m_tabname.
        l_catalog-ref_field = l_catalog-fieldname.

        zcl_sde_rtti=>find_drop_down(
         EXPORTING
          io_grid      = mo_sel_alv
         CHANGING
          cs_fieldcat  = l_catalog
          cv_drdn_hndl =  lv_drop ).

        IF lv_drop_new NE lv_drop.
          lv_drop_new = <sel_tab>-drop_down = lv_drop.
        ENDIF.
      ENDIF.
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
      DATA: ls_row TYPE zcl_sde_appl=>t_sel_row.
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

    IF c_sel_row-low CA  '*%+&' AND c_sel_row-opti <> 'NP'.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'CP'.
    ENDIF.

    IF c_sel_row-opti IS NOT INITIAL AND c_sel_row-sign IS INITIAL.
      c_sel_row-sign = 'I'.
    ENDIF.

    TRY.
        c_sel_row-option_icon = zcl_sde_appl=>m_option_icons[ sign = c_sel_row-sign option = c_sel_row-opti ]-icon_name.
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

      " Get and execute domain conversion routine - by https://github.com/Koch013
*      IF c_sel_row-domain IS NOT INITIAL.
*        DATA ls_dd01v TYPE dd01v.
*
*        CALL FUNCTION 'DDIF_DOMA_GET'
*          EXPORTING
*            name          = CONV ddobjname( c_sel_row-domain )
*          IMPORTING
*            dd01v_wa      = ls_dd01v
*          EXCEPTIONS
*            illegal_input = 1
*            OTHERS        = 2.
*
*        IF sy-subrc = 0 AND ls_dd01v-convexit IS NOT INITIAL. "AND ls_dd01v-convexit <> 'ALPHA'.
*          DO 2 TIMES.
*            ASSIGN COMPONENT COND string( WHEN sy-index = 1 THEN 'LOW' ELSE 'HIGH'  ) OF STRUCTURE <range> TO <field>.
*            IF <field> IS INITIAL.
*              CONTINUE.
*            ENDIF.
*
*            <field> =  |{ <field> ALPHA = IN }|.
*
*            DATA(length) = strlen( <field> ).
*            IF length > ls_dd01v-leng.
*              DATA(shift) = length - ls_dd01v-leng.
*              <field> = <field>+shift(ls_dd01v-leng).
*            ENDIF.
*
*          ENDDO.
*        ENDIF.
*      ENDIF." c_sel_row-domain IS NOT INITIAL.

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
          l_multiple TYPE boolean,
          l_clear    TYPE boolean.

    IF e_fieldname = 'LOW'.
      l_multiple = abap_true.
    ENDIF.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
    DATA(l_fname) =  <sel>-field_label.

    zcl_sde_appl=>mt_sel[] = mt_sel_tab[].
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
        LOOP AT return_tab ASSIGNING FIELD-SYMBOL(<ret>) WHERE retfield = |{ mo_viewer->m_tabname }-{ l_fname }|.
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
    DATA: l_start TYPE i.
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

*      IF l_cat-convexit = 'ALPHA' AND NOT  <ls_cells>-value CA '+*'.
*        <ls_cells>-value = |{ <ls_cells>-value ALPHA = IN }|.
*        l_start = 128 - l_cat-dd_outlen.
*        <ls_cells>-value = <ls_cells>-value+l_start(l_cat-dd_outlen).
*      ENDIF.

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
          DATA: lv_time TYPE sy-uzeit.
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
    "no refresh/selection_done here: refresh_table_display inside the data_changed
    "callback is forbidden in S/4 (CX_SALV_METHOD_NOT_SUPPORTED dump);
    "both happen in on_data_changed_finished right afterwards
  ENDMETHOD.

  METHOD on_data_changed_finished.
    CHECK e_modified IS NOT INITIAL.
    Zcl_SDE_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout ).
    RAISE EVENT selection_done.
  ENDMETHOD.

  METHOD handle_context_menu_request.
    DATA: ls_func TYPE ui_func,
          lt_func TYPE ui_functions.

    DATA(l_index) = Zcl_SDE_common=>get_selected( mo_sel_alv ).

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

    Zcl_SDE_common=>refresh( mo_viewer->mo_alv ).
    RAISE EVENT selection_done.
  ENDMETHOD.                           "handle_user_command
ENDCLASS.

CLASS zcl_sde_rtti IMPLEMENTATION.

  METHOD create_struc_handle.
    DATA: ls_comp       TYPE abap_componentdescr,
          lt_components TYPE abap_component_tab,
          lt_field_info TYPE TABLE OF dfies.

    zcl_sde_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = DATA(l_texttab) ).
    e_handle ?= cl_abap_typedescr=>describe_by_name( i_tname ).

    IF l_texttab IS NOT INITIAL.
      DATA(lo_texttab)  = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( l_texttab ) ).
      LOOP AT e_handle->components INTO DATA(l_descr).
        ls_comp-name = l_descr-name.
        ls_comp-type ?= e_handle->get_component_type( ls_comp-name ).
        APPEND ls_comp TO lt_components.
      ENDLOOP.

      LOOP AT lo_texttab->components INTO l_descr.

        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = l_texttab
            fieldname      = l_descr-name
            langu          = sy-langu
          TABLES
            dfies_tab      = lt_field_info
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.

        CHECK sy-subrc = 0.

        IF lt_field_info[ 1 ]-keyflag = abap_false.
          ls_comp-name =  l_texttab && '_' && l_descr-name.
          ls_comp-type ?= lo_texttab->get_component_type( l_descr-name ).
          APPEND: ls_comp TO lt_components,
                  ls_comp TO e_t_comp.

          READ TABLE Zcl_SDE_common=>mt_tabfields INTO DATA(ls_tf) WITH KEY tabname = i_tname fieldname = l_texttab.
          IF sy-subrc NE 0.
            MOVE-CORRESPONDING lt_field_info[ 1 ] TO ls_tf.
            ls_tf-tabname = i_tname.
            ls_tf-fieldname = ls_comp-name.
            ls_tf-is_text = abap_true.
            INSERT ls_tf INTO TABLE Zcl_SDE_common=>mt_tabfields.
          ENDIF.
        ENDIF.
      ENDLOOP.

      e_handle  = cl_abap_structdescr=>create( lt_components ).
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

  METHOD is_list_box. "copied from https://github.com/bizhuka/eui
    CLEAR: ev_list_box,
           es_sh_desc.

    CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
      EXPORTING
        tabname   = iv_tabname
        fieldname = iv_fieldname
      IMPORTING
        shlp      = es_sh_desc
      EXCEPTIONS
        OTHERS    = 1.

    " Fixed values of domains
    CHECK sy-subrc = 0 AND es_sh_desc-shlptype = 'FV'.
    ev_list_box = abap_true.
  ENDMETHOD.

  METHOD find_drop_down. "copied from https://github.com/bizhuka/eui
    DATA:
      ls_sh_desc        TYPE shlp_descr,
      lv_list_box       TYPE abap_bool,
      lt_fielddescr     TYPE ddfields,
      ls_field          TYPE REF TO dfies,
      lt_field_desc     TYPE tt_field_desc,
      ls_field_desc     TYPE ts_field_desc,
      lo_struc          TYPE REF TO cl_abap_structdescr,
      lo_table          TYPE REF TO cl_abap_tabledescr,
      lr_table          TYPE REF TO data,
      lt_shlp_return    TYPE STANDARD TABLE OF ddshretval,
      ls_shlp_return    TYPE REF TO ddshretval,
      lv_prev_pos       TYPE i,
      ls_call_control   TYPE ddshf4ctrl,
      ls_fld_prop       TYPE REF TO ddshfprop,
      lt_shlp_descr_tab TYPE shlp_desct,
      lt_shlp_record    TYPE STANDARD TABLE OF seahlpres,
      lt_dropdown       TYPE lvc_t_dral,
      ls_dropdown       TYPE lvc_s_dral.

    FIELD-SYMBOLS:
      <lt_table> TYPE STANDARD TABLE,
      <ls_row>   TYPE any,
      <lv_value> TYPE any,
      <lv_low>   TYPE any,
      <lv_txt>   TYPE csequence.

    " No need
    IF cs_fieldcat-ref_table = abap_undefined AND cs_fieldcat-ref_field = abap_undefined.
      CLEAR cs_fieldcat-ref_table.
      CLEAR cs_fieldcat-ref_field.
      RETURN.
    ENDIF.

    " No need
    CHECK cs_fieldcat-checkbox <> abap_true
      AND cs_fieldcat-hotspot  <> abap_true.

    " Get top SH
    is_list_box(
     EXPORTING
      iv_tabname   = cs_fieldcat-ref_table
      iv_fieldname = cs_fieldcat-ref_field
     IMPORTING
      ev_list_box  = lv_list_box
      es_sh_desc   = ls_sh_desc ).
    CHECK lv_list_box = abap_true.

    " Work with copy
    lt_fielddescr[] = ls_sh_desc-fielddescr[].

    "TRY.
    " Strucure fields
    LOOP AT lt_fielddescr REFERENCE INTO ls_field.
      ls_field_desc = get_field_desc( is_sh_field = ls_field->* ).
      INSERT ls_field_desc INTO TABLE lt_field_desc.
    ENDLOOP.

    " Output table
    lo_struc = create_structure( it_field_desc = lt_field_desc ).
    lo_table = cl_abap_tabledescr=>create( p_line_type = lo_struc ).

    " Asign it
    CREATE DATA lr_table TYPE HANDLE lo_table.
    ASSIGN lr_table->* TO <lt_table>.

    CALL FUNCTION 'F4IF_SELECT_VALUES'
      EXPORTING
        shlp           = ls_sh_desc
        maxrows        = 0         " all values of domain
        call_shlp_exit = abap_true " 'SELECT' only!
      TABLES
        return_tab     = lt_shlp_return.

**********************************************************************
    " Copied from --> METHOD get_sh_table.
**********************************************************************
    " Show all fields
    LOOP AT ls_sh_desc-fieldprop REFERENCE INTO ls_fld_prop.
      ls_fld_prop->shlpoutput = abap_true.
    ENDLOOP.

    " Call with SELECT event only (probably no texts)
    IF ls_sh_desc-intdescr-selmexit IS INITIAL.
      CALL FUNCTION 'F4IF_SELECT_VALUES'
        EXPORTING
          shlp           = ls_sh_desc
          maxrows        = 0         " all values of domain
          call_shlp_exit = abap_true " 'SELECT' only!
        TABLES
          return_tab     = lt_shlp_return.
    ELSE.
      " Get records first
      CALL FUNCTION 'F4IF_SELECT_VALUES'
        EXPORTING
          shlp           = ls_sh_desc
          maxrows        = 0         " all values of domain
          call_shlp_exit = abap_true
        TABLES
          record_tab     = lt_shlp_record.

      " Disp event
      ls_call_control-step       = 'DISP'.
      ls_call_control-maxrecords = 0. " all values of domain

      APPEND ls_sh_desc TO lt_shlp_descr_tab.
      CALL FUNCTION ls_sh_desc-intdescr-selmexit
        TABLES
          shlp_tab    = lt_shlp_descr_tab
          record_tab  = lt_shlp_record
        CHANGING
          shlp        = ls_sh_desc
          callcontrol = ls_call_control.

      " To normal state -> lt_shlp_return
      CLEAR lt_shlp_return.
      PERFORM transform_outval IN PROGRAM saplsdsd
        TABLES lt_shlp_record lt_shlp_return
        USING ls_call_control ls_sh_desc.
    ENDIF.

    " Write data to table
    lv_prev_pos = 0.
    SORT ls_sh_desc-fielddescr BY fieldname.
    LOOP AT lt_shlp_return REFERENCE INTO ls_shlp_return.
      " New row ?
      IF lv_prev_pos <> ls_shlp_return->recordpos.
        APPEND INITIAL LINE TO <lt_table> ASSIGNING <ls_row>.
      ENDIF.
      lv_prev_pos = ls_shlp_return->recordpos.

      " value
      ASSIGN COMPONENT ls_shlp_return->fieldname OF STRUCTURE <ls_row> TO <lv_value>.
      CHECK sy-subrc = 0.

      " Copy field field
      READ TABLE ls_sh_desc-fielddescr REFERENCE INTO ls_field BINARY SEARCH
        WITH KEY fieldname = ls_shlp_return->fieldname.

      " Special case for certain types
      CASE ls_field->inttype.
        WHEN cl_abap_typedescr=>typekind_time.
          CONCATENATE ls_shlp_return->fieldval+0(2)
                      ls_shlp_return->fieldval+3(2)
                      ls_shlp_return->fieldval+6(2) INTO <lv_value>.

        WHEN cl_abap_typedescr=>typekind_date.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external = ls_shlp_return->fieldval
            IMPORTING
              date_internal = <lv_value>
            EXCEPTIONS
              OTHERS        = 1.
          IF sy-subrc <> 0.
            CLEAR <lv_value>.
          ENDIF.

          " Integer, byte, short
        WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
          RETURN.
*          REPLACE ALL OCCURRENCES OF '.' IN ls_shlp_return->fieldval WITH ''.
*          CONDENSE ls_shlp_return->fieldval NO-GAPS.
*          <lv_value> = ls_shlp_return->fieldval.

        WHEN OTHERS.
          <lv_value> = ls_shlp_return->fieldval.
      ENDCASE.
    ENDLOOP.

    " Next handle
    ADD 1 TO cv_drdn_hndl.

    " Prepare field catalog
    cs_fieldcat-drdn_hndl  = cv_drdn_hndl.
    cs_fieldcat-drdn_alias = abap_true.

    LOOP AT <lt_table> ASSIGNING <ls_row>.
      ASSIGN COMPONENT:
       '_LOW'  OF STRUCTURE <ls_row> TO <lv_low>,
       '_TEXT' OF STRUCTURE <ls_row> TO <lv_txt>.

      ls_dropdown-handle    = cs_fieldcat-drdn_hndl.
      ls_dropdown-int_value = <lv_low>.
      ls_dropdown-value     = <lv_low>.
      "CONCATENATE ls_dropdown-value ` - ` <lv_txt> INTO ls_dropdown-value.

      " Add new item to dropdown
      APPEND ls_dropdown TO lt_dropdown.
    ENDLOOP.

    io_grid->set_drop_down_table(
     it_drop_down_alias = lt_dropdown ).
  ENDMETHOD.

  METHOD get_field_desc.
    DATA:
      ls_header       TYPE x030l,
      lr_table_descr  TYPE REF TO cl_abap_tabledescr,
      lr_struct_descr TYPE REF TO cl_abap_structdescr,
      lv_cnt          TYPE i,
      lr_row          TYPE REF TO data,
      lo_type         TYPE REF TO cl_abap_typedescr,
      lt_sub_fdesc    TYPE tt_field_desc,
      ls_subfield     TYPE ts_field_desc.
    FIELD-SYMBOLS:
      <ls_comp_tab> TYPE abap_compdescr,
      <ls_row>      TYPE any,
      <lv_subvalue> TYPE any,
      <ls_subfield> LIKE ls_subfield.

    IF is_sh_field IS NOT INITIAL.
      rs_field_desc-name     = is_sh_field-fieldname.
      rs_field_desc-sys_type = is_sh_field-inttype.
      rs_field_desc-length   = is_sh_field-leng.
      rs_field_desc-decimals = is_sh_field-decimals.
      rs_field_desc-label    = is_sh_field-fieldtext.
      rs_field_desc-rollname = is_sh_field-rollname.
    ELSE.
      lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).
      rs_field_desc-name     = iv_field_name.
      rs_field_desc-sys_type = lo_type->type_kind. "kind.
      rs_field_desc-length   = lo_type->length.
      rs_field_desc-decimals = lo_type->decimals.
      IF lo_type->is_ddic_type( ) = abap_true.
        rs_field_desc-rollname = lo_type->get_relative_name( ).
      ENDIF.
    ENDIF.

    CASE rs_field_desc-sys_type.

      WHEN cl_abap_typedescr=>typekind_char.
        " Also CHAR
        CASE rs_field_desc-rollname.
          WHEN 'XSDBOOLEAN'.
            rs_field_desc-ui_type = mc_ui_type-boolean.

          WHEN 'XSDDATETIME_Z' OR 'XSDDATETIME_LONG_Z' OR
               'XSDDATETIME_OFFSET' OR 'XSDDATETIME_LOCAL' OR 'XSDDATETIME_LOCAL_DT'.
            rs_field_desc-ui_type = mc_ui_type-datetime.

          WHEN OTHERS.
            rs_field_desc-ui_type  = mc_ui_type-char.
        ENDCASE.

      WHEN cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_numeric.
        rs_field_desc-ui_type  = mc_ui_type-numc.

        " Memo text
      WHEN cl_abap_typedescr=>typekind_string.
        rs_field_desc-ui_type  = mc_ui_type-string.
        rs_field_desc-rollname = 'STRINGVAL'.

      WHEN cl_abap_typedescr=>typekind_table.
        rs_field_desc-ui_type  = mc_ui_type-table.
        lr_table_descr ?= lo_type.

        rs_field_desc-table_kind   = lr_table_descr->table_kind.
        rs_field_desc-unique       = lr_table_descr->has_unique_key.
        rs_field_desc-key          = lr_table_descr->key.
        rs_field_desc-key_defkind  = lr_table_descr->key_defkind.

        " No need for standardc table
        IF rs_field_desc-table_kind = cl_abap_tabledescr=>tablekind_std.
          CLEAR rs_field_desc-key.
        ENDIF.

        " Only for structures
        TRY.
            lr_struct_descr ?= lr_table_descr->get_table_line_type( ).
          CATCH cx_sy_move_cast_error.
*          MESSAGE s016(zeui_message) WITH rs_field_desc-name INTO sy-msgli.
*          zcx_eui_exception=>raise_sys_error( ).
        ENDTRY.

        " For speed creation
        IF lr_struct_descr->is_ddic_type( ) = abap_true.
          ls_header = lr_struct_descr->get_ddic_header( ).
          rs_field_desc-rollname     = ls_header-tabname.
        ENDIF.

        " Create STANDARD table for field catalog!
        CREATE DATA lr_row TYPE HANDLE lr_struct_descr.
        ASSIGN lr_row->* TO <ls_row>.

        CLEAR:
         rs_field_desc-sub_fdesc,
         lt_sub_fdesc.
        LOOP AT lr_struct_descr->components ASSIGNING <ls_comp_tab>.
          ASSIGN COMPONENT <ls_comp_tab>-name OF STRUCTURE <ls_row> TO <lv_subvalue>.

          " Recursion
          ls_subfield = get_field_desc( iv_field_name  = <ls_comp_tab>-name
                                        iv_data        = <lv_subvalue>
                                        ir_unique_type = ir_unique_type ).

          INSERT ls_subfield INTO TABLE lt_sub_fdesc.
        ENDLOOP.

        " Select option ?
        DO 1 TIMES.
          lv_cnt = lines( lt_sub_fdesc ).

          CHECK lv_cnt = 4.
          " Check by name
          LOOP AT lt_sub_fdesc TRANSPORTING NO FIELDS WHERE
             name = 'SIGN' OR name = 'OPTION' OR name = 'LOW' OR name = 'HIGH'. "#EC CI_HASHSEQ
            lv_cnt = lv_cnt - 1.
          ENDLOOP.

          " Select-option
          CHECK lv_cnt = 0.
          rs_field_desc-ui_type  = mc_ui_type-range.

          " No need in components
          CLEAR rs_field_desc-sub_fdesc.

          " Where to find TABLE-FIELDNAME
          READ TABLE lt_sub_fdesc ASSIGNING <ls_subfield>
           WITH TABLE KEY name = 'LOW'.
          rs_field_desc-rollname = <ls_subfield>-rollname.
          rs_field_desc-label    = <ls_subfield>-label.
        ENDDO.

        " Date
      WHEN cl_abap_typedescr=>typekind_date.
        rs_field_desc-ui_type  = mc_ui_type-date.
        " Time
      WHEN cl_abap_typedescr=>typekind_time.
        rs_field_desc-ui_type  = mc_ui_type-time.
        " Integer, byte, short
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
        rs_field_desc-ui_type  = mc_ui_type-numeric.
        " Double
      WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
           '/' OR 'a' OR 'e'. " cl_abap_typedescr=>typekind_decfloat  OR cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
        rs_field_desc-ui_type  = mc_ui_type-numeric.
      WHEN OTHERS.
    ENDCASE.

    " TABLE-FIELDNAME from search help
    IF is_sh_field-reffield IS NOT INITIAL.
      CONCATENATE is_sh_field-reftable '-' is_sh_field-reffield INTO rs_field_desc-rollname.
    ENDIF.

    " Try to find TABLE-FIELDNAME
    IF
      rs_field_desc-ui_type <> mc_ui_type-table  AND
      rs_field_desc-ui_type <> mc_ui_type-string AND
      rs_field_desc-rollname NP '*-*'.
      find_table_fieldname(
       EXPORTING
        ir_unique_type = ir_unique_type
       CHANGING
        cv_rollname    = rs_field_desc-rollname
        cv_label       = rs_field_desc-label ).
    ENDIF.

    " Set default text
    IF rs_field_desc-label IS INITIAL.
      rs_field_desc-label = rs_field_desc-name.
    ENDIF.
  ENDMETHOD.

  METHOD find_table_fieldname.
    TYPES:
      BEGIN OF ts_dd03l,
        tabname    TYPE dd03l-tabname,
        fieldname  TYPE dd03l-fieldname,
        shlporigin TYPE dd03l-shlporigin,
        tab_len    TYPE i,
      END OF ts_dd03l.

    DATA:
      lv_rollname TYPE rollname,
      lt_dd03l    TYPE STANDARD TABLE OF ts_dd03l,
      ls_dd03l    TYPE REF TO ts_dd03l,
      lv_tabfld   TYPE string,
      ls_dd04t    TYPE dd04t,
      lo_type     TYPE REF TO cl_abap_datadescr.
    FIELD-SYMBOLS:
      <lt_unique_type> TYPE tt_unique_type.

    " Table Fields
    CHECK cv_rollname IS NOT INITIAL.
    lv_rollname = cv_rollname.

    SELECT d~tabname d~fieldname d~shlporigin INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
    FROM dd03l AS d UP TO 100 ROWS
    WHERE d~rollname = lv_rollname AND d~as4local = 'A' AND d~tabname NOT LIKE '/%' AND d~depth = 0.

    " Find short table name
    LOOP AT lt_dd03l REFERENCE INTO ls_dd03l.
      ls_dd03l->tab_len = strlen( ls_dd03l->tabname ).

      " In the end
      IF ls_dd03l->shlporigin IS NOT INITIAL.
        ls_dd03l->tab_len = ls_dd03l->tab_len - 1000.
      ENDIF.
    ENDLOOP.
    SORT lt_dd03l BY tab_len ASCENDING.

    " Try to find
    ASSIGN ir_unique_type->* TO <lt_unique_type>.
    LOOP AT lt_dd03l REFERENCE INTO ls_dd03l.
      CONCATENATE ls_dd03l->tabname '-' ls_dd03l->fieldname INTO lv_tabfld.

      " if type exist
      TRY.
          lo_type = create_type_descr( iv_rollname = lv_tabfld ).
*      CATCH zcx_eui_exception.
*        CLEAR lo_type.
      ENDTRY.
      CHECK lo_type IS NOT INITIAL.

      " Get next item
      IF ir_unique_type IS NOT INITIAL.
        READ TABLE <lt_unique_type> TRANSPORTING NO FIELDS
         WITH TABLE KEY table_line = lv_tabfld.
        CHECK sy-subrc <> 0.

        " Do not repeat types
        INSERT lv_tabfld INTO TABLE <lt_unique_type>.
      ENDIF.
      cv_rollname = lv_tabfld.

      DO 1 TIMES.
        " If have no text
        CHECK cv_label IS SUPPLIED AND cv_label IS INITIAL.

        " №2
        SELECT SINGLE * INTO ls_dd04t
        FROM dd04t
        WHERE rollname   = lv_rollname
          AND ddlanguage = sy-langu
          AND as4local   = 'A'
          AND as4vers    = 0.
        CHECK sy-subrc = 0.

        IF ls_dd04t-ddtext IS NOT INITIAL.
          cv_label = ls_dd04t-ddtext.
        ELSE.
          cv_label = ls_dd04t-reptext.
        ENDIF.
      ENDDO.

      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_structure. "copied from https://github.com/bizhuka/eui
    DATA:
      lt_comp      TYPE abap_component_tab,
      lt_sub_fdesc TYPE tt_field_desc.

    FIELD-SYMBOLS:
      <ls_field_desc> TYPE ts_field_desc,
      <ls_subfield>   TYPE ts_field_desc,
      <ls_comp>       LIKE LINE OF lt_comp.

    " №2 For select-options
    IF io_range IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = 'SIGN'.
      <ls_comp>-type = cl_abap_elemdescr=>get_c( p_length = 1 ).

      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = 'OPTION'.
      <ls_comp>-type = cl_abap_elemdescr=>get_c( p_length = 2 ).

      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = 'LOW'.
      <ls_comp>-type = io_range.

      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = 'HIGH'.
      <ls_comp>-type = io_range.
    ENDIF.

    " №4 Called from constructor if have in DB cluster
    LOOP AT it_field_desc ASSIGNING <ls_field_desc>.
      " Create sub levels
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = <ls_field_desc>-name.
      <ls_comp>-type = create_type_descr( is_field_desc = <ls_field_desc> ).
    ENDLOOP.
    ro_struct = cl_abap_structdescr=>create( lt_comp ).
  ENDMETHOD.

  METHOD create_type_descr. "copied from https://github.com/bizhuka/eui
    DATA:
      lo_line     TYPE REF TO cl_abap_datadescr,
      lo_type     TYPE REF TO cl_abap_typedescr,
      lv_sys_type TYPE abap_typekind,
      lv_message  TYPE string.

    " No type
    CLEAR ro_type.

    " №0
    IF is_field_desc IS SUPPLIED.

      " For tables speed 1
      IF is_field_desc-rollname IS NOT INITIAL.
        ro_type = create_type_descr(
          iv_rollname = is_field_desc-rollname ).
      ENDIF.

      IF ro_type IS INITIAL.
        lv_sys_type = is_field_desc-sys_type.

        " For old option wrong is_field_desc-sys_type
        IF lv_sys_type = cl_abap_typedescr=>typekind_table AND is_field_desc-ui_type = mc_ui_type-range.
          " usually char
          lv_sys_type = cl_abap_typedescr=>typekind_char.
          " Show warning
          CONCATENATE `No right type for ` is_field_desc-name ` ` is_field_desc-rollname `!` INTO lv_message.
          MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

        CASE lv_sys_type.
          WHEN cl_abap_typedescr=>typekind_char.
            ro_type = cl_abap_elemdescr=>get_c( p_length = is_field_desc-length ).
          WHEN cl_abap_typedescr=>typekind_date.
            ro_type = cl_abap_elemdescr=>get_d( ).
          WHEN cl_abap_typedescr=>typekind_int.
            ro_type = cl_abap_elemdescr=>get_i( ).
          WHEN cl_abap_typedescr=>typekind_float.
            ro_type = cl_abap_elemdescr=>get_f( ).
          WHEN cl_abap_typedescr=>typekind_num.
            ro_type = cl_abap_elemdescr=>get_n( p_length = is_field_desc-length ).
          WHEN cl_abap_typedescr=>typekind_packed.
            ro_type = cl_abap_elemdescr=>get_p( p_length = is_field_desc-length p_decimals = is_field_desc-decimals ).
          WHEN cl_abap_typedescr=>typekind_string.
            ro_type = cl_abap_elemdescr=>get_string( ).
          WHEN cl_abap_typedescr=>typekind_time.
            ro_type = cl_abap_elemdescr=>get_t( ).
          WHEN cl_abap_typedescr=>typekind_table.
            " Below in code CASE is_field_desc-ui_type.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.

      CASE is_field_desc-ui_type.
        WHEN mc_ui_type-range.
          IF ro_type IS INITIAL.
          ENDIF.

          " Call №2 recursion
          lo_line = create_structure( io_range = ro_type ).
          ro_type = cl_abap_tabledescr=>create( p_line_type = lo_line ).

        WHEN mc_ui_type-table.
          " Call №3 recursion
          IF ro_type IS INITIAL.
            ro_type = create_structure( iv_sub_fdesc = is_field_desc-sub_fdesc ).
          ENDIF.

          ro_type = cl_abap_tabledescr=>create(
            p_line_type   = ro_type
            p_table_kind  = is_field_desc-table_kind
            p_unique      = is_field_desc-unique
            p_key         = is_field_desc-key
            p_key_kind    = is_field_desc-key_defkind ).
      ENDCASE.
    ENDIF.

    CHECK ro_type IS INITIAL.

    " №0
    IF iv_rollname NP '*-*'.
      cl_abap_datadescr=>describe_by_name(
       EXPORTING
        p_name         = iv_rollname
       RECEIVING
        p_descr_ref    = lo_type
       EXCEPTIONS
        type_not_found = 1 ).

      ro_type ?= lo_type.
      RETURN.
    ENDIF.

    " №1 - Create from text
    IF ir_type IS INITIAL AND iv_rollname IS NOT INITIAL.
      CREATE DATA ir_type TYPE (iv_rollname).
    ENDIF.

    " №2 - Based on incoming reference
    cl_abap_datadescr=>describe_by_data_ref(
     EXPORTING
      p_data_ref           = ir_type
     RECEIVING
      p_descr_ref          = lo_type
     EXCEPTIONS
      reference_is_initial = 1 ).

    ro_type ?= lo_type.
  ENDMETHOD.

ENDCLASS.

CLASS zcl_sde_receiver IMPLEMENTATION.
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
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    IF <to>-range[] = e_row-range[].
      DATA(l_updated) = abap_true."so as not to have an infinite event loop
    ENDIF.
    MOVE-CORRESPONDING e_row TO <to>.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = e_row ).
    ENDIF.
    lo_sel_to->raise_selection_done( ).
  ENDMETHOD.

  METHOD update_col.
    DATA: lt_sel_row   TYPE zcl_sde_appl=>t_sel_row.
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
      DATA(l_updated) = abap_true."so as not to have an infinite event loop
    ENDIF.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = lt_sel_row ).
      lo_sel_to->raise_selection_done( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS  zcl_sde_py_cluster_viewer IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    m_pernr = i_pernr.
    m_seqnr = i_seqnr.
    mo_box = create( i_name = CONV #( m_pernr ) i_width = 180 i_hight = 280  ).
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
       container = mo_parent ).

    SET HANDLER on_box_close FOR mo_box.

    read_cluster( ).
    create_hierarchy( ).
    show_tree( ).
  ENDMETHOD.

  METHOD init_alv_tree.
    TRY.
        CALL METHOD cl_salv_tree=>factory
          EXPORTING
            r_container = mo_parent
          IMPORTING
            r_salv_tree = mo_tree
          CHANGING
            t_table     = mt_empty.
      CATCH cx_salv_error.
    ENDTRY.

    CALL METHOD mo_tree->get_event RECEIVING value = DATA(lo_event).
    SET HANDLER me->hndl_double_click FOR lo_event.
  ENDMETHOD.

  METHOD on_box_close.
    sender->free( ).

    LOOP AT mt_children INTO DATA(child).
      READ TABLE zcl_sde_appl=>mt_obj WITH KEY alv_viewer = child-item ASSIGNING FIELD-SYMBOL(<obj>).
      CHECK sy-subrc = 0.
      DATA(l_indx) = sy-tabix.

      child-item->mo_box->free( EXCEPTIONS cntl_error = 1  ).
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
      DELETE zcl_sde_appl=>mt_obj INDEX l_indx.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_tree.
    init_alv_tree( ).
    create_tree( ).

    DATA(lo_columns) = mo_tree->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    lo_columns->get_column( 'KEY' )->set_visible( abap_false ).
    lo_columns->get_column( 'NAME' )->set_visible( abap_false ).
    lo_columns->get_column( 'ANYNODE' )->set_visible( abap_false ).
    lo_columns->get_column( 'ANYPARENT' )->set_visible( abap_false ).
    lo_columns->get_column( 'TYPE' )->set_visible( abap_false ).

    DATA(lo_tree_settings) = mo_tree->get_tree_settings( ).
    lo_tree_settings->set_hierarchy_header( CONV #( m_seqnr ) ).
    mo_nodes->expand_all( ).
    mo_tree->get_functions( )->set_all( ).
    mo_tree->display( ).
    mo_box->set_focus( mo_box ).
  ENDMETHOD.

  METHOD hndl_double_click.

    DATA: go_struct    TYPE REF TO cl_abap_structdescr,
          go_table     TYPE REF TO cl_abap_tabledescr,
          go_abapstr   TYPE REF TO cl_abap_typedescr,
          l_struc_name TYPE tabname,
          lr_tab       TYPE REF TO data.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                   <str>   TYPE any.

    DATA(ls_hier) = mt_hier[ node_key ].
    CHECK ls_hier-tab_ref IS NOT INITIAL.
    go_abapstr = cl_abap_structdescr=>describe_by_data_ref( ls_hier-tab_ref ).
    IF go_abapstr->type_kind = 'h'.
      go_table  ?= cl_abap_structdescr=>describe_by_data_ref( ls_hier-tab_ref ).
      go_struct ?= go_table->get_table_line_type( ).
      l_struc_name = go_struct->absolute_name.
    ELSE.
      go_struct  ?= cl_abap_structdescr=>describe_by_data_ref( ls_hier-tab_ref ).
      l_struc_name = go_struct->absolute_name.
      zcl_sde_rtti=>create_table_by_name( EXPORTING i_tname = l_struc_name CHANGING c_table = lr_tab  ).
      ASSIGN lr_tab->* TO <table>.
      ASSIGN ls_hier-tab_ref->* TO <str>.
      APPEND INITIAL LINE TO <table> ASSIGNING FIELD-SYMBOL(<line>).
      MOVE-CORRESPONDING <str> TO <line>.
      ls_hier-tab_ref = lr_tab.
    ENDIF.
    ls_hier-type = go_abapstr->type_kind.
    DATA(l_name) = |{ m_pernr }: ({ m_seqnr }) |.
    REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_struc_name WITH ''.
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_struc_name ir_tab = ls_hier-tab_ref i_additional_name = l_name.
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
    APPEND VALUE #( item = <obj>-alv_viewer  ) TO mt_children.
  ENDMETHOD.

  METHOD read_cluster.
    DATA: lo_handle  TYPE REF TO cl_abap_complexdescr.

    FIELD-SYMBOLS: <cluster> TYPE any.

    lo_handle ?= cl_abap_typedescr=>describe_by_name( 'PAYRU_RESULT' ).
    CREATE DATA mr_cluster TYPE HANDLE lo_handle.
    ASSIGN mr_cluster->* TO <cluster>.

    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        employeenumber               = m_pernr
        sequencenumber               = m_seqnr
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
  ENDMETHOD.

  METHOD create_hierarchy.
    DATA: lo_stru    TYPE REF TO cl_abap_structdescr,
          lo_element TYPE REF TO cl_abap_structdescr,
          ls_hier    LIKE LINE OF mt_hier,
          l_lines    TYPE i.

    FIELD-SYMBOLS: <table>   TYPE ANY TABLE,
                   <struc>   TYPE any,
                   <cluster> TYPE any.

    lo_stru ?= cl_abap_typedescr=>describe_by_name( 'PAYRU_RESULT' ).

    "top node
    APPEND VALUE #( anynode = 'Main' anyparent = '' name = 'Cluster' ) TO mt_hier.

    LOOP AT lo_stru->components INTO DATA(ls_comp).

      ls_hier-anynode = ls_comp-name.
      ls_hier-anyparent = 'Main'.
      ls_hier-name = ls_comp-name.
      APPEND ls_hier TO mt_hier.

      ASSIGN mr_cluster->* TO <cluster>.
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE <cluster> TO FIELD-SYMBOL(<element>).
      lo_element ?= cl_abap_typedescr=>describe_by_data( <element> ).
      DATA(lt_comp) = lo_element->get_components( ).

      LOOP AT lt_comp INTO DATA(ls_el).
        CHECK ls_el-type->type_kind = 'u' "structure
           OR ls_el-type->type_kind = 'h'. "table

        CLEAR: l_lines, ls_hier.
        IF ls_el-type->type_kind = 'h'.
          ASSIGN COMPONENT ls_el-name OF STRUCTURE <element> TO <table>.
          l_lines = lines( <table> ).
          GET REFERENCE OF <table> INTO ls_hier-tab_ref.
        ELSE.
          ASSIGN COMPONENT ls_el-name OF STRUCTURE <element> TO <struc>.
          GET REFERENCE OF <struc> INTO ls_hier-tab_ref.
        ENDIF.
        CHECK l_lines NE 0 OR ls_el-type->type_kind = 'u'.

        ls_hier-anynode = ls_el-name.
        ls_hier-anyparent = ls_comp-name.
        ls_hier-name = ls_el-name.
        IF ls_el-type->type_kind = 'h'.
          ls_hier-name = |{ ls_hier-name } ({ l_lines })|.
        ENDIF.
        ls_hier-type = ls_el-type->type_kind.
        APPEND ls_hier TO mt_hier.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_tree.
    DATA: lv_text TYPE lvc_value,
          lv_icon TYPE salv_de_tree_image.

    TRY.
        mo_nodes = mo_tree->get_nodes( ).
        LOOP AT mt_hier ASSIGNING FIELD-SYMBOL(<hier>).
          READ TABLE mt_hier WITH KEY anynode = <hier>-anyparent ASSIGNING FIELD-SYMBOL(<hier_up>).
          IF sy-subrc NE 0."root node

            mo_node = mo_nodes->add_node(
            related_node = ''
            relationship = if_salv_c_node_relation=>parent ).
          ELSE.
            IF <hier>-type = 'h'.
              lv_icon = icon_view_table.
            ELSE.
              lv_icon = icon_structure.
            ENDIF.
            mo_node = mo_nodes->add_node(
            related_node = <hier_up>-key
            relationship = if_salv_c_node_relation=>last_child
            collapsed_icon = lv_icon
            expanded_icon  = lv_icon  ).
          ENDIF.

          <hier>-key = mo_node->get_key( ).
          mo_node->set_data_row( <hier> ).
          lv_text = <hier>-name.
          mo_node->set_text( lv_text ).
        ENDLOOP.
      CATCH cx_salv_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_sde_popup IMPLEMENTATION.

  METHOD constructor.
    m_additional_name = i_additional_name.
  ENDMETHOD.

  METHOD create.
    DATA: l_top  TYPE i,
          l_left TYPE i.

    ADD 1 TO m_counter.
    l_top  = l_left =  10 + 10 * ( m_counter DIV 5 ) +  ( m_counter MOD 5 ) * 50.

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

CLASS zcl_sde_plugins IMPLEMENTATION.
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

    LOOP AT zcl_sde_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS NOT INITIAL.
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
    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).

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
    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).

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
    NEW zcl_sde_text_viewer( io_viewer ).
  ENDMETHOD.

  METHOD run_hrpy_rgdir.
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = 'HRPY_RGDIR'.
    ASSIGN COMPONENT 'PERNR' OF STRUCTURE i_str TO FIELD-SYMBOL(<pernr>).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'PERNR' i_low = <pernr>  ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.

  METHOD run_hrp1001_adatanr.
    IF i_column = 'ADATANR' AND io_viewer->m_tabname = 'HRP1001'.
      ASSIGN COMPONENT 'ADATANR' OF STRUCTURE i_str TO FIELD-SYMBOL(<datanr>).
      ASSIGN COMPONENT 'RELAT' OF STRUCTURE i_str TO FIELD-SYMBOL(<relat>).
      SELECT SINGLE pasub INTO @DATA(lv_struc) FROM t77ar WHERE relat = @<relat>.
      SELECT SINGLE dbtab INTO @DATA(lv_dbtab) FROM t77ad WHERE pasub = @lv_struc.

      IF sy-subrc = 0.
        APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
        CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = lv_dbtab.
        <obj>-alv_viewer->mo_sel->set_value( i_field = 'ADATANR' i_low = <datanr>  ).
        <obj>-alv_viewer->mo_sel->raise_selection_done( ).
        is_done = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD run_subty.
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).
    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<str>).

    SELECT SINGLE stypt, namst INTO @DATA(l_result)   FROM t777d WHERE dbtab = @io_viewer->m_tabname.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <str> TO FIELD-SYMBOL(<subty>).
    DATA(l_infty) = io_viewer->m_tabname+2(4).
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_result-stypt.
    <obj>-alv_viewer->mo_sel->set_value( i_field = l_result-namst i_low = <subty> ).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'SUBTY' i_low = <subty> ).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'INFTY' i_low = l_infty ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.

  METHOD run_py_cluster.
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).
    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<str>).

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <str> TO FIELD-SYMBOL(<pernr>).
    ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <str> TO FIELD-SYMBOL(<seqnr>).
    NEW zcl_sde_py_cluster_viewer( i_pernr = <pernr> i_seqnr = <seqnr> ).
  ENDMETHOD.

  METHOD run_field_2_plugin.
    LOOP AT zcl_sde_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS NOT INITIAL.
      CASE ls_link-method.
        WHEN 'RUN_PY_CLUSTER'.
          run_py_cluster( io_viewer ).
      ENDCASE.
      is_done = 'X'.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_field_2_field.
    DATA: lo_viewer TYPE REF TO zcl_sde_table_viewer.
    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    LOOP AT zcl_sde_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS INITIAL.
      ASSIGN COMPONENT ls_link-field OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
      IF lo_viewer IS INITIAL.
        IF ls_link-rtab IS INITIAL.
          lo_viewer = io_viewer.
        ELSE.
          APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
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
    DATA: lo_viewer TYPE REF TO zcl_sde_table_viewer.

    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    READ TABLE Zcl_SDE_common=>mt_tabfields WITH KEY tabname = io_viewer->m_tabname fieldname = i_column INTO DATA(l_field).
    LOOP AT zcl_sde_plugins=>mt_el_links INTO DATA(l_el_link) WHERE element = l_field-rollname AND plugin NE '' ." 'PA20' .
      CASE l_el_link-plugin.
        WHEN 'PA20'.
          zcl_sde_plugins=>run_pa20( io_viewer ).
        WHEN 'PP01'.
          zcl_sde_plugins=>run_pp01( io_viewer ).
        WHEN 'SHOW_TEXT'.
          zcl_sde_plugins=>run_text( io_viewer ).
        WHEN 'SUBTY'.
          zcl_sde_plugins=>run_subty( io_viewer ).
      ENDCASE.
      is_done = abap_true.
    ENDLOOP.
    IF is_done = abap_true.
      RETURN.
    ENDIF.

    LOOP AT zcl_sde_plugins=>mt_el_links INTO l_el_link WHERE element = l_field-rollname .
      IF lo_viewer IS INITIAL.
        APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
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
    READ TABLE Zcl_SDE_common=>mt_tabfields INTO DATA(field) WITH KEY tabname = io_viewer->m_tabname fieldname = i_column .
    ASSIGN COMPONENT i_column OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
    CHECK sy-subrc = 0.
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
      APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
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
        IF m_pick IS NOT INITIAL AND lines( mt_cols ) < 3 AND NOT line_exists( mt_cols[ table_line = m_pick ] ).
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
          IF lines( mt_cols ) < 3 AND NOT line_exists( mt_cols[ table_line = l_ckey ] ).
            APPEND l_ckey TO mt_cols.
          ENDIF.
          CLEAR m_pick.
          RETURN.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'dv_'.
          APPEND VALUE #( key = substring( val = i_act off = 3 len = strlen( i_act ) - 3 )
                          agg = 'COUNT' ) TO mt_vals.
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

  METHOD normalize_aggs.
    LOOP AT mt_vals ASSIGNING FIELD-SYMBOL(<val>).
      DATA(lt_allowed) = allowed_aggs( i_key = <val>-key it_fields = it_fields ).
      IF NOT line_exists( lt_allowed[ table_line = <val>-agg ] ).
        <val>-agg = default_agg( i_key = <val>-key it_fields = it_fields ).
      ENDIF.
    ENDLOOP.
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
    rv_html = rv_html && `<h4>Dimensions (plain columns)</h4><div class="zone" onmouseover="zo(event,this,'dr')">`.
    LOOP AT mt_rows INTO DATA(l_row).
      rv_html = rv_html &&
        |<a class="zchip" href="SAPEVENT:pv?rr_{ l_row }" title="remove">{ to_lower( l_row ) } &#10005;</a>|.
    ENDLOOP.
    IF m_pick IS NOT INITIAL.
      rv_html = rv_html &&
        |<a class="zchip add" href="SAPEVENT:pv?tr">+ { l_pick_low }</a>|.
    ELSEIF mt_rows IS INITIAL.
      rv_html = rv_html && `<span class="dir">fields kept as regular result columns go here</span>`.
    ENDIF.

    rv_html = rv_html && `</div><h4>Spread (values to columns)</h4><div class="zone" onmouseover="zo(event,this,'dc')">`.
    LOOP AT mt_cols INTO DATA(l_col).
      rv_html = rv_html &&
        |<a class="zchip" href="SAPEVENT:pv?rc_{ l_col }" title="remove">{ to_lower( l_col ) } &#10005;</a>|.
    ENDLOOP.
    IF m_pick IS NOT INITIAL.
      rv_html = rv_html &&
        |<a class="zchip add" href="SAPEVENT:pv?tc">+ { l_pick_low }</a>|.
    ELSEIF mt_cols IS INITIAL.
      rv_html = rv_html && `<span class="dir">field values become separate result columns (max 50)</span>`.
    ELSEIF lines( mt_cols ) >= 3.
      rv_html = rv_html && `<span class="dir">maximum 3 spread fields</span>`.
    ENDIF.

    rv_html = rv_html && `</div><h4>Measures (aggregates)</h4><div class="zone" onmouseover="zo(event,this,'dv')">`.
    LOOP AT mt_vals INTO DATA(ls_val).
      DATA(l_vidx) = sy-tabix.
      DATA(lt_aggs) = allowed_aggs( i_key = ls_val-key it_fields = it_fields ).
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
      LOOP AT allowed_aggs( i_key = m_pick it_fields = it_fields ) INTO DATA(l_add_agg).
        rv_html = rv_html &&
          |<a class="zchip add" href="SAPEVENT:pv?ag_{ l_add_agg }">+ { l_add_agg }( { l_pick_low } )</a>|.
      ENDLOOP.
    ELSEIF mt_vals IS INITIAL.
      rv_html = rv_html && `<span class="dir">numeric fields for SUM/COUNT/MIN/MAX/AVG go here</span>`.
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
          lt_names TYPE tt_keys,
          lt_vals  TYPE tt_vals.
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
      lt_vals = mt_vals.
      IF lt_vals IS INITIAL.
        APPEND VALUE #( key = mt_cols[ 1 ] agg = 'COUNT' ) TO lt_vals.
      ENDIF.
      "matrix: one CASE bucket per column value and aggregate
      LOOP AT it_col_vals INTO DATA(ls_cv).
        LOOP AT lt_vals INTO DATA(ls_val).
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
          DATA(l_then) = COND string( WHEN ls_val-agg = 'COUNT' THEN `1` ELSE l_fq ).
          l_fields = |{ l_fields }{ ls_val-agg }( CASE WHEN { l_cond } | &&
                     |THEN { l_then } END ) AS { to_lower( l_name ) }|.
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

CLASS zcl_sde_dragdrop IMPLEMENTATION.
  METHOD drag.
    DATA(dataobj) = NEW zcl_sde_dd_data( ).
    dataobj->m_row = e_row-index.
    dataobj->m_column = e_column.
    e_dragdropobj->object = dataobj.
  ENDMETHOD.

  METHOD drop."It should be refactored someday...
    DATA: ls_row          TYPE zcl_sde_appl=>t_sel_row,
          lv_set_receiver.

    LOOP AT zcl_sde_appl=>mt_obj INTO DATA(lo).
      "to
      IF e_dragdropobj->droptargetctrl = lo-alv_viewer->mo_sel->mo_sel_alv.
        DATA(lo_to) = lo-alv_viewer->mo_sel.
        DATA(to_obj) = lo.
      ENDIF.
      "from tab
      IF lo-alv_viewer->mo_alv = e_dragdropobj->dragsourcectrl.
        DATA(lo_from_tab) = lo-alv_viewer.
        DATA(from_obj) = lo.
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

    IF from_obj NE to_obj.
      DATA(lo_alv) = CAST cl_gui_alv_grid( e_dragdropobj->dragsourcectrl ).
      Zcl_SDE_common=>refresh( EXPORTING i_obj = lo_alv i_soft = abap_true ).
    ENDIF.

    lo_alv ?= e_dragdropobj->droptargetctrl.
    lo_to->raise_selection_done( ).
  ENDMETHOD.
ENDCLASS.

CLASS zcl_sde_dd_data IMPLEMENTATION.
ENDCLASS.

CLASS zcl_sde_ddic IMPLEMENTATION.
  METHOD get_text_table.
    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname   = i_tname
      IMPORTING
        texttable = e_tab.
  ENDMETHOD.
ENDCLASS.

CLASS Zcl_SDE_common IMPLEMENTATION.
  METHOD refresh.
    DATA l_stable TYPE lvc_s_stbl.
    l_stable = 'XX'.
    IF i_layout IS SUPPLIED.
      i_obj->set_frontend_layout( i_layout ) .
    ENDIF.
    TRY. "S/4 guards refresh_table_display inside grid callbacks (CX_SALV_METHOD_NOT_SUPPORTED)
        i_obj->refresh_table_display( EXPORTING is_stable = l_stable i_soft_refresh = i_soft  ).
      CATCH cx_root.                                    "#EC NO_HANDLER
        "the grid refreshes itself after the callback anyway
    ENDTRY.
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

CLASS zcl_sde_appl IMPLEMENTATION.
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

  METHOD suppress_run_button.
    DATA itab TYPE TABLE OF sy-ucomm.

    itab = VALUE #( ( 'ONLI' ) ( 'WB_EXEC' ) ).
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = itab.
  ENDMETHOD.

  METHOD open_int_table.

    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND.
      r_tab = it_ref.
    ELSE.
      GET REFERENCE OF it_tab INTO r_tab.
    ENDIF.
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW #(  i_additional_name = iv_name ir_tab = r_tab ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).

  ENDMETHOD.

  METHOD exit.
    DATA: l_answer.
    DESCRIBE TABLE zcl_sde_appl=>mt_obj.
    IF sy-tfill NE 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = 'Exit'
          text_question  = 'Do you want to exit?'
        IMPORTING
          answer         = l_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF l_answer = '1'.
        LEAVE PROGRAM.
      ELSE.
        "CALL screen 101.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

TABLES sscrfields.
DATA: g_mode TYPE i VALUE 1.
"selection-screen begin of screen 101.
SELECTION-SCREEN: FUNCTION KEY 1."Tables
SELECTION-SCREEN: FUNCTION KEY 2."Views
SELECTION-SCREEN: FUNCTION KEY 3."CDS

PARAMETERS: gv_tname TYPE tabname VISIBLE LENGTH 15 MATCHCODE OBJECT dd_bastab_for_view MODIF ID tab.
PARAMETERS: gv_vname TYPE tabname VISIBLE LENGTH 15 MATCHCODE OBJECT viewmaint MODIF ID vie.
PARAMETERS: gv_cds   TYPE tabname VISIBLE LENGTH 15 MODIF ID cds.
PARAMETERS: gv_rows  TYPE i DEFAULT 500.
"selection-screen end of screen 101.

INITIALIZATION.
  zcl_sde_appl=>init_lang( ).
  zcl_sde_appl=>init_icons_table( ).
  zcl_sde_plugins=>init( ).
  sscrfields-functxt_01 = 'Tables'.
  sscrfields-functxt_02 = 'Views'.
  sscrfields-functxt_03 = 'CDS'.
  " call screen 101.

AT SELECTION-SCREEN OUTPUT.
  %_gv_tname_%_app_%-text = 'Enter Table name and hit Enter'.
  %_gv_vname_%_app_%-text = 'Enter View name and hit Enter'.
  %_gv_cds_%_app_%-text = 'Enter CDS name and hit Enter'.
  zcl_sde_appl=>suppress_run_button( ).

  LOOP AT SCREEN.
    IF screen-group1 = 'TAB'.
      IF g_mode = 1.
        screen-active = '1'.
        screen-invisible = '0'.
      ELSE.
        screen-active = '0'.
        screen-invisible = '1'.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'VIE'.
      IF g_mode = 2.
        screen-active = '1'.
        screen-invisible = '0'.
      ELSE.
        screen-active = '0'.
        screen-invisible = '1'.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'CDS'.
      IF g_mode = 3.
        screen-active = '1'.
        screen-invisible = '0'.
      ELSE.
        screen-active = '0'.
        screen-invisible = '1'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  zcl_sde_appl=>exit( ).
AT SELECTION-SCREEN ON VALUE-REQUEST FOR gv_cds.
  PERFORM search_cds.

AT SELECTION-SCREEN .
  zcl_sde_appl=>gv_rows  = gv_rows.
  zcl_sde_appl=>gv_vname = gv_vname.

  CASE sy-ucomm.
    WHEN 'FC01'.
      g_mode = 1.
    WHEN 'FC02'.
      g_mode = 2.
    WHEN 'FC03'.
      g_mode = 3.
  ENDCASE.

  CHECK sy-ucomm IS INITIAL.

  IF g_mode = 1. "table
    CONDENSE gv_tname.
    CHECK zcl_sde_sql=>exist_table( gv_tname ) = 1.
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = gv_tname.
  ENDIF.

  IF g_mode = 2. "view
    CONDENSE gv_vname.
    CHECK zcl_sde_sql=>exist_view( gv_vname ) = 1.
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING <obj>.
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = gv_vname i_is_view = abap_true.
  ENDIF.

  IF g_mode = 3. "CDS
    CONDENSE gv_cds.
    CHECK zcl_sde_sql=>exist_cds( gv_cds ) = 1.
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING <obj>.
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = gv_cds i_is_cds = abap_true.
  ENDIF.

FORM search_cds.
  TYPES: BEGIN OF t_cds,
           tabname TYPE tabname,
         END OF t_cds.

  DATA: lt_cds        TYPE TABLE OF t_cds,
        l_search      TYPE string,
        lt_dynpfields TYPE TABLE OF dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
      request              = 'A'
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  l_search = lt_dynpfields[ fieldname = 'GV_CDS' ]-fieldvalue && '%'.

  SELECT tabname INTO CORRESPONDING FIELDS OF TABLE lt_cds
    FROM dd02l
    UP TO 500 ROWS
    WHERE tabclass = 'VIEW'
      AND applclass = 'SDGV'
      AND tabname LIKE l_search.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'TABNAME'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_CDS'
      value_org   = 'S'
    TABLES
      value_tab   = lt_cds.

ENDFORM.

FORM callback_f4_sel TABLES record_tab STRUCTURE seahlpres
          CHANGING shlp TYPE shlp_descr
                   callcontrol LIKE ddshf4ctrl.

  LOOP AT shlp-interface ASSIGNING FIELD-SYMBOL(<interface>) WHERE f4field NE abap_true.
    READ TABLE zcl_sde_appl=>mt_sel WITH KEY field_label = <interface>-shlpfield INTO DATA(l_sel).
    IF sy-subrc = 0.
      LOOP AT l_sel-range INTO DATA(l_range).
        APPEND INITIAL LINE TO shlp-selopt ASSIGNING FIELD-SYMBOL(<searchsel>).
        MOVE-CORRESPONDING l_range TO <searchsel>.
        <searchsel>-shlpfield = <interface>-shlpfield.
        <searchsel>-option = 'EQ'.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM callback_f4_tab TABLES record_tab STRUCTURE seahlpres
          CHANGING shlp TYPE shlp_descr
                   callcontrol LIKE ddshf4ctrl.

  FIELD-SYMBOLS: <field> TYPE any,
                 <row>   TYPE any.
  CHECK zcl_sde_appl=>gr_current_row IS BOUND.
  ASSIGN zcl_sde_appl=>gr_current_row->* TO <row>. "7.50: can't deref a generic ref inline in ASSIGN COMPONENT
  LOOP AT shlp-interface ASSIGNING FIELD-SYMBOL(<interface>) WHERE f4field NE abap_true.
    ASSIGN COMPONENT <interface>-shlpfield OF STRUCTURE <row> TO <field>.
    IF sy-subrc = 0.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <field> shlpfield = <interface>-shlpfield  ) TO shlp-selopt.
    ENDIF.
  ENDLOOP.
ENDFORM.

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.16.7 - 2026-07-15T14:03:19.104Z
  CONSTANTS c_merge_timestamp TYPE string VALUE `2026-07-15T14:03:19.104Z`.
  CONSTANTS c_abapmerge_version TYPE string VALUE `0.16.7`.
ENDINTERFACE.
****************************************************
