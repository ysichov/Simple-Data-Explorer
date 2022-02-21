*&---------------------------------------------------------------------*
*& Simple SQL Explorer
*&---------------------------------------------------------------------*
*& version: beta 0.7.252.202
*& GIT:            https://github.com/ysichov/SDE/blob/master/SDE_750.abap - here may be most actual version
*& AbapGit         https://github.com/ysichov/SDE_abapgit
*& RU description  https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/
*& EN description  https://blogs.sap.com/2020/03/22/simple-data-explorer/

*& Multi-windows program for viewing tables, views, salary clusters, CDS and some links between them
*& Written by Yurii Sychov
*& e-mail:   ysichov@gmail.com
*& skype:    ysichov
*& blog:     https://ysychov.wordpress.com/blog/
*& LinkedIn: https://www.linkedin.com/in/ysychov/
*&---------------------------------------------------------------------*
*& External resources
*& https://github.com/bizhuka/eui - ALV listboxes

REPORT z_sde.

FIELD-SYMBOLS: <g_str> TYPE any.

CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.

CLASS lcl_types DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF selection_display_s,
        ind         TYPE        i,
        field_label TYPE        lvc_fname,
        int_type(1),
        inherited   TYPE        aqadh_type_of_icon,
        emitter     TYPE        aqadh_type_of_icon,
        sign        TYPE        tvarv_sign,
        opti        TYPE        tvarv_opti,
        option_icon TYPE        aqadh_type_of_icon,
        low         TYPE        string,
        high        TYPE        string,
        more_icon   TYPE        aqadh_type_of_icon,
        range       TYPE        aqadh_t_ranges,
        name        TYPE        reptext,
        element     TYPE        text60,
        domain      TYPE        text60,
        datatype    TYPE        string,
        length      TYPE        i,
        transmitter TYPE REF TO lcl_data_transmitter,
        receiver    TYPE REF TO lcl_data_receiver,
        color       TYPE        lvc_t_scol,
        style       TYPE        lvc_t_styl,
        drop_down   TYPE        int4,
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

    CLASS-DATA: mt_sel TYPE TABLE OF lcl_types=>selection_display_s.
ENDCLASS.

CLASS lcl_table_viewer DEFINITION DEFERRED.
CLASS lcl_box_handler  DEFINITION DEFERRED.
CLASS lcl_sel_opt DEFINITION DEFERRED.

"Begin of INCLUDE YS_SDE_CLASSES.
CLASS lcl_popup DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA m_counter TYPE i.
    DATA: mo_box            TYPE REF TO cl_gui_dialogbox_container,
          mo_splitter       TYPE REF TO cl_gui_splitter_container,
          mo_parent         TYPE REF TO cl_gui_container,
          m_additional_name TYPE        string.

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
          SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF  TABLE <f_tab> UP TO 100 ROWS WHERE (i_where) ORDER BY PRIMARY KEY
           .
        CATCH cx_sy_dynamic_osql_semantics.             "#EC NO_HANDLER
        CATCH cx_sy_dynamic_osql_syntax.                "#EC NO_HANDLER
        CATCH cx_sy_conversion_no_number.               "#EC NO_HANDLER
      ENDTRY.
    ELSE.
      IF i_row_count IS NOT SUPPLIED.
        SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF TABLE <f_tab> UP TO 100 ROWS ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF TABLE <f_tab> UP TO i_row_count ROWS ORDER BY PRIMARY KEY..
      ENDIF.
    ENDIF.
    c_count = sy-dbcnt.
  ENDMETHOD.

  METHOD exist_table.
    SELECT SINGLE tabname
       FROM dd02l
      INTO @data(lv_tabname)
     WHERE tabname = @i_tab
       AND ( tabclass = 'TRANSP' OR tabclass = 'CLUSTER' ).
    IF sy-subrc = 0.
      e_subrc = 1.
    ENDIF.
  ENDMETHOD.

  METHOD exist_view.

    SELECT SINGLE tabname
       FROM dd02l
      INTO @data(lv_tabname)
     WHERE tabname = @i_tab
       AND tabclass = 'VIEW'.
    IF sy-subrc = 0.
      e_subrc = 1.
    ENDIF.

  ENDMETHOD.

  METHOD exist_cds.

    SELECT SINGLE tabname
       FROM dd02l
      INTO @data(lv_tabname)
     WHERE tabname = @i_tab
       AND tabclass = 'VIEW'
       AND applclass = 'SDGV'.
.
    IF sy-subrc = 0.
      e_subrc = 1.
    ENDIF.

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
    DATA l_stable TYPE lvc_s_stbl VALUE 'XX'.

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
                  iv_field_name        TYPE csequence OPTIONAL
                  iv_data              TYPE any OPTIONAL
                  is_sh_field          TYPE dfies OPTIONAL
                  ir_unique_type       TYPE REF TO tt_unique_type OPTIONAL
        RETURNING VALUE(rs_field_desc) TYPE ts_field_desc,

      create_type_descr IMPORTING iv_rollname    TYPE csequence OPTIONAL
                                  is_field_desc  TYPE ts_field_desc OPTIONAL
                                  VALUE(ir_type) TYPE REF TO data OPTIONAL
                        RETURNING
                                  VALUE(ro_type) TYPE REF TO cl_abap_datadescr,

      find_drop_down IMPORTING io_grid      TYPE REF TO cl_gui_alv_grid
                     CHANGING  cs_fieldcat  TYPE lvc_s_fcat cv_drdn_hndl TYPE i,

      is_list_box IMPORTING iv_tabname   TYPE dfies-tabname iv_fieldname  TYPE dfies-fieldname
                  EXPORTING ev_list_box  TYPE abap_bool es_sh_desc  TYPE shlp_descr.
ENDCLASS.

CLASS lcl_rtti IMPLEMENTATION.

  METHOD create_struc_handle.
    DATA: ls_comp       TYPE          abap_componentdescr,
          lt_components TYPE          abap_component_tab,
          lt_field_info TYPE TABLE OF dfies.

    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = DATA(l_texttab) ).
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

          READ TABLE lcl_alv_common=>mt_tabfields INTO DATA(ls_tf) WITH KEY tabname = i_tname fieldname = l_texttab.
          IF sy-subrc NE 0.
            MOVE-CORRESPONDING lt_field_info[ 1 ] TO ls_tf.
            ls_tf-tabname = i_tname.
            ls_tf-fieldname = ls_comp-name.
            ls_tf-is_text = abap_true.
            INSERT ls_tf INTO TABLE lcl_alv_common=>mt_tabfields.
          ENDIF.
        ENDIF.
      ENDLOOP.

      e_handle  = cl_abap_structdescr=>create( lt_components ).
    ENDIF.
  ENDMETHOD.

  METHOD create_table_by_name.
    create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_handle = data(lo_new_type) ).
    data(lo_new_tab) = cl_abap_tabledescr=>create(
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
      ls_sh_desc        TYPE                   shlp_descr,
      lv_list_box       TYPE                   abap_bool,
      lt_fielddescr     TYPE                   ddfields,
      ls_field          TYPE REF TO            dfies,
      lt_field_desc     TYPE                   tt_field_desc,
      ls_field_desc     TYPE                   ts_field_desc,
      lo_struc          TYPE REF TO            cl_abap_structdescr,
      lo_table          TYPE REF TO            cl_abap_tabledescr,
      lr_table          TYPE REF TO            data,
      lt_shlp_return    TYPE STANDARD TABLE OF ddshretval,
      ls_shlp_return    TYPE REF TO            ddshretval,
      lv_prev_pos       TYPE                   i,
      ls_call_control   TYPE                   ddshf4ctrl,
      ls_fld_prop       TYPE REF TO            ddshfprop,
      lt_shlp_descr_tab TYPE                   shlp_desct,
      lt_shlp_record    TYPE STANDARD TABLE OF seahlpres,
      lt_dropdown       TYPE                   lvc_t_dral,
      ls_dropdown       TYPE                   lvc_s_dral.

    FIELD-SYMBOLS:
      <lt_table> TYPE STANDARD TABLE,
      <ls_row>   TYPE                 any,
      <lv_value> TYPE                 any,
      <lv_low>   TYPE                 any,
      <lv_txt>   TYPE                 csequence.

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
      ls_header       TYPE        x030l,
      lr_table_descr  TYPE REF TO cl_abap_tabledescr,
      lr_struct_descr TYPE REF TO cl_abap_structdescr,
      lv_cnt          TYPE        i,
      lr_row          TYPE REF TO data,
      lo_type         TYPE REF TO cl_abap_typedescr,
      lt_sub_fdesc    TYPE        tt_field_desc,
      ls_subfield     TYPE        ts_field_desc.
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
      lv_rollname TYPE                   rollname,
      lt_dd03l    TYPE STANDARD TABLE OF ts_dd03l,
      ls_dd03l    TYPE REF TO            ts_dd03l,
      lv_tabfld   TYPE                   string,
      ls_dd04t    TYPE                   dd04t,
      lo_type     TYPE REF TO            cl_abap_datadescr.
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
      <ls_field_desc> TYPE         ts_field_desc,
      <ls_subfield>   TYPE         ts_field_desc,
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
      lv_sys_type TYPE        abap_typekind,
      lv_message  TYPE        string.

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

CLASS lcl_appl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF sign_option_icon_s,
             sign          TYPE tvarv_sign,
             option        TYPE tvarv_opti,
             icon_name(64) TYPE c,
             icon          TYPE aqadh_type_of_icon,
           END OF sign_option_icon_s,

           BEGIN OF t_obj,
             alv_viewer TYPE REF TO lcl_table_viewer,
           END OF t_obj,

           BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang  .

    CLASS-DATA: m_option_icons     TYPE TABLE OF sign_option_icon_s,
                mt_lang            TYPE TABLE OF t_lang,
                mt_obj             TYPE TABLE OF t_obj, "main object table
                m_ctrl_box_handler TYPE REF TO   lcl_box_handler,
                c_dragdropalv      TYPE REF TO   cl_dragdrop.

    CLASS-METHODS:
      init_icons_table,
      init_lang,
      suppress_run_button,
      open_int_table IMPORTING it_tab  TYPE ANY TABLE OPTIONAL
                               it_ref  TYPE REF TO data OPTIONAL
                               iv_name TYPE string,
      exit.
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
          m_from_field   TYPE        lvc_fname,
          m_to_field     TYPE        lvc_fname.
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
    DATA: mo_viewer  TYPE REF TO   lcl_table_viewer,
          mo_sel_alv TYPE REF TO   cl_gui_alv_grid,
          mt_fcat    TYPE          lvc_t_fcat,
          mt_sel_tab TYPE TABLE OF lcl_types=>selection_display_s,
          ms_layout  TYPE          lvc_s_layo.

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
             column  TYPE        lvc_fname,
             emitter TYPE REF TO lcl_data_transmitter,
           END OF t_column_emitter.

    DATA: m_lang             TYPE          ddlanguage,
          m_is_sql           TYPE          xfeld,
          m_is_view          TYPE          xfeld,
          m_is_cds           TYPE          xfeld,
          m_tabname          TYPE          tabname,
          m_texttabname      TYPE          tabname,
          m_count            TYPE          i,
          mo_alv             TYPE REF TO   cl_gui_alv_grid,
          mo_sel             TYPE REF TO   lcl_sel_opt,
          mr_table           TYPE REF TO   data,
          mr_text_table      TYPE REF TO   data,
          mo_sel_parent      TYPE REF TO   cl_gui_container,
          mo_alv_parent      TYPE REF TO   cl_gui_container,
          mt_alv_catalog     TYPE          lvc_t_fcat,
          mt_text_components TYPE          abap_component_tab,
          mo_column_emitters TYPE TABLE OF t_column_emitter,
          mo_sel_width       TYPE          i,
          m_visible,
          m_std_tbar         TYPE          x,
          m_show_empty.

    METHODS:
      constructor IMPORTING i_tname           TYPE any OPTIONAL
                            ir_tab            TYPE REF TO data OPTIONAL
                            i_additional_name TYPE string OPTIONAL
                            i_is_view         TYPE xfeld OPTIONAL
                            i_is_cds          TYPE xfeld OPTIONAL,
      get_where RETURNING VALUE(c_where) TYPE string,
      refresh_table FOR EVENT selection_done OF lcl_sel_opt.

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
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                    es_row_no
                    er_event_data,
      on_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid IMPORTING e_object,
      handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
      handle_menu_button  FOR EVENT menu_button OF cl_gui_alv_grid IMPORTING e_object e_ucomm,
      before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no.
ENDCLASS.

CLASS lcl_py_cluster_viewer DEFINITION INHERITING FROM lcl_popup.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_hier,
        anynode   TYPE        string,
        anyparent TYPE        string,
        key       TYPE        salv_de_node_key, "internal tree key
        name      TYPE        string,
        tab_ref   TYPE REF TO data,
        type(1),
      END OF ts_hier,
      tt_hier TYPE TABLE OF ts_hier,
      BEGIN OF t_children,
        item TYPE REF TO lcl_table_viewer,
      END OF t_children.

    DATA: mt_hier     TYPE          tt_hier, " Tree hierarchy
          mo_nodes    TYPE REF TO   cl_salv_nodes,
          mo_node     TYPE REF TO   cl_salv_node,
          mo_events   TYPE REF TO   cl_salv_events_tree,
          mt_empty    TYPE          tt_hier,
          mr_cluster  TYPE REF TO   data, "payru_result,
          m_pernr(8)  TYPE          n,
          m_seqnr(5)  TYPE          n,
          mt_children TYPE TABLE OF t_children.

    DATA :  mo_tree TYPE REF TO cl_salv_tree.
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

CLASS  lcl_py_cluster_viewer IMPLEMENTATION.
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
      READ TABLE lcl_appl=>mt_obj WITH KEY alv_viewer = child-item ASSIGNING FIELD-SYMBOL(<obj>).
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
      DELETE lcl_appl=>mt_obj INDEX l_indx.
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
          l_struc_name TYPE        tabname,
          lr_tab       TYPE REF TO data.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                   <str>   TYPE                 any.

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
      lcl_rtti=>create_table_by_name( EXPORTING i_tname = l_struc_name CHANGING c_table = lr_tab  ).
      ASSIGN lr_tab->* TO <table>.
      ASSIGN ls_hier-tab_ref->* TO <str>.
      APPEND INITIAL LINE TO <table> ASSIGNING FIELD-SYMBOL(<line>).
      MOVE-CORRESPONDING <str> TO <line>.
      ls_hier-tab_ref = lr_tab.
    ENDIF.
    ls_hier-type = go_abapstr->type_kind.
    DATA(l_name) = |{ m_pernr }: ({ m_seqnr }) |.
    REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_struc_name WITH ''.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_struc_name ir_tab = ls_hier-tab_ref i_additional_name = l_name.
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
    APPEND VALUE #( item = <obj>-alv_viewer  ) TO mt_children.
  ENDMETHOD.

  METHOD read_cluster.
    DATA: lo_handle TYPE REF TO cl_abap_complexdescr.

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
    DATA: lo_stru    TYPE REF TO  cl_abap_structdescr,
          lo_element TYPE REF TO  cl_abap_structdescr,
          ls_hier    LIKE LINE OF mt_hier,
          l_lines    TYPE         i.

    FIELD-SYMBOLS: <table>   TYPE ANY TABLE,
                   <struc>   TYPE            any,
                   <cluster> TYPE            any.

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

CLASS lcl_text_viewer DEFINITION FINAL INHERITING FROM lcl_popup.
  PUBLIC SECTION.
    DATA: mo_text TYPE REF TO cl_gui_textedit.

    METHODS: constructor IMPORTING io_viewer TYPE REF TO lcl_table_viewer,
      load_text  IMPORTING io_viewer TYPE REF TO lcl_table_viewer.

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
*    DATA(l_row) = lcl_alv_common=>get_selected( io_viewer->mo_alv ).
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

    CLASS-DATA: mt_field_links TYPE TABLE OF t_field_links,
                mt_el_links    TYPE TABLE OF t_el_links.
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

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD TABLE.
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
    DATA: it_bdcdata    TYPE TABLE OF bdcdata,
          save_plvar(2),
          save_otype(2),
          save_objid(8),
          l_infty(4),
          l_subty(4),
          l_temp(10).

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD TABLE.
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
    NEW lcl_text_viewer( io_viewer ).
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

      DATA: lv_struc TYPE t77ar,
            lv_dbtab TYPE t77ad.
      SELECT SINGLE pasub INTO lv_struc FROM t77ar WHERE relat = <relat>.
      SELECT SINGLE dbtab INTO lv_dbtab FROM t77ad WHERE pasub = lv_struc.

      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
        CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = lv_dbtab.
        <obj>-alv_viewer->mo_sel->set_value( i_field = 'ADATANR' i_low = <datanr>  ).
        <obj>-alv_viewer->mo_sel->raise_selection_done( ).
        is_done = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD run_subty.
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD TABLE.
    DATA(l_row) = lcl_alv_common=>get_selected( io_viewer->mo_alv ).
    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<str>).

    DATA l_result TYPE t777d.

    SELECT SINGLE stypt, namst INTO @l_result   FROM t777d WHERE dbtab = @io_viewer->m_tabname.
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
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD TABLE.
    DATA(l_row) = lcl_alv_common=>get_selected( io_viewer->mo_alv ).
    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<str>).

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <str> TO FIELD-SYMBOL(<pernr>).
    ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <str> TO FIELD-SYMBOL(<seqnr>).
    NEW lcl_py_cluster_viewer( i_pernr = <pernr> i_seqnr = <seqnr> ).
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
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD TABLE.

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
    DATA: lt_sel_row TYPE lcl_types=>t_sel_row.
    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE                 any.

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

CLASS lcl_box_handler DEFINITION."for memory clearing
  PUBLIC SECTION.
    METHODS: on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.
ENDCLASS.

CLASS lcl_box_handler IMPLEMENTATION.
  METHOD on_box_close.
    sender->free( ).

    "Free Memory
    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
      IF <obj>-alv_viewer->mo_box = sender.
        data(lv_tabix) = sy-tabix.
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
      DELETE lcl_appl=>mt_obj INDEX lv_tabix.
    ENDIF.
  ENDMETHOD.                    "ON_BOX_CLOSE
ENDCLASS.               "lcl_box_handler

CLASS lcl_table_viewer IMPLEMENTATION.

  METHOD constructor.

    DATA: ls_comp         TYPE        abap_componentdescr,
          lt_comp_notab   TYPE        abap_component_tab,
          lt_comp_tab2str TYPE        abap_component_tab,
          lt_comp_str     TYPE        abap_component_tab,
          lv_s            TYPE        string,
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

    lcl_ddic=>get_text_table( EXPORTING i_tname = m_tabname IMPORTING e_tab = m_texttabname ).
    IF m_texttabname IS NOT INITIAL.
      get_field_info( m_texttabname ).
    ENDIF.
    get_field_info( m_tabname ).
    IF ir_tab IS NOT BOUND.
      lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table  ).
      IF m_is_view IS INITIAL.
        m_is_sql = abap_true.
      ENDIF.
    ELSE.

      FIELD-SYMBOLS:<any> TYPE any.
      ASSIGN ir_tab->* TO <any>.
      DATA lo_tabl TYPE REF TO cl_abap_tabledescr.
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
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD TABLE.

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
      ELSE.
        read_text_table( ).
        lcl_sql=>read_any_table( EXPORTING i_tabname = m_tabname i_where = get_where( ) i_row_count = 100
                             CHANGING cr_tab =  mr_table c_count = m_count ).
        update_texts( ).

      ENDIF.
    ENDIF.
    set_header( ).
    ls_layout-col_opt = abap_true.
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
                  handle_menu_button
                  handle_tab_toolbar
                  handle_doubleclick
                  lcl_dragdrop=>drag
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
      lcl_alv_common=>translate_field(  CHANGING c_fld = <cat> ).
    ENDLOOP.

    mo_alv->set_frontend_fieldcatalog( EXPORTING  it_fieldcatalog = mt_alv_catalog ).
    me->handle_user_command( EXPORTING e_ucomm = 'HIDE' ).
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
    lcl_ddic=>get_text_table( EXPORTING i_tname =  m_tabname IMPORTING e_tab = DATA(l_tab) ).
    CHECK l_tab IS NOT INITIAL.
    lcl_rtti=>create_table_by_name( EXPORTING i_tname = l_tab CHANGING c_table = mr_text_table ).
    ASSIGN mr_text_table->* TO <f_tab>.
    SELECT * FROM (l_tab) INTO TABLE <f_tab> ORDER BY PRIMARY KEY.
  ENDMETHOD.

  METHOD set_header.
    DATA: lv_text       TYPE as4text,
          lv_header(80) TYPE c.

    SELECT SINGLE ddtext INTO lv_text
      FROM dd02t
     WHERE tabname = m_tabname
       AND ddlanguage = m_lang.

    lv_header = |{ m_tabname } - { lv_text } ({ m_count }) { m_additional_name }|.
    mo_box->set_caption( lv_header ).
  ENDMETHOD.

  METHOD on_f4.
    FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <tab>.
    READ TABLE <tab> INDEX es_row_no-row_id ASSIGNING <g_str>.
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

    DATA l_dbtab TYPE t777d.
    SELECT SINGLE dbtab INTO @l_dbtab
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
      DATA: lt_toolbar TYPE ttb_button,
            ls_toolbar TYPE stb_button.
      ls_toolbar-function = 'SEL_ON'.
      ls_toolbar-icon = icon_arrow_left.
      ls_toolbar-quickinfo = 'Select-Options'.
      ls_toolbar-butn_type = 0.
      APPEND ls_toolbar TO lt_toolbar.

      CLEAR ls_toolbar.
      ls_toolbar-butn_type = 3.
      APPEND ls_toolbar TO lt_toolbar.
    ENDIF.

    ls_toolbar-function = 'REFRESH'.
    ls_toolbar-icon = icon_refresh.
    ls_toolbar-quickinfo = 'Refresh'.
    ls_toolbar-butn_type = 0.
    APPEND ls_toolbar TO lt_toolbar.


    ls_toolbar-function = 'LANGUAGE'.
    ls_toolbar-icon = icon_foreign_trade.
    ls_toolbar-quickinfo = 'Languages'.
    ls_toolbar-butn_type = 2.
    APPEND ls_toolbar TO lt_toolbar.

    ls_toolbar-function = 'OPTIONS'.
    ls_toolbar-icon  = icon_list.
    ls_toolbar-quickinfo = 'Empty columns options'.
    APPEND ls_toolbar TO lt_toolbar.

    ls_toolbar-function = 'TABLES'.
    ls_toolbar-icon  = icon_net_graphic.
    ls_toolbar-quickinfo = 'Table links'.
    ls_toolbar-butn_type = 0.
    APPEND ls_toolbar TO lt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO lt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = 'TBAR'.
    ls_toolbar-butn_type = 0.

    IF m_std_tbar IS INITIAL.
      ls_toolbar-icon  = icon_column_right.
      ls_toolbar-quickinfo = 'Show standard ALV function'.

    ELSE.
      ls_toolbar-icon  = icon_column_left.
      ls_toolbar-quickinfo = 'Hide standard ALV functio'.

    ENDIF.
    APPEND ls_toolbar TO lt_toolbar.

    IF m_std_tbar IS INITIAL.
      e_object->mt_toolbar =  lt_toolbar.
    ELSE.
      APPEND LINES OF e_object->mt_toolbar TO lt_toolbar.
    ENDIF.

    e_object->mt_toolbar = lt_toolbar.

  ENDMETHOD.

  METHOD get_field_info.
    DATA: lv_clause      TYPE          string,
          lr_struc       TYPE REF TO   data,
          lr_table_descr TYPE REF TO   cl_abap_structdescr,
          it_tabdescr    TYPE          abap_compdescr_tab,
          lt_field_info  TYPE TABLE OF dfies,
          l_fname        TYPE          fieldname,
          l_tname        TYPE          tabname,
          ls_tf          LIKE LINE OF  lcl_alv_common=>mt_tabfields,
          dref           TYPE REF TO   data,
          l_x            TYPE          xstring.

    CREATE DATA lr_struc TYPE (i_tab).
    lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_struc ).
    it_tabdescr[] = lr_table_descr->components[].

*    DATA(l_exist) = lcl_sql=>exist_table( i_tab ).
*    IF  l_exist = 1.
*      SELECT  COUNT( * ) FROM (i_tab).
*      DATA(l_count) = sy-dbcnt.
*    ENDIF.

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
*        IF l_exist = 1 AND l_count < 10000.
*          IF ls_tf-rollname IS NOT INITIAL.
*            CREATE DATA dref TYPE (ls_tf-rollname).
*            ASSIGN dref->* TO FIELD-SYMBOL(<field>).
*            lv_clause = |{ ls_tf-fieldname } NE ''|.
*            SELECT SINGLE (ls_tf-fieldname) INTO @<field>
*              FROM (i_tab)
*             WHERE (lv_clause).
*            IF sy-subrc NE 0.
*              ls_tf-empty = abap_true.
*            ENDIF.
*          ELSEIF ls_tf-datatype = 'RAWSTRING'.
*            lv_clause = |{ ls_tf-fieldname } NE ''|.
*            SELECT SINGLE (ls_tf-fieldname) INTO @l_x
*              FROM (i_tab)
*             WHERE (lv_clause).
*            IF sy-subrc NE 0.
*              ls_tf-empty = abap_true.
*            ENDIF.
*          ENDIF.
*        ENDIF.
        INSERT ls_tf INTO TABLE lcl_alv_common=>mt_tabfields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_field_cat.
    DATA: lr_struc       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          it_tabdescr    TYPE        abap_compdescr_tab,
          l_replace      TYPE        string,
          l_texttab      TYPE        tabname,
          lo_str         TYPE REF TO cl_abap_structdescr.

    lcl_rtti=>create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_t_comp = mt_text_components e_handle = lo_str ).
    CREATE DATA lr_struc TYPE HANDLE lo_str.
    lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_struc ).
    it_tabdescr[] = lr_table_descr->components[].
    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = l_texttab ).
    l_replace = l_texttab && '_'.

    LOOP AT it_tabdescr INTO DATA(ls) WHERE name NE 'MANDT' AND name NE 'CLIENT'.
      DATA(l_ind) = sy-tabix.
      APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      <catalog>-col_pos = l_ind.
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
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_menu_button.
    CALL METHOD cl_gui_cfw=>flush.
    IF e_ucomm = 'LANGUAGE'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TECH'
          text  = 'Technical name'. "Teхническое имя
      LOOP AT lcl_appl=>mt_lang INTO DATA(ls_lang).
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

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD TABLE.
    CHECK es_row_no-row_id IS NOT INITIAL.
    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    lcl_plugins=>link( EXPORTING i_str = <tab> i_column = e_column io_viewer = me ).

    ASSIGN COMPONENT |{ e_column-fieldname }_REF| OF STRUCTURE <tab> TO FIELD-SYMBOL(<ref>).
    IF sy-subrc = 0.
      lcl_appl=>open_int_table( EXPORTING iv_name = CONV #( e_column-fieldname ) it_ref = <ref> ).
    ELSE.
*      TRY.
*          lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ 1 ]-{ e_column-fieldname }| ).
*          table_clone = lo_table_descr->elem_clone( ).
*          lcl_appl=>open_int_table( EXPORTING iv_name = |{ m_additional_name }[ 1 ]-{ e_column-fieldname }| it_ref = table_clone ).
*        CATCH cx_sy_move_cast_error.
*      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD before_user_command.
    CASE e_ucomm.
      WHEN '&INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_user_command.
    DATA:
      it_fields     TYPE lvc_t_fcat,
      lv_clause(45),
      lv_sel_width  TYPE i.

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD TABLE.

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
      READ TABLE <f_tab> INDEX lcl_alv_common=>get_selected( mo_alv ) ASSIGNING FIELD-SYMBOL(<f_line>).
      lcl_plugins=>run_hrpy_rgdir( <f_line> ).
    ELSEIF e_ucomm = 'DETAIL'.
      IF m_tabname+0(2) = 'PA'.
        lcl_plugins=>run_pa20( me ).
      ELSEIF m_tabname+0(3) = 'HRP'.
        lcl_plugins=>run_pp01( me ).
      ENDIF.
    ELSEIF e_ucomm = 'REFRESH'.
      "CHECK get_where( ) IS NOT INITIAL.
      mo_sel->raise_selection_done( ).
      IF lcl_sql=>exist_table( m_tabname ) = 1.
        m_is_sql = 'X'.
      ENDIF.
    ELSEIF e_ucomm = 'TBAR'.
      m_std_tbar = BIT-NOT  m_std_tbar.
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
            IF m_show_empty = abap_false.
              <fields>-no_out = ' '.
            ELSE.
              lv_clause = |{ <fields>-fieldname } IS NOT INITIAL|.
              LOOP AT <f_tab> ASSIGNING <f_line>  WHERE (lv_clause).
                EXIT.
              ENDLOOP.
              IF sy-subrc NE 0.
                <fields>-no_out = abap_true.
              ENDIF.
            ENDIF.
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
            IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
              lcl_alv_common=>translate_field( EXPORTING i_lang = CONV #( e_ucomm ) CHANGING c_fld = <fields> ).
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

    IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
      m_lang = e_ucomm.
      set_header( ).
      update_texts( ).
      mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang  ).
    ENDIF.

    CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].

    IF e_ucomm = 'TBAR'.
      RETURN.
    ENDIF.

    lcl_alv_common=>refresh( mo_alv ).
    IF mo_sel IS BOUND.
      IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
        mo_sel->update_sel_tab( ).
      ENDIF.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
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
    DATA: ls_row    TYPE lcl_types=>t_sel_row,
          lt_filter TYPE lvc_t_filt.
    IF m_is_sql = abap_true.
      DATA(l_where) = get_where( ).
      lcl_sql=>read_any_table( EXPORTING i_tabname = m_tabname i_where = l_where CHANGING cr_tab =  mr_table c_count = m_count ).
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

    lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
    lcl_alv_common=>refresh( mo_alv ).
    mo_sel->mo_viewer->handle_user_command( 'UPDATE' ).
    LOOP AT mo_column_emitters INTO DATA(l_emit).
      l_emit-emitter->emit_col( l_emit-column ).
    ENDLOOP.
  ENDMETHOD.

  METHOD update_texts.
    DATA: l_text_field TYPE fieldname,
          l_replace    TYPE string,
          lv_clause    TYPE string.

    FIELD-SYMBOLS: <f_tab> TYPE ANY TABLE.
    FIELD-SYMBOLS: <text_tab> TYPE STANDARD TABLE,
                   <check>    TYPE                 any.

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
ENDCLASS.

CLASS lcl_sel_opt IMPLEMENTATION.
  METHOD constructor.
    DATA: effect     TYPE i,
          handle_alv TYPE i.

    mo_viewer = io_viewer.
    mo_sel_alv = NEW #( i_parent = io_container ).
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
    lcl_alv_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    DATA: ls_row TYPE lcl_types=>t_sel_row.
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
      READ TABLE lcl_alv_common=>mt_tabfields WITH KEY tabname = l_catalog-tabname fieldname = l_catalog-fieldname  INTO DATA(l_tfield).
      IF l_tfield-empty = '' OR mo_viewer->m_show_empty IS NOT INITIAL OR mo_viewer->m_count = 0.
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

        lcl_rtti=>find_drop_down(
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
      DATA: ls_row TYPE lcl_types=>t_sel_row.
      MOVE-CORRESPONDING <to> TO ls_row.
      <to>-transmitter->emit( EXPORTING e_row = ls_row ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_doubleclick.
    DATA: it_bdcdata TYPE TABLE OF bdcdata.
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
          lt_objec   TYPE TABLE OF          objec,
          ls_objec   TYPE                   objec,
          l_otype    TYPE                   otype,
          l_plvar    TYPE                   plvar,
          l_multiple TYPE                   xfeld,
          l_clear    TYPE                   xfeld.

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
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW #(  i_additional_name = iv_name ir_tab = r_tab ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).

  ENDMETHOD.

  METHOD exit.
    DATA: l_answer.
    DESCRIBE TABLE lcl_appl=>mt_obj.
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
                     <f_field> TYPE                 any.
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
      lcl_alv_common=>refresh( EXPORTING i_obj = lo_alv i_soft = abap_true ).
    ENDIF.

    lo_alv ?= e_dragdropobj->droptargetctrl.
    lo_to->raise_selection_done( ).
  ENDMETHOD.
ENDCLASS.

*------------REPORT EVENTS--------------------
TABLES sscrfields.
DATA: g_mode TYPE i VALUE 1.
"selection-screen begin of screen 101.
SELECTION-SCREEN: FUNCTION KEY 1."Tables
SELECTION-SCREEN: FUNCTION KEY 2."Views
SELECTION-SCREEN: FUNCTION KEY 3."CDS

PARAMETERS: gv_tname TYPE tabname VISIBLE LENGTH 15 MATCHCODE OBJECT dd_bastab_for_view MODIF ID tab.
PARAMETERS: gv_vname TYPE tabname VISIBLE LENGTH 15 MATCHCODE OBJECT viewmaint MODIF ID vie.
PARAMETERS: gv_cds   TYPE tabname VISIBLE LENGTH 15 MODIF ID cds.
"selection-screen end of screen 101.

INITIALIZATION.


  lcl_appl=>init_lang( ).
  lcl_appl=>init_icons_table( ).
  lcl_plugins=>init( ).
  sscrfields-functxt_01 = 'Tables'.
  sscrfields-functxt_02 = 'Views'.
  sscrfields-functxt_03 = 'CDS'.
  " call screen 101.

AT SELECTION-SCREEN OUTPUT.
  %_gv_tname_%_app_%-text = 'Enter Table name and hit Enter'.
  %_gv_vname_%_app_%-text = 'Enter View name and hit Enter'.
  %_gv_cds_%_app_%-text = 'Enter CDS name and hit Enter'.
  lcl_appl=>suppress_run_button( ).

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
  lcl_appl=>exit( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR gv_cds.
  PERFORM search_cds.

AT SELECTION-SCREEN .
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
    CHECK lcl_sql=>exist_table( gv_tname ) = 1.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = gv_tname.
  ENDIF.

  IF g_mode = 2. "view
    CONDENSE gv_vname.
    CHECK lcl_sql=>exist_view( gv_vname ) = 1.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING <obj>.
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = gv_vname i_is_view = abap_true.
  ENDIF.

  IF g_mode = 3. "CDS
    CONDENSE gv_cds.
    CHECK lcl_sql=>exist_cds( gv_cds ) = 1.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING <obj>.
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = gv_cds i_is_cds = abap_true.
  ENDIF.

FORM search_cds.
  TYPES: BEGIN OF t_cds,
           tabname TYPE tabname,
         END OF t_cds.

  DATA: lt_cds        TYPE TABLE OF t_cds,
        l_search      TYPE          string,
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
    READ TABLE lcl_types=>mt_sel WITH KEY field_label = <interface>-shlpfield INTO DATA(l_sel).
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

  FIELD-SYMBOLS: <field> TYPE any.
  LOOP AT shlp-interface ASSIGNING FIELD-SYMBOL(<interface>) WHERE f4field NE abap_true.
    ASSIGN COMPONENT <interface>-shlpfield OF STRUCTURE <g_str> TO <field>.
    IF sy-subrc = 0.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <field> shlpfield = <interface>-shlpfield  ) TO shlp-selopt.
    ENDIF.
  ENDLOOP.
ENDFORM.
