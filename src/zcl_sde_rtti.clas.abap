CLASS zcl_sde_rtti DEFINITION PUBLIC CREATE PUBLIC.
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
                  EXPORTING ev_list_box	TYPE abap_bool es_sh_desc	TYPE shlp_descr.
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
