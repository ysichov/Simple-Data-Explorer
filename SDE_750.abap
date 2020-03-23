*&---------------------------------------------------------------------*
*& Report YS_SDE - Simple Data Explorer
*&---------------------------------------------------------------------*
*& version: beta 0.5.200.160
*& GIT:       https://github.com/ysichov/SDE/blob/master/SDE%20for%207.50%20abap - here may be most actual version
*& Multi-windows program for viewing tables and links between them
*& Written by Yurii Sychov
*& e-mail:   ysichov@gmail.com
*& skype:    ysichov
*& blog:     https://ysychov.wordpress.com/blog/
*& LinkedIn: https://www.linkedin.com/in/ysychov/
*&---------------------------------------------------------------------*
REPORT ys_sde.
CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.

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
    low         TYPE aqadh_range_value,
    high        TYPE aqadh_range_value,
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
    low         TYPE aqadh_range_value,
    high        TYPE aqadh_range_value,
    more_icon   TYPE aqadh_type_of_icon,
    range       TYPE aqadh_t_ranges,
  END OF t_sel_row.

DATA: gt_sel TYPE TABLE OF selection_display_s.
FIELD-SYMBOLS: <g_str> TYPE any.

PARAMETERS: gv_tname TYPE tabname VISIBLE LENGTH 15 MATCHCODE OBJECT dd_bastab_for_view.

"Begin of INCLUDE YS_SDE_CLASSES.
CLASS lcl_plugins DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_field_links,
             tab    TYPE tablename,
             field  TYPE fieldname,
             rtab   TYPE tablename,
             rfield TYPE fieldname,
             const  TYPE aqadh_range_value,
           END OF t_field_links,
           BEGIN OF t_el_links,
             element TYPE tablename,
             rtab    TYPE tablename,
             rfield  TYPE fieldname,
             tcode   TYPE tcode,
           END OF t_el_links.

    CLASS-DATA: mt_field_links TYPE  TABLE OF t_field_links,
                mt_el_links    TYPE  TABLE OF t_el_links.
    CLASS-METHODS: init.
ENDCLASS.

CLASS lcl_plugins IMPLEMENTATION.
  METHOD init.
    "data elements links
    mt_el_links = VALUE #(
      ( element = 'PERSNO'   tcode = 'PA20' )
      ( element = 'HROBJID'  tcode = 'PP01' )
      ( element = 'HROBJID'  rtab = 'HRP1000' rfield = 'OTYPE' )
      ( element = 'LGART'    rtab = 'T512W'   rfield = 'LGART' )
      ).

    "field to field links
    mt_field_links = VALUE #(
      ( tab = 'PA0001'    field = 'PLANS' rtab = 'HRP1000' rfield = 'OTYPE' const = 'S' )
      ( tab = 'PA0001'    field = 'PLANS' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'PA0001'    field = 'ORGEH' rtab = 'HRP1000' rfield = 'OTYPE' const = 'O' )
      ( tab = 'PA0001'    field = 'ORGEH' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'PA0001'    field = 'STELL' rtab = 'HRP1000' rfield = 'OBJID' const = 'C' )
      ( tab = 'PA0001'    field = 'STELL' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'HRP1001'   field = 'SCLAS' rfield = 'OTYPE' )
      ( tab = 'HRP1001'   field = 'SOBID' rfield = 'OBJID' )
      ( tab = 'HRP1002'   field = 'TABNR' rtab = 'HRT1002'  rfield = 'TABNR' )
      ( tab = 'PA2006'    field = 'QUONR' rtab = 'PTQUODED' rfield = 'QUONR' )
      ( tab = 'PTQUODED'  field = 'QUONR' rtab = 'PA2006'   rfield = 'QUONR' )
      ( tab = 'PTQUODED'  field = 'DOCNR' rtab = 'PA2001'   rfield = 'DOCNR' )
      ).
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

CLASS lcl_table_viewer DEFINITION DEFERRED.
CLASS lcl_box_handler  DEFINITION DEFERRED.
CLASS lcl_sel_opt DEFINITION DEFERRED.

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
      exist_table IMPORTING i_tab LIKE gv_tname RETURNING VALUE(e_subrc) LIKE sy-subrc.
ENDCLASS.

CLASS lcl_sql IMPLEMENTATION.
  METHOD read_any_table.
    FIELD-SYMBOLS: <f_tab> TYPE ANY TABLE.

    ASSIGN cr_tab->* TO <f_tab>.
    IF i_where IS NOT INITIAL.
      TRY.
          SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF  TABLE <f_tab> WHERE (i_where) ORDER BY PRIMARY KEY.
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
    "update_texts( ).
  ENDMETHOD.


  METHOD exist_table.
    SELECT COUNT( * ) FROM dd02l
     WHERE tabname = i_tab
       AND ( tabclass = 'TRANSP' OR tabclass = 'CLUSTER' ).
    e_subrc = sy-dbcnt.
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

CLASS lcl_rtti IMPLEMENTATION.
  METHOD create_struc_handle.
    DATA: lo_texttab    TYPE REF TO cl_abap_structdescr,
          ls_comp       TYPE abap_componentdescr,
          lt_components TYPE abap_component_tab,
          lt_field_info TYPE TABLE OF dfies.

    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = DATA(l_texttab) ).
    e_handle ?= cl_abap_typedescr=>describe_by_name( i_tname ).

    IF l_texttab IS NOT INITIAL.
      lo_texttab  ?= cl_abap_typedescr=>describe_by_name( l_texttab ).
      LOOP AT e_handle->components INTO DATA(l_descr).
        ls_comp-name = l_descr-name.
        ls_comp-type ?= e_handle->get_component_type( ls_comp-name ).
        APPEND ls_comp TO lt_components.
      ENDLOOP.

      "Read table lo_texttab->components with key
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

        READ TABLE lt_field_info INDEX 1 INTO DATA(l_field).
        IF l_field-keyflag = abap_false.
          ls_comp-name =  l_texttab && '_' && l_descr-name.
          ls_comp-type ?= lo_texttab->get_component_type( l_descr-name ).
          APPEND ls_comp TO lt_components.
          APPEND ls_comp TO e_t_comp.
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
ENDCLASS.



CLASS lcl_alv_common DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: c_white(4) TYPE x VALUE '00000001', "white background
               c_grey(4)  TYPE x VALUE '00000003', "white background
               c_green(4) TYPE x VALUE '00000216', "green +underline
               c_blue(4)  TYPE x VALUE '00000209', " blue font +underline
               c_bold(4)  TYPE x VALUE '00000020'.

    TYPES: BEGIN OF t_tabfields.
             INCLUDE TYPE   dfies.
             TYPES: empty TYPE xfeld,
           END OF t_tabfields.
    CLASS-DATA: mt_tabfields TYPE HASHED TABLE OF t_tabfields WITH UNIQUE KEY tabname fieldname.

    CLASS-METHODS:
      refresh IMPORTING i_obj TYPE REF TO cl_gui_alv_grid i_layout TYPE lvc_s_layo OPTIONAL i_soft TYPE char01 OPTIONAL,
      translate_field IMPORTING i_lang TYPE ddlanguage OPTIONAL CHANGING c_fld TYPE lvc_s_fcat,
      get_field_info IMPORTING i_tab TYPE tabname,
      get_selected IMPORTING i_obj TYPE REF TO cl_gui_alv_grid RETURNING VALUE(e_index) TYPE lvc_index.
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

  METHOD get_field_info.
    DATA: lv_clause      TYPE string,
          lr_struc       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          lt_field_info  TYPE TABLE OF dfies,
          l_fname        TYPE fieldname,
          l_tname        TYPE tabname,
          l_replace      TYPE string,
          l_texttab      TYPE tabname,
          lo_str         TYPE REF TO cl_abap_structdescr.

    lcl_rtti=>create_struc_handle( EXPORTING i_tname = i_tab
                                   IMPORTING e_handle = lo_str ).
    CREATE DATA lr_struc TYPE HANDLE lo_str.
    lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_struc ).
    it_tabdescr[] = lr_table_descr->components[].

    LOOP AT it_tabdescr INTO DATA(ls) WHERE name NE 'MANDT'.
      READ TABLE lcl_alv_common=>mt_tabfields INTO DATA(ls_tf) WITH KEY tabname = i_tab fieldname = ls-name.
      IF sy-subrc NE 0.
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
        READ TABLE lt_field_info INDEX 1 INTO DATA(l_info).
        CHECK sy-subrc = 0.
        CLEAR ls_tf.
        MOVE-CORRESPONDING l_info TO ls_tf.

        "check empty field
        DATA: dref TYPE REF TO data,
              l_x  TYPE xstring.
        FIELD-SYMBOLS <field> TYPE any.

        IF ls_tf-rollname IS NOT INITIAL.
          CREATE DATA dref TYPE (ls_tf-rollname).
          ASSIGN dref->* TO <field>.
          lv_clause = |{ ls_tf-fieldname } NE ''|.
          SELECT SINGLE (ls_tf-fieldname) INTO @<field>
            FROM (i_tab)
           WHERE (lv_clause).
          IF sy-subrc NE 0.
            ls_tf-empty = 'X'.
          ENDIF.
        ELSEIF ls_tf-datatype = 'RAWSTRING'.
          lv_clause = |{ ls_tf-fieldname } NE ''|.
          SELECT SINGLE (ls_tf-fieldname) INTO @l_x
            FROM (i_tab)
           WHERE (lv_clause).
          IF sy-subrc NE 0.
            ls_tf-empty = 'X'.
          ENDIF.
        ENDIF.

        INSERT ls_tf INTO TABLE lcl_alv_common=>mt_tabfields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD translate_field.
    DATA: lv_lang       LIKE sy-langu,
          lt_field_info TYPE TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = c_fld-tabname "c_fld-ref_table
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
    READ TABLE lt_sel_cells INTO DATA(l_cells) INDEX 1 TRANSPORTING row_id.
    IF sy-subrc = 0.
      e_index = l_cells-row_id.
    ELSE.
      i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).
      READ TABLE lt_sel_rows INDEX 1 INTO DATA(l_row) TRANSPORTING index.
      IF sy-subrc = 0.
        e_index = l_row-index.
      ENDIF.
    ENDIF.
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
             spras TYPE spras,
             sptxt TYPE sptxt,
           END OF t_lang  .

    CLASS-DATA: m_option_icons     TYPE TABLE OF sign_option_icon_s,
                mt_lang            TYPE TABLE OF t_lang,
                mt_obj             TYPE TABLE OF t_obj, "main object table
                m_ctrl_box_handler TYPE REF TO lcl_box_handler,
                c_dragdropalv      TYPE REF TO cl_dragdrop.

    CLASS-METHODS:
      init_icons_table,
      init_lang,
      suppress_run_button,
      exit.
ENDCLASS.

CLASS lcl_data_transmitter DEFINITION.
  PUBLIC SECTION.
    EVENTS: data_changed EXPORTING VALUE(e_row) TYPE t_sel_row,
             col_changed EXPORTING VALUE(e_column) TYPE lvc_fname.
    METHODS: emit IMPORTING e_row TYPE t_sel_row,
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
          mt_sel_tab TYPE TABLE OF selection_display_s,
          ms_layout  TYPE lvc_s_layo.

    EVENTS: selection_done.
    METHODS:
      constructor IMPORTING io_viewer TYPE REF TO lcl_table_viewer io_container TYPE REF TO cl_gui_container,
      raise_selection_done,
      update_sel_tab,
      set_value IMPORTING  i_field TYPE any i_low TYPE any OPTIONAL i_high TYPE any OPTIONAL i_clear TYPE xfeld DEFAULT 'X' ,
      update_sel_row CHANGING c_sel_row TYPE selection_display_s.

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

CLASS lcl_table_viewer DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_column_emitter,
             column  TYPE lvc_fname,
             emitter TYPE REF TO lcl_data_transmitter,
           END OF t_column_emitter.

    DATA: m_lang             TYPE ddlanguage,
          m_tabname          TYPE tabname,
          m_texttabname      TYPE tabname,
          m_count            TYPE i,
          mo_alv             TYPE REF TO cl_gui_alv_grid,
          mo_sel             TYPE REF TO lcl_sel_opt,
          mo_box             TYPE REF TO cl_gui_dialogbox_container,
          mr_table           TYPE REF TO data,
          mr_text_table      TYPE REF TO data,
          mo_splitter        TYPE REF TO cl_gui_splitter_container,
          mo_sel_parent      TYPE REF TO cl_gui_container,
          mo_alv_parent      TYPE REF TO cl_gui_container,
          mt_alv_catalog     TYPE lvc_t_fcat,
          mt_text_components TYPE abap_component_tab,
          "m_check_domain     TYPE fieldname,
          mo_column_emitters TYPE TABLE OF t_column_emitter,
          mo_sel_width       TYPE i,
          m_visible,
          m_std_tbar         TYPE x,
          m_show_empty.

    METHODS:
      constructor IMPORTING i_tname TYPE any,
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
      link IMPORTING  i_str TYPE any
                      i_column TYPE any RETURNING VALUE(r_done) TYPE xfeld,
      create_field_cat IMPORTING i_tname TYPE tabname RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname es_row_no er_event_data,
      on_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid IMPORTING e_object,
      handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
      handle_menu_button  FOR EVENT menu_button OF cl_gui_alv_grid IMPORTING e_object e_ucomm,
      before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
      jump_pa20,
      jump_pp01.
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
    FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE,
                   <f_field> TYPE any.

    CHECK m_from_field = es_col_id-fieldname.
    ASSIGN lo_tab_from->mr_table->* TO <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <tab> TO  <f_field>.

    CHECK lo_sel_to IS NOT INITIAL.
    lo_sel_to->set_value( i_field = m_to_field i_low = <f_field>  ).
    lo_sel_to->raise_selection_done( ).
  ENDMETHOD.

  METHOD  update.
    DATA: l_updated.
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    IF <to>-range[] = e_row-range[].
      l_updated = 'X'."so as not to have an infinite event loop
    ENDIF.
    MOVE-CORRESPONDING e_row TO <to>.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = e_row ).
    ENDIF.
    lo_sel_to->raise_selection_done( ).
  ENDMETHOD.

  METHOD update_col.
    DATA: l_updated,
          lt_old_range TYPE aqadh_t_ranges.

    DATA: lt_sel_row TYPE t_sel_row.
    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE any.

    CHECK lo_sel_to IS NOT INITIAL.
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    lt_old_range = <to>-range.
    CLEAR: <to>-sign, <to>-opti, <to>-low, <to>-high, <to>-range.
    ASSIGN lo_tab_from->mr_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT e_column OF STRUCTURE <row> TO <field>.
      READ TABLE <to>-range WITH KEY low = <field>  TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
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
      l_updated = 'X'."so as not to have an infinite event loop
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
    DATA: lv_tabix LIKE sy-tabix.
    sender->free( ).

    "Free Memory
    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
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
      DELETE lcl_appl=>mt_obj INDEX lv_tabix.
    ENDIF.
  ENDMETHOD.                    "ON_BOX_CLOSE
ENDCLASS.               "lcl_box_handler

CLASS lcl_table_viewer IMPLEMENTATION.
  METHOD constructor.
    m_lang = sy-langu.
    mo_sel_width = 0.
    m_tabname = i_tname.
    create_popup( ).
    lcl_ddic=>get_text_table( EXPORTING i_tname = m_tabname IMPORTING e_tab = m_texttabname ).
    IF m_texttabname IS NOT INITIAL.
      lcl_alv_common=>get_field_info( m_texttabname ).
    ENDIF.
    lcl_alv_common=>get_field_info( m_tabname ).

    lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table  ).
    create_alv( ).
    create_sel_alv( ).
    mo_alv->set_focus( mo_alv ).
  ENDMETHOD.

  METHOD create_popup.
    DATA: l_top  TYPE i,
          l_left TYPE i.

    DATA(l_lines) = lines( lcl_appl=>mt_obj ) - 1.

    l_top  = 20 + 30 * ( l_lines DIV 5 ) +  ( l_lines MOD 5 ) * 50.
    l_left = 420 + 300 * ( l_lines DIV 5 )  +  ( l_lines MOD 5 ) * 50.

    CREATE OBJECT mo_box
      EXPORTING
        width                       = '800'
        height                      = '150'
        top                         = l_top
        left                        = l_left
        caption                     = m_tabname
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

    CREATE OBJECT mo_splitter
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
    FIELD-SYMBOLS: <f_tab>   TYPE ANY TABLE.

    mo_alv = NEW #( i_parent = mo_alv_parent ).
    mt_alv_catalog = create_field_cat( m_tabname ).
    ASSIGN mr_table->* TO <f_tab>.
    read_text_table( ).

    lcl_sql=>read_any_table( EXPORTING i_tabname = m_tabname i_where = get_where( ) i_row_count = 100
                         CHANGING cr_tab =  mr_table c_count = m_count ).
    update_texts( ).
    set_header( ).
    ls_layout-col_opt = 'X'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-sel_mode = 'D'.
    CREATE OBJECT lcl_appl=>c_dragdropalv.
    effect = cl_dragdrop=>move + cl_dragdrop=>copy.

    CALL METHOD lcl_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line'
        dragsrc    = 'X'
        droptarget = 'X'
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
        i_save          = 'X'
        i_default       = 'X'
        is_layout       = ls_layout
      CHANGING
        it_fieldcatalog = mt_alv_catalog
        it_outtab       = <f_tab>.

    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mt_alv_catalog ).
    LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      CLEAR <catalog>-key.
      DATA(ls_f4) = VALUE  lvc_s_f4( register   = 'X' chngeafter = 'X' fieldname  = <catalog>-fieldname ).
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

    lv_header = |{ m_tabname } - { lv_text } ({ m_count })|.
    mo_box->set_caption( lv_header ).
  ENDMETHOD.

  METHOD on_f4.

    FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.
    ASSIGN mr_table->* TO <tab>.
    READ TABLE <tab> INDEX es_row_no-row_id ASSIGNING <g_str>.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
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
          fcode = 'DETAIL'
          text  = 'Просмотр объекта'.

      IF l_dbtab+0(2) = 'PA'.
        CALL METHOD l_smenu->add_function
          EXPORTING
            fcode = 'PY'
            text  = 'PaYroll Clusters'.
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
    DATA: lv_clause      TYPE string,
          lr_struc       TYPE REF TO data,
          lr_table_descr TYPE REF TO cl_abap_structdescr,
          it_tabdescr    TYPE abap_compdescr_tab,
          lt_field_info  TYPE TABLE OF dfies,
          l_fname        TYPE fieldname,
          l_tname        TYPE tabname,
          l_replace      TYPE string,
          l_texttab      TYPE tabname,
          lo_str         TYPE REF TO cl_abap_structdescr.

    lcl_rtti=>create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_t_comp = mt_text_components e_handle = lo_str ).
    CREATE DATA lr_struc TYPE HANDLE lo_str.
    lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_struc ).
    it_tabdescr[] = lr_table_descr->components[].

    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = l_texttab ).

    l_replace = l_texttab && '_'.

    LOOP AT it_tabdescr INTO DATA(ls) WHERE name NE 'MANDT'.
      DATA(l_ind) = sy-tabix.
      APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
      <catalog>-col_pos = l_ind.
      READ TABLE lcl_alv_common=>mt_tabfields INTO DATA(ls_tf) WITH KEY tabname = i_tname fieldname = ls-name.
      IF sy-subrc NE 0.
        l_tname = i_tname.
        l_fname = ls-name.

        IF l_texttab IS NOT INITIAL.
          l_fname = ls-name.
          REPLACE l_replace IN l_fname WITH ''.
          IF sy-subrc = 0.
            l_tname = l_texttab.
          ENDIF.
        ENDIF.
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
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        CLEAR ls_tf.
        MOVE-CORRESPONDING lt_field_info[ 1 ] TO ls_tf.
        ls_tf-fieldname = ls-name.
        INSERT ls_tf INTO TABLE lcl_alv_common=>mt_tabfields.


      ENDIF.

      <catalog>-style = lcl_alv_common=>c_white.
      MOVE-CORRESPONDING ls_tf TO <catalog>.
      <catalog>-no_zero = 'X'.
      <catalog>-f4availabl = 'X'.
      IF ls_tf-tabname NE m_tabname.
        <catalog>-style = lcl_alv_common=>c_grey.
      ENDIF.

      IF ls_tf-checktable IS NOT INITIAL.
        <catalog>-style = lcl_alv_common=>c_blue.
      ENDIF.

      READ TABLE lcl_plugins=>mt_field_links WITH KEY tab = i_tname field = ls_tf-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <catalog>-style = lcl_alv_common=>c_green.
      ENDIF.

      READ TABLE lcl_plugins=>mt_el_links WITH KEY element = ls_tf-rollname TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <catalog>-style = lcl_alv_common=>c_green.
      ENDIF.

      IF ls_tf-keyflag = 'X'.
        <catalog>-style = <catalog>-style BIT-OR lcl_alv_common=>c_bold.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_menu_button.
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
    DATA: lt_keys TYPE TABLE OF dd05p.
    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD TABLE.
    CHECK es_row_no-row_id IS NOT INITIAL.
    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    link( EXPORTING i_str = <tab> i_column = e_column ).
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

    FIELD-SYMBOLS: <field>  LIKE LINE OF it_fields,
                   <f_line> TYPE any,
                   <f_tab>  TYPE STANDARD  TABLE.

    ASSIGN mr_table->* TO <f_tab>.
    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = it_fields[] ).
    IF e_ucomm = 'SEL_ON' AND m_visible IS INITIAL.
      create_sel_alv( ).
      m_visible = 'X'.
      IF mo_sel_width = 0.
        lv_sel_width = 500.
      ELSE.
        lv_sel_width = mo_sel_width.
      ENDIF.

      mo_splitter->set_column_width( EXPORTING id    = 1 width = lv_sel_width ).
      mo_alv->set_toolbar_interactive( ).
      RETURN.
    ELSEIF e_ucomm = 'PY'.
*      APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
*      CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = 'HRPY_RGDIR'.
*      <obj>-alv_viewer->mo_sel->set_value( i_field = 'PERNR' i_low = '33'  ).
*      <obj>-alv_viewer->mo_sel->raise_selection_done( ).
    ELSEIF e_ucomm = 'DETAIL'.
      IF m_tabname+0(2) = 'PA'.
        jump_pa20( ).
      ELSEIF m_tabname+0(3) = 'HRP'.
        jump_pp01( ).
      ENDIF.
    ELSEIF e_ucomm = 'REFRESH'.
      mo_sel->raise_selection_done( ).
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
      DATA: r_struc TYPE REF TO data,
            lo_str  TYPE REF TO cl_abap_structdescr.
      lcl_rtti=>create_struc_handle( EXPORTING i_tname = m_tabname  IMPORTING e_handle = lo_str ).
      CREATE DATA r_struc TYPE HANDLE lo_str.
      ASSIGN r_struc->* TO <f_line>.
      LOOP AT it_fields ASSIGNING <field> WHERE domname NE 'MANDT'.
        <field>-col_pos = sy-tabix.
        CASE e_ucomm.
          WHEN 'HIDE'. "hide select options
            IF <field>-tabname = m_tabname AND lines( <f_tab> ) > 0.
              CLEAR m_show_empty.
              lv_clause = |{ <field>-fieldname } IS NOT INITIAL|.
              LOOP AT <f_tab> ASSIGNING <f_line>  WHERE (lv_clause).
                EXIT.
              ENDLOOP.
              IF sy-subrc NE 0.
                <field>-no_out = 'X'.
              ENDIF.
            ENDIF.
          WHEN 'SHOW'.
            m_show_empty = 'X'.
            <field>-no_out = ' '.
          WHEN 'UPDATE'.
            lv_clause = |{ <field>-fieldname } IS NOT INITIAL|.
            LOOP AT <f_tab> ASSIGNING <f_line>  WHERE (lv_clause).
              EXIT.
            ENDLOOP.
            IF sy-subrc = 0.
              <field>-no_out = ''.
            ENDIF.
          WHEN 'TECH'. "technical field name
            <field>-scrtext_l = <field>-scrtext_m = <field>-scrtext_s =  <field>-reptext = <field>-fieldname.
          WHEN OTHERS. "header names translation
            READ TABLE lcl_appl=>mt_lang WITH KEY spras = e_ucomm TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              lcl_alv_common=>translate_field( EXPORTING i_lang = CONV #( e_ucomm ) CHANGING c_fld = <field> ).
              IF mo_sel IS BOUND.
                READ TABLE mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) WITH KEY field_label = <field>-fieldname.
                IF sy-subrc = 0.
                  <sel>-name = <field>-scrtext_l.
                  IF <sel>-name IS INITIAL.
                    <sel>-name = <field>-reptext.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    READ TABLE lcl_appl=>mt_lang WITH KEY spras = e_ucomm TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      m_lang = e_ucomm.
      set_header( ).
      update_texts( ).
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
    DATA: ls_row TYPE t_sel_row.
    lcl_sql=>read_any_table( EXPORTING i_tabname = m_tabname i_where = get_where( ) CHANGING cr_tab =  mr_table c_count = m_count ).
    update_texts( ).
    set_header( ).

    LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      ENDIF.
    ENDLOOP.
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
    FIELD-SYMBOLS: <text_tab> TYPE  table,
                   <check>    TYPE any.

    CHECK m_texttabname IS NOT INITIAL.

    "text fields
    ASSIGN mr_text_table->* TO <text_tab>.
    READ TABLE <text_tab> INDEX 1 ASSIGNING FIELD-SYMBOL(<text_dummy>).
    CHECK sy-subrc = 0.

    l_replace = m_texttabname && '_'.
    ASSIGN mr_table->* TO <f_tab>.

    LOOP AT <f_tab> ASSIGNING FIELD-SYMBOL(<str>).
      CLEAR lv_clause.
      LOOP AT lcl_alv_common=>mt_tabfields INTO DATA(l_texttabfield) WHERE tabname = m_texttabname AND keyflag = 'X'.
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

      ASSIGN COMPONENT 'SPRSL' OF STRUCTURE <text_dummy> TO FIELD-SYMBOL(<dummy>).
      IF sy-subrc = 0.
        lv_clause = |{ lv_clause } AND SPRSL = '{ m_lang }'|.
      ENDIF.
      ASSIGN COMPONENT 'SPRAS' OF STRUCTURE <text_dummy> TO <dummy>.
      IF sy-subrc = 0.
        lv_clause = |{ lv_clause } AND SPRAS = '{ m_lang }'|.
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

  METHOD jump_pa20.
    DATA: l_infty    TYPE infty,
          l_temp(10) TYPE c.

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = lcl_alv_common=>get_selected( mo_alv ).

    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<tab>).

    SELECT SINGLE infty INTO l_infty
      FROM t777d
     WHERE dbtab = m_tabname.
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

  METHOD jump_pp01.
    DATA: save_plvar(2),
          save_otype(2),
          save_objid(8),
          l_infty(4),
          l_subty(4),
          l_temp(10).

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.

    DATA(l_row) = lcl_alv_common=>get_selected( mo_alv ).

    ASSIGN mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<tab>).

    SELECT SINGLE infty INTO l_infty FROM t777d WHERE dbtab = m_tabname.

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

    DATA: it_bdcdata TYPE TABLE OF  bdcdata.

    it_bdcdata = VALUE #(
      ( program = 'SAPMH5A0' dynpro = '1000' dynbegin = 'X' )
      ( fnam = 'PPHDR-INFTY' fval = l_infty )
      ( fnam = 'PPHDR-subty' fval = l_subty )
      ( fnam = 'PPHDR-istat' fval = l_temp )
      ( fnam = 'BDC_OKCODE' fval = 'DISP' )
      ).
    CALL TRANSACTION 'PP02' USING it_bdcdata MODE 'E'.
  ENDMETHOD.

  METHOD link.
    DATA: i_viewer TYPE REF TO lcl_table_viewer.
    CLEAR r_done.
    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    "field to field links
    LOOP AT lcl_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = m_tabname AND field = i_column.
      ASSIGN COMPONENT ls_link-field OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
      IF i_viewer IS INITIAL.
        IF ls_link-rtab IS INITIAL.
          i_viewer = me.
        ELSE.
          APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
          CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = ls_link-rtab.
          i_viewer = <obj>-alv_viewer.
        ENDIF.
      ENDIF.
      i_viewer->mo_sel->set_value( i_field = ls_link-rfield i_low = COND aqadh_type_of_icon( WHEN ls_link-const IS INITIAL THEN <field> ELSE ls_link-const ) ).
    ENDLOOP.
    IF sy-subrc = 0.
      i_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang  ).
      i_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      i_viewer->mo_sel->raise_selection_done( ).
      r_done = 'X'.
    ENDIF.
    CHECK r_done IS INITIAL.

    READ TABLE lcl_alv_common=>mt_tabfields WITH KEY tabname = m_tabname fieldname = i_column INTO DATA(l_field).
    "data element to field links
    LOOP AT lcl_plugins=>mt_el_links INTO DATA(l_el_link) WHERE element = l_field-rollname." 'PA20' .
      CASE l_el_link-tcode.
        WHEN 'PA20'.
          jump_pa20( ).
        WHEN 'PP01'.
          jump_pp01( ).
      ENDCASE.
      r_done = 'X'.

    ENDLOOP.
    CHECK r_done IS INITIAL.

    LOOP AT lcl_plugins=>mt_el_links INTO l_el_link WHERE element = l_field-rollname .
      IF i_viewer IS INITIAL.
        APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING <obj>.
        CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_el_link-rtab.
        i_viewer = <obj>-alv_viewer.
      ENDIF.
      ASSIGN COMPONENT i_column OF STRUCTURE i_str TO <field>.
      i_viewer->mo_sel->set_value( i_field = l_el_link-rfield i_low = <field>  ).
    ENDLOOP.
    IF sy-subrc = 0.
      i_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang  ).
      i_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      i_viewer->mo_sel->raise_selection_done( ).
      r_done = 'X'.
    ENDIF.
    CHECK r_done IS INITIAL.

    "special plugin for HRP1001-ADATANR
    IF i_column = 'ADATANR' AND m_tabname = 'HRP1001'.
      ASSIGN COMPONENT 'ADATANR' OF STRUCTURE i_str TO FIELD-SYMBOL(<datanr>).
      ASSIGN COMPONENT 'RELAT' OF STRUCTURE i_str TO FIELD-SYMBOL(<relat>).
      SELECT SINGLE pasub INTO @DATA(lv_struc)
        FROM t77ar
       WHERE relat = @<relat>.

      SELECT SINGLE dbtab INTO @DATA(lv_dbtab)
        FROM t77ad
       WHERE pasub = @lv_struc.

      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING <obj>.
        CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = lv_dbtab.
        <obj>-alv_viewer->mo_sel->set_value( i_field = 'ADATANR' i_low = <datanr>  ).
        <obj>-alv_viewer->mo_sel->raise_selection_done( ).
        r_done = 'X'.
      ENDIF.
    ENDIF.
    CHECK r_done IS INITIAL.

    "dictionary key links plugin
    DATA: lt_keys TYPE TABLE OF dd05p.
    READ TABLE lcl_alv_common=>mt_tabfields INTO DATA(field) WITH KEY tabname = m_tabname fieldname = i_column .
    ASSIGN COMPONENT i_column OF STRUCTURE i_str TO <field>.
    CHECK <field> IS NOT INITIAL.
    CALL FUNCTION 'DD_FORKEY_GET'
      EXPORTING
        feldname  = CONV fieldname( i_column )
        tabname   = m_tabname
      TABLES
        forkeytab = lt_keys
      EXCEPTIONS
        not_equal = 1
        not_found = 2
        not_valid = 3
        OTHERS    = 4.

    IF sy-subrc < 2.
      APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING <obj>.
      CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = field-checktable.
      LOOP AT lt_keys INTO DATA(l_keys).
        ASSIGN COMPONENT l_keys-forkey OF STRUCTURE i_str TO <field>.
        CHECK sy-subrc = 0.
        <obj>-alv_viewer->mo_sel->set_value( i_field = l_keys-checkfield i_low = <field>  ).
      ENDLOOP.
      <obj>-alv_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang  ).
      <obj>-alv_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      <obj>-alv_viewer->mo_sel->raise_selection_done( ).
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
        dragsrc    = 'X'
        droptarget = 'X'
        effect     = effect.

    CALL METHOD lcl_appl=>c_dragdropalv->get_handle IMPORTING handle = handle_alv.
    ms_layout-s_dragdrop-col_ddid = handle_alv.
    init_fcat( handle_alv ).
    ms_layout-cwidth_opt = 'X'.
    ms_layout-col_opt = 'X'.
    ms_layout-ctab_fname = 'COLOR'.
    ms_layout-stylefname = 'STYLE'.

    "fields for F4 event handling
    DATA(gt_f4) = VALUE  lvc_t_f4( register   = 'X' chngeafter = 'X'
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
        i_save          = 'X'
        i_default       = 'X'
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
     ( fieldname = 'SIGN'        coltext = 'SIGN'   tech = 'X' )
     ( fieldname = 'OPTI'        coltext = 'Option' tech = 'X' )
     ( fieldname = 'OPTION_ICON' coltext = 'Option' icon = 'X' outputlen = 4 style = cl_gui_alv_grid=>mc_style_button )
     ( fieldname = 'LOW'         coltext = 'From data' edit = 'X' lowercase = 'X' outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4 col_opt = 'X'  )
     ( fieldname = 'HIGH'        coltext = 'To data' edit = 'X' lowercase = 'X' outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4  col_opt = 'X' )
     ( fieldname = 'MORE_ICON'   coltext = 'Range' icon = 'X'  style = cl_gui_alv_grid=>mc_style_button  )
     ( fieldname = 'RANGE'   tech = 'X'  )
     ( fieldname = 'INHERITED'   coltext = 'Inh.' icon = 'X' outputlen = 4 seltext = 'Inherited' style = '00000003')
     ( fieldname = 'EMITTER'    coltext = 'Emit.' icon = 'X' outputlen = 4 seltext = 'Emitter' style = '00000003')
     ( fieldname = 'NAME' coltext = 'Field name'  outputlen = 60 style = '00000003')
     ( fieldname = 'ELEMENT' coltext = 'Data element'  outputlen = 15 style = '00000209' )
     ( fieldname = 'DOMAIN'  coltext = 'Domain'  outputlen = 15 style = '00000209' )
     ( fieldname = 'DATATYPE' coltext = 'Type'  outputlen = 5 style = '00000003')
     ( fieldname = 'LENGTH' coltext = 'Length'  outputlen = 5 style = '00000003')
     ( fieldname = 'TRANSMITTER'   tech = 'X'  )
     ( fieldname = 'RECEIVER'    tech = 'X'  )
     ( fieldname = 'COLOR'    tech = 'X'  )
      ).
  ENDMETHOD.

  METHOD raise_selection_done.
    lcl_alv_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    DATA: ls_row TYPE t_sel_row.
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

        IF l_tfield-keyflag = 'X'.
          <sel_tab>-style = VALUE #( ( fieldname = 'FIELD_LABEL' style = '00000020' ) ).
        ENDIF.
        IF l_tfield-empty = 'X'.
          <sel_tab>-color = VALUE #( ( fname = 'FIELD_LABEL' color-col = 2 color-int = 0 color-inv = 1 ) ).
        ELSE.
          <sel_tab>-color = VALUE #( ( fname = 'FIELD_LABEL' color-col = 4 color-int = 0 color-inv = 1 ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD handle_sel_toolbar.
    e_object->mt_toolbar[] = VALUE #( butn_type = 0 disabled = ''
     ( function = 'SEL_OFF' icon = icon_arrow_right    quickinfo = 'Hide' )
     ( function = 'SEL_CLEAR' icon = icon_delete_row    quickinfo = 'Clear Select-Options' )
      ).
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
      DATA: ls_row TYPE t_sel_row.
      MOVE-CORRESPONDING <to> TO ls_row.
      <to>-transmitter->emit( EXPORTING e_row = ls_row ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_doubleclick.
    DATA: it_bdcdata TYPE TABLE OF  bdcdata.
    CHECK es_row_no-row_id IS NOT INITIAL.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id INTO DATA(l_sel).
    APPEND VALUE #( program = 'SAPLSD_ENTRY' dynpro = '1000' dynbegin = 'X' ) TO it_bdcdata.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WB_DISPLAY' ) TO it_bdcdata.

    IF e_column = 'ELEMENT'.
      SET PARAMETER ID 'DTYP' FIELD l_sel-element.
      APPEND VALUE #( fnam = 'RSRD1-DDTYPE' fval = 'X' ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSEIF e_column = 'DOMAIN'.
      SET PARAMETER ID 'DOM' FIELD l_sel-domain.
      APPEND VALUE #( fnam = 'RSRD1-DOMA' fval = 'X' ) TO it_bdcdata.
      CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
    ELSE.
      CALL FUNCTION 'DOCU_CALL'
        EXPORTING
          id                = 'DE'
          langu             = mo_viewer->m_lang
          object            = l_sel-element
          typ               = 'E'
          displ             = 'X'
          displ_mode        = 3
          use_sec_langu     = 'X'
          display_shorttext = 'X'.
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
      IF c_sel_row-int_type = 'D'.
        DO 2 TIMES.
          ASSIGN COMPONENT  COND string( WHEN sy-index = 1 THEN 'LOW' ELSE 'HIGH'  ) OF STRUCTURE <range> TO FIELD-SYMBOL(<field>).
          IF <field> IS INITIAL.
            CONTINUE.
          ENDIF.

          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external            = <field>
            IMPORTING
              date_internal            = <field>
            EXCEPTIONS
              date_external_is_invalid = 1
              OTHERS                   = 2.
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

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    IF e_fieldname = 'LOW'.
      l_multiple = 'X'.
    ENDIF.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
    DATA(l_fname) =  <sel>-field_label.

    gt_sel[] = mt_sel_tab[].
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
        ASSIGN er_event_data->m_data->* TO <itab>.
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
    er_event_data->m_event_handled = 'X'.
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
              plausibility_check        = 'X'
            IMPORTING
              output                    = lv_date
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.
          <ls_cells>-value = |{ lv_date DATE = USER }|.
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
    DATA: lt_fcodes TYPE ui_funcattr,
          ls_fcode  TYPE uiattentry,
          ls_func   TYPE ui_func,
          lt_func   TYPE ui_functions.

    DATA(l_index) = lcl_alv_common=>get_selected( mo_sel_alv ).

    IF l_index IS NOT INITIAL.
      READ TABLE mt_sel_tab INTO DATA(l_sel) INDEX l_index.
    ENDIF.

    CALL METHOD e_object->get_functions IMPORTING fcodes = lt_fcodes. "Inactivate all standard functions

    LOOP AT lt_fcodes INTO ls_fcode WHERE fcode NE '&OPTIMIZE'.
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
      CALL METHOD mo_viewer->mo_splitter->get_column_width
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
      mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = data(lt_sel_rows) ).

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
    APPEND: 'ONLI' TO itab.
    APPEND: 'WB_EXEC' TO itab.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = itab.
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
        CALL SCREEN 101.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_dragdrop IMPLEMENTATION.
  METHOD drag.
    DATA: dataobj TYPE REF TO lcl_dd_data.
    CREATE OBJECT dataobj.
    dataobj->m_row = e_row-index.
    dataobj->m_column = e_column.
    e_dragdropobj->object = dataobj.
  ENDMETHOD.

  METHOD drop."It should be refactored someday...
    DATA: lo_from_sel     TYPE REF TO lcl_sel_opt,
          lo_from_tab     TYPE REF TO lcl_table_viewer,
          lo_to           TYPE REF TO lcl_sel_opt,
          lo_alv          TYPE REF TO cl_gui_alv_grid,
          lt_sel_col      TYPE  lvc_t_col,
          ls_row          TYPE t_sel_row,
          lv_set_receiver.

    LOOP AT lcl_appl=>mt_obj INTO DATA(lo).
      "to
      IF lo-alv_viewer->mo_sel IS BOUND.
        IF e_dragdropobj->droptargetctrl = lo-alv_viewer->mo_sel->mo_sel_alv.
          lo_to = lo-alv_viewer->mo_sel.
        ENDIF.
      ENDIF.

      "from tab
      IF lo-alv_viewer->mo_alv = e_dragdropobj->dragsourcectrl.
        lo_from_tab = lo-alv_viewer.
        CONTINUE.
      ENDIF.

      CHECK lo-alv_viewer->mo_sel IS BOUND.
      IF e_dragdropobj->dragsourcectrl = lo-alv_viewer->mo_sel->mo_sel_alv.
        lo_from_sel = lo-alv_viewer->mo_sel.
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_cells( IMPORTING et_cell = DATA(lt_sel_cells) ).
      ENDIF.
    ENDLOOP.

    IF lo_from_tab IS BOUND." tab to select
      FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE,
                     <f_str>   TYPE any,
                     <f_field> TYPE any.
      lo_from_tab->mo_alv->get_selected_cells( IMPORTING et_cell = lt_sel_cells  ).
      lo_from_tab->mo_alv->get_selected_columns( IMPORTING et_index_columns = lt_sel_col  ).

      LOOP AT lt_sel_col INTO DATA(l_col).
        TRY.
            lo_from_tab->mt_alv_catalog[ fieldname = l_col-fieldname ]-style = cl_gui_alv_grid=>mc_style_button.
          CATCH cx_sy_itab_line_not_found.              "#EC NO_HANDLER
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
            READ TABLE <f_tab> INDEX l_cell-row_id ASSIGNING <f_str>.
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
              READ TABLE <to_tab>-range WITH KEY low = <f_field> TRANSPORTING NO FIELDS.
              IF sy-subrc NE 0.
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

    lo_alv ?= e_dragdropobj->dragsourcectrl.
    lcl_alv_common=>refresh( EXPORTING i_obj = lo_alv i_soft = 'X' ).

    lo_alv ?= e_dragdropobj->droptargetctrl.
    lo_to->raise_selection_done( ).
  ENDMETHOD.
ENDCLASS.
"END OF INCLUDE YS_SDE_CLASSES.

*------------REPORT EVENTS--------------------
INITIALIZATION.
  lcl_appl=>init_lang( ).
  lcl_appl=>init_icons_table( ).
  lcl_plugins=>init( ).

AT SELECTION-SCREEN OUTPUT.
  %_gv_tname_%_app_%-text = 'Enter table name and hit Enter'.
  lcl_appl=>suppress_run_button( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  lcl_appl=>exit( ).

AT SELECTION-SCREEN .
  CHECK lcl_sql=>exist_table( gv_tname ) = 1.
  APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
  CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = gv_tname.

FORM callback_f4_sel TABLES record_tab STRUCTURE seahlpres
          CHANGING shlp TYPE shlp_descr
                   callcontrol LIKE ddshf4ctrl.

  LOOP AT shlp-interface ASSIGNING FIELD-SYMBOL(<interface>) WHERE f4field NE 'X'.
    READ TABLE gt_sel WITH KEY field_label = <interface>-shlpfield INTO DATA(l_sel).
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
  LOOP AT shlp-interface ASSIGNING FIELD-SYMBOL(<interface>) WHERE f4field NE 'X'.
    ASSIGN COMPONENT <interface>-shlpfield OF STRUCTURE <g_str> TO <field>.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO shlp-selopt ASSIGNING FIELD-SYMBOL(<searchsel>).
      <searchsel>-sign = 'I'.
      <searchsel>-low = <field>.
      <searchsel>-shlpfield = <interface>-shlpfield.
      <searchsel>-option = 'EQ'.
    ENDIF.
  ENDLOOP.
ENDFORM.
