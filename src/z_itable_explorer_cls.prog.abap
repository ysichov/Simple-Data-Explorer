*& Written by Yurii Sychov
*& e-mail:   ysichov@gmail.com
*& skype:    ysichov
*& blog:     https://ysychov.wordpress.com/blog/
*& LinkedIn: https://www.linkedin.com/in/ysychov/
*&---------------------------------------------------------------------*

*& External resources
*& https://github.com/ysichov/SDE_abapgit - Simple Data Explorer

CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.
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

           BEGIN OF t_obj,
             name       TYPE string,
             alv_viewer TYPE REF TO lcl_table_viewer,
           END OF t_obj,

           BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang  .

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
                               iv_show  type xfeld OPTIONAL.
ENDCLASS.

CLASS lcl_popup DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: m_counter  TYPE i.

    DATA: m_additional_name      TYPE string,
          mo_box                 TYPE REF TO cl_gui_dialogbox_container,
          mo_splitter            TYPE REF TO cl_gui_splitter_container,
          mo_variables_container TYPE REF TO cl_gui_container.

    METHODS: "constructor,
      create IMPORTING i_width       TYPE i
                       i_hight       TYPE i
                       i_name        TYPE text100 OPTIONAL
             RETURNING VALUE(ro_box) TYPE REF TO cl_gui_dialogbox_container,

      on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender,
      constructor IMPORTING i_additional_name TYPE string OPTIONAL.

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
    CONSTANTS: c_white(4) TYPE x VALUE '00000001', "white background
               c_grey(4)  TYPE x VALUE '00000003', "gray background
               c_green(4) TYPE x VALUE '00000216', "green +underline
               c_blue(4)  TYPE x VALUE '00000209', " blue font +underline
               c_bold(4)  TYPE x VALUE '00000020'.

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

CLASS lcl_sel_opt DEFINITION DEFERRED.

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
          m_line_count       TYPE i,
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
      create_field_cat IMPORTING i_tname           TYPE tabname
                       RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
      translate_field IMPORTING i_lang TYPE ddlanguage CHANGING c_fld TYPE lvc_s_fcat,
      handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
      before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no.
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
      "lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table  ).
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

    FIELD-SYMBOLS: <f_tab>   TYPE table.

    mo_alv = NEW #( i_parent = mo_alv_parent ).
    mt_alv_catalog = create_field_cat( m_tabname ).

    IF mt_alv_catalog IS INITIAL.
      RETURN. "todo show tables without structure
    ENDIF.

    ASSIGN mr_table->* TO <f_tab>.
    m_line_count = lines( <f_tab> ).
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

  METHOD set_header.
    DATA: lv_text       TYPE as4text,
          lv_header(80) TYPE c.

    SELECT SINGLE ddtext INTO lv_text
      FROM dd02t
     WHERE tabname = m_tabname
       AND ddlanguage = m_lang.

   IF m_tabname is not INITIAL.
    lv_header = |{ m_tabname } - { lv_text } { m_additional_name }|.
   ELSE.
     mo_alv->get_filtered_entries( IMPORTING et_filtered_entries = data(lt_filtered) ).
     data(lv_filtered) = lines( lt_filtered ).
    lv_header = |{ m_additional_name } ({ m_line_count - lv_filtered })|.
   ENDIF.
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

      <catalog>-col_pos = l_ind.
      <catalog>-style = lcl_alv_common=>c_white.
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
    ELSE.
      TRY.
          lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ 1 ]-{ e_column-fieldname }| ).
          table_clone = lo_table_descr->elem_clone( ).
          lcl_appl=>open_int_table( EXPORTING iv_name = |{ m_additional_name }[ 1 ]-{ e_column-fieldname }| it_ref = table_clone ).
        CATCH cx_sy_move_cast_error.
      ENDTRY.
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
    set_header( ).

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
