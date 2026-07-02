CLASS zcl_sde_text_viewer DEFINITION PUBLIC FINAL INHERITING FROM zcl_sde_popup CREATE PUBLIC.
  PUBLIC SECTION.
    DATA: mo_text     TYPE REF TO cl_gui_textedit.

    METHODS: constructor IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      load_text  IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer.

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
