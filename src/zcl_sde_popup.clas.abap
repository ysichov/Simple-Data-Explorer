CLASS zcl_sde_popup DEFINITION PUBLIC CREATE PUBLIC.
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
