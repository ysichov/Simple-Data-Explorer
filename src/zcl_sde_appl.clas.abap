CLASS zcl_sde_appl DEFINITION PUBLIC CREATE PUBLIC.
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
