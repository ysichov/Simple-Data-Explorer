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

REPORT z_sde.

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

  FIELD-SYMBOLS: <field> TYPE any.
  CHECK zcl_sde_appl=>gr_current_row IS BOUND.
  LOOP AT shlp-interface ASSIGNING FIELD-SYMBOL(<interface>) WHERE f4field NE abap_true.
    ASSIGN COMPONENT <interface>-shlpfield OF STRUCTURE zcl_sde_appl=>gr_current_row->* TO <field>.
    IF sy-subrc = 0.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <field> shlpfield = <interface>-shlpfield  ) TO shlp-selopt.
    ENDIF.
  ENDLOOP.
ENDFORM.
