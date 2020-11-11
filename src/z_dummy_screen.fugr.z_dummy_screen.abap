FUNCTION z_dummy_screen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_TAB) TYPE  ANY
*"     REFERENCE(IV_NAME) TYPE  ANY
*"     REFERENCE(SHOW) TYPE  XFELD OPTIONAL
*"----------------------------------------------------------------------

  DATA: r_tab TYPE REF TO data.
  GET REFERENCE OF it_tab INTO r_tab.

  APPEND INITIAL LINE TO  gt_tabs ASSIGNING FIELD-SYMBOL(<tab>).
  <tab>-tab = r_tab.
  <tab>-name = iv_name.

  IF show = abap_true.
    CALL SCREEN 333.
  ENDIF.

ENDFUNCTION.
