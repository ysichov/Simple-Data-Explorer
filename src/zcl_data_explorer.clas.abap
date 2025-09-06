class ZCL_DATA_EXPLORER definition
  public
  final
  create public .

public section.

  class-methods OPEN_TABLE
    importing
      !IV_NAME type STRING
      !IT_TAB type ANY optional
      !IV_VAR type ANY optional
      !IV_SHOW type XFELD optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DATA_EXPLORER IMPLEMENTATION.


  method OPEN_TABLE.

      CALL FUNCTION 'Z_DUMMY_SCREEN' EXPORTING it_tab = it_tab iv_name = iv_name show = iv_show.

  endmethod.
ENDCLASS.
