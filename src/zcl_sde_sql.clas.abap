CLASS zcl_sde_sql DEFINITION PUBLIC CREATE PUBLIC.
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

CLASS zcl_sde_sql IMPLEMENTATION.
  METHOD read_any_table.
    FIELD-SYMBOLS: <f_tab> TYPE ANY TABLE.

    ASSIGN cr_tab->* TO <f_tab>.
    c_count = lines( <f_tab> ).
    CHECK zcl_sde_sql=>exist_table( i_tabname ) = 1.
    IF i_where IS NOT INITIAL.
      TRY.
          SELECT * FROM (i_tabname) INTO CORRESPONDING FIELDS OF TABLE <f_tab>  UP TO i_row_count ROWS  WHERE (i_where)  ORDER BY PRIMARY KEY.

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
  ENDMETHOD.

  METHOD exist_table.
    SELECT COUNT( * ) FROM dd02l
     WHERE tabname = i_tab
       AND ( tabclass = 'TRANSP' OR tabclass = 'CLUSTER' ).
    e_subrc = sy-dbcnt.
  ENDMETHOD.

  METHOD exist_view.
    SELECT COUNT( * ) FROM dd02l
     WHERE tabname = i_tab
       AND tabclass = 'VIEW'.
    e_subrc = sy-dbcnt.
  ENDMETHOD.

  METHOD exist_cds.
    SELECT COUNT( * ) FROM dd02l
     WHERE tabname = i_tab
       AND tabclass = 'VIEW'
       AND applclass = 'SDGV'.
    e_subrc = sy-dbcnt.
  ENDMETHOD.
ENDCLASS.
