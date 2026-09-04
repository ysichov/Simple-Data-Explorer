CLASS zcl_sde_adt_res_table DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_field,
             name     TYPE string,
             position TYPE i,
             key      TYPE abap_bool,
             datatype TYPE string,
             length   TYPE i,
             decimals TYPE i,
             text     TYPE string,
           END OF ty_field,
           tt_field TYPE STANDARD TABLE OF ty_field WITH EMPTY KEY.

    METHODS field_catalog
      IMPORTING io_struct       TYPE REF TO cl_abap_structdescr
                i_tabname       TYPE tabname
      RETURNING VALUE(rt_field) TYPE tt_field
      RAISING   cx_adt_rest.

    METHODS not_found
      IMPORTING i_type TYPE string
                i_id   TYPE string
      RAISING   cx_adt_res_not_found.
ENDCLASS.

CLASS zcl_sde_adt_res_table IMPLEMENTATION.

  METHOD get.
    DATA: lv_name  TYPE tabname,
          lv_rows  TYPE i,
          lr_tab   TYPE REF TO data,
          lv_count TYPE i,
          lo_type  TYPE REF TO cl_abap_typedescr.
    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).
    TRANSLATE lv_name TO UPPER CASE.

    request->get_uri_query_parameter( EXPORTING name      = 'rows'
                                                mandatory = abap_false
                                                default   = 100
                                      IMPORTING value     = lv_rows ).

    " zcl_sde_sql=>read_any_table swallows a missing table and returns nothing,
    " so the existence check has to happen here to produce a real 404.
    IF zcl_sde_sql=>exist_table( lv_name ) <> 1.
      not_found( i_type = `table` i_id = CONV string( lv_name ) ).
    ENDIF.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING  p_name         = lv_name
      RECEIVING  p_descr_ref    = lo_type
      EXCEPTIONS type_not_found = 1
                 OTHERS         = 2 ).
    IF sy-subrc <> 0 OR lo_type->kind <> cl_abap_typedescr=>kind_struct.
      not_found( i_type = `structure` i_id = CONV string( lv_name ) ).
    ENDIF.

    DATA(lo_struct) = CAST cl_abap_structdescr( lo_type ).
    DATA(lo_tabtype) = cl_abap_tabledescr=>create( p_line_type = lo_struct ).
    CREATE DATA lr_tab TYPE HANDLE lo_tabtype.
    ASSIGN lr_tab->* TO <lt_data>.

    zcl_sde_sql=>read_any_table( EXPORTING i_tabname   = lv_name
                                           i_where     = ``
                                           i_row_count = lv_rows
                                 CHANGING  cr_tab      = lr_tab
                                           c_count     = lv_count ).

    DATA(lv_body) =
      |\{"table":"{ to_lower( lv_name ) }",|                                                        &&
      |"count":{ lv_count },|                                                                       &&
      |"fields":{ /ui2/cl_json=>serialize(
                    data        = field_catalog( io_struct = lo_struct i_tabname = lv_name )
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },|                          &&
      |"rows":{ /ui2/cl_json=>serialize(
                    data        = <lt_data>
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.

    response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      data            = lv_body ).
  ENDMETHOD.

  METHOD field_catalog.
    DATA lt_ddic TYPE ddfields.

    io_struct->get_ddic_field_list(
      RECEIVING  p_field_list = lt_ddic
      EXCEPTIONS not_found    = 1
                 no_ddic_type = 2
                 OTHERS       = 3 ).
    IF sy-subrc <> 0.
      not_found( i_type = `fieldlist` i_id = CONV string( i_tabname ) ).
    ENDIF.

    LOOP AT lt_ddic ASSIGNING FIELD-SYMBOL(<ls_ddic>).
      APPEND VALUE #( name     = to_lower( <ls_ddic>-fieldname )
                      position = <ls_ddic>-position
                      key      = <ls_ddic>-keyflag
                      datatype = <ls_ddic>-datatype
                      length   = <ls_ddic>-leng
                      decimals = <ls_ddic>-decimals
                      text     = <ls_ddic>-fieldtext ) TO rt_field.
    ENDLOOP.
  ENDMETHOD.

  METHOD not_found.
    RAISE EXCEPTION TYPE cx_adt_res_not_found
      EXPORTING resource_type = i_type
                resource_id   = i_id.
  ENDMETHOD.

ENDCLASS.
