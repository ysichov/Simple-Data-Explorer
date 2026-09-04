CLASS zcl_sde_adt_res_table DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.
ENDCLASS.

CLASS zcl_sde_adt_res_table IMPLEMENTATION.
  METHOD get.
    DATA: lv_name TYPE tabname,
          lv_rows TYPE i,
          lv_body TYPE string.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).

    request->get_uri_query_parameter( EXPORTING name      = 'rows'
                                                mandatory = abap_false
                                                default   = 100
                                      IMPORTING value     = lv_rows ).

    lv_body = |SDE ADT resource alive| && cl_abap_char_utilities=>cr_lf &&
              |table = { lv_name }|    && cl_abap_char_utilities=>cr_lf &&
              |rows  = { lv_rows }|    && cl_abap_char_utilities=>cr_lf &&
              |user  = { sy-uname }|   && cl_abap_char_utilities=>cr_lf &&
              |sysid = { sy-sysid }|.

    response->set_body_data( content_handler = NEW cl_adt_rest_plain_text_handler( )
                             data            = lv_body ).
  ENDMETHOD.
ENDCLASS.
