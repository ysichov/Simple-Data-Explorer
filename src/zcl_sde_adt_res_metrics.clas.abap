CLASS zcl_sde_adt_res_metrics DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PRIVATE SECTION.
    " One code unit - a method, FORM, module or function. The token detail
    " ZCL_ACE_METRICS also returns is left out: it is debugging material and
    " larger than everything else together.
    TYPES: BEGIN OF ty_unit,
             include     TYPE string,
             unit_type   TYPE string,
             unit_name   TYPE string,
             cyclomatic  TYPE i,
             loc         TYPE i,
             lloc        TYPE i,
             cloc        TYPE i,
             mi          TYPE p LENGTH 8 DECIMALS 2,
             volume      TYPE p LENGTH 12 DECIMALS 2,
             difficulty  TYPE p LENGTH 8 DECIMALS 2,
             effort      TYPE p LENGTH 15 DECIMALS 2,
             bugs        TYPE p LENGTH 8 DECIMALS 3,
             n1          TYPE i,
             n2          TYPE i,
             big_n1      TYPE i,
             big_n2      TYPE i,
             vocabulary  TYPE i,
             prog_length TYPE i,
           END OF ty_unit,
           tt_unit TYPE STANDARD TABLE OF ty_unit WITH EMPTY KEY.

    TYPES: BEGIN OF ty_totals,
             units          TYPE i,
             cyclomatic     TYPE i,
             avg_cyclomatic TYPE p LENGTH 8 DECIMALS 2,
             loc            TYPE i,
             lloc           TYPE i,
             cloc           TYPE i,
             volume         TYPE p LENGTH 12 DECIMALS 2,
             effort         TYPE p LENGTH 15 DECIMALS 2,
             bugs           TYPE p LENGTH 8 DECIMALS 3,
           END OF ty_totals.

ENDCLASS.


CLASS zcl_sde_adt_res_metrics IMPLEMENTATION.

  METHOD get.
    DATA: lv_name    TYPE string,
          lv_type    TYPE string,
          lv_head    TYPE string,
          lv_program TYPE program,
          lt_unit    TYPE tt_unit.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).

    request->get_uri_query_parameter( EXPORTING name      = 'type'
                                                mandatory = abap_false
                                                default   = 'PROG'
                                      IMPORTING value     = lv_type ).

    zcl_sde_ace_source=>resolve( EXPORTING i_name     = lv_name
                                           i_type     = lv_type
                                 IMPORTING ev_type    = lv_head
                                           ev_program = lv_program ).

    DATA(ls_source) = zcl_sde_ace_source=>parse( lv_program ).

    DATA(ls_result) = zcl_ace_metrics=>calculate( is_parse_data = ls_source
                                                  i_program     = lv_program ).

    LOOP AT ls_result-units ASSIGNING FIELD-SYMBOL(<ls_u>).
      APPEND VALUE #( include     = to_lower( <ls_u>-include )
                      unit_type   = to_lower( <ls_u>-unit_type )
                      unit_name   = to_lower( <ls_u>-unit_name )
                      cyclomatic  = <ls_u>-cyclomatic
                      loc         = <ls_u>-loc
                      lloc        = <ls_u>-lloc
                      cloc        = <ls_u>-cloc
                      mi          = <ls_u>-mi
                      volume      = <ls_u>-volume
                      difficulty  = <ls_u>-difficulty
                      effort      = <ls_u>-effort
                      bugs        = <ls_u>-bugs
                      n1          = <ls_u>-n1
                      n2          = <ls_u>-n2
                      big_n1      = <ls_u>-big_n1
                      big_n2      = <ls_u>-big_n2
                      vocabulary  = <ls_u>-vocabulary
                      prog_length = <ls_u>-prog_length ) TO lt_unit.
    ENDLOOP.

    DATA(ls_totals) = VALUE ty_totals(
        units          = lines( ls_result-units )
        cyclomatic     = ls_result-total_cyclomatic
        avg_cyclomatic = ls_result-avg_cyclomatic
        loc            = ls_result-total_loc
        lloc           = ls_result-total_lloc
        cloc           = ls_result-total_cloc
        volume         = ls_result-total_volume
        effort         = ls_result-total_effort
        bugs           = ls_result-total_bugs ).

    DATA(lv_body) =
      |\{"object":"{ to_lower( lv_name ) }",| &&
      |"type":"{ to_lower( lv_head ) }",| &&
      |"program":"{ to_lower( lv_program ) }",| &&
      |"totals":{ /ui2/cl_json=>serialize(
                    data        = ls_totals
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },| &&
      |"units":{ /ui2/cl_json=>serialize(
                    data        = lt_unit
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.

    response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      data            = lv_body ).
  ENDMETHOD.


ENDCLASS.
