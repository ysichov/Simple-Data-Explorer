CLASS zcl_sde_adt_res_join DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PRIVATE SECTION.
    " A join of more tables than this is not a join anybody is reading.
    CONSTANTS c_max_tables TYPE i VALUE 10.

    " A table the dictionary offers around the ones already in the join.
    TYPES: BEGIN OF ty_candidate,
             tabname   TYPE string,
             ddtext    TYPE string,
             direction TYPE string,
             alias     TYPE string,
             selected  TYPE abap_bool,
           END OF ty_candidate,
           tt_candidate TYPE STANDARD TABLE OF ty_candidate WITH EMPTY KEY.

    TYPES: BEGIN OF ty_table,
             alias   TYPE string,
             tabname TYPE string,
             ddtext  TYPE string,
             jtype   TYPE string,
             cond    TYPE string,
           END OF ty_table,
           tt_table TYPE STANDARD TABLE OF ty_table WITH EMPTY KEY.

    TYPES: BEGIN OF ty_field,
             sel       TYPE abap_bool,
             pos       TYPE i,
             alias     TYPE string,
             tabname   TYPE string,
             fieldname TYPE string,
             key       TYPE abap_bool,
             ddtext    TYPE string,
             datatype  TYPE string,
           END OF ty_field,
           tt_field TYPE STANDARD TABLE OF ty_field WITH EMPTY KEY.

    METHODS not_found
      IMPORTING i_type TYPE string
                i_id   TYPE string
      RAISING   cx_adt_res_not_found.

    METHODS bad_request
      IMPORTING i_text TYPE string
      RAISING   cx_adt_res_bad_request.
ENDCLASS.


CLASS zcl_sde_adt_res_join IMPLEMENTATION.

  METHOD get.
    DATA: lv_name TYPE tabname,
          lv_take TYPE tabname,
          lt_cand TYPE tt_candidate,
          lt_tab  TYPE tt_table,
          lt_fld  TYPE tt_field.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).
    TRANSLATE lv_name TO UPPER CASE.

    IF zcl_sde_sql=>exist_table( lv_name ) <> 1 AND zcl_sde_sql=>exist_view( lv_name ) <> 1.
      not_found( i_type = `table` i_id = CONV string( lv_name ) ).
    ENDIF.

    " The builder is stateful - an alias is handed out when a table is first
    " taken in and never reused - and there is no session here to keep that
    " state between requests. So the client sends the whole selection every
    " time, in the order it was made, and the model is replayed from the base
    " table. Same order, same aliases; a client that reorders its own list
    " renames its columns.
    DATA(lo_tools) = NEW zcl_sde_tools( i_tabname = lv_name ).

    DO c_max_tables TIMES.
      CLEAR lv_take.
      request->get_uri_query_parameter( EXPORTING name  = |t{ sy-index }|
                                        IMPORTING value = lv_take ).
      IF lv_take IS INITIAL.
        EXIT.
      ENDIF.
      TRANSLATE lv_take TO UPPER CASE.

      " TOGGLE_CANDIDATE ignores a name it does not know, which would answer
      " with a join quietly missing a table. Only what the dictionary offered
      " can be taken in; a table nobody proposed is a 400, not a silence.
      " Re-read every round: taking a table in discovers its neighbours too.
      DATA(lt_offered) = lo_tools->candidates( ).
      IF NOT line_exists( lt_offered[ tabname = lv_take ] ).
        bad_request( |{ lv_take } is not among the tables offered around | &&
                     |{ lv_name }. Ask without t-parameters to see what is.| ).
      ENDIF.
      lo_tools->toggle_table( lv_take ).
    ENDDO.

    DATA(lt_all_cand) = lo_tools->candidates( ).
    LOOP AT lt_all_cand INTO DATA(ls_cand).
      APPEND VALUE #( tabname   = ls_cand-tabname
                      ddtext    = ls_cand-ddtext
                      direction = ls_cand-direction
                      alias     = ls_cand-alias
                      selected  = ls_cand-selected ) TO lt_cand.
    ENDLOOP.

    DATA(lt_jtab) = lo_tools->join_tables( ).
    LOOP AT lt_jtab INTO DATA(ls_tab).
      APPEND VALUE #( alias   = ls_tab-alias
                      tabname = ls_tab-tabname
                      ddtext  = ls_tab-ddtext
                      jtype   = ls_tab-jtype
                      cond    = ls_tab-cond ) TO lt_tab.
    ENDLOOP.

    DATA(lt_jfld) = lo_tools->join_fields( ).
    LOOP AT lt_jfld INTO DATA(ls_fld).
      APPEND VALUE #( sel       = ls_fld-sel
                      pos       = ls_fld-pos
                      alias     = ls_fld-alias
                      tabname   = ls_fld-tabname
                      fieldname = ls_fld-fieldname
                      key       = ls_fld-keyflag
                      ddtext    = ls_fld-ddtext
                      datatype  = ls_fld-datatype ) TO lt_fld.
    ENDLOOP.

    DATA(lv_body) =
      |\{"table":"{ to_lower( lv_name ) }",| &&
      |"sql":{ /ui2/cl_json=>serialize( data = lo_tools->sql( ) ) },| &&
      |"candidates":{ /ui2/cl_json=>serialize(
                        data        = lt_cand
                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },| &&
      |"tables":{ /ui2/cl_json=>serialize(
                    data        = lt_tab
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },| &&
      |"fields":{ /ui2/cl_json=>serialize(
                    data        = lt_fld
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.

    response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      data            = lv_body ).
  ENDMETHOD.


  METHOD not_found.
    RAISE EXCEPTION TYPE cx_adt_res_not_found
      EXPORTING resource_type = i_type
                resource_id   = i_id.
  ENDMETHOD.


  METHOD bad_request.
    RAISE EXCEPTION TYPE cx_adt_res_bad_request
      EXPORTING explanation = i_text.
  ENDMETHOD.

ENDCLASS.
