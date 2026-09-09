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

    METHODS not_found
      IMPORTING i_type TYPE string
                i_id   TYPE string
      RAISING   cx_adt_res_not_found.

    METHODS bad_request
      IMPORTING i_text TYPE string
      RAISING   cx_adt_res_bad_request.
ENDCLASS.


CLASS zcl_sde_adt_res_metrics IMPLEMENTATION.

  METHOD get.
    DATA: lv_name    TYPE string,
          lv_type    TYPE string,
          lv_sub     TYPE string,
          lv_program TYPE program,
          lv_clsname TYPE seoclsname,
          lv_clstype TYPE seoclstype,
          lv_progname TYPE progname,
          ls_source  TYPE zif_ace_parse_data=>ts_parse_data,
          lt_unit    TYPE tt_unit.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).
    lv_name = to_upper( lv_name ).

    request->get_uri_query_parameter( EXPORTING name      = 'type'
                                                mandatory = abap_false
                                                default   = 'PROG'
                                      IMPORTING value     = lv_type ).
    lv_type = to_upper( lv_type ).

    " The context menu passes the ADT type, which carries a subtype: CLAS/OC,
    " PROG/P. Only the part in front of the slash decides how the name is
    " resolved to the program ACE parses.
    SPLIT lv_type AT '/' INTO lv_type lv_sub.

    CASE lv_type.
      WHEN 'CLAS' OR 'INTF'.
        lv_clsname = lv_name.
        SELECT SINGLE clstype FROM seoclass
          WHERE clsname = @lv_clsname
          INTO @lv_clstype.
        IF sy-subrc <> 0.
          not_found( i_type = `class` i_id = lv_name ).
        ENDIF.
        " An interface pool ends in IP, a class pool in CP. The name is padded
        " to 30 characters first - that is how the generated pool is named.
        lv_program = lv_name && repeat( val = `=` occ = 30 - strlen( lv_name ) ).
        IF lv_clstype = 1.
          lv_program = lv_program && `IP`.
        ELSE.
          lv_program = lv_program && `CP`.
        ENDIF.

      WHEN 'PROG' OR 'INCL'.
        lv_progname = lv_name.
        SELECT SINGLE name FROM trdir
          WHERE name = @lv_progname
          INTO @DATA(lv_exists).
        IF sy-subrc <> 0.
          not_found( i_type = `program` i_id = lv_name ).
        ENDIF.
        lv_program = lv_name.

      WHEN OTHERS.
        bad_request( |Type { lv_type } is not supported here.| &&
                     | Metrics are computed for CLAS, INTF, PROG and INCL.| ).
    ENDCASE.

    " ZCL_ACE_METRICS aggregates every parsed include whose program is the one
    " asked for, so each include is parsed under the pool rather than under
    " itself. A class pool holds nothing but INCLUDE statements; the methods
    " live in its CM includes, and without them the answer would be an empty
    " unit list rather than an error.
    zcl_ace_parser=>parse( EXPORTING i_program = lv_program
                                     i_include = lv_program
                           CHANGING  cs_source = ls_source ).

    SELECT include FROM d010inc
      WHERE master = @lv_program
      INTO TABLE @DATA(lt_include).

    LOOP AT lt_include INTO DATA(ls_include).
      IF ls_include-include = lv_program.
        CONTINUE.
      ENDIF.
      " D010INC also lists the system includes every program gets - <SYSINI>
      " and its kin. Their units (SYSTEM-EXIT, %_CTL_END) belong to SAP's
      " runtime, not to the object being measured, and left in they show up as
      " rows nobody wrote and are counted into the totals. The angle bracket
      " is what marks them: it cannot occur in a repository object name.
      IF ls_include-include CS '<'.
        CONTINUE.
      ENDIF.
      zcl_ace_parser=>parse( EXPORTING i_program = lv_program
                                       i_include = ls_include-include
                             CHANGING  cs_source = ls_source ).
    ENDLOOP.

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
      |"type":"{ to_lower( lv_type ) }",| &&
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
