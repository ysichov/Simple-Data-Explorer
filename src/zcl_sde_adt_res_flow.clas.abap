CLASS zcl_sde_adt_res_flow DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PRIVATE SECTION.
    " What the window gets back: the mermaid text ACE writes for one unit,
    " and enough about the unit to caption it. The diagram is a string and
    " nothing else - the page owns how it is drawn, the same way it owns how
    " the metrics table is drawn.
    TYPES: BEGIN OF ty_answer,
             object    TYPE string,
             type      TYPE string,
             program   TYPE string,
             include   TYPE string,
             unit      TYPE string,
             unit_type TYPE string,
             line_from TYPE i,
             line_to   TYPE i,
             mermaid   TYPE string,
           END OF ty_answer.

    " Lines whose collapsed stretch is open, as the page echoes them back:
    " "12,40,73". They are node numbers of this diagram, so they mean nothing
    " outside the slice they were produced for.
    METHODS expanded
      IMPORTING i_list          TYPE string
      RETURNING VALUE(rt_lines) TYPE zcl_ace_code_html=>tt_lines.
ENDCLASS.


CLASS zcl_sde_adt_res_flow IMPLEMENTATION.

  METHOD get.
    DATA: lv_name    TYPE string,
          lv_type    TYPE string,
          lv_head    TYPE string,
          lv_include TYPE string,
          lv_unit    TYPE string,
          lv_expand  TYPE string,
          lv_program TYPE program,
          lv_from    TYPE i,
          lv_to      TYPE i,
          lv_title   TYPE string,
          lv_utype   TYPE string.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).

    request->get_uri_query_parameter( EXPORTING name      = 'type'
                                                mandatory = abap_false
                                                default   = 'PROG'
                                      IMPORTING value     = lv_type ).

    " The include is what identifies the code: for a class it is the method's
    " own CM include, for a program the include the unit was found in. The
    " metrics row carries it, so the window never has to work it out.
    request->get_uri_query_parameter( EXPORTING name      = 'include'
                                                mandatory = abap_true
                                      IMPORTING value     = lv_include ).

    request->get_uri_query_parameter( EXPORTING name      = 'unit'
                                                mandatory = abap_false
                                                default   = ''
                                      IMPORTING value     = lv_unit ).

    request->get_uri_query_parameter( EXPORTING name      = 'expand'
                                                mandatory = abap_false
                                                default   = ''
                                      IMPORTING value     = lv_expand ).

    zcl_sde_ace_source=>resolve( EXPORTING i_name     = lv_name
                                           i_type     = lv_type
                                 IMPORTING ev_type    = lv_head
                                           ev_program = lv_program ).

    DATA(ls_source) = zcl_sde_ace_source=>parse( lv_program ).

    DATA(lv_inc) = CONV program( to_upper( lv_include ) ).

    " A call is what the picture is about: it gets a node of its own instead
    " of disappearing into an "N operations" block. The parser fills that
    " table one statement at a time, on demand, so the whole include has to
    " be asked for before the scheme is drawn.
    zcl_ace_parser=>parse_calls( EXPORTING i_program = lv_program
                                           i_include = lv_inc
                                 CHANGING  cs_source = ls_source ).

    READ TABLE ls_source-tt_progs WITH KEY include = lv_inc INTO DATA(ls_prog).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_adt_res_not_found
        EXPORTING resource_type = `include`
                  resource_id   = lv_include.
    ENDIF.

    IF lv_unit IS INITIAL.
      " No unit named: the whole include, which is what a program's top-level
      " code amounts to.
      lv_from  = 1.
      lv_to    = lines( ls_prog-source_tab ).
      lv_title = to_upper( lv_include ).
    ELSE.
      " ACE already knows where each unit of an include begins and ends, and
      " under which name the metrics list showed it. Asking it here is what
      " keeps the row that was clicked and the code that is drawn the same.
      DATA(lt_units) = zcl_ace_metrics=>unit_boundaries( is_parse_data = ls_source
                                                         is_prog       = ls_prog ).
      LOOP AT lt_units INTO DATA(ls_unit).
        CHECK to_upper( ls_unit-qname ) = to_upper( lv_unit ).
        lv_from  = ls_unit-line_from.
        lv_to    = ls_unit-line_to.
        lv_title = ls_unit-qname.
        lv_utype = ls_unit-unit_type.
        EXIT.
      ENDLOOP.
      IF lv_from = 0 OR lv_to < lv_from.
        RAISE EXCEPTION TYPE cx_adt_res_not_found
          EXPORTING resource_type = `code unit`
                    resource_id   = lv_unit.
      ENDIF.
    ENDIF.

    " Only the unit's own lines go in. The keyword table and the scan stay
    " whole and are read through the offset, which is how ACE's own source
    " popups look at a stretch of an include.
    DATA lt_slice LIKE ls_prog-source_tab.
    LOOP AT ls_prog-source_tab ASSIGNING FIELD-SYMBOL(<lv_line>)
      FROM lv_from TO lv_to.
      APPEND <lv_line> TO lt_slice.
    ENDLOOP.

    DATA(ls_answer) = VALUE ty_answer(
      object    = to_lower( lv_name )
      type      = to_lower( lv_head )
      program   = to_lower( lv_program )
      include   = to_lower( lv_include )
      unit      = lv_title
      unit_type = to_lower( lv_utype )
      line_from = lv_from
      line_to   = lv_to
      mermaid   = zcl_ace_code_html=>build_scheme( it_source   = lt_slice
                                                   it_kw       = ls_prog-t_keywords
                                                   io_scan     = ls_prog-scan
                                                   i_title     = lv_title
                                                   it_expanded = expanded( lv_expand )
                                                   i_offset    = lv_from ) ).

    response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      data            = /ui2/cl_json=>serialize( data        = ls_answer
                                                 pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
  ENDMETHOD.


  METHOD expanded.
    CHECK i_list IS NOT INITIAL.
    SPLIT i_list AT ',' INTO TABLE DATA(lt_part).
    LOOP AT lt_part INTO DATA(lv_part).
      CONDENSE lv_part.
      CHECK lv_part IS NOT INITIAL AND lv_part CO '0123456789'.
      APPEND CONV i( lv_part ) TO rt_lines.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
