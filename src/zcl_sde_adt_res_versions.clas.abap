CLASS zcl_sde_adt_res_versions DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PRIVATE SECTION.
    " One versionable part of an object: a program is one, a class is its
    " sections, its local includes and one per method.
    TYPES: BEGIN OF ty_part,
             class     TYPE string,
             unit      TYPE string,
             name      TYPE string,
             part_type TYPE string,
           END OF ty_part,
           tt_part TYPE STANDARD TABLE OF ty_part WITH EMPTY KEY.

    " Dates and times are passed as the dictionary holds them, YYYYMMDD and
    " HHMMSS. Formatting belongs to the reader, who knows the locale.
    TYPES: BEGIN OF ty_version,
             version     TYPE string,
             date        TYPE string,
             time        TYPE string,
             author      TYPE string,
             author_name TYPE string,
             request     TYPE string,
             task        TYPE string,
           END OF ty_version,
           tt_version TYPE STANDARD TABLE OF ty_version WITH EMPTY KEY.

    METHODS not_found
      IMPORTING i_type TYPE string
                i_id   TYPE string
      RAISING   cx_adt_res_not_found.

    METHODS bad_request
      IMPORTING i_text TYPE string
      RAISING   cx_adt_res_bad_request.

    " ZCX_AVE carries no text of its own - its constructor only passes the
    " exception it wrapped - so the sentence worth showing is down the chain.
    CLASS-METHODS reason
      IMPORTING ix_error       TYPE REF TO cx_root
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.


CLASS zcl_sde_adt_res_versions IMPLEMENTATION.

  METHOD get.
    DATA: lv_name  TYPE string,
          lv_type  TYPE string,
          lv_sub   TYPE string,
          lv_part  TYPE string,
          lv_ptype TYPE string,
          lv_ave   TYPE string,
          lt_part  TYPE tt_part,
          lt_ver   TYPE tt_version,
          lv_body  TYPE string.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).
    lv_name = to_upper( lv_name ).

    request->get_uri_query_parameter( EXPORTING name      = 'type'
                                                mandatory = abap_false
                                                default   = 'PROG'
                                      IMPORTING value     = lv_type ).
    lv_type = to_upper( lv_type ).
    " The caller may pass the ADT type as it comes from the object tree, with
    " its subtype: CLAS/OC, PROG/P. Only the part in front of the slash counts.
    SPLIT lv_type AT '/' INTO lv_type lv_sub.

    " Which part to read the versions of. Absent, the answer is the parts list
    " itself - the two together are the left and the middle pane of AVE, and
    " keeping them apart keeps a class of eighty methods from reading the
    " version directory eighty times to draw a list of names.
    request->get_uri_query_parameter( EXPORTING name      = 'part'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_part ).
    request->get_uri_query_parameter( EXPORTING name      = 'ptype'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_ptype ).

    " ADT names an object type differently from AVE's factory, and the DDIC
    " types carry their VRSD part type already.
    lv_ave = SWITCH string( lv_type
      WHEN 'CLAS' THEN 'CLAS'
      WHEN 'INTF' THEN 'INTF'
      WHEN 'PROG' THEN 'PROG'
      WHEN 'INCL' THEN 'PROG'
      WHEN 'FUGR' THEN 'FUGR'
      WHEN 'FUNC' THEN 'FUNC'
      WHEN 'DDLS' THEN 'DDLS'
      WHEN 'TABL' THEN 'TABD'
      WHEN 'DOMA' THEN 'DOMD'
      WHEN 'DTEL' THEN 'DTED'
      ELSE '' ).

    IF lv_type = 'TR' OR lv_type = 'DEVC'.
      " AVE reads these, and reading them is the point of AVE - but a request
      " or a package is dozens of objects, and AVE shows a progress bar with an
      " estimate while it works. One blocking HTTP call has nowhere to put that,
      " so it is refused rather than left to time out.
      bad_request( |A { lv_type } is read object by object and needs a progress| &&
                   | channel this resource does not have yet. Ask for one object.| ).
    ENDIF.
    IF lv_ave IS INITIAL.
      bad_request( |Type { lv_type } is not supported here.| &&
                   | Versions are read for CLAS, INTF, PROG, INCL, FUGR, FUNC,| &&
                   | DDLS, TABL, DOMA and DTEL.| ).
    ENDIF.

    TRY.
        DATA(lo_object) = NEW zcl_ave_object_factory( )->get_instance(
                              object_type = lv_ave
                              object_name = CONV #( lv_name ) ).
      CATCH zcx_ave.
        " The factory raises this for an object it cannot find, which is the
        " only thing it promises about the exception.
        not_found( i_type = to_lower( lv_type ) i_id = lv_name ).
    ENDTRY.

    IF lv_part IS INITIAL.
      TRY.
          DATA(lt_parts) = lo_object->get_parts( ).
          LOOP AT lt_parts INTO DATA(ls_part).
            APPEND VALUE #( class     = ls_part-class
                            unit      = ls_part-unit
                            name      = condense( CONV string( ls_part-object_name ) )
                            part_type = ls_part-type ) TO lt_part.
          ENDLOOP.
        CATCH zcx_ave INTO DATA(lx_parts).
          bad_request( |AVE cannot list the parts of { lv_name }: { reason( lx_parts ) }| ).
      ENDTRY.

      lv_body = |\{"object":"{ to_lower( lv_name ) }",| &&
                |"type":"{ to_lower( lv_type ) }",| &&
                |"parts":{ /ui2/cl_json=>serialize(
                             data        = lt_part
                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.
    ELSE.
      IF lv_ptype IS INITIAL.
        bad_request( |Reading the versions of part { lv_part } needs its type in ptype.| ).
      ENDIF.

      TRY.
          DATA(lo_vrsd) = NEW zcl_ave_vrsd( type = CONV #( to_upper( lv_ptype ) )
                                            name = CONV #( to_upper( lv_part ) ) ).
          LOOP AT lo_vrsd->vrsd_list INTO DATA(ls_vrsd).
            DATA(lo_version) = NEW zcl_ave_version( ls_vrsd ).
            APPEND VALUE #( version     = |{ lo_version->version_number }|
                            date        = |{ lo_version->date }|
                            time        = |{ lo_version->time }|
                            author      = condense( CONV string( lo_version->author ) )
                            author_name = condense( CONV string( lo_version->author_name ) )
                            request     = condense( CONV string( lo_version->request ) )
                            task        = condense( CONV string( lo_version->task ) )
                          ) TO lt_ver.
          ENDLOOP.
        CATCH zcx_ave INTO DATA(lx_ver).
          bad_request( |AVE cannot read the versions of { lv_part }: { reason( lx_ver ) }| ).
      ENDTRY.

      lv_body = |\{"object":"{ to_lower( lv_name ) }",| &&
                |"type":"{ to_lower( lv_type ) }",| &&
                |"part":"{ to_lower( lv_part ) }",| &&
                |"part_type":"{ to_lower( lv_ptype ) }",| &&
                |"versions":{ /ui2/cl_json=>serialize(
                                data        = lt_ver
                                pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.
    ENDIF.

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


  METHOD reason.
    DATA(lo_error) = ix_error.
    WHILE lo_error IS BOUND.
      DATA(lv_text) = lo_error->get_text( ).
      IF lv_text IS NOT INITIAL.
        IF rv_text IS INITIAL.
          rv_text = lv_text.
        ELSE.
          rv_text = rv_text && ` - ` && lv_text.
        ENDIF.
      ENDIF.
      lo_error = lo_error->previous.
    ENDWHILE.
    IF rv_text IS INITIAL.
      rv_text = `it raised an exception carrying no message.`.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
