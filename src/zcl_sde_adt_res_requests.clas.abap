CLASS zcl_sde_adt_res_requests DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PRIVATE SECTION.
    " One transport request as the search lists it. Dates and times are passed
    " as the dictionary holds them, YYYYMMDD and HHMMSS. Formatting belongs to
    " the reader, who knows the locale.
    TYPES: BEGIN OF ty_request,
             request    TYPE string,
             text       TYPE string,
             owner      TYPE string,
             owner_name TYPE string,
             type       TYPE string,
             status     TYPE string,
             date       TYPE string,
             time       TYPE string,
           END OF ty_request,
           tt_request TYPE STANDARD TABLE OF ty_request WITH EMPTY KEY.

    METHODS bad_request
      IMPORTING i_text TYPE string
      RAISING   cx_adt_res_bad_request.
ENDCLASS.


CLASS zcl_sde_adt_res_requests IMPLEMENTATION.

  METHOD get.
    DATA: lv_user     TYPE string,
          lv_released TYPE string,
          lv_uname    TYPE syuname,
          lt_status   TYPE RANGE OF trstatus,
          lt_function TYPE RANGE OF trfunction,
          lt_trkorr   TYPE STANDARD TABLE OF trkorr WITH EMPTY KEY,
          lt_request  TYPE tt_request,
          lv_body     TYPE string.

    " Whose requests. Nobody named is the user asking, which is how SE09 opens,
    " and it is the server that knows who that is - the page does not.
    request->get_uri_query_parameter( EXPORTING name      = 'user'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_user ).
    lv_user = to_upper( condense( lv_user ) ).
    IF lv_user IS INITIAL.
      lv_user = sy-uname.
    ELSEIF strlen( lv_user ) > 12.
      bad_request( |{ lv_user } is not a user name: an SAP user name has at most 12 characters.| ).
    ELSEIF lv_user CA ` *+%`.
      " A pattern would be compared as a name and quietly find nothing.
      bad_request( |Name one user, not a pattern: { lv_user }.| ).
    ENDIF.
    lv_uname = lv_user.

    " Open requests, unless the released ones are asked for as well.
    request->get_uri_query_parameter( EXPORTING name      = 'released'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_released ).
    IF lv_released <> `` AND lv_released <> `true` AND lv_released <> `false`.
      bad_request( |released is true or false, not { lv_released }.| ).
    ENDIF.

    " Open is what SE09 calls modifiable, protected or not. A request whose
    " release has started is no longer open, so it comes with the released ones.
    lt_status = VALUE #( sign = 'I' option = 'EQ' ( low = 'D' ) ( low = 'L' ) ).
    IF lv_released = `true`.
      lt_status = VALUE #( BASE lt_status sign = 'I' option = 'EQ'
                           ( low = 'O' ) ( low = 'R' ) ( low = 'N' ) ).
    ENDIF.

    " The kinds of request a developer creates: workbench, customizing, transport
    " of copies and the three relocations. Tasks are not listed here - a task is
    " how a request becomes somebody's, below.
    lt_function = VALUE #( sign = 'I' option = 'EQ'
                           ( low = 'K' ) ( low = 'W' ) ( low = 'T' )
                           ( low = 'C' ) ( low = 'O' ) ( low = 'E' ) ).

    " A request is the user's when they own it, and also when all they have in
    " it is a task under somebody else's request: SE09 lists those as well. The
    " status that counts is the request's own - a task is often released long
    " before the request it belongs to.
    SELECT trkorr FROM e070
      WHERE as4user    = @lv_uname
        AND strkorr    = @space
        AND trfunction IN @lt_function
        AND trstatus   IN @lt_status
      INTO TABLE @lt_trkorr.

    SELECT r~trkorr FROM e070 AS t
      INNER JOIN e070 AS r ON r~trkorr = t~strkorr
      WHERE t~as4user    = @lv_uname
        AND r~trfunction IN @lt_function
        AND r~trstatus   IN @lt_status
      APPENDING TABLE @lt_trkorr.

    " A user's own request usually holds their own task too, so most requests
    " are found twice.
    SORT lt_trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_trkorr.

    DATA(lo_author) = NEW zcl_ave_author( ).
    LOOP AT lt_trkorr INTO DATA(lv_trkorr).
      " AVE's own header read, so a description is chosen the way the rest of
      " the Versions window chooses it.
      DATA(ls_head) = zcl_ave_request=>get_header( lv_trkorr ).
      " Gone between the two reads: deleted in the meantime, and not a request
      " of anybody's any more.
      IF ls_head-found = abap_false.
        CONTINUE.
      ENDIF.
      APPEND VALUE #( request    = CONV string( ls_head-trkorr )
                      text       = CONV string( ls_head-as4text )
                      owner      = CONV string( ls_head-as4user )
                      owner_name = lo_author->get_name( ls_head-as4user )
                      type       = SWITCH string( ls_head-trfunction
                                     WHEN 'K' THEN `workbench`
                                     WHEN 'W' THEN `customizing`
                                     WHEN 'T' THEN `transport of copies`
                                     WHEN 'C' OR 'O' OR 'E' THEN `relocation`
                                     ELSE CONV string( ls_head-trfunction ) )
                      status     = SWITCH string( ls_head-trstatus
                                     WHEN 'D' THEN `modifiable`
                                     WHEN 'L' THEN `modifiable, protected`
                                     WHEN 'O' THEN `release started`
                                     WHEN 'R' THEN `released`
                                     WHEN 'N' THEN `released, import protected`
                                     ELSE CONV string( ls_head-trstatus ) )
                      date       = |{ ls_head-as4date }|
                      time       = |{ ls_head-as4time }| ) TO lt_request.
    ENDLOOP.

    " Newest first: the request somebody is looking for is nearly always one
    " they touched lately.
    SORT lt_request BY date DESCENDING time DESCENDING request DESCENDING.

    lv_body = |\{"user":"{ escape( val = lv_user format = cl_abap_format=>e_json_string ) }",| &&
              |"released":{ COND string( WHEN lv_released = `true` THEN `true` ELSE `false` ) },| &&
              |"requests":{ /ui2/cl_json=>serialize(
                              data        = lt_request
                              pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.

    response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      data            = lv_body ).
  ENDMETHOD.


  METHOD bad_request.
    RAISE EXCEPTION TYPE cx_adt_res_bad_request
      EXPORTING explanation = i_text.
  ENDMETHOD.

ENDCLASS.
