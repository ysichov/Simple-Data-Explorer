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

    " One line of the diff. The op is what AVE's engine returns: '=' kept,
    " '-' from the old version, '+' from the new one.
    TYPES: BEGIN OF ty_op,
             op   TYPE string,
             text TYPE string,
           END OF ty_op,
           tt_op TYPE STANDARD TABLE OF ty_op WITH EMPTY KEY.

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

    " The source of one recorded version. A number that is not in the directory
    " is refused rather than diffed against nothing, which would report the
    " whole part as added and look like a real answer.
    METHODS source_of
      IMPORTING io_vrsd          TYPE REF TO zcl_ave_vrsd
                i_versno         TYPE versno
      RETURNING VALUE(rt_source) TYPE abaptxt255_tab
      RAISING   zcx_ave cx_adt_res_bad_request.
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
          lv_from  TYPE versno,
          lv_to    TYPE versno,
          lt_op    TYPE tt_op,
          lt_old   TYPE abaptxt255_tab,
          lt_new   TYPE abaptxt255_tab,
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

    " Both present, the answer is the difference between those two versions of
    " the part. An empty FROM is the oldest version compared against nothing,
    " which is how a first version reads: every line added.
    request->get_uri_query_parameter( EXPORTING name      = 'from'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_from ).
    request->get_uri_query_parameter( EXPORTING name      = 'to'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_to ).

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
      WHEN 'TR'   THEN 'TR'
      WHEN 'DEVC' THEN 'DEVC'
      ELSE '' ).

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
                            name      = CONV string( ls_part-object_name )
                            part_type = ls_part-type ) TO lt_part.
          ENDLOOP.
        CATCH zcx_ave INTO DATA(lx_parts).
          bad_request( |AVE cannot list the parts of { lv_name }: { reason( lx_parts ) }| ).
      ENDTRY.

      " A transport request and a package are scopes, not objects: what comes
      " back is the objects in them, and an object is drilled into rather than
      " asked for the versions of a part it does not have. The client is told
      " which of the two it is holding.
      DATA(lv_scope) = COND string( WHEN lv_type = 'TR' OR lv_type = 'DEVC'
                                    THEN `true` ELSE `false` ).

      lv_body = |\{"object":"{ to_lower( lv_name ) }",| &&
                |"type":"{ to_lower( lv_type ) }",| &&
                |"scope":{ lv_scope },| &&
                |"parts":{ /ui2/cl_json=>serialize(
                             data        = lt_part
                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.
    ELSE.
      IF lv_ptype IS INITIAL.
        bad_request( |Reading the versions of part { lv_part } needs its type in ptype.| ).
      ENDIF.

      " A key that is not one of this object's parts reads as an object with no
      " history, which is indistinguishable from a part nobody ever changed.
      " The parts list is cheap by construction, so it is worth asking.
      TRY.
          DATA(lt_known) = lo_object->get_parts( ).
        CATCH zcx_ave INTO DATA(lx_known).
          bad_request( |AVE cannot list the parts of { lv_name }: { reason( lx_known ) }| ).
      ENDTRY.
      IF NOT line_exists( lt_known[ object_name = to_upper( lv_part )
                                    type        = to_upper( lv_ptype ) ] ).
        bad_request( |{ lv_part } of type { lv_ptype } is not a part of { lv_name }.| &&
                     | Ask for the parts list first; a method key carries the class name| &&
                     | padded to thirty characters and every blank of it matters.| ).
      ENDIF.

      TRY.
          DATA(lo_vrsd) = NEW zcl_ave_vrsd( type = CONV #( to_upper( lv_ptype ) )
                                            name = CONV #( to_upper( lv_part ) ) ).

          IF lv_to IS INITIAL.
            LOOP AT lo_vrsd->vrsd_list INTO DATA(ls_vrsd).
              DATA(lo_version) = NEW zcl_ave_version( ls_vrsd ).
              APPEND VALUE #( version     = |{ lo_version->version_number }|
                              date        = |{ lo_version->date }|
                              time        = |{ lo_version->time }|
                              author      = CONV string( lo_version->author )
                              author_name = CONV string( lo_version->author_name )
                              request     = CONV string( lo_version->request )
                              task        = CONV string( lo_version->task )
                            ) TO lt_ver.
            ENDLOOP.
            " AVE sorts the directory ascending so that 99998, its key for the
            " active version, lands after the numbered ones. A reader wants the
            " newest first, and so does the client: it compares a version with
            " the one below it, which is the change that version made.
            SORT lt_ver BY version DESCENDING.
          ELSE.
            lt_new = source_of( io_vrsd = lo_vrsd i_versno = lv_to ).
            IF lv_from IS NOT INITIAL.
              lt_old = source_of( io_vrsd = lo_vrsd i_versno = lv_from ).
            ENDIF.
          ENDIF.

        CATCH zcx_ave INTO DATA(lx_ver).
          bad_request( |AVE cannot read the versions of { lv_part }: { reason( lx_ver ) }| ).
      ENDTRY.

      IF lv_to IS INITIAL.
        lv_body = |\{"object":"{ to_lower( lv_name ) }",| &&
                  |"type":"{ to_lower( lv_type ) }",| &&
                  |"part":"{ to_lower( lv_part ) }",| &&
                  |"part_type":"{ to_lower( lv_ptype ) }",| &&
                  |"versions":{ /ui2/cl_json=>serialize(
                                  data        = lt_ver
                                  pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.
      ELSE.
        " AVE's own engine, unchanged: it pairs the declarations of a class
        " section by signature rather than by position, because SAP regenerates
        " those includes in an arbitrary order and a plain line diff reports
        " every moved declaration as a deletion and an insertion far apart.
        DATA(lt_diff) = zcl_ave_popup_diff=>compute_diff( it_old = lt_old
                                                          it_new = lt_new ).
        DATA lv_added   TYPE i.
        DATA lv_deleted TYPE i.
        DATA lv_kept    TYPE i.
        LOOP AT lt_diff INTO DATA(ls_diff).
          CASE ls_diff-op.
            WHEN '+'.  lv_added   = lv_added + 1.
            WHEN '-'.  lv_deleted = lv_deleted + 1.
            WHEN OTHERS. lv_kept  = lv_kept + 1.
          ENDCASE.
          APPEND VALUE #( op = CONV string( ls_diff-op ) text = ls_diff-text ) TO lt_op.
        ENDLOOP.

        " VERSNO is numeric, so an absent FROM would print as 00000 and read
        " like a version number somebody could look up.
        DATA(lv_from_text) = COND string( WHEN lv_from IS INITIAL THEN ``
                                          ELSE |{ lv_from }| ).

        lv_body = |\{"object":"{ to_lower( lv_name ) }",| &&
                  |"type":"{ to_lower( lv_type ) }",| &&
                  |"part":"{ to_lower( lv_part ) }",| &&
                  |"part_type":"{ to_lower( lv_ptype ) }",| &&
                  |"from":"{ lv_from_text }","to":"{ lv_to }",| &&
                  |"added":{ lv_added },"deleted":{ lv_deleted },"kept":{ lv_kept },| &&
                  |"ops":{ /ui2/cl_json=>serialize(
                             data        = lt_op
                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.
      ENDIF.
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


  METHOD source_of.
    LOOP AT io_vrsd->vrsd_list INTO DATA(ls_vrsd) WHERE versno = i_versno.
      rt_source = NEW zcl_ave_version( ls_vrsd )->get_source( ).
      RETURN.
    ENDLOOP.
    bad_request( |Version { i_versno } is not in the version directory of this part.| ).
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
