CLASS zcl_sde_adt_res_review DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PRIVATE SECTION.
    " One object of the request, with what the review has made of it. The counts
    " are of blocks, not lines: a reviewer approves a block.
    TYPES: BEGIN OF ty_object,
             objtype      TYPE string,
             obj_name     TYPE string,
             class_name   TYPE string,
             display_name TYPE string,
             author       TYPE string,
             author_name  TYPE string,
             is_created   TYPE abap_bool,
             hunks        TYPE i,
             inserted     TYPE i,
             deleted      TYPE i,
             modified     TYPE i,
             approved     TYPE i,
             declined     TYPE i,
             open         TYPE i,
           END OF ty_object,
           tt_object TYPE STANDARD TABLE OF ty_object WITH EMPTY KEY.

    TYPES: BEGIN OF ty_reviewer,
             reviewer      TYPE string,
             reviewer_name TYPE string,
             saved_at      TYPE string,
             approved      TYPE i,
             declined      TYPE i,
             notes         TYPE i,
           END OF ty_reviewer,
           tt_reviewer TYPE STANDARD TABLE OF ty_reviewer WITH EMPTY KEY.

    TYPES: BEGIN OF ty_save,
             saved_at      TYPE string,
             saved_by      TYPE string,
             saved_by_name TYPE string,
             approved      TYPE i,
             declined      TYPE i,
             notes         TYPE i,
           END OF ty_save,
           tt_save TYPE STANDARD TABLE OF ty_save WITH EMPTY KEY.

    METHODS bad_request
      IMPORTING i_text TYPE string
      RAISING   cx_adt_res_bad_request.
ENDCLASS.


CLASS zcl_sde_adt_res_review IMPLEMENTATION.

  METHOD get.
    DATA: lv_trkorr TYPE trkorr,
          lv_remote TYPE verssysnam,
          lt_object TYPE tt_object,
          lt_person TYPE tt_reviewer,
          lt_save   TYPE tt_save.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_trkorr ).
    TRANSLATE lv_trkorr TO UPPER CASE.

    " A review run against another system is a different review: its baseline is
    " the state that system already has, so its blocks and its approvals are not
    " the ones of the plain review. That is why REMOTE is part of the key, and
    " why it has to travel with the request.
    request->get_uri_query_parameter( EXPORTING name      = 'remote'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_remote ).
    TRANSLATE lv_remote TO UPPER CASE.

    " Not having the table is a state AVE handles with a setup page rather than
    " an error, and so does this: nothing is broken, there is simply nowhere for
    " a review to have been saved.
    DATA(lv_table) = zcl_ave_acr_repository=>has_review_table( ).
    DATA(lv_saved) = abap_false.
    DATA ls_payload TYPE zif_ave_acr_types=>ty_saved_payload.

    IF lv_table = abap_true.
      lv_saved = zcl_ave_acr_repository=>load_review_payload(
                   EXPORTING iv_trkorr  = lv_trkorr
                             iv_remote  = lv_remote
                   CHANGING  cs_payload = ls_payload ).
    ENDIF.

    IF lv_saved = abap_true.
      LOOP AT ls_payload-obj_stats INTO DATA(ls_stat).
        DATA(lv_approved) = 0.
        DATA(lv_declined) = 0.

        " An action names a block, not an object, so the blocks of this object
        " are what ties the two together.
        LOOP AT ls_payload-hunks INTO DATA(ls_hunk)
          WHERE objtype = ls_stat-objtype AND obj_name = ls_stat-obj_name.
          LOOP AT ls_payload-hunk_actions INTO DATA(ls_action)
            WHERE hunk_key = ls_hunk-hunk_key.
            CASE ls_action-action.
              WHEN 'A'. lv_approved = lv_approved + 1.
              WHEN 'D'. lv_declined = lv_declined + 1.
            ENDCASE.
          ENDLOOP.
        ENDLOOP.

        APPEND VALUE #( objtype      = ls_stat-objtype
                        obj_name     = ls_stat-obj_name
                        class_name   = ls_stat-class_name
                        display_name = ls_stat-display_name
                        author       = ls_stat-author
                        author_name  = ls_stat-author_name
                        is_created   = ls_stat-is_created
                        hunks        = ls_stat-hunk_count
                        inserted     = ls_stat-ins_count
                        deleted      = ls_stat-del_count
                        modified     = ls_stat-mod_count
                        approved     = lv_approved
                        declined     = lv_declined
                        open         = ls_stat-hunk_count - lv_approved - lv_declined
                      ) TO lt_object.
      ENDLOOP.

      LOOP AT ls_payload-user_states INTO DATA(ls_user).
        APPEND VALUE #( reviewer      = ls_user-reviewer
                        reviewer_name = ls_user-reviewer_name
                        saved_at      = |{ ls_user-saved_at }|
                        approved      = lines( ls_user-approved )
                        declined      = lines( ls_user-declined )
                        notes         = lines( ls_user-notes ) ) TO lt_person.
      ENDLOOP.

      LOOP AT ls_payload-history INTO DATA(ls_history).
        APPEND VALUE #( saved_at      = |{ ls_history-saved_at }|
                        saved_by      = ls_history-saved_by
                        saved_by_name = ls_history-saved_by_name
                        approved      = ls_history-approved_count
                        declined      = ls_history-declined_count
                        notes         = ls_history-note_count ) TO lt_save.
      ENDLOOP.
    ENDIF.

    DATA(lv_body) =
      |\{"request":"{ to_lower( lv_trkorr ) }",| &&
      |"remote":"{ to_lower( lv_remote ) }",| &&
      |"table":{ COND string( WHEN lv_table = abap_true THEN `true` ELSE `false` ) },| &&
      |"saved":{ COND string( WHEN lv_saved = abap_true THEN `true` ELSE `false` ) },| &&
      |"saved_at":"{ COND string( WHEN lv_saved = abap_true
                                  THEN |{ ls_payload-last_saved_at }| ELSE `` ) }",| &&
      |"saved_by":"{ ls_payload-last_saved_by }",| &&
      |"objects":{ /ui2/cl_json=>serialize(
                     data        = lt_object
                     pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },| &&
      |"reviewers":{ /ui2/cl_json=>serialize(
                       data        = lt_person
                       pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },| &&
      |"history":{ /ui2/cl_json=>serialize(
                     data        = lt_save
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
