CLASS zcl_sde_adt_res_review DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.
    METHODS post REDEFINITION.

  PRIVATE SECTION.
    " TEMPORARY, FOR TESTING ONLY - REMOVE.
    " AVE refuses to let a developer approve or decline their own block, and so
    " should this. It is off while the write path is being tried out on a
    " request whose every block belongs to the person testing it, because with
    " the rule on there would be nothing to press.
    CONSTANTS c_allow_self_review TYPE abap_bool VALUE abap_true.

    " One object of the request, with what the review has made of it. The counts
    " are of blocks, not lines: a reviewer approves a block.
    TYPES: BEGIN OF ty_object,
             objtype      TYPE string,
             obj_name     TYPE string,
             class_name   TYPE string,
             " The section a part that belongs to no class falls into. AVE's
             " own label, because the report and this table have to break their
             " groups in the same places.
             group        TYPE string,
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

    " One changed block of one part, with whatever verdict it already carries,
    " and where it sits in the operations of the stored diff.
    TYPES: BEGIN OF ty_block,
             hunk_key      TYPE string,
             hunk_no       TYPE i,
             start_line    TYPE i,
             change_count  TYPE i,
             change_kind   TYPE string,
             author        TYPE string,
             author_name   TYPE string,
             op_from       TYPE i,
             op_to         TYPE i,
             action        TYPE string,
             reviewer      TYPE string,
             reviewer_name TYPE string,
             note          TYPE string,
           END OF ty_block,
           tt_block TYPE STANDARD TABLE OF ty_block WITH EMPTY KEY.

    TYPES: BEGIN OF ty_op,
             op   TYPE string,
             text TYPE string,
           END OF ty_op,
           tt_op TYPE STANDARD TABLE OF ty_op WITH EMPTY KEY.

    METHODS part_body
      IMPORTING is_payload     TYPE zif_ave_acr_types=>ty_saved_payload
                i_trkorr       TYPE trkorr
                i_part         TYPE versobjnam
                i_ptype        TYPE versobjtyp
                i_table        TYPE abap_bool
                i_saved        TYPE abap_bool
      RETURNING VALUE(rv_json) TYPE string.

    METHODS locate_blocks
      IMPORTING it_diff  TYPE zif_ave_popup_types=>ty_t_diff
      CHANGING  ct_block TYPE tt_block.

    " What the page asks for: one reviewer action on one block.
    " SAVED_AT is the stamp the page last read. A review is written by several
    " people at once, and a save writes the whole payload, so a write built on
    " a state that has since moved would take somebody's approvals with it.
    TYPES: BEGIN OF ty_command,
             hunk_key TYPE string,
             action   TYPE string,
             note     TYPE string,
             saved_at TYPE string,
           END OF ty_command.

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

    " Name a part and the answer is that part instead of the summary: its
    " blocks and the lines behind them. Both are read out of the saved payload,
    " never computed - a prepared review already holds the diff, and what it
    " stores is the operations rather than the rendering.
    DATA lv_part  TYPE versobjnam.
    DATA lv_ptype TYPE versobjtyp.

    request->get_uri_query_parameter( EXPORTING name      = 'part'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_part ).
    request->get_uri_query_parameter( EXPORTING name      = 'ptype'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_ptype ).
    TRANSLATE lv_part TO UPPER CASE.
    TRANSLATE lv_ptype TO UPPER CASE.
    IF lv_part IS NOT INITIAL AND lv_ptype IS INITIAL.
      bad_request( |Reading the blocks of { lv_part } needs its type in ptype.| ).
    ENDIF.

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

    IF lv_part IS NOT INITIAL.
      response->set_body_data(
        content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
        data            = part_body( is_payload = ls_payload
                                     i_trkorr   = lv_trkorr
                                     i_part     = lv_part
                                     i_ptype    = lv_ptype
                                     i_table    = lv_table
                                     i_saved    = lv_saved ) ).
      RETURN.
    ENDIF.

    IF lv_saved = abap_true.
      " AVE's own order and AVE's own grouping, read from AVE rather than
      " restated here: the parts of a class under their class, everything else
      " in a section by kind, and an object with no changed line left out. A
      " second opinion about where a method belongs would be a bug on sight,
      " because the report is what a reviewer compares this against.
      DATA(lt_stat) = zcl_ave_acr_report=>report_objects( ls_payload-obj_stats ).

      LOOP AT lt_stat INTO DATA(ls_stat).
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
                        group        = COND string(
                          WHEN ls_stat-class_name IS INITIAL
                          THEN zcl_ave_acr_report=>cat_label( ls_stat-objtype ) )
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


  METHOD post.
    " The first thing in VERTEX that changes state on the server. It changes it
    " the way AVE changes it: load the payload, hand it to AVE's own state, let
    " AVE apply the action and build the next payload, save. Nothing here knows
    " what approving means.
    DATA lv_trkorr TYPE trkorr.
    DATA lv_remote TYPE verssysnam.
    DATA lv_part   TYPE versobjnam.
    DATA lv_ptype  TYPE versobjtyp.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_trkorr ).
    TRANSLATE lv_trkorr TO UPPER CASE.
    request->get_uri_query_parameter( EXPORTING name      = 'remote'
                                                mandatory = abap_false
                                      IMPORTING value     = lv_remote ).
    " The part is not needed to write - the block key names it - but the answer
    " is that part as it now stands, so that the page renders one shape whether
    " it asked or wrote.
    request->get_uri_query_parameter( EXPORTING name      = 'part'
                                                mandatory = abap_true
                                      IMPORTING value     = lv_part ).
    request->get_uri_query_parameter( EXPORTING name      = 'ptype'
                                                mandatory = abap_true
                                      IMPORTING value     = lv_ptype ).
    TRANSLATE lv_remote TO UPPER CASE.
    TRANSLATE lv_part TO UPPER CASE.
    TRANSLATE lv_ptype TO UPPER CASE.

    DATA lv_body TYPE string.
    request->get_body_data(
      EXPORTING content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      IMPORTING data            = lv_body ).

    DATA ls_cmd TYPE ty_command.
    /ui2/cl_json=>deserialize( EXPORTING json        = lv_body
                                         pretty_name = /ui2/cl_json=>pretty_mode-low_case
                               CHANGING  data        = ls_cmd ).

    IF ls_cmd-hunk_key IS INITIAL.
      bad_request( |A reviewer action has to name the block it is about.| ).
    ENDIF.
    TRANSLATE ls_cmd-action TO UPPER CASE.
    IF ls_cmd-action <> `A` AND ls_cmd-action <> `D`
       AND ls_cmd-action <> `C` AND ls_cmd-action <> `U`.
      bad_request( |"{ ls_cmd-action }" is not a reviewer action. A approves,|
                && | D declines, C comments, U takes a verdict back.| ).
    ENDIF.
    IF ( ls_cmd-action = `D` OR ls_cmd-action = `C` ) AND ls_cmd-note IS INITIAL.
      bad_request( |A decline and a comment are the words that go with them.| ).
    ENDIF.

    IF zcl_ave_acr_repository=>has_review_table( ) = abap_false.
      bad_request( |This system has no ZAVE_REVIEW table, so a review has|
                && | nowhere to be written. AVE's documentation says how to create it.| ).
    ENDIF.

    DATA ls_payload TYPE zif_ave_acr_types=>ty_saved_payload.
    IF zcl_ave_acr_repository=>load_review_payload(
         EXPORTING iv_trkorr  = lv_trkorr
                   iv_remote  = lv_remote
         CHANGING  cs_payload = ls_payload ) = abap_false.
      bad_request( |No review is saved for { lv_trkorr }. AVE prepares one;|
                && | this writes into it.| ).
    ENDIF.

    " A review is written by more than one person, and a save writes the whole
    " payload. Writing onto a state that moved since the page read it would
    " carry the other person's approvals away, so the page sends back the stamp
    " it read and a changed one is refused. Loudly: there is no merge here, and
    " pretending there is would be how a review quietly loses work.
    IF ls_cmd-saved_at IS NOT INITIAL
       AND ls_cmd-saved_at <> |{ ls_payload-last_saved_at }|.
      bad_request( |{ ls_payload-last_saved_by } saved this review while the page|
                && | was open. Read it again, then write.| ).
    ENDIF.

    DATA lt_obj_stats  TYPE zif_ave_acr_types=>ty_t_obj_stats.
    DATA lt_hunk_info  TYPE zif_ave_acr_types=>ty_t_hunk_info.
    DATA lt_diff_cache TYPE zif_ave_acr_types=>ty_t_diff_cache.
    DATA lt_diff_data  TYPE zif_ave_acr_types=>ty_t_diff_data.
    DATA lt_approved   TYPE zif_ave_acr_types=>ty_approved.
    DATA lt_declined   TYPE zif_ave_acr_types=>ty_approved.
    DATA lt_notes      TYPE zif_ave_acr_types=>ty_t_decline_notes.
    DATA lt_threads    TYPE zif_ave_acr_types=>ty_t_hunk_threads.
    DATA lt_actions    TYPE zif_ave_acr_types=>ty_t_hunk_actions.
    DATA lt_timings    TYPE zif_ave_acr_types=>ty_t_part_timings.

    zcl_ave_acr_state=>apply_saved_payload(
      EXPORTING
        is_payload          = ls_payload
        " AVE drops generated Gateway classes here when its own setting says to.
        " That setting is AVE's, and this is not the place to act on it: a write
        " from VERTEX must add one verdict and take nothing away.
        iv_ignore_generated = abap_false
      CHANGING
        ct_obj_stats        = lt_obj_stats
        ct_hunk_info        = lt_hunk_info
        ct_diff_cache       = lt_diff_cache
        ct_diff_data        = lt_diff_data
        ct_approved         = lt_approved
        ct_declined         = lt_declined
        ct_decline_notes    = lt_notes
        ct_hunk_threads     = lt_threads
        ct_hunk_actions     = lt_actions
        ct_timings          = lt_timings ).

    READ TABLE lt_hunk_info INTO DATA(ls_hunk)
      WITH TABLE KEY hunk_key = ls_cmd-hunk_key.
    IF sy-subrc <> 0.
      bad_request( |{ ls_cmd-hunk_key } is not a block of this review.| ).
    ENDIF.

    IF c_allow_self_review = abap_false
       AND zcl_ave_acr_state=>is_own_hunk( iv_hunk_key  = ls_cmd-hunk_key
                                           it_hunk_info = lt_hunk_info ) = abap_true.
      bad_request( |A block is reviewed by somebody other than whoever wrote it.| ).
    ENDIF.

    zcl_ave_acr_state=>apply_reviewer_action(
      EXPORTING
        iv_hunk_key      = ls_cmd-hunk_key
        iv_action        = CONV #( ls_cmd-action )
        is_hunk          = ls_hunk
        iv_note          = ls_cmd-note
      CHANGING
        ct_approved      = lt_approved
        ct_declined      = lt_declined
        ct_decline_notes = lt_notes
        ct_hunk_actions  = lt_actions
        ct_hunk_threads  = lt_threads ).

    DATA(ls_next) = zcl_ave_acr_state=>build_save_payload(
      is_existing_payload = ls_payload
      iv_trkorr           = lv_trkorr
      it_obj_stats        = lt_obj_stats
      it_hunk_info        = lt_hunk_info
      it_diff_cache       = lt_diff_cache
      it_diff_data        = lt_diff_data
      it_hunk_actions     = lt_actions
      it_approved         = lt_approved
      it_declined         = lt_declined
      it_decline_notes    = lt_notes
      it_hunk_threads     = lt_threads
      it_timings          = lt_timings ).

    IF zcl_ave_acr_repository=>save_review_payload(
         iv_trkorr  = lv_trkorr
         iv_remote  = lv_remote
         is_payload = ls_next ) = abap_false.
      " The one reason AVE knows of is a ZAVE_REVIEW without the REMOTE key
      " field, which only a review compared against another system runs into.
      bad_request( |ZAVE_REVIEW would not take the write|
                && COND string( WHEN lv_remote IS NOT INITIAL
                                THEN | (REMOTE key field missing?)| ELSE `` )
                && |. Nothing was changed.| ).
    ENDIF.

    response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      data            = part_body( is_payload = ls_next
                                   i_trkorr   = lv_trkorr
                                   i_part     = lv_part
                                   i_ptype    = lv_ptype
                                   i_table    = abap_true
                                   i_saved    = abap_true ) ).
  ENDMETHOD.


  METHOD part_body.
    DATA lt_block TYPE tt_block.
    DATA lt_op    TYPE tt_op.
    DATA lv_old   TYPE versno.
    DATA lv_new   TYPE versno.
    DATA lv_added TYPE i.
    DATA lv_dele  TYPE i.
    DATA lv_ddic  TYPE abap_bool.

    IF i_saved = abap_true.
      LOOP AT is_payload-hunks INTO DATA(ls_hunk).
        IF ls_hunk-objtype <> i_ptype OR ls_hunk-obj_name <> i_part.
          CONTINUE.
        ENDIF.
        " Every block of a part was cut from the same comparison, so the pair
        " belongs to the part and not to the block.
        lv_old = ls_hunk-versno_old.
        lv_new = ls_hunk-versno_new.
        APPEND VALUE #( hunk_key     = ls_hunk-hunk_key
                        hunk_no      = ls_hunk-hunk_no
                        start_line   = ls_hunk-start_line
                        change_count = ls_hunk-change_count
                        change_kind  = ls_hunk-change_kind
                        author       = ls_hunk-author
                        author_name  = ls_hunk-author_name ) TO lt_block.
      ENDLOOP.
      " HUNKS is hashed, and the numbering is what orders the blocks.
      SORT lt_block BY hunk_no.

      " A verdict belongs to a block and to whoever gave it. The note that
      " explains a decline is filed under that same person, so the two are read
      " together.
      LOOP AT lt_block ASSIGNING FIELD-SYMBOL(<block>).
        LOOP AT is_payload-hunk_actions INTO DATA(ls_action)
          WHERE hunk_key = <block>-hunk_key.
          <block>-action        = ls_action-action.
          <block>-reviewer      = ls_action-reviewer.
          <block>-reviewer_name = ls_action-reviewer_name.
        ENDLOOP.
        LOOP AT is_payload-user_states INTO DATA(ls_user).
          LOOP AT ls_user-notes INTO DATA(ls_note)
            WHERE hunk_key = <block>-hunk_key.
            <block>-note = ls_note-note.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

      " The stored diff carries the pair it was taken from, so the one these
      " blocks were cut from is the one taken from the same pair. A part can
      " have more than one row: the comparison against the remote system is
      " another, and it is not this diff.
      DATA ls_pick  TYPE zif_ave_acr_types=>ty_diff_data.
      DATA lv_found TYPE abap_bool.
      LOOP AT is_payload-diff_data INTO DATA(ls_diff).
        IF ls_diff-key-objtype <> i_ptype OR ls_diff-key-objname <> i_part
           OR ls_diff-retrofit = abap_true.
          CONTINUE.
        ENDIF.
        IF lv_found = abap_false.
          ls_pick  = ls_diff.
          lv_found = abap_true.
        ENDIF.
        IF ls_diff-key-versno_o = lv_old AND ls_diff-key-versno_n = lv_new.
          ls_pick = ls_diff.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_found = abap_true.
        " A part with no block left after the rendering filter still has a
        " diff, and the pair it names is the only one there is.
        IF lt_block IS INITIAL.
          lv_old = ls_pick-key-versno_o.
          lv_new = ls_pick-key-versno_n.
        ENDIF.
        LOOP AT ls_pick-diff INTO DATA(ls_line).
          APPEND VALUE #( op = ls_line-op text = ls_line-text ) TO lt_op.
          CASE ls_line-op.
            WHEN '+'. lv_added = lv_added + 1.
            WHEN '-'. lv_dele  = lv_dele + 1.
          ENDCASE.
        ENDLOOP.
        " A DDIC object has no line diff to slice. Its review page is a table
        " of fields, kept as ready-made html because there is nothing left to
        " rebuild it from, and VERTEX does not render that html. Said here so
        " the page can say it rather than show an empty diff.
        lv_ddic = boolc( lt_op IS INITIAL AND ls_pick-html IS NOT INITIAL ).

        " The operations travel out in the order they are stored, so an index
        " into the stored diff is an index into OPS.
        locate_blocks( EXPORTING it_diff  = ls_pick-diff
                       CHANGING  ct_block = lt_block ).
      ENDIF.
    ENDIF.

    rv_json =
      |\{"request":"{ to_lower( i_trkorr ) }",| &&
      |"part":"{ to_lower( i_part ) }",| &&
      |"part_type":"{ to_lower( i_ptype ) }",| &&
      |"table":{ COND string( WHEN i_table = abap_true THEN `true` ELSE `false` ) },| &&
      |"saved":{ COND string( WHEN i_saved = abap_true THEN `true` ELSE `false` ) },| &&
      |"ddic":{ COND string( WHEN lv_ddic = abap_true THEN `true` ELSE `false` ) },| &&
      " The stamp the page writes back with, so a write onto a review that has
      " moved underneath it is refused instead of overwriting the move.
      |"saved_at":"{ is_payload-last_saved_at }",| &&
      |"versno_old":"{ COND string( WHEN lv_old IS INITIAL OR lv_old = '00000'
                                   THEN `` ELSE |{ lv_old }| ) }",| &&
      |"versno_new":"{ COND string( WHEN lv_new IS INITIAL THEN `` ELSE |{ lv_new }| ) }",| &&
      |"added":{ lv_added },"deleted":{ lv_dele },| &&
      |"blocks":{ /ui2/cl_json=>serialize(
                    data        = lt_block
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },| &&
      |"ops":{ /ui2/cl_json=>serialize(
                 data        = lt_op
                 pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.
  ENDMETHOD.


  METHOD locate_blocks.
    " Where each block sits in the operations - asked of AVE, not worked out
    " here. AVE cuts its blocks while it walks the diff, and the rule is not one
    " a reader of the result can reproduce: a block swallows the context inside
    " an unfinished statement, and keeps a blank line when more changes follow.
    " ZCL_AVE_ACR_HUNK_HTML=>HUNK_RANGES is that walk, and the html of a block
    " is rendered from what it returns - so these are the very operations the
    " saved review was cut from.
    "
    " A block is recognised by the line it opens on. Blocks open on strictly
    " increasing lines, because whatever ends one block is a line of the new
    " version, and START_LINE is what the payload keeps.
    DATA(lt_range) = zcl_ave_acr_hunk_html=>hunk_ranges( it_diff ).

    LOOP AT ct_block ASSIGNING FIELD-SYMBOL(<block>).
      READ TABLE lt_range INTO DATA(ls_range)
        WITH KEY start_line = <block>-start_line.
      IF sy-subrc <> 0.
        " The saved blocks and the saved diff disagree. Left at zero, which the
        " page shows rather than hides.
        CONTINUE.
      ENDIF.
      <block>-op_from = ls_range-op_from.
      <block>-op_to   = ls_range-op_to.
    ENDLOOP.
  ENDMETHOD.


  METHOD bad_request.
    RAISE EXCEPTION TYPE cx_adt_res_bad_request
      EXPORTING explanation = i_text.
  ENDMETHOD.

ENDCLASS.
