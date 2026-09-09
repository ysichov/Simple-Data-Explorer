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

    " As many selection lines as the table resource reads, for the same reason:
    " a limit that is never met in practice still has to exist.
    CONSTANTS c_max_filters TYPE i VALUE 20.

    CONSTANTS c_options TYPE string VALUE `EQ NE GT GE LT LE CP NP BT NB`.

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

    "! The pivot cross, as indexed parameters: r1..rN are the row dimensions,
    "! c1..cN the columns and v1..vN the measures, each key an ALIAS~FIELD of
    "! the join. a1..aN carry the aggregate of the matching measure; left out,
    "! the pivot settles one the field's type can carry.
    METHODS read_pivot
      IMPORTING io_request TYPE REF TO if_adt_rest_request
      EXPORTING et_rows    TYPE zcl_sde_pivot=>tt_keys
                et_cols    TYPE zcl_sde_pivot=>tt_keys
                et_vals    TYPE zcl_sde_pivot=>tt_vals.

    "! Reads the indexed selection parameters f1/s1/o1/l1/h1, f2/... - the same
    "! contract the table resource uses - and checks every label against the
    "! fields the join actually has. BUILD_WHERE drops a label it does not know
    "! without a word, which would answer with a filter nobody applied.
    METHODS read_filters
      IMPORTING io_request       TYPE REF TO if_adt_rest_request
                it_field         TYPE zcl_sde_tools=>tt_jfld
      RETURNING VALUE(rt_filter) TYPE zcl_sde_tools=>tt_filter
      RAISING   cx_adt_rest.

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
    DATA: lv_name  TYPE tabname,
          lv_take  TYPE tabname,
          lv_rows  TYPE i,
          lr_rows  TYPE REF TO data,
          lv_error TYPE string,
          lt_cand  TYPE tt_candidate,
          lt_tab   TYPE tt_table,
          lt_fld   TYPE tt_field.
    FIELD-SYMBOLS <lt_rows> TYPE STANDARD TABLE.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).
    TRANSLATE lv_name TO UPPER CASE.

    " Nought asks for the statement without running it, which is what the page
    " wants while the join is still being assembled.
    request->get_uri_query_parameter( EXPORTING name      = 'rows'
                                                mandatory = abap_false
                                                default   = 0
                                      IMPORTING value     = lv_rows ).

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

    " Before the statement is generated: the WHERE is part of it.
    lo_tools->set_filters( read_filters( io_request = request
                                         it_field   = lt_jfld ) ).

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

    DATA(lt_prows) = VALUE zcl_sde_pivot=>tt_keys( ).
    DATA(lt_pcols) = VALUE zcl_sde_pivot=>tt_keys( ).
    DATA(lt_pvals) = VALUE zcl_sde_pivot=>tt_vals( ).
    read_pivot( EXPORTING io_request = request
                IMPORTING et_rows    = lt_prows
                          et_cols    = lt_pcols
                          et_vals    = lt_pvals ).
    DATA(lv_pivot) = xsdbool( lt_prows IS NOT INITIAL
                           OR lt_pcols IS NOT INITIAL
                           OR lt_pvals IS NOT INITIAL ).
    IF lv_pivot = abap_true.
      lo_tools->set_pivot( it_rows = lt_prows it_cols = lt_pcols it_vals = lt_pvals ).
    ENDIF.

    DATA(lv_rows_json) = `null`.
    IF lv_rows > 0.
      IF lv_pivot = abap_true.
        " The matrix is spread in ABAP: a dynamically specified SELECT list
        " cannot carry the CASE expressions a SQL-side one would need.
        lo_tools->run_pivot( EXPORTING i_rows    = lv_rows
                             IMPORTING er_result = lr_rows
                                       ev_error  = lv_error ).
      ELSE.
        lo_tools->run( EXPORTING i_rows    = lv_rows
                       IMPORTING er_result = lr_rows
                                 ev_error  = lv_error ).
      ENDIF.
      IF lv_error IS NOT INITIAL.
        " The statement is generated, not typed, so a failure here is ours and
        " not the caller's mistake to guess at.
        bad_request( |The statement did not run: { lv_error }| ).
      ENDIF.
      IF lr_rows IS BOUND.
        ASSIGN lr_rows->* TO <lt_rows>.
        lv_rows_json = /ui2/cl_json=>serialize(
                         data        = <lt_rows>
                         pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      ENDIF.
    ENDIF.

    DATA(lv_body) =
      |\{"table":"{ to_lower( lv_name ) }",| &&
      |"sql":{ /ui2/cl_json=>serialize( data = lo_tools->sql( lv_rows ) ) },| &&
      |"pivot":{ COND string( WHEN lv_pivot = abap_true THEN `true` ELSE `false` ) },| &&
      |"rows":{ lv_rows_json },| &&
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


  METHOD read_pivot.
    DATA: lv_key TYPE string,
          lv_agg TYPE string.

    DO c_max_filters TIMES.
      DATA(lv_i) = |{ sy-index }|.

      CLEAR lv_key.
      io_request->get_uri_query_parameter( EXPORTING name  = |r{ lv_i }|
                                           IMPORTING value = lv_key ).
      IF lv_key IS NOT INITIAL.
        APPEND to_lower( lv_key ) TO et_rows.
      ENDIF.

      CLEAR lv_key.
      io_request->get_uri_query_parameter( EXPORTING name  = |c{ lv_i }|
                                           IMPORTING value = lv_key ).
      IF lv_key IS NOT INITIAL.
        APPEND to_lower( lv_key ) TO et_cols.
      ENDIF.

      CLEAR: lv_key, lv_agg.
      io_request->get_uri_query_parameter( EXPORTING name  = |v{ lv_i }|
                                           IMPORTING value = lv_key ).
      IF lv_key IS NOT INITIAL.
        io_request->get_uri_query_parameter( EXPORTING name  = |a{ lv_i }|
                                             IMPORTING value = lv_agg ).
        APPEND VALUE #( key = to_lower( lv_key )
                        agg = to_upper( lv_agg ) ) TO et_vals.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD read_filters.
    DATA: lv_label  TYPE lvc_fname,
          lv_sign   TYPE ddsign,
          lv_option TYPE ddoption,
          lv_low    TYPE string,
          lv_high   TYPE string,
          lv_alias  TYPE char5,
          lv_field  TYPE fieldname.

    DO c_max_filters TIMES.
      DATA(lv_i) = |{ sy-index }|.
      CLEAR: lv_label, lv_sign, lv_option, lv_low, lv_high.

      io_request->get_uri_query_parameter( EXPORTING name  = |f{ lv_i }|
                                           IMPORTING value = lv_label ).
      IF lv_label IS INITIAL.
        EXIT.
      ENDIF.
      TRANSLATE lv_label TO UPPER CASE.

      io_request->get_uri_query_parameter( EXPORTING name    = |s{ lv_i }|
                                                     default = 'I'
                                           IMPORTING value   = lv_sign ).
      io_request->get_uri_query_parameter( EXPORTING name    = |o{ lv_i }|
                                                     default = 'EQ'
                                           IMPORTING value   = lv_option ).
      io_request->get_uri_query_parameter( EXPORTING name  = |l{ lv_i }|
                                           IMPORTING value = lv_low ).
      io_request->get_uri_query_parameter( EXPORTING name  = |h{ lv_i }|
                                           IMPORTING value = lv_high ).

      " A label is the panel's: the plain field name for the base table,
      " T1_FIELD for a joined one.
      DATA(lv_text) = condense( CONV string( lv_label ) ).
      lv_alias = 'T0'.
      lv_field = lv_text.
      FIND REGEX '^T\d+_' IN lv_text MATCH LENGTH DATA(lv_len).
      IF sy-subrc = 0.
        lv_alias = substring( val = lv_text len = lv_len - 1 ). "without the '_'
        lv_field = substring( val = lv_text off = lv_len ).
      ENDIF.

      IF NOT line_exists( it_field[ alias = lv_alias fieldname = lv_field ] ).
        bad_request( |{ lv_label } is not a field of this join. Filter on the| &&
                     | names the statement gives its columns - MATNR for the| &&
                     | base table, T1_MATNR for a joined one.| ).
      ENDIF.
      IF lv_sign <> 'I' AND lv_sign <> 'E'.
        bad_request( |Sign { lv_sign } for { lv_label } must be I or E.| ).
      ENDIF.
      IF NOT contains( val = c_options sub = CONV string( lv_option ) ).
        bad_request( |Option { lv_option } for { lv_label } is not one of { c_options }.| ).
      ENDIF.
      IF ( lv_option = 'BT' OR lv_option = 'NB' ) AND lv_high IS INITIAL.
        bad_request( |Option { lv_option } for { lv_label } needs an upper bound in h{ lv_i }.| ).
      ENDIF.

      " Lines for one field belong to one panel row, which is what makes them
      " a select-option rather than a chain of conditions.
      READ TABLE rt_filter ASSIGNING FIELD-SYMBOL(<filter>) WITH KEY label = lv_label.
      IF sy-subrc <> 0.
        APPEND VALUE #( label = lv_label ) TO rt_filter ASSIGNING <filter>.
      ENDIF.
      " The free-selection range calls it OPTI, not OPTION - the same name
      " ZCL_SDE_SEL_OPT fills when the panel builds a line.
      APPEND VALUE #( sign = lv_sign
                      opti = lv_option
                      low  = lv_low
                      high = lv_high ) TO <filter>-range.
    ENDDO.
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
