CLASS zcl_sde_adt_res_table DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_max_filters TYPE i VALUE 20.

    TYPES: BEGIN OF ty_field,
             name     TYPE string,
             position TYPE i,
             key      TYPE abap_bool,
             datatype TYPE string,
             length   TYPE i,
             decimals TYPE i,
             text     TYPE string,
           END OF ty_field,
           tt_field  TYPE STANDARD TABLE OF ty_field WITH EMPTY KEY,

           BEGIN OF ty_filter,
             field  TYPE fieldname,
             sign   TYPE ddsign,
             option TYPE ddoption,
             low    TYPE string,
             high   TYPE string,
           END OF ty_filter,
           tt_filter TYPE STANDARD TABLE OF ty_filter WITH EMPTY KEY.

    METHODS field_catalog
      IMPORTING it_ddic         TYPE ddfields
      RETURNING VALUE(rt_field) TYPE tt_field.

    "! Reads the indexed selection parameters f1/s1/o1/l1/h1, f2/... and checks
    "! every part against the table, so a typo answers 400 instead of silently
    "! selecting nothing.
    METHODS read_filters
      IMPORTING io_request       TYPE REF TO if_adt_rest_request
                it_ddic          TYPE ddfields
                i_tabname        TYPE tabname
      RETURNING VALUE(rt_filter) TYPE tt_filter
      RAISING   cx_adt_rest.

    "! Select-option semantics: lines for one field are ORed, excluding lines
    "! become AND NOT, and different fields are ANDed.
    METHODS where_clause
      IMPORTING it_filter       TYPE tt_filter
      RETURNING VALUE(rv_where) TYPE string.

    CLASS-METHODS term
      IMPORTING is_filter      TYPE ty_filter
      RETURNING VALUE(rv_term) TYPE string.

    CLASS-METHODS quote
      IMPORTING i_value           TYPE string
      RETURNING VALUE(rv_literal) TYPE string.

    CLASS-METHODS pattern
      IMPORTING i_value           TYPE string
      RETURNING VALUE(rv_pattern) TYPE string.

    METHODS not_found
      IMPORTING i_type TYPE string
                i_id   TYPE string
      RAISING   cx_adt_res_not_found.

    METHODS bad_request
      IMPORTING i_text TYPE string
      RAISING   cx_adt_res_bad_request.
ENDCLASS.

CLASS zcl_sde_adt_res_table IMPLEMENTATION.

  METHOD get.
    DATA: lv_name  TYPE tabname,
          lv_rows  TYPE i,
          lr_tab   TYPE REF TO data,
          lv_count TYPE i,
          lo_type  TYPE REF TO cl_abap_typedescr,
          lt_ddic  TYPE ddfields.
    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.

    request->get_uri_attribute( EXPORTING name      = 'name'
                                          mandatory = abap_true
                                IMPORTING value     = lv_name ).
    TRANSLATE lv_name TO UPPER CASE.

    request->get_uri_query_parameter( EXPORTING name      = 'rows'
                                                mandatory = abap_false
                                                default   = 100
                                      IMPORTING value     = lv_rows ).

    " zcl_sde_sql=>read_any_table swallows a missing table and returns nothing,
    " so the existence check has to happen here to produce a real 404.
    IF zcl_sde_sql=>exist_table( lv_name ) <> 1.
      not_found( i_type = `table` i_id = CONV string( lv_name ) ).
    ENDIF.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING  p_name         = lv_name
      RECEIVING  p_descr_ref    = lo_type
      EXCEPTIONS type_not_found = 1
                 OTHERS         = 2 ).
    IF sy-subrc <> 0 OR lo_type->kind <> cl_abap_typedescr=>kind_struct.
      not_found( i_type = `structure` i_id = CONV string( lv_name ) ).
    ENDIF.

    DATA(lo_struct) = CAST cl_abap_structdescr( lo_type ).
    lo_struct->get_ddic_field_list(
      RECEIVING  p_field_list = lt_ddic
      EXCEPTIONS not_found    = 1
                 no_ddic_type = 2
                 OTHERS       = 3 ).
    IF sy-subrc <> 0.
      not_found( i_type = `fieldlist` i_id = CONV string( lv_name ) ).
    ENDIF.

    DATA(lt_filter) = read_filters( io_request = request
                                    it_ddic    = lt_ddic
                                    i_tabname  = lv_name ).
    DATA(lv_where) = where_clause( lt_filter ).

    DATA(lo_tabtype) = cl_abap_tabledescr=>create( p_line_type = lo_struct ).
    CREATE DATA lr_tab TYPE HANDLE lo_tabtype.
    ASSIGN lr_tab->* TO <lt_data>.

    zcl_sde_sql=>read_any_table( EXPORTING i_tabname   = lv_name
                                           i_where     = lv_where
                                           i_row_count = lv_rows
                                 CHANGING  cr_tab      = lr_tab
                                           c_count     = lv_count ).

    DATA(lv_body) =
      |\{"table":"{ to_lower( lv_name ) }",| &&
      |"count":{ lv_count },| &&
      |"fields":{ /ui2/cl_json=>serialize(
                    data        = field_catalog( lt_ddic )
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },| &&
      |"rows":{ /ui2/cl_json=>serialize(
                    data        = <lt_data>
                    pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.

    response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      data            = lv_body ).
  ENDMETHOD.

  METHOD field_catalog.
    LOOP AT it_ddic ASSIGNING FIELD-SYMBOL(<ls_ddic>).
      " Open SQL already restricts the read to the session's client, so the
      " client field holds the same value in every row: noise in the grid and
      " useless as a selection criterion. Recognised by its data type, because
      " it is not always called MANDT.
      IF <ls_ddic>-datatype = 'CLNT'.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( name     = to_lower( <ls_ddic>-fieldname )
                      position = <ls_ddic>-position
                      key      = <ls_ddic>-keyflag
                      datatype = <ls_ddic>-datatype
                      length   = <ls_ddic>-leng
                      decimals = <ls_ddic>-decimals
                      text     = <ls_ddic>-fieldtext ) TO rt_field.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_filters.
    CONSTANTS c_options TYPE string VALUE `EQ NE GT GE LT LE CP NP BT NB`.

    DATA: lv_field  TYPE fieldname,
          lv_sign   TYPE ddsign,
          lv_option TYPE ddoption,
          lv_low    TYPE string,
          lv_high   TYPE string.

    DO c_max_filters TIMES.
      DATA(lv_i) = |{ sy-index }|.
      CLEAR: lv_field, lv_sign, lv_option, lv_low, lv_high.

      io_request->get_uri_query_parameter( EXPORTING name  = |f{ lv_i }|
                                           IMPORTING value = lv_field ).
      IF lv_field IS INITIAL.
        EXIT.
      ENDIF.
      TRANSLATE lv_field TO UPPER CASE.

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
      TRANSLATE lv_sign   TO UPPER CASE.
      TRANSLATE lv_option TO UPPER CASE.

      READ TABLE it_ddic ASSIGNING FIELD-SYMBOL(<ls_ddic>)
           WITH KEY fieldname = lv_field.
      IF sy-subrc <> 0.
        bad_request( |Table { i_tabname } has no field { lv_field }.| ).
      ENDIF.

      " LOB columns cannot appear in a WHERE clause at all.
      IF <ls_ddic>-datatype = 'STRG' OR <ls_ddic>-datatype = 'RSTR'.
        bad_request( |Field { lv_field } is a LOB and cannot be selected on.| ).
      ENDIF.

      IF lv_sign <> 'I' AND lv_sign <> 'E'.
        bad_request( |Sign { lv_sign } for { lv_field } must be I or E.| ).
      ENDIF.

      IF NOT c_options CS lv_option.
        bad_request( |Option { lv_option } for { lv_field } is not one of { c_options }.| ).
      ENDIF.

      IF ( lv_option = 'BT' OR lv_option = 'NB' ) AND lv_high IS INITIAL.
        bad_request( |Option { lv_option } for { lv_field } needs an upper bound in h{ lv_i }.| ).
      ENDIF.

      APPEND VALUE #( field  = lv_field
                      sign   = lv_sign
                      option = lv_option
                      low    = lv_low
                      high   = lv_high ) TO rt_filter.
    ENDDO.
  ENDMETHOD.

  METHOD where_clause.
    DATA lt_field TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line.

    LOOP AT it_filter ASSIGNING FIELD-SYMBOL(<ls_any>).
      INSERT <ls_any>-field INTO TABLE lt_field.
    ENDLOOP.

    LOOP AT lt_field ASSIGNING FIELD-SYMBOL(<lv_field>).
      DATA(lv_include) = ``.
      DATA(lv_exclude) = ``.

      LOOP AT it_filter ASSIGNING FIELD-SYMBOL(<ls_filter>) WHERE field = <lv_field>.
        DATA(lv_term) = term( <ls_filter> ).
        IF <ls_filter>-sign = 'I'.
          lv_include = COND #( WHEN lv_include IS INITIAL THEN lv_term
                               ELSE |{ lv_include } OR { lv_term }| ).
        ELSE.
          lv_exclude = COND #( WHEN lv_exclude IS INITIAL THEN lv_term
                               ELSE |{ lv_exclude } OR { lv_term }| ).
        ENDIF.
      ENDLOOP.

      DATA(lv_group) = ``.
      IF lv_include IS NOT INITIAL.
        lv_group = |( { lv_include } )|.
      ENDIF.
      IF lv_exclude IS NOT INITIAL.
        lv_group = COND #( WHEN lv_group IS INITIAL THEN |NOT ( { lv_exclude } )|
                           ELSE |{ lv_group } AND NOT ( { lv_exclude } )| ).
      ENDIF.

      rv_where = COND #( WHEN rv_where IS INITIAL THEN lv_group
                         ELSE |{ rv_where } AND { lv_group }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD term.
    DATA(lv_low) = quote( is_filter-low ).

    CASE is_filter-option.
      WHEN 'EQ'. rv_term = |{ is_filter-field } = { lv_low }|.
      WHEN 'NE'. rv_term = |{ is_filter-field } <> { lv_low }|.
      WHEN 'GT'. rv_term = |{ is_filter-field } > { lv_low }|.
      WHEN 'GE'. rv_term = |{ is_filter-field } >= { lv_low }|.
      WHEN 'LT'. rv_term = |{ is_filter-field } < { lv_low }|.
      WHEN 'LE'. rv_term = |{ is_filter-field } <= { lv_low }|.
      WHEN 'CP'. rv_term = |{ is_filter-field } LIKE { quote( pattern( is_filter-low ) ) }|.
      WHEN 'NP'. rv_term = |{ is_filter-field } NOT LIKE { quote( pattern( is_filter-low ) ) }|.
      WHEN 'BT'. rv_term = |{ is_filter-field } BETWEEN { lv_low } AND { quote( is_filter-high ) }|.
      WHEN 'NB'. rv_term = |{ is_filter-field } NOT BETWEEN { lv_low } AND { quote( is_filter-high ) }|.
    ENDCASE.
  ENDMETHOD.

  METHOD quote.
    DATA(lv_value) = i_value.
    REPLACE ALL OCCURRENCES OF `'` IN lv_value WITH `''`.
    rv_literal = |'{ lv_value }'|.
  ENDMETHOD.

  METHOD pattern.
    rv_pattern = i_value.
    REPLACE ALL OCCURRENCES OF `*` IN rv_pattern WITH `%`.
    REPLACE ALL OCCURRENCES OF `+` IN rv_pattern WITH `_`.
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
