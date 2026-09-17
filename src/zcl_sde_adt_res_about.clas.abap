CLASS zcl_sde_adt_res_about DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " One service of the hub: the route it answers on, the class that answers
    " it, and the tool that class reads through, when it needs one.
    TYPES: BEGIN OF ty_service,
             name     TYPE string,
             template TYPE string,
             handler  TYPE seoclsname,
             backend  TYPE string,
           END OF ty_service,
           tt_service TYPE STANDARD TABLE OF ty_service WITH EMPTY KEY.

    "! Every route of the hub, this one included. ZCL_SDE_ADT_RES_APP attaches
    "! exactly these, so what this resource reports and what the router serves
    "! are one list and cannot drift apart.
    CLASS-METHODS services
      RETURNING VALUE(rt_service) TYPE tt_service.

    METHODS get REDEFINITION.

  PRIVATE SECTION.
    " What a window is told about one service. ACTIVE is whether the class that
    " answers it has an active version: a class whose tool is not installed
    " never gets one, and its route then fails however it is asked.
    TYPES: BEGIN OF ty_state,
             name    TYPE string,
             handler TYPE string,
             backend TYPE string,
             active  TYPE abap_bool,
           END OF ty_state,
           tt_state TYPE STANDARD TABLE OF ty_state WITH EMPTY KEY.

    TYPES: BEGIN OF ty_backend,
             name      TYPE string,
             installed TYPE abap_bool,
           END OF ty_backend,
           tt_backend TYPE STANDARD TABLE OF ty_backend WITH EMPTY KEY.

    "! Whether a class has an active version. Read from the program directory
    "! rather than by loading the class: a class whose tool has since gone
    "! would stop this resource with a syntax error instead of being reported.
    CLASS-METHODS is_active
      IMPORTING i_class       TYPE seoclsname
      RETURNING VALUE(rv_yes) TYPE abap_bool.
ENDCLASS.


CLASS zcl_sde_adt_res_about IMPLEMENTATION.

  METHOD services.
    " In the order the routes have always been attached in.
    rt_service = VALUE #(
      ( name = `table`    template = `/zsde/table/{name}`    handler = 'ZCL_SDE_ADT_RES_TABLE' )
      ( name = `metrics`  template = `/zsde/metrics/{name}`  handler = 'ZCL_SDE_ADT_RES_METRICS'  backend = `ACE` )
      ( name = `flow`     template = `/zsde/flow/{name}`     handler = 'ZCL_SDE_ADT_RES_FLOW'     backend = `ACE` )
      ( name = `versions` template = `/zsde/versions/{name}` handler = 'ZCL_SDE_ADT_RES_VERSIONS' backend = `AVE` )
      ( name = `join`     template = `/zsde/join/{name}`     handler = 'ZCL_SDE_ADT_RES_JOIN' )
      ( name = `review`   template = `/zsde/review/{name}`   handler = 'ZCL_SDE_ADT_RES_REVIEW'   backend = `AVE` )
      " The routes with no name in them: what they are asked comes as query
      " parameters, and all of those are optional.
      ( name = `requests` template = `/zsde/requests`        handler = 'ZCL_SDE_ADT_RES_REQUESTS' backend = `AVE` )
      ( name = `about`    template = `/zsde/about`           handler = 'ZCL_SDE_ADT_RES_ABOUT' ) ).
  ENDMETHOD.


  METHOD get.
    DATA: lt_state   TYPE tt_state,
          lt_backend TYPE tt_backend,
          lv_body    TYPE string.

    " A window asks this once when it opens, and leaves out what the system
    " does not have instead of offering a button that can only fail.
    DATA(lt_service) = services( ).
    LOOP AT lt_service INTO DATA(ls_service).
      APPEND VALUE #( name    = ls_service-name
                      handler = CONV string( ls_service-handler )
                      backend = ls_service-backend
                      active  = is_active( ls_service-handler ) ) TO lt_state.
    ENDLOOP.

    " A tool is there when the class its resources start from is active.
    lt_backend = VALUE #( ( name = `AVE` installed = is_active( 'ZCL_AVE_OBJECT_FACTORY' ) )
                          ( name = `ACE` installed = is_active( 'ZCL_ACE_METRICS' ) ) ).

    " Who is logged on, too: the finder's user field starts from it, and it is
    " the server that knows who that is, not the page.
    lv_body = |\{"user":"{ escape( val    = CONV string( sy-uname )
                                  format = cl_abap_format=>e_json_string ) }",| &&
              |"services":{ /ui2/cl_json=>serialize(
                                data        = lt_state
                                pretty_name = /ui2/cl_json=>pretty_mode-low_case ) },| &&
              |"backends":{ /ui2/cl_json=>serialize(
                                data        = lt_backend
                                pretty_name = /ui2/cl_json=>pretty_mode-low_case ) }\}|.

    response->set_body_data(
      content_handler = NEW cl_adt_rest_plain_text_handler( content_type = if_rest_media_type=>gc_appl_json )
      data            = lv_body ).
  ENDMETHOD.


  METHOD is_active.
    DATA(lv_pool) = cl_oo_classname_service=>get_classpool_name( i_class ).
    SELECT SINGLE @abap_true FROM progdir
      WHERE name  = @lv_pool
        AND state = 'A'
      INTO @rv_yes.
  ENDMETHOD.

ENDCLASS.
