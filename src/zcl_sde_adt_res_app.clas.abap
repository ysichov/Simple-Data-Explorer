class ZCL_SDE_ADT_RES_APP definition
  public
  inheriting from CL_ADT_RES_APP_BASE
  final
  create public .

public section.
  PROTECTED SECTION.
    METHODS fill_router REDEFINITION.
private section.
ENDCLASS.



CLASS ZCL_SDE_ADT_RES_APP IMPLEMENTATION.


  METHOD fill_router.
    " Every VERTEX service registers here rather than under a prefix of its
    " own: the BAdI filter already claims /sap/bc/adt/zsde/*, and a second
    " prefix would mean a second implementation and a second filter to get
    " wrong.
    " The routes are the list ZCL_SDE_ADT_RES_ABOUT reports, so what a window
    " is told this system has and what the router serves cannot drift apart.
    DATA(lt_service) = zcl_sde_adt_res_about=>services( ).
    LOOP AT lt_service INTO DATA(ls_service).
      router->attach( iv_template      = ls_service-template
                      iv_handler_class = ls_service-handler ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
