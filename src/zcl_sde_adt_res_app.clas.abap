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
    router->attach( iv_template      = '/zsde/table/{name}'
                    iv_handler_class = 'ZCL_SDE_ADT_RES_TABLE' ).
    " Every VERTEX service registers here rather than under a prefix of its
    " own: the BAdI filter already claims /sap/bc/adt/zsde/*, and a second
    " prefix would mean a second implementation and a second filter to get
    " wrong.
    router->attach( iv_template      = '/zsde/metrics/{name}'
                    iv_handler_class = 'ZCL_SDE_ADT_RES_METRICS' ).
    router->attach( iv_template      = '/zsde/versions/{name}'
                    iv_handler_class = 'ZCL_SDE_ADT_RES_VERSIONS' ).
    router->attach( iv_template      = '/zsde/join/{name}'
                    iv_handler_class = 'ZCL_SDE_ADT_RES_JOIN' ).
    router->attach( iv_template      = '/zsde/review/{name}'
                    iv_handler_class = 'ZCL_SDE_ADT_RES_REVIEW' ).
  ENDMETHOD.
ENDCLASS.
