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
  ENDMETHOD.
ENDCLASS.
