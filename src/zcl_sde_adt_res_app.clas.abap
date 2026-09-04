CLASS zcl_sde_adt_res_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_app_base
  FINAL
  CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS fill_router REDEFINITION.
ENDCLASS.

CLASS zcl_sde_adt_res_app IMPLEMENTATION.
  METHOD fill_router.
    router->attach( iv_template      = '/zsde/table/{name}'
                    iv_handler_class = 'ZCL_SDE_ADT_RES_TABLE' ).
  ENDMETHOD.
ENDCLASS.
