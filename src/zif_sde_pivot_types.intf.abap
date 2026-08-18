INTERFACE zif_sde_pivot_types
  PUBLIC.

  "Shared type between zcl_sde_tools (join field list) and zcl_sde_pivot -
  "each class needs the other's type, so a direct cross-reference would
  "create a circular type dependency that cannot be resolved when both
  "classes are flattened into one standalone program (see generate_standalone.sh).

  TYPES: BEGIN OF t_jfld,
           sel       TYPE abap_bool,
           pos       TYPE i, "order in the SELECT list (only when sel = X)
           alias     TYPE char5,
           tabname   TYPE tabname,
           fieldname TYPE fieldname,
           keyflag   TYPE keyflag,
           ddtext    TYPE reptext,
           datatype  TYPE c LENGTH 30,
           inttype   TYPE c LENGTH 1,
           domname   TYPE domname, "fixed values behind a pivot column header, see value_label
         END OF t_jfld,
         tt_jfld TYPE STANDARD TABLE OF t_jfld WITH DEFAULT KEY.

ENDINTERFACE.
