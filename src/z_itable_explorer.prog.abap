REPORT z_itable_explorer.
INCLUDE z_itable_explorer_cls.

START-OF-SELECTION.
  PERFORM test.


FORM test.

  TYPES: BEGIN OF t_header.
           INCLUDE STRUCTURE spfli.
           TYPES: sflight_tab TYPE flighttab,
         END OF t_header.

  DATA: lt_spfli  TYPE TABLE OF t_header,
        lt_flight TYPE TABLE OF sflight.

  SELECT * INTO TABLE lt_flight   FROM sflight.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_spfli   FROM spfli.

  LOOP AT lt_spfli ASSIGNING FIELD-SYMBOL(<spfli>).
    SELECT * INTO TABLE <spfli>-sflight_tab
      FROM sflight
     WHERE  carrid = <spfli>-carrid.
  ENDLOOP.

  lcl_appl=>open_int_table( EXPORTING it_tab = lt_flight iv_name = 'SFLIGHT' iv_dummy = 'X'  ).
  lcl_appl=>open_int_table( EXPORTING it_tab = lt_spfli iv_name = 'SPFLI header with Sflight body tables' iv_dummy = 'X' iv_show = 'X'   ).

ENDFORM.
