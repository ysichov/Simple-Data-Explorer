REPORT z_itable_explorer.

START-OF-SELECTION.
  PERFORM test.

FORM test.

  TYPES: BEGIN OF t_header.
           INCLUDE STRUCTURE scarr.
  TYPES:   sflight_tab TYPE flighttab,
         END OF t_header.

  DATA: lt_spfli  TYPE TABLE OF t_header,
        lt_flight TYPE TABLE OF sflight.
        "ls_header type t_header.

  "SELECT * INTO TABLE lt_flight   FROM sflight.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_spfli   FROM spfli.

  LOOP AT lt_spfli ASSIGNING FIELD-SYMBOL(<spfli>).
**    SELECT * INTO TABLE ls_header-sflight_tab
**      FROM sflight
**     WHERE  carrid = <spfli>-carrid.
      SELECT  * INTO TABLE <spfli>-sflight_tab
      FROM sflight
     WHERE  carrid = <spfli>-carrid.
IF sy-subrc = 0.
  exit.
  endif.
  ENDLOOP.

"UNASSIGN <spfli>.
"ASSIGN  ls_header TO FIELD-SYMBOL(<spfli>).
  zcl_data_explorer=>open_table( EXPORTING it_tab = lt_flight iv_name = 'SFLIGHT'  ).
  zcl_data_explorer=>open_table( EXPORTING it_tab = lt_spfli iv_name = 'SPFLI header with Sflight body tables' iv_show = 'X' ).

ENDFORM.
