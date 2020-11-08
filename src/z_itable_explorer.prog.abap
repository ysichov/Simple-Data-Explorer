REPORT z_itable_explorer.
INCLUDE z_itable_explorer_cls.


START-OF-SELECTION.
  PERFORM test.


FORM test.

  types: begin of t_header.
         include STRUCTURE spfli.
  types: sflight_tab type FLIGHTTAB,
         end of t_header.

  DATA: lt_spfli TYPE TABLE OF t_header,
        lt_flight type table of sflight.

  SELECT * INTO TABLE lt_flight   FROM sflight.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_spfli   FROM spfli.

  LOOP AT lt_spfli ASSIGNING FIELD-SYMBOL(<spfli>).
    Select * into table <spfli>-sflight_tab
      from sflight
     where  carrid = <spfli>-carrid.
  ENDLOOP.

 lcl_appl=>open_int_table( EXPORTING it_tab = lt_flight iv_name = 'scarr' iv_dummy = 'X'  ).
   lcl_appl=>open_int_table( EXPORTING it_tab = lt_spfli iv_name = 'spfli' iv_dummy = 'X' iv_show = 'X'   ).

ENDFORM.
