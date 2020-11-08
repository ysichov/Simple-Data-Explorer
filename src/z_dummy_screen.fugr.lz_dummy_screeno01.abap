*----------------------------------------------------------------------*
***INCLUDE LZ_DUMMY_SCREENO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0333 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0333 OUTPUT.
 SET PF-STATUS 'Z_DUMMY'.
* SET TITLEBAR 'xxx'.

  data lt_flight type TABLE OF sflight .

LOOP AT gt_tabs into data(tab).
   lcl_appl=>open_int_table( EXPORTING it_ref = tab-tab iv_name = tab-name ).
ENDLOOP.

clear gt_tabs.

ENDMODULE.
