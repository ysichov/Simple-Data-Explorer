FUNCTION-POOL Z_DUMMY_SCREEN.               "MESSAGE-ID ..

INCLUDE z_itable_explorer_cls.

types: begin of t_tab,
        tab type ref to data,
       name type string,
       end of t_tab.
data: gt_tabs type table of t_tab.

* INCLUDE LZ_DUMMY_SCREEND...                " Local class definition

MODULE user_command_0333 INPUT.
 Case sy-ucomm.
  When 'BACK'.
    LEAVE to SCREEN 0.
 ENDCASE.

ENDMODULE.

MODULE status_0333 OUTPUT.
 SET PF-STATUS 'Z_DUMMY'.
* SET TITLEBAR 'xxx'.

  data lt_flight type TABLE OF sflight .

LOOP AT gt_tabs into data(tab).
   lcl_appl=>open_int_table( EXPORTING it_ref = tab-tab iv_name = tab-name ).
ENDLOOP.

clear gt_tabs.

ENDMODULE.
