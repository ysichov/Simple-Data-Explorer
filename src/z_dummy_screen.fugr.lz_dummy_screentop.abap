FUNCTION-POOL z_dummy_screen.               "MESSAGE-ID ..

INCLUDE z_itable_explorer_cls.

TYPES: BEGIN OF t_tab,
         tab  TYPE REF TO data,
         name TYPE string,
       END OF t_tab.
DATA: gt_tabs TYPE TABLE OF t_tab.

* INCLUDE LZ_DUMMY_SCREEND...                " Local class definition

MODULE user_command_0333 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE status_0333 OUTPUT.
  SET PF-STATUS 'Z_DUMMY'.
  SET TITLEBAR 'Z_SIMPLE'.

  "data lt_flight type TABLE OF sflight .
  DATA: go_container TYPE REF TO cl_gui_custom_container.



  IF go_container IS INITIAL.
    " создаём контейнер, связанный с элементом экрана CC_ALV
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'C_TREE'.

  ENDIF.

  DATA(lo_viewer) = NEW lcl_run( go_container ).

   LOOP AT gt_tabs INTO DATA(tab).

     APPEND INITIAL LINE TO lo_viewer->mt_vars_hist ASSIGNING FIELD-SYMBOL(<var>).

     "GET REFERENCE OF g INTO gr_var.
*    APPEND INITIAL LINE TO mt_vars_hist ASSIGNING <hist>.
    <var>-ref = tab-tab.
    <var>-short = <var>-path = <var>-name = tab-name.

    "lcl_appl=>open_int_table2( EXPORTING it_ref = tab-tab iv_name = tab-name ).
  ENDLOOP.

  CLEAR gt_tabs.


  lo_viewer->init( ).




ENDMODULE.
