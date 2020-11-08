FUNCTION-POOL Z_DUMMY_SCREEN.               "MESSAGE-ID ..

INCLUDE z_itable_explorer_cls.

types: begin of t_tab,
        tab type ref to data,
       name type string,
       end of t_tab.
data: gt_tabs type table of t_tab.

* INCLUDE LZ_DUMMY_SCREEND...                " Local class definition
