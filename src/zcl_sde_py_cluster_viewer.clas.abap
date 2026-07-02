CLASS zcl_sde_py_cluster_viewer DEFINITION PUBLIC INHERITING FROM zcl_sde_popup CREATE PUBLIC.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_hier,
        anynode   TYPE string,
        anyparent TYPE string,
        key       TYPE salv_de_node_key, "internal tree key
        name      TYPE string,
        tab_ref   TYPE REF TO data,
        type(1),
      END OF ts_hier,
      tt_hier TYPE TABLE OF ts_hier,
      BEGIN OF t_children,
        item TYPE REF TO zcl_sde_table_viewer,
      END OF t_children.

    DATA: mt_hier     TYPE tt_hier, " Tree hierarchy
          mo_nodes    TYPE REF TO cl_salv_nodes,
          mo_node     TYPE REF TO cl_salv_node,
          mo_events   TYPE REF TO cl_salv_events_tree,
          mt_empty    TYPE tt_hier,
          mr_cluster  TYPE REF TO data, "payru_result,
          m_pernr(8)  TYPE n,
          m_seqnr(5)  TYPE n,
          mt_children TYPE TABLE OF t_children.

    DATA :  mo_tree  TYPE REF TO cl_salv_tree.
    METHODS: constructor IMPORTING i_pernr TYPE any i_seqnr TYPE any,
      show_tree,
      on_box_close  REDEFINITION .

  PRIVATE SECTION.
    METHODS: init_alv_tree,
      create_tree,
      create_hierarchy,
      read_cluster,
      hndl_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key.
ENDCLASS.

CLASS  zcl_sde_py_cluster_viewer IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    m_pernr = i_pernr.
    m_seqnr = i_seqnr.
    mo_box = create( i_name = CONV #( m_pernr ) i_width = 180 i_hight = 280  ).
    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_box
        rows    = 1
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->get_container(
     EXPORTING
       row       = 1
       column    = 1
     RECEIVING
       container = mo_parent ).

    SET HANDLER on_box_close FOR mo_box.

    read_cluster( ).
    create_hierarchy( ).
    show_tree( ).
  ENDMETHOD.

  METHOD init_alv_tree.
    TRY.
        CALL METHOD cl_salv_tree=>factory
          EXPORTING
            r_container = mo_parent
          IMPORTING
            r_salv_tree = mo_tree
          CHANGING
            t_table     = mt_empty.
      CATCH cx_salv_error.
    ENDTRY.

    CALL METHOD mo_tree->get_event RECEIVING value = DATA(lo_event).
    SET HANDLER me->hndl_double_click FOR lo_event.
  ENDMETHOD.

  METHOD on_box_close.
    sender->free( ).

    LOOP AT mt_children INTO DATA(child).
      READ TABLE zcl_sde_appl=>mt_obj WITH KEY alv_viewer = child-item ASSIGNING FIELD-SYMBOL(<obj>).
      CHECK sy-subrc = 0.
      DATA(l_indx) = sy-tabix.

      child-item->mo_box->free( EXCEPTIONS cntl_error = 1  ).
      FREE <obj>-alv_viewer->mr_table.
      FREE <obj>-alv_viewer->mo_alv.

      "shutdown receivers.
      IF <obj>-alv_viewer->mo_sel IS NOT INITIAL.
        LOOP AT <obj>-alv_viewer->mo_sel->mt_sel_tab INTO DATA(l_sel).
          IF l_sel-receiver IS BOUND.
            l_sel-receiver->shut_down( ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      FREE <obj>-alv_viewer.
      DELETE zcl_sde_appl=>mt_obj INDEX l_indx.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_tree.
    init_alv_tree( ).
    create_tree( ).

    DATA(lo_columns) = mo_tree->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    lo_columns->get_column( 'KEY' )->set_visible( abap_false ).
    lo_columns->get_column( 'NAME' )->set_visible( abap_false ).
    lo_columns->get_column( 'ANYNODE' )->set_visible( abap_false ).
    lo_columns->get_column( 'ANYPARENT' )->set_visible( abap_false ).
    lo_columns->get_column( 'TYPE' )->set_visible( abap_false ).

    DATA(lo_tree_settings) = mo_tree->get_tree_settings( ).
    lo_tree_settings->set_hierarchy_header( CONV #( m_seqnr ) ).
    mo_nodes->expand_all( ).
    mo_tree->get_functions( )->set_all( ).
    mo_tree->display( ).
    mo_box->set_focus( mo_box ).
  ENDMETHOD.

  METHOD hndl_double_click.

    DATA: go_struct    TYPE REF TO cl_abap_structdescr,
          go_table     TYPE REF TO cl_abap_tabledescr,
          go_abapstr   TYPE REF TO cl_abap_typedescr,
          l_struc_name TYPE tabname,
          lr_tab       TYPE REF TO data.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                   <str>   TYPE any.

    DATA(ls_hier) = mt_hier[ node_key ].
    CHECK ls_hier-tab_ref IS NOT INITIAL.
    go_abapstr = cl_abap_structdescr=>describe_by_data_ref( ls_hier-tab_ref ).
    IF go_abapstr->type_kind = 'h'.
      go_table  ?= cl_abap_structdescr=>describe_by_data_ref( ls_hier-tab_ref ).
      go_struct ?= go_table->get_table_line_type( ).
      l_struc_name = go_struct->absolute_name.
    ELSE.
      go_struct  ?= cl_abap_structdescr=>describe_by_data_ref( ls_hier-tab_ref ).
      l_struc_name = go_struct->absolute_name.
      zcl_sde_rtti=>create_table_by_name( EXPORTING i_tname = l_struc_name CHANGING c_table = lr_tab  ).
      ASSIGN lr_tab->* TO <table>.
      ASSIGN ls_hier-tab_ref->* TO <str>.
      APPEND INITIAL LINE TO <table> ASSIGNING FIELD-SYMBOL(<line>).
      MOVE-CORRESPONDING <str> TO <line>.
      ls_hier-tab_ref = lr_tab.
    ENDIF.
    ls_hier-type = go_abapstr->type_kind.
    DATA(l_name) = |{ m_pernr }: ({ m_seqnr }) |.
    REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_struc_name WITH ''.
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_struc_name ir_tab = ls_hier-tab_ref i_additional_name = l_name.
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
    APPEND VALUE #( item = <obj>-alv_viewer  ) TO mt_children.
  ENDMETHOD.

  METHOD read_cluster.
    DATA: lo_handle  TYPE REF TO cl_abap_complexdescr.

    FIELD-SYMBOLS: <cluster> TYPE any.

    lo_handle ?= cl_abap_typedescr=>describe_by_name( 'PAYRU_RESULT' ).
    CREATE DATA mr_cluster TYPE HANDLE lo_handle.
    ASSIGN mr_cluster->* TO <cluster>.

    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        employeenumber               = m_pernr
        sequencenumber               = m_seqnr
      CHANGING
        payroll_result               = <cluster>
      EXCEPTIONS
        illegal_isocode_or_clusterid = 1
        error_generating_import      = 2
        import_mismatch_error        = 3
        subpool_dir_full             = 4
        no_read_authority            = 5
        no_record_found              = 6
        versions_do_not_match        = 7
        error_reading_archive        = 8
        error_reading_relid          = 9
        OTHERS                       = 10.
  ENDMETHOD.

  METHOD create_hierarchy.
    DATA: lo_stru    TYPE REF TO cl_abap_structdescr,
          lo_element TYPE REF TO cl_abap_structdescr,
          ls_hier    LIKE LINE OF mt_hier,
          l_lines    TYPE i.

    FIELD-SYMBOLS: <table>   TYPE ANY TABLE,
                   <struc>   TYPE any,
                   <cluster> TYPE any.

    lo_stru ?= cl_abap_typedescr=>describe_by_name( 'PAYRU_RESULT' ).

    "top node
    APPEND VALUE #( anynode = 'Main' anyparent = '' name = 'Cluster' ) TO mt_hier.

    LOOP AT lo_stru->components INTO DATA(ls_comp).

      ls_hier-anynode = ls_comp-name.
      ls_hier-anyparent = 'Main'.
      ls_hier-name = ls_comp-name.
      APPEND ls_hier TO mt_hier.

      ASSIGN mr_cluster->* TO <cluster>.
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE <cluster> TO FIELD-SYMBOL(<element>).
      lo_element ?= cl_abap_typedescr=>describe_by_data( <element> ).
      DATA(lt_comp) = lo_element->get_components( ).

      LOOP AT lt_comp INTO DATA(ls_el).
        CHECK ls_el-type->type_kind = 'u' "structure
           OR ls_el-type->type_kind = 'h'. "table

        CLEAR: l_lines, ls_hier.
        IF ls_el-type->type_kind = 'h'.
          ASSIGN COMPONENT ls_el-name OF STRUCTURE <element> TO <table>.
          l_lines = lines( <table> ).
          GET REFERENCE OF <table> INTO ls_hier-tab_ref.
        ELSE.
          ASSIGN COMPONENT ls_el-name OF STRUCTURE <element> TO <struc>.
          GET REFERENCE OF <struc> INTO ls_hier-tab_ref.
        ENDIF.
        CHECK l_lines NE 0 OR ls_el-type->type_kind = 'u'.

        ls_hier-anynode = ls_el-name.
        ls_hier-anyparent = ls_comp-name.
        ls_hier-name = ls_el-name.
        IF ls_el-type->type_kind = 'h'.
          ls_hier-name = |{ ls_hier-name } ({ l_lines })|.
        ENDIF.
        ls_hier-type = ls_el-type->type_kind.
        APPEND ls_hier TO mt_hier.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_tree.
    DATA: lv_text TYPE lvc_value,
          lv_icon TYPE salv_de_tree_image.

    TRY.
        mo_nodes = mo_tree->get_nodes( ).
        LOOP AT mt_hier ASSIGNING FIELD-SYMBOL(<hier>).
          READ TABLE mt_hier WITH KEY anynode = <hier>-anyparent ASSIGNING FIELD-SYMBOL(<hier_up>).
          IF sy-subrc NE 0."root node

            mo_node = mo_nodes->add_node(
            related_node = ''
            relationship = if_salv_c_node_relation=>parent ).
          ELSE.
            IF <hier>-type = 'h'.
              lv_icon = icon_view_table.
            ELSE.
              lv_icon = icon_structure.
            ENDIF.
            mo_node = mo_nodes->add_node(
            related_node = <hier_up>-key
            relationship = if_salv_c_node_relation=>last_child
            collapsed_icon = lv_icon
            expanded_icon  = lv_icon  ).
          ENDIF.

          <hier>-key = mo_node->get_key( ).
          mo_node->set_data_row( <hier> ).
          lv_text = <hier>-name.
          mo_node->set_text( lv_text ).
        ENDLOOP.
      CATCH cx_salv_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
