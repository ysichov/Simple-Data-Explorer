*&---------------------------------------------------------------------*
*& Report YS_SDE - Simple Data Explorer
*& version: alpha 0.5.178.145
*& Written by Yurii Sychov
*& e-mail:   ysichov@gmail.com
*& skype:    ysichov
*& blog:     https://ysychov.wordpress.com/blog/
*& LinkedIn: https://www.linkedin.com/in/ysychov/
*&---------------------------------------------------------------------*
report ys_sde.

class lcl_data_receiver definition deferred.
class lcl_data_transmitter definition deferred.

types:
 begin of selection_display_s,
    ind         type i,
    field_label type lvc_fname,
    int_type(1),
    inherited   type aqadh_type_of_icon,
    emitter     type aqadh_type_of_icon,
    sign        type tvarv_sign,
    opti        type tvarv_opti,
    option_icon type aqadh_type_of_icon,
    low         type aqadh_range_value,
    high        type aqadh_range_value,
    more_icon   type aqadh_type_of_icon,
    range       type aqadh_t_ranges,
    name        type reptext,
    element     type text60,
    domain      type text60,
    datatype    type string,
    length      type i,
    transmitter type ref to lcl_data_transmitter,
    receiver    type ref to lcl_data_receiver,
    color       type lvc_t_scol,
    style       type lvc_t_styl,
  end of selection_display_s,
  begin of t_sel_row,
    sign        type tvarv_sign,
    opti        type tvarv_opti,
    option_icon type aqadh_type_of_icon,
    low         type aqadh_range_value,
    high        type aqadh_range_value,
    more_icon   type aqadh_type_of_icon,
    range       type aqadh_t_ranges,
  end of t_sel_row.

data: gt_sel type table of selection_display_s.
field-symbols: <g_str> type any.

selection-screen begin of screen 101.
parameters: gv_tname type tabname visible length 15 matchcode object dd_bastab_for_view.
selection-screen end of screen 0101.

"Begin of INCLUDE YS_SDE_CLASSES.

*----------------------------------------------------------------------*
*       CLASS lcl_plugins DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_plugins definition.
  public section.
    types: begin of t_field_links,
             tab    type tabname,
             field  type fieldname,
             rtab   type tabname,
             rfield type fieldname,
             const  type aqadh_range_value,
           end of t_field_links,
           begin of t_el_links,
             element type tablename,
             rtab    type tabname,
             rfield  type fieldname,
           end of t_el_links.

    class-data: mt_field_links type  table of t_field_links,
                mt_el_links    type  table of t_el_links.
    class-methods: init.
endclass.                    "lcl_plugins DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_plugins IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_plugins implementation.
  method init.
    "data elements links
*    mt_el_links = VALUE #(
*      ( element = 'PERSNO'   rtab = 'PA0002'  rfield = 'PERNR' )
*      ( element = 'HROBJID'  rtab = 'HRP1000' rfield = 'OBJID' )
*      ( element = 'HROBJID'  rtab = 'HRP1000' rfield = 'OTYPE' )
*      ( element = 'LGART'    rtab = 'T512W'   rfield = 'LGART' )
*      ).

    "field to field links
*    mt_field_links = VALUE #(
*      ( tab = 'PA0001'    field = 'PLANS' rtab = 'HRP1000' rfield = 'OTYPE' const = 'S' )
*      ( tab = 'PA0001'    field = 'PLANS' rtab = 'HRP1000' rfield = 'OBJID' )
*      ( tab = 'PA0001'    field = 'ORGEH' rtab = 'HRP1000' rfield = 'OTYPE' const = 'O' )
*      ( tab = 'PA0001'    field = 'ORGEH' rtab = 'HRP1000' rfield = 'OBJID' )
*      ( tab = 'PA0001'    field = 'STELL' rtab = 'HRP1000' rfield = 'OBJID' const = 'C' )
*      ( tab = 'PA0001'    field = 'STELL' rtab = 'HRP1000' rfield = 'OBJID' )
*      ( tab = 'HRP1000'   field = 'OTYPE' rtab = 'HRP1001' rfield = 'OTYPE' )
*      ( tab = 'HRP1000'   field = 'OBJID' rtab = 'HRP1001' rfield = 'OBJID' )
*      ( tab = 'HRP1001'   field = 'OTYPE' rtab = 'HRP1000' rfield = 'OTYPE' )
*      ( tab = 'HRP1001'   field = 'OBJID' rtab = 'HRP1000' rfield = 'OBJID' )
*      ( tab = 'HRP1001'   field = 'SCLAS' rfield = 'OTYPE' )
*      ( tab = 'HRP1001'   field = 'SOBID' rfield = 'OBJID' )
*      ( tab = 'HRP1002'   field = 'TABNR' rtab = 'HRT1002'  rfield = 'TABNR' )
*      ( tab = 'PA2006'    field = 'QUONR' rtab = 'PTQUODED' rfield = 'QUONR' )
*      ( tab = 'PTQUODED'  field = 'QUONR' rtab = 'PA2006'   rfield = 'QUONR' )
*      ( tab = 'PTQUODED'  field = 'DOCNR' rtab = 'PA2001'   rfield = 'DOCNR' )
*      ).
  endmethod.                    "init
endclass.                    "lcl_plugins IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_ddic DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_ddic definition.
  public section.
    class-methods: get_text_table importing i_tname      type tabname
                                  exporting e_checkfield type fieldname
                                            e_tab        type tabname.
endclass.                    "lcl_ddic DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_ddic IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_ddic implementation.
  method get_text_table.
    call function 'DDUT_TEXTTABLE_GET'
      exporting
        tabname    = i_tname
      importing
        texttable  = e_tab
        checkfield = e_checkfield.
  endmethod.                    "get_text_table

endclass.                    "lcl_ddic IMPLEMENTATION

class lcl_table_viewer definition deferred.
class lcl_box_handler  definition deferred.
class lcl_sel_opt definition deferred.

*----------------------------------------------------------------------*
*       CLASS LCL_DD_DATA DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_dd_data definition."drag&drop data
  public  section.
    data: m_row    type i,
          m_column type lvc_s_col.
endclass.                    "lcl_dd_data DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_DRAGDROP DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_dragdrop definition.
  public section.
    class-methods:
      drag for event ondrag of cl_gui_alv_grid
        importing es_row_no e_dragdropobj e_row e_column ,
      drop for event ondrop of cl_gui_alv_grid
        importing es_row_no e_dragdropobj e_row.
endclass.                    "lcl_dragdrop DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SQL DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_sql definition.
  public section.
    class-methods:
     exist_table importing i_tab like gv_tname returning value(e_subrc) like sy-subrc.
endclass.                    "lcl_sql DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SQL IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_sql implementation.
  method exist_table.
    select count( * ) from dd02l
     where tabname = i_tab
       and ( tabclass = 'TRANSP' or tabclass = 'CLUSTER' ).
    e_subrc = sy-dbcnt.
  endmethod.                    "exist_table

endclass.                    "lcl_sql IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_rtti DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_rtti definition.
  public section.
    class-methods:
      create_table_by_name importing i_tname type tabname changing c_table type ref to data,
      create_struc_handle importing i_tname type tabname exporting e_t_comp type abap_component_tab e_handle type ref to cl_abap_structdescr.
endclass.                    "lcl_rtti DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_rtti IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_rtti implementation.
  method create_struc_handle.
    data: l_texttab     type tabname,
          lo_texttab    type ref to cl_abap_structdescr,
          ls_comp       type abap_componentdescr,
          lt_components type abap_component_tab,
          lt_field_info type table of dfies,
          ls_field      type dfies.

    lcl_ddic=>get_text_table( exporting i_tname = i_tname importing e_tab = l_texttab ).
    e_handle ?= cl_abap_typedescr=>describe_by_name( i_tname ).

    if l_texttab is not initial.
      lo_texttab  ?= cl_abap_typedescr=>describe_by_name( l_texttab ).
      data: l_descr like line of cl_abap_structdescr=>components.
      loop at e_handle->components into l_descr.
        ls_comp-name = l_descr-name.
        ls_comp-type ?= e_handle->get_component_type( ls_comp-name ).
        append ls_comp to lt_components.
      endloop.

      loop at lo_texttab->components into l_descr.

        call function 'DDIF_FIELDINFO_GET'
          exporting
            tabname        = l_texttab
            fieldname      = l_descr-name
            langu          = sy-langu
          tables
            dfies_tab      = lt_field_info
          exceptions
            not_found      = 1
            internal_error = 2
            others         = 3.

        read table lt_field_info into ls_field index 1.
        if ls_field-keyflag = abap_false.
          ls_comp-name =  l_texttab && '_' && l_descr-name.
          ls_comp-type ?= lo_texttab->get_component_type( l_descr-name ).
          append ls_comp to lt_components.
          append ls_comp to e_t_comp.
        endif.
      endloop.
      e_handle  = cl_abap_structdescr=>create( lt_components ).
    endif.
  endmethod.                    "create_struc_handle

  method create_table_by_name.
    data: lo_new_tab  type ref to cl_abap_tabledescr,
          lo_new_type type ref to cl_abap_structdescr.

    create_struc_handle( exporting i_tname = i_tname importing e_handle = lo_new_type ).
    lo_new_tab = cl_abap_tabledescr=>create(
                    p_line_type  = lo_new_type
                    p_table_kind = cl_abap_tabledescr=>tablekind_std
                    p_unique     = abap_false ).
    create data c_table type handle lo_new_tab.  "Create a New table type
  endmethod.                    "create_table_by_name
endclass.                    "lcl_rtti IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_ALV_COMMON DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_alv_common definition.
  public section.
    constants: c_white(4) type x value '00000001', "white background
              c_grey(4)  type x value '00000003', "white background
              c_green(4) type x value '00000216', "green +underline
              c_blue(4)  type x value '00000209', " blue font +underline
              c_bold(4)  type x value '00000020'.

    types: begin of t_tabfields.
            include type   dfies.
    types: empty type xfeld,
           end of t_tabfields.
    class-data: mt_tabfields type hashed table of t_tabfields with unique key tabname fieldname.

    class-methods:
      refresh importing i_obj type ref to cl_gui_alv_grid i_layout type lvc_s_layo optional,
      translate_field importing i_lang type ddlanguage optional changing c_fld type lvc_s_fcat.
endclass.                    "lcl_alv_common DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_COMMON IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_alv_common implementation.

  method refresh.
    data l_stable type lvc_s_stbl.
    l_stable-row = 'X'.
    l_stable-col = 'X'.
    if i_layout is supplied.
      i_obj->set_frontend_layout( i_layout ) .
    endif.
    i_obj->refresh_table_display( exporting is_stable = l_stable  ).
  endmethod.                    "refresh

  method translate_field.
    data: lv_lang       like sy-langu,
          lt_field_info type table of dfies.

    call function 'DDIF_FIELDINFO_GET'
      exporting
        tabname        = c_fld-ref_table
        fieldname      = c_fld-fieldname
        langu          = lv_lang
      tables
        dfies_tab      = lt_field_info
      exceptions
        not_found      = 1
        internal_error = 2
        others         = 3.

    if sy-subrc = 0.
      data: l_info type dfies.
      read table lt_field_info index 1 into l_info.
      if l_info-scrtext_l is initial and l_info-scrtext_m is initial and l_info-scrtext_s is initial.
        if l_info-fieldtext is not initial.
          move l_info-fieldtext to: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        else.
          move l_info-fieldname to: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        endif.
      else.
        c_fld-scrtext_l = l_info-scrtext_l.
        c_fld-scrtext_m = l_info-scrtext_m.
        c_fld-scrtext_s = l_info-scrtext_s.
        if l_info-reptext is not initial.
          c_fld-reptext   = l_info-reptext.
        endif.
      endif.
    endif.
  endmethod.                    "translate_field
endclass.                    "lcl_alv_common IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_APPL DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_appl definition.
  public section.

    types: begin of sign_option_icon_s,
             sign          type tvarv_sign,
             option        type tvarv_opti,
             icon_name(64) type c,
             icon          type aqadh_type_of_icon,
           end of sign_option_icon_s,

           begin of t_obj,
             alv_viewer type ref to lcl_table_viewer,
           end of t_obj,

           begin of t_lang,
             spras type spras,
             sptxt type sptxt,
           end of t_lang  .

    class-data: m_option_icons     type table of sign_option_icon_s,
                mt_lang            type table of t_lang,
                mt_obj             type table of t_obj, "main object table
                m_ctrl_box_handler type ref to lcl_box_handler,
                c_dragdropalv      type ref to cl_dragdrop.

    class-methods:
      init_icons_table,
      init_lang,
      suppress_run_button,
      exit.
endclass.                    "lcl_appl DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_DATA_TRANSMITTER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_data_transmitter definition.
  public section.
    events: data_changed exporting value(e_row) type t_sel_row,
             col_changed exporting value(e_column) type lvc_fname.
    methods: emit importing e_row type t_sel_row,
      emit_col importing e_column type lvc_fname.
endclass.                    "lcl_data_transmitter DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_DATA_TRANSMITTER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_data_transmitter implementation.
  method  emit.
    raise event data_changed exporting e_row = e_row.
  endmethod.                    "emit

  method emit_col.
    raise event col_changed exporting e_column = e_column.
  endmethod.                    "emit_col
endclass.                    "lcl_data_transmitter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_DATA_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_data_receiver definition.
  public section.
    data: mo_transmitter type ref to lcl_data_transmitter,
          lo_tab_from    type ref to lcl_table_viewer,
          lo_sel_to      type ref to lcl_sel_opt,
          m_from_field   type lvc_fname,
          m_to_field     type lvc_fname.
    methods: constructor
      importing io_transmitter type ref to lcl_data_transmitter optional
                io_tab_from    type ref to lcl_table_viewer optional
                io_sel_to      type ref to lcl_sel_opt optional
                i_from_field   type lvc_fname optional
                i_to_field     type lvc_fname optional,
      shut_down,
      update for event data_changed of lcl_data_transmitter importing e_row,
      update_col for event col_changed of lcl_data_transmitter importing e_column,
      on_grid_button_click
            for event button_click of cl_gui_alv_grid
        importing
            es_col_id
            es_row_no.
endclass.                    "lcl_data_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SEL_OPT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_sel_opt definition.
  public section.

    data: mo_viewer  type ref to lcl_table_viewer,
          mo_sel_alv type ref to cl_gui_alv_grid,
          mt_fcat    type lvc_t_fcat,
          mt_sel_tab type table of selection_display_s,
          ms_layout  type lvc_s_layo.

    events: selection_done.

    methods:
      constructor importing io_viewer type ref to lcl_table_viewer io_container type ref to cl_gui_container,
      raise_selection_done,
      update_sel_tab,
      set_value importing  i_field type any i_low type any optional i_high type any optional i_clear type xfeld optional,
      update_sel_row changing c_sel_row type selection_display_s.

  private section.

    methods:
      init_fcat importing i_dd_handle type i,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
      handle_sel_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object e_interactive,
      on_f4 for event onf4 of cl_gui_alv_grid
        importing e_fieldname
                    es_row_no
                    er_event_data,
      on_grid_button_click for event button_click of cl_gui_alv_grid
        importing
            es_col_id
            es_row_no,
      on_data_changed for event
                    data_changed of cl_gui_alv_grid
        importing e_onf4
                    e_onf4_before
                    er_data_changed
                    sender,
      on_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells,
      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm,
      handle_context_menu_request for event context_menu_request
            of cl_gui_alv_grid
        importing
            e_object
            sender.
endclass.                    "lcl_sel_opt DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_TABLE_VIEWER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_table_viewer definition.

  public section.

    types: begin of t_column_emitter,
             column  type lvc_fname,
             emitter type ref to lcl_data_transmitter,
           end of t_column_emitter.

    data: m_lang             type ddlanguage,
          m_tabname          type tabname,
          m_count            type i,
          mo_alv             type ref to cl_gui_alv_grid,
          mo_sel             type ref to lcl_sel_opt,
          mo_box             type ref to cl_gui_dialogbox_container,
          mr_table           type ref to data,
          mr_text_table      type ref to data,
          mo_splitter        type ref to cl_gui_splitter_container,
          mo_sel_parent      type ref to cl_gui_container,
          mo_alv_parent      type ref to cl_gui_container,
          mt_alv_catalog     type lvc_t_fcat,
          mt_text_components type abap_component_tab,
          m_checkfield       type fieldname,
          mo_column_emitters type table of t_column_emitter,
          mo_sel_width       type i,
          m_visible,
          m_std_tbar         type x,
          m_show_empty.

    methods:
      constructor importing i_tname type tabname,
      get_where returning value(c_where) type string,
      refresh_table for event selection_done of lcl_sel_opt.

  private section.
    methods:
      create_popup,
      create_alv,
      create_sel_alv,
      set_header,
      read_text_table,
      update_texts,
      read_table importing i_tabname   type tabname
                           i_where     type string
                           i_row_count type i optional
                 changing  cr_tab      type ref to data
                           c_count     type i,
      link importing  i_str type any
                      i_column type any returning value(r_done) type xfeld,

       create_field_cat importing i_tname type tabname returning value(et_catalog) type lvc_t_fcat,
 on_f4 for event onf4 of cl_gui_alv_grid importing e_fieldname es_row_no er_event_data,

      handle_tab_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object e_interactive,

      handle_menu_button
                    for event menu_button of cl_gui_alv_grid
        importing e_object e_ucomm,
      before_user_command for event before_user_command of cl_gui_alv_grid importing e_ucomm,
      handle_doubleclick for event double_click of cl_gui_alv_grid importing e_column es_row_no,
      handle_user_command
                    for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.                    "lcl_table_viewer DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_DATA_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_data_receiver implementation.

  method constructor.
    lo_sel_to = io_sel_to.
    m_from_field =  i_from_field.
    m_to_field =  i_to_field.

    lo_tab_from = io_tab_from.
    mo_transmitter = io_transmitter.

    if mo_transmitter is not initial.
      if lo_tab_from is initial.
        set handler me->update for io_transmitter.
      else.
        set handler me->update_col for io_transmitter.
      endif.
    else.
      set handler me->update for all instances.
    endif.
  endmethod.                    "constructor

  method shut_down.
    if mo_transmitter is not initial.
      set handler me->update for mo_transmitter  activation space.
    else.
      set handler me->update for all instances  activation space.
    endif.
    clear lo_sel_to.
  endmethod.                    "shut_down

  method on_grid_button_click.
    field-symbols: <f_tab>   type standard table,
                   <f_field> type any.
    check m_from_field = es_col_id-fieldname.

    assign lo_tab_from->mr_table->* to <f_tab>.
    field-symbols <tab> type any.
    read table <f_tab> index es_row_no-row_id assigning <tab>.
    assign component es_col_id-fieldname of structure <tab> to  <f_field>.

    check lo_sel_to is not initial.
    field-symbols <to> like line of lo_sel_to->mt_sel_tab.
    read table lo_sel_to->mt_sel_tab assigning <to> with key field_label = m_to_field.
    clear: <to>-high, <to>-opti, <to>-sign, <to>-range.
    <to>-low = <f_field>.
    lo_sel_to->update_sel_row( changing c_sel_row = <to> ).

    if <to>-transmitter is bound.
      data: ls_row type t_sel_row.
      move-corresponding <to> to ls_row.
      <to>-transmitter->emit( exporting e_row = ls_row ).
    endif.

    lcl_alv_common=>refresh( lo_sel_to->mo_sel_alv ).
    lo_sel_to->raise_selection_done( ).
  endmethod.                    "on_grid_button_click

  method  update.
    data: l_updated.
    field-symbols <to> like line of lo_sel_to->mt_sel_tab.
    read table lo_sel_to->mt_sel_tab assigning <to> with key field_label = m_to_field.
    if <to>-range[] = e_row-range[].
      l_updated = 'X'."so as not to have an infinite event loop
    endif.
    move-corresponding e_row to <to>.

    if <to>-transmitter is bound and l_updated is initial.
      <to>-transmitter->emit( exporting e_row = e_row ).
    endif.

    lcl_alv_common=>refresh( lo_sel_to->mo_sel_alv ).
    lo_sel_to->raise_selection_done( ).
  endmethod.                    "update

  method update_col.

    data: lt_sel_row type t_sel_row.
    field-symbols: <tab>   type standard table,
                   <field> type any.

    check lo_sel_to is not initial.
    field-symbols <to> like line of lo_sel_to->mt_sel_tab.
    read table lo_sel_to->mt_sel_tab assigning <to> with key field_label = m_to_field.
    clear: <to>-sign, <to>-opti, <to>-low, <to>-high, <to>-range.
    assign lo_tab_from->mr_table->* to <tab>.

    field-symbols <row> type any.
    loop at <tab> assigning <row>.
      assign component e_column of structure <row> to <field>.
      read table <to>-range with key low = <field>  transporting no fields.
      if sy-subrc ne 0.
        data ls_range type aqadh_s_ranges.
        ls_range-sign = 'I'.
        ls_range-opti = 'EQ'.
        ls_range-low = <field>.
        append ls_range to <to>-range.
      endif.
    endloop.

    if sy-subrc ne 0." empty column
      ls_range-sign = 'I'.
      ls_range-opti = 'EQ'.
      ls_range-low = ''.
      append ls_range to <to>-range.
    endif.

    field-symbols <sel> type aqadh_s_ranges.
    loop at <to>-range assigning <sel>.
      <to>-low = <sel>-low.
      lo_sel_to->update_sel_row( changing c_sel_row = <to> ).
      exit.
    endloop.

    move-corresponding <to> to lt_sel_row.

    if <to>-transmitter is bound. " AND l_updated IS INITIAL.
      <to>-transmitter->emit( exporting e_row = lt_sel_row ).
    endif.

    lcl_alv_common=>refresh( lo_sel_to->mo_sel_alv ).
    lo_sel_to->raise_selection_done( ).

  endmethod.                    "update_col
endclass.                    "lcl_data_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_BOX_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_box_handler definition."for memory clearing
  public section.
    methods: on_box_close for event close of cl_gui_dialogbox_container importing sender.
endclass.                    "lcl_box_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_BOX_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_box_handler implementation.

  method on_box_close.

    data: lv_tabix like sy-tabix.
    sender->free( ).

    "Free Memory
    field-symbols <obj> type lcl_appl=>t_obj.
    loop at lcl_appl=>mt_obj assigning <obj>.
      if <obj>-alv_viewer->mo_box = sender.
        lv_tabix = sy-tabix.
        exit.
      endif.
    endloop.
    if sy-subrc = 0.
      free <obj>-alv_viewer->mr_table.
      free <obj>-alv_viewer->mo_alv.

      "shutdown receivers.
      if <obj>-alv_viewer->mo_sel is not initial.
        data l_sel type selection_display_s.
        loop at <obj>-alv_viewer->mo_sel->mt_sel_tab into l_sel.
          if l_sel-receiver is bound.
            l_sel-receiver->shut_down( ).
          endif.
        endloop.
      endif.
      free <obj>-alv_viewer.

      delete lcl_appl=>mt_obj index lv_tabix.
    endif.
  endmethod.                    "ON_BOX_CLOSE
endclass.               "lcl_box_handler

*----------------------------------------------------------------------*
*       CLASS LCL_TABLE_VIEWER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_table_viewer implementation.

  method constructor.
    m_lang = sy-langu.
    mo_sel_width = 0.
    m_tabname = i_tname.
    create_popup( ).
    lcl_rtti=>create_table_by_name( exporting i_tname = m_tabname changing c_table = mr_table ).
    create_alv( ).
    create_sel_alv( ).
    mo_alv->set_focus( mo_alv ).
  endmethod.                    "constructor

  method create_popup.
    data: l_top  type i,
          l_left type i.

    data l_lines type i.
    l_lines = lines( lcl_appl=>mt_obj ) - 1.
    l_top  = 20 + 30 * ( l_lines div 5 ) +  ( l_lines mod 5 ) * 50.
    l_left = 350 + 300 * ( l_lines div 5 )  +  ( l_lines mod 5 ) * 50.

    create object mo_box
      exporting
        width                       = '800'
        height                      = '150'
        top                         = l_top
        left                        = l_left
        caption                     = m_tabname
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        others                      = 8.
    if sy-subrc <> 0.
      return.
    endif.

    create object mo_splitter
      exporting
        parent  = mo_box
        rows    = 1
        columns = 2
      exceptions
        others  = 1.

    mo_splitter->set_column_mode(  mode = mo_splitter->mode_absolute ).
    mo_splitter->set_column_width( id = 1 width = mo_sel_width ).

    call method:
     mo_splitter->get_container(  exporting
        row       = 1
        column    = 1
      receiving
        container = mo_sel_parent ),

      mo_splitter->get_container
       exporting
        row       = 1
        column    = 2
       receiving
        container = mo_alv_parent.

    if lcl_appl=>m_ctrl_box_handler is initial.
      create object lcl_appl=>m_ctrl_box_handler.
    endif.
    set handler lcl_appl=>m_ctrl_box_handler->on_box_close for mo_box.
  endmethod.                    "create_popup

  method create_alv.

    data: ls_layout  type lvc_s_layo,
          effect     type i,
          lt_f4      type lvc_t_f4,

          handle_alv type i.
    field-symbols: <f_tab>   type any table.

    create object mo_alv
      exporting
        i_parent = mo_alv_parent.
    mt_alv_catalog = create_field_cat( m_tabname ).
    assign mr_table->* to <f_tab>.
    read_text_table( ).
    read_table( exporting i_tabname = m_tabname i_where = get_where( ) i_row_count = 100
                         changing cr_tab =  mr_table c_count = m_count ).

    set_header( ).
    ls_layout-col_opt = 'X'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-sel_mode = 'D'.

    create object lcl_appl=>c_dragdropalv.

    effect = cl_dragdrop=>move + cl_dragdrop=>copy.

    call method lcl_appl=>c_dragdropalv->add
      exporting
        flavor     = 'Line'
        dragsrc    = 'X'
        droptarget = 'X'
        effect     = effect.

    call method lcl_appl=>c_dragdropalv->get_handle
      importing
        handle = handle_alv.

    ls_layout-s_dragdrop-grid_ddid = handle_alv.
    set handler   before_user_command
                  handle_user_command
                  handle_menu_button
                  handle_tab_toolbar
                  handle_doubleclick
                  lcl_dragdrop=>drag
                  on_f4
                   for mo_alv.

    call method mo_alv->set_table_for_first_display
      exporting
        i_save          = 'X'
        i_default       = 'X'
        is_layout       = ls_layout
      changing
        it_fieldcatalog = mt_alv_catalog
        it_outtab       = <f_tab>.

    mo_alv->get_frontend_fieldcatalog( importing et_fieldcatalog = mt_alv_catalog ).

    field-symbols <cat> type lvc_s_fcat.
    loop at mt_alv_catalog assigning <cat> where scrtext_l is initial.
      lcl_alv_common=>translate_field(  changing c_fld = <cat> ).
    endloop.

    data ls_f4 type lvc_s_f4.
    ls_f4-register   = 'X'.
    loop at mt_alv_catalog assigning <cat>.
      clear <cat>-key.
      ls_f4-fieldname  = <cat>-fieldname .
      insert ls_f4 into table lt_f4.
    endloop.
    mo_alv->register_f4_for_fields( it_f4 = lt_f4 ).
    mo_alv->set_frontend_fieldcatalog( exporting it_fieldcatalog = mt_alv_catalog ).


    mo_alv->set_frontend_fieldcatalog( exporting  it_fieldcatalog = mt_alv_catalog ).

    me->handle_user_command( exporting e_ucomm = 'HIDE' ).
    mo_alv->set_toolbar_interactive( ).
  endmethod.                    "create_alv

  method create_sel_alv.
    if mo_sel is initial.
      create object mo_sel
        exporting
          io_viewer    = me
          io_container = mo_sel_parent.
      set handler refresh_table for mo_sel.
    else.
      mo_sel->update_sel_tab( ).
    endif.
  endmethod.                    "create_sel_alv

  method set_header.
    data: lv_text       type as4text,
          lv_header(80) type c.

    select single ddtext into lv_text
      from dd02t
      where tabname = m_tabname
        and ddlanguage = m_lang.

    lv_header = |{ m_tabname } - { lv_text } ({ m_count })|.
    mo_box->set_caption( lv_header ).
  endmethod.                    "set_header

  method read_text_table.
    data: l_tab type tabname.
    field-symbols: <f_tab> type any table.

    lcl_ddic=>get_text_table( exporting i_tname =  m_tabname importing e_tab = l_tab ).
    check l_tab is not initial.
    lcl_rtti=>create_table_by_name( exporting i_tname = l_tab changing c_table = mr_text_table ).
    assign mr_text_table->* to <f_tab>.
    select * from (l_tab) into table <f_tab> order by primary key.
  endmethod.                    "read_text_table

  method update_texts.
    data: l_text_field type fieldname,
          l_replace    type string,
          lv_clause    type string,
          l_tab type tabname.

    field-symbols: <f_tab> type any table,
                   <text_tab> type any table.
    field-symbols: <str> type any,
                   <to> type any,
                  <check> type any,
                   <text_str> type any,
                   <dummy> type any,
                   <from> type any.

    "text fields
    lcl_ddic=>get_text_table( exporting i_tname =  m_tabname importing e_tab = l_tab ).
    check l_tab is not initial.

    assign mr_table->* to <f_tab>.
    assign mr_text_table->* to <text_tab>.

    l_replace = l_tab && '_'.
    loop at <f_tab> assigning <str>.
      data: ls_comp type abap_componentdescr.
      loop at mt_text_components into ls_comp.
        l_text_field = ls_comp-name.
        replace l_replace in l_text_field with ''.
        assign component ls_comp-name of structure <str> to <to>.
        assign component m_checkfield of structure <str> to <check>.
        check sy-subrc = 0.
        lv_clause = |{ m_checkfield } = '{ <check> }'|.
        loop at <text_tab> assigning <text_str>  where (lv_clause).
          exit.
        endloop.
        if sy-subrc = 0.
          assign component 'SPRSL' of structure <text_str> to <dummy>.
          if sy-subrc = 0.
            lv_clause = |{ lv_clause } AND SPRSL = '{ m_lang }'|.
          endif.
          assign component 'SPRAS' of structure <text_str> to <dummy>.
          if sy-subrc = 0.
            lv_clause = |{ lv_clause } AND SPRAS = '{ m_lang }'|.
          endif.
        else.
          continue.
        endif.

        loop at <text_tab> assigning <text_str>  where (lv_clause).
          exit.
        endloop.
        check sy-subrc = 0.
        assign component l_text_field of structure <text_str> to <from>.
        <to> = <from>.
      endloop.
    endloop.

  endmethod.                    "update_texts


  method read_table.
    field-symbols: <f_tab> type any table.

    assign cr_tab->* to <f_tab>.
    if i_where is not initial.
      try.
          select * from (i_tabname) into corresponding fields of table <f_tab> where (i_where) order by primary key.
        catch cx_sy_dynamic_osql_semantics.
        catch cx_sy_dynamic_osql_syntax.
        catch cx_sy_conversion_no_number.
      endtry.
    else.
      if i_row_count is not supplied.
        select * from (i_tabname) into corresponding fields of table <f_tab>.
      else.
        select * from (i_tabname) into corresponding fields of table <f_tab> up to i_row_count rows.
      endif.
    endif.
    c_count = sy-dbcnt.
    update_texts( ).
  endmethod.                    "read_table



  method handle_tab_toolbar.
    if m_visible is initial.
      data: lt_toolbar type ttb_button,
            ls_toolbar type stb_button.
      ls_toolbar-function = 'SEL_ON'.
      ls_toolbar-icon = icon_arrow_left.
      ls_toolbar-quickinfo = 'Select-Options'.
      ls_toolbar-butn_type = 0.
      append ls_toolbar to lt_toolbar.

      clear ls_toolbar.
      ls_toolbar-butn_type = 3.
      append ls_toolbar to lt_toolbar.
    endif.
    ls_toolbar-function = 'LANGUAGE'.
    ls_toolbar-icon = icon_foreign_trade.
    ls_toolbar-quickinfo = 'Languages'.
    ls_toolbar-butn_type = 2.
    append ls_toolbar to lt_toolbar.

    ls_toolbar-function = 'OPTIONS'.
    ls_toolbar-icon  = icon_list.
    ls_toolbar-quickinfo = 'Empty columns options'.
    append ls_toolbar to lt_toolbar.

    ls_toolbar-function = 'TABLES'.
    ls_toolbar-icon  = icon_net_graphic.
    ls_toolbar-quickinfo = 'Table links'.
    ls_toolbar-butn_type = 0.
    append ls_toolbar to lt_toolbar.

    clear ls_toolbar.
    ls_toolbar-butn_type = 3.
    append ls_toolbar to lt_toolbar.

    append lines of e_object->mt_toolbar to lt_toolbar.
    e_object->mt_toolbar = lt_toolbar.
  endmethod.                    "handle_tab_toolbar

  method create_field_cat.
    data: lv_clause      type string,
          lr_struc       type ref to data,
          lr_table_descr type ref to cl_abap_structdescr,
          it_tabdescr    type abap_compdescr_tab,
          lt_field_info  type table of dfies,
          l_fname        type fieldname,
          l_tname        type tabname,
          l_replace      type string,
          l_texttab       type tabname,
          lo_str type ref to cl_abap_structdescr .

    lcl_rtti=>create_struc_handle( exporting i_tname = i_tname importing e_t_comp = mt_text_components e_handle = lo_str ).
    create data lr_struc type handle lo_str.
    lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_struc ).
    it_tabdescr[] = lr_table_descr->components[].

    lcl_ddic=>get_text_table( exporting i_tname = i_tname importing e_checkfield = m_checkfield e_tab = l_texttab ).

    l_replace = l_texttab && '_'.

    data: l_ind type i,
          ls like line of it_tabdescr,
          ls_tf like line of lcl_alv_common=>mt_tabfields.
    field-symbols: <catalog> like line of  et_catalog.
    loop at it_tabdescr into ls where name ne 'MANDT'.
      l_ind = sy-tabix.
      append initial line to et_catalog assigning <catalog>.
      <catalog>-col_pos = l_ind.
      read table lcl_alv_common=>mt_tabfields into ls_tf with key tabname = i_tname fieldname = ls-name.
      if sy-subrc ne 0.
        l_tname = i_tname.
        l_fname = ls-name.

        if l_texttab is not initial.

          l_fname = ls-name.
          replace l_replace in l_fname with ''.
          if sy-subrc = 0.
            l_tname = l_texttab.
          endif.
        endif.
        call function 'DDIF_FIELDINFO_GET'
          exporting
            tabname        = l_tname
            fieldname      = l_fname
            langu          = sy-langu
          tables
            dfies_tab      = lt_field_info
          exceptions
            not_found      = 1
            internal_error = 2
            others         = 3.
        if sy-subrc ne 0.
          continue.
        endif.
        clear ls_tf.

        read table lt_field_info index 1 into ls_tf.
        ls_tf-fieldname = ls-name.

        "check empty field
        if ls_tf-domname ne 'MANDT'.
          data: dref type ref to data,
                l_x  type xstring.
          field-symbols <field> type any.

          if ls_tf-rollname is not initial.
            create data dref type (ls_tf-rollname).
            assign dref->* to <field>.
            lv_clause = |{ l_fname } NE ''|.
            select single (l_fname) into <field>
              from (l_tname)
             where (lv_clause).
            if sy-subrc ne 0.
              ls_tf-empty = 'X'.
            endif.
          elseif ls_tf-datatype = 'RAWSTRING'.
            lv_clause = |{ ls_tf-fieldname } NE ''|.
            select single (ls_tf-fieldname) into l_x
              from (l_tname)
             where (lv_clause).
            if sy-subrc ne 0.
              ls_tf-empty = 'X'.
            endif.
          endif.
        endif.
        insert ls_tf into table lcl_alv_common=>mt_tabfields.
      endif.
      <catalog>-style = lcl_alv_common=>c_white.
      move-corresponding ls_tf to <catalog>.
      <catalog>-no_zero = 'X'.
      <catalog>-f4availabl = 'X'.
      if ls_tf-tabname ne m_tabname.
        <catalog>-style = lcl_alv_common=>c_grey.
      endif.

      if ls_tf-checktable is not initial.
        <catalog>-style = lcl_alv_common=>c_blue.
      endif.

      read table lcl_plugins=>mt_field_links with key tab = i_tname field = ls_tf-fieldname transporting no fields.
      if sy-subrc = 0.
        <catalog>-style = lcl_alv_common=>c_green.
      endif.

      read table lcl_plugins=>mt_el_links with key element = ls_tf-rollname transporting no fields.
      if sy-subrc = 0.
        <catalog>-style = lcl_alv_common=>c_green.
      endif.

      if ls_tf-keyflag = 'X'.
        <catalog>-style = <catalog>-style bit-or lcl_alv_common=>c_bold.
      endif.
    endloop.
  endmethod.                    "create_field_cat

  method on_f4.

    field-symbols: <tab> type standard table.
    assign mr_table->* to <tab>.
    read table <tab> index es_row_no-row_id assigning <g_str>.
    call function 'F4IF_FIELD_VALUE_REQUEST'
      exporting
        tabname           = m_tabname
        fieldname         = e_fieldname
        callback_program  = sy-repid
        callback_form     = 'CALLBACK_F4_TAB' "callback_method - doesn't work for local class
      exceptions
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        others            = 5.
  endmethod.                    "on_f4

  method link.
    data: i_viewer type ref to lcl_table_viewer,
          ls_link type lcl_plugins=>t_field_links.

    field-symbols: <field> type any,
                   <obj> type lcl_appl=>t_obj.
    clear r_done.
    "field to field links
    loop at lcl_plugins=>mt_field_links into ls_link where tab = m_tabname and field = i_column.
      assign component ls_link-field of structure i_str to <field>.
      if i_viewer is initial.
        if ls_link-rtab is initial.
          i_viewer = me.
        else.
          append initial line to lcl_appl=>mt_obj assigning <obj>.
          create object <obj>-alv_viewer
            exporting
              i_tname = ls_link-rtab.
          i_viewer = <obj>-alv_viewer.
        endif.
      endif.
      if ls_link-const is initial.
        i_viewer->mo_sel->set_value( i_field = ls_link-rfield i_low = <field> i_clear = 'X' ).
      else.
        i_viewer->mo_sel->set_value( i_field = ls_link-rfield i_low = ls_link-const i_clear = 'X'  ).
      endif.
    endloop.
    if sy-subrc = 0.
      i_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang i_clear = 'X' ).
      i_viewer->mo_sel->raise_selection_done( ).
      r_done = 'X'.
    endif.
    check r_done is initial.

    data l_field type lcl_alv_common=>t_tabfields.
    read table lcl_alv_common=>mt_tabfields with key tabname = m_tabname fieldname = i_column into l_field transporting rollname.
    "data element to field links
    data l_el_link type lcl_plugins=>t_el_links.
    loop at lcl_plugins=>mt_el_links into l_el_link where element = l_field-rollname .
      if i_viewer is initial.
        append initial line to lcl_appl=>mt_obj assigning <obj>.
        create object <obj>-alv_viewer
          exporting
            i_tname = l_el_link-rtab.
        i_viewer = <obj>-alv_viewer.
      endif.
      assign component i_column of structure i_str to <field>.
      i_viewer->mo_sel->set_value( i_field = l_el_link-rfield i_low = <field> i_clear = 'X' ).
    endloop.
    if sy-subrc = 0.
      i_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang i_clear = 'X' ).
      i_viewer->mo_sel->raise_selection_done( ).
      r_done = 'X'.
    endif.
    check r_done is initial.

    "dictionary key links plugin
    data: lt_keys type table of dd05p,
          ls_key  type dd05p.
    read table lcl_alv_common=>mt_tabfields into l_field with key tabname = m_tabname fieldname = i_column .
    assign component i_column of structure i_str to <field>.
    check <field> is not initial.
    data: l_column(30).
    l_column = i_column.
    call function 'DD_FORKEY_GET'
      exporting
        feldname  = l_column
        tabname   = m_tabname
      tables
        forkeytab = lt_keys
      exceptions
        not_equal = 1
        not_found = 2
        not_valid = 3
        others    = 4.

    if sy-subrc < 2.
      append initial line to lcl_appl=>mt_obj assigning <obj>.
      data: l_table type tabname.
      l_table = l_field-checktable.
      create object <obj>-alv_viewer
        exporting
          i_tname = l_table.
      loop at lt_keys into ls_key.
        assign component ls_key-forkey of structure i_str to <field>.
        check sy-subrc = 0.
        <obj>-alv_viewer->mo_sel->set_value( i_field = ls_key-checkfield i_low = <field> i_clear = 'X'  ).
      endloop.
      <obj>-alv_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang i_clear = 'X' ).
      <obj>-alv_viewer->mo_sel->raise_selection_done( ).
    endif.
  endmethod.                    "link

  method handle_menu_button.
    if e_ucomm = 'LANGUAGE'.
      call method e_object->add_function
        exporting
          fcode = 'TECH'
          text  = 'Technical name'. "Teхническое имя
      data ls_lang type lcl_appl=>t_lang.
      loop at lcl_appl=>mt_lang into ls_lang.
        data:  lv_lang type ui_func,
               lv_text type gui_text.

        call function 'CONVERSION_EXIT_ISOLA_OUTPUT'
          exporting
            input            = ls_lang-spras
          importing
            output           = lv_lang
          exceptions
            unknown_language = 1
            others           = 2.

        lv_text = ls_lang-sptxt.
        call method e_object->add_function
          exporting
            fcode = lv_lang
            text  = lv_text.
      endloop.
    elseif e_ucomm = 'OPTIONS'.
      call method e_object->add_function
        exporting
          fcode = 'HIDE'
          text  = 'Hide empty columns'. "Спрятать пустые столбцы
      call method e_object->add_function
        exporting
          fcode = 'SHOW'
          text  = 'Show empty columns'. "Отобразить пустые столбцы
    endif.
  endmethod.                    "handle_menu_button

  method before_user_command.
    data l_url(100) type c.
    case e_ucomm.
      when '&INFO'.
        l_url = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.

        call function 'CALL_BROWSER'
          exporting
            url = l_url.
    endcase.
  endmethod.                    "before_user_command

  method handle_doubleclick.
    data: lt_keys type table of dd05p.
    field-symbols: <f_tab>  type standard table,
                   <tab> type any.
    check es_row_no-row_id is not initial.
    assign mr_table->* to  <f_tab>.
    read table <f_tab> index es_row_no-row_id assigning <tab>.
    link( exporting i_str = <tab> i_column = e_column ).
  endmethod.                    "handle_doubleclick

  method handle_user_command.
    data:
      it_fields    type lvc_t_fcat,
      lv_clause    type string,
      lv_sel_width type i,
      ls_lang type lcl_appl=>t_lang.

    field-symbols: <field>  like line of it_fields,
                   <f_tab>  type any table,
                   <f_line> type any.

    mo_alv->get_frontend_fieldcatalog( importing et_fieldcatalog = it_fields[] ).
    assign mr_table->* to <f_tab>.
    if e_ucomm = 'SEL_ON' and m_visible is initial.
      create_sel_alv( ).
      m_visible = 'X'.
      if mo_sel_width = 0.
        lv_sel_width = 500.
      else.
        lv_sel_width = mo_sel_width.
      endif.

      mo_splitter->set_column_width( exporting
          id    = 1
          width = lv_sel_width ).

      mo_alv->set_toolbar_interactive( ).
      return.
    endif.

    loop at it_fields assigning <field>.
      case e_ucomm.
        when 'HIDE'. "hide select options
          clear m_show_empty.
          lv_clause = |{ <field>-fieldname } IS NOT INITIAL|.
          loop at <f_tab> assigning <f_line>  where (lv_clause).
            exit.
          endloop.

          if sy-subrc ne 0.
            <field>-no_out = 'X'.
          endif.

        when 'SHOW'.

          m_show_empty = 'X'.
          <field>-no_out = ' '.
        when 'TECH'. "technical field name
          <field>-scrtext_l = <field>-scrtext_m = <field>-scrtext_s =  <field>-reptext = <field>-fieldname.
        when 'TABLES'.
          data: lt_obj type sdg1_obj,
                ls_obj type sdg1_obj1.
          ls_obj-obj_name = m_tabname.
          ls_obj-type = 'TABL'.
          append ls_obj to lt_obj.
          call function 'REPOSITORY_STRUCTURE_GRAPH'
            exporting
              type    = 'TABL'
            tables
              objects = lt_obj.
          set titlebar 'SDE'.
          return.
        when others. "header names translation
          read table lcl_appl=>mt_lang with key spras = e_ucomm into ls_lang.
          if sy-subrc = 0.
            lcl_alv_common=>translate_field( exporting i_lang = ls_lang-spras changing c_fld = <field> ).
            if mo_sel is bound.
              field-symbols <sel> type selection_display_s.
              read table mo_sel->mt_sel_tab assigning <sel> with key field_label = <field>-fieldname.
              if sy-subrc = 0.
                <sel>-name = <field>-scrtext_l.
                if <sel>-name is initial.
                  <sel>-name = <field>-reptext.
                endif.
              endif.
            endif.
          endif.
      endcase.
    endloop.

    read table lcl_appl=>mt_lang with key spras = e_ucomm transporting no fields.
    if sy-subrc = 0.
      m_lang = e_ucomm.
      set_header( ).
    endif.

    call method mo_alv->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = it_fields[].
    lcl_alv_common=>refresh( mo_alv ).
    if mo_sel is bound.
      if  e_ucomm = 'HIDE' or e_ucomm = 'SHOW'.
        mo_sel->update_sel_tab( ).
      endif.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
    endif.
  endmethod.                           "handle_user_command

  method get_where."dynamic where clause

    data: lt_where type rsds_twhere,
          ls_where type rsds_where,
          l_where type rsdswhere,
           lt_range type rsds_trange.


    field-symbols: <tabl> type rsds_range,
                   <t_range> type rsds_frange.

    if  mo_sel is not initial.
      append initial line to lt_range assigning <tabl>.
      <tabl>-tablename = m_tabname.
      data: ls_tab type selection_display_s.
      loop at mo_sel->mt_sel_tab into ls_tab where range is not initial.
        append initial line to <tabl>-frange_t assigning <t_range>.
        if sy-subrc = 0.
          <t_range>-fieldname = ls_tab-field_label.
          <t_range>-selopt_t  = ls_tab-range.
        endif.
      endloop.

      call function 'FREE_SELECTIONS_RANGE_2_WHERE'
        exporting
          field_ranges  = lt_range
        importing
          where_clauses = lt_where.

      loop at lt_where into ls_where where tablename = m_tabname.
        loop at ls_where-where_tab into l_where.
          condense l_where-line.
          c_where = |{ c_where } { l_where-line }|.
        endloop.
      endloop.
    endif.
  endmethod.                    "get_where

  method refresh_table.
    data: ls_row type t_sel_row.
    read_table( exporting i_tabname = m_tabname i_where = get_where( ) changing cr_tab =  mr_table c_count = m_count ).
    set_header( ).
    field-symbols <sel> type selection_display_s.
    loop at mo_sel->mt_sel_tab  assigning <sel>.
      if <sel>-transmitter is not initial.
        move-corresponding <sel> to ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      endif.
    endloop.

    lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
    lcl_alv_common=>refresh( mo_alv ).

    data l_emit type t_column_emitter.
    loop at mo_column_emitters into l_emit.
      l_emit-emitter->emit_col( l_emit-column ).
    endloop.
  endmethod.                    "refresh_table
endclass.                    "lcl_table_viewer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_SEL_OPT IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_sel_opt implementation.
  method constructor.
    data: ls_layout  type lvc_s_layo,
          effect     type i,
          handle_alv type i.

    mo_viewer = io_viewer.
    create object mo_sel_alv
      exporting
        i_parent = io_container.
    update_sel_tab( ).
    create object lcl_appl=>c_dragdropalv.
    effect =  cl_dragdrop=>copy. " + cl_dragdrop=>move.

    call method lcl_appl=>c_dragdropalv->add
      exporting
        flavor     = 'Line'
        dragsrc    = 'X'
        droptarget = 'X'
        effect     = effect.

    call method lcl_appl=>c_dragdropalv->get_handle
      importing
        handle = handle_alv.

    ls_layout-s_dragdrop-col_ddid = handle_alv.
    init_fcat( handle_alv ).

    ls_layout-cwidth_opt = 'X'.
    ls_layout-ctab_fname = 'COLOR'.
    ls_layout-stylefname = 'STYLE'.

    "fields for F4 event handling
    data: gt_f4 type lvc_t_f4,
          gs_f4 type lvc_s_f4.
    gs_f4-register = 'X'.
    gs_f4-chngeafter = 'X'.
    gs_f4-fieldname  = 'LOW'.
    insert gs_f4 into table gt_f4.
    gs_f4-fieldname  = 'HIGH'.
    insert gs_f4 into table gt_f4.

    mo_sel_alv->register_f4_for_fields( it_f4 = gt_f4 ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    set handler handle_user_command
                handle_sel_toolbar
                handle_doubleclick
                lcl_dragdrop=>drag
                lcl_dragdrop=>drop
                on_data_changed
                on_data_changed_finished
                on_grid_button_click
                handle_context_menu_request for mo_sel_alv.
    set handler on_f4 for mo_sel_alv.

    call method mo_sel_alv->set_table_for_first_display
      exporting
        i_save          = 'X'
        i_default       = 'X'
        is_layout       = ls_layout
      changing
        it_outtab       = mt_sel_tab[]
        it_fieldcatalog = mt_fcat.

    mo_sel_alv->set_toolbar_interactive( ).

  endmethod.                    "constructor

  method init_fcat.
    data ls_fcat type lvc_s_fcat.
    ls_fcat-style = '00000003'.
    ls_fcat-fieldname = 'IND'.
    ls_fcat-coltext = '№'.
    ls_fcat-outputlen = 3.
    append ls_fcat to mt_fcat.
    ls_fcat-fieldname = 'FIELD_LABEL'.
    ls_fcat-coltext = 'Label'.
    ls_fcat-outputlen = 30.
    ls_fcat-dragdropid = i_dd_handle.
    ls_fcat-emphasize = 'X'.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'SIGN'.
    ls_fcat-coltext = 'Sign'.
    ls_fcat-tech = 'X'.
    append ls_fcat to mt_fcat.
    ls_fcat-fieldname = 'OPTI'.
    ls_fcat-coltext = 'Option'.
    ls_fcat-tech = 'X'.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'OPTION_ICON'.
    ls_fcat-coltext = 'Option'.
    ls_fcat-outputlen = 4.
    ls_fcat-style = cl_gui_alv_grid=>mc_style_button.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'LOW'.
    ls_fcat-coltext = 'From data'.
    ls_fcat-edit = 'X'.
    ls_fcat-lowercase = 'X'.
    ls_fcat-outputlen = 45.
    ls_fcat-style = cl_gui_alv_grid=>mc_style_f4.
    append ls_fcat to mt_fcat.
    ls_fcat-fieldname = 'HIGH'.
    ls_fcat-coltext = 'To data'.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'MORE_ICON'.
    ls_fcat-coltext = 'Range'.
    ls_fcat-outputlen = 4.
    ls_fcat-style = cl_gui_alv_grid=>mc_style_button.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'RANGE'.
    ls_fcat-tech = 'X'.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'INHERITED'.
    ls_fcat-coltext = 'Inh.'.
    ls_fcat-outputlen = 4.
    ls_fcat-icon = 'X'.
    ls_fcat-seltext = 'Inherited'.
    append ls_fcat to mt_fcat.
    ls_fcat-fieldname = 'Emitter'.
    ls_fcat-coltext = 'Emit.'.
    ls_fcat-seltext = 'Emitter'.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'NAME'.
    ls_fcat-coltext = 'Field name'.
    ls_fcat-style = '00000003'.
    ls_fcat-outputlen = 60.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'ELEMENT'.
    ls_fcat-coltext = 'Data element'.
    ls_fcat-style = '00000209'.
    ls_fcat-outputlen = 15.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'DOMAIN'.
    ls_fcat-coltext = 'Domain'.
    ls_fcat-style = '00000209'.
    ls_fcat-outputlen = 15.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'DATATYPE'.
    ls_fcat-coltext = 'Type'.
    ls_fcat-style = '00000003'.
    ls_fcat-outputlen = 5.
    append ls_fcat to mt_fcat.
    clear ls_fcat.
    ls_fcat-fieldname = 'LENGTH'.
    ls_fcat-coltext = 'Length'.
    ls_fcat-style = '00000003'.
    ls_fcat-outputlen = 5.
    append ls_fcat to mt_fcat.

    clear ls_fcat.
    ls_fcat-fieldname = 'TRANSMITTER'.
    ls_fcat-tech = 'X'.
    append ls_fcat to mt_fcat.
    ls_fcat-fieldname = 'RECEIVER'.
    append ls_fcat to mt_fcat.
    ls_fcat-fieldname = 'CHANGE'.
    append ls_fcat to mt_fcat.

  endmethod.                    "init_fcat

  method raise_selection_done.
    raise event selection_done.
    data: ls_row type t_sel_row.
    field-symbols <sel> type selection_display_s.
    loop at mt_sel_tab  assigning <sel>." WHERE change IS NOT INITIAL.
      if <sel>-transmitter is not initial.
        move-corresponding <sel> to ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      endif.
    endloop.
  endmethod.                    "raise_selection_done

  method update_sel_tab.
    data: l_tfield type lcl_alv_common=>t_tabfields.
    if mt_sel_tab[] is not initial.
      data lt_copy type table of selection_display_s.
      lt_copy = mt_sel_tab.
    endif.
    clear mt_sel_tab[].
    mo_viewer->mo_alv->get_frontend_fieldcatalog( importing et_fieldcatalog = mo_viewer->mt_alv_catalog ).
    data l_catalog type lvc_s_fcat.
    loop at mo_viewer->mt_alv_catalog into l_catalog where domname ne 'MANDT'.
      data lv_ind like sy-tabix.
      lv_ind = sy-tabix.
      read table lcl_alv_common=>mt_tabfields with key tabname = l_catalog-tabname fieldname = l_catalog-fieldname  into l_tfield.
      if l_tfield-empty = '' or mo_viewer->m_show_empty is not initial.
        "check l_catalog-no_out is initial.
        field-symbols <sel_tab> type selection_display_s.
        append initial line to mt_sel_tab assigning <sel_tab>.
        data ls_copy type selection_display_s.
        read table lt_copy into ls_copy with key field_label = l_catalog-fieldname.
        if sy-subrc = 0.
          move-corresponding ls_copy to <sel_tab>.
        else.
          <sel_tab>-option_icon = icon_led_inactive.
          <sel_tab>-more_icon = icon_enter_more.
        endif.
        <sel_tab>-ind = lv_ind.
        <sel_tab>-field_label = l_catalog-fieldname.
        <sel_tab>-int_type = l_catalog-inttype.
        <sel_tab>-element = l_catalog-rollname.
        <sel_tab>-domain =  l_catalog-domname.
        <sel_tab>-datatype = l_catalog-datatype.
        <sel_tab>-length = l_catalog-outputlen.

        lcl_alv_common=>translate_field( exporting i_lang = mo_viewer->m_lang changing c_fld = l_catalog ).

        <sel_tab>-name = l_catalog-scrtext_l.

        data: l_style type lvc_s_styl,
                       l_color type lvc_s_scol.
        if l_tfield-keyflag = 'X'.
          "append initial line to <sel_tab>-style assigning <style>.
          l_style-fieldname = 'FIELD_LABEL'.
          l_style-style = '00000020'.
          insert l_style into table <sel_tab>-style.
        endif.
        if l_tfield-empty = 'X'.
          "append initial line to <sel_tab>-color assigning <color>.
          l_color-fname = 'FIELD_LABEL'.
          l_color-color-col = 2.
          l_color-color-int = 0.
          l_color-color-inv = 1.
        else.
          "append initial line to <sel_tab>-color assigning <color>.
          l_color-fname = 'FIELD_LABEL'.
          l_color-color-col = 4.
          l_color-color-int = 0.
          l_color-color-inv = 1.
        endif.
        insert l_color into table <sel_tab>-color.
      endif.
    endloop.
  endmethod.                    "update_sel_tab

  METHOD handle_doubleclick.
    data: l_sel type selection_display_s.
    CHECK es_row_no-row_id IS NOT INITIAL.
    READ TABLE mt_sel_tab INDEX es_row_no-row_id INTO l_sel.
    IF e_column = 'ELEMENT'.
      SET PARAMETER ID 'DTYP' FIELD l_sel-element.
      CALL TRANSACTION 'SE11' AND SKIP FIRST SCREEN.
    ELSEIF e_column = 'DOMAIN'.
      SET PARAMETER ID 'DOM' FIELD l_sel-domain.
      CALL TRANSACTION 'SE11' AND SKIP FIRST SCREEN.
    ELSE.
      CALL FUNCTION 'DOCU_CALL'
        EXPORTING
          id                = 'DE'
          langu             = mo_viewer->m_lang
          object            = l_sel-element
          typ               = 'E'
          displ             = 'X'
          displ_mode        = 3
          use_sec_langu     = 'X'
          display_shorttext = 'X'.
    ENDIF.
  ENDMETHOD.


  method handle_sel_toolbar.
    data: ls_toolbar type stb_button.

    clear e_object->mt_toolbar[].
    ls_toolbar-function = 'SEL_OFF'.
    ls_toolbar-icon = icon_arrow_right.
    ls_toolbar-quickinfo = 'Hide'.
    ls_toolbar-butn_type = 0.
    ls_toolbar-disabled = ''.
    append ls_toolbar to e_object->mt_toolbar[].
    ls_toolbar-function = 'SEL_CLEAR'.
    ls_toolbar-icon = icon_delete_row.
    ls_toolbar-quickinfo = 'Clear Select-Options'.
    append ls_toolbar to e_object->mt_toolbar[].
  endmethod.                    "handle_sel_toolbar

  method set_value.
    field-symbols: <to> type selection_display_s,
                   <range> type aqadh_s_ranges.
    read table mt_sel_tab assigning <to> with key field_label = i_field.
    check sy-subrc = 0.

    if i_low is supplied.
      if i_clear is initial.
        append initial line to <to>-range assigning <range>.
        <range> = 'IEQ'.
        <range>-low = i_low.
      else.
        clear:  <to>-opti, <to>-sign,<to>-range.
        if i_low is supplied.
          <to>-low = i_low.
        endif.
        if i_high is supplied.
          <to>-high = i_high.
        endif.
        update_sel_row( changing c_sel_row = <to> ).
      endif.
    else.
      clear:  <to>-opti, <to>-sign.
      <to>-high = i_high.
      update_sel_row( changing c_sel_row = <to> ).
    endif.
    if <to>-transmitter is bound.
      data: ls_row type t_sel_row.
      move-corresponding <to> to ls_row.
      <to>-transmitter->emit( exporting e_row = ls_row ).
    endif.
  endmethod.                    "set_value


  method update_sel_row. "select patterns rules
    if c_sel_row-high is initial and c_sel_row-opti = 'BT'.
      clear c_sel_row-opti.
    endif.

    if c_sel_row-low is not initial and c_sel_row-opti is initial.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'EQ'.
    endif.

    if c_sel_row-high is not initial and c_sel_row-opti ne 'NB' .
      c_sel_row-opti = 'BT'.
    endif.

    if c_sel_row-sign is initial and c_sel_row-opti is initial.
      clear: c_sel_row-low, c_sel_row-low.
    endif.

    if c_sel_row-low ca  '*%+&'.
      c_sel_row-sign = 'I'.
      c_sel_row-opti = 'CP'.
    endif.

    if c_sel_row-opti is not initial and c_sel_row-sign is initial.
      c_sel_row-sign = 'I'.
    endif.

    data l_icons type lcl_appl=>sign_option_icon_s.
    read table lcl_appl=>m_option_icons with key sign = c_sel_row-sign option = c_sel_row-opti into l_icons.
    if sy-subrc = 0.
      c_sel_row-option_icon = l_icons-icon_name.
    endif.

    if c_sel_row-sign is not initial.
      field-symbols <range> type aqadh_s_ranges.
      read table c_sel_row-range assigning <range> index 1.
      if sy-subrc ne 0.
        append initial line to c_sel_row-range assigning <range>.
      endif.
      move-corresponding c_sel_row to <range>.
      if c_sel_row-opti ne 'BT' and c_sel_row-opti ne 'NB' .
        clear c_sel_row-high.
      endif.
    endif.
    if c_sel_row-range is initial.
      c_sel_row-more_icon = icon_enter_more.
    else.
      c_sel_row-more_icon = icon_display_more.
    endif.
    if c_sel_row-receiver is bound and c_sel_row-inherited is initial.
      c_sel_row-inherited = icon_businav_value_chain.
    endif.
  endmethod.                    "update_sel_row

  method on_f4.
    data: return_tab  type standard table of ddshretval,
          l_multiple type xfeld,
          l_clear    type xfeld,
          l_fname type fieldname.

    field-symbols: <itab> type lvc_t_modi,
                   <sel> type selection_display_s.


    if e_fieldname = 'LOW'.
      l_multiple = 'X'.
    endif.

    read table mt_sel_tab assigning <sel> index es_row_no-row_id .
    if sy-subrc = 0.
      l_fname = <sel>-field_label.
    endif.
    call function 'F4IF_FIELD_VALUE_REQUEST'
      exporting
        tabname           = mo_viewer->m_tabname "todo: change PAXXXX for PXXXX
        fieldname         = l_fname
        callback_program  = sy-repid
        callback_form     = 'CALLBACK_F4_SEL' "callback_method - doesn't work for local class
        multiple_choice   = l_multiple
      tables
        return_tab        = return_tab
                                                                                                        "todo: callback for  f4 dependent fields
      exceptions
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        others            = 5.

    if sy-subrc = 0 and lines( return_tab ) > 0..
      field-symbols <ret> type ddshretval.
      assign er_event_data->m_data->* to <itab>.
      clear <sel>-range.
      l_clear = abap_true.
      loop at return_tab assigning <ret> where fieldname = l_fname.
        if e_fieldname = 'LOW'.
          set_value( exporting i_field = <sel>-field_label i_low = <ret>-fieldval i_clear = l_clear ).
          clear l_clear.
        else.
          set_value( exporting i_field = <sel>-field_label i_high = <ret>-fieldval ).
        endif.
      endloop.
      er_event_data->m_event_handled = 'X'.
    endif.
    raise_selection_done( ).
  endmethod.                    "on_f4

  method on_grid_button_click.
    data:
      l_tabfield type rstabfield,
      ls_opt     type rsoptions value 'XXXXXXXXXX',
      lv_sign    type raldb_sign,
      lv_option  type raldb_opti.

    field-symbols <tab> type selection_display_s.
    read table mt_sel_tab index es_row_no-row_id assigning <tab>.
    case es_col_id.
      when 'OPTION_ICON'. "edit select logical expression type
        call function 'SELECT_OPTION_OPTIONS'
          exporting
            selctext     = 'nnnn'
            option_list  = ls_opt
          importing
            sign         = lv_sign
            option       = lv_option
          exceptions
            delete_line  = 1
            not_executed = 2
            illegal_sign = 3
            others       = 4.
        if sy-subrc = 0.
          <tab>-sign = lv_sign.
          <tab>-opti = lv_option.
        elseif sy-subrc = 1.
          clear: <tab>-low, <tab>-high,<tab>-sign, <tab>-opti, <tab>-range.
        endif.
      when 'MORE_ICON'. "edit ranges
        l_tabfield-tablename = mo_viewer->m_tabname.
        l_tabfield-fieldname = <tab>-field_label.

        call function 'COMPLEX_SELECTIONS_DIALOG'
          exporting
            title             = 'title'
            text              = 'text'
            tab_and_field     = l_tabfield
          tables
            range             = <tab>-range
          exceptions
            no_range_tab      = 1
            cancelled         = 2
            internal_error    = 3
            invalid_fieldname = 4
            others            = 5.
        if sy-subrc = 0.
          data l_range type aqadh_s_ranges.
          read table <tab>-range index 1 into l_range.
          move-corresponding l_range to <tab>.
          if <tab>-opti ne 'BT'.
            clear <tab>-high.
          endif.
        endif.
    endcase.

    update_sel_row( changing c_sel_row = <tab> ).
    lcl_alv_common=>refresh( mo_sel_alv ).
    raise event selection_done.
  endmethod.                    "on_grid_button_click

  method on_data_changed.
    data: l_start type i,
          l_cat type lvc_s_fcat,
          l_second type aqadh_s_ranges.
    field-symbols: <field> type any,
                   <ls_cells> type lvc_s_modi,
                   <tab> type selection_display_s.

    loop at er_data_changed->mt_good_cells assigning <ls_cells>.
      read table mt_sel_tab index <ls_cells>-row_id assigning <tab>.
      assign component <ls_cells>-fieldname of structure <tab> to <field>.
      read table mo_viewer->mt_alv_catalog with key fieldname = <tab>-field_label into l_cat.

      if <field> is not initial and <ls_cells>-value is initial.

        read table <tab>-range into l_second index 2.
        if sy-subrc = 0.
          if ( <ls_cells>-fieldname = 'LOW' and <tab>-high is initial ) or  ( <ls_cells>-fieldname = 'HIGH' and <tab>-low is initial  ).
            delete <tab>-range index 1.
          else.
            clear l_second.
          endif.
        endif.
      endif.

      if l_cat-convexit = 'ALPHA' and not  <ls_cells>-value ca '+*'.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <ls_cells>-value
          importing
            output = <ls_cells>-value.

        l_start = 128 - l_cat-dd_outlen.
        <ls_cells>-value = <ls_cells>-value+l_start(l_cat-dd_outlen).
      endif.

      if <ls_cells>-value is not initial.
        if <tab>-int_type = 'D'.
          data: lv_date type sy-datum.
          call function 'CONVERT_DATE_INPUT'
            exporting
              input                     = <ls_cells>-value
              plausibility_check        = 'X'
            importing
              output                    = lv_date
            exceptions
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              others                    = 3.
          <ls_cells>-value = |{ lv_date date = user }|.
        elseif <tab>-int_type = 'T'.
          data: lv_time type sy-uzeit.
          call function 'CONVERT_TIME_INPUT'
            exporting
              input                     = <ls_cells>-value
            importing
              output                    = lv_time
            exceptions
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              others                    = 3.
          <ls_cells>-value = lv_time+0(2) && ':' && lv_time+2(2) && ':' && lv_time+4(2).
        endif.
      endif.
    endloop.
    check sy-subrc = 0.

    if l_second is initial.
      <field> = <ls_cells>-value.
      er_data_changed->modify_cell( exporting i_row_id = <ls_cells>-row_id i_fieldname = <ls_cells>-fieldname i_value = <ls_cells>-value ).
    else.
      <tab>-low = l_second-low.
      er_data_changed->modify_cell( exporting i_row_id = <ls_cells>-row_id i_fieldname = 'LOW' i_value = l_second-low ).
      if l_second-high co '0 '.
        clear l_second-high.
      endif.
      <tab>-high = l_second-high.
      er_data_changed->modify_cell( exporting i_row_id = <ls_cells>-row_id i_fieldname = 'HIGH' i_value = l_second-high ).

      <tab>-opti = l_second-opti.
      er_data_changed->modify_cell( exporting i_row_id = <ls_cells>-row_id i_fieldname = 'OPTI' i_value = l_second-opti ).
      <tab>-sign = l_second-sign.
      er_data_changed->modify_cell( exporting i_row_id = <ls_cells>-row_id i_fieldname = 'SIGN' i_value = l_second-sign ).
    endif.

    update_sel_row( changing c_sel_row = <tab> ).
    lcl_alv_common=>refresh( exporting i_obj = mo_sel_alv i_layout = ms_layout  ).
    raise_selection_done( ).


  endmethod.                    "on_data_changed

  method on_data_changed_finished.
    check e_modified is not initial.
    raise event selection_done.
  endmethod.                    "on_data_changed_finished

  method handle_context_menu_request.
    data: lt_fcodes    type ui_funcattr,
          ls_fcode     type uiattentry,
          ls_func      type ui_func,
          lt_func      type ui_functions,
          lt_sel_cells type lvc_t_cell,
          lt_sel_rows  type lvc_t_row,
          l_index      type lvc_index.

    mo_sel_alv->get_selected_cells( importing et_cell = lt_sel_cells ).
    data l_cells type lvc_s_cell.
    read table lt_sel_cells into l_cells index 1 transporting row_id.
    if sy-subrc = 0.
      l_index = l_cells-row_id.
    else.
      mo_sel_alv->get_selected_rows( importing et_index_rows = lt_sel_rows ).
      data l_row type lvc_s_row.
      read table lt_sel_rows index 1 into l_row transporting index.
      if sy-subrc = 0.
        l_index = l_row-index.
      endif.
    endif.

    if l_index is not initial.
      data l_sel type selection_display_s.
      read table mt_sel_tab into l_sel index l_index.
    endif.

    call method e_object->get_functions
      importing
        fcodes = lt_fcodes. "Inactivate all standard functions

    loop at lt_fcodes into ls_fcode where fcode ne '&OPTIMIZE'.
      ls_func = ls_fcode-fcode.
      append ls_func to lt_func.
    endloop.
    e_object->hide_functions( lt_func ).
    e_object->add_separator( ).

    if l_sel-range[]  is not initial or l_index is initial.
      call method e_object->add_function
        exporting
          fcode = 'SEL_CLEAR'
          text  = 'Clear Select-Options'.
    endif.

    if l_sel-receiver is not initial or l_index is initial.
      call method e_object->add_function
        exporting
          fcode = 'DELR'
          text  = 'Delete receiver'.
    endif.
  endmethod.                    "handle_context_menu_request

  method handle_user_command.
    data: lv_sel_width type i,
          lt_sel_rows  type lvc_t_row.

    if e_ucomm = 'SEL_OFF'. "Hide select-options alv
      mo_viewer->m_visible = ''.

      lv_sel_width = 0.
      call method mo_viewer->mo_splitter->get_column_width
        exporting
          id                = 1
        importing
          result            = mo_viewer->mo_sel_width
        exceptions
          cntl_error        = 1
          cntl_system_error = 2
          others            = 3.

      call method mo_viewer->mo_splitter->set_column_width
        exporting
          id    = 1
          width = lv_sel_width.
      mo_viewer->mo_alv->set_toolbar_interactive( ).
      return.
    endif.

    if e_ucomm = 'SEL_CLEAR' or e_ucomm = 'DELR'. "clear all selections
      mo_sel_alv->get_selected_rows( importing et_index_rows = lt_sel_rows ).

      data l_row type lvc_s_row.
      loop at lt_sel_rows into l_row.
        field-symbols <sel> type selection_display_s.
        read table mt_sel_tab assigning <sel> index l_row-index.
        if e_ucomm = 'SEL_CLEAR'.
          clear : <sel>-low, <sel>-high, <sel>-sign, <sel>-opti, <sel>-range.
        elseif e_ucomm = 'DELR'.
          if <sel>-receiver is not initial.
            <sel>-receiver->shut_down( ).
            free <sel>-receiver.
            clear <sel>-receiver.
            clear <sel>-inherited.
          endif.
        endif.
        update_sel_row( changing c_sel_row = <sel> ).
      endloop.

      lcl_alv_common=>refresh( mo_sel_alv ).
      raise event selection_done.
    endif.

    lcl_alv_common=>refresh( mo_viewer->mo_alv ).
    raise event selection_done.
  endmethod.                           "handle_user_command
endclass.                    "lcl_sel_opt IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_appl IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_appl implementation.
  method init_icons_table.
    data: l_icon type sign_option_icon_s.
    l_icon-sign = space.
    l_icon-option = space.
    l_icon-icon_name = icon_led_inactive.
    append l_icon to m_option_icons.
    l_icon-sign = 'I'.
    l_icon-option = 'EQ'.
    l_icon-icon_name =  icon_equal_green.
    append l_icon to m_option_icons.
    l_icon-option = 'NE'.
    l_icon-icon_name =  icon_not_equal_green.
    append l_icon to m_option_icons.
    l_icon-option = 'LT'.
    l_icon-icon_name =  icon_less_green.
    append l_icon to m_option_icons.
    l_icon-option = 'LE'.
    l_icon-icon_name =  icon_less_equal_green.
    append l_icon to m_option_icons.
    l_icon-option = 'GT'.
    l_icon-icon_name =  icon_greater_green.
    append l_icon to m_option_icons.
    l_icon-option = 'GE'.
    l_icon-icon_name =  icon_greater_equal_green.
    append l_icon to m_option_icons.
    l_icon-option = 'CP'.
    l_icon-icon_name =  icon_pattern_include_green.
    append l_icon to m_option_icons.
    l_icon-option = 'NP'.
    l_icon-icon_name =  icon_pattern_exclude_green.
    append l_icon to m_option_icons.
    l_icon-option = 'BT'.
    l_icon-icon_name =  icon_interval_include_green.
    append l_icon to m_option_icons.
    l_icon-option = 'NB'.
    l_icon-icon_name =  icon_interval_exclude_green.
    append l_icon to m_option_icons.

    l_icon-sign = 'E'.
    l_icon-option = 'EQ'.
    l_icon-icon_name =  icon_equal_red.
    append l_icon to m_option_icons.
    l_icon-option = 'NE'.
    l_icon-icon_name =  icon_not_equal_red.
    append l_icon to m_option_icons.
    l_icon-option = 'LT'.
    l_icon-icon_name =  icon_less_red.
    append l_icon to m_option_icons.
    l_icon-option = 'LE'.
    l_icon-icon_name =  icon_less_equal_red.
    append l_icon to m_option_icons.
    l_icon-option = 'GT'.
    l_icon-icon_name =  icon_greater_red.
    append l_icon to m_option_icons.
    l_icon-option = 'GE'.
    l_icon-icon_name =  icon_greater_equal_red.
    append l_icon to m_option_icons.
    l_icon-option = 'CP'.
    l_icon-icon_name =  icon_pattern_include_red.
    append l_icon to m_option_icons.
    l_icon-option = 'NP'.
    l_icon-icon_name =  icon_pattern_exclude_red.
    append l_icon to m_option_icons.
    l_icon-option = 'BT'.
    l_icon-icon_name =  icon_interval_include_red.
    append l_icon to m_option_icons.
    l_icon-option = 'NB'.
    l_icon-icon_name =  icon_interval_exclude_red.
    append l_icon to m_option_icons.

  endmethod.                    "init_icons_table

  method exit.
    data: l_answer.
    describe table lcl_appl=>mt_obj.
    if sy-tfill ne 0.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar       = 'Exit'
          text_question  = 'Do you want to exit?'
        importing
          answer         = l_answer
        exceptions
          text_not_found = 1
          others         = 2.
      if l_answer = '1'.
        leave program.
      else.
        call screen 101.
      endif.
    endif.
  endmethod.                    "exit

  method init_lang.
    select c~spras t~sptxt into corresponding fields of table mt_lang
      from t002c as c
      inner join t002t as t
      on c~spras = t~sprsl
      where t~spras = sy-langu
      order by c~ladatum descending c~lauzeit descending.
  endmethod.                    "init_lang

  method suppress_run_button.
    data itab type table of sy-ucomm.
    append: 'ONLI' to itab.
    call function 'RS_SET_SELSCREEN_STATUS'
      exporting
        p_status  = sy-pfkey
      tables
        p_exclude = itab.
  endmethod.                    "suppress_run_button

endclass.                    "lcl_appl IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_dragdrop IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_dragdrop implementation.
  method drag.
    data: dataobj type ref to lcl_dd_data.
    create object dataobj.
    dataobj->m_row = e_row-index.
    dataobj->m_column = e_column.
    e_dragdropobj->object = dataobj.
  endmethod.                    "drag

  method drop.
    data: lo_from_sel     type ref to lcl_sel_opt,
          lo_from_tab     type ref to lcl_table_viewer,
          lo_to           type ref to lcl_sel_opt,
          lo_alv          type ref to cl_gui_alv_grid,
          lt_sel_rows     type lvc_t_row,
          lt_sel_cells    type lvc_t_cell,
          lt_sel_col      type lvc_t_col,
          ls_row          type t_sel_row,
          lv_set_receiver.

    data lo type lcl_appl=>t_obj.
    loop at lcl_appl=>mt_obj into lo.
      "to
      if lo-alv_viewer->mo_sel is bound.
        if e_dragdropobj->droptargetctrl = lo-alv_viewer->mo_sel->mo_sel_alv.
          lo_to = lo-alv_viewer->mo_sel.
        endif.
      endif.

      "from tab
      if lo-alv_viewer->mo_alv = e_dragdropobj->dragsourcectrl.
        lo_from_tab = lo-alv_viewer.
        continue.
      endif.

      check lo-alv_viewer->mo_sel is bound.
      if e_dragdropobj->dragsourcectrl = lo-alv_viewer->mo_sel->mo_sel_alv.
        lo_from_sel = lo-alv_viewer->mo_sel.
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_rows( importing et_index_rows = lt_sel_rows ).
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_cells( importing et_cell = lt_sel_cells ).
      endif.
    endloop.

    if lo_from_tab is bound." tab to select
      field-symbols: <f_tab>   type standard table,
                     <f_str>   type any,
                     <f_field> type any.
      lo_from_tab->mo_alv->get_selected_cells( importing et_cell = lt_sel_cells  ).
      lo_from_tab->mo_alv->get_selected_columns( importing et_index_columns = lt_sel_col  ).

      data l_col type lvc_s_col.
      loop at lt_sel_col into l_col.
        field-symbols <catalog> type lvc_s_fcat.
        read table lo_from_tab->mt_alv_catalog with key fieldname = l_col-fieldname assigning <catalog>.
        if sy-subrc = 0.
          <catalog>-style =  cl_gui_alv_grid=>mc_style_button.
        endif.
        field-symbols <emitter> type lcl_table_viewer=>t_column_emitter.
        read table lo_from_tab->mo_column_emitters with key column = l_col assigning <emitter>.
        if sy-subrc ne 0.
          append initial line to lo_from_tab->mo_column_emitters assigning <emitter>.
          <emitter>-column = l_col.
          create object <emitter>-emitter.
        endif.
      endloop.

      if sy-subrc = 0.
        lv_set_receiver = abap_true.
        call method lo_from_tab->mo_alv->set_frontend_fieldcatalog
          exporting
            it_fieldcatalog = lo_from_tab->mt_alv_catalog.
      endif.

      assign lo_from_tab->mr_table->* to <f_tab>.
      field-symbols <to_tab> type selection_display_s.
      read table lo_to->mt_sel_tab assigning <to_tab> index e_row.
      data l_cell type lvc_s_cell.
      loop at lt_sel_cells into l_cell.
        if sy-tabix = 1.
          data: l_colname type lvc_fname.
          l_colname = l_cell-col_id-fieldname.
        endif.
        read table <f_tab> index l_cell-row_id assigning <f_str>.
        assign component l_colname of structure <f_str> to <f_field>.
        if sy-subrc = 0.
          if lv_set_receiver is not initial.
            if <to_tab>-receiver is bound.
              <to_tab>-receiver->shut_down( ).
            endif.
            create object <to_tab>-receiver
              exporting
                io_transmitter = <emitter>-emitter
                i_from_field   = l_colname
                i_to_field     = <to_tab>-field_label
                io_sel_to      = lo_to
                io_tab_from    = lo_from_tab.
            set handler <to_tab>-receiver->on_grid_button_click for lo_from_tab->mo_alv.
          endif.

          if <to_tab>-range is initial.
            <to_tab>-low = <f_field>.
          endif.
          read table <to_tab>-range with key low = <f_field> transporting no fields.
          if sy-subrc ne 0.
            data ls_range type aqadh_s_ranges.
            ls_range-sign = 'I'.
            ls_range-low = <f_field>.
            append ls_range to <to_tab>-range.
          endif.
        endif.
      endloop.
      lo_to->update_sel_row( changing c_sel_row = <to_tab> ).
    endif.

    "select to select
    if lo_from_sel ne lo_to.
      if lt_sel_rows[] is initial.
        delete lt_sel_cells where col_id ne 'FIELD_LABEL'.
        data l_sel type lvc_s_cell.
        loop at lt_sel_cells into l_sel.
          field-symbols <row> type lvc_s_row.
          append initial line to lt_sel_rows assigning <row>.
          <row>-index = l_sel-row_id-index.
        endloop.
      endif.

      loop at lt_sel_rows assigning <row>.
        field-symbols <from_tab> type selection_display_s.
        read table lo_from_sel->mt_sel_tab assigning <from_tab> index <row>-index."7/
        if lines( lt_sel_rows ) = 1.
          read table lo_to->mt_sel_tab assigning <to_tab> index e_row.
        else.
          read table lo_to->mt_sel_tab assigning <to_tab> with key field_label = <from_tab>-field_label.
          if sy-subrc ne 0.
            continue.
          endif.
        endif.
        move-corresponding <from_tab> to ls_row.
        move-corresponding ls_row to <to_tab>.
        <from_tab>-emitter = icon_workflow_external_event.
        <to_tab>-inherited = icon_businav_value_chain.
        if <from_tab>-transmitter is initial.
          create object <from_tab>-transmitter.
        endif.
        if <to_tab>-receiver is not initial.
          <to_tab>-receiver->shut_down( ). "receiver clearing
          free <to_tab>-receiver.
        endif.
        create object <to_tab>-receiver
          exporting
            io_transmitter = <from_tab>-transmitter
            io_sel_to      = lo_to
            i_to_field     = <to_tab>-field_label.
      endloop.
    endif.

    lo_alv ?= e_dragdropobj->dragsourcectrl.
    lcl_alv_common=>refresh( lo_alv ).

    lo_alv ?= e_dragdropobj->droptargetctrl.
    lcl_alv_common=>refresh( lo_alv ).
    lo_to->raise_selection_done( ).
  endmethod.                    "drop
endclass.                    "lcl_dragdrop IMPLEMENTATION
"END OF INCLUDE YS_SDE_CLASSES.

*------------REPORT EVENTS--------------------
initialization.
  lcl_appl=>init_lang( ).
  lcl_appl=>init_icons_table( ).
  "lcl_plugins=>init( ).

  call selection-screen 101.

at selection-screen output.
  %_gv_tname_%_app_%-text = 'Enter table name and hit Enter'.
  lcl_appl=>suppress_run_button( ).

at selection-screen on exit-command.
  lcl_appl=>exit( ).

at selection-screen.
  check lcl_sql=>exist_table( gv_tname ) = 1.
  field-symbols <obj> type lcl_appl=>t_obj.
  append initial line to lcl_appl=>mt_obj assigning <obj>.
  create object <obj>-alv_viewer
    exporting
      i_tname = gv_tname.

*&---------------------------------------------------------------------*
*&      Form  callback_f4_sel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RECORD_TAB   text
*      <--SHLP         text
*      <--CALLCONTROL  text
*----------------------------------------------------------------------*
form callback_f4_sel tables record_tab structure seahlpres
          changing shlp type shlp_descr
                   callcontrol like ddshf4ctrl.

  data: l_sel type selection_display_s,
        l_range type aqadh_s_ranges.
  field-symbols: <interface> type ddshiface,
                 <searchsel> type ddshselopt.

  loop at shlp-interface assigning <interface> where f4field ne 'X'.
    read table gt_sel with key field_label = <interface>-shlpfield into l_sel.
    if sy-subrc = 0.
      loop at l_sel-range into l_range.
        append initial line to shlp-selopt assigning <searchsel>.
        move-corresponding l_range to <searchsel>.
        <searchsel>-shlpfield = <interface>-shlpfield.
        <searchsel>-option = 'EQ'.
      endloop.
    endif.
  endloop.
endform.                    "callback_f4_sel

*&---------------------------------------------------------------------*
*&      Form  callback_f4_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RECORD_TAB   text
*      <--SHLP         text
*      <--CALLCONTROL  text
*----------------------------------------------------------------------*
form callback_f4_tab tables record_tab structure seahlpres
          changing shlp type shlp_descr
                   callcontrol like ddshf4ctrl.

  field-symbols: <field> type any,
                 <interface> type ddshiface,
                 <searchsel> type ddshselopt.

  loop at shlp-interface assigning <interface> where f4field ne 'X'.
    assign component <interface>-shlpfield of structure <g_str> to <field>.
    if sy-subrc = 0.
      append initial line to shlp-selopt assigning <searchsel>.
      <searchsel>-sign = 'I'.
      <searchsel>-low = <field>.
      <searchsel>-shlpfield = <interface>-shlpfield.
      <searchsel>-option = 'EQ'.
    endif.
  endloop.
endform.                    "callback_f4_tab
