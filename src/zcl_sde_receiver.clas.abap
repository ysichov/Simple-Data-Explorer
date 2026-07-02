CLASS zcl_sde_receiver DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    DATA: mo_transmitter TYPE REF TO zcl_sde_transmitter,
          lo_tab_from    TYPE REF TO zcl_sde_table_viewer,
          lo_sel_to      TYPE REF TO zcl_sde_sel_opt,
          m_from_field   TYPE lvc_fname,
          m_to_field     TYPE lvc_fname.
    METHODS: constructor
      IMPORTING io_transmitter TYPE REF TO zcl_sde_transmitter OPTIONAL
                io_tab_from    TYPE REF TO zcl_sde_table_viewer OPTIONAL
                io_sel_to      TYPE REF TO zcl_sde_sel_opt OPTIONAL
                i_from_field   TYPE lvc_fname OPTIONAL
                i_to_field     TYPE lvc_fname OPTIONAL,
      shut_down,
      update FOR EVENT data_changed OF zcl_sde_transmitter IMPORTING e_row,
      update_col FOR EVENT col_changed OF zcl_sde_transmitter IMPORTING e_column,
      on_grid_button_click
        FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no.
ENDCLASS.

CLASS zcl_sde_receiver IMPLEMENTATION.
  METHOD constructor.
    lo_sel_to = io_sel_to.
    m_from_field =  i_from_field.
    m_to_field =  i_to_field.
    lo_tab_from = io_tab_from.
    mo_transmitter = io_transmitter.

    IF mo_transmitter IS NOT INITIAL.
      IF lo_tab_from IS INITIAL.
        SET HANDLER me->update FOR io_transmitter.
      ELSE.
        SET HANDLER me->update_col FOR io_transmitter.
      ENDIF.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES.
    ENDIF.
  ENDMETHOD.

  METHOD shut_down.
    IF mo_transmitter IS NOT INITIAL.
      SET HANDLER me->update FOR mo_transmitter  ACTIVATION space.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES  ACTIVATION space.
    ENDIF.
    CLEAR lo_sel_to.
  ENDMETHOD.

  METHOD on_grid_button_click.
    FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE.

    CHECK m_from_field = es_col_id-fieldname.
    ASSIGN lo_tab_from->mr_table->* TO <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <tab> TO  FIELD-SYMBOL(<f_field>).
    CHECK lo_sel_to IS NOT INITIAL.
    lo_sel_to->set_value( i_field = m_to_field i_low = <f_field>  ).
    lo_sel_to->raise_selection_done( ).
  ENDMETHOD.

  METHOD  update.
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    IF <to>-range[] = e_row-range[].
      DATA(l_updated) = abap_true."so as not to have an infinite event loop
    ENDIF.
    MOVE-CORRESPONDING e_row TO <to>.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = e_row ).
    ENDIF.
    lo_sel_to->raise_selection_done( ).
  ENDMETHOD.

  METHOD update_col.
    DATA: lt_sel_row   TYPE zcl_sde_appl=>t_sel_row.
    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE any.

    CHECK lo_sel_to IS NOT INITIAL.
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    DATA(lt_old_range) = <to>-range.
    CLEAR: <to>-sign, <to>-opti, <to>-low, <to>-high, <to>-range.
    ASSIGN lo_tab_from->mr_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT e_column OF STRUCTURE <row> TO <field>.
      IF line_exists( <to>-range[ low = <field> ] ).
        APPEND VALUE #( sign = 'I' opti = 'EQ' low = <field> ) TO <to>-range.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0." empty column
      APPEND VALUE #( sign = 'I' opti = 'EQ' low = '' ) TO <to>-range.
    ENDIF.

    LOOP AT <to>-range ASSIGNING FIELD-SYMBOL(<sel>).
      <to>-low = <sel>-low.
      lo_sel_to->update_sel_row( CHANGING c_sel_row = <to> ).
      EXIT.
    ENDLOOP.

    MOVE-CORRESPONDING <to> TO lt_sel_row.
    IF <to>-range = lt_old_range.
      DATA(l_updated) = abap_true."so as not to have an infinite event loop
    ENDIF.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = lt_sel_row ).
      lo_sel_to->raise_selection_done( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
