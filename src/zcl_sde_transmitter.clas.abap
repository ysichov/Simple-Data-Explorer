CLASS zcl_sde_transmitter DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    EVENTS: data_changed EXPORTING VALUE(e_row) TYPE zcl_sde_appl=>t_sel_row,
      col_changed EXPORTING VALUE(e_column) TYPE lvc_fname.
    METHODS: emit IMPORTING e_row TYPE zcl_sde_appl=>t_sel_row,
      emit_col IMPORTING e_column TYPE lvc_fname.
ENDCLASS.

CLASS zcl_sde_transmitter IMPLEMENTATION.
  METHOD  emit.
    RAISE EVENT data_changed EXPORTING e_row = e_row.
  ENDMETHOD.

  METHOD emit_col.
    RAISE EVENT col_changed EXPORTING e_column = e_column.
  ENDMETHOD.
ENDCLASS.
