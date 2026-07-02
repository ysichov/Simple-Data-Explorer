CLASS zcl_sde_plugins DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_field_links,
             tab        TYPE tablename,
             field      TYPE fieldname,
             method(30),
             rtab       TYPE tablename,
             rfield     TYPE fieldname,
             const      TYPE string, "aqadh_range_value,
           END OF t_field_links,
           BEGIN OF t_el_links,
             element TYPE tablename,
             rtab    TYPE tablename,
             rfield  TYPE fieldname,
             plugin  TYPE tcode,
           END OF t_el_links.

    CLASS-DATA: mt_field_links TYPE  TABLE OF t_field_links,
                mt_el_links    TYPE  TABLE OF t_el_links.
    CLASS-METHODS: init,
      link IMPORTING i_str     TYPE any
                     io_viewer TYPE REF TO zcl_sde_table_viewer
                     i_column  TYPE any,
      run_pa20 IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_pp01 IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_text IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_subty IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_py_cluster IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer,
      run_dictionary_key IMPORTING i_str     TYPE any
                                   io_viewer TYPE REF TO zcl_sde_table_viewer
                                   i_column  TYPE any,
      run_data_element IMPORTING i_str          TYPE any
                                 io_viewer      TYPE REF TO zcl_sde_table_viewer
                                 i_column       TYPE any
                       RETURNING VALUE(is_done) TYPE boolean,
      run_field_2_field IMPORTING i_str          TYPE any
                                  io_viewer      TYPE REF TO zcl_sde_table_viewer
                                  i_column       TYPE any
                        RETURNING VALUE(is_done) TYPE boolean,
      run_field_2_plugin IMPORTING "i_str          TYPE any
                                   io_viewer      TYPE REF TO zcl_sde_table_viewer
                                   i_column       TYPE any
                         RETURNING VALUE(is_done) TYPE boolean,

      run_hrp1001_adatanr IMPORTING i_str          TYPE any
                                    io_viewer      TYPE REF TO zcl_sde_table_viewer
                                    i_column       TYPE any
                          RETURNING VALUE(is_done) TYPE boolean,
      run_hrpy_rgdir IMPORTING i_str          TYPE any.
ENDCLASS.

CLASS zcl_sde_plugins IMPLEMENTATION.
  METHOD init.
    "data elements links
    mt_el_links = VALUE #(
      ( element = 'PERSNO'   plugin = 'PA20' )
      ( element = 'HROBJID'  plugin = 'PP01' )
      ( element = 'LGART'    rtab = 'T512W'   rfield = 'LGART' )
      ( element = 'ITXEX'    plugin = 'SHOW_TEXT' )
      ( element = 'SUBTY'    plugin = 'SUBTY' ) ).

    "field to field links
    mt_field_links = VALUE #(
      ( tab = 'PA0001'     field = 'PLANS' rtab = 'HRP1000' rfield = 'OTYPE' const = 'S' )
      ( tab = 'PA0001'     field = 'PLANS' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'PA0001'     field = 'ORGEH' rtab = 'HRP1000' rfield = 'OTYPE' const = 'O' )
      ( tab = 'PA0001'     field = 'ORGEH' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'PA0001'     field = 'STELL' rtab = 'HRP1000' rfield = 'OBJID' const = 'C' )
      ( tab = 'PA0001'     field = 'STELL' rtab = 'HRP1000' rfield = 'OBJID' )
      ( tab = 'HRP1001'    field = 'SCLAS' rfield = 'OTYPE' )
      ( tab = 'HRP1001'    field = 'SOBID' rfield = 'OBJID' )
      ( tab = 'HRP1002'    field = 'TABNR' rtab = 'HRT1002'  rfield = 'TABNR' )
      ( tab = 'HRP1035'    field = 'TABNR' rtab = 'HRT1035'  rfield = 'TABNR' )
      ( tab = 'HRP1222'    field = 'TABNR' rtab = 'HRT1222'  rfield = 'TABNR' )
      ( tab = 'PA2006'     field = 'QUONR' rtab = 'PTQUODED' rfield = 'QUONR' )
      ( tab = 'PTQUODED'   field = 'QUONR' rtab = 'PA2006'   rfield = 'QUONR' )
      ( tab = 'PTQUODED'   field = 'DOCNR' rtab = 'PA2001'   rfield = 'DOCNR' )
      ( tab = 'HRPY_RGDIR' field = 'SEQNR' method = 'RUN_PY_CLUSTER' ) ).
  ENDMETHOD.

  METHOD link.
    CHECK run_field_2_field( EXPORTING io_viewer = io_viewer i_column = i_column i_str = i_str ) = abap_false.
    CHECK run_field_2_plugin( EXPORTING io_viewer = io_viewer i_column = i_column ) = abap_false.

    CHECK run_data_element(  EXPORTING io_viewer = io_viewer i_column = i_column i_str = i_str ) = abap_false.
    CHECK run_hrp1001_adatanr( EXPORTING io_viewer = io_viewer i_column = i_column i_str = i_str ) = abap_false.
    run_dictionary_key( EXPORTING io_viewer = io_viewer i_column = i_column i_str = i_str ).

    LOOP AT zcl_sde_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS NOT INITIAL.
      CASE ls_link-method.
        WHEN 'RUN_PY_CLUSTER'.
          ASSIGN COMPONENT 'PERNR' OF STRUCTURE i_str TO FIELD-SYMBOL(<pernr>).
          ASSIGN COMPONENT ls_link-field OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_pa20.
    DATA: l_infty    TYPE infty,
          l_temp(10) TYPE c.

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).

    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<tab>).

    SELECT SINGLE infty INTO l_infty FROM t777d WHERE dbtab = io_viewer->m_tabname.
    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <tab> TO FIELD-SYMBOL(<field>).
    l_temp = <field>.
    SET PARAMETER ID 'PER' FIELD l_temp.
    SET PARAMETER ID 'FCD' FIELD 'DIS'.
    SET PARAMETER ID 'ITP' FIELD l_infty.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <tab> TO <field>.
    l_temp = <field>.
    SET PARAMETER ID 'SUB' FIELD l_temp.
    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <tab> TO <field>.
    l_temp = <field>.
    SET PARAMETER ID 'BEG' FIELD <field>.
    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <tab> TO <field>.
    l_temp = <field>.
    SET PARAMETER ID 'END' FIELD <field>.
    CALL TRANSACTION 'PA20' AND SKIP FIRST SCREEN.
  ENDMETHOD.

  METHOD run_pp01.
    DATA: it_bdcdata    TYPE TABLE OF  bdcdata,
          save_plvar(2),
          save_otype(2),
          save_objid(8),
          l_infty(4),
          l_subty(4),
          l_temp(10).

    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).

    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<tab>).

    SELECT SINGLE infty INTO l_infty FROM t777d WHERE dbtab = io_viewer->m_tabname.

    GET PARAMETER ID 'POP' FIELD save_plvar.
    GET PARAMETER ID 'POT' FIELD save_otype.
    GET PARAMETER ID 'PON' FIELD save_objid.

    ASSIGN COMPONENT 'PLVAR' OF STRUCTURE <tab> TO FIELD-SYMBOL(<plvar>).
    SET PARAMETER ID 'POP' FIELD <plvar>.                   "RITPP01

    ASSIGN COMPONENT 'OBJID' OF STRUCTURE <tab> TO FIELD-SYMBOL(<field>).
    l_temp = <field>.
    SET PARAMETER ID 'PON' FIELD l_temp.

    ASSIGN COMPONENT 'OTYPE' OF STRUCTURE <tab> TO <field>.
    l_temp = <field>.
    SET PARAMETER ID 'POT' FIELD l_temp.

    ASSIGN COMPONENT 'ISTAT' OF STRUCTURE <tab> TO FIELD-SYMBOL(<istat>).
    l_temp = <istat>.

    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <tab> TO <field>.
    IF sy-subrc = 0.
      l_subty = <field>.
    ELSE.
      CLEAR l_subty.
    ENDIF.

    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <tab> TO FIELD-SYMBOL(<date>).
    SET PARAMETER ID 'BEG' FIELD <date>.

    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <tab> TO <date>.
    SET PARAMETER ID 'END' FIELD <date>.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <tab> TO <field>.

    it_bdcdata = VALUE #(
      ( program = 'SAPMH5A0' dynpro = '1000' dynbegin = abap_true )
      ( fnam = 'PPHDR-INFTY' fval = l_infty )
      ( fnam = 'PPHDR-SUBTY' fval = l_subty )
      ( fnam = 'PPHDR-ISTAT' fval = l_temp )
      ( fnam = 'BDC_OKCODE' fval = 'DISP' )
      ).
    CALL TRANSACTION 'PP02' USING it_bdcdata MODE 'E'.
  ENDMETHOD.

  METHOD run_text.
    NEW zcl_sde_text_viewer( io_viewer ).
  ENDMETHOD.

  METHOD run_hrpy_rgdir.
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = 'HRPY_RGDIR'.
    ASSIGN COMPONENT 'PERNR' OF STRUCTURE i_str TO FIELD-SYMBOL(<pernr>).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'PERNR' i_low = <pernr>  ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.

  METHOD run_hrp1001_adatanr.
    IF i_column = 'ADATANR' AND io_viewer->m_tabname = 'HRP1001'.
      ASSIGN COMPONENT 'ADATANR' OF STRUCTURE i_str TO FIELD-SYMBOL(<datanr>).
      ASSIGN COMPONENT 'RELAT' OF STRUCTURE i_str TO FIELD-SYMBOL(<relat>).
      SELECT SINGLE pasub INTO @DATA(lv_struc) FROM t77ar WHERE relat = @<relat>.
      SELECT SINGLE dbtab INTO @DATA(lv_dbtab) FROM t77ad WHERE pasub = @lv_struc.

      IF sy-subrc = 0.
        APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
        CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = lv_dbtab.
        <obj>-alv_viewer->mo_sel->set_value( i_field = 'ADATANR' i_low = <datanr>  ).
        <obj>-alv_viewer->mo_sel->raise_selection_done( ).
        is_done = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD run_subty.
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).
    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<str>).

    SELECT SINGLE stypt, namst INTO @DATA(l_result)   FROM t777d WHERE dbtab = @io_viewer->m_tabname.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <str> TO FIELD-SYMBOL(<subty>).
    DATA(l_infty) = io_viewer->m_tabname+2(4).
    APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_result-stypt.
    <obj>-alv_viewer->mo_sel->set_value( i_field = l_result-namst i_low = <subty> ).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'SUBTY' i_low = <subty> ).
    <obj>-alv_viewer->mo_sel->set_value( i_field = 'INFTY' i_low = l_infty ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.

  METHOD run_py_cluster.
    FIELD-SYMBOLS: <f_tab> TYPE STANDARD  TABLE.
    DATA(l_row) = Zcl_SDE_common=>get_selected( io_viewer->mo_alv ).
    ASSIGN io_viewer->mr_table->* TO  <f_tab>.
    READ TABLE <f_tab> INDEX l_row ASSIGNING FIELD-SYMBOL(<str>).

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <str> TO FIELD-SYMBOL(<pernr>).
    ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <str> TO FIELD-SYMBOL(<seqnr>).
    NEW zcl_sde_py_cluster_viewer( i_pernr = <pernr> i_seqnr = <seqnr> ).
  ENDMETHOD.

  METHOD run_field_2_plugin.
    LOOP AT zcl_sde_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS NOT INITIAL.
      CASE ls_link-method.
        WHEN 'RUN_PY_CLUSTER'.
          run_py_cluster( io_viewer ).
      ENDCASE.
      is_done = 'X'.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_field_2_field.
    DATA: lo_viewer TYPE REF TO zcl_sde_table_viewer.
    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    LOOP AT zcl_sde_plugins=>mt_field_links INTO DATA(ls_link) WHERE tab = io_viewer->m_tabname AND field = i_column AND method IS INITIAL.
      ASSIGN COMPONENT ls_link-field OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
      IF lo_viewer IS INITIAL.
        IF ls_link-rtab IS INITIAL.
          lo_viewer = io_viewer.
        ELSE.
          APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
          CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = ls_link-rtab.
          lo_viewer = <obj>-alv_viewer.
        ENDIF.
      ENDIF.
      lo_viewer->mo_sel->set_value( i_field = ls_link-rfield i_low = COND aqadh_type_of_icon( WHEN ls_link-const IS INITIAL THEN <field> ELSE ls_link-const ) ).
    ENDLOOP.
    IF sy-subrc = 0.
      lo_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = io_viewer->m_lang  ).
      lo_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      lo_viewer->mo_sel->raise_selection_done( ).
      is_done = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD run_data_element.
    DATA: lo_viewer TYPE REF TO zcl_sde_table_viewer.

    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    READ TABLE Zcl_SDE_common=>mt_tabfields WITH KEY tabname = io_viewer->m_tabname fieldname = i_column INTO DATA(l_field).
    LOOP AT zcl_sde_plugins=>mt_el_links INTO DATA(l_el_link) WHERE element = l_field-rollname AND plugin NE '' ." 'PA20' .
      CASE l_el_link-plugin.
        WHEN 'PA20'.
          zcl_sde_plugins=>run_pa20( io_viewer ).
        WHEN 'PP01'.
          zcl_sde_plugins=>run_pp01( io_viewer ).
        WHEN 'SHOW_TEXT'.
          zcl_sde_plugins=>run_text( io_viewer ).
        WHEN 'SUBTY'.
          zcl_sde_plugins=>run_subty( io_viewer ).
      ENDCASE.
      is_done = abap_true.
    ENDLOOP.
    IF is_done = abap_true.
      RETURN.
    ENDIF.

    LOOP AT zcl_sde_plugins=>mt_el_links INTO l_el_link WHERE element = l_field-rollname .
      IF lo_viewer IS INITIAL.
        APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
        CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = l_el_link-rtab.
        lo_viewer = <obj>-alv_viewer.
      ENDIF.
      ASSIGN COMPONENT i_column OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
      lo_viewer->mo_sel->set_value( i_field = l_el_link-rfield i_low = <field>  ).
    ENDLOOP.
    IF sy-subrc = 0.
      lo_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = io_viewer->m_lang  ).
      lo_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      lo_viewer->mo_sel->raise_selection_done( ).
      is_done = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD run_dictionary_key.
    DATA: lt_keys TYPE TABLE OF dd05p.

    GET PARAMETER ID 'MOL' FIELD DATA(l_mol).
    READ TABLE Zcl_SDE_common=>mt_tabfields INTO DATA(field) WITH KEY tabname = io_viewer->m_tabname fieldname = i_column .
    ASSIGN COMPONENT i_column OF STRUCTURE i_str TO FIELD-SYMBOL(<field>).
    CHECK sy-subrc = 0.
    CHECK <field> IS NOT INITIAL.
    CALL FUNCTION 'DD_FORKEY_GET'
      EXPORTING
        feldname  = CONV fieldname( i_column )
        tabname   = io_viewer->m_tabname
      TABLES
        forkeytab = lt_keys
      EXCEPTIONS
        not_equal = 1
        not_found = 2
        not_valid = 3
        OTHERS    = 4.

    IF sy-subrc < 2.
      APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
      CREATE OBJECT <obj>-alv_viewer EXPORTING i_tname = field-checktable.
      LOOP AT lt_keys INTO DATA(l_keys).
        ASSIGN COMPONENT l_keys-forkey OF STRUCTURE i_str TO <field>.
        CHECK sy-subrc = 0.
        <obj>-alv_viewer->mo_sel->set_value( i_field = l_keys-checkfield i_low = <field>  ).
      ENDLOOP.
      <obj>-alv_viewer->mo_sel->set_value( i_field = 'SPRSL' i_low = io_viewer->m_lang  ).
      <obj>-alv_viewer->mo_sel->set_value( i_field = 'MOLGA' i_low = l_mol  ).
      <obj>-alv_viewer->mo_sel->raise_selection_done( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
