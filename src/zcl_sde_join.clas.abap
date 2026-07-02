CLASS zcl_sde_join DEFINITION PUBLIC INHERITING FROM zcl_sde_popup CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_pair,
             cand_field TYPE fieldname,
             base_field TYPE fieldname,
           END OF t_pair,
           tt_pairs TYPE STANDARD TABLE OF t_pair WITH DEFAULT KEY,

           BEGIN OF t_cand,
             tabname   TYPE tabname,
             ddtext    TYPE as4text,
             direction TYPE char1, "O-outgoing FK, I-incoming FK, T-text table, M-manual
             pairs     TYPE tt_pairs,
             selected  TYPE abap_bool,
             sel_order TYPE i,
           END OF t_cand,
           tt_cand TYPE STANDARD TABLE OF t_cand WITH DEFAULT KEY,

           BEGIN OF t_jtab,
             alias   TYPE char5,
             tabname TYPE tabname,
             ddtext  TYPE as4text,
             jtype   TYPE char10,          "INNER / LEFT OUTER
             cond    TYPE c LENGTH 255,    "ON condition, editable
           END OF t_jtab,
           tt_jtab TYPE STANDARD TABLE OF t_jtab WITH DEFAULT KEY,

           BEGIN OF t_jfld,
             sel       TYPE abap_bool,
             pos       TYPE i, "order in the SELECT list (only when sel = X)
             alias     TYPE char5,
             tabname   TYPE tabname,
             fieldname TYPE fieldname,
             keyflag   TYPE keyflag,
             ddtext    TYPE reptext,
           END OF t_jfld,
           tt_jfld TYPE STANDARD TABLE OF t_jfld WITH DEFAULT KEY.

    METHODS: constructor IMPORTING io_viewer TYPE REF TO zcl_sde_table_viewer.

protected section.
  PRIVATE SECTION.
    DATA: mo_viewer    TYPE REF TO zcl_sde_table_viewer,
          m_tabname    TYPE tabname,
          mt_cand      TYPE tt_cand,
          mt_jtabs     TYPE tt_jtab,
          mt_jflds     TYPE tt_jfld,
          m_sel_count  TYPE i,
          m_base_pos   TYPE i VALUE 1, "position of the base table in the join order (1 = FROM)
          mo_html      TYPE REF TO cl_gui_html_viewer,
          mo_low_split TYPE REF TO cl_gui_splitter_container,
          mo_flds_html TYPE REF TO cl_gui_html_viewer,
          mo_sql_text  TYPE REF TO cl_gui_textedit,
          mo_result    TYPE REF TO zcl_sde_table_viewer, "result window, rebuilt in place
          m_auto       TYPE abap_bool VALUE abap_true,   "auto-refresh the result on changes
          m_show_texts TYPE abap_bool,                   "field chips: texts instead of tech names
          m_fld_lang   TYPE spras.                       "language of the field texts

    METHODS:
      find_candidates,
      get_fieldlist IMPORTING i_tabname       TYPE tabname
                    RETURNING VALUE(rt_dfies) TYPE ddfields,
      render_html,
      render_flds,
      show_html IMPORTING io_html TYPE REF TO cl_gui_html_viewer i_html TYPE string,
      add_candidate IMPORTING i_tabname TYPE tabname,
      toggle_candidate IMPORTING i_tabname TYPE tabname,
      rebuild_selection,
      normalize_pos,
      move_table IMPORTING i_alias TYPE char5 i_dir TYPE i,
      move_table_before IMPORTING i_from TYPE string i_to TYPE string,
      move_field_before IMPORTING i_from TYPE string i_to TYPE string,
      handle_fld_action IMPORTING i_act TYPE string,
      reload_field_texts,
      apply_postdata IMPORTING it_postdata TYPE cnht_post_data_tab RETURNING VALUE(rv_act) TYPE string,
      create_sql_view,
      update_sql_view,
      refresh_all,
      generate_select RETURNING VALUE(rv_sql) TYPE string,
      run_select,
      execute_sql IMPORTING i_sql TYPE string i_quiet TYPE abap_bool DEFAULT abap_false,
      result_alive RETURNING VALUE(rv_alive) TYPE abap_bool,
      upper_outside_quotes IMPORTING i_sql         TYPE string
                           RETURNING VALUE(rv_sql) TYPE string,

      on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
        IMPORTING action getdata postdata.
ENDCLASS.



CLASS ZCL_SDE_JOIN IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_viewer = io_viewer.
    m_tabname = io_viewer->m_tabname.
    m_fld_lang = sy-langu.

    mo_box = create( i_width = 1000 i_hight = 400 ).
    mo_box->set_caption( |Join builder: { m_tabname }| ).
    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_splitter ##FM_SUBRC_OK
      EXPORTING
        parent  = mo_box
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.
    mo_splitter->set_row_mode( mode = mo_splitter->mode_relative ).
    mo_splitter->set_row_height( id = 1 height = 45 ).

    mo_splitter->get_container( EXPORTING row = 1 column = 1 RECEIVING container = DATA(lo_top) ).
    mo_splitter->get_container( EXPORTING row = 2 column = 1 RECEIVING container = DATA(lo_bottom) ).

    CREATE OBJECT mo_low_split ##FM_SUBRC_OK
      EXPORTING
        parent  = lo_bottom
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    CREATE OBJECT mo_html
      EXPORTING
        parent             = lo_top
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    IF sy-subrc = 0.
      mo_html->set_registered_events( VALUE #( ( eventid = cl_gui_html_viewer=>m_id_sapevent ) ) ).
      SET HANDLER on_sapevent FOR mo_html.
    ENDIF.

    mo_low_split->set_column_width( id = 1 width = 45 ).
    mo_low_split->get_container( EXPORTING row = 1 column = 2 RECEIVING container = DATA(lo_right) ).
    CREATE OBJECT mo_flds_html
      EXPORTING
        parent = lo_right
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc = 0.
      mo_flds_html->set_registered_events( VALUE #( ( eventid = cl_gui_html_viewer=>m_id_sapevent ) ) ).
      SET HANDLER on_sapevent FOR mo_flds_html.
    ENDIF.

    find_candidates( ).
    rebuild_selection( ).
    create_sql_view( ).
    refresh_all( ).
  ENDMETHOD.


  METHOD get_fieldlist.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = i_tabname
        langu          = COND spras( WHEN m_fld_lang IS NOT INITIAL THEN m_fld_lang ELSE sy-langu )
      TABLES
        dfies_tab      = rt_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      CLEAR rt_dfies.
    ENDIF.
    DELETE rt_dfies WHERE datatype = 'CLNT' OR fieldname CP '.*'.
  ENDMETHOD.


  METHOD find_candidates.
    DATA: lt_dd08l TYPE TABLE OF dd08l,
          lt_keys  TYPE TABLE OF dd05p. "resolved field pairs incl. checkfield

    "Outgoing: foreign keys defined on the base table -> check tables
    SELECT * FROM dd08l INTO TABLE lt_dd08l
      WHERE tabname = m_tabname
        AND as4local = 'A'.                             "#EC CI_GENBUFF
    LOOP AT lt_dd08l INTO DATA(ls_fk) WHERE checktable IS NOT INITIAL
                                        AND checktable NE m_tabname
                                        AND checktable NE '*'.
      CLEAR lt_keys.
      CALL FUNCTION 'DD_FORKEY_GET'
        EXPORTING
          feldname  = ls_fk-fieldname
          tabname   = m_tabname
        TABLES
          forkeytab = lt_keys
        EXCEPTIONS
          OTHERS    = 4.
      CHECK sy-subrc < 2.
      READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = ls_fk-checktable.
      IF sy-subrc NE 0.
        APPEND VALUE #( tabname = ls_fk-checktable direction = 'O' ) TO mt_cand ASSIGNING <cand>.
      ENDIF.
      LOOP AT lt_keys INTO DATA(ls_key) WHERE fortable = m_tabname
                                          AND checkfield NE 'MANDT'
                                          AND checkfield IS NOT INITIAL.
        IF NOT line_exists( <cand>-pairs[ cand_field = ls_key-checkfield ] ).
          APPEND VALUE #( cand_field = ls_key-checkfield base_field = ls_key-forkey ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    "Incoming: tables having the base table as check table
    CLEAR lt_dd08l.
    SELECT * FROM dd08l INTO TABLE lt_dd08l UP TO 100 ROWS
      WHERE checktable = m_tabname
        AND as4local = 'A'.                             "#EC CI_GENBUFF
    LOOP AT lt_dd08l INTO ls_fk WHERE tabname NE m_tabname.
      IF NOT line_exists( mt_cand[ tabname = ls_fk-tabname ] ) AND lines( mt_cand ) >= 30.
        CONTINUE. "keep the canvas readable
      ENDIF.
      CLEAR lt_keys.
      CALL FUNCTION 'DD_FORKEY_GET'
        EXPORTING
          feldname  = ls_fk-fieldname
          tabname   = ls_fk-tabname
        TABLES
          forkeytab = lt_keys
        EXCEPTIONS
          OTHERS    = 4.
      CHECK sy-subrc < 2.
      READ TABLE mt_cand ASSIGNING <cand> WITH KEY tabname = ls_fk-tabname.
      IF sy-subrc NE 0.
        APPEND VALUE #( tabname = ls_fk-tabname direction = 'I' ) TO mt_cand ASSIGNING <cand>.
      ENDIF.
      LOOP AT lt_keys INTO ls_key WHERE fortable = ls_fk-tabname
                                    AND checkfield NE 'MANDT'
                                    AND checkfield IS NOT INITIAL.
        IF NOT line_exists( <cand>-pairs[ cand_field = ls_key-forkey ] ).
          APPEND VALUE #( cand_field = ls_key-forkey base_field = ls_key-checkfield ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    "Keep only real database tables (drop structures and generated maintenance views)
    IF mt_cand IS NOT INITIAL.
      SELECT tabname FROM dd02l
        INTO TABLE @DATA(lt_real)
        FOR ALL ENTRIES IN @mt_cand
        WHERE tabname = @mt_cand-tabname
          AND tabclass IN ('TRANSP','POOL','CLUSTER').
      LOOP AT mt_cand ASSIGNING <cand>.
        IF NOT line_exists( lt_real[ table_line = <cand>-tabname ] ).
          DELETE mt_cand.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Text table
    zcl_sde_ddic=>get_text_table( EXPORTING i_tname = m_tabname IMPORTING e_tab = DATA(l_texttab) ).
    IF l_texttab IS NOT INITIAL AND NOT line_exists( mt_cand[ tabname = l_texttab ] ).
      APPEND VALUE #( tabname = l_texttab direction = 'T' ) TO mt_cand ASSIGNING <cand>.
      DATA(lt_base_keys) = get_fieldlist( m_tabname ).
      LOOP AT get_fieldlist( l_texttab ) INTO DATA(ls_tf) WHERE keyflag = abap_true.
        IF line_exists( lt_base_keys[ fieldname = ls_tf-fieldname keyflag = abap_true ] ).
          APPEND VALUE #( cand_field = ls_tf-fieldname base_field = ls_tf-fieldname ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Descriptions
    IF mt_cand IS NOT INITIAL.
      SELECT tabname, ddtext FROM dd02t
        INTO TABLE @DATA(lt_texts)
        FOR ALL ENTRIES IN @mt_cand
        WHERE tabname = @mt_cand-tabname
          AND ddlanguage = @sy-langu.
      LOOP AT mt_cand ASSIGNING <cand>.
        READ TABLE lt_texts INTO DATA(ls_text) WITH KEY tabname = <cand>-tabname.
        IF sy-subrc = 0.
          <cand>-ddtext = ls_text-ddtext.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD add_candidate.
    DATA(l_tabname) = i_tabname.
    TRANSLATE l_tabname TO UPPER CASE.
    CONDENSE l_tabname.
    CHECK l_tabname IS NOT INITIAL AND l_tabname NE m_tabname.

    IF zcl_sde_sql=>exist_table( l_tabname ) NE 1 AND zcl_sde_sql=>exist_view( l_tabname ) NE 1.
      MESSAGE |Table { l_tabname } does not exist| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = l_tabname.
    IF sy-subrc NE 0.
      APPEND VALUE #( tabname = l_tabname direction = 'M' ) TO mt_cand ASSIGNING <cand>.
      SELECT SINGLE ddtext FROM dd02t INTO <cand>-ddtext
        WHERE tabname = l_tabname AND ddlanguage = sy-langu.
      "propose ON condition by matching key field names
      DATA(lt_base) = get_fieldlist( m_tabname ).
      LOOP AT get_fieldlist( l_tabname ) INTO DATA(ls_f) WHERE keyflag = abap_true.
        IF line_exists( lt_base[ fieldname = ls_f-fieldname ] ).
          APPEND VALUE #( cand_field = ls_f-fieldname base_field = ls_f-fieldname ) TO <cand>-pairs.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF <cand>-selected = abap_false.
      toggle_candidate( l_tabname ).
    ENDIF.
  ENDMETHOD.


  METHOD toggle_candidate.
    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = i_tabname.
    CHECK sy-subrc = 0.
    IF <cand>-selected = abap_true.
      <cand>-selected = abap_false.
      CLEAR <cand>-sel_order.
    ELSE.
      <cand>-selected = abap_true.
      ADD 1 TO m_sel_count.
      <cand>-sel_order = m_sel_count.
    ENDIF.
    rebuild_selection( ).
    refresh_all( ).
  ENDMETHOD.


  METHOD refresh_all.
    render_html( ).
    render_flds( ).
    update_sql_view( ).
  ENDMETHOD.


  METHOD rebuild_selection.
    DATA: lt_old_flds TYPE tt_jfld,
          lt_old_tabs TYPE tt_jtab,
          lt_sorted   TYPE tt_cand.

    lt_old_flds = mt_jflds.
    lt_old_tabs = mt_jtabs.
    CLEAR: mt_jtabs, mt_jflds.

    "base table fields always listed first
    LOOP AT get_fieldlist( m_tabname ) INTO DATA(ls_f).
      APPEND VALUE #( sel = ls_f-keyflag alias = 'T0' tabname = m_tabname
                      fieldname = ls_f-fieldname keyflag = ls_f-keyflag
                      ddtext = ls_f-fieldtext ) TO mt_jflds.
    ENDLOOP.

    lt_sorted = VALUE #( FOR wa IN mt_cand WHERE ( selected = abap_true ) ( wa ) ).
    SORT lt_sorted BY sel_order.

    "candidates: aliases T1..Tn by candidate order (stable regardless of base position)
    LOOP AT lt_sorted INTO DATA(ls_cand).
      DATA(l_alias) = |T{ sy-tabix }|.
      APPEND INITIAL LINE TO mt_jtabs ASSIGNING FIELD-SYMBOL(<jtab>).
      <jtab>-alias   = l_alias.
      <jtab>-tabname = ls_cand-tabname.
      <jtab>-ddtext  = ls_cand-ddtext.
      <jtab>-jtype   = 'LEFT OUTER'.
      DATA l_cond TYPE string. "build in a string: char field would eat trailing blanks after AND
      CLEAR l_cond.
      LOOP AT ls_cand-pairs INTO DATA(ls_pair).
        IF l_cond IS NOT INITIAL.
          l_cond = |{ l_cond } AND |.
        ENDIF.
        l_cond = |{ l_cond }{ l_alias CASE = LOWER }~{ ls_pair-cand_field } = t0~{ ls_pair-base_field }|.
      ENDLOOP.
      <jtab>-cond = l_cond.
      "keep user's edits from previous rebuild (only while the alias is unchanged,
      "otherwise the edited condition would reference a wrong alias)
      READ TABLE lt_old_tabs INTO DATA(ls_old_tab) WITH KEY tabname = ls_cand-tabname.
      IF sy-subrc = 0.
        <jtab>-jtype = ls_old_tab-jtype.
        IF ls_old_tab-cond IS NOT INITIAL AND ls_old_tab-alias = <jtab>-alias.
          <jtab>-cond = ls_old_tab-cond.
        ENDIF.
      ENDIF.

      LOOP AT get_fieldlist( ls_cand-tabname ) INTO ls_f.
        APPEND VALUE #( alias = l_alias tabname = ls_cand-tabname
                        fieldname = ls_f-fieldname keyflag = ls_f-keyflag
                        ddtext = ls_f-fieldtext
                        sel = boolc( line_exists( ls_cand-pairs[ cand_field = ls_f-fieldname ] ) )
                      ) TO mt_jflds.
      ENDLOOP.
    ENDLOOP.

    "insert the base table at its chosen position in the join order.
    "star topology: every ON references t0, so the base must be the 1st or 2nd
    "table - otherwise the DB sees t0 before it is joined ("table T0 unknown")
    IF m_base_pos < 1.
      m_base_pos = 1.
    ELSEIF m_base_pos > 2.
      m_base_pos = 2.
    ENDIF.
    IF m_base_pos > lines( mt_jtabs ) + 1.
      m_base_pos = lines( mt_jtabs ) + 1.
    ENDIF.
    DATA(ls_base) = VALUE t_jtab( alias = 'T0' tabname = m_tabname jtype = 'LEFT OUTER' ).
    READ TABLE lt_old_tabs INTO DATA(ls_old_base) WITH KEY tabname = m_tabname.
    IF sy-subrc = 0 AND ls_old_base-jtype IS NOT INITIAL.
      ls_base-jtype = ls_old_base-jtype.
    ENDIF.
    INSERT ls_base INTO mt_jtabs INDEX m_base_pos.

    "if the base is not the FROM table, the first table's link condition moves to the base row
    IF m_base_pos > 1.
      READ TABLE mt_jtabs ASSIGNING FIELD-SYMBOL(<first>) INDEX 1.
      READ TABLE mt_jtabs ASSIGNING FIELD-SYMBOL(<base>) INDEX m_base_pos.
      IF <base>-cond IS INITIAL.
        <base>-cond = <first>-cond.
      ENDIF.
      CLEAR <first>-cond. "FROM table has no ON

      "old base ON edits survive
      READ TABLE lt_old_tabs INTO ls_old_base WITH KEY tabname = m_tabname.
      IF sy-subrc = 0 AND ls_old_base-cond IS NOT INITIAL.
        <base>-cond = ls_old_base-cond.
      ENDIF.
    ENDIF.

    "restore previous manual (de)selections and the SELECT order
    LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>).
      READ TABLE lt_old_flds INTO DATA(ls_old) WITH KEY tabname = <fld>-tabname fieldname = <fld>-fieldname.
      IF sy-subrc = 0.
        <fld>-sel = ls_old-sel.
        <fld>-pos = ls_old-pos.
      ENDIF.
    ENDLOOP.
    normalize_pos( ).
  ENDMETHOD.


  METHOD normalize_pos.
    DATA l_max TYPE i.
    LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) WHERE sel = abap_true AND pos > 0.
      IF <fld>-pos > l_max.
        l_max = <fld>-pos.
      ENDIF.
    ENDLOOP.
    LOOP AT mt_jflds ASSIGNING <fld> WHERE sel = abap_true AND pos = 0.
      l_max = l_max + 1.
      <fld>-pos = l_max.
    ENDLOOP.
    LOOP AT mt_jflds ASSIGNING <fld> WHERE sel = abap_false AND pos NE 0.
      CLEAR <fld>-pos.
    ENDLOOP.
  ENDMETHOD.


  METHOD render_html.
    CHECK mo_html IS BOUND.

    DATA(l_html) =
      `<html><head><meta charset="utf-8"><style>` &&
      `body{font-family:Arial,sans-serif;font-size:11px;margin:4px;background:#f6f8fa;}` &&
      `.base{display:inline-block;background:#2c5f8a;color:#fff;border-radius:6px;padding:6px 12px;margin:3px;font-weight:bold;}` &&
      `.card{display:inline-block;border:1px solid #aaa;border-radius:6px;padding:5px 10px;margin:3px;background:#fff;text-decoration:none;color:#000;}` &&
      `.card:hover{border-color:#2c5f8a;}` &&
      `.sel{background:#d3f2d3;border:2px solid #2e8b2e;font-weight:bold;}` &&
      `.dir{color:#888;font-size:9px;}` &&
      `form{display:inline-block;margin:3px;}` &&
      `table.j{border-collapse:collapse;margin-top:6px;background:#fff;border:1px solid #ddd;border-radius:4px;}` &&
      `table.j td{padding:2px 5px;vertical-align:middle;white-space:nowrap;}` &&
      `.alias{font-weight:bold;color:#2c5f8a;}` &&
      `.btn{border:1px solid #bbb;background:#f2f2f2;border-radius:3px;cursor:pointer;font-size:10px;padding:0 4px;}` &&
      `.grip{cursor:move;color:#888;font-weight:bold;}` &&
      `input.cond{width:380px;font-family:Consolas,monospace;font-size:11px;}` &&
      `select{font-size:11px;}` &&
      `</style>` &&
      `<script>var dk=null,lt=null;` &&
      `function ds(e,k){dk=k;try{e.dataTransfer.setData('text',k);}catch(x){}}` &&
      "highlight the element the dragged one will be inserted BEFORE
      `function uh(){if(lt){lt.style.boxShadow='';lt=null;}}` &&
      `function ov(e,el){if(e.preventDefault)e.preventDefault();` &&
      `if(el&&el!==lt){uh();lt=el;el.style.boxShadow='-4px 0 0 0 #d2691e';}return false;}` &&
      `function dp(e,k,p){if(e.preventDefault)e.preventDefault();uh();` &&
      `if(dk&&dk!=k){window.location.href='SAPEVENT:'+p+'?'+dk+'__'+k;}dk=null;return false;}` &&
      `document.ondragend=function(){uh();};` &&
      `</script>` &&
      `</head><body>` &&
      |<span class="base">{ m_tabname }</span> &#8646; |.

    LOOP AT mt_cand INTO DATA(ls_cand).
      DATA(l_class) = COND string( WHEN ls_cand-selected = abap_true THEN 'card sel' ELSE 'card' ).
      DATA(l_dir) = SWITCH string( ls_cand-direction
                      WHEN 'O' THEN '&#8594;'   "outgoing
                      WHEN 'I' THEN '&#8592;'   "incoming
                      WHEN 'T' THEN 'TXT'
                      ELSE '+' ).
      l_html = l_html &&
        |<a class="{ l_class }" href="SAPEVENT:toggle?{ ls_cand-tabname }">| &&
        |{ ls_cand-tabname } <span class="dir">{ l_dir } { escape( val = ls_cand-ddtext format = cl_abap_format=>e_html_text ) }</span></a>|.
    ENDLOOP.

    l_html = l_html &&
      `<form method="post" action="SAPEVENT:addtab">` &&
      `<input type="text" name="newtab" size="16" maxlength="30">` &&
      `<input type="submit" value="Add table"></form>`.

    "joined tables: order (the first one is FROM), join type, ON condition
    IF lines( mt_jtabs ) > 1.
      l_html = l_html && `<form method="post" action="SAPEVENT:tabs"><table class="j">`.
      LOOP AT mt_jtabs INTO DATA(ls_tab).
        DATA(l_idx) = sy-tabix.
        DATA(l_key) = condense( CONV string( ls_tab-alias ) ).
        DATA(l_del) = COND string( WHEN ls_tab-tabname NE m_tabname
          THEN |<button class="btn" type="submit" name="act" value="del_{ l_key }">&#10005;</button>| ).
        l_html = l_html &&
          |<tr draggable="true" ondragstart="ds(event,'{ l_key }')" ondragover="return ov(event,this)"| &&
          | ondrop="return dp(event,'{ l_key }','tmv')">| &&
          |<td><span class="grip">&#8801;</span> | &&
          |<button class="btn" type="submit" name="act" value="up_{ l_key }">&#9650;</button>| &&
          |<button class="btn" type="submit" name="act" value="dn_{ l_key }">&#9660;</button>{ l_del }</td>| &&
          |<td class="alias">{ l_key }</td>| &&
          |<td><b>{ ls_tab-tabname }</b></td>|.
        IF l_idx = 1.
          l_html = l_html && |<td>FROM</td><td></td></tr>|.
        ELSE.
          DATA(l_inner) = COND string( WHEN ls_tab-jtype = 'INNER' THEN ' selected' ).
          DATA(l_left)  = COND string( WHEN ls_tab-jtype NE 'INNER' THEN ' selected' ).
          l_html = l_html &&
            |<td><select name="jt_{ l_key }">| &&
            |<option{ l_inner }>INNER</option><option{ l_left }>LEFT OUTER</option></select> JOIN ON</td>| &&
            |<td><input class="cond" type="text" name="on_{ l_key }" value="{
               escape( val = CONV string( ls_tab-cond ) format = cl_abap_format=>e_html_attr ) }"></td></tr>|.
        ENDIF.
      ENDLOOP.
      l_html = l_html &&
        `</table><button class="btn" type="submit" name="act" value="apply">Apply</button></form>`.
    ENDIF.

    l_html = l_html && `</body></html>`.
    show_html( io_html = mo_html i_html = l_html ).
  ENDMETHOD.


  METHOD render_flds.
    DATA: lt_sel TYPE tt_jfld.

    CHECK mo_flds_html IS BOUND.

    DATA(l_html) =
      `<html><head><meta charset="utf-8"><style>` &&
      `body{font-family:Arial,sans-serif;font-size:11px;margin:4px;background:#fff;}` &&
      `h4{margin:4px 0 2px 0;font-size:11px;color:#555;}` &&
      `.tabhdr{margin:6px 0 2px 0;font-weight:bold;color:#2c5f8a;}` &&
      `.chip{display:inline-block;border:1px solid #bbb;border-radius:10px;padding:1px 8px;margin:2px;` &&
      `background:#fff;text-decoration:none;color:#000;}` &&
      `.chip:hover{border-color:#2c5f8a;}` &&
      `.on{background:#d3f2d3;border-color:#2e8b2e;font-weight:bold;}` &&
      `.key{border-style:double;border-width:3px;}` &&
      `.fld{display:inline-block;border:1px solid #2e8b2e;border-radius:4px;background:#eef8ee;` &&
      `padding:1px 6px;margin:2px;font-family:Consolas,monospace;cursor:move;}` &&
      `.fld a{color:#a00;text-decoration:none;margin-left:4px;}` &&
      `.zone{display:inline-block;border:1px dashed #bbb;border-radius:4px;color:#999;padding:1px 8px;margin:2px;}` &&
      `.dir{color:#888;font-size:9px;}` &&
      `.act{color:#2c5f8a;text-decoration:none;margin-right:6px;}` &&
      `.run{display:inline-block;background:#2e8b2e;color:#fff;border-radius:4px;padding:2px 10px;` &&
      `text-decoration:none;font-weight:bold;margin-right:10px;}` &&
      `.paint{outline:2px solid #2e8b2e;}` &&
      `</style>` &&
      `<script>var dk=null,lt=null;` &&
      `function ds(e,k){dk=k;try{e.dataTransfer.setData('text',k);}catch(x){}}` &&
      "highlight the element the dragged one will be inserted BEFORE
      `function uh(){if(lt){lt.style.boxShadow='';lt=null;}}` &&
      `function ov(e,el){if(e.preventDefault)e.preventDefault();` &&
      `if(el&&el!==lt){uh();lt=el;el.style.boxShadow='-4px 0 0 0 #d2691e';}return false;}` &&
      `function dp(e,k,p){if(e.preventDefault)e.preventDefault();uh();` &&
      `if(dk&&dk!=k){window.location.href='SAPEVENT:'+p+'?'+dk+'__'+k;}dk=null;return false;}` &&
      `document.ondragend=function(){uh();};` &&
      "lasso selection: hold the left mouse button and sweep over the chips
      `var pt=false,pks={},pn=0,painted=false;` &&
      `function pd(e,el,k){pt=true;pks={};pn=0;painted=false;pa(el,k);return false;}` &&
      `function pv(e,el,k){if(pt&&!pks[k]){pa(el,k);if(pn>1)painted=true;}}` &&
      `function pa(el,k){pks[k]=1;pn++;el.className+=' paint';}` &&
      `function pc(e){if(painted){if(e.preventDefault)e.preventDefault();return false;}return true;}` &&
      `document.onmouseup=function(){if(!pt)return;pt=false;` &&
      `if(pn>1){var a=[];for(var k in pks)a.push(k);` &&
      `document.getElementById('pk').value=a.join(';');document.getElementById('pf').submit();}};` &&
      `</script>` &&
      `</head><body onselectstart="return false">` &&
      `<form id="pf" method="post" action="SAPEVENT:fsel" style="display:none">` &&
      `<input type="hidden" name="keys" id="pk"></form>` &&
      `<a class="run" href="SAPEVENT:fld?RUN">&#9654; Run</a>` &&
      `<a class="act" href="SAPEVENT:fld?ALL">select all</a>` &&
      `<a class="act" href="SAPEVENT:fld?NONE">clear</a>` &&
      `<a class="act" href="SAPEVENT:fld?KEYS">keys</a>` &&
      |<a class="act" href="SAPEVENT:fld?AUTO">auto-refresh: { COND string( WHEN m_auto = abap_true THEN 'on' ELSE 'off' ) }</a>|.

    "field name variant: technical or one of the installed languages (as in the table viewer)
    l_html = l_html &&
      `<select onchange="window.location.href='SAPEVENT:fld?lng_'+this.value">` &&
      |<option value="TECH"{ COND string( WHEN m_show_texts = abap_false THEN ' selected' ) }>Technical name</option>|.
    LOOP AT zcl_sde_appl=>mt_lang INTO DATA(ls_lang).
      DATA(l_sel_opt) = COND string( WHEN m_show_texts = abap_true AND m_fld_lang = ls_lang-spras THEN ' selected' ).
      l_html = l_html &&
        |<option value="{ ls_lang-spras }"{ l_sel_opt }>{ escape( val = ls_lang-sptxt format = cl_abap_format=>e_html_text ) }</option>|.
    ENDLOOP.
    l_html = l_html && `</select>`.

    "selected fields in SELECT order: drag to reorder, x to remove
    lt_sel = VALUE #( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
    SORT lt_sel BY pos.
    l_html = l_html && `<h4>SELECT fields (drag to reorder - drops before the marked chip):</h4>`.
    LOOP AT lt_sel INTO DATA(ls_sel).
      DATA(l_key) = |{ condense( CONV string( ls_sel-alias ) ) }~{ ls_sel-fieldname }|.
      l_html = l_html &&
        |<span class="fld" draggable="true" ondragstart="ds(event,'{ l_key }')"| &&
        | ondragover="return ov(event,this)" ondrop="return dp(event,'{ l_key }','fmv')">| &&
        |{ to_lower( l_key ) }<a href="SAPEVENT:fld?tg_{ l_key }">&#10005;</a></span>|.
    ENDLOOP.
    l_html = l_html &&
      `<span class="zone" ondragover="return ov(event,this)" ondrop="return dp(event,'#END','fmv')">&#8677; end</span>`.

    "all fields grouped by table: click chip to toggle
    LOOP AT mt_jtabs INTO DATA(ls_tab).
      DATA(l_alias) = condense( CONV string( ls_tab-alias ) ).
      l_html = l_html &&
        |<div class="tabhdr">{ l_alias } { ls_tab-tabname }&nbsp;&nbsp;| &&
        |<a class="act" href="SAPEVENT:fld?all_{ l_alias }">all</a>| &&
        |<a class="act" href="SAPEVENT:fld?non_{ l_alias }">none</a>| &&
        |<a class="act" href="SAPEVENT:fld?key_{ l_alias }">keys</a></div>|.
      LOOP AT mt_jflds INTO DATA(ls_fld) WHERE alias = ls_tab-alias.
        DATA(l_cls) = COND string( WHEN ls_fld-sel = abap_true THEN 'chip on' ELSE 'chip' ).
        IF ls_fld-keyflag = abap_true.
          l_cls = l_cls && ' key'.
        ENDIF.
        DATA(l_fkey) = |{ l_alias }~{ ls_fld-fieldname }|.
        "compact chip label: technical name or text in the logon language
        DATA(l_label) = COND string(
          WHEN m_show_texts = abap_true AND ls_fld-ddtext IS NOT INITIAL
          THEN escape( val = ls_fld-ddtext format = cl_abap_format=>e_html_text )
          ELSE |{ ls_fld-fieldname }| ).
        l_html = l_html &&
          |<a class="{ l_cls }" href="SAPEVENT:fld?tg_{ l_fkey }"| &&
          | onmousedown="return pd(event,this,'{ l_fkey }')"| &&
          | onmouseover="pv(event,this,'{ l_fkey }')"| &&
          | onclick="return pc(event)" title="{ l_fkey } { escape( val = ls_fld-ddtext format = cl_abap_format=>e_html_attr ) }">| &&
          |{ l_label }</a>|.
      ENDLOOP.
    ENDLOOP.

    l_html = l_html && `</body></html>`.
    show_html( io_html = mo_flds_html i_html = l_html ).
  ENDMETHOD.


  METHOD show_html.
    DATA: lt_html TYPE TABLE OF char255,
          l_url   TYPE char255.

    DATA(l_len) = strlen( i_html ).
    DATA(l_off) = 0.
    WHILE l_off < l_len.
      DATA(l_chunk) = nmin( val1 = 255 val2 = l_len - l_off ).
      APPEND i_html+l_off(l_chunk) TO lt_html.
      l_off = l_off + l_chunk.
    ENDWHILE.

    io_html->load_data(
      EXPORTING
        type         = 'text'
        subtype      = 'html'
      IMPORTING
        assigned_url = l_url
      CHANGING
        data_table   = lt_html
      EXCEPTIONS
        OTHERS       = 1 ).
    IF sy-subrc = 0.
      io_html->show_url( url = l_url ).
    ENDIF.
  ENDMETHOD.


  METHOD move_table.
    "visual swap in the join order; the base table takes part via m_base_pos
    READ TABLE mt_jtabs INTO DATA(ls_tab) WITH KEY alias = i_alias.
    CHECK sy-subrc = 0.
    DATA(l_idx) = sy-tabix.
    DATA(l_other_idx) = l_idx + i_dir.
    CHECK l_other_idx >= 1 AND l_other_idx <= lines( mt_jtabs ).
    READ TABLE mt_jtabs INTO DATA(ls_other) INDEX l_other_idx.

    IF ls_tab-tabname = m_tabname. "moving the base itself
      m_base_pos = m_base_pos + i_dir.
      RETURN.
    ENDIF.
    IF ls_other-tabname = m_tabname. "candidate crosses the base
      m_base_pos = m_base_pos - i_dir.
      RETURN.
    ENDIF.

    "two candidates: swap their selection order
    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = ls_tab-tabname.
    CHECK sy-subrc = 0.
    READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<other>) WITH KEY tabname = ls_other-tabname.
    CHECK sy-subrc = 0.
    DATA(l_tmp) = <cand>-sel_order.
    <cand>-sel_order  = <other>-sel_order.
    <other>-sel_order = l_tmp.
  ENDMETHOD.


  METHOD move_table_before.
    "drag&drop: place table i_from before i_to ('#END' = last position)
    DATA lt_order TYPE TABLE OF tabname.

    LOOP AT mt_jtabs INTO DATA(ls_tab).
      IF condense( CONV string( ls_tab-alias ) ) = i_from.
        DATA(l_from_tab) = ls_tab-tabname.
      ELSE.
        APPEND ls_tab-tabname TO lt_order.
      ENDIF.
    ENDLOOP.
    CHECK l_from_tab IS NOT INITIAL.

    IF i_to = '#END'.
      APPEND l_from_tab TO lt_order.
    ELSE.
      READ TABLE mt_jtabs INTO ls_tab WITH KEY alias = i_to.
      CHECK sy-subrc = 0.
      READ TABLE lt_order TRANSPORTING NO FIELDS WITH KEY table_line = ls_tab-tabname.
      IF sy-subrc = 0.
        INSERT l_from_tab INTO lt_order INDEX sy-tabix.
      ELSE.
        APPEND l_from_tab TO lt_order.
      ENDIF.
    ENDIF.

    "write the new order back: base position + candidate selection order
    DATA(l_ord) = 0.
    LOOP AT lt_order INTO DATA(l_tabname).
      IF l_tabname = m_tabname.
        m_base_pos = sy-tabix.
        CONTINUE.
      ENDIF.
      ADD 1 TO l_ord.
      READ TABLE mt_cand ASSIGNING FIELD-SYMBOL(<cand>) WITH KEY tabname = l_tabname.
      IF sy-subrc = 0.
        <cand>-sel_order = l_ord.
      ENDIF.
    ENDLOOP.
    m_sel_count = l_ord.

    rebuild_selection( ).
  ENDMETHOD.


  METHOD move_field_before.
    "drag&drop: place field i_from (ALIAS~FIELD) before i_to ('#END' = last)
    DATA lt_keys TYPE TABLE OF string.

    DATA(lt_sel) = VALUE tt_jfld( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
    SORT lt_sel BY pos.
    LOOP AT lt_sel INTO DATA(ls_sel).
      DATA(l_key) = |{ condense( CONV string( ls_sel-alias ) ) }~{ ls_sel-fieldname }|.
      IF l_key NE i_from.
        APPEND l_key TO lt_keys.
      ENDIF.
    ENDLOOP.

    IF i_to = '#END'.
      APPEND i_from TO lt_keys.
    ELSE.
      READ TABLE lt_keys TRANSPORTING NO FIELDS WITH KEY table_line = i_to.
      IF sy-subrc = 0.
        INSERT i_from INTO lt_keys INDEX sy-tabix.
      ELSE.
        APPEND i_from TO lt_keys.
      ENDIF.
    ENDIF.

    LOOP AT lt_keys INTO l_key.
      SPLIT l_key AT '~' INTO DATA(l_alias) DATA(l_field).
      READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) WITH KEY alias = l_alias fieldname = l_field.
      IF sy-subrc = 0.
        <fld>-pos = sy-tabix. "position in lt_keys loop
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_fld_action.
    CASE i_act.
      WHEN 'RUN'.
        run_select( ).
        RETURN.
      WHEN 'ALL'.
        LOOP AT mt_jflds ASSIGNING FIELD-SYMBOL(<fld>).
          <fld>-sel = abap_true.
        ENDLOOP.
      WHEN 'NONE'.
        LOOP AT mt_jflds ASSIGNING <fld>.
          <fld>-sel = abap_false.
        ENDLOOP.
      WHEN 'KEYS'.
        LOOP AT mt_jflds ASSIGNING <fld>.
          <fld>-sel = <fld>-keyflag.
          CLEAR <fld>-pos.
        ENDLOOP.
      WHEN 'AUTO'.
        m_auto = boolc( m_auto = abap_false ).
      WHEN OTHERS.
        IF strlen( i_act ) > 4 AND i_act(4) = 'lng_'. "field name variant: lng_TECH / lng_<spras>
          IF i_act+4 = 'TECH'.
            m_show_texts = abap_false.
          ELSE.
            m_show_texts = abap_true.
            m_fld_lang = i_act+4.
            reload_field_texts( ).
          ENDIF.
        ELSEIF strlen( i_act ) > 3 AND i_act(3) = 'tg_'. "toggle single field: tg_ALIAS~FIELD
          SPLIT i_act+3 AT '~' INTO DATA(l_alias) DATA(l_field).
          READ TABLE mt_jflds ASSIGNING <fld> WITH KEY alias = l_alias fieldname = l_field.
          IF sy-subrc = 0.
            <fld>-sel = boolc( <fld>-sel = abap_false ).
            CLEAR <fld>-pos.
          ENDIF.
        ELSEIF strlen( i_act ) > 4.
          DATA(l_arg) = CONV char5( i_act+4 ).
          CASE i_act(4).
            WHEN 'all_'.
              LOOP AT mt_jflds ASSIGNING <fld> WHERE alias = l_arg.
                <fld>-sel = abap_true.
              ENDLOOP.
            WHEN 'non_'.
              LOOP AT mt_jflds ASSIGNING <fld> WHERE alias = l_arg.
                <fld>-sel = abap_false.
              ENDLOOP.
            WHEN 'key_'.
              LOOP AT mt_jflds ASSIGNING <fld> WHERE alias = l_arg.
                <fld>-sel = <fld>-keyflag.
              ENDLOOP.
          ENDCASE.
        ENDIF.
    ENDCASE.
    normalize_pos( ).
    render_flds( ).
    update_sql_view( ).
  ENDMETHOD.


  METHOD reload_field_texts.
    LOOP AT mt_jtabs INTO DATA(ls_tab).
      LOOP AT get_fieldlist( ls_tab-tabname ) INTO DATA(ls_f). "uses m_fld_lang
        READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>)
          WITH KEY alias = ls_tab-alias fieldname = ls_f-fieldname.
        IF sy-subrc = 0.
          <fld>-ddtext = ls_f-fieldtext.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD apply_postdata.
    "form fields: act=..., jt_Tn=..., on_Tn=... (url-encoded)
    DATA(l_post) = concat_lines_of( table = it_postdata ).
    SPLIT l_post AT '&' INTO TABLE DATA(lt_pairs).
    LOOP AT lt_pairs INTO DATA(l_pair).
      SPLIT l_pair AT '=' INTO DATA(l_name) DATA(l_value).
      REPLACE ALL OCCURRENCES OF '+' IN l_value WITH ` `.
      l_value = cl_http_utility=>unescape_url( l_value ).
      IF l_name = 'act'.
        rv_act = l_value.
      ELSEIF l_name CP 'jt_*'.
        READ TABLE mt_jtabs ASSIGNING FIELD-SYMBOL(<jtab>) WITH KEY alias = l_name+3.
        IF sy-subrc = 0.
          <jtab>-jtype = l_value.
        ENDIF.
      ELSEIF l_name CP 'on_*'.
        READ TABLE mt_jtabs ASSIGNING <jtab> WITH KEY alias = l_name+3.
        IF sy-subrc = 0.
          <jtab>-cond = l_value.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_sapevent.
    CASE action.
      WHEN 'toggle'.
        toggle_candidate( CONV #( getdata ) ).
      WHEN 'addtab'.
        DATA(l_post) = concat_lines_of( table = postdata ).
        SPLIT l_post AT '=' INTO DATA(l_dummy) DATA(l_newtab).
        REPLACE ALL OCCURRENCES OF '+' IN l_newtab WITH ` `.
        l_newtab = cl_http_utility=>unescape_url( l_newtab ).
        add_candidate( CONV #( l_newtab ) ).
      WHEN 'tabs'.
        DATA(l_act) = apply_postdata( postdata ). "stores jtype/cond edits into mt_jtabs
        IF strlen( l_act ) > 3.
          CASE l_act(3).
            WHEN 'up_'.
              move_table( i_alias = CONV #( l_act+3 ) i_dir = -1 ).
              rebuild_selection( ).
            WHEN 'dn_'.
              move_table( i_alias = CONV #( l_act+3 ) i_dir = 1 ).
              rebuild_selection( ).
            WHEN 'del'. "del_Tn
              READ TABLE mt_jtabs INTO DATA(ls_tab) WITH KEY alias = l_act+4.
              IF sy-subrc = 0.
                toggle_candidate( ls_tab-tabname ). "deselects + rebuilds + refreshes
                RETURN.
              ENDIF.
          ENDCASE.
        ENDIF.
        refresh_all( ).
      WHEN 'tmv'. "drag&drop of tables: FROM__TO
        SPLIT getdata AT '__' INTO DATA(l_from) DATA(l_to).
        IF l_from IS NOT INITIAL AND l_to IS NOT INITIAL.
          move_table_before( i_from = l_from i_to = l_to ).
          refresh_all( ).
        ENDIF.
      WHEN 'fmv'. "drag&drop of fields: ALIAS~FIELD__ALIAS~FIELD
        SPLIT getdata AT '__' INTO l_from l_to.
        IF l_from IS NOT INITIAL AND l_to IS NOT INITIAL.
          move_field_before( i_from = l_from i_to = l_to ).
          render_flds( ).
          update_sql_view( ).
        ENDIF.
      WHEN 'fld'.
        handle_fld_action( CONV #( getdata ) ).
      WHEN 'fsel'. "lasso: select all painted fields (keys=T0~F1;T1~F2;...)
        l_post = concat_lines_of( table = postdata ).
        SPLIT l_post AT '=' INTO l_dummy DATA(l_keys).
        l_keys = cl_http_utility=>unescape_url( l_keys ).
        SPLIT l_keys AT ';' INTO TABLE DATA(lt_keys).
        LOOP AT lt_keys INTO DATA(l_fkey).
          SPLIT l_fkey AT '~' INTO DATA(l_alias) DATA(l_field).
          READ TABLE mt_jflds ASSIGNING FIELD-SYMBOL(<fld>) WITH KEY alias = l_alias fieldname = l_field.
          IF sy-subrc = 0.
            <fld>-sel = abap_true.
          ENDIF.
        ENDLOOP.
        normalize_pos( ).
        render_flds( ).
        update_sql_view( ).
    ENDCASE.
  ENDMETHOD.


  METHOD create_sql_view.
    mo_low_split->get_container( EXPORTING row = 1 column = 1 RECEIVING container = DATA(lo_cont) ).

    CREATE OBJECT mo_sql_text
      EXPORTING
        parent = lo_cont
      EXCEPTIONS
        OTHERS = 1.
  ENDMETHOD.


  METHOD update_sql_view.
    CHECK mo_sql_text IS BOUND.
    mo_sql_text->set_textstream( generate_select( ) ).
    "rebuild the open result window right away
    IF m_auto = abap_true AND result_alive( ) = abap_true.
      execute_sql( i_sql = generate_select( ) i_quiet = abap_true ).
    ENDIF.
  ENDMETHOD.


  METHOD result_alive.
    rv_alive = boolc( mo_result IS BOUND AND line_exists( zcl_sde_appl=>mt_obj[ alv_viewer = mo_result ] ) ).
  ENDMETHOD.


  METHOD generate_select.
    DATA: l_fields TYPE string,
          l_from   TYPE string,
          lt_sel   TYPE tt_jfld.

    lt_sel = VALUE #( FOR wa IN mt_jflds WHERE ( sel = abap_true ) ( wa ) ).
    SORT lt_sel BY pos.
    LOOP AT lt_sel INTO DATA(ls_fld).
      IF l_fields IS NOT INITIAL.
        l_fields = |{ l_fields },{ cl_abap_char_utilities=>newline }       |.
      ENDIF.
      l_fields = |{ l_fields }{ ls_fld-alias CASE = LOWER }~{ ls_fld-fieldname CASE = LOWER } AS { ls_fld-alias CASE = LOWER }_{ ls_fld-fieldname CASE = LOWER }|.
    ENDLOOP.
    IF l_fields IS INITIAL.
      l_fields = 't0~*'.
    ENDIF.

    LOOP AT mt_jtabs INTO DATA(ls_tab).
      IF sy-tabix = 1.
        l_from = |{ ls_tab-tabname CASE = LOWER } AS { ls_tab-alias CASE = LOWER }|.
      ELSE.
        l_from = |{ l_from }{ cl_abap_char_utilities=>newline }  { ls_tab-jtype } JOIN { ls_tab-tabname CASE = LOWER } AS { ls_tab-alias CASE = LOWER } ON { ls_tab-cond }|.
      ENDIF.
    ENDLOOP.

    rv_sql = |SELECT { l_fields }{ cl_abap_char_utilities=>newline }| &&
             |  FROM { l_from }|.

    "inherit WHERE from the base window selections, qualified with t0~
    DATA(l_where) = mo_viewer->get_where( ).
    IF l_where IS NOT INITIAL.
      LOOP AT get_fieldlist( m_tabname ) INTO DATA(ls_f).
        REPLACE ALL OCCURRENCES OF REGEX |\\b{ ls_f-fieldname }\\b| IN l_where WITH |t0~{ ls_f-fieldname }|.
      ENDLOOP.
      rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } WHERE { l_where }|.
    ENDIF.

    DATA(l_rows) = COND i( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 ).
    rv_sql = |{ rv_sql }{ cl_abap_char_utilities=>newline } UP TO { l_rows } ROWS|.
  ENDMETHOD.


  METHOD run_select.
    DATA l_sql TYPE string.
    mo_sql_text->get_textstream( IMPORTING text = l_sql ).
    cl_gui_cfw=>flush( ).
    CHECK l_sql IS NOT INITIAL.
    execute_sql( l_sql ).
  ENDMETHOD.


  METHOD execute_sql.
    DATA: l_sql    TYPE string,
          l_rows   TYPE i,
          lt_comp  TYPE abap_component_tab,
          lr_table TYPE REF TO data.
    FIELD-SYMBOLS: <result> TYPE STANDARD TABLE.

    l_sql = i_sql.
    CHECK l_sql IS NOT INITIAL.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf   IN l_sql WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_sql WITH ` `.
    CONDENSE l_sql.
    DATA(l_upper) = to_upper( l_sql ).

    "cut out UP TO n ROWS
    l_rows = COND i( WHEN zcl_sde_appl=>gv_rows > 0 THEN zcl_sde_appl=>gv_rows ELSE 500 ).
    FIND REGEX 'UP\s+TO\s+(\d+)\s+ROWS' IN l_upper
      MATCH OFFSET DATA(l_off) MATCH LENGTH DATA(l_len) SUBMATCHES DATA(l_rows_c).
    IF sy-subrc = 0.
      l_rows = l_rows_c.
      REPLACE SECTION OFFSET l_off LENGTH l_len OF l_sql   WITH ``.
      REPLACE SECTION OFFSET l_off LENGTH l_len OF l_upper WITH ``.
    ENDIF.

    "split into SELECT <fields> FROM <from> [WHERE <where>]
    FIND REGEX '^\s*SELECT\s' IN l_upper MATCH LENGTH DATA(l_sel_len).
    IF sy-subrc NE 0.
      IF i_quiet = abap_false.
        MESSAGE 'Statement must start with SELECT' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      RETURN.
    ENDIF.
    FIND REGEX '\sFROM\s' IN l_upper MATCH OFFSET DATA(l_from_off) MATCH LENGTH DATA(l_from_len).
    IF sy-subrc NE 0.
      IF i_quiet = abap_false.
        MESSAGE 'FROM clause not found' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      RETURN.
    ENDIF.

    DATA(l_fields) = condense( substring( val = l_sql off = l_sel_len len = l_from_off - l_sel_len ) ).
    DATA(l_rest_off) = l_from_off + l_from_len.
    DATA(l_where) = ``.
    FIND REGEX '\sWHERE\s' IN l_upper MATCH OFFSET DATA(l_where_off) MATCH LENGTH DATA(l_where_len).
    IF sy-subrc = 0.
      DATA(l_from) = condense( substring( val = l_sql off = l_rest_off len = l_where_off - l_rest_off ) ).
      l_where = condense( substring( val = l_sql off = l_where_off + l_where_len len = strlen( l_sql ) - l_where_off - l_where_len ) ).
    ELSE.
      l_from = condense( substring( val = l_sql off = l_rest_off len = strlen( l_sql ) - l_rest_off ) ).
    ENDIF.

    "alias -> table map from the FROM clause
    TYPES: BEGIN OF t_alias, alias TYPE string, tabname TYPE tabname, END OF t_alias.
    DATA lt_alias TYPE TABLE OF t_alias.
    FIND ALL OCCURRENCES OF REGEX '(\w+)\s+AS\s+(\w+)' IN to_upper( l_from ) RESULTS DATA(lt_matches).
    LOOP AT lt_matches INTO DATA(ls_match).
      DATA(l_tab)   = to_upper( substring( val = l_from off = ls_match-submatches[ 1 ]-offset len = ls_match-submatches[ 1 ]-length ) ).
      DATA(l_alias) = to_upper( substring( val = l_from off = ls_match-submatches[ 2 ]-offset len = ls_match-submatches[ 2 ]-length ) ).
      APPEND VALUE #( alias = l_alias tabname = l_tab ) TO lt_alias.
    ENDLOOP.
    IF lt_alias IS INITIAL.
      "no aliases: single table select
      APPEND VALUE #( alias = '' tabname = to_upper( segment( val = condense( l_from ) index = 1 sep = ` ` ) ) ) TO lt_alias.
    ENDIF.

    "build result structure from the field list
    SPLIT l_fields AT ',' INTO TABLE DATA(lt_fld_str).
    LOOP AT lt_fld_str INTO DATA(l_fld_str).
      CONDENSE l_fld_str.
      CHECK l_fld_str IS NOT INITIAL.
      DATA(l_fld_up) = to_upper( l_fld_str ).
      DATA: l_alias2 TYPE string, l_field TYPE string, l_name TYPE string.
      CLEAR: l_alias2, l_field, l_name.
      FIND REGEX '^(?:(\w+)~)?(\w+)(?:\s+AS\s+(\w+))?$' IN l_fld_up
        SUBMATCHES l_alias2 l_field l_name.
      IF sy-subrc NE 0.
        IF i_quiet = abap_false.
          MESSAGE |Cannot parse field: { l_fld_str }| TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        RETURN.
      ENDIF.
      IF l_name IS INITIAL.
        l_name = COND #( WHEN l_alias2 IS INITIAL THEN l_field ELSE |{ l_alias2 }_{ l_field }| ).
      ENDIF.
      IF strlen( l_name ) > 30.
        l_name = l_name+0(30).
      ENDIF.

      READ TABLE lt_alias INTO DATA(ls_alias) WITH KEY alias = l_alias2.
      IF sy-subrc NE 0.
        READ TABLE lt_alias INTO ls_alias INDEX 1.
      ENDIF.

      TRY.
          DATA(lo_type) = CAST cl_abap_datadescr(
            cl_abap_typedescr=>describe_by_name( |{ ls_alias-tabname }-{ l_field }| ) ).
        CATCH cx_root.
          IF i_quiet = abap_false.
            MESSAGE |Unknown field { ls_alias-tabname }-{ l_field }| TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
          RETURN.
      ENDTRY.

      "keep names unique
      WHILE line_exists( lt_comp[ name = l_name ] ).
        l_name = |{ COND string( WHEN strlen( l_name ) > 28 THEN l_name+0(28) ELSE l_name ) }_{ sy-index }|.
      ENDWHILE.
      APPEND VALUE #( name = l_name type = lo_type ) TO lt_comp.
    ENDLOOP.

    IF lt_comp IS INITIAL.
      IF i_quiet = abap_false.
        MESSAGE 'No fields to select' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      RETURN.
    ENDIF.

    "dynamic Open SQL tokens must be uppercase (keep quoted literals as-is)
    l_fields = to_upper( l_fields ).
    l_from   = to_upper( l_from ).
    l_where  = upper_outside_quotes( l_where ).

    TRY.
        DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
        DATA(lo_tab) = cl_abap_tabledescr=>create(
                         p_line_type  = lo_struct
                         p_table_kind = cl_abap_tabledescr=>tablekind_std
                         p_unique     = abap_false ).
        CREATE DATA lr_table TYPE HANDLE lo_tab.
        ASSIGN lr_table->* TO <result>.

        "new strict-mode Open SQL: dynamic tokens with AS aliases are supported here
        SELECT (l_fields)
          FROM (l_from)
          WHERE (l_where)
          INTO CORRESPONDING FIELDS OF TABLE @<result>
          UP TO @l_rows ROWS.

      CATCH cx_root INTO DATA(lx).
        IF i_quiet = abap_false.
          MESSAGE lx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        RETURN.
    ENDTRY.

    l_name = |JOIN { m_tabname } ({ lines( <result> ) })|.
    IF result_alive( ) = abap_true. "rebuild the existing result window in place
      mo_result->rebind( ir_tab = lr_table i_name = l_name ).
    ELSE.
      APPEND INITIAL LINE TO zcl_sde_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
      <obj>-alv_viewer = NEW #( i_additional_name = l_name ir_tab = lr_table ).
      <obj>-alv_viewer->mo_sel->raise_selection_done( ).
      mo_result = <obj>-alv_viewer.
    ENDIF.
  ENDMETHOD.


  METHOD upper_outside_quotes.
    "segments alternate: outside / inside single quotes
    SPLIT i_sql AT '''' INTO TABLE DATA(lt_parts).
    LOOP AT lt_parts ASSIGNING FIELD-SYMBOL(<part>).
      IF sy-tabix MOD 2 = 1. "outside quotes
        <part> = to_upper( <part> ).
      ENDIF.
    ENDLOOP.
    rv_sql = concat_lines_of( table = lt_parts sep = '''' ).
  ENDMETHOD.
ENDCLASS.
