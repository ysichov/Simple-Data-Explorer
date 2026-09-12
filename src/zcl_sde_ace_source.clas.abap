CLASS zcl_sde_ace_source DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Which generated program ACE has to parse for an object named in ADT,
    " and that object's type with the subtype dropped. Both VERTEX windows
    " that read ACE start here, so the rule lives once.
    CLASS-METHODS resolve
      IMPORTING i_name     TYPE string
                i_type     TYPE string
      EXPORTING ev_type    TYPE string
                ev_program TYPE program
      RAISING   cx_adt_res_not_found
                cx_adt_res_bad_request.

    " The parse every ACE analysis starts from: the pool itself and each
    " include that belongs to it.
    CLASS-METHODS parse
      IMPORTING i_program        TYPE program
      RETURNING VALUE(rs_source) TYPE zif_ace_parse_data=>ts_parse_data.

ENDCLASS.


CLASS zcl_sde_ace_source IMPLEMENTATION.

  METHOD resolve.
    DATA: lv_name     TYPE string,
          lv_type     TYPE string,
          lv_sub      TYPE string,
          lv_clsname  TYPE seoclsname,
          lv_clstype  TYPE seoclstype,
          lv_progname TYPE progname.

    lv_name = to_upper( i_name ).
    lv_type = to_upper( i_type ).

    " The context menu passes the ADT type, which carries a subtype: CLAS/OC,
    " PROG/P. Only the part in front of the slash decides how the name is
    " resolved to the program ACE parses.
    SPLIT lv_type AT '/' INTO lv_type lv_sub.
    ev_type = lv_type.

    CASE lv_type.
      WHEN 'CLAS' OR 'INTF'.
        lv_clsname = lv_name.
        SELECT SINGLE clstype FROM seoclass
          WHERE clsname = @lv_clsname
          INTO @lv_clstype.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_adt_res_not_found
            EXPORTING resource_type = `class`
                      resource_id   = lv_name.
        ENDIF.
        " An interface pool ends in IP, a class pool in CP. The name is padded
        " to 30 characters first - that is how the generated pool is named.
        ev_program = lv_name && repeat( val = `=` occ = 30 - strlen( lv_name ) ).
        IF lv_clstype = 1.
          ev_program = ev_program && `IP`.
        ELSE.
          ev_program = ev_program && `CP`.
        ENDIF.

      WHEN 'PROG' OR 'INCL'.
        lv_progname = lv_name.
        SELECT SINGLE name FROM trdir
          WHERE name = @lv_progname
          INTO @DATA(lv_exists).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_adt_res_not_found
            EXPORTING resource_type = `program`
                      resource_id   = lv_name.
        ENDIF.
        ev_program = lv_name.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_adt_res_bad_request
          EXPORTING explanation = |Type { lv_type } is not supported here.| &&
                                  | ACE reads CLAS, INTF, PROG and INCL.|.
    ENDCASE.
  ENDMETHOD.


  METHOD parse.
    " ZCL_ACE_METRICS aggregates every parsed include whose program is the one
    " asked for, so each include is parsed under the pool rather than under
    " itself. A class pool holds nothing but INCLUDE statements; the methods
    " live in its CM includes, and without them the answer would be an empty
    " unit list rather than an error.
    zcl_ace_parser=>parse( EXPORTING i_program = i_program
                                     i_include = i_program
                           CHANGING  cs_source = rs_source ).

    SELECT include FROM d010inc
      WHERE master = @i_program
      INTO TABLE @DATA(lt_include).

    LOOP AT lt_include INTO DATA(ls_include).
      IF ls_include-include = i_program.
        CONTINUE.
      ENDIF.
      " D010INC also lists the system includes every program gets - <SYSINI>
      " and its kin. Their units (SYSTEM-EXIT, %_CTL_END) belong to SAP's
      " runtime, not to the object being measured, and left in they show up as
      " rows nobody wrote and are counted into the totals. The angle bracket
      " is what marks them: it cannot occur in a repository object name.
      IF ls_include-include CS '<'.
        CONTINUE.
      ENDIF.
      zcl_ace_parser=>parse( EXPORTING i_program = i_program
                                       i_include = ls_include-include
                             CHANGING  cs_source = rs_source ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
