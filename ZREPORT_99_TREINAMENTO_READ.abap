*&---------------------------------------------------------------------*
*& Report ZREPORT_99_TREINAMENTO
*&---------------------------------------------------------------------*
REPORT zreport_99_treinamento_read.

*-------------------------------------------------------------*
* Declarações
*-------------------------------------------------------------*
TABLES: bapisflkey, bapisfldra.

DATA: gt_flight_list TYPE TABLE OF bapisfldat,
      wa_flight_list TYPE bapisfldat,

      gt_return      TYPE TABLE OF bapiret2,
      wa_return      TYPE bapiret2.

*-------------------------------------------------------------*
* SELECT-OPTIONS
*-------------------------------------------------------------*
SELECT-OPTIONS: s_id   FOR bapisflkey-airlineid.
SELECT-OPTIONS: s_date FOR bapisfldra-low.

PARAMETERS p_restr  TYPE flag AS CHECKBOX.
PARAMETERS p_clearl TYPE flag AS CHECKBOX.

*-------------------------------------------------------------*
INITIALIZATION.
*-------------------------------------------------------------*

*-------------------------------------------------------------*
START-OF-SELECTION.
*-------------------------------------------------------------*
  PERFORM: seleciona_dados,
           grava_log.

*-------------------------------------------------------------*
END-OF-SELECTION.
*-------------------------------------------------------------*
  PERFORM exibe_lista.
*  PERFORM exibe_lista_alv.

*-------------------------------------------------------------*
* Forms
*-------------------------------------------------------------*
*-------------------------------------------------------------*
* FORM seleciona_dados
*-------------------------------------------------------------*
FORM seleciona_dados.

*  DATA lt_sflights2 LIKE sflights2 OCCURS 0 WITH HEADER LINE.
  DATA: lt_sflights2 TYPE TABLE OF sflights2,
        wa_sflights2 TYPE sflights2.

* Busca Lista de Voo
*--BAPI--*
*  CALL FUNCTION 'BAPI_FLIGHT_GETLIST'
*    EXPORTING
*      airline     = p_id
*    TABLES
*      date_range  = s_date
*      flight_list = gt_flight_list
*      return      = gt_return.

*--Seleção da visão de Banco de Dados--*
*  SELECT * FROM sflights2 INTO TABLE lt_sflights2
*        WHERE carrid    = p_id
*          AND fldate    IN s_date.

***--Seleção com Join--*
*  SELECT scarr~mandt
*         scarr~carrid
*         spfli~connid
*         sflight~fldate
*         scarr~carrname
*         spfli~countryfr
*         spfli~cityfrom
*         spfli~airpfrom
*         spfli~countryto
*         spfli~cityto
*         spfli~airpto
*         spfli~fltime
*         spfli~deptime
*         spfli~arrtime
*         spfli~distance
*         spfli~distid
*         spfli~fltype
*         spfli~period
*         sflight~price
*         sflight~currency
*         sflight~planetype
*         sflight~seatsmax
*         sflight~seatsocc
*         sflight~paymentsum
*         sflight~seatsmax_b
*         sflight~seatsocc_b
*         sflight~seatsmax_f
*         sflight~seatsocc_f
*      FROM spfli
*      INNER JOIN scarr AS scarr ON scarr~mandt    =  spfli~mandt AND
*                                   scarr~carrid   =  spfli~carrid
*      INNER JOIN sflight ON spfli~mandt    =  sflight~mandt  AND
*                            spfli~carrid   =  sflight~carrid AND
*                            spfli~connid   =  sflight~connid
*      INTO TABLE lt_sflights2
*      WHERE scarr~carrid   IN s_id
*        AND sflight~fldate IN s_date.

*--Seleção Individual--*

  DATA: lt_scarr        TYPE TABLE OF scarr,
        lt_spfli        TYPE TABLE OF spfli,
        lt_sflight      TYPE TABLE OF sflight,
        lt_ztfl0001     TYPE TABLE OF ztfl0001,

        wa_scarr        TYPE scarr,
        wa_spfli        TYPE spfli,
        wa_sflight      TYPE sflight,
        wa_ztfl0001     TYPE ztfl0001,

        vl_index_flight TYPE sy-tabix.

* Obtém restrições
  SELECT *
    FROM ztfl0001
    INTO TABLE lt_ztfl0001.
  SORT lt_ztfl0001 BY airp_restrict.

* Obtém dados de Voo
  SELECT *
    INTO TABLE lt_scarr
    FROM scarr
    WHERE carrid IN s_id.

  IF lt_scarr[] IS NOT INITIAL.
    SELECT *
      INTO TABLE lt_spfli
      FROM spfli
      FOR ALL ENTRIES IN lt_scarr
      WHERE carrid = lt_scarr-carrid.

    IF sy-subrc EQ 0.
      SELECT *
        INTO TABLE lt_sflight
        FROM sflight
        FOR ALL ENTRIES IN lt_spfli
       WHERE carrid = lt_spfli-carrid AND
             connid = lt_spfli-connid AND
             fldate IN s_date.
    ENDIF.
  ENDIF.

  SORT: lt_scarr   BY carrid,
        lt_spfli   BY carrid connid,
        lt_sflight BY carrid connid.

  LOOP AT lt_scarr INTO wa_scarr.
    LOOP AT lt_spfli INTO wa_spfli WHERE carrid = wa_scarr-carrid.
*      LOOP AT lt_sflight INTO wa_sflight WHERE carrid = wa_spfli-carrid AND
*                                               connid = wa_spfli-connid.

      READ TABLE lt_sflight INTO wa_sflight  WITH KEY carrid = wa_spfli-carrid
                                                      connid = wa_spfli-connid
                                             BINARY SEARCH.

      IF sy-subrc EQ 0.
        vl_index_flight = sy-tabix.
        DO.
          READ TABLE lt_sflight INTO wa_sflight INDEX vl_index_flight.
          IF sy-subrc = 0                        AND
             wa_sflight-carrid = wa_spfli-carrid AND
             wa_sflight-connid = wa_spfli-connid.

            wa_sflights2-carrid    = wa_scarr-carrid.
            wa_sflights2-connid    = wa_spfli-connid.
            wa_sflights2-fldate    = wa_sflight-fldate.
            wa_sflights2-carrname  = wa_scarr-carrname.
            wa_sflights2-countryfr = wa_spfli-countryfr.
            wa_sflights2-cityfrom  = wa_spfli-cityfrom.
            wa_sflights2-airpfrom  = wa_spfli-airpfrom.
            wa_sflights2-countryto = wa_spfli-countryto.
            wa_sflights2-cityto    = wa_spfli-cityto.
            wa_sflights2-airpto    = wa_spfli-airpto.
            wa_sflights2-fltime    = wa_spfli-fltime.
            wa_sflights2-deptime   = wa_spfli-deptime.
            wa_sflights2-arrtime   = wa_spfli-arrtime.
            wa_sflights2-distance  = wa_spfli-distance.
            wa_sflights2-distid    = wa_spfli-distid.
            wa_sflights2-fltype    = wa_spfli-fltype.
            wa_sflights2-period    = wa_spfli-period.
            wa_sflights2-price     = wa_sflight-price.
            wa_sflights2-currency  = wa_sflight-currency.
            wa_sflights2-planetype = wa_sflight-planetype.


            IF p_restr IS INITIAL.
              READ TABLE lt_ztfl0001 INTO wa_ztfl0001 WITH KEY airp_restrict = wa_sflights2-airpfrom
                                                      BINARY SEARCH.
              IF sy-subrc NE 0.
                READ TABLE lt_ztfl0001 INTO wa_ztfl0001 WITH KEY airp_restrict = wa_sflights2-airpto
                                          BINARY SEARCH.
                IF sy-subrc NE 0.
                  APPEND wa_sflights2 TO lt_sflights2.
                ENDIF.
              ENDIF.
            ELSE.
              READ TABLE lt_ztfl0001 INTO wa_ztfl0001 WITH KEY airp_restrict = wa_sflights2-airpfrom
                                                      BINARY SEARCH.
              IF sy-subrc EQ 0.
                APPEND wa_sflights2 TO lt_sflights2.
              ELSE.
                READ TABLE lt_ztfl0001 INTO wa_ztfl0001 WITH KEY airp_restrict = wa_sflights2-airpto
                                          BINARY SEARCH.
                IF sy-subrc EQ 0.
                  APPEND wa_sflights2 TO lt_sflights2.
                ENDIF.
              ENDIF.
            ENDIF.

            CLEAR wa_sflights2.
            ADD 1 TO vl_index_flight.

          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
*      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

* Converte sflights2 para flight_list
  LOOP AT lt_sflights2 INTO wa_sflights2.
    CALL FUNCTION 'MAP2E_SFLIGHTS2_TO_BAPISFLDAT'
      EXPORTING
        sflights2  = wa_sflights2
      CHANGING
        bapisfldat = wa_flight_list
                     EXCEPTIONS
                     error_converting_curr_amount.

    wa_flight_list-arrdate =  wa_flight_list-flightdate +  wa_sflights2-period.
    APPEND wa_flight_list TO gt_flight_list.
    CLEAR wa_flight_list.
  ENDLOOP.

ENDFORM.

*-------------------------------------------------------------*
* FORM exibe_lista
*-------------------------------------------------------------*
FORM exibe_lista.
  DATA: lv_total             TYPE bapisfldat-price,
        lv_al_id             TYPE bapisfldat-airline,
        lv_al_subtotal       TYPE bapisfldat-price,
        lv_tabix             TYPE sy-tabix,
        wa_flight_list_index TYPE bapisfldat.

  SORT gt_flight_list BY airlineid.

  LOOP AT gt_flight_list INTO wa_flight_list.
    lv_tabix = sy-tabix + 1.

    WRITE:/ wa_flight_list-airlineid,
            wa_flight_list-airline(10),
            wa_flight_list-connectid,
            wa_flight_list-flightdate,
            wa_flight_list-airportfr,
            wa_flight_list-cityfrom,
            wa_flight_list-airportto,
            wa_flight_list-cityto,
            wa_flight_list-deptime,
            wa_flight_list-arrtime,
            wa_flight_list-arrdate,
            wa_flight_list-price,
            wa_flight_list-curr,
            wa_flight_list-curr_iso.

    " SUB-TOTAL
    READ TABLE gt_flight_list INTO wa_flight_list_index INDEX lv_tabix. "BINARY SEARCH -> Sort!!!
    IF sy-subrc = 0 AND wa_flight_list-airlineid = wa_flight_list_index-airlineid.
      lv_al_subtotal = lv_al_subtotal + wa_flight_list-price.
    ELSE.
      WRITE: sy-uline(55), sy-vline.
      IF lv_al_subtotal < 20000.
        FORMAT COLOR COL_NEGATIVE ON.
      ELSE.
        FORMAT COLOR COL_HEADING ON.
      ENDIF.

      WRITE: 'Sub-total:', lv_al_subtotal, sy-vline.
      FORMAT COLOR OFF.
      WRITE: sy-uline(55).
      CLEAR: lv_al_id, lv_al_subtotal.
      lv_al_subtotal = lv_al_subtotal + wa_flight_list-price.
    ENDIF.

    " TOTAL
    lv_total = lv_total + wa_flight_list-price.
  ENDLOOP.

* Imprime total subtotal
  WRITE: sy-uline(55), sy-vline, 'Total:', lv_total, sy-vline, sy-uline(55).

ENDFORM.

*-------------------------------------------------------------*
* FORM exibe_lista_alv
*-------------------------------------------------------------*
FORM exibe_lista_alv.

  CONSTANTS c_table TYPE dd02l-tabname VALUE 'BAPISFLDAT'.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo.

* Ajusta layout
  wa_layout-zebra      = 'X'.
* wa_layout-edit       = 'X'.
  wa_layout-no_hgridln = 'X'.

* Obtém estrutura do report
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_table
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

* Exibe report
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      is_layout_lvc   = wa_layout
      it_fieldcat_lvc = lt_fieldcat
    TABLES
      t_outtab        = gt_flight_list
    EXCEPTIONS
      program_error   = 1
      OTHERS          = 2.

ENDFORM.


*-------------------------------------------------------------*
* FORM grava_log
*-------------------------------------------------------------*
FORM grava_log.

  DATA: lt_ztfl0002 TYPE TABLE OF ztfl0002,
        wa_ztfl0002 TYPE ztfl0002,
        lv_uzeit    TYPE sy-uzeit,
        lv_datum    TYPE sy-datum.

  lv_uzeit = sy-uzeit.
  lv_datum = sy-datum.

  IF  p_clearl IS NOT INITIAL.
    DELETE FROM ztfl0002 WHERE uname = sy-uname.
  ENDIF.

  LOOP AT gt_flight_list INTO wa_flight_list.
    wa_ztfl0002-carrid   = wa_flight_list-airlineid.
    wa_ztfl0002-currency = wa_flight_list-price.
    wa_ztfl0002-uzeit = lv_uzeit.
    wa_ztfl0002-datum = lv_datum.
    wa_ztfl0002-uname = sy-uname.
    COLLECT wa_ztfl0002 INTO lt_ztfl0002.
  ENDLOOP.

  INSERT ztfl0002 FROM TABLE lt_ztfl0002.

ENDFORM.