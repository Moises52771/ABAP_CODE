*&---------------------------------------------------------------------*
*& Report ZTA_04_REPORT_RANGE_LIST_VOO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zta_04_report_range_list_voo.

*&---------------------------------------------------------------------*
*& Declarações
*&---------------------------------------------------------------------*
DATA: gt_lista_voo    TYPE TABLE OF sflights2,
      wa_lista_voo    TYPE          sflights2,

      gt_lista_voo_ij TYPE TABLE OF zsflights2_st,
      wa_lista_voo_ij TYPE          zsflights2_st.

SELECTION-SCREEN BEGIN OF BLOCK screen WITH FRAME.

PARAMETERS: p_id_ini TYPE bapisflkey-airlineid,
            p_id_fim TYPE bapisflkey-airlineid.

SELECTION-SCREEN END OF BLOCK screen.

SELECTION-SCREEN BEGIN OF BLOCK screen1 WITH FRAME.

PARAMETERS: p_dt_ini TYPE bapisflkey-flightdate,
            p_dt_fim TYPE bapisflkey-flightdate.

SELECTION-SCREEN END OF BLOCK screen1.


*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*

  PERFORM: range_for_all_entries,
*           range_inner_join,
*           range,
           imprime_alv.

*&---------------------------------------------------------------------*
*& Forms
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Range com SELECT simples
*&---------------------------------------------------------------------*
FORM range.

  DATA: tr_id  TYPE RANGE OF bapisflkey-airlineid,
        tr_dat TYPE RANGE OF bapisflkey-flightdate,
        it_id  TYPE TABLE OF sflights2,
        it_dat TYPE TABLE OF sflights2.

  tr_id = VALUE #( ( sign   = 'I'
                     option = 'BT'
                     low    = p_id_ini
                     high   = p_id_fim
                  ) ).


  tr_dat = VALUE #( ( sign  = 'I'
                     option = 'BT'
                     low    = p_dt_ini
                     high   = p_dt_fim
                  ) ).


*& Seleção dos dados
*&---------------------------------------------------------------------*
  SELECT * FROM sflights2
    INTO TABLE it_id
    WHERE carrid  IN tr_id
    AND   fldate  IN tr_dat.


*& Carrega tabela global
*&---------------------------------------------------------------------*
  LOOP AT it_id INTO wa_lista_voo.
    IF sy-subrc = 0.

      APPEND wa_lista_voo TO gt_lista_voo.

    ENDIF.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Range com uso de inner join
*&---------------------------------------------------------------------*
FORM range_inner_join.

  DATA: tr_id  TYPE RANGE OF zsflights2_st-carrid,
        tr_dat TYPE RANGE OF zsflights2_st-fldate,
        it_id  TYPE          zsflights2_tt,
        it_dat TYPE          zsflights2_tt.

  tr_id = VALUE #( ( sign   = 'I'
                     option = 'BT'
                     low    = p_id_ini
                     high   = p_id_fim
                  ) ).


  tr_dat = VALUE #( ( sign  = 'I'
                     option = 'BT'
                     low    = p_dt_ini
                     high   = p_dt_fim
                  ) ).


*& Seleção dos dados
*&---------------------------------------------------------------------*
  SELECT tb3~carrid
         tb3~connid
         tb3~fldate
         tb2~carrname
         tb1~countryfr
         tb1~cityfrom
         tb1~airpfrom
         tb1~countryto
         tb1~cityto
         tb1~airpto
         tb1~fltime
         tb1~deptime
         tb1~arrtime
         tb1~distance
    INTO TABLE it_id
    FROM spfli AS tb1
    INNER JOIN scarr   AS tb2 ON  tb2~carrid  = tb1~carrid
    INNER JOIN sflight AS tb3 ON  tb1~carrid  = tb3~carrid
    AND                           tb1~connid  = tb3~connid
    WHERE tb2~carrid  IN tr_id
    AND   fldate      IN tr_dat.


*& Carrega tabela global
*&---------------------------------------------------------------------*
  LOOP AT it_id INTO wa_lista_voo_ij.
    IF sy-subrc = 0.

      APPEND wa_lista_voo_ij TO gt_lista_voo_ij.

    ENDIF.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Range com uso do for all entries
*&---------------------------------------------------------------------*
FORM range_for_all_entries.

  TYPES: BEGIN OF ty_spfli,
           carrid    TYPE spfli-carrid,
           connid    TYPE spfli-connid,
           countryfr TYPE spfli-countryfr,
           cityfrom  TYPE spfli-cityfrom,
           airpfrom  TYPE spfli-airpfrom,
           countryto TYPE spfli-countryto,
           cityto    TYPE spfli-cityto,
           airpto    TYPE spfli-airpto,
           fltime    TYPE spfli-fltime,
           deptime   TYPE spfli-deptime,
           arrtime   TYPE spfli-arrtime,
           distance  TYPE spfli-distance,
         END OF ty_spfli.

  TYPES: BEGIN OF ty_scarr,
           carrid   TYPE scarr-carrid,
           carrname TYPE scarr-carrname,
         END OF ty_scarr.

  TYPES: BEGIN OF ty_sflight,
           carrid TYPE sflight-carrid,
           connid TYPE sflight-connid,
           fldate TYPE sflight-fldate,
         END OF ty_sflight.

  DATA: tr_id      TYPE RANGE OF zsflights2_st-carrid,
        tr_dat     TYPE RANGE OF zsflights2_st-fldate,
        it_sflight TYPE TABLE OF ty_sflight,
        it_spfli   TYPE TABLE OF ty_spfli,
        it_scarr   TYPE TABLE OF ty_scarr.

  tr_id = VALUE #( ( sign   = 'I'
                     option = 'BT'
                     low    = p_id_ini
                     high   = p_id_fim
                     ) ).


  tr_dat = VALUE #( ( sign   = 'I'
                      option = 'BT'
                      low    = p_dt_ini
                      high   = p_dt_fim
                      ) ).


*& Seleção dos dados
*&---------------------------------------------------------------------*
  SELECT sflight~carrid
         sflight~connid
         sflight~fldate
  INTO TABLE it_sflight
  FROM sflight AS sflight
  WHERE sflight~carrid  IN tr_id
    AND sflight~fldate  IN tr_dat.

  IF it_sflight[] IS NOT INITIAL.

    SELECT  spfli~carrid
            spfli~connid
            spfli~countryfr
            spfli~cityfrom
            spfli~airpfrom
            spfli~countryto
            spfli~cityto
            spfli~airpto
            spfli~fltime
            spfli~deptime
            spfli~arrtime
            spfli~distance
    FROM spfli
    INTO TABLE it_spfli
    FOR ALL ENTRIES IN it_sflight
    WHERE carrid = it_sflight-carrid
    AND   connid = it_sflight-connid.

    SELECT  scarr~carrid
            scarr~carrname
    FROM scarr
    INTO TABLE it_scarr
    FOR ALL ENTRIES IN it_sflight
    WHERE carrid = it_sflight-carrid.

  ENDIF.


*& Carrega tabela global
*&---------------------------------------------------------------------*
  LOOP AT it_sflight ASSIGNING FIELD-SYMBOL(<lfs_sflight>).
    READ TABLE it_spfli ASSIGNING FIELD-SYMBOL(<lfs_spfli>) WITH KEY carrid = <lfs_sflight>-carrid.
    IF sy-subrc = 0.
      IF <lfs_sflight>-connid = <lfs_spfli>-connid.

        MOVE-CORRESPONDING <lfs_sflight> TO wa_lista_voo_ij.
        MOVE-CORRESPONDING <lfs_spfli>   TO wa_lista_voo_ij.

        READ TABLE it_scarr ASSIGNING FIELD-SYMBOL(<lfs_scarr>) WITH KEY carrid = <lfs_sflight>-carrid.

        MOVE-CORRESPONDING <lfs_scarr> TO wa_lista_voo_ij.

      ENDIF.

      APPEND wa_lista_voo_ij TO gt_lista_voo_ij.
    ENDIF.
  ENDLOOP.

*& Carrega tabela global
*&---------------------------------------------------------------------*
*  LOOP AT it_scarr ASSIGNING FIELD-SYMBOL(<lfs_scarr>).
*    READ TABLE it_sflight ASSIGNING FIELD-SYMBOL(<lfs_sflight>) WITH KEY carrid = <lfs_scarr>-carrid BINARY SEARCH.
*    IF sy-subrc = 0.
*      MOVE-CORRESPONDING <lfs_sflight> TO wa_lista_voo_ij.
*      MOVE-CORRESPONDING <lfs_scarr>   TO wa_lista_voo_ij.
*
*      READ TABLE it_spfli ASSIGNING FIELD-SYMBOL(<lfs_spfli>) WITH KEY carrid = <lfs_sflight>-carrid BINARY SEARCH.
*
*      IF <lfs_sflight>-connid = <lfs_spfli>-connid.
*
*        MOVE-CORRESPONDING <lfs_spfli>   TO wa_lista_voo_ij.
*
*      ENDIF.
*
*      APPEND wa_lista_voo_ij TO gt_lista_voo_ij.
*    ENDIF.
*  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Gerar fieldcat e preencher o ALV
*&---------------------------------------------------------------------*
FORM imprime_alv.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo,
        wa_fieldcat TYPE LINE OF lvc_t_fcat.

  wa_layout-zebra      = 'X'.
  wa_layout-no_hgridln = 'X'.

  CONSTANTS c_estrutura TYPE dd02l-tabname VALUE 'zsflights2_st'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = c_estrutura
    CHANGING
      ct_fieldcat      = lt_fieldcat.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      is_layout_lvc   = wa_layout
      it_fieldcat_lvc = lt_fieldcat
    TABLES
      t_outtab        = gt_lista_voo_ij.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.