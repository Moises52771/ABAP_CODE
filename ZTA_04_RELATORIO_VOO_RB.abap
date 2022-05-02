*&---------------------------------------------------------------------*
*& Report ZTA_04_RELATORIO_VOO_RB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zta_04_relatorio_voo_rb.

*&---------------------------------------------------------------------*
*& Declarações
*&---------------------------------------------------------------------*
TABLES: bapisflkey, bapisfldra, bapisfladd.
DATA: gt_lista_voo      TYPE TABLE OF bapisfldat,
      wa_lista_voo      TYPE          bapisfldat,

      gt_return         TYPE TABLE OF bapiret2,
      wa_return         TYPE          bapiret2,

      wa_adicional_info TYPE bapisfladd,

      gt_det_voo        TYPE zta_04_tt_det_voo,
      wa_det_voo        TYPE zta_04_st_det_voo.


SELECTION-SCREEN BEGIN OF BLOCK screen WITH FRAME.

PARAMETERS: p_id   TYPE bapisflkey-airlineid MODIF ID s2.
SELECT-OPTIONS: s_data FOR bapisflkey-flightdate MODIF ID s1.

SELECTION-SCREEN END OF BLOCK screen.


SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME.
PARAMETERS: r_lista RADIOBUTTON GROUP rad USER-COMMAND invisible DEFAULT 'X',
            r_det   RADIOBUTTON GROUP rad.
SELECTION-SCREEN END OF BLOCK part1.

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*&---------------------------------------------------------------------*
  PERFORM esconde_parameters.

*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  IF r_lista = 'X'.
    PERFORM: seleciona_dados_list.

  ELSEIF r_det = 'X'.
    PERFORM: seleciona_dados_det.

  ENDIF.

*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*

  IF r_lista = 'X'.
    PERFORM: imprime_lista_alv.

  ELSEIF r_det = 'X'.
    PERFORM: imprime_lista_alv_det.

  ENDIF.

*&---------------------------------------------------------------------*
*& FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Seleciona Dados Lista
*&---------------------------------------------------------------------*
FORM seleciona_dados_list.

  CALL FUNCTION 'BAPI_FLIGHT_GETLIST'
    EXPORTING
      airline     = p_id
*     DESTINATION_FROM       =
*     DESTINATION_TO         =
*     MAX_ROWS    =
    TABLES
      date_range  = s_data
*     EXTENSION_IN           =
      flight_list = gt_lista_voo
*     EXTENSION_OUT          =
      return      = gt_return.

ENDFORM.


*&---------------------------------------------------------------------*
*& Seleciona Dados Detalhes
*&---------------------------------------------------------------------*
FORM seleciona_dados_det.

  PERFORM seleciona_dados_list.

  LOOP AT gt_lista_voo INTO wa_lista_voo.

    CALL FUNCTION 'BAPI_FLIGHT_GETDETAIL'
      EXPORTING
        airlineid       = wa_lista_voo-airlineid
        connectionid    = wa_lista_voo-connectid
        flightdate      = wa_lista_voo-flightdate
      IMPORTING
*       flight_data     = gt_lista_voo1
        additional_info = wa_adicional_info
*       availibility    = av
*    TABLES
*       EXTENSION_IN    =
*       EXTENSION_OUT   =
*       return          = gt_return
      .

    wa_det_voo-airlineid  = wa_lista_voo-airlineid.
    wa_det_voo-airline    = wa_lista_voo-airline.
    wa_det_voo-connectid  = wa_lista_voo-connectid.
    wa_det_voo-flightdate = wa_lista_voo-flightdate.
    wa_det_voo-flighttime = wa_adicional_info-flighttime.
    wa_det_voo-distance   = wa_adicional_info-distance.
    wa_det_voo-unit       = wa_adicional_info-unit.
    wa_det_voo-unit_iso   = wa_adicional_info-unit_iso.
    wa_det_voo-planetype  = wa_adicional_info-planetype.
    wa_det_voo-flighttype = wa_adicional_info-flighttype.

    APPEND wa_det_voo TO gt_det_voo.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& imprime lista ALV Lista
*&---------------------------------------------------------------------*
FORM imprime_lista_alv.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo,
        wa_fieldcat TYPE LINE OF lvc_t_fcat.

  wa_layout-zebra      = 'X'.
  wa_layout-no_hgridln = 'X'.
*  wa_layout-sel_mode   = 'C'.
*  wa_layout-box_fname  = 'BOX'.

  CONSTANTS c_estrutura TYPE dd02l-tabname VALUE 'BAPISFLDAT'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE  =
      i_structure_name = c_estrutura
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER           =
*     I_INTERNAL_TABNAME           =
    CHANGING
      ct_fieldcat      = lt_fieldcat
*   EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR    = 2
*     OTHERS           = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      is_layout_lvc   = wa_layout
      it_fieldcat_lvc = lt_fieldcat
    TABLES
      t_outtab        = gt_lista_voo
*   EXCEPTIONS
*     PROGRAM_ERROR   = 1
*     OTHERS          = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& imprime lista ALV Detalhe
*&---------------------------------------------------------------------*
FORM imprime_lista_alv_det.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo,
        wa_fieldcat TYPE LINE OF lvc_t_fcat.

  wa_layout-zebra      = 'X'.
  wa_layout-no_hgridln = 'X'.
*  wa_layout-sel_mode   = 'C'.
*  wa_layout-box_fname  = 'BOX'.

  CONSTANTS c_estrutura TYPE dd02l-tabname VALUE 'ZTA_04_ST_DET_VOO'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE  =
      i_structure_name = c_estrutura
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER           =
*     I_INTERNAL_TABNAME           =
    CHANGING
      ct_fieldcat      = lt_fieldcat
*   EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR    = 2
*     OTHERS           = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      is_layout_lvc   = wa_layout
      it_fieldcat_lvc = lt_fieldcat
    TABLES
      t_outtab        = gt_det_voo
*   EXCEPTIONS
*     PROGRAM_ERROR   = 1
*     OTHERS          = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

FORM esconde_parameters.
  LOOP AT SCREEN.
    IF r_lista = 'X'.
      IF screen-group1 = 's1'.
        screen-invisible  = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF screen-group1 = 'S1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ELSE.

      IF screen-group1 = 'S1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.


      IF screen-group1 = 'S2'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.