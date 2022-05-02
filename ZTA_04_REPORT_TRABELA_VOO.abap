*&---------------------------------------------------------------------*
*& Report ZTA_04_REPORT_TRABELA_VOO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zta_04_report_trabela_voo LINE-SIZE 180.

*&---------------------------------------------------------------------*
*& Declarações
*&---------------------------------------------------------------------*
TABLES: bapisflkey,
        bapisfldra.

DATA: gt_lista_voo TYPE TABLE OF zbapisfldat,
      wa_lista_voo TYPE zbapisfldat,

      gt_return    TYPE TABLE OF bapiret2,
      wa_return    TYPE bapiret2.

*&---------------------------------------------------------------------*
*& Parameters
*&---------------------------------------------------------------------*
PARAMETERS:     p_id TYPE bapisflkey-airlineid.
SELECT-OPTIONS: s_date FOR bapisfldra-low.

*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*

  PERFORM seleciona_dados.

*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*

  PERFORM exibe_lista_alv.



*&---------------------------------------------------------------------*
*& Seleciona Dados
*&---------------------------------------------------------------------*
FORM seleciona_dados.

  CALL FUNCTION 'BAPI_FLIGHT_GETLIST'
    EXPORTING
      airline     = p_id
*     DESTINATION_FROM       =
*     DESTINATION_TO         =
*     MAX_ROWS    =
    TABLES
      date_range  = s_date
*     EXTENSION_IN           =
      flight_list = gt_lista_voo
*     EXTENSION_OUT          =
      return      = gt_return.

ENDFORM.



*&---------------------------------------------------------------------*
*& Exibe a lista
*&---------------------------------------------------------------------*
FORM exibe_lista.

  SORT gt_lista_voo BY airlineid.

  FORMAT COLOR COL_POSITIVE ON.
  WRITE: sy-vline,    'COD',
         sy-vline,    'NOME',
         20 sy-vline, 'CNX ',
         sy-vline,    '   DATA   ',
         sy-vline,    'COD',
         sy-vline,    'PARTIDA             ',
         sy-vline,    'COD',
         sy-vline,    'DESTINO             ',
         sy-vline,    'HORA PT ',
         sy-vline,    'HORA CG ',
         sy-vline,    '   DATA   ',
         sy-vline,    '            PREÇO             ',
         sy-vline,    'COD  ',
         sy-vline,    'COD', sy-vline.

  FORMAT COLOR OFF.

  IF gt_lista_voo IS INITIAL.
    MESSAGE i000(zmessage).
  ELSE.
    LOOP AT gt_lista_voo INTO wa_lista_voo.


      WRITE:/ sy-uline,
              sy-vline, wa_lista_voo-airlineid,
              sy-vline, wa_lista_voo-airline(10),
              sy-vline, wa_lista_voo-connectid,
              sy-vline, wa_lista_voo-flightdate,
              sy-vline, wa_lista_voo-airportfr,
              sy-vline, wa_lista_voo-cityfrom,
              sy-vline, wa_lista_voo-airportto,
              sy-vline, wa_lista_voo-cityto,
              sy-vline, wa_lista_voo-deptime,
              sy-vline, wa_lista_voo-arrtime,
              sy-vline, wa_lista_voo-arrdate, sy-vline.

      IF wa_lista_voo-price > 1000.
        FORMAT COLOR COL_NEGATIVE ON.
      ELSE.
        FORMAT COLOR COL_HEADING ON.
      ENDIF.

      WRITE:  wa_lista_voo-price.
      FORMAT COLOR OFF.

      WRITE:  sy-vline, wa_lista_voo-curr,
              sy-vline, wa_lista_voo-curr_iso,
              sy-vline.

    ENDLOOP.
  ENDIF.



ENDFORM.


FORM exibe_lista_alv.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo,
        wa_fieldcat TYPE LINE OF lvc_t_fcat.

  wa_layout-zebra      = 'X'.
  wa_layout-no_hgridln = 'X'.
  wa_layout-sel_mode   = 'C'.
  wa_layout-box_fname  = 'BOX'.


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
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                =
*     I_BUFFER_ACTIVE =
*     I_CALLBACK_PROGRAM                = ' '
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID = ' '
*     I_GRID_TITLE    =
*     I_GRID_SETTINGS =
      is_layout_lvc   = wa_layout
      it_fieldcat_lvc = lt_fieldcat
*     IT_EXCLUDING    =
*     IT_SPECIAL_GROUPS_LVC             =
*     IT_SORT_LVC     =
*     IT_FILTER_LVC   =
*     IT_HYPERLINK    =
*     IS_SEL_HIDE     =
*     I_DEFAULT       = 'X'
*     I_SAVE          = ' '
*     IS_VARIANT      =
*     IT_EVENTS       =
*     IT_EVENT_EXIT   =
*     IS_PRINT_LVC    =
*     IS_REPREP_ID_LVC                  =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 =
*     I_HTML_HEIGHT_END                 =
*     IT_ALV_GRAPHICS =
*     IT_EXCEPT_QINFO_LVC               =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
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