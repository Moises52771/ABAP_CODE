*&---------------------------------------------------------------------*
*& Report ZREPORT_99_TREINAMENTO
*&---------------------------------------------------------------------*
REPORT zreport_99_treinamento NO STANDARD PAGE HEADING LINE-SIZE 26.

TYPE-POOLS: icon.

*-------------------------------------------------------------*
* Declarações
*-------------------------------------------------------------*
TABLES: bapisflkey, bapisfldra.

DATA: gt_flight_list TYPE TABLE OF zbapisfldat,
      wa_flight_list TYPE zbapisfldat,

      gt_return      TYPE TABLE OF bapiret2,
      wa_return      TYPE bapiret2.

RANGES: r_data FOR sy-datum.

*-------------------------------------------------------------*
* SELECT-OPTIONS
*-------------------------------------------------------------*
PARAMETERS     p_id  AS CHECKBOX.
SELECT-OPTIONS s_date FOR bapisfldra-low.

*-------------------------------------------------------------*
INITIALIZATION.
*-------------------------------------------------------------*


*-------------------------------------------------------------*
START-OF-SELECTION.
*-------------------------------------------------------------*
*  MESSAGE i000(zdante).

  DATA lv_exec.
*  DO.
*    CHECK lv_exec = 'X'.
*    EXIT.
*  ENDDO.

  PERFORM seleciona_dados.

*-------------------------------------------------------------*
END-OF-SELECTION.
*-------------------------------------------------------------*
*  PERFORM exibe_lista.
  PERFORM exibe_lista_alv.
*  PERFORM grava_arquivo.

*-------------------------------------------------------------*
* Forms
*-------------------------------------------------------------*
*-------------------------------------------------------------*
* FORM seleciona_dados
*-------------------------------------------------------------*
FORM seleciona_dados.

*MESSAGE S000(zdev) DISPLAY LIKE 'S'.

* Busca Lista de Voo
  CALL FUNCTION 'BAPI_FLIGHT_GETLIST'
    EXPORTING
      airline     = p_id
    TABLES
      date_range  = s_date
      flight_list = gt_flight_list
      return      = gt_return.

  PERFORM set_status_flight.


ENDFORM.

FORM set_status_flight.

  LOOP AT gt_flight_list INTO wa_flight_list.
    DATA(lv_tabix) = sy-tabix.

*---Regra de definição de datas
    IF wa_flight_list-flightdate < '20220101'.
      wa_flight_list-status = '@15@'.
      MODIFY gt_flight_list FROM wa_flight_list INDEX lv_tabix.
    ELSE.
      wa_flight_list-status = '@1A@'.
      MODIFY gt_flight_list FROM wa_flight_list INDEX lv_tabix.
    ENDIF.

*---Bloqueio de Companias
    IF wa_flight_list-airlineid = 'SQ'.
      wa_flight_list-status = '@3U@'.
      MODIFY gt_flight_list FROM wa_flight_list INDEX lv_tabix.
    ENDIF.

  ENDLOOP.

ENDFORM.

*-------------------------------------------------------------*
* FORM exibe_lista
*-------------------------------------------------------------*
FORM exibe_lista.
  DATA: lv_total       TYPE bapisfldat-price,
        lv_al_id       TYPE bapisfldat-airline,  "Guarda ultimo ID processado
        lv_al_subtotal TYPE bapisfldat-price.

  DATA: lv_count  TYPE i,
        lv_tabix  TYPE sy-tabix,
        lv_linhas TYPE sy-tabix.

  SORT gt_flight_list BY airlineid.

  WRITE:  sy-uline,
             sy-vline, 2 'Code',
          8  sy-vline, 9 'Name',
          20 sy-vline, 21 'ConId',
          26 sy-vline,
          sy-uline.

*           25'Connec.Number',32 sy-vline,
*           33'Flight date',   sy-vline, sy-uline,/.

  DESCRIBE TABLE gt_flight_list LINES lv_linhas.
  LOOP AT gt_flight_list INTO wa_flight_list.
    lv_tabix = sy-tabix + 1.
    ADD 1 TO lv_count.

    WRITE:   /    sy-vline, 2 wa_flight_list-airlineid(3),
                8 sy-vline, 9 wa_flight_list-airline(10),
               20 sy-vline, 21 wa_flight_list-connectid, 26 sy-vline, sy-uline.
*             33 wa_flight_list-flightdate,   sy-vline,
*                wa_flight_list-airportfr,    sy-vline,
*                wa_flight_list-cityfrom,     sy-vline,
*                wa_flight_list-airportto,    sy-vline,
*                wa_flight_list-cityto,       sy-vline,
*                wa_flight_list-deptime,      sy-vline,
*                wa_flight_list-arrtime,      sy-vline,
*                wa_flight_list-arrdate.

*    IF wa_flight_list-price > 1000.
*      FORMAT COLOR COL_NEGATIVE ON.
*    ELSE.
*      FORMAT COLOR COL_HEADING ON.
*    ENDIF.
*    WRITE:  wa_flight_list-price.
*    FORMAT COLOR OFF.
*
*    WRITE:  wa_flight_list-curr,
*                wa_flight_list-curr_iso.


*    READ TABLE gt_flight_list INTO DATA(l_flight) INDEX lv_tabix.
*    IF sy-subrc <> 0 OR l_flight-airlineid <> wa_flight_list-airlineid.
*      WRITE:'Qtde. Voos:', lv_count, sy-uline, / .
*      CLEAR lv_count.
*    ENDIF.

  ENDLOOP.



ENDFORM.

*-------------------------------------------------------------*
* FORM exibe_lista_alv
*-------------------------------------------------------------*
FORM exibe_lista_alv.

  CONSTANTS c_table TYPE dd02l-tabname VALUE 'BAPISFLDAT'.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo,
        wa_fieldcat TYPE LINE OF lvc_t_fcat.

* Ajusta layout
  wa_layout-zebra      = 'X'.
*  wa_layout-edit       = 'X'.
  wa_layout-no_hgridln = 'X'.

  wa_layout-sel_mode   = 'C'.
  wa_layout-box_fname  = 'BOX'.


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

  PERFORM ajusta_fieldcat TABLES lt_fieldcat.

*  sg_fieldcat-edit = 'X'.
*  MODIFY lt_fieldcat FROM sg_fieldcat TRANSPORTING edit WHERE key IS INITIAL.

*  LOOP AT lt_fieldcat INTO wa_fieldcat.
*    wa_fieldcat-edit = 'X'.
*    MODIFY lt_fieldcat FROM wa_fieldcat INDEX sy-tabix.
*  ENDLOOP.

* Exibe report
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout_lvc            = wa_layout
      i_callback_pf_status_set = 'F_SET_PF_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
      it_fieldcat_lvc          = lt_fieldcat
    TABLES
      t_outtab                 = gt_flight_list
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.

FORM ajusta_fieldcat TABLES lt_fieldcat TYPE lvc_t_fcat.

  LOOP AT lt_fieldcat INTO DATA(wa_fieldcat).
    DATA(lv_tabix) = sy-tabix.
    wa_fieldcat-col_pos = wa_fieldcat-col_pos + 1.
    MODIFY lt_fieldcat FROM wa_fieldcat INDEX lv_tabix.
  ENDLOOP.

* Insere coluna Status
  wa_fieldcat-col_pos = 1.
  wa_fieldcat-fieldname = 'STATUS'.
  wa_fieldcat-datatype = 'CHAR'.
  wa_fieldcat-inttype = 'C'.
  wa_fieldcat-intlen = '4'.
  wa_fieldcat-reptext = 'Status'.
  APPEND wa_fieldcat TO lt_fieldcat.

  SORT lt_fieldcat BY col_pos.

ENDFORM.


*-------------------------------------------------------------*
* FORM f_user_command
*-------------------------------------------------------------*
FORM f_user_command USING  vl_ucomm    LIKE sy-ucomm
                           st_selfield TYPE slis_selfield.
  CASE sy-ucomm.
    WHEN 'DELE'.
      DELETE gt_flight_list WHERE box = 'X'.
      st_selfield-refresh = 'X'.

  ENDCASE.

ENDFORM.

*-------------------------------------------------------------*
* FORM f_set_pf_status
*-------------------------------------------------------------*
FORM f_set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZBARRA'.
ENDFORM.



FORM  grava_arquivo.

  DATA: lt_arquivo TYPE TABLE OF char200,
        wa_arquivo TYPE char200.

  CLEAR lt_arquivo[].
  LOOP AT gt_flight_list INTO wa_flight_list.
    CONCATENATE  wa_flight_list-airlineid
                 wa_flight_list-airline
                 wa_flight_list-connectid
                 wa_flight_list-flightdate
                 wa_flight_list-airportfr
                 wa_flight_list-cityfrom
                 wa_flight_list-airportto
                 wa_flight_list-cityto
                 INTO wa_arquivo SEPARATED BY '|'.
    APPEND wa_arquivo TO lt_arquivo.
  ENDLOOP.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE            = ' '
*     CODEPAGE                = ' '
      filename                = 'C:\SAP\teste.txt'
      filetype                = 'ASC'
*     MODE                    = ' '
*     WK1_N_FORMAT            = ' '
*     WK1_N_SIZE              = ' '
*     WK1_T_FORMAT            = ' '
*     WK1_T_SIZE              = ' '
*     COL_SELECT              = ' '
*     COL_SELECTMASK          = ' '
*     NO_AUTH_CHECK           = ' '
* IMPORTING
*     FILELENGTH              =
    TABLES
      data_tab                = lt_arquivo
*     FIELDNAMES              =
    EXCEPTIONS
      file_open_error         = 1
      file_write_error        = 2
      invalid_filesize        = 3
      invalid_type            = 4
      no_batch                = 5
      unknown_error           = 6
      invalid_table_width     = 7
      gui_refuse_filetransfer = 8
      customer_error          = 9
      no_authority            = 10
      OTHERS                  = 11.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.