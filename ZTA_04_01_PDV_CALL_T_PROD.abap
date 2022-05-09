*&---------------------------------------------------------------------*
*& Report ZTA_04_01_PDV_CALL_T_PROD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zta_04_01_pdv_call_t_prod.

*&---------------------------------------------------------------------*
*& Declarações
*&---------------------------------------------------------------------*

TABLES: t100, zta_04_produto.

* Declara variaveis
DATA: fnome(128), ftipo(3), ftamanho TYPE i.

* Cria tabela
TYPES: BEGIN OF line,
         nome_prod TYPE znome_prod_04,
         tp_unid   TYPE ztp_unid_04,
         vl_unid   TYPE zvl_unid_04,
       END OF line.

DATA: lin     TYPE line,
      tab     TYPE TABLE OF line,
      it_bdc  TYPE TABLE OF bdcdata WITH HEADER LINE,
      it_mess LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
      wa_mess TYPE bdcmsgcoll.

SELECTION-SCREEN BEGIN OF BLOCK p WITH FRAME.

PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY DEFAULT 'C:\'.

SELECTION-SCREEN END OF BLOCK p.

SELECTION-SCREEN BEGIN OF BLOCK rb WITH FRAME.

PARAMETERS: r_up  RADIOBUTTON GROUP rad DEFAULT 'X',
            r_daw RADIOBUTTON GROUP rad.

SELECTION-SCREEN END OF BLOCK rb.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*&---------------------------------------------------------------------*

  PERFORM z_upload_file.

INITIALIZATION.
*&---------------------------------------------------------------------*

START-OF-SELECTION.
*&---------------------------------------------------------------------*

  IF r_up = 'X'.
    PERFORM: z_load_file,           " Lê arquivo txt
             exec_call_transaction. " Executa call transaction

  ELSEIF r_daw = 'X'.
    PERFORM z_download_file.  " Transfere dados do banco para um txt

  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Executa Call Transaction
*&---------------------------------------------------------------------*
FORM exec_call_transaction.

  LOOP AT tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).

    IF <lfs_tab> IS ASSIGNED.

      PERFORM bdc_dynpro USING 'SAPMZTA_04_01_PDV' '9001'.
      PERFORM bdc_field  USING 'BDC_OKCODE' '=CREATE'.
      PERFORM bdc_field  USING 'BDC_CURSOR' 'WA_PRODUTOS-COD_PROD'.

      PERFORM bdc_dynpro USING  'SAPMZTA_04_01_PDV' '9001'.
      PERFORM bdc_field  USING: 'BDC_CURSOR' 'WA_PRODUTOS-COD_PROD',
            'BDC_OKCODE' '=SAVE',
            'WA_PRODUTOS-NOME_PROD' <lfs_tab>-nome_prod,
            'WA_PRODUTOS-TP_UNID'   <lfs_tab>-tp_unid,
            'WA_PRODUTOS-VL_UNID'   <lfs_tab>-vl_unid.

    ENDIF.

  ENDLOOP.

  PERFORM bdc_dynpro USING 'SAPMZTA_04_01_PDV' '9001'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=EXIT'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'WA_PRODUTOS-COD_PROD'.

  PERFORM call_transaction TABLES it_bdc USING 'ZTA_04_PDV'.

  IF sy-subrc EQ 0.
    MESSAGE 'Dados carregados com sucesso!' TYPE 'S'.
  ELSE.
    MESSAGE 'Erro ao carregar os dados!' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
* Rotinas para criação da T_BDC
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.

  CLEAR it_bdc.

  it_bdc-program = program.
  it_bdc-dynpro = dynpro.
  it_bdc-dynbegin = 'X'.
  APPEND it_bdc.

ENDFORM.


FORM bdc_field USING fnam fval.

  IF fval <> ''.

    CLEAR it_bdc.

    it_bdc-fnam = fnam.
*    it_bdc-fval = fval.
    WRITE fval TO it_bdc-fval.
    CONDENSE it_bdc-fval NO-GAPS.
    APPEND it_bdc.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
* FORM CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM call_transaction TABLES t_bdc_data USING w_tcode.

  CALL TRANSACTION w_tcode
  USING t_bdc_data MODE 'A' UPDATE 'S'
  MESSAGES INTO it_mess.

  IF sy-subrc NE 0.

    LOOP AT it_mess.

      PERFORM bdc_message.
      WRITE: / wa_mess.

    ENDLOOP.

  ENDIF.

  CLEAR: it_mess, it_bdc.
  REFRESH: it_mess, it_bdc.

ENDFORM. " CALL_TRANSACTION

*&---------------------------------------------------------------------*
* FORM BDC_MESSAGE
*&---------------------------------------------------------------------*
FORM bdc_message.

  DATA straux4 TYPE text100.

  CLEAR wa_mess.

  SELECT SINGLE * FROM t100
  WHERE sprsl = 'P'
    AND arbgb = it_mess-msgid
    AND msgnr = it_mess-msgnr.

  wa_mess = t100-text.
  straux4 = it_mess-msgv1.
  REPLACE '&' WITH straux4 INTO wa_mess.
  CONDENSE wa_mess.
  straux4 = it_mess-msgv2.
  REPLACE '&' WITH straux4 INTO wa_mess.
  CONDENSE wa_mess.
  straux4 = it_mess-msgv3.
  REPLACE '&' WITH straux4 INTO wa_mess.
  CONDENSE wa_mess.
  straux4 = it_mess-msgv4.
  REPLACE '&' WITH straux4 INTO wa_mess.
  CONDENSE wa_mess.

ENDFORM.

*&---------------------------------------------------------------------*
* Lê o arquivo txt
*&---------------------------------------------------------------------*
FORM  z_load_file.

* Chama a função de Upload
  CALL FUNCTION 'UPLOAD'
    EXPORTING
      codepage            = 'IBM'
      filename            = p_file
      filetype            = 'DAT'
      item                = ' Produtos... '
    IMPORTING
      filesize            = ftamanho
      act_filename        = fnome
      act_filetype        = ftipo
    TABLES
      data_tab            = tab
    EXCEPTIONS
      conversion_error    = 1
      invalid_table_width = 2
      invalid_type        = 3.


ENDFORM.

*&---------------------------------------------------------------------*
* Abre o popup de busca do caminho do arquivo
*&---------------------------------------------------------------------*
FORM z_upload_file.

  DATA: ret    TYPE int4,
        act    TYPE int4,
        t_file TYPE TABLE OF file_table,
        s_file TYPE file_table.

  CLEAR: t_file[].

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Buscar...'
      default_extension       = 'TXT'
      file_filter             = ' TXT (*.txt)|*.txt| XLSX (*.xlsx)|*.xlsx| XLS(*.xls)|*.xls| Todas(*.*)|*.* '
      multiselection          = space
    CHANGING
      file_table              = t_file[]
      rc                      = ret
      user_action             = act
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc EQ 0.
    READ TABLE t_file INTO s_file INDEX 1.
    MOVE s_file TO p_file.
  ENDIF.

ENDFORM.               " z_load_file

*&---------------------------------------------------------------------*
* Transfere dados do banco para um arquivo txt
*&---------------------------------------------------------------------*
FORM z_download_file.

  DATA: it_file TYPE TABLE OF line,
        wa_file TYPE line.

  SELECT nome_prod
         tp_unid
         vl_unid
  INTO TABLE it_file
  FROM zta_04_produto.

  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      codepage                = 'IBM'
      filename                = p_file
      filetype                = 'DAT'
      item                    = ' Produtos... '
    IMPORTING
      filesize                = ftamanho
      act_filename            = fnome
      act_filetype            = ftipo
    TABLES
      data_tab                = it_file
    EXCEPTIONS
      invalid_filesize        = 1
      invalid_table_width     = 2
      invalid_type            = 3
      no_batch                = 4
      unknown_error           = 5
      gui_refuse_filetransfer = 6
      OTHERS                  = 7.
  IF sy-subrc = 0.
    MESSAGE 'Download dos dados realizado com sucesso!' TYPE 'S'.

  ELSE.
    MESSAGE 'Erro ao tentar realizar o download dos dados!' TYPE 'E'.

  ENDIF.

ENDFORM.