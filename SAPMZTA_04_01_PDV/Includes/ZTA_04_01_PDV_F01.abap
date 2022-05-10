*&---------------------------------------------------------------------*
*& Include          ZTA_04_01_PDV_F01
*&---------------------------------------------------------------------*

FORM carrega_prod.

  MODIFY zta_04_produto FROM wa_produtos.

ENDFORM.

FORM z_control_fields_cli.

  IF gv_block_cod_cli = 'X'.
    LOOP AT SCREEN.
      CHECK screen-group1 = 'PRD'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.

  ELSEIF gv_block_cod_cli = 'A'.
    LOOP AT SCREEN.
      CHECK screen-group1 = 'COD'.
      screen-input = '1'.
      MODIFY SCREEN.

    ENDLOOP.

    LOOP AT SCREEN.
      CHECK screen-group1 = 'CHK'.
      screen-invisible = '0'.
      MODIFY SCREEN.

    ENDLOOP.

  ELSEIF gv_block_cod_cli = 'B'.
    LOOP AT SCREEN.
      CHECK screen-group1 = 'COD'.
      screen-input = '1'.
      MODIFY SCREEN.

    ENDLOOP.

    LOOP AT SCREEN.
      CHECK screen-group1 = 'CHK'.
      screen-invisible = '0'.
      MODIFY SCREEN.

    ENDLOOP.

    LOOP AT SCREEN.
      CHECK screen-group1 = 'PRD'.
      screen-input = '1'.
      MODIFY SCREEN.

    ENDLOOP.

  ELSE.
    LOOP AT SCREEN.
      CHECK screen-group1 = 'PRD'.
      screen-input = '1'.
      MODIFY SCREEN.

    ENDLOOP.
  ENDIF.


ENDFORM.

FORM busca_prod.

  SELECT *
    INTO wa_produtos
    FROM zta_04_produto
    WHERE zta_04_produto~cod_prod = wa_produtos-cod_prod.
  ENDSELECT.


ENDFORM.

FORM del_prod.
  DATA: l_msg       TYPE char100,
        l_t_msg     TYPE char100,
        p_wa_answer TYPE c.

  l_t_msg = 'Atenção!'.
  l_msg   = 'Tem certeza que deseja deletar esse registro?'.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = l_t_msg
      text_question         = l_msg
      text_button_1         = 'Yes'(002)
      text_button_2         = 'No'(005)
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = p_wa_answer.

  IF p_wa_answer = 1.

    DELETE FROM zta_04_produto WHERE zta_04_produto~cod_prod = wa_produtos-cod_prod.

    MESSAGE: 'Produto deletado com sucesso!' TYPE 'S'.

  ELSE.
  ENDIF.

ENDFORM.

FORM imprime_alv TABLES gt_list.

  CONSTANTS c_table TYPE dd02l-tabname VALUE 'zta_04_produto'.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo,
        wa_fieldcat TYPE LINE OF lvc_t_fcat.

* Ajusta layout
  wa_layout-zebra      = 'X'.
*  wa_layout-edit       = 'X'.
  wa_layout-no_hgridln = 'X'.

*  wa_layout-sel_mode   = 'C'.
*  wa_layout-box_fname  = 'BOX'.

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


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout_lvc            = wa_layout
      i_callback_pf_status_set = 'F_SET_PF_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
      it_fieldcat_lvc          = lt_fieldcat
    TABLES
      t_outtab                 = gt_list
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.