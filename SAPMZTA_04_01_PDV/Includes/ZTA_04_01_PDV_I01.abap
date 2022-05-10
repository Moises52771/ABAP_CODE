*&---------------------------------------------------------------------*
*& Include          ZTA_04_01_PDV_I01
*&---------------------------------------------------------------------*

DATA: gt_prod TYPE TABLE OF zta_04_produto, " Table used into ALV to show data
      go_alv  TYPE REF TO   cl_salv_table. " ALV object


MODULE user_command_9002.

  CASE gv_code9002.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 9001.

  ENDCASE.

ENDMODULE.

MODULE user_command_9001.

  CASE gv_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

*&  BT Create
*&---------------------------------------------------------------------*
    WHEN 'CREATE'.

      CLEAR gv_block_cod_cli.
      gv_test_op = 'CREATE'.

*&  BT Save
*&---------------------------------------------------------------------*
    WHEN 'SAVE'.

      CASE gv_test_op.
        WHEN 'CREATE'.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ZCODPRO04'
            IMPORTING
              number      = wa_produtos-cod_prod.

          PERFORM carrega_prod.
          gv_block_cod_cli = 'X'.

          IF sy-subrc = 0.
            MESSAGE: 'Produto cadastrado com sucesso!' TYPE 'S'.
          ELSE.
            MESSAGE: 'Erro não foi possivél cadastrar o produto!' TYPE 'E'.
          ENDIF.

        WHEN 'EDIT'.
          PERFORM carrega_prod.
          gv_block_cod_cli = 'X'.
          IF sy-subrc = 0.
            MESSAGE: 'Edição realizada com sucesso!' TYPE 'S'.
          ENDIF.

      ENDCASE.

*&  BT Edit
*&---------------------------------------------------------------------*
    WHEN 'EDIT'.

      gv_block_cod_cli = 'A'.
      gv_test_op = 'EDIT'.
      PERFORM carrega_prod.

*&  BT Check
*&---------------------------------------------------------------------*
    WHEN 'CHECK'.

      CASE gv_test_op.
        WHEN 'EDIT'.
          gv_block_cod_cli = 'B'.
          PERFORM: busca_prod.

        WHEN 'DELETE'.

          gv_test_op = 'DELETE'.
          PERFORM: busca_prod.
          gv_block_cod_cli = 'A'.

          IF sy-subrc = 0.

            gv_test_op = 'DELETE'.
            gv_block_cod_cli = 'A'.

          ELSE.

            gv_test_op = ''.
            gv_block_cod_cli = 'A'.
            MESSAGE 'Produto não encontrado!' TYPE 'E'.

          ENDIF.

      ENDCASE.



*&  BT Delete
*&---------------------------------------------------------------------*
    WHEN 'DELETE'.

      IF gv_test_op = 'DELETE'.

        gv_block_cod_cli = 'A'.
        gv_test_op = 'DELETE'.

        PERFORM: del_prod.

      ELSE.

        gv_block_cod_cli = 'A'.
        gv_test_op = 'DELETE'.

      ENDIF.

*&  BT List
*&---------------------------------------------------------------------*
    WHEN 'LIST'.

      gv_test_op = 'LIST'.

      SELECT *
        FROM zta_04_produto
        INTO TABLE gt_prod.

      TRY .
          PERFORM imprime_alv TABLES gt_prod.

        CATCH cx_salv_msg.
          WRITE: / 'ALV error'.
      ENDTRY.


*&  BT Execute
*&---------------------------------------------------------------------*
    WHEN 'EXECUTE'.

      LEAVE TO SCREEN 9002.

*      CASE gv_test_op.
*        WHEN 'CREATE'.
*
*          IF  wa_produtos-nome_prod IS NOT INITIAL
*          AND wa_produtos-tp_unid   IS NOT INITIAL.
*
*            CALL FUNCTION 'NUMBER_GET_NEXT'
*              EXPORTING
*                nr_range_nr = '01'
*                object      = 'ZCODPRO04'
*              IMPORTING
*                number      = wa_produtos-cod_prod.
*
*            PERFORM carrega_prod.
*            gv_block_cod_cli = 'X'.
*
*            IF sy-subrc = 0.
*              MESSAGE: 'Produto cadastrado com sucesso!' TYPE 'S'.
*            ELSE.
*              MESSAGE: 'Erro não foi possivél cadastrar o produto!' TYPE 'E'.
*            ENDIF.
*
*          ELSE.
*
*            MESSAGE: 'Porfavor preencha todos os campos!' TYPE 'I'.
*
*          ENDIF.
*
*        WHEN 'EDIT'.
*          PERFORM carrega_prod.
*          gv_block_cod_cli = 'X'.
*          IF sy-subrc = 0.
*            MESSAGE: 'Edição realizada com sucesso!' TYPE 'S'.
*          ENDIF.
*
*      ENDCASE.
  ENDCASE.

ENDMODULE.

MODULE valida.

  IF wa_produtos-nome_prod = '' OR wa_produtos-tp_unid = ''.

    MESSAGE 'Preencha todos os campos!' TYPE 'E'.

  ENDIF.

ENDMODULE.