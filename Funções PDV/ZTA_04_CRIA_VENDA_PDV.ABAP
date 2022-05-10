FUNCTION zta_04_cria_venda_pdv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CPF_VENDEDOR) TYPE  ZCPF_04
*"     REFERENCE(CNPJ_ESTAB) TYPE  ZCNPJ_04
*"     REFERENCE(DOC_CLIENT) TYPE  ZCOD_CLT_04 OPTIONAL
*"     REFERENCE(FORMA_PAG) TYPE  ZFRM_PAG
*"     REFERENCE(ITENS) TYPE  ZTA_04_ITENS_TT
*"  EXPORTING
*"     REFERENCE(RETURN) TYPE  CHAR100
*"     REFERENCE(COD_VENDA) TYPE  Z_COD_VENDA_04
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_prod,
           cod_prod TYPE zta_04_produto-cod_prod,
           quant    TYPE zta_04_estoque-quant,
         END OF ty_prod.

  DATA: it_prod    TYPE TABLE OF ty_prod,
        lv_test(1) TYPE          c,
        subrc      TYPE          sy-subrc,
        lv_est     TYPE          zta_04_estoque_tt.

*" Busca os dados dos produtos
*"----------------------------------------------------------------------
  IF itens[] IS NOT INITIAL.

    SELECT zta_04_estoque~fk_cod_prod
           zta_04_estoque~quant
      INTO TABLE it_prod
      FROM zta_04_estoque AS zta_04_estoque
      FOR ALL ENTRIES IN itens
      WHERE zta_04_estoque~fk_cod_prod = itens-cod_prod.

  ELSE.
    return = 'ERROR:    A venda não possui itens'.

  ENDIF.

  SORT: it_prod BY cod_prod.

*"  Validação do produto em estoque
*"--------------------------------
  LOOP AT itens ASSIGNING FIELD-SYMBOL(<lfs_itens>).

    READ TABLE it_prod ASSIGNING FIELD-SYMBOL(<lfs_prod>)
    WITH KEY cod_prod = <lfs_itens>-cod_prod BINARY SEARCH.

    IF <lfs_itens>-quant <= <lfs_prod>-quant
      AND itens[] IS NOT INITIAL.

      lv_test = 'X'.

    ELSE.
      lv_test = 'I'.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF lv_test = 'X'.

*"   Função que realiza a venda
*"  --------------------------------
    CALL FUNCTION 'ZTA_04_CADASTRA_VENDA_PDV'
      EXPORTING
        cpf_vendedor = cpf_vendedor
        cnpj_estab   = cnpj_estab
        doc_client   = doc_client
        forma_pag    = forma_pag
        itens        = itens
      IMPORTING
        subrc        = subrc
        e_at_est     = lv_est.
        cod_venda    = cod_venda.

    IF subrc = 0.

*"     Função que atualiza o estoque
*"    --------------------------------

      CALL FUNCTION 'ZTA_04_ATUALIZA_ESTOQUE_PDV'
        EXPORTING
          i_option = 'E'
          i_prod   = lv_est
*       IMPORTING
*         SUBRC    =
        .

      return = 'SUCESS:    Venda realizada com sucesso'.
    ELSE.
      return = 'ERROR:    Venda não realizada'.
    ENDIF.

  ELSEIF lv_test = 'I'.
    return = 'ERROR:    Não a quantidade sulficiente do produto em estoque!'.
  ENDIF.

ENDFUNCTION.