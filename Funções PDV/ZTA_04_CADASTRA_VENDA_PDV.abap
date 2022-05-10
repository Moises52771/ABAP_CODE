FUNCTION zta_04_cadastra_venda_pdv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CPF_VENDEDOR) TYPE  ZCPF_04
*"     REFERENCE(CNPJ_ESTAB) TYPE  ZCNPJ_04
*"     REFERENCE(DOC_CLIENT) TYPE  ZCOD_CLT_04 OPTIONAL
*"     REFERENCE(FORMA_PAG) TYPE  ZFRM_PAG
*"     REFERENCE(ITENS) TYPE  ZTA_04_ITENS_TT
*"  EXPORTING
*"     REFERENCE(SUBRC) TYPE  SY-SUBRC
*"     REFERENCE(E_AT_EST) TYPE  ZTA_04_ESTOQUE_TT
*"     REFERENCE(COD_VENDA) TYPE  Z_COD_VENDA_04
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_prod,
           vl_unid  TYPE zta_04_produto-vl_unid,
           cod_prod TYPE zta_04_produto-cod_prod,
         END OF ty_prod.

  TABLES: zta_04_venda,
          zta_04_vend_prod.

  DATA: lv_vltotal TYPE          zvalor_tt_04,
        it_prod    TYPE TABLE OF ty_prod,
        lv_cont    TYPE          int1,
        wa_lv_est  TYPE          zta_04_estoque_st,
        cod_vend   TYPE          numc10.

*" Busca os dados dos produtos
*"----------------------------------------------------------------------
  IF itens[] IS NOT INITIAL.

    SELECT zta_04_produto~vl_unid
           zta_04_produto~cod_prod
    INTO TABLE it_prod
    FROM zta_04_produto AS zta_04_produto
    FOR ALL ENTRIES IN itens
    WHERE zta_04_produto~cod_prod = itens-cod_prod.

  ENDIF.

*" Cria codigo da venda
*"----------------------------------------------------------------------
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZCODVEND04'
    IMPORTING
      number      = cod_vend.


*" Busca o valor total da venda
*"----------------------------------------------------------------------
  SORT: it_prod BY cod_prod.
  lv_vltotal = 0.
  lv_cont = 0.

  LOOP AT itens ASSIGNING FIELD-SYMBOL(<lfs_itens>).

    READ TABLE it_prod ASSIGNING FIELD-SYMBOL(<lfs_prod>)
    WITH KEY cod_prod = <lfs_itens>-cod_prod BINARY SEARCH.

    IF sy-subrc = 0.

      lv_vltotal = lv_vltotal + ( <lfs_prod>-vl_unid * <lfs_itens>-quant ).

    ENDIF.

*"  Cadastra venda no banco
*"  ----------------------------------------------------------------------
    zta_04_vend_prod-fk_cod_venda = cod_vend.
    zta_04_vend_prod-cod_iten     = lv_cont.
    zta_04_vend_prod-fk_cod_prod  = <lfs_itens>-cod_prod.
    zta_04_vend_prod-quant        = <lfs_itens>-quant.

    INSERT INTO zta_04_vend_prod VALUES zta_04_vend_prod.

    wa_lv_est-fk_cnpj     = cnpj_estab.
    wa_lv_est-fk_cod_prod = <lfs_itens>-cod_prod.
    wa_lv_est-quant       = <lfs_itens>-quant.

    APPEND wa_lv_est TO e_at_est.

    lv_cont = lv_cont + 1.

  ENDLOOP.

  zta_04_venda-cod_venda  = cod_vend.
  zta_04_venda-fk_cpf_fun = cpf_vendedor.
  zta_04_venda-fk_cnpj    = cnpj_estab.
  zta_04_venda-data       = sy-datum.
  zta_04_venda-hora       = sy-uzeit.
  zta_04_venda-fk_cod_clt = doc_client.
  zta_04_venda-valor_tt   = lv_vltotal.
  zta_04_venda-frm_pag    = forma_pag.

  INSERT INTO zta_04_venda VALUES zta_04_venda.

  subrc     = sy-subrc.
  cod_venda = cod_vend.

ENDFUNCTION.