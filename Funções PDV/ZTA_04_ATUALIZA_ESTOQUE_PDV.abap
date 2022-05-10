FUNCTION zta_04_atualiza_estoque_pdv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_OPTION) TYPE  ZOPT_04
*"     REFERENCE(I_PROD) TYPE  ZTA_04_ESTOQUE_TT
*"  EXPORTING
*"     REFERENCE(SUBRC) TYPE  SY-SUBRC
*"     REFERENCE(RETURN) TYPE  CHAR100
*"----------------------------------------------------------------------
  DATA: lv_qnt     TYPE dec10_2,
        it_estoque TYPE zta_04_estoque_tt.

*    Busca dados dos produtos em estoque
*"----------------------------------------------------------------------
  IF i_prod[] IS NOT INITIAL.

    SELECT fk_cnpj
           fk_cod_prod
           quant
      INTO TABLE it_estoque
      FROM zta_04_estoque
      FOR ALL ENTRIES IN i_prod
      WHERE fk_cnpj     = i_prod-fk_cnpj
      AND   fk_cod_prod = i_prod-fk_cod_prod.

  ENDIF.


  SORT it_estoque BY fk_cod_prod fk_cnpj.
  LOOP AT i_prod ASSIGNING FIELD-SYMBOL(<lfs_prod>).

    READ TABLE it_estoque ASSIGNING FIELD-SYMBOL(<lfs_est>)
    WITH KEY fk_cod_prod = <lfs_prod>-fk_cod_prod
             fk_cnpj     = <lfs_prod>-fk_cnpj BINARY SEARCH.

*    Inclusão
*"----------------------------------------------------------------------
    IF i_option = 'I' AND <lfs_est> IS ASSIGNED.

      lv_qnt = ( <lfs_est>-quant + <lfs_prod>-quant ).

      UPDATE zta_04_estoque
      FROM @( VALUE #( fk_cnpj     = <lfs_prod>-fk_cnpj
                       fk_cod_prod = <lfs_prod>-fk_cod_prod
                       quant       = lv_qnt ) ).

*    Exclusão
*"----------------------------------------------------------------------
    ELSEIF i_option = 'E' AND <lfs_est> IS ASSIGNED.

      lv_qnt = ( <lfs_est>-quant - <lfs_prod>-quant ).

      UPDATE zta_04_estoque
      FROM @( VALUE #( fk_cnpj     = <lfs_prod>-fk_cnpj
      fk_cod_prod = <lfs_prod>-fk_cod_prod
      quant       = lv_qnt ) ).

    ELSE.
      return = 'ERROR:    Option inexistente'.

    ENDIF.

  ENDLOOP.

ENDFUNCTION.