*&---------------------------------------------------------------------*
*& Include ZTA_04_01_PDV_TOP                        - Module Pool      SAPMZTA_04_01_PDV
*&---------------------------------------------------------------------*
PROGRAM sapmzta_04_01_pdv.

DATA: wa_produtos      TYPE zta_04_produto,
      gv_code          TYPE sy-ucomm,
      gv_code9002      TYPE sy-ucomm,
      gv_block_cod_cli TYPE c VALUE 'X',
      gv_test_op       TYPE char10.