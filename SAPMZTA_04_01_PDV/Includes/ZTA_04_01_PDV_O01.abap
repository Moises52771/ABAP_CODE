*&---------------------------------------------------------------------*
*& Include          ZTA_04_01_PDV_O01
*&---------------------------------------------------------------------*

MODULE inicializa_tela OUTPUT.

  PERFORM z_control_fields_cli.

ENDMODULE.

MODULE status_9001 OUTPUT.

  SET PF-STATUS 'STATUS_9001'.

ENDMODULE.