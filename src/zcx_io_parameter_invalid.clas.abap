class ZCX_IO_PARAMETER_INVALID definition
  public
  inheriting from CX_DYNAMIC_CHECK
  create public .

public section.

  constants ZCX_IO_PARAMETER_INVALID type SOTR_CONC value '06690F3C8163FF17E10000000A11447B' ##NO_TEXT.
  data PARAMETER type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      value(PARAMETER) type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_IO_PARAMETER_INVALID IMPLEMENTATION.


  method CONSTRUCTOR.

    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid .
    ENDIF.
    me->parameter = parameter .

  endmethod.
ENDCLASS.
