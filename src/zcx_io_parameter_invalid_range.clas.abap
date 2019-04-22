class ZCX_IO_PARAMETER_INVALID_RANGE definition
  public
  inheriting from ZCX_IO_PARAMETER_INVALID
  create public .

public section.

  constants ZCX_IO_PARAMETER_INVALID_RANGE type SOTR_CONC value '9FB7E23B75B7157FE10000000A11447B' ##NO_TEXT.
  data VALUE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      value(PARAMETER) type STRING optional
      value(VALUE) type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_IO_PARAMETER_INVALID_RANGE IMPLEMENTATION.


  method CONSTRUCTOR.

    CALL METHOD super->constructor
      EXPORTING
        textid    = textid
        previous  = previous
        parameter = parameter.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid_range .
    ENDIF.
    me->value = value .

  endmethod.
ENDCLASS.
