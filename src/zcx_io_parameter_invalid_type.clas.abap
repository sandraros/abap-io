class ZCX_IO_PARAMETER_INVALID_TYPE definition
  public
  inheriting from ZCX_IO_PARAMETER_INVALID
  create public .

public section.

  constants ZCX_IO_PARAMETER_INVALID_TYPE type SOTR_CONC value '0017206A92371DECB18030FE4A64F060' ##NO_TEXT.
  data TYPE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !PARAMETER type STRING optional
      !TYPE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_IO_PARAMETER_INVALID_TYPE IMPLEMENTATION.


  method CONSTRUCTOR.

    CALL METHOD super->constructor
      EXPORTING
        textid    = textid
        previous  = previous
        parameter = parameter.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid_type .
    ENDIF.
    me->type = type .

  endmethod.
ENDCLASS.
