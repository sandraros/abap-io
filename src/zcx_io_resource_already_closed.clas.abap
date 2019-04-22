class ZCX_IO_RESOURCE_ALREADY_CLOSED definition
  public
  inheriting from CX_DYNAMIC_CHECK
  create public .

public section.

  constants ZCX_IO_RESOURCE_ALREADY_CLOSED type SOTR_CONC value '000F206A92371DECB18030FE4A64F060' ##NO_TEXT.
  data RESOURCE type ref to OBJECT .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !RESOURCE type ref to OBJECT optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_IO_RESOURCE_ALREADY_CLOSED IMPLEMENTATION.


  method CONSTRUCTOR.

    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = zcx_io_resource_already_closed .
    ENDIF.
    me->resource = resource .

  endmethod.
ENDCLASS.
