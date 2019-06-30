"! <p class="shorttext synchronized" lang="en">Invalid parameter</p>
"!
CLASS zcx_io_parameter_invalid DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS zcx_io_parameter_invalid TYPE sotr_conc VALUE '06690F3C8163FF17E10000000A11447B' ##NO_TEXT.
    DATA parameter TYPE string .

    METHODS constructor
      IMPORTING
        !textid          LIKE textid OPTIONAL
        !previous        LIKE previous OPTIONAL
        VALUE(parameter) TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_io_parameter_invalid IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid .
    ENDIF.
    me->parameter = parameter .

  ENDMETHOD.
ENDCLASS.
