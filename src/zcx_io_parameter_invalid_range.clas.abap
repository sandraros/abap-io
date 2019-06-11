"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcx_io_parameter_invalid_range DEFINITION
  PUBLIC
  INHERITING FROM zcx_io_parameter_invalid
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS zcx_io_parameter_invalid_range TYPE sotr_conc VALUE '9FB7E23B75B7157FE10000000A11447B' ##NO_TEXT.
    DATA value TYPE string .

    METHODS constructor
      IMPORTING
        !textid          LIKE textid OPTIONAL
        !previous        LIKE previous OPTIONAL
        VALUE(parameter) TYPE string OPTIONAL
        VALUE(value)     TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_io_parameter_invalid_range IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        textid    = textid
        previous  = previous
        parameter = parameter.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid_range .
    ENDIF.
    me->value = value .

  ENDMETHOD.
ENDCLASS.
