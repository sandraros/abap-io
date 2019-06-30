"! <p class="shorttext synchronized" lang="en">Invalid parameter type</p>
"!
CLASS zcx_io_parameter_invalid_type DEFINITION
  PUBLIC
  INHERITING FROM zcx_io_parameter_invalid
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS zcx_io_parameter_invalid_type TYPE sotr_conc VALUE '0017206A92371DECB18030FE4A64F060' ##NO_TEXT.
    DATA type TYPE string .

    METHODS constructor
      IMPORTING
        !textid    LIKE textid OPTIONAL
        !previous  LIKE previous OPTIONAL
        !parameter TYPE string OPTIONAL
        !type      TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_io_parameter_invalid_type IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        textid    = textid
        previous  = previous
        parameter = parameter.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid_type .
    ENDIF.
    me->type = type .

  ENDMETHOD.
ENDCLASS.
