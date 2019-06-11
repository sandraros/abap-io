"! <p class="shorttext synchronized" lang="en">ABAP I/O exception</p>
"!
CLASS zcx_io_resource_already_closed DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS zcx_io_resource_already_closed TYPE sotr_conc VALUE '000F206A92371DECB18030FE4A64F060' ##NO_TEXT.
    DATA resource TYPE REF TO object .

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        !resource TYPE REF TO object OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_IO_RESOURCE_ALREADY_CLOSED IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = zcx_io_resource_already_closed .
    ENDIF.
    me->resource = resource .

  ENDMETHOD.
ENDCLASS.
