"! <p class="shorttext synchronized" lang="en">Back-end file byte writer</p>
"!
CLASS zcl_io_backend_x_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_file_x_writer
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_io_x_writer .

  PUBLIC SECTION.

    INTERFACES zif_io_backend_writer .

    METHODS constructor
      IMPORTING
        !file TYPE REF TO zcl_io_backend
      RAISING
        zcx_io_parameter_invalid .

  PROTECTED SECTION.
  PRIVATE SECTION.

  METHODS write_internal
  importing
  data type xstring.

ENDCLASS.



CLASS zcl_io_backend_x_writer IMPLEMENTATION.


  METHOD constructor.

super->constructor( ).

  ENDMETHOD.


  METHOD write_internal.


  ENDMETHOD.


ENDCLASS.
