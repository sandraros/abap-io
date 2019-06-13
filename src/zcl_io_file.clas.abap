"! <p class="shorttext synchronized" lang="en">File super class</p>
"!
CLASS zcl_io_file DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_file .

    ALIASES close
      FOR zif_io_file~close .
    ALIASES open
      FOR zif_io_file~open .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_file IMPLEMENTATION.


  METHOD zif_io_file~close.
  ENDMETHOD.


  METHOD zif_io_file~open.
  ENDMETHOD.
ENDCLASS.
