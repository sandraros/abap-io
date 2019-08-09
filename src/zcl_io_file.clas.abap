"! <p class="shorttext synchronized" lang="en">Abstract file super class</p>
"!
CLASS zcl_io_file DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_file ALL METHODS ABSTRACT.

    ALIASES close
      FOR zif_io_file~close .
    ALIASES open
      FOR zif_io_file~open .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_file IMPLEMENTATION.

ENDCLASS.
