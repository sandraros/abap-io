"! <p class="shorttext synchronized" lang="en">Abstract file character reader</p>
CLASS zcl_io_file_c_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_c_reader
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_file_reader .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_file_c_reader IMPLEMENTATION.
ENDCLASS.
