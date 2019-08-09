"! <p class="shorttext synchronized" lang="en">Abstract file byte writer</p>
CLASS zcl_io_file_x_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_x_writer
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_file_writer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_file_x_writer IMPLEMENTATION.
ENDCLASS.
