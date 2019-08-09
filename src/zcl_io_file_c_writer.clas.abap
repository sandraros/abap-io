"! <p class="shorttext synchronized" lang="en">Abstract file character writer</p>
CLASS zcl_io_file_c_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_c_writer
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_file_writer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IO_FILE_C_WRITER IMPLEMENTATION.
ENDCLASS.
