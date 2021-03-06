"! <p class="shorttext synchronized" lang="en">Abstract data object character writer</p>
"!
CLASS zcl_io_memory_c_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_c_writer
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_memory_writer
      ABSTRACT METHODS get_result
      get_result_type .

    ALIASES get_result
      FOR zif_io_memory_writer~get_result .
    ALIASES get_result_type
      FOR zif_io_memory_writer~get_result_type .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IO_MEMORY_C_WRITER IMPLEMENTATION.
ENDCLASS.
