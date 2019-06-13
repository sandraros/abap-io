"! <p class="shorttext synchronized" lang="en">Abstract data object byte writer</p>
CLASS zcl_io_memory_x_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_x_writer
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_memory_writer .

    ALIASES get_result
      FOR zif_io_memory_writer~get_result .
    ALIASES get_result_type
      FOR zif_io_memory_writer~get_result_type .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_memory_x_writer IMPLEMENTATION.


  METHOD zif_io_memory_writer~get_result.
  ENDMETHOD.


  METHOD zif_io_memory_writer~get_result_type.
  ENDMETHOD.
ENDCLASS.
