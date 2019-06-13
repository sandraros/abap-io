"! <p class="shorttext synchronized" lang="en">Abstract data object character reader</p>
CLASS zcl_io_memory_c_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_c_reader
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_memory_reader .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_memory_c_reader IMPLEMENTATION.
ENDCLASS.
