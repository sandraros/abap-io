"! <p class="shorttext synchronized" lang="en">Abstract data object byte reader</p>
"!
CLASS zcl_io_memory_x_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_x_reader
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_memory_reader .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_memory_x_reader IMPLEMENTATION.
ENDCLASS.
