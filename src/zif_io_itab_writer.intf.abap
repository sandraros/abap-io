"! <p class="shorttext synchronized" lang="en">Internal table writer</p>
"!
INTERFACE zif_io_itab_writer
  PUBLIC .


  INTERFACES zif_io_close_resource .
  INTERFACES zif_io_memory_writer .
  INTERFACES zif_io_writer .

  ALIASES get_result
    FOR zif_io_memory_writer~get_result .
  ALIASES get_result_type
    FOR zif_io_memory_writer~get_result_type .

  METHODS bind_result_area
    IMPORTING
      string_line_length  TYPE i DEFAULT 255
    EXPORTING
      table               TYPE STANDARD TABLE
      length_of_last_line TYPE i
      length              TYPE i.

  METHODS get_result_table
    EXPORTING
      !table               TYPE STANDARD TABLE
      !length_of_last_line TYPE i
      !length              TYPE i .

  METHODS get_max_line_length
    EXPORTING
      !line_length TYPE i .
ENDINTERFACE.
