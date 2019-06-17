"! <p class="shorttext synchronized" lang="en">String writer</p>
"!
INTERFACE zif_io_string_writer
  PUBLIC .


  INTERFACES zif_io_close_resource .
  INTERFACES zif_io_memory_writer .
  INTERFACES zif_io_writer .

  ALIASES get_result
    FOR zif_io_memory_writer~get_result .
  ALIASES get_result_type
    FOR zif_io_memory_writer~get_result_type .
ENDINTERFACE.
