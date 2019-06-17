"! <p class="shorttext synchronized" lang="en">Memory writer</p>
"!
INTERFACE zif_io_memory_writer
  PUBLIC .


  INTERFACES zif_io_close_resource .
  INTERFACES zif_io_writer .

  METHODS get_result
    EXPORTING
      !result              TYPE any
      !length_of_last_line TYPE i
      "! Total length
      !length              TYPE i
    RAISING
      zcx_io_parameter_invalid_type .

  METHODS get_result_type
    EXPORTING
      !result_type TYPE REF TO cl_abap_datadescr .

ENDINTERFACE.
