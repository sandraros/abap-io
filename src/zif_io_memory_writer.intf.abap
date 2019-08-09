"! <p class="shorttext synchronized" lang="en">Data Object Writer</p>
"!
INTERFACE zif_io_memory_writer
  PUBLIC .


  INTERFACES zif_io_close_resource .
  INTERFACES zif_io_writer .

  "! <p class="shorttext synchronized" lang="en">GET_RESULT</p>
  "!
  "! @parameter result                        | <p class="shorttext synchronized" lang="en">RESULT</p>
  "! @parameter length_of_last_line           | <p class="shorttext synchronized" lang="en">Length of last line (if internal table)</p>
  "! @parameter length                        | <p class="shorttext synchronized" lang="en">Total length</p>
  "! @raising   zcx_io_parameter_invalid_type | <p class="shorttext synchronized" lang="en">ZCX_IO_PARAMETER_INVALID_TYPE</p>
  "! @raising   zcx_io_stream_state_error     | <p class="shorttext synchronized" lang="en">A data object was bound so GET_RESULT is non-sense, etc.</p>
  METHODS get_result
    EXPORTING
      !result              TYPE any
      !length_of_last_line TYPE i
      !length              TYPE i
    RAISING
      zcx_io_parameter_invalid_type
      zcx_io_stream_state_error .

  METHODS get_result_type
    EXPORTING
      !result_type TYPE REF TO cl_abap_datadescr .

ENDINTERFACE.
