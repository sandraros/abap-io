"! <p class="shorttext synchronized" lang="en">Writer</p>
"!
INTERFACE zif_io_writer
  PUBLIC .


  INTERFACES zif_io_close_resource .

  ALIASES close
    FOR zif_io_close_resource~close .
  ALIASES is_closed
    FOR zif_io_close_resource~is_closed .

  METHODS write
    IMPORTING
      !data TYPE any
    RAISING
      zcx_io_parameter_invalid_type
      zcx_io_resource_already_closed
      zcx_io_stream_error .
  METHODS flush
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_error .
  METHODS is_x_writer
    RETURNING
      VALUE(result) TYPE abap_bool .
ENDINTERFACE.
