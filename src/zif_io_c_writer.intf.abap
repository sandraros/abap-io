"! <p class="shorttext synchronized" lang="en">Character writer</p>
INTERFACE zif_io_c_writer
  PUBLIC .


  INTERFACES zif_io_close_resource .
  INTERFACES zif_io_writer .

  ALIASES close
    FOR zif_io_writer~close .
  ALIASES flush
    FOR zif_io_writer~flush .
  ALIASES is_closed
    FOR zif_io_writer~is_closed .
  ALIASES is_x_writer
    FOR zif_io_writer~is_x_writer .

  METHODS write
    IMPORTING
      !data TYPE string
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_error .

ENDINTERFACE.
