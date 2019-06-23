"! <p class="shorttext synchronized" lang="en">Byte writer</p>
"!
INTERFACE zif_io_x_writer
  PUBLIC .


  INTERFACES zif_io_close_resource .
  INTERFACES zif_io_writer .

  ALIASES close     FOR zif_io_writer~close .
  ALIASES is_closed FOR zif_io_writer~is_closed .
  ALIASES flush     FOR zif_io_writer~flush.


  METHODS write
    IMPORTING
      !data TYPE xstring
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_error .

ENDINTERFACE.
