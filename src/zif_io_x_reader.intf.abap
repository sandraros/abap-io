"! <p class="shorttext synchronized" lang="en">Byte reader</p>
"!
INTERFACE zif_io_x_reader
  PUBLIC .


  INTERFACES zif_io_close_resource .
  INTERFACES zif_io_reader .

  ALIASES close
    FOR zif_io_reader~close .
  ALIASES data_available
    FOR zif_io_reader~data_available .
  ALIASES delete_mark
    FOR zif_io_reader~delete_mark .
  ALIASES is_closed
    FOR zif_io_reader~is_closed .
  ALIASES is_mark_supported
    FOR zif_io_reader~is_mark_supported .
  ALIASES is_reset_supported
    FOR zif_io_reader~is_reset_supported .
  ALIASES is_x_reader
    FOR zif_io_reader~is_x_reader .
  ALIASES reset
    FOR zif_io_reader~reset .
  ALIASES reset_to_mark
    FOR zif_io_reader~reset_to_mark .
  ALIASES set_mark
    FOR zif_io_reader~set_mark .
  ALIASES skip
    FOR zif_io_reader~skip .
  ALIASES is_auto_close_performed
    FOR zif_io_reader~is_auto_close_performed .

  METHODS read
    IMPORTING
      !length       TYPE numeric
    RETURNING
      VALUE(result) TYPE xstring
    RAISING
      zcx_io_parameter_invalid_range
      zcx_io_resource_already_closed
      zcx_io_stream_error .
ENDINTERFACE.
