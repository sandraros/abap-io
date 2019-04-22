"! <p class="shorttext synchronized" lang="en">Reader methods</p>
"!
INTERFACE zif_io_reader
  PUBLIC .


  INTERFACES zif_io_close_resource .

  ALIASES close
    FOR zif_io_close_resource~close .
  ALIASES is_closed
    FOR zif_io_close_resource~is_closed .

  METHODS skip
    IMPORTING
      !length TYPE abap_msize
    RAISING
      zcx_io_parameter_invalid_range
      zcx_io_resource_already_closed
      zcx_io_stream_error .
  METHODS is_reset_supported
    RETURNING
      VALUE(result) TYPE abap_bool .
  METHODS set_mark
    RAISING
      zcx_io_stream_position_error
      zcx_io_resource_already_closed .
  METHODS delete_mark
    RAISING
      zcx_io_resource_already_closed .
  METHODS is_mark_supported
    RETURNING
      VALUE(res) TYPE abap_bool .
  METHODS reset_to_mark
    RAISING
      zcx_io_stream_position_error
      zcx_io_resource_already_closed .
  METHODS data_available
    RETURNING
      VALUE(available) TYPE abap_bool
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_error .
  METHODS read
    IMPORTING
      !length    TYPE abap_msize
    EXPORTING
      !read_data TYPE any
    RAISING
      zcx_io_parameter_invalid_range
      zcx_io_parameter_invalid_type
      zcx_io_resource_already_closed
      zcx_io_stream_error .
  METHODS is_x_reader
    RETURNING
      VALUE(result) TYPE abap_bool
    RAISING
      zcx_io_stream_state_error .
  METHODS reset
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_position_error .
ENDINTERFACE.
