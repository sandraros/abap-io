"! <p class="shorttext synchronized" lang="en">Reader methods</p>
"!
INTERFACE zif_io_reader
  PUBLIC .


  INTERFACES zif_io_close_resource .

  ALIASES close
    FOR zif_io_close_resource~close .
  ALIASES is_closed
    FOR zif_io_close_resource~is_closed .

  "! <p class="shorttext synchronized" lang="en">SKIP</p>
  "!
  "! @parameter length                         | <p class="shorttext synchronized" lang="en">LENGTH</p>
  "! @raising   zcx_io_parameter_invalid_range | <p class="shorttext synchronized" lang="en">ZCX_IO_PARAMETER_INVALID_RANGE</p>
  "! @raising   zcx_io_resource_already_closed | <p class="shorttext synchronized" lang="en">ZCX_IO_RESOURCE_ALREADY_CLOSED</p>
  "! @raising   zcx_io_stream_error            | <p class="shorttext synchronized" lang="en">ZCX_IO_STREAM_ERROR</p>
  METHODS skip
    IMPORTING
      !length TYPE abap_msize
    RAISING
      zcx_io_parameter_invalid_range
      zcx_io_resource_already_closed
      zcx_io_stream_error .

  "! <p class="shorttext synchronized" lang="en">IS_RESET_SUPPORTED</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en">RESULT</p>
  METHODS is_reset_supported
    RETURNING
      VALUE(result) TYPE abap_bool .

  "! <p class="shorttext synchronized" lang="en">SET_MARK</p>
  "!
  "! @raising   zcx_io_stream_position_error   | <p class="shorttext synchronized" lang="en">ZCX_IO_STREAM_POSITION_ERROR</p>
  "! @raising   zcx_io_resource_already_closed | <p class="shorttext synchronized" lang="en">ZCX_IO_RESOURCE_ALREADY_CLOSED</p>
  METHODS set_mark
    RAISING
      zcx_io_stream_position_error
      zcx_io_resource_already_closed .

  "! <p class="shorttext synchronized" lang="en">DELETE_MARK</p>
  "!
  "! @raising   zcx_io_resource_already_closed | <p class="shorttext synchronized" lang="en">ZCX_IO_RESOURCE_ALREADY_CLOSED</p>
  METHODS delete_mark
    RAISING
      zcx_io_resource_already_closed .

  "! <p class="shorttext synchronized" lang="en">IS_MARK_SUPPORTED</p>
  "!
  "! @parameter res | <p class="shorttext synchronized" lang="en">RES</p>
  METHODS is_mark_supported
    RETURNING
      VALUE(res) TYPE abap_bool .

  "! <p class="shorttext synchronized" lang="en">RESET_TO_MARK</p>
  "!
  "! @raising   zcx_io_stream_position_error   | <p class="shorttext synchronized" lang="en">ZCX_IO_STREAM_POSITION_ERROR</p>
  "! @raising   zcx_io_resource_already_closed | <p class="shorttext synchronized" lang="en">ZCX_IO_RESOURCE_ALREADY_CLOSED</p>
  METHODS reset_to_mark
    RAISING
      zcx_io_stream_position_error
      zcx_io_resource_already_closed .

  "! <p class="shorttext synchronized" lang="en">DATA_AVAILABLE</p>
  "!
  "! @parameter available                      | <p class="shorttext synchronized" lang="en">AVAILABLE</p>
  "! @raising   zcx_io_resource_already_closed | <p class="shorttext synchronized" lang="en">ZCX_IO_RESOURCE_ALREADY_CLOSED</p>
  "! @raising   zcx_io_stream_error            | <p class="shorttext synchronized" lang="en">ZCX_IO_STREAM_ERROR</p>
  METHODS data_available
    RETURNING
      VALUE(available) TYPE abap_bool
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_error .

  "! <p class="shorttext synchronized" lang="en">READ</p>
  "!
  "! @parameter length                         | <p class="shorttext synchronized" lang="en">LENGTH</p>
  "! @parameter read_data                      | <p class="shorttext synchronized" lang="en">READ_DATA</p>
  "! @raising   zcx_io_parameter_invalid_range | <p class="shorttext synchronized" lang="en">ZCX_IO_PARAMETER_INVALID_RANGE</p>
  "! @raising   zcx_io_parameter_invalid_type  | <p class="shorttext synchronized" lang="en">ZCX_IO_PARAMETER_INVALID_TYPE</p>
  "! @raising   zcx_io_resource_already_closed | <p class="shorttext synchronized" lang="en">ZCX_IO_RESOURCE_ALREADY_CLOSED</p>
  "! @raising   zcx_io_stream_error            | <p class="shorttext synchronized" lang="en">ZCX_IO_STREAM_ERROR</p>
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

  "! <p class="shorttext synchronized" lang="en">IS_X_READER</p>
  "!
  "! @parameter result                    | <p class="shorttext synchronized" lang="en">RESULT</p>
  "! @raising   zcx_io_stream_state_error | <p class="shorttext synchronized" lang="en">ZCX_IO_STREAM_STATE_ERROR</p>
  METHODS is_x_reader
    RETURNING
      VALUE(result) TYPE abap_bool
    RAISING
      zcx_io_stream_state_error .

  "! <p class="shorttext synchronized" lang="en">RESET_TO_MARK</p>
  "!
  "! @raising   zcx_io_stream_position_error   | <p class="shorttext synchronized" lang="en">ZCX_IO_STREAM_POSITION_ERROR</p>
  "! @raising   zcx_io_resource_already_closed | <p class="shorttext synchronized" lang="en">ZCX_IO_RESOURCE_ALREADY_CLOSED</p>
  METHODS reset
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_position_error .

  "! For some types of stream readers, the stream may be automatically closed
  "! after the last character or byte is read. For example, it's the case of
  "! database LOB column reader streams (ZCL_IO_DB_C_READER and ZCL_IO_DB_X_READER).
  "! <p class="shorttext synchronized" lang="en">Stream may be automatically closed</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en">'X' if automatically closed at end of stream</p>
  METHODS is_auto_close_performed
    RETURNING
      VALUE(result) TYPE abap_bool .

ENDINTERFACE.
