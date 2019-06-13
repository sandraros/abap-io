"! <p class="shorttext synchronized" lang="en"></p>
CLASS zcx_io_stream_position_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_io_stream_error
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS zcx_io_stream_position_error TYPE sotr_conc VALUE '001560AA0E0802DB8CED7F7BF73D822C' ##NO_TEXT.
    CONSTANTS zcx_io_mark_not_supported TYPE sotr_conc VALUE '001560AA0E0802DB8CED980D364E426D' ##NO_TEXT.
    CONSTANTS zcx_io_reset_not_supported TYPE sotr_conc VALUE '001560AA0E0802EB9A9AFE4CA53E01C1' ##NO_TEXT.
    CONSTANTS zcx_io_mark_not_set TYPE sotr_conc VALUE '001560AA0E0802DBB9FF94E921E8C7EB' ##NO_TEXT.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_io_stream_position_error IMPLEMENTATION.
ENDCLASS.