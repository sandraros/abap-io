class ZCL_IO_FILTER_X_READER definition
  public
  abstract
  create public

  global friends ZCL_IO_X_READER .

public section.

  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_READER .
  interfaces ZIF_IO_X_READER .

  aliases CLOSE
    for ZIF_IO_CLOSE_RESOURCE~CLOSE .
  aliases DATA_AVAILABLE
    for ZIF_IO_READER~DATA_AVAILABLE .
  aliases DELETE_MARK
    for ZIF_IO_READER~DELETE_MARK .
  aliases IS_CLOSED
    for ZIF_IO_CLOSE_RESOURCE~IS_CLOSED .
  aliases IS_MARK_SUPPORTED
    for ZIF_IO_READER~IS_MARK_SUPPORTED .
  aliases IS_RESET_SUPPORTED
    for ZIF_IO_READER~IS_RESET_SUPPORTED .
  aliases IS_X_READER
    for ZIF_IO_READER~IS_X_READER .
  aliases READ
    for ZIF_IO_X_READER~READ .
  aliases RESET
    for ZIF_IO_READER~RESET .
  aliases RESET_TO_MARK
    for ZIF_IO_READER~RESET_TO_MARK .
  aliases SET_MARK
    for ZIF_IO_READER~SET_MARK .
  aliases SKIP
    for ZIF_IO_READER~SKIP .

  methods CONSTRUCTOR
    importing
      !IO_X_READER type ref to ZIF_IO_X_READER .
protected section.

  data AOO_X_READER type ref to ZIF_IO_X_READER .
private section.
ENDCLASS.



CLASS ZCL_IO_FILTER_X_READER IMPLEMENTATION.


  method CONSTRUCTOR.

    aoo_x_reader    = io_x_reader.

  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~CLOSE.
  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~IS_CLOSED.
  endmethod.


  method ZIF_IO_READER~DATA_AVAILABLE.
  endmethod.


  method ZIF_IO_READER~DELETE_MARK.
  endmethod.


  method ZIF_IO_READER~IS_MARK_SUPPORTED.
  endmethod.


  method ZIF_IO_READER~IS_RESET_SUPPORTED.
  endmethod.


  method ZIF_IO_READER~IS_X_READER.
  endmethod.


  method ZIF_IO_READER~READ.

    zcl_io_stream_utilities=>check_data_type_is_xstring( read_data ).
    read_data = aoo_x_reader->read( length ).

  endmethod.


  method ZIF_IO_READER~RESET.
  endmethod.


  method ZIF_IO_READER~RESET_TO_MARK.
  endmethod.


  method ZIF_IO_READER~SET_MARK.
  endmethod.


  method ZIF_IO_READER~SKIP.
  endmethod.


  method ZIF_IO_X_READER~READ.
  endmethod.
ENDCLASS.
