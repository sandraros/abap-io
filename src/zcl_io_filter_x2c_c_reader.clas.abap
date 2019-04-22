class ZCL_IO_FILTER_X2C_C_READER definition
  public
  create public .

public section.

  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_C_READER .
  interfaces ZIF_IO_READER .

  aliases CLOSE
    for ZIF_IO_C_READER~CLOSE .
  aliases DATA_AVAILABLE
    for ZIF_IO_C_READER~DATA_AVAILABLE .
  aliases DELETE_MARK
    for ZIF_IO_C_READER~DELETE_MARK .
  aliases IS_CLOSED
    for ZIF_IO_C_READER~IS_CLOSED .
  aliases IS_MARK_SUPPORTED
    for ZIF_IO_C_READER~IS_MARK_SUPPORTED .
  aliases IS_RESET_SUPPORTED
    for ZIF_IO_C_READER~IS_RESET_SUPPORTED .
  aliases IS_X_READER
    for ZIF_IO_C_READER~IS_X_READER .
  aliases READ
    for ZIF_IO_C_READER~READ .
  aliases RESET
    for ZIF_IO_READER~RESET .
  aliases RESET_TO_MARK
    for ZIF_IO_C_READER~RESET_TO_MARK .
  aliases SET_MARK
    for ZIF_IO_C_READER~SET_MARK .
  aliases SKIP
    for ZIF_IO_C_READER~SKIP .

  methods CONSTRUCTOR
    importing
      !IO_X_READER type ref to ZIF_IO_X_READER
      !I_ENCODING type ABAP_ENCODING .
protected section.
private section.

  data AIO_X_READER type ref to ZIF_IO_X_READER .
  data AI_ENCODING type ABAP_ENCODING .
ENDCLASS.



CLASS ZCL_IO_FILTER_X2C_C_READER IMPLEMENTATION.


  method CONSTRUCTOR.

    aio_x_reader = io_x_reader.
    ai_encoding  = i_encoding.

  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~CLOSE.
  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~IS_CLOSED.
  endmethod.


  method ZIF_IO_C_READER~READ.
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


  endmethod.


  method ZIF_IO_READER~RESET.
  endmethod.


  method ZIF_IO_READER~RESET_TO_MARK.
  endmethod.


  method ZIF_IO_READER~SET_MARK.
  endmethod.


  method ZIF_IO_READER~SKIP.
  endmethod.
ENDCLASS.
