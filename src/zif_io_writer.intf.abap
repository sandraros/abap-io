interface ZIF_IO_WRITER
  public .


  interfaces ZIF_IO_CLOSE_RESOURCE .

  aliases CLOSE
    for ZIF_IO_CLOSE_RESOURCE~CLOSE .
  aliases IS_CLOSED
    for ZIF_IO_CLOSE_RESOURCE~IS_CLOSED .

  methods WRITE
    importing
      !DATA type ANY
    raising
      ZCX_IO_PARAMETER_INVALID_TYPE
      ZCX_IO_RESOURCE_ALREADY_CLOSED
      ZCX_IO_STREAM_ERROR .
  methods FLUSH
    raising
      ZCX_IO_RESOURCE_ALREADY_CLOSED
      ZCX_IO_STREAM_ERROR .
  methods IS_X_WRITER
    returning
      value(RESULT) type ABAP_BOOL .
endinterface.
