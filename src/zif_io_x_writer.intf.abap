interface ZIF_IO_X_WRITER
  public .


  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_WRITER .

  aliases CLOSE
    for ZIF_IO_WRITER~CLOSE .
  aliases IS_CLOSED
    for ZIF_IO_WRITER~IS_CLOSED .

  methods WRITE
    importing
      !DATA type XSTRING
    raising
      ZCX_IO_RESOURCE_ALREADY_CLOSED
      ZCX_IO_STREAM_ERROR .
endinterface.
