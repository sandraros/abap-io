interface ZIF_IO_C_WRITER
  public .


  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_WRITER .

  aliases CLOSE
    for ZIF_IO_WRITER~CLOSE .
  aliases FLUSH
    for ZIF_IO_WRITER~FLUSH .
  aliases IS_CLOSED
    for ZIF_IO_WRITER~IS_CLOSED .
  aliases IS_X_WRITER
    for ZIF_IO_WRITER~IS_X_WRITER .

  methods WRITE
    importing
      !DATA type STRING
    raising
      ZCX_IO_RESOURCE_ALREADY_CLOSED
      ZCX_IO_STREAM_ERROR .
endinterface.
