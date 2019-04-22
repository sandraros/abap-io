interface ZIF_IO_X_READER
  public .


  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_READER .

  aliases CLOSE
    for ZIF_IO_READER~CLOSE .
  aliases DATA_AVAILABLE
    for ZIF_IO_READER~DATA_AVAILABLE .
  aliases DELETE_MARK
    for ZIF_IO_READER~DELETE_MARK .
  aliases IS_CLOSED
    for ZIF_IO_READER~IS_CLOSED .
  aliases IS_MARK_SUPPORTED
    for ZIF_IO_READER~IS_MARK_SUPPORTED .
  aliases IS_RESET_SUPPORTED
    for ZIF_IO_READER~IS_RESET_SUPPORTED .
  aliases IS_X_READER
    for ZIF_IO_READER~IS_X_READER .
  aliases RESET
    for ZIF_IO_READER~RESET .
  aliases RESET_TO_MARK
    for ZIF_IO_READER~RESET_TO_MARK .
  aliases SET_MARK
    for ZIF_IO_READER~SET_MARK .
  aliases SKIP
    for ZIF_IO_READER~SKIP .

  methods READ
    importing
      !LENGTH type NUMERIC
    returning
      value(RESULT) type XSTRING
    raising
      ZCX_IO_PARAMETER_INVALID_RANGE
      ZCX_IO_RESOURCE_ALREADY_CLOSED
      ZCX_IO_STREAM_ERROR .
endinterface.
