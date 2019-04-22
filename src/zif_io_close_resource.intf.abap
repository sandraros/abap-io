interface ZIF_IO_CLOSE_RESOURCE
  public .


  methods CLOSE
    raising
      ZCX_IO_CLOSE_RESOURCE_ERROR .
  methods IS_CLOSED
    returning
      value(CLOSED) type ABAP_BOOL .
endinterface.
