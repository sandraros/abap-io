interface ZIF_IO_MEMORY_WRITER
  public .


  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_WRITER .

  methods GET_RESULT
    exporting
      !RESULT type ANY
      !LENGTH_OF_LAST_LINE type I
    raising
      ZCX_IO_PARAMETER_INVALID_TYPE .
  methods GET_RESULT_TYPE
    exporting
      !RESULT_TYPE type ref to CL_ABAP_DATADESCR .
endinterface.
