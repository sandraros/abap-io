interface ZIF_IO_STRING_WRITER
  public .


  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_MEMORY_WRITER .
  interfaces ZIF_IO_WRITER .

  aliases GET_RESULT
    for ZIF_IO_MEMORY_WRITER~GET_RESULT .
  aliases GET_RESULT_TYPE
    for ZIF_IO_MEMORY_WRITER~GET_RESULT_TYPE .
endinterface.
