interface ZIF_IO_ITAB_WRITER
  public .


  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_MEMORY_WRITER .
  interfaces ZIF_IO_WRITER .

  aliases GET_RESULT
    for ZIF_IO_MEMORY_WRITER~GET_RESULT .
  aliases GET_RESULT_TYPE
    for ZIF_IO_MEMORY_WRITER~GET_RESULT_TYPE .

  methods GET_RESULT_TABLE
    exporting
      !TABLE type STANDARD TABLE
      !LENGTH_OF_LAST_LINE type I .
  methods GET_MAX_LINE_LENGTH
    exporting
      !LINE_LENGTH type I .
endinterface.
