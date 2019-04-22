class ZCL_IO_MEMORY_X_WRITER definition
  public
  inheriting from ZCL_IO_X_WRITER
  abstract
  create public .

public section.

  interfaces ZIF_IO_MEMORY_WRITER .

  aliases GET_RESULT
    for ZIF_IO_MEMORY_WRITER~GET_RESULT .
  aliases GET_RESULT_TYPE
    for ZIF_IO_MEMORY_WRITER~GET_RESULT_TYPE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IO_MEMORY_X_WRITER IMPLEMENTATION.


  method ZIF_IO_MEMORY_WRITER~GET_RESULT.
  endmethod.


  method ZIF_IO_MEMORY_WRITER~GET_RESULT_TYPE.
  endmethod.
ENDCLASS.
