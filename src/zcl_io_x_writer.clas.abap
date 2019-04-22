class ZCL_IO_X_WRITER definition
  public
  create public .

public section.

  interfaces ZIF_IO_CLOSE_RESOURCE
      all methods final .
  interfaces ZIF_IO_WRITER
      all methods final .
  interfaces ZIF_IO_X_WRITER
      all methods final .

  aliases CLOSE
    for ZIF_IO_CLOSE_RESOURCE~CLOSE .
  aliases FLUSH
    for ZIF_IO_WRITER~FLUSH .
  aliases IS_CLOSED
    for ZIF_IO_CLOSE_RESOURCE~IS_CLOSED .
  aliases IS_X_WRITER
    for ZIF_IO_WRITER~IS_X_WRITER .
  aliases WRITE
    for ZIF_IO_X_WRITER~WRITE .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IO_X_WRITER IMPLEMENTATION.


  method CONSTRUCTOR.


  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~CLOSE.
  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~IS_CLOSED.
  endmethod.


  method ZIF_IO_WRITER~FLUSH.
  endmethod.


  method ZIF_IO_WRITER~IS_X_WRITER.
  endmethod.


  method ZIF_IO_WRITER~WRITE.

    DATA type TYPE REF TO cl_abap_typedescr.
    DATA l_name TYPE string.
    type = cl_abap_typedescr=>describe_by_data( data ).
    IF type = cl_abap_elemdescr=>get_xstring( ).
      write( data ).
    ELSE.
      l_name = type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
         EXPORTING
           textid = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
           parameter = `DATA`
           type = l_name.
    ENDIF.

  endmethod.


  method ZIF_IO_X_WRITER~WRITE.
  endmethod.
ENDCLASS.
