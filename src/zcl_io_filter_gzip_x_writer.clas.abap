class ZCL_IO_FILTER_GZIP_X_WRITER definition
  public
  create public

  global friends ZCL_IO_X_WRITER .

public section.

  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_WRITER .
  interfaces ZIF_IO_X_WRITER .
  interfaces IF_ABAP_GZIP_BINARY_HANDLER .

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

  methods CONSTRUCTOR
    importing
      !IO_X_WRITER type ref to ZIF_IO_X_WRITER
      !COMPRESS_LEVEL type I default 6
      !I_BUFFER_SIZE type I optional
    raising
      ZCX_IO_PARAMETER_INVALID_RANGE .
protected section.
private section.

  types:
    BEGIN OF ty_is_gzip_stream,
              o_gzip_stream TYPE REF TO cl_abap_gzip_binary_stream,
              o_x_writer    TYPE REF TO zif_io_x_writer,
            END OF ty_is_gzip_stream .

  class-data:
    kit_gzip_stream TYPE HASHED TABLE OF ty_is_gzip_stream
          WITH UNIQUE KEY o_gzip_stream .
  data AIO_GZIP_BIN type ref to CL_ABAP_GZIP_BINARY_STREAM .
  data AI_BUFFER_SIZE type I .
  data AI_BUFFER type XSTRING .

  methods WRITE_INTERNAL
    importing
      !DATA type XSEQUENCE .
ENDCLASS.



CLASS ZCL_IO_FILTER_GZIP_X_WRITER IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA l_buf_len TYPE i.
    DATA ls_gzip_stream TYPE ty_is_gzip_stream.
    CREATE OBJECT aio_gzip_bin
      EXPORTING
        compress_level = compress_level
        output_handler = me.
    l_buf_len = -1.
    CALL METHOD aio_gzip_bin->set_out_buf
      IMPORTING
        out_buf     = ai_buffer
        out_buf_len = l_buf_len.
    ai_buffer_size = i_buffer_size.
    ls_gzip_stream-o_gzip_stream = aio_gzip_bin.
    ls_gzip_stream-o_x_writer = me.
    INSERT ls_gzip_stream INTO TABLE kit_gzip_stream.

  endmethod.


  method IF_ABAP_GZIP_BINARY_HANDLER~USE_OUT_BUF.

    DATA ls_gzip_stream TYPE ty_is_gzip_stream.
* méthode appelée tous les 1000 octets (cf plus loin, à cause de SET_OUT_BUF), et à la fin
* OUT_BUF       XSEQUENCE   Output Buffer
* OUT_BUF_LEN   I           Length of Output Buffer
* PART          I           Segment (compteur à partir de 1, qui s'incrémente de 1 à chaque appel)
* GZIP_STREAM   Type Ref To cl_abap_GZIP_BINARY_STREAM
    READ TABLE kit_gzip_stream INTO ls_gzip_stream
          WITH TABLE KEY o_gzip_stream = gzip_stream.
    IF sy-subrc = 0.
      ls_gzip_stream-o_x_writer->write( out_buf(out_buf_len) ).
*    if part = cl_abap_GZIP_BINARY_STREAM=>last.
    ENDIF.

  endmethod.


  method WRITE_INTERNAL.

    CALL METHOD aio_gzip_bin->compress_binary_stream
      EXPORTING
        raw_in = data.

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


  endmethod.


  method ZIF_IO_X_WRITER~WRITE.
  endmethod.
ENDCLASS.
