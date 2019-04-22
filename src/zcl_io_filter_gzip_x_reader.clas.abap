class ZCL_IO_FILTER_GZIP_X_READER definition
  public
  create public

  global friends ZCL_IO_X_READER .

public section.

  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_READER .
  interfaces ZIF_IO_X_READER .
  interfaces IF_ABAP_UNGZIP_BINARY_HANDLER .

  aliases CLOSE
    for ZIF_IO_CLOSE_RESOURCE~CLOSE .
  aliases DATA_AVAILABLE
    for ZIF_IO_READER~DATA_AVAILABLE .
  aliases DELETE_MARK
    for ZIF_IO_READER~DELETE_MARK .
  aliases IS_CLOSED
    for ZIF_IO_CLOSE_RESOURCE~IS_CLOSED .
  aliases IS_MARK_SUPPORTED
    for ZIF_IO_READER~IS_MARK_SUPPORTED .
  aliases IS_RESET_SUPPORTED
    for ZIF_IO_READER~IS_RESET_SUPPORTED .
  aliases IS_X_READER
    for ZIF_IO_READER~IS_X_READER .
  aliases READ
    for ZIF_IO_X_READER~READ .
  aliases RESET
    for ZIF_IO_READER~RESET .
  aliases RESET_TO_MARK
    for ZIF_IO_READER~RESET_TO_MARK .
  aliases SET_MARK
    for ZIF_IO_READER~SET_MARK .
  aliases SKIP
    for ZIF_IO_READER~SKIP .

  methods CONSTRUCTOR
    importing
      !IO_X_READER type ref to ZIF_IO_X_READER
      !I_GZIP_BUFFER type I optional
      !I_STREAM_BUFFER type I optional
    raising
      ZCX_IO_PARAMETER_INVALID_RANGE .
protected section.
private section.

  types:
    BEGIN OF ty_is_ungzip_stream,
              o_ungzip_stream TYPE REF TO cl_abap_ungzip_binary_stream,
              o_x_reader      TYPE REF TO zif_io_x_reader,
            END OF ty_is_ungzip_stream .

  class-data:
    kit_ungzip_stream TYPE HASHED TABLE OF ty_is_ungzip_stream
          WITH UNIQUE KEY o_ungzip_stream .
  data AIO_UNGZIP_BIN type ref to CL_ABAP_UNGZIP_BINARY_STREAM .
  data AI_GZIP_BUFFER type I .
  data AI_STREAM_BUFFER type I .
  data AI_BUFFER type XSTRING .
  data AIO_X_READER type ref to ZIF_IO_X_READER .

  methods READ_INTERNAL
    importing
      !LENGTH type NUMERIC
    returning
      value(RESULT) type XSTRING .
ENDCLASS.



CLASS ZCL_IO_FILTER_GZIP_X_READER IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA l_buf_len TYPE i.
    DATA ls_ungzip_stream TYPE ty_is_ungzip_stream.
    CREATE OBJECT aio_ungzip_bin
      EXPORTING
        output_handler = me.
    l_buf_len = -1.
    CALL METHOD aio_ungzip_bin->set_out_buf
      IMPORTING
        out_buf     = ai_buffer
        out_buf_len = l_buf_len.
    aio_x_reader = io_x_reader.
    ai_gzip_buffer = i_gzip_buffer.
    ai_stream_buffer = i_stream_buffer.
    ls_ungzip_stream-o_ungzip_stream = aio_ungzip_bin.
    ls_ungzip_stream-o_x_reader = me.
    INSERT ls_ungzip_stream INTO TABLE kit_ungzip_stream.

  endmethod.


  method IF_ABAP_UNGZIP_BINARY_HANDLER~USE_OUT_BUF.

    DATA ls_ungzip_stream TYPE ty_is_ungzip_stream.
    DATA lo_gzip_x_reader TYPE REF TO zcl_io_filter_gzip_x_reader.
* méthode appelée tous les 1000 octets (cf plus loin, à cause de SET_OUT_BUF), et à la fin
* OUT_BUF       XSEQUENCE   Output Buffer
* OUT_BUF_LEN   I           Length of Output Buffer
* PART          I           Segment (compteur à partir de 1, qui s'incrémente de 1 à chaque appel)
* GZIP_STREAM   Type Ref To cl_abap_UNGZIP_BINARY_STREAM
    READ TABLE kit_ungzip_stream INTO ls_ungzip_stream
          WITH TABLE KEY o_ungzip_stream = gzip_stream.
    IF sy-subrc = 0.
      lo_gzip_x_reader ?= ls_ungzip_stream-o_x_reader.
      lo_gzip_x_reader->read( length = lo_gzip_x_reader->ai_gzip_buffer ).
*    if part = cl_abap_unGZIP_BINARY_STREAM=>last.
    ENDIF.

  endmethod.


  method READ_INTERNAL.

    DATA l_xstring TYPE xstring.
    WHILE XSTRLEN( ai_buffer ) < length AND abap_false = aio_x_reader->data_available( ).
      l_xstring = aio_x_reader->read( ai_stream_buffer ).
      CALL METHOD aio_ungzip_bin->decompress_binary_stream
        EXPORTING
          gzip_in     = l_xstring
          gzip_in_len = -1.
    ENDWHILE.

  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~CLOSE.
  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~IS_CLOSED.
  endmethod.


  method ZIF_IO_READER~DATA_AVAILABLE.
  endmethod.


  method ZIF_IO_READER~DELETE_MARK.
  endmethod.


  method ZIF_IO_READER~IS_MARK_SUPPORTED.
  endmethod.


  method ZIF_IO_READER~IS_RESET_SUPPORTED.
  endmethod.


  method ZIF_IO_READER~IS_X_READER.
  endmethod.


  method ZIF_IO_READER~READ.


  endmethod.


  method ZIF_IO_READER~RESET.
  endmethod.


  method ZIF_IO_READER~RESET_TO_MARK.
  endmethod.


  method ZIF_IO_READER~SET_MARK.
  endmethod.


  method ZIF_IO_READER~SKIP.
  endmethod.


  method ZIF_IO_X_READER~READ.
  endmethod.
ENDCLASS.
