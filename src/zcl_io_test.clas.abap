"! <p class="shorttext synchronized" lang="en">Unit Tests</p>
CLASS zcl_io_test DEFINITION
  PUBLIC
  INHERITING FROM cl_aunit_assert
  ABSTRACT
  FOR TESTING
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS test_reader
      IMPORTING
        !io_reader TYPE REF TO zif_io_reader .
    METHODS test_writer
      IMPORTING
        !io_writer TYPE REF TO zif_io_writer .
    METHODS test_c_reader
      IMPORTING
        !io_c_reader TYPE REF TO zif_io_c_reader .
    METHODS test_c_writer
      IMPORTING
        !io_c_writer TYPE REF TO zif_io_c_writer .
    METHODS test_x_reader
      IMPORTING
        !io_x_reader TYPE REF TO zif_io_x_reader .
    METHODS test_x_writer
      IMPORTING
        !io_x_writer TYPE REF TO zif_io_x_writer .
    METHODS test_string_c
        FOR TESTING .
    METHODS test_backend_c
        FOR TESTING .
    METHODS test_gzip
        FOR TESTING .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_test IMPLEMENTATION.


  METHOD test_backend_c.

    DATA l_string TYPE string.

    "==================
    " write file
    "==================
    DATA(lo_file) = NEW zcl_io_backend(
        filename    = 'test.txt'
        mode        = zcl_io_backend=>cs_mode-text
        access_type = zcl_io_backend=>cs_access_type-output ).

    " it first executes IO_FILE->OPEN( )
    DATA(lo_c_writer) = NEW zcl_io_backend_c_writer( lo_file ).

    " stream->close( ) does a close( ) of the file
    test_c_writer( lo_c_writer ).

    "==================
    " read file
    "==================
    lo_file = NEW zcl_io_backend(
        filename    = 'test.txt'
        mode        = zcl_io_backend=>cs_mode-text
        access_type = zcl_io_backend=>cs_access_type-input ).

    DATA(lo_c_reader) = NEW zcl_io_backend_c_reader( lo_file ).

    test_c_reader( lo_c_reader ).

  ENDMETHOD.


  METHOD test_c_reader.

    DATA snippet TYPE c LENGTH 1.
    io_c_reader->skip( 3 ).
    IF abap_true = io_c_reader->is_mark_supported( ).
    ENDIF.
    IF abap_true = io_c_reader->is_reset_supported( ).
    ENDIF.
    WHILE abap_true = io_c_reader->data_available( ).
      snippet = io_c_reader->read( 1 ).
      cl_aunit_assert=>assert_equals( act = snippet exp = 'a' ).
    ENDWHILE.
    io_c_reader->close( ).

  ENDMETHOD.


  METHOD test_c_writer.

    DO 10 TIMES.
      io_c_writer->write( 'a' ).
    ENDDO.
    io_c_writer->close( ).

  ENDMETHOD.


  METHOD test_gzip.

    DATA lo_gzip_x_writer TYPE REF TO zcl_io_filter_gzip_x_writer.
    DATA lo_gzip_x_reader TYPE REF TO zcl_io_filter_gzip_x_reader.
    DATA lo_gzip_c_writer TYPE REF TO zcl_io_filter_gzip_c_writer.
    DATA lo_gzip_c_reader TYPE REF TO zcl_io_filter_gzip_c_reader.
    DATA lo_string_x_writer TYPE REF TO zcl_io_string_x_writer.
    DATA lo_string_x_reader TYPE REF TO zcl_io_string_x_reader.
    DATA l_string TYPE string.
    DATA l_xstring TYPE xstring.

    CREATE OBJECT lo_string_x_writer
      EXPORTING
        xstr = l_xstring.

    CREATE OBJECT lo_gzip_x_writer
      EXPORTING
        io_x_writer   = lo_string_x_writer
        i_buffer_size = 1000.
    CALL METHOD test_writer
      EXPORTING
        io_writer = lo_gzip_x_writer.
    ASSERT l_xstring = ''. "valeur zippÃ©e

    CREATE OBJECT lo_gzip_c_writer
      EXPORTING
        io_x_writer   = lo_string_x_writer
        i_buffer_size = 1000.
    lo_gzip_c_writer->write( 'AAA' ).


    CREATE OBJECT lo_string_x_reader
      EXPORTING
        str  = ``
        xstr = l_xstring.
    CREATE OBJECT lo_gzip_x_reader
      EXPORTING
        io_x_reader   = lo_string_x_reader "containing GZIPped data
        i_gzip_buffer = 1000.
    l_xstring = lo_gzip_x_reader->read( 100 ). "lire 100 octets dÃ©compressÃ©s

    CREATE OBJECT lo_gzip_c_reader
      EXPORTING
        io_x_reader   = lo_string_x_reader
        i_gzip_buffer = 1000.
    l_string = lo_gzip_c_reader->read( 100 ). "lire 100 caractÃ¨res dÃ©compressÃ©s


  ENDMETHOD.


  METHOD test_reader.

    DATA lo_c_reader TYPE REF TO zif_io_c_reader.
    DATA lo_x_reader TYPE REF TO zif_io_x_reader.
    IF abap_true = io_reader->is_x_reader( ).
      lo_x_reader ?= io_reader.
      CALL METHOD test_x_reader
        EXPORTING
          io_x_reader = lo_x_reader.
    ELSE.
      lo_c_reader ?= io_reader.
      CALL METHOD test_c_reader
        EXPORTING
          io_c_reader = lo_c_reader.
    ENDIF.

  ENDMETHOD.


  METHOD test_string_c.

    DATA lo_c_writer TYPE REF TO zcl_io_string_c_writer.
    DATA lo_c_reader TYPE REF TO zcl_io_string_c_reader.
    DATA l_string TYPE string.
    DATA l_dummy TYPE string.

    CREATE OBJECT lo_c_writer
      EXPORTING
        str = l_string.
    CALL METHOD test_writer
      EXPORTING
        io_writer = lo_c_writer.
    l_dummy = lo_c_writer->get_result_string( ).
    cl_aunit_assert=>assert_equals( act = l_dummy exp = l_string ).

    CREATE OBJECT lo_c_reader
      EXPORTING
        str = l_string.
    CALL METHOD test_reader
      EXPORTING
        io_reader = lo_c_reader.

  ENDMETHOD.


  METHOD test_writer.


  ENDMETHOD.


  METHOD test_x_reader.


  ENDMETHOD.


  METHOD test_x_writer.


  ENDMETHOD.
ENDCLASS.
