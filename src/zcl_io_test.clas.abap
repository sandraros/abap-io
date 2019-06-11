"! <p class="shorttext synchronized" lang="en">Unit Tests</p>
CLASS zcl_io_test DEFINITION
  PUBLIC
  ABSTRACT
  FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS test_reader
      IMPORTING
        !io_reader TYPE REF TO zif_io_reader .
    METHODS test_writer
      IMPORTING
        !io_writer TYPE REF TO zif_io_writer .
    METHODS test_c_reader
      IMPORTING
        !io_c_reader TYPE REF TO zif_io_c_reader
      RAISING
        cx_static_check cx_dynamic_check .
    METHODS test_c_writer
      IMPORTING
        !io_c_writer TYPE REF TO zif_io_c_writer .
    METHODS test_x_reader
      IMPORTING
        !io_x_reader TYPE REF TO zif_io_x_reader .
    METHODS test_x_writer
      IMPORTING
        !io_x_writer TYPE REF TO zif_io_x_writer .
*    METHODS test_string_c
*        FOR TESTING .
*    METHODS test_backend_c
*        FOR TESTING .
*    METHODS test_gzip
*        FOR TESTING .

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_test IMPLEMENTATION.


  METHOD test_c_reader.

    DATA: snippet TYPE c LENGTH 2.

    " BEGIN OF STREAM: SKIP, READ, DATA_AVAILABLE
    cl_abap_unit_assert=>assert_equals( act = io_c_reader->data_available( ) exp = abap_true ).
    io_c_reader->skip( 3 ).
    snippet = io_c_reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+3(1) ).
    io_c_reader->skip( 1 ).
    snippet = io_c_reader->read( 2 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+5(2) ).

    " MARK, RESET_TO_MARK, DELETE_MARK
    IF abap_true = io_c_reader->is_mark_supported( ).

      io_c_reader->set_mark( ). " from 8th character
      io_c_reader->skip( 2 ).
      snippet = io_c_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+9(2) ).
      io_c_reader->reset_to_mark( ).
      snippet = io_c_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+7(2) ).

      io_c_reader->delete_mark( ).
      TRY.
          io_c_reader->reset_to_mark( ).
        CATCH zcx_io_stream_position_error INTO DATA(lx_no_mark).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( act = lx_no_mark ).

    ELSE.
      io_c_reader->skip( 2 ).
    ENDIF.

    " here, cursor should be before 10th character

    " RESET
    " RESET being equivalent to doing SET_MARK at beginning of stream + RESET_TO_MARK,
    " if MARK is supported, then RESET is usually supported.
    " But if MARK is not supported, RESET may be supported.
    IF abap_true = io_c_reader->is_reset_supported( ).
      io_c_reader->reset( ).
      snippet = io_c_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+0(2) ).
      io_c_reader->skip( 8 ).
    ENDIF.

    " here, cursor should be before 10th character

    " END OF STREAM: DATA_AVAILABLE, READ, CLOSE
    io_c_reader->skip( 16 ).
    cl_abap_unit_assert=>assert_equals( act = io_c_reader->data_available( ) exp = abap_false ).
    TRY.
        snippet = io_c_reader->read( 1 ).
      CATCH zcx_io_stream_error INTO DATA(lx_end).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound( act = lx_end ).

    io_c_reader->close( ).

    TRY.
        io_c_reader->close( ).
      CATCH zcx_io_close_resource_error INTO DATA(lx_already_closed).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound( act = lx_already_closed ).

  ENDMETHOD.


  METHOD test_c_writer.

    DO 10 TIMES.
      io_c_writer->write( sy-abcde+sy-index(1) ).
    ENDDO.
    io_c_writer->close( ).

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


  METHOD test_writer.


  ENDMETHOD.


  METHOD test_x_reader.


  ENDMETHOD.


  METHOD test_x_writer.


*  ENDMETHOD.
*
*
*
*  METHOD test_string_c.
*
*    DATA lo_c_writer TYPE REF TO zcl_io_string_c_writer.
*    DATA lo_c_reader TYPE REF TO zcl_io_string_c_reader.
*    DATA l_string TYPE string.
*    DATA l_dummy TYPE string.
*
*    CREATE OBJECT lo_c_writer
*      EXPORTING
*        str = l_string.
*    CALL METHOD test_writer
*      EXPORTING
*        io_writer = lo_c_writer.
*    l_dummy = lo_c_writer->get_result_string( ).
*    cl_aunit_assert=>assert_equals( act = l_dummy exp = l_string ).
*
*    CREATE OBJECT lo_c_reader
*      EXPORTING
*        str = l_string.
*    CALL METHOD test_reader
*      EXPORTING
*        io_reader = lo_c_reader.
*
*  ENDMETHOD.
*
*
*  METHOD test_backend_c.
*
*    DATA l_string TYPE string.
*
*    "==================
*    " write file
*    "==================
*    DATA(lo_file) = NEW zcl_io_backend(
*        filename    = 'test.txt'
*        mode        = zcl_io_backend=>cs_mode-text
*        access_type = zcl_io_backend=>cs_access_type-output ).
*
*    " it first executes IO_FILE->OPEN( )
*    DATA(lo_c_writer) = NEW zcl_io_backend_c_writer( lo_file ).
*
*    " stream->close( ) does a close( ) of the file
*    test_c_writer( lo_c_writer ).
*
*    "==================
*    " read file
*    "==================
*    lo_file = NEW zcl_io_backend(
*        filename    = 'test.txt'
*        mode        = zcl_io_backend=>cs_mode-text
*        access_type = zcl_io_backend=>cs_access_type-input ).
*
*    DATA(lo_c_reader) = NEW zcl_io_backend_c_reader( lo_file ).
*
*    test_c_reader( lo_c_reader ).
*
*  ENDMETHOD.
*
*
*  METHOD test_gzip.
*
*    DATA lo_gzip_x_writer TYPE REF TO zcl_io_filter_gzip_x_writer.
*    DATA lo_gzip_x_reader TYPE REF TO zcl_io_filter_gzip_x_reader.
*    DATA lo_gzip_c_writer TYPE REF TO zcl_io_filter_gzip_c_writer.
*    DATA lo_gzip_c_reader TYPE REF TO zcl_io_filter_gzip_c_reader.
*    DATA lo_string_x_writer TYPE REF TO zcl_io_string_x_writer.
*    DATA lo_string_x_reader TYPE REF TO zcl_io_string_x_reader.
*    DATA l_string TYPE string.
*    DATA l_xstring TYPE xstring.
*
*    CREATE OBJECT lo_string_x_writer
*      EXPORTING
*        xstr = l_xstring.
*
*    CREATE OBJECT lo_gzip_x_writer
*      EXPORTING
*        io_x_writer   = lo_string_x_writer
*        i_buffer_size = 1000.
*    CALL METHOD test_writer
*      EXPORTING
*        io_writer = lo_gzip_x_writer.
*    ASSERT l_xstring = ''. "valeur zippÃ©e
*
*    CREATE OBJECT lo_gzip_c_writer
*      EXPORTING
*        io_x_writer   = lo_string_x_writer
*        i_buffer_size = 1000.
*    lo_gzip_c_writer->write( 'AAA' ).
*
*
*    lo_string_x_reader = new zcl_io_string_x_reader( xstr = l_xstring ).
*    CREATE OBJECT lo_gzip_x_reader
*      EXPORTING
*        io_x_reader   = lo_string_x_reader "containing GZIPped data
*        i_gzip_buffer = 1000.
*    l_xstring = lo_gzip_x_reader->read( 100 ). "lire 100 octets dÃ©compressÃ©s
*
*    CREATE OBJECT lo_gzip_c_reader
*      EXPORTING
*        io_x_reader   = lo_string_x_reader
*        i_gzip_buffer = 1000.
*    l_string = lo_gzip_c_reader->read( 100 ). "lire 100 caractÃ¨res dÃ©compressÃ©s


  ENDMETHOD.


ENDCLASS.
