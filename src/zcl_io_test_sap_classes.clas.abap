CLASS zcl_io_test_sap_classes DEFINITION
  PUBLIC
  ABSTRACT
  FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS test_c_reader
      IMPORTING
        !c_reader                   TYPE REF TO if_abap_c_reader
        expect_read_auto_close TYPE abap_bool
        expect_exc_reset_if_no_mark TYPE abap_bool
        expect_avail_last_char      TYPE abap_bool
        expect_exc_read_beyond_end  TYPE abap_bool
      RAISING
        cx_static_check cx_dynamic_check .

    METHODS test_x_reader
      IMPORTING
        !x_reader                   TYPE REF TO if_abap_x_reader
        expect_read_auto_close TYPE abap_bool
        expect_exc_reset_if_no_mark TYPE abap_bool
        expect_avail_last_char      TYPE abap_bool
        expect_exc_read_beyond_end  TYPE abap_bool
      RAISING
        cx_static_check cx_dynamic_check .

    CONSTANTS _01_to_1A TYPE X LENGTH 26 VALUE '0102030405060708090A0B0C0D0E0F101112131415161718191A'.

ENDCLASS.



CLASS ZCL_IO_TEST_SAP_CLASSES IMPLEMENTATION.


  METHOD test_c_reader.

    DATA: snippet TYPE string.

    " BEGIN OF STREAM: SKIP, READ, DATA_AVAILABLE
    cl_abap_unit_assert=>assert_equals( act = c_reader->data_available( ) exp = abap_true ).
    c_reader->skip( 3 ).
    snippet = c_reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+3(1) ). " expects D
    c_reader->skip( 1 ).
    snippet = c_reader->read( 2 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+5(2) ). " expects FG

    " MARK, RESET_TO_MARK, DELETE_MARK
    IF abap_true = c_reader->is_mark_supported( ).

      c_reader->set_mark( ). " from 8th character
      c_reader->skip( 2 ).
      snippet = c_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+9(2) ). " expects JK
      c_reader->reset_to_mark( ).
      snippet = c_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+7(2) ). " expects HI

      c_reader->delete_mark( ).
      TRY.
          c_reader->reset_to_mark( ).
        CATCH cx_stream_position_error INTO DATA(lx_no_mark).
      ENDTRY.
      IF expect_exc_reset_if_no_mark = abap_true.
        cl_abap_unit_assert=>assert_bound( msg = 'reset to non-existing mark should do an exception' act = lx_no_mark ).
      ELSE.
        cl_abap_unit_assert=>assert_not_bound( msg = 'reset to non-existing mark should NOT do an exception' act = lx_no_mark ).
      ENDIF.

    ELSE.
      c_reader->skip( 3 ).
    ENDIF.

    " here, cursor should be before 10th character

    " RESET
    " RESET being equivalent to doing SET_MARK at beginning of stream + RESET_TO_MARK,
    " if MARK is supported, then RESET is usually supported.
    " But if MARK is not supported, RESET may be supported.
    IF abap_true = c_reader->is_reset_supported( ).
      c_reader->reset( ).
      snippet = c_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+0(2) ). " expects AB
      c_reader->skip( 8 ).
    ENDIF.

    " here, cursor should be before 10th character

    " read last character
    " END OF STREAM: DATA_AVAILABLE, READ, CLOSE
    c_reader->skip( 15 ).
    snippet = c_reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+25(1) ). " expects Z
    IF expect_avail_last_char = abap_true.
      cl_abap_unit_assert=>assert_equals( act = c_reader->data_available( ) exp = abap_true ).
    ELSE.
      cl_abap_unit_assert=>assert_equals( act = c_reader->data_available( ) exp = abap_false ).
    ENDIF.

    " beyond last character
    TRY.
        snippet = c_reader->read( 1 ).
        cl_abap_unit_assert=>assert_initial( msg = 'read should return empty read after end' act = snippet ).
      CATCH cx_root INTO DATA(lx_end1).
    ENDTRY.
    IF expect_read_auto_close = abap_true.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception end of stream' act = lx_end1 ).
      cl_abap_unit_assert=>assert_equals( msg = 'read beyond last character -> expects cx_resource_already_closed' exp = abap_true act = xsdbool( lx_end1 IS INSTANCE OF cx_resource_already_closed ) ).
      cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should be true' exp = abap_true act = c_reader->is_closed( ) ).
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( msg = 'expecting NO exception end of stream' act = lx_end1 ).
      " read again
      TRY.
          snippet = c_reader->read( 1 ).
        CATCH cx_root INTO DATA(lx_end2).
      ENDTRY.
      IF expect_exc_read_beyond_end = abap_true.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception read after end' act = lx_end2 ).
      else.
      cl_abap_unit_assert=>assert_initial( msg = 'read should return empty read after end' act = snippet ).
      endif.
    ENDIF.

    " close
    c_reader->close( ).

    " close twice, no exception
    c_reader->close( ).

  ENDMETHOD.

  METHOD test_x_reader.

    DATA: snippet TYPE xstring.

    " BEGIN OF STREAM: SKIP, READ, DATA_AVAILABLE
    cl_abap_unit_assert=>assert_equals( act = x_reader->data_available( ) exp = abap_true ).
    x_reader->skip( 3 ).
    snippet = x_reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = conv xstring( _01_to_1A+3(1) ) ). " expects D
    x_reader->skip( 1 ).
    snippet = x_reader->read( 2 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = conv xstring( _01_to_1A+5(2) ) ). " expects FG

    " MARK, RESET_TO_MARK, DELETE_MARK
    IF abap_true = x_reader->is_mark_supported( ).

      x_reader->set_mark( ). " from 8th byte
      x_reader->skip( 2 ).
      snippet = x_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = conv xstring( _01_to_1A+9(2) ) ). " expects JK
      x_reader->reset_to_mark( ).
      snippet = x_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = conv xstring( _01_to_1A+7(2) ) ). " expects HI

      x_reader->delete_mark( ).
      TRY.
          x_reader->reset_to_mark( ).
        CATCH cx_stream_position_error INTO DATA(lx_no_mark).
      ENDTRY.
      IF expect_exc_reset_if_no_mark = abap_true.
        cl_abap_unit_assert=>assert_bound( msg = 'reset to non-existing mark should do an exception' act = lx_no_mark ).
      ELSE.
        cl_abap_unit_assert=>assert_not_bound( msg = 'reset to non-existing mark should NOT do an exception' act = lx_no_mark ).
      ENDIF.

    ELSE.
      x_reader->skip( 3 ).
    ENDIF.

    " here, cursor should be before 10th byte

    " RESET
    " RESET being equivalent to doing SET_MARK at beginning of stream + RESET_TO_MARK,
    " if MARK is supported, then RESET is usually supported.
    " But if MARK is not supported, RESET may be supported.
    IF abap_true = x_reader->is_reset_supported( ).
      x_reader->reset( ).
      snippet = x_reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = conv xstring( _01_to_1A+0(2) ) ). " expects AB
      x_reader->skip( 8 ).
    ENDIF.

    " here, cursor should be before 10th byte

    " read last byte
    " END OF STREAM: DATA_AVAILABLE, READ, CLOSE
    x_reader->skip( 15 ).
    snippet = x_reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = conv xstring( _01_to_1A+25(1) ) ). " expects Z
    IF expect_avail_last_char = abap_true.
      cl_abap_unit_assert=>assert_equals( act = x_reader->data_available( ) exp = abap_true ).
    ELSE.
      cl_abap_unit_assert=>assert_equals( act = x_reader->data_available( ) exp = abap_false ).
    ENDIF.

    " beyond last byte
    TRY.
        snippet = x_reader->read( 1 ).
        cl_abap_unit_assert=>assert_initial( msg = 'read should return empty read after end' act = snippet ).
      CATCH cx_root INTO DATA(lx_end1).
    ENDTRY.
    IF expect_read_auto_close = abap_true.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception end of stream' act = lx_end1 ).
      cl_abap_unit_assert=>assert_equals( msg = 'read beyond last byte -> expects cx_resource_already_closed' exp = abap_true act = xsdbool( lx_end1 IS INSTANCE OF cx_resource_already_closed ) ).
      cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should be true' exp = abap_true act = x_reader->is_closed( ) ).
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( msg = 'expecting NO exception end of stream' act = lx_end1 ).
      " read again
      TRY.
          snippet = x_reader->read( 1 ).
        CATCH cx_root INTO DATA(lx_end2).
      ENDTRY.
      IF expect_exc_read_beyond_end = abap_true.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception read after end' act = lx_end2 ).
      else.
      cl_abap_unit_assert=>assert_initial( msg = 'read should return empty read after end' act = snippet ).
      endif.
    ENDIF.

    " close
    x_reader->close( ).

    " close twice, no exception
    x_reader->close( ).

  ENDMETHOD.

ENDCLASS.
