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
        reader                      TYPE REF TO if_abap_c_reader
        expect_read_auto_close      TYPE abap_bool
        expect_exc_reset_if_no_mark TYPE abap_bool
        expect_avail_last_char      TYPE abap_bool
        expect_exc_read_beyond_end  TYPE abap_bool
      RAISING
        cx_static_check cx_dynamic_check .

    METHODS test_x_reader
      IMPORTING
        reader                      TYPE REF TO if_abap_x_reader
        expect_read_auto_close      TYPE abap_bool
        expect_exc_reset_if_no_mark TYPE abap_bool
        expect_avail_last_char      TYPE abap_bool
        expect_exc_read_beyond_end  TYPE abap_bool
      RAISING
        cx_static_check cx_dynamic_check .

    METHODS test_c_writer
      IMPORTING
        writer TYPE REF TO if_abap_c_writer.

    METHODS test_x_writer
      IMPORTING
        writer TYPE REF TO if_abap_x_writer.

ENDCLASS.



CLASS zcl_io_test_sap_classes IMPLEMENTATION.


  METHOD test_c_reader.

    DATA: snippet TYPE string.

    " BEGIN OF STREAM: SKIP, READ, DATA_AVAILABLE
    cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    reader->skip( 3 ).
    snippet = reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+3(1) ). " expects D
    reader->skip( 1 ).
    snippet = reader->read( 2 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+5(2) ). " expects FG

    " skip <= 0 or read <= 0 -> exception
    DO 4 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1. reader->skip( 0 ).
            WHEN 2. snippet = reader->read( 0 ).
            WHEN 3. reader->skip( -1 ).
            WHEN 4. snippet = reader->read( -1 ).
          ENDCASE.
        CATCH cx_parameter_invalid_range INTO DATA(lx_length).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'skip 0 or read 0 should do an exception' act = lx_length ).
      CLEAR lx_length.
    ENDDO.

    " MARK, RESET_TO_MARK, DELETE_MARK
    IF abap_true = reader->is_mark_supported( ).

      reader->set_mark( ). " from 8th character
      reader->skip( 2 ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+9(2) ). " expects JK
      reader->reset_to_mark( ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+7(2) ). " expects HI

      reader->delete_mark( ).
      " delete mark if it's not set -> no exception
      reader->delete_mark( ).

      TRY.
          reader->reset_to_mark( ).
        CATCH cx_stream_position_error INTO DATA(lx_no_mark).
      ENDTRY.
      IF expect_exc_reset_if_no_mark = abap_true.
        cl_abap_unit_assert=>assert_bound( msg = 'reset to non-existing mark should do an exception' act = lx_no_mark ).
      ELSE.
        cl_abap_unit_assert=>assert_not_bound( msg = 'reset to non-existing mark should NOT do an exception' act = lx_no_mark ).
      ENDIF.

    ELSE.
      reader->skip( 3 ).
    ENDIF.

    " here, cursor should be before 10th character

    " RESET
    " RESET being equivalent to doing SET_MARK at beginning of stream + RESET_TO_MARK,
    " if MARK is supported, then RESET is usually supported.
    " But if MARK is not supported, RESET may be supported.
    IF abap_true = reader->is_reset_supported( ).
      reader->reset( ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+0(2) ). " expects AB
      reader->skip( 8 ).
    ENDIF.

    " here, cursor should be before 10th character

    " END OF STREAM: DATA_AVAILABLE, READ, CLOSE
    reader->skip( 15 ).
    cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    IF abap_true = reader->is_mark_supported( ).
      reader->set_mark( ).
    ENDIF.

    " last character
    snippet = reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+25(1) ). " expects Z
    IF expect_avail_last_char = abap_true.
      cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    ELSE.
      cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_false ).
    ENDIF.

    IF abap_true = reader->is_mark_supported( ).
      reader->reset_to_mark( ).
      cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
      reader->skip( 1 ).
    ENDIF.

    " beyond last character
    TRY.
        snippet = reader->read( 1 ).
        cl_abap_unit_assert=>assert_initial( msg = 'read should return empty read after end' act = snippet ).
      CATCH cx_root INTO DATA(lx_end1).
    ENDTRY.
    IF expect_read_auto_close = abap_true.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception end of stream' act = lx_end1 ).
      cl_abap_unit_assert=>assert_equals( msg = 'read beyond last character -> expects cx_resource_already_closed' exp = abap_true act = xsdbool( lx_end1 IS INSTANCE OF cx_resource_already_closed ) ).
      cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should be true' exp = abap_true act = reader->is_closed( ) ).
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( msg = 'expecting NO exception end of stream' act = lx_end1 ).
      " read again
      TRY.
          snippet = reader->read( 1 ).
        CATCH cx_root INTO DATA(lx_end2).
      ENDTRY.
      IF expect_exc_read_beyond_end = abap_true.
        cl_abap_unit_assert=>assert_bound( msg = 'expecting exception read after end' act = lx_end2 ).
      ELSE.
        cl_abap_unit_assert=>assert_initial( msg = 'read should return empty read after end' act = snippet ).
      ENDIF.
    ENDIF.

    " close
    reader->close( ).

    " close twice, no exception
    reader->close( ).

    " exceptions for all operations on a closed stream
    DO 7 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1. reader->read( 1 ).
            WHEN 2. reader->delete_mark( ).
            WHEN 3. reader->reset( ).
            WHEN 4. reader->reset_to_mark( ).
            WHEN 5. reader->skip( 1 ).
            WHEN 6. reader->data_available( ).
            WHEN 7. reader->set_mark( ).
          ENDCASE.
        CATCH cx_resource_already_closed INTO DATA(lx_closed).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception on closed stream' act = lx_closed ).
    ENDDO.

  ENDMETHOD.

  METHOD test_x_reader.

    DATA: snippet TYPE xstring.

    " BEGIN OF STREAM: SKIP, READ, DATA_AVAILABLE
    cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    reader->skip( 3 ).
    snippet = reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( zcl_io_test=>_01_to_1a+3(1) ) ). " expects D
    reader->skip( 1 ).
    snippet = reader->read( 2 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( zcl_io_test=>_01_to_1a+5(2) ) ). " expects FG

    " skip <= 0 or read <= 0 -> exception
    DO 4 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1. reader->skip( 0 ).
            WHEN 2. snippet = reader->read( 0 ).
            WHEN 3. reader->skip( -1 ).
            WHEN 4. snippet = reader->read( -1 ).
          ENDCASE.
        CATCH cx_parameter_invalid_range INTO DATA(lx_length).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'skip 0 or read 0 should do an exception' act = lx_length ).
      CLEAR lx_length.
    ENDDO.


    " MARK, RESET_TO_MARK, DELETE_MARK
    IF abap_true = reader->is_mark_supported( ).

      reader->set_mark( ). " from 8th byte
      reader->skip( 2 ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( zcl_io_test=>_01_to_1a+9(2) ) ). " expects JK
      reader->reset_to_mark( ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( zcl_io_test=>_01_to_1a+7(2) ) ). " expects HI

      reader->delete_mark( ).
      " delete mark if it's not set -> no exception
      reader->delete_mark( ).

      TRY.
          reader->reset_to_mark( ).
        CATCH cx_stream_position_error INTO DATA(lx_no_mark).
      ENDTRY.
      IF expect_exc_reset_if_no_mark = abap_true.
        cl_abap_unit_assert=>assert_bound( msg = 'reset to non-existing mark should do an exception' act = lx_no_mark ).
        cl_abap_unit_assert=>assert_equals( msg = 'reset to non-existing mark should trigger cx_mark_not_set' act = lx_no_mark->textid exp = cx_stream_position_error=>cx_mark_not_set ).
      ELSE.
        cl_abap_unit_assert=>assert_not_bound( msg = 'reset to non-existing mark should NOT do an exception' act = lx_no_mark ).
      ENDIF.

    ELSE.
      reader->skip( 3 ).
    ENDIF.

    " here, cursor should be before 10th byte

    " RESET
    " RESET being equivalent to doing SET_MARK at beginning of stream + RESET_TO_MARK,
    " if MARK is supported, then RESET is usually supported.
    " But if MARK is not supported, RESET may be supported.
    IF abap_true = reader->is_reset_supported( ).
      reader->reset( ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( zcl_io_test=>_01_to_1a+0(2) ) ). " expects AB
      reader->skip( 8 ).
    ENDIF.

    " here, cursor should be before 10th byte

    " END OF STREAM: DATA_AVAILABLE, READ, CLOSE
    reader->skip( 15 ).
    cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    IF abap_true = reader->is_mark_supported( ).
      reader->set_mark( ).
    ENDIF.

    " last byte
    snippet = reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( zcl_io_test=>_01_to_1a+25(1) ) ). " expects Z
    IF expect_avail_last_char = abap_true.
      cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    ELSE.
      cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_false ).
    ENDIF.

    IF abap_true = reader->is_mark_supported( ).
      reader->reset_to_mark( ).
      cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
      reader->skip( 1 ).
    ENDIF.

    " beyond last byte
    TRY.
        snippet = reader->read( 1 ).
        cl_abap_unit_assert=>assert_initial( msg = 'read should return empty read after end' act = snippet ).
      CATCH cx_root INTO DATA(lx_end1).
    ENDTRY.
    IF expect_read_auto_close = abap_true.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception end of stream' act = lx_end1 ).
      cl_abap_unit_assert=>assert_equals( msg = 'read beyond last byte -> expects cx_resource_already_closed' exp = abap_true act = xsdbool( lx_end1 IS INSTANCE OF cx_resource_already_closed ) ).
      cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should be true' exp = abap_true act = reader->is_closed( ) ).
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( msg = 'expecting NO exception end of stream' act = lx_end1 ).
      " read again
      TRY.
          snippet = reader->read( 1 ).
        CATCH cx_root INTO DATA(lx_end2).
      ENDTRY.
      IF expect_exc_read_beyond_end = abap_true.
        cl_abap_unit_assert=>assert_bound( msg = 'expecting exception read after end' act = lx_end2 ).
      ELSE.
        cl_abap_unit_assert=>assert_initial( msg = 'read should return empty read after end' act = snippet ).
      ENDIF.
    ENDIF.

    " close
    reader->close( ).

    " close twice, no exception
    reader->close( ).

    " exceptions for all operations on a closed stream
    DO 7 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1. reader->read( 1 ).
            WHEN 2. reader->delete_mark( ).
            WHEN 3. reader->reset( ).
            WHEN 4. reader->reset_to_mark( ).
            WHEN 5. reader->skip( 1 ).
            WHEN 6. reader->data_available( ).
            WHEN 7. reader->set_mark( ).
          ENDCASE.
        CATCH cx_resource_already_closed INTO DATA(lx_closed).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception on closed stream' act = lx_closed ).
    ENDDO.

  ENDMETHOD.

  METHOD test_c_writer.

    cl_abap_unit_assert=>assert_equals( msg = 'IS_X_WRITER should return false' exp = ABAp_false act = writer->is_x_writer( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should return false' exp = ABAp_false act = writer->is_closed( ) ).

    DO 2 TIMES.
      writer->write( substring( val = sy-abcde off = ( sy-index - 1 ) * 13 len = 13 ) ).
    ENDDO.

    writer->close( ).
    cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should return true' exp = ABAp_true act = writer->is_closed( ) ).

    writer->close( ).
    cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should return true' exp = ABAp_true act = writer->is_closed( ) ).

    " exceptions for all operations on a closed stream
    DO 2 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1. writer->write( 'A' ).
            WHEN 2. writer->flush( ).
          ENDCASE.
        CATCH cx_resource_already_closed INTO DATA(lx_closed).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception on closed stream' act = lx_closed ).
    ENDDO.

  ENDMETHOD.

  METHOD test_x_writer.

    cl_abap_unit_assert=>assert_equals( msg = 'IS_X_WRITER should return true' exp = ABAp_true act = writer->is_x_writer( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should return false' exp = ABAp_false act = writer->is_closed( ) ).

    DO 2 TIMES.
      data(i) = ( sy-index - 1 ) * 13.
      writer->write( conv #( zcl_io_test=>_01_to_1a+i(13) ) ).
    ENDDO.

    writer->close( ).
    cl_abap_unit_assert=>assert_equals( msg = 'IS_CLOSED should return true' exp = ABAp_true act = writer->is_closed( ) ).

    writer->close( ).

    " exceptions for all operations on a closed stream
    DO 2 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1. writer->write( 'A' ).
            WHEN 2. writer->flush( ).
          ENDCASE.
        CATCH cx_resource_already_closed INTO DATA(lx_closed).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception on closed stream' act = lx_closed ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.
