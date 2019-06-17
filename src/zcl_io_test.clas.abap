"! <p class="shorttext synchronized" lang="en">Unit Tests</p>
CLASS zcl_io_test DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS .

  PUBLIC SECTION.

    TYPES: c2      TYPE c LENGTH 2,
           ty_c2_s TYPE STANDARD TABLE OF c2 WITH EMPTY KEY,
           x2      TYPE x LENGTH 2,
           ty_x2_s TYPE STANDARD TABLE OF x2 WITH EMPTY KEY,
           c3      TYPE c LENGTH 3,
           ty_c3_s TYPE STANDARD TABLE OF c3 WITH EMPTY KEY,
           x3      TYPE x LENGTH 3,
           ty_x3_s TYPE STANDARD TABLE OF x3 WITH EMPTY KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS split_c
      IMPORTING
        i_field  TYPE clike
      EXPORTING
        et_chunk TYPE STANDARD TABLE.

    CLASS-METHODS split_x
      IMPORTING
        i_field  TYPE xsequence
      EXPORTING
        et_chunk TYPE STANDARD TABLE.

    CONSTANTS:
      _01_to_1a TYPE x LENGTH 26 VALUE '0102030405060708090A0B0C0D0E0F101112131415161718191A',
      _01_to_1c TYPE x LENGTH 28 VALUE '0102030405060708090A0B0C0D0E0F101112131415161718191A1B1C'.

    CLASS-DATA:
      itab_a_to_z   TYPE zcl_io_test=>ty_c2_s,
      itab_a_to_z01 TYPE zcl_io_test=>ty_c3_s,
      itab_01_to_1a TYPE zcl_io_test=>ty_x2_s,
      itab_01_to_1c TYPE zcl_io_test=>ty_x3_s.

  PROTECTED SECTION.

    METHODS test_reader
      IMPORTING
        !reader TYPE REF TO zif_io_reader .

    METHODS test_writer
      IMPORTING
        !writer TYPE REF TO zif_io_writer .

    METHODS test_c_reader
      IMPORTING
        !reader TYPE REF TO zif_io_c_reader
      RAISING
        cx_static_check cx_dynamic_check .

    METHODS test_c_reader_empty
      IMPORTING
        !reader TYPE REF TO zif_io_c_reader
      RAISING
        cx_static_check cx_dynamic_check .

    METHODS test_c_writer
      IMPORTING
        !writer TYPE REF TO zif_io_c_writer .

    METHODS test_x_reader
      IMPORTING
        !reader TYPE REF TO zif_io_x_reader
      RAISING
        cx_static_check cx_dynamic_check .

    METHODS test_x_reader_empty
      IMPORTING
        !reader TYPE REF TO zif_io_x_reader
      RAISING
        cx_static_check cx_dynamic_check .

    METHODS test_x_writer
      IMPORTING
        !writer TYPE REF TO zif_io_x_writer .

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_test IMPLEMENTATION.

  METHOD class_constructor.
    zcl_io_test=>split_c( EXPORTING i_field = sy-abcde IMPORTING et_chunk = itab_a_to_z ).
    zcl_io_test=>split_c( EXPORTING i_field = sy-abcde && '01' IMPORTING et_chunk = itab_a_to_z01 ).
    zcl_io_test=>split_x( EXPORTING i_field = zcl_io_test=>_01_to_1a IMPORTING et_chunk = itab_01_to_1a ).
    zcl_io_test=>split_x( EXPORTING i_field = _01_to_1c IMPORTING et_chunk = itab_01_to_1c ).
  ENDMETHOD.

  METHOD test_c_reader.

    DATA: snippet TYPE c LENGTH 2.

    " BEGIN OF STREAM: SKIP, READ, DATA_AVAILABLE
    cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    reader->skip( 3 ).
    snippet = reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+3(1) ).
    reader->skip( 1 ).
    snippet = reader->read( 2 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+5(2) ).

    " skip <= 0 or read <= 0 -> exception
    DO 4 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1. reader->skip( 0 ).
            WHEN 2. snippet = reader->read( 0 ).
            WHEN 3. reader->skip( -1 ).
            WHEN 4. snippet = reader->read( -1 ).
          ENDCASE.
        CATCH zcx_io_parameter_invalid_range INTO DATA(lx_length).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'skip <= 0 or read <= 0 should do an exception' act = lx_length ).
      CLEAR lx_length.
    ENDDO.

    " MARK, RESET_TO_MARK, DELETE_MARK
    IF abap_true = reader->is_mark_supported( ).

      reader->set_mark( ). " from 8th character
      reader->skip( 2 ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+9(2) ).
      reader->reset_to_mark( ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+7(2) ).

      reader->delete_mark( ).
      " delete mark if it's not set -> no exception
      reader->delete_mark( ).

      TRY.
          reader->reset_to_mark( ).
        CATCH zcx_io_stream_position_error INTO DATA(lx_no_mark).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( act = lx_no_mark ).
      cl_abap_unit_assert=>assert_equals( msg = 'reset to non-existing mark should trigger zcx_io_mark_not_set' act = lx_no_mark->textid exp = zcx_io_stream_position_error=>zcx_io_mark_not_set ).

    ELSE.
      reader->skip( 2 ).
    ENDIF.

    " here, cursor should be before 10th character

    " RESET
    " RESET being equivalent to doing SET_MARK at beginning of stream + RESET_TO_MARK,
    " so if MARK is supported, then RESET is supported too.
    " But if MARK is not supported, RESET may be supported.
    IF abap_true = reader->is_reset_supported( ).
      reader->reset( ).

      TRY.
          reader->reset_to_mark( ).
        CATCH zcx_io_stream_position_error INTO DATA(lx_no_mark2).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( act = lx_no_mark2 ).
      cl_abap_unit_assert=>assert_equals( msg = 'reset to non-existing mark should trigger zcx_io_mark_not_set' act = lx_no_mark2->textid exp = zcx_io_stream_position_error=>zcx_io_mark_not_set ).

      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+0(2) ).

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
    cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = sy-abcde+25(1) ).

    IF abap_true = reader->is_mark_supported( ).
      reader->reset_to_mark( ).
      cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    ENDIF.

    " beyond last character
    TRY.
        snippet = reader->read( 1 ).
      CATCH zcx_io_stream_error INTO DATA(lx_end).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_end ).

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
        CATCH zcx_io_resource_already_closed INTO DATA(lx_closed).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception on closed stream' act = lx_closed ).
    ENDDO.

  ENDMETHOD.


  METHOD test_c_reader_empty.

    cl_abap_unit_assert=>assert_equals( msg = 'after constructor of empty stream, data_available should be false' act = reader->data_available( ) exp = abap_false ).

  ENDMETHOD.


  METHOD test_c_writer.

    DO 2 TIMES.
      writer->write( CONV #( substring( val = sy-abcde off = ( sy-index - 1 ) * 13 len = 13 ) ) ).
      writer->flush( ).
    ENDDO.

    writer->close( ).

    writer->close( ).

  ENDMETHOD.


  METHOD test_reader.

    DATA lo_c_reader TYPE REF TO zif_io_c_reader.
    DATA lo_x_reader TYPE REF TO zif_io_x_reader.
    IF abap_true = reader->is_x_reader( ).
      lo_x_reader ?= reader.
      CALL METHOD test_x_reader
        EXPORTING
          reader = lo_x_reader.
    ELSE.
      lo_c_reader ?= reader.
      CALL METHOD test_c_reader
        EXPORTING
          reader = lo_c_reader.
    ENDIF.

  ENDMETHOD.


  METHOD test_writer.


  ENDMETHOD.


  METHOD test_x_reader.

    DATA: snippet TYPE xstring.

    " BEGIN OF STREAM: SKIP, READ, DATA_AVAILABLE
    cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
    reader->skip( 3 ).
    snippet = reader->read( 1 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( _01_to_1a+3(1) ) ).
    reader->skip( 1 ).
    snippet = reader->read( 2 ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( _01_to_1a+5(2) ) ).

    " skip <= 0 or read <= 0 -> exception
    DO 4 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1. reader->skip( 0 ).
            WHEN 2. snippet = reader->read( 0 ).
            WHEN 3. reader->skip( -1 ).
            WHEN 4. snippet = reader->read( -1 ).
          ENDCASE.
        CATCH zcx_io_parameter_invalid_range INTO DATA(lx_length).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'skip <= 0 or read <= 0 should do an exception' act = lx_length ).
      CLEAR lx_length.
    ENDDO.

    " MARK, RESET_TO_MARK, DELETE_MARK
    IF abap_true = reader->is_mark_supported( ).

      reader->set_mark( ). " from 8th character
      reader->skip( 2 ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( _01_to_1a+9(2) ) ).
      reader->reset_to_mark( ).
      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( _01_to_1a+7(2) ) ).

      reader->delete_mark( ).
      " delete mark if it's not set -> no exception
      reader->delete_mark( ).

      TRY.
          reader->reset_to_mark( ).
        CATCH zcx_io_stream_position_error INTO DATA(lx_no_mark).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( act = lx_no_mark ).
      cl_abap_unit_assert=>assert_equals( msg = 'reset to non-existing mark should trigger zcx_io_mark_not_set' act = lx_no_mark->textid exp = zcx_io_stream_position_error=>zcx_io_mark_not_set ).

    ELSE.
      reader->skip( 2 ).
    ENDIF.

    " here, cursor should be before 10th character

    " RESET
    " RESET being equivalent to doing SET_MARK at beginning of stream + RESET_TO_MARK,
    " so if MARK is supported, then RESET is supported too.
    " But if MARK is not supported, RESET may be supported.
    IF abap_true = reader->is_reset_supported( ).
      reader->reset( ).

      TRY.
          reader->reset_to_mark( ).
        CATCH zcx_io_stream_position_error INTO DATA(lx_no_mark2).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( act = lx_no_mark2 ).
      cl_abap_unit_assert=>assert_equals( msg = 'reset to non-existing mark should trigger zcx_io_mark_not_set' act = lx_no_mark2->textid exp = zcx_io_stream_position_error=>zcx_io_mark_not_set ).

      snippet = reader->read( 2 ).
      cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( _01_to_1a+0(2) ) ).

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
    cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = snippet exp = CONV xstring( _01_to_1a+25(1) ) ).

    IF abap_true = reader->is_mark_supported( ).
      reader->reset_to_mark( ).
      cl_abap_unit_assert=>assert_equals( act = reader->data_available( ) exp = abap_true ).
      reader->skip( 1 ).
    ENDIF.

    " beyond last character
    TRY.
        snippet = reader->read( 1 ).
      CATCH zcx_io_stream_error INTO DATA(lx_end).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_end ).

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
        CATCH zcx_io_resource_already_closed INTO DATA(lx_closed).
      ENDTRY.
      cl_abap_unit_assert=>assert_bound( msg = 'expecting exception on closed stream' act = lx_closed ).
    ENDDO.

  ENDMETHOD.

  METHOD test_x_reader_empty.
    cl_abap_unit_assert=>assert_equals( msg = 'after constructor of empty stream, data_available should be false' act = reader->data_available( ) exp = abap_false ).
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
*        reader   = lo_string_x_reader "containing GZIPped data
*        i_gzip_buffer = 1000.
*    l_xstring = lo_gzip_x_reader->read( 100 ). "lire 100 octets dÃ©compressÃ©s
*
*    CREATE OBJECT lo_gzip_c_reader
*      EXPORTING
*        reader   = lo_string_x_reader
*        i_gzip_buffer = 1000.
*    l_string = lo_gzip_c_reader->read( 100 ). "lire 100 caractÃ¨res dÃ©compressÃ©s

  ENDMETHOD.

  METHOD split_c.
    DATA(i) = CAST cl_abap_elemdescr( CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( et_chunk ) )->get_table_line_type( ) )->length / cl_abap_char_utilities=>charsize.
    FIND ALL OCCURRENCES OF REGEX replace( val = '.{5}|.{1,5}$' sub = '5' with = |{ i }| occ = 0 ) IN i_field RESULTS DATA(matches).
    et_chunk = VALUE string_table( FOR <match> IN matches ( CONV #( i_field+<match>-offset(<match>-length) ) ) ).
  ENDMETHOD.

  METHOD split_x.
    DATA(i) = CAST cl_abap_elemdescr( CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( et_chunk ) )->get_table_line_type( ) )->length.
    et_chunk = VALUE xstring_table( FOR j = 0 THEN j + i WHILE j < xstrlen( i_field )
        LET i2 = nmin( val1 = i val2 = xstrlen( i_field ) - j ) IN ( CONV xstring( i_field+j(i2) ) ) ).
  ENDMETHOD.

ENDCLASS.
