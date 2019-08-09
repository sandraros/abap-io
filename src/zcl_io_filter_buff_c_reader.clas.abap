"! <p class="shorttext synchronized" lang="en">Buffer character reader</p>
CLASS zcl_io_filter_buff_c_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_filter_c_reader
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS newline TYPE string VALUE cl_abap_char_utilities=>newline ##NO_TEXT.
    CONSTANTS cr_lf TYPE string VALUE cl_abap_char_utilities=>cr_lf ##NO_TEXT.
    CONSTANTS regex_line_end_marker TYPE string VALUE `\r\n|\n|\r` ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !reader          TYPE REF TO zif_io_c_reader
        !i_buffer_size   TYPE i DEFAULT 10000
        !line_end_marker TYPE string DEFAULT regex_line_end_marker
      RAISING
        zcx_io_parameter_invalid_range .

    METHODS read_line
      RETURNING
        VALUE(line) TYPE string.

    DATA m_buffer_size TYPE i READ-ONLY .
    DATA m_line_end_marker TYPE string READ-ONLY .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA m_buffer TYPE string.
    DATA m_offset TYPE i.

    METHODS read_internal
      IMPORTING
        !length       TYPE abap_msize
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_io_filter_buff_c_reader IMPLEMENTATION.


  METHOD constructor.

    CALL METHOD super->constructor
      EXPORTING
        reader = reader.

    m_buffer_size = i_buffer_size.

  ENDMETHOD.


  METHOD read_internal.
    DATA: l_new_offset TYPE i,
          l_string     TYPE string.

    IF mo_reader->is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.

    DATA(l_num_remain_to_read) = length.
    WHILE l_num_remain_to_read > 0.

      l_new_offset = m_offset + l_num_remain_to_read.
      IF l_new_offset < strlen( m_buffer ).
        result = result && m_buffer+m_offset(l_num_remain_to_read).
        ADD l_num_remain_to_read TO m_offset.
        RETURN.
      ENDIF.

      " Add all remaining bytes.
      DATA(l_num_to_read) = strlen( m_buffer ) - m_offset.
      result = result && m_buffer+m_offset(l_num_to_read).
      SUBTRACT l_num_to_read FROM l_num_remain_to_read.

      " Fill buffer.
      m_buffer = mo_reader->read( m_buffer_size ).
      m_offset = 0.
      IF m_buffer IS INITIAL.
        " End of input stream
        RETURN.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD read_line.

*    if matches( val = m_buffer regex = regex ).
    line = line && read( m_buffer_size ).
*    endif.

  ENDMETHOD.


ENDCLASS.
