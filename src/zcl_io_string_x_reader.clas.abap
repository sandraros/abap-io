"! <p class="shorttext synchronized" lang="en">Byte string reader</p>
"!
CLASS zcl_io_string_x_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_memory_x_reader
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_x_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_string_reader .

    METHODS constructor
      IMPORTING
        !xstr TYPE xstring OPTIONAL .

    METHODS zif_io_reader~is_mark_supported
        REDEFINITION .
    METHODS zif_io_reader~is_reset_supported
        REDEFINITION .
    METHODS zif_io_reader~reset
        REDEFINITION .
    METHODS zif_io_reader~reset_to_mark
        REDEFINITION .
    METHODS zif_io_reader~set_mark
        REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA m_str TYPE xstring .
    TYPE-POOLS abap.
    DATA m_offset TYPE abap_msize VALUE 0 ##NO_TEXT.
    DATA m_mark TYPE abap_msize VALUE -1 ##NO_TEXT.

    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        !length       TYPE abap_msize
      RETURNING
        VALUE(result) TYPE xstring .
ENDCLASS.



CLASS zcl_io_string_x_reader IMPLEMENTATION.


  METHOD constructor.

    CALL METHOD super->constructor.
    m_str = xstr.

  ENDMETHOD.


  METHOD data_available_internal.

    IF m_offset < xstrlen( m_str ).
      available = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD read_internal.

    IF length + m_offset > xstrlen( m_str ).
      result = m_str+m_offset(*).
    ELSE.
      result = m_str+m_offset(length).
    ENDIF.
    m_offset = m_offset + length.

  ENDMETHOD.


  METHOD zif_io_reader~is_mark_supported.

    res = abap_true.

  ENDMETHOD.


  METHOD zif_io_reader~is_reset_supported.

    result = abap_true.

  ENDMETHOD.


  METHOD zif_io_reader~reset.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_offset = 0.
    m_mark = -1.

  ENDMETHOD.


  METHOD zif_io_reader~reset_to_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF m_mark = -1.
      RAISE EXCEPTION TYPE zcx_io_stream_position_error
        EXPORTING
          textid = zcx_io_stream_position_error=>zcx_io_mark_not_set.
    ENDIF.
    m_offset = m_mark.

  ENDMETHOD.


  METHOD zif_io_reader~set_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_mark = m_offset.

  ENDMETHOD.


ENDCLASS.
