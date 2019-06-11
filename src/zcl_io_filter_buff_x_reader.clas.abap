"! <p class="shorttext synchronized" lang="en">Byte reader via buffer</p>
CLASS zcl_io_filter_buff_x_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_filter_x_reader
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA buffer TYPE xstring READ-ONLY .
    DATA offset TYPE i READ-ONLY .

    METHODS constructor
      IMPORTING
        !io_x_reader   TYPE REF TO zif_io_x_reader
        !i_buffer_size TYPE i DEFAULT 10000
      RAISING
        zcx_io_parameter_invalid_range .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ai_buffer_size TYPE i .
    DATA ai_data_available TYPE abap_bool VALUE abap_true ##NO_TEXT.
    DATA m_mark TYPE i VALUE -1 ##NO_TEXT.

    METHODS read_internal
      IMPORTING
        !length       TYPE abap_msize
      RETURNING
        VALUE(result) TYPE xstring .
ENDCLASS.



CLASS zcl_io_filter_buff_x_reader IMPLEMENTATION.


  METHOD constructor.

    CALL METHOD super->constructor
      EXPORTING
        io_x_reader = io_x_reader.
    ai_buffer_size = i_buffer_size.

  ENDMETHOD.


  METHOD read_internal.

    DATA l_new_offset TYPE i.
    DATA l_xstring TYPE xstring.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF length <= 0.
      "exception
    ENDIF.
    l_new_offset = offset + length.
    IF l_new_offset > xstrlen( buffer ).
      " Case buffer does not contain enough bytes
      l_xstring = aoo_x_reader->read( ai_buffer_size ).
      CONCATENATE buffer l_xstring INTO buffer IN BYTE MODE.
    ENDIF.
    IF l_new_offset > xstrlen( buffer ).
      " There is not enough bytes remaining
      ai_data_available = abap_false.
      result = buffer+offset. " return all remaining bytes
    ELSE.
      result = buffer+offset(length).
    ENDIF.
    IF m_mark = -1.
      " if there's no marker, alors enlever la partie inutile du
      " buffer et le pointeur reste au dÃ©but du buffer
      IF l_new_offset = xstrlen( buffer ).
        CLEAR buffer.
      ELSE.
        buffer = buffer+l_new_offset.
      ENDIF.
      offset = 0.
    ELSE.
      "s'il y a un marqueur, alors conserver tout le buffer
      "et avancer le pointeur
      ADD length TO offset.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
