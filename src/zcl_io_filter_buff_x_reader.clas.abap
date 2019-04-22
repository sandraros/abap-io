class ZCL_IO_FILTER_BUFF_X_READER definition
  public
  inheriting from ZCL_IO_FILTER_X_READER
  create public .

public section.

  data BUFFER type XSTRING read-only .
  data OFFSET type I read-only .

  methods CONSTRUCTOR
    importing
      !IO_X_READER type ref to ZIF_IO_X_READER
      !I_BUFFER_SIZE type I default 10000
    raising
      ZCX_IO_PARAMETER_INVALID_RANGE .
protected section.
private section.

  data AI_BUFFER_SIZE type I .
  data AI_DATA_AVAILABLE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  data M_MARK type I value -1 ##NO_TEXT.

  methods READ_INTERNAL
    importing
      !LENGTH type ABAP_MSIZE
    returning
      value(RESULT) type XSTRING .
ENDCLASS.



CLASS ZCL_IO_FILTER_BUFF_X_READER IMPLEMENTATION.


  method CONSTRUCTOR.

    CALL METHOD super->constructor
      EXPORTING
        io_x_reader = io_x_reader.
    ai_buffer_size = i_buffer_size.

  endmethod.


  method READ_INTERNAL.

    DATA l_new_offset TYPE i.
    DATA l_xstring TYPE xstring.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF length <= 0.
      "exception
    ENDIF.
    l_new_offset = offset + length.
    IF l_new_offset > XSTRLEN( buffer ).
      "cas où le buffer ne contient pas le nombre d'octets demandé
      l_xstring = aoo_x_reader->read( ai_buffer_size ).
      CONCATENATE buffer l_xstring INTO buffer IN BYTE MODE.
    ENDIF.
    IF l_new_offset > XSTRLEN( buffer ).
      "la lecture n'a pas permis d'en trouver assez
      ai_data_available = abap_false.
      result = buffer+offset. "retourner tout ce qui reste
    ELSE.
      result = buffer+offset(length).
    ENDIF.
    IF m_mark = -1.
      "s'il n'y a pas de marqueur, alors enlever la partie inutile du
      "buffer et le pointeur reste au début du buffer
      IF l_new_offset = XSTRLEN( buffer ).
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


  endmethod.
ENDCLASS.
