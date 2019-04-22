class ZCL_IO_STRING_C_READER definition
  public
  inheriting from ZCL_IO_MEMORY_C_READER
  final
  create public

  global friends ZCL_IO_C_READER .

public section.
  type-pools ABAP .

  interfaces ZIF_IO_STRING_READER .

  methods CONSTRUCTOR
    importing
      !STR type STRING .

  methods DELETE_MARK
    redefinition .
  methods ZIF_IO_READER~IS_MARK_SUPPORTED
    redefinition .
  methods ZIF_IO_READER~IS_RESET_SUPPORTED
    redefinition .
  methods ZIF_IO_READER~RESET
    redefinition .
  methods ZIF_IO_READER~RESET_TO_MARK
    redefinition .
  methods ZIF_IO_READER~SET_MARK
    redefinition .
protected section.
private section.

  data M_STR type STRING .
  data M_OFFSET type ABAP_MSIZE value 0 ##NO_TEXT.
  data M_MARK type ABAP_MSIZE value -1 ##NO_TEXT.

  methods DATA_AVAILABLE_INTERNAL
    returning
      value(AVAILABLE) type ABAP_BOOL .
  methods READ_INTERNAL
    importing
      !LENGTH type NUMERIC
    returning
      value(RESULT) type STRING .
ENDCLASS.



CLASS ZCL_IO_STRING_C_READER IMPLEMENTATION.


  method CONSTRUCTOR.

    super->constructor( ).
    m_str = str.

  endmethod.


  method DATA_AVAILABLE_INTERNAL.

    IF m_offset < STRLEN( m_str ).
      available = abap_true.
    ENDIF.

  endmethod.


  method DELETE_MARK.
*CALL METHOD SUPER->DELETE_MARK
*    .
  endmethod.


  method READ_INTERNAL.

    DATA l_dummy TYPE i.
    l_dummy = length + m_offset.
    IF l_dummy > STRLEN( m_str ).
      result = m_str+m_offset(*).
    ELSE.
      result = m_str+m_offset(length).
    ENDIF.
    m_offset = l_dummy.

  endmethod.


  method ZIF_IO_READER~IS_MARK_SUPPORTED.

    res = abap_true.

  endmethod.


  method ZIF_IO_READER~IS_RESET_SUPPORTED.

    result = abap_true.

  endmethod.


  method ZIF_IO_READER~RESET.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_offset = 0.
    m_mark = -1.

  endmethod.


  method ZIF_IO_READER~RESET_TO_MARK.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF m_mark = -1.
      RAISE EXCEPTION TYPE zcx_io_stream_position_error
        EXPORTING textid = zcx_io_stream_position_error=>zcx_io_mark_not_set.
    ENDIF.
    m_offset = m_mark.

  endmethod.


  method ZIF_IO_READER~SET_MARK.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_mark = m_offset.

  endmethod.
ENDCLASS.
