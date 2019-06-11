"! <p class="shorttext synchronized" lang="en">Itab character reader</p>
class ZCL_IO_ITAB_C_READER definition
  public
  inheriting from ZCL_IO_MEMORY_C_READER
  create public

  global friends ZCL_IO_C_READER .

public section.
  type-pools ABAP .

  interfaces ZIF_IO_ITAB_READER .

  methods CONSTRUCTOR
    importing
      !ITAB type STANDARD TABLE
    raising
      ZCX_IO_PARAMETER_INVALID_TYPE .

protected section.
private section.

  data M_ITAB type ref to DATA .
  data M_LINE_INDEX type I value 1 ##NO_TEXT.
  data M_POSITION type I value 0 ##NO_TEXT.
  data M_LINE_INDEX_MARK type I value 0 ##NO_TEXT.
  data M_POS_MARK type I value -1 ##NO_TEXT.
  data M_LINE_TYPE type ref to CL_ABAP_DATADESCR .
  data M_DATA_AVAILABLE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  data M_LINE_TYPE_IS_STRING type ABAP_BOOL .
  data M_LINE_LENGTH type I .

  methods DATA_AVAILABLE_INTERNAL
    returning
      value(AVAILABLE) type ABAP_BOOL .
  methods READ_INTERNAL
    importing
      value(LENGTH) type ABAP_MSIZE
    returning
      value(RESULT) type STRING .
  methods FIND_FIRST .
ENDCLASS.



CLASS ZCL_IO_ITAB_C_READER IMPLEMENTATION.


  method CONSTRUCTOR.

    FIELD-SYMBOLS <input> TYPE STANDARD TABLE.
    DATA itab_desc TYPE REF TO cl_abap_tabledescr .
    DATA l_name TYPE string.

    super->constructor( ).
    " Check type
    itab_desc ?= cl_abap_typedescr=>describe_by_data( itab ).
    m_line_type = itab_desc->get_table_line_type( ).
    IF m_line_type->type_kind <> cl_abap_typedescr=>typekind_char AND
       m_line_type->type_kind <> cl_abap_typedescr=>typekind_string.
      l_name = m_line_type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `ITAB`
          type      = l_name.
    ENDIF.
    IF m_line_type->type_kind = cl_abap_typedescr=>typekind_string.
      m_line_type_is_string = abap_true.
    ELSE.
      m_line_length = m_line_type->length.
    ENDIF.
    CREATE DATA m_itab LIKE itab.
    ASSIGN m_itab->* TO <input> CASTING LIKE itab.
    <input> = itab.
    find_first( ).

  endmethod.


  method DATA_AVAILABLE_INTERNAL.

    available = m_data_available.

  endmethod.


  method FIND_FIRST.

    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line> TYPE csequence.
    DATA line TYPE REF TO data.
    ASSIGN m_itab->* TO <input>.
    DO.
      READ TABLE <input> INDEX m_line_index REFERENCE INTO line.
      IF sy-subrc <> 0.
        m_data_available = abap_false.
        RETURN.
      ENDIF.
      ASSIGN line->* TO <line> CASTING TYPE HANDLE m_line_type.
      IF <line> IS NOT INITIAL.
        EXIT.
      ENDIF.
      m_line_index = m_line_index + 1.
    ENDDO.

  endmethod.


  method READ_INTERNAL.
 "BY KERNEL MODULE ab_km_ItabCReadInternal.
    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line> TYPE csequence.
    DATA line TYPE REF TO data.
    DATA l_take TYPE i.
    DATA l_remain_eol TYPE i.
    DATA l_remain TYPE i.

    IF abap_true = data_available_internal( ) AND length > 0.
      l_remain = length.
      ASSIGN m_itab->* TO <input>.
      DO.
        READ TABLE <input> INDEX m_line_index REFERENCE INTO line.
        IF sy-subrc <> 0.
          m_data_available = abap_false.
          EXIT.
        ENDIF.
        ASSIGN line->* TO <line> CASTING TYPE HANDLE m_line_type.
        IF m_line_type_is_string = abap_true.
          l_remain_eol = STRLEN( <line> ) - m_position.
        ELSE.
          l_remain_eol = m_line_length - m_position.
        ENDIF.
        IF l_remain_eol > l_remain.
          l_take = l_remain.
        ELSE.
          l_take = l_remain_eol.
        ENDIF.
        IF l_take > 0.
          CONCATENATE result <line>+m_position(l_take) INTO result.

          IF l_take = l_remain_eol.
            ADD 1 TO m_line_index.
            m_position = 0.
          ELSE.
            ADD l_take TO m_position.
          ENDIF.

          SUBTRACT l_take FROM l_remain.
          IF l_remain = 0.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.

  endmethod.



ENDCLASS.
