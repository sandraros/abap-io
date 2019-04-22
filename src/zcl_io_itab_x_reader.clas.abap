"! <p class="shorttext synchronized" lang="en">Z</p>
CLASS zcl_io_itab_x_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_memory_x_reader
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_x_reader .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    INTERFACES zif_io_itab_reader .

    METHODS constructor
      IMPORTING
        !itab   TYPE STANDARD TABLE
        !length TYPE i
      RAISING
        zcx_io_parameter_invalid_type .

    METHODS delete_mark
        REDEFINITION .
    METHODS is_mark_supported
        REDEFINITION .
    METHODS is_reset_supported
        REDEFINITION .
    METHODS reset
        REDEFINITION .
    METHODS reset_to_mark
        REDEFINITION .
    METHODS set_mark
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA m_itab TYPE REF TO data .
    DATA m_length TYPE i.
    DATA m_line_index TYPE i VALUE 1 ##NO_TEXT.
    DATA m_position TYPE i VALUE 0 ##NO_TEXT.
    DATA m_line_index_mark TYPE i VALUE 0 ##NO_TEXT.
    DATA m_pos_mark TYPE i VALUE -1 ##NO_TEXT.
    DATA m_line_type TYPE REF TO cl_abap_datadescr .
    DATA m_data_available TYPE abap_bool VALUE abap_true ##NO_TEXT.
    DATA m_line_type_is_xstring TYPE abap_bool .
    DATA m_line_length TYPE i .
    DATA m_global_position TYPE i.

    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        VALUE(length) TYPE abap_msize
      RETURNING
        VALUE(result) TYPE xstring .
    METHODS find_first .
ENDCLASS.



CLASS zcl_io_itab_x_reader IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS <input> TYPE STANDARD TABLE.
    DATA itab_desc TYPE REF TO cl_abap_tabledescr .
    DATA l_name TYPE string.

    super->constructor( ).
    " Check type
    itab_desc ?= cl_abap_typedescr=>describe_by_data( itab ).
    m_line_type = itab_desc->get_table_line_type( ).
    IF m_line_type->type_kind <> cl_abap_typedescr=>typekind_hex AND
       m_line_type->type_kind <> cl_abap_typedescr=>typekind_xstring.
      l_name = m_line_type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `ITAB`
          type      = l_name.
    ENDIF.
    IF m_line_type->type_kind = cl_abap_typedescr=>typekind_xstring.
      m_line_type_is_xstring = abap_true.
    ELSE.
      m_line_length = m_line_type->length.
    ENDIF.
    CREATE DATA m_itab LIKE itab.
    ASSIGN m_itab->* TO <input> CASTING LIKE itab.
    <input> = itab.
    m_length = length.
    find_first( ).

  ENDMETHOD.


  METHOD data_available_internal.

    available = m_data_available.

  ENDMETHOD.


  METHOD delete_mark.
*CALL METHOD SUPER->DELETE_MARK
*    .
  ENDMETHOD.


  METHOD find_first.

    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line>  TYPE xsequence.
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

  ENDMETHOD.


  METHOD is_mark_supported.
*CALL METHOD SUPER->IS_MARK_SUPPORTED
*    .
  ENDMETHOD.


  METHOD is_reset_supported.
*CALL METHOD SUPER->IS_RESET_SUPPORTED
*  RECEIVING
*    RESULT =
*    .
  ENDMETHOD.


  METHOD read_internal.
    "BY KERNEL MODULE ab_km_ItabCReadInternal.
    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line>  TYPE xsequence.
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
        IF m_line_type_is_xstring = abap_true.
          l_remain_eol = xstrlen( <line> ) - m_position.
        ELSE.
          l_remain_eol = m_line_length - m_position.
        ENDIF.
        IF m_global_position + l_remain_eol > m_length.
          l_remain_eol = m_length - m_global_position.
        ENDIF.
        IF l_remain_eol > l_remain.
          l_take = l_remain.
        ELSE.
          l_take = l_remain_eol.
        ENDIF.
        IF l_take > 0.
          CONCATENATE result <line>+m_position(l_take) INTO result IN BYTE MODE.

          IF l_take = l_remain_eol.
            ADD 1 TO m_line_index.
            m_position = 0.
          ELSE.
            ADD l_take TO m_position.
          ENDIF.
          ADD l_take TO m_global_position.

          SUBTRACT l_take FROM l_remain.
          IF l_remain = 0.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.

  ENDMETHOD.


  METHOD reset.
*CALL METHOD SUPER->RESET
**  EXCEPTIONS
**    zcx_io_resource_already_closed = 1
**    zcx_io_stream_position_error   = 2
**    others                         = 3
*        .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
  ENDMETHOD.


  METHOD reset_to_mark.
*CALL METHOD SUPER->RESET_TO_MARK
*    .
  ENDMETHOD.


  METHOD set_mark.
*CALL METHOD SUPER->SET_MARK
*    .
  ENDMETHOD.
ENDCLASS.
