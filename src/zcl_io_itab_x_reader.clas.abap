"! <p class="shorttext synchronized" lang="en">Internal table byte reader</p>
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
        !length TYPE i DEFAULT -1
      RAISING
        zcx_io_parameter_invalid_type .

    METHODS zif_io_reader~delete_mark
        REDEFINITION .
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

    DATA: m_itab                 TYPE REF TO data,
          "! Length of stream (-1 = whole ITAB)
          m_length               TYPE i,
          "! Current line of stream (position)
          m_line_index           TYPE i VALUE 1,
          "! Offset in current line of stream (position)
          m_position             TYPE i VALUE 0,
          "! Offset in stream (position)
          m_global_position      TYPE i VALUE 0,
          m_line_index_mark      TYPE i VALUE 0,
          m_pos_mark             TYPE i VALUE -1,
          m_global_pos_mark      TYPE i VALUE -1,
          m_line_type            TYPE REF TO cl_abap_datadescr,
          m_data_available       TYPE abap_bool VALUE abap_true,
          m_data_available_mark  TYPE abap_bool,
          m_line_type_is_xstring TYPE abap_bool,
          "! If internal table has lines of type C, length of lines (0 for lines of type String)
          m_line_length          TYPE i.

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
    DATA: itab_desc TYPE REF TO cl_abap_tabledescr,
          l_name    TYPE string.

    super->constructor( ).
    m_itab = REF #( itab ).
    m_length = nmax( val1 = length val2 = -1 ).

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
*    CREATE DATA m_itab LIKE itab.
*    ASSIGN m_itab->* TO <input> CASTING LIKE itab.
*    <input> = itab.
    find_first( ).

  ENDMETHOD.


  METHOD data_available_internal.
    available = m_data_available.
  ENDMETHOD.


  METHOD find_first.

    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line>  TYPE xsequence.
    DATA line TYPE REF TO data.

    IF m_length = 0.
      m_data_available = abap_false.
      RETURN.
    ENDIF.

    ASSIGN m_itab->* TO <input>.
    DO.
      READ TABLE <input> INDEX m_line_index REFERENCE INTO line.
      IF sy-subrc <> 0.
        m_data_available = abap_false.
        RETURN.
      ENDIF.
      IF m_line_type_is_xstring = abap_false.
        RETURN.
      ELSE.
        ASSIGN line->* TO <line> CASTING TYPE HANDLE m_line_type.
        IF <line> IS NOT INITIAL.
          RETURN.
        ENDIF.
        m_line_index = m_line_index + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD read_internal.
    "BY KERNEL MODULE ab_km_ItabCReadInternal.
    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line>  TYPE xsequence.
    DATA: line         TYPE REF TO data,
          l_take       TYPE i,
          "! Number of bytes left in the current line, after the current offset (m_position)
          l_remain_eol TYPE i,
          "! Number of bytes left to be taken from the stream (counter)
          l_remain     TYPE i.

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
        IF m_length <> -1.
          DATA(l_remain_eos) = m_length - m_global_position.
          l_remain_eol = nmin( val1 = l_remain_eol val2 = l_remain_eos ).
        ENDIF.
        IF l_remain_eol > l_remain.
          l_take = l_remain.
        ELSE.
          l_take = l_remain_eol.
        ENDIF.

        IF l_take > 0.
          " (note: l_take may be 0 with tables of type XString, if a line = empty XString)
          CONCATENATE result <line>+m_position(l_take) INTO result IN BYTE MODE.
        ENDIF.

        IF l_take = l_remain_eol.
          " end of line
          ADD 1 TO m_line_index.
          m_position = 0.
        ELSE.
          ADD l_take TO m_position.
        ENDIF.
        ADD l_take TO m_global_position.

        SUBTRACT l_take FROM l_remain.
        IF l_remain = 0.
          " The requested number of bytes have been extracted
          IF m_line_index > lines( <input> ) OR ( m_length > -1 AND m_global_position >= m_length ).
            m_data_available = abap_false.
          ENDIF.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

  ENDMETHOD.

  METHOD zif_io_reader~delete_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_line_index_mark = 0.
    m_pos_mark = -1.
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
    m_line_index = 1.
    m_position = 0.
    m_global_position = 0.
    m_line_index_mark = 0.
    m_pos_mark = -1.
    find_first( ).
  ENDMETHOD.

  METHOD zif_io_reader~reset_to_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF m_pos_mark = -1.
      RAISE EXCEPTION TYPE zcx_io_stream_position_error EXPORTING textid = zcx_io_stream_position_error=>zcx_io_mark_not_set.
    ENDIF.
    m_line_index = m_line_index_mark.
    m_position = m_pos_mark.
    m_global_position = m_global_pos_mark.
    m_data_available = m_data_available_mark.
    find_first( ).
  ENDMETHOD.

  METHOD zif_io_reader~set_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_line_index_mark = m_line_index.
    m_pos_mark = m_position.
    m_global_pos_mark = m_global_position.
    m_data_available_mark = m_data_available.
  ENDMETHOD.

ENDCLASS.
