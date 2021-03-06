"! <p class="shorttext synchronized" lang="en">Internal table character reader</p>
CLASS zcl_io_itab_c_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_memory_c_reader
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_reader .

  PUBLIC SECTION.

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

    DATA m_itab TYPE REF TO data.
    "! Current line of stream (position)
    DATA m_line_index TYPE i VALUE 1.
    "! Offset in current line of stream (position)
    DATA m_position TYPE i VALUE 0.
    DATA m_line_index_mark TYPE i VALUE 0.
    DATA m_pos_mark TYPE i VALUE -1.
    DATA m_line_type TYPE REF TO cl_abap_datadescr.
    TYPE-POOLS abap .
    DATA m_data_available TYPE abap_bool VALUE abap_true.
    DATA m_line_type_is_string TYPE abap_bool.
    "! If internal table has lines of type C, length of lines (0 for lines of type String)
    DATA m_line_length TYPE i.
    DATA:
      "! Offset in stream (position)
      m_global_position     TYPE i VALUE 0,
      m_global_pos_mark     TYPE i VALUE -1,
      m_data_available_mark TYPE abap_bool,
      "! Length of stream (-1 = whole ITAB)
      m_length              TYPE i.

    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .

    METHODS read_internal
      IMPORTING
        VALUE(length) TYPE abap_msize
      RETURNING
        VALUE(result) TYPE string .

    "! Determines whether the stream is empty for DATA_AVAILABLE
    METHODS find_first .

ENDCLASS.



CLASS zcl_io_itab_c_reader IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS <input> TYPE STANDARD TABLE.
    DATA itab_desc TYPE REF TO cl_abap_tabledescr.

    super->constructor( ).

    m_itab = REF #( itab ).
    m_length = nmax( val1 = length val2 = -1 ).

    " Check type
    itab_desc ?= cl_abap_typedescr=>describe_by_data( itab ).
    m_line_type = itab_desc->get_table_line_type( ).
    IF m_line_type->type_kind <> cl_abap_typedescr=>typekind_char AND
       m_line_type->type_kind <> cl_abap_typedescr=>typekind_string.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `ITAB`
          type      = m_line_type->get_relative_name( ).
    ENDIF.

    IF m_line_type->type_kind = cl_abap_typedescr=>typekind_string.
      m_line_type_is_string = abap_true.
    ELSE.
      m_line_length = m_line_type->length / cl_abap_char_utilities=>charsize.
    ENDIF.

    find_first( ).

  ENDMETHOD.


  METHOD data_available_internal.
    available = m_data_available.
  ENDMETHOD.


  METHOD find_first.
    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line>  TYPE csequence.
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
      IF m_line_type_is_string = abap_false.
        EXIT.
      ELSE.
        ASSIGN line->* TO <line> CASTING TYPE HANDLE m_line_type.
        IF <line> IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDIF.
      m_line_index = m_line_index + 1.
    ENDDO.
  ENDMETHOD.


  METHOD read_internal.
    "BY KERNEL MODULE ab_km_ItabCReadInternal.
    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line>  TYPE csequence.
    DATA: line         TYPE REF TO data,
          l_take       TYPE i,
          "! Number of characters left in the current line, after the current offset (m_position)
          l_remain_eol TYPE i,
          "! Number of characters left to be taken from the stream (counter)
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
        IF m_line_type_is_string = abap_true.
          l_remain_eol = strlen( <line> ) - m_position.
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
          " (note: l_take may be 0 with tables of type String, if a line = empty String)
          CONCATENATE result <line>+m_position(l_take) INTO result.
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
          " The requested number of characters have been extracted
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
    find_first( ).
    m_line_index_mark = 0.
    m_pos_mark = -1.
    m_global_position = 0.
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
