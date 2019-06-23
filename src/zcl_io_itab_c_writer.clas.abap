"! <p class="shorttext synchronized" lang="en">Internal table character writer</p>
"!
CLASS zcl_io_itab_c_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_memory_c_writer
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_writer .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    CLASS cl_abap_typedescr DEFINITION LOAD .

    INTERFACES zif_io_itab_writer .

    ALIASES bind_result_area
      FOR zif_io_itab_writer~bind_result_area .
    ALIASES get_max_line_length
      FOR zif_io_itab_writer~get_max_line_length .
    ALIASES get_result_table
      FOR zif_io_itab_writer~get_result_table .

    METHODS constructor
      IMPORTING
        !line_length TYPE i DEFAULT 255
        !line_type   TYPE abap_typecategory DEFAULT cl_abap_typedescr=>typekind_string
      RAISING
        zcx_io_parameter_invalid_range .

    METHODS zif_io_memory_writer~get_result
        REDEFINITION .
    METHODS zif_io_memory_writer~get_result_type
        REDEFINITION .

  PROTECTED SECTION.

    DATA m_line_length TYPE i .
    DATA m_line_type TYPE abap_typecategory.

  PRIVATE SECTION.

    DATA m_ref_table TYPE REF TO data .
    DATA m_ref_length TYPE REF TO i .
    DATA m_ref_offset TYPE REF TO i .

    METHODS create_table.

    METHODS write_internal
      IMPORTING
        !data TYPE string .
ENDCLASS.



CLASS zcl_io_itab_c_writer IMPLEMENTATION.


  METHOD constructor.

    DATA: str TYPE string.

    super->constructor( ).

    m_line_type = line_type.
    m_line_length = line_length.

    IF line_type <> cl_abap_typedescr=>typekind_string AND
       line_type <> cl_abap_typedescr=>typekind_char.
      str = line_type.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_range
        EXPORTING
          parameter = `LINE_TYPE`
          value     = str.
    ENDIF.
    IF line_length <= 0.
      str = line_length.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_range
        EXPORTING
          parameter = `LINE_LENGTH`
          value     = str.
    ENDIF.

  ENDMETHOD.


  METHOD write_internal.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    IF m_ref_table IS NOT BOUND.
      create_table( ).
    ENDIF.
    ASSIGN m_ref_table->* TO <table>.

    DATA(offset) = 0.
    DATA(remain) = strlen( data ).
    WHILE remain > 0.
      " Add a new line
      IF m_ref_offset->* = 0 OR m_ref_offset->* >= m_line_length.
        m_ref_offset->* = 0.
        APPEND INITIAL LINE TO <table> ASSIGNING FIELD-SYMBOL(<line>).
      ELSE.
        READ TABLE <table> INDEX lines( <table> ) ASSIGNING <line>.
      ENDIF.
      " Determine how many characters can be written in the current line
      DATA(length) = nmin( val1 = remain
                           val2 = m_line_length - m_ref_offset->* ).
      " Initialize line
      IF m_line_type = cl_abap_typedescr=>typekind_string.
        <line> = <line> && data+offset(length).
      ELSE.
        <line>+m_ref_offset->* = data+offset(length).
      ENDIF.
      " Update counters
      remain = remain - length.
      offset = offset + length.
      m_ref_offset->* = m_ref_offset->* + length.
      m_ref_length->* = m_ref_length->* + length.
    ENDWHILE.

  ENDMETHOD.


  METHOD create_table.

    IF m_line_type = cl_abap_typedescr=>typekind_string.
      CREATE DATA m_ref_table TYPE STANDARD TABLE OF string.
    ELSE.
      DATA(table_type) = cl_abap_tabledescr=>create( cl_abap_elemdescr=>get_c( m_line_length ) ).
      CREATE DATA m_ref_table TYPE HANDLE table_type.
    ENDIF.
    CREATE DATA m_ref_length TYPE i.
    CREATE DATA m_ref_offset TYPE i.

  ENDMETHOD.


  METHOD zif_io_itab_writer~bind_result_area.

    m_ref_table = REF #( table ).
    DATA(line_rtti) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) )->get_table_line_type( ).
    m_line_type = line_rtti->type_kind.
    IF m_line_type = cl_abap_typedescr=>typekind_string.
      m_line_length = string_line_length.
    ELSE.
      m_line_length = line_rtti->length / cl_abap_char_utilities=>charsize.
    ENDIF.

    IF length IS SUPPLIED.
      m_ref_length = REF #( length ).
    ELSE.
      CREATE DATA m_ref_length TYPE i.
    ENDIF.
    IF length_of_last_line IS SUPPLIED.
      m_ref_offset = REF #( length_of_last_line ).
    ELSE.
      CREATE DATA m_ref_offset TYPE i.
    ENDIF.

  ENDMETHOD.


  METHOD zif_io_itab_writer~get_max_line_length.

    line_length = m_line_length.

  ENDMETHOD.


  METHOD zif_io_itab_writer~get_result_table.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN m_ref_table->* TO <table>.
    ASSERT sy-subrc = 0.
    table = <table>.
    length_of_last_line = m_ref_offset->*.
    length = m_ref_length->*.

  ENDMETHOD.


  METHOD zif_io_memory_writer~get_result.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN m_ref_table->* TO <table>.
    ASSERT sy-subrc = 0.
    result = <table>.
    length_of_last_line = m_ref_offset->*.
    length = m_ref_length->*.

  ENDMETHOD.


  METHOD zif_io_memory_writer~get_result_type.

    result_type ?= cl_abap_typedescr=>describe_by_data_ref( m_ref_table ).

  ENDMETHOD.
ENDCLASS.
