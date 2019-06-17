"! <p class="shorttext synchronized" lang="en">Internal table byte writer</p>
"!
CLASS zcl_io_itab_x_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_memory_x_writer
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_x_writer .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    CLASS cl_abap_typedescr DEFINITION LOAD .

    INTERFACES zif_io_itab_writer .

    ALIASES get_max_line_length
      FOR zif_io_itab_writer~get_max_line_length .
    ALIASES bind_result_area
      FOR zif_io_itab_writer~bind_result_area.
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

    DATA m_table TYPE REF TO data .
    DATA m_line_index TYPE i .
    DATA m_line_length TYPE i .
  PRIVATE SECTION.

    DATA m_offset TYPE i .
    DATA m_line_type TYPE REF TO cl_abap_elemdescr .
    DATA m_line_type_is_xstring TYPE abap_bool .

    METHODS write_internal
      IMPORTING
        !data TYPE xstring .
ENDCLASS.



CLASS zcl_io_itab_x_writer IMPLEMENTATION.


  METHOD constructor.

    DATA table_type TYPE REF TO cl_abap_tabledescr.
    DATA str TYPE string.
    super->constructor( ).
    IF line_type <> cl_abap_typedescr=>typekind_xstring AND
       line_type <> cl_abap_typedescr=>typekind_hex.
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
    IF line_type = cl_abap_typedescr=>typekind_xstring.
      m_line_type = cl_abap_elemdescr=>get_xstring( ).
      m_line_type_is_xstring = abap_true.
    ELSE.
      m_line_type = cl_abap_elemdescr=>get_x( line_length ).
    ENDIF.
    table_type = cl_abap_tabledescr=>create( m_line_type ).
    CREATE DATA m_table TYPE HANDLE table_type.
    m_line_length = line_length.
    m_line_index = 1.

  ENDMETHOD.


  METHOD write_internal.
    "BY KERNEL MODULE ab_km_ItabCWRiteInternal.



  ENDMETHOD.


  METHOD zif_io_itab_writer~get_max_line_length.

    line_length = m_line_length.

  ENDMETHOD.


  METHOD zif_io_itab_writer~get_result_table.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN m_table->* TO <table>.
    ASSERT sy-subrc = 0.
    table = <table>.
    length_of_last_line = m_offset.

  ENDMETHOD.


  METHOD zif_io_memory_writer~get_result.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN m_table->* TO <table>.
    ASSERT sy-subrc = 0.
    result = <table>.
    length_of_last_line = m_offset.

  ENDMETHOD.


  METHOD zif_io_memory_writer~get_result_type.

    result_type ?= cl_abap_typedescr=>describe_by_data_ref( m_table ).

  ENDMETHOD.

  METHOD zif_io_itab_writer~bind_result_area.

    m_table = REF #( table ).

  ENDMETHOD.

ENDCLASS.
