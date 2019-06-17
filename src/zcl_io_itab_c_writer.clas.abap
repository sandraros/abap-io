"! <p class="shorttext synchronized" lang="en">Internal table character writer</p>
"!
class ZCL_IO_ITAB_C_WRITER definition
  public
  inheriting from ZCL_IO_MEMORY_C_WRITER
  final
  create public

  global friends ZCL_IO_C_WRITER .

public section.
  type-pools ABAP .
  class CL_ABAP_TYPEDESCR definition load .

  interfaces ZIF_IO_ITAB_WRITER .

  aliases GET_MAX_LINE_LENGTH
    for ZIF_IO_ITAB_WRITER~GET_MAX_LINE_LENGTH .
  aliases bind_result_area
    for zif_io_itab_writer~bind_result_area.
  aliases GET_RESULT_TABLE
    for ZIF_IO_ITAB_WRITER~GET_RESULT_TABLE .

  methods CONSTRUCTOR
    importing
      !LINE_LENGTH type I default 255
      !LINE_TYPE type ABAP_TYPECATEGORY default CL_ABAP_TYPEDESCR=>TYPEKIND_STRING
    raising
      ZCX_IO_PARAMETER_INVALID_RANGE .

  methods ZIF_IO_MEMORY_WRITER~GET_RESULT
    redefinition .
  methods ZIF_IO_MEMORY_WRITER~GET_RESULT_TYPE
    redefinition .
protected section.

  data M_TABLE type ref to DATA .
  data M_LINE_INDEX type I .
  data M_LINE_LENGTH type I .
private section.

  data M_OFFSET type I .
  data M_LINE_TYPE type ref to CL_ABAP_ELEMDESCR .
  data M_LINE_TYPE_IS_STRING type ABAP_BOOL .

  methods WRITE_INTERNAL
    importing
      !DATA type STRING .
ENDCLASS.



CLASS ZCL_IO_ITAB_C_WRITER IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA table_type TYPE REF TO cl_abap_tabledescr.
    DATA str TYPE string.
    super->constructor( ).
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
    IF line_type = cl_abap_typedescr=>typekind_string.
      m_line_type = cl_abap_elemdescr=>get_string( ).
      m_line_type_is_string = abap_true.
    ELSE.
      m_line_type = cl_abap_elemdescr=>get_c( line_length ).
    ENDIF.
    table_type = cl_abap_tabledescr=>create( m_line_type ).
    CREATE DATA m_table TYPE HANDLE table_type.
    m_line_length = line_length.
    m_line_index = 1.

  endmethod.


  method WRITE_INTERNAL.
 "BY KERNEL MODULE ab_km_ItabCWRiteInternal.
    "TODO

  endmethod.


  method ZIF_IO_ITAB_WRITER~GET_MAX_LINE_LENGTH.

    line_length = m_line_length.

  endmethod.


  method ZIF_IO_ITAB_WRITER~GET_RESULT_TABLE.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN m_table->* TO <table>.
    ASSERT sy-subrc = 0.
    table = <table>.
    length_of_last_line = m_offset.

  endmethod.


  method ZIF_IO_MEMORY_WRITER~GET_RESULT.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN m_table->* TO <table>.
    ASSERT sy-subrc = 0.
    result = <table>.
    length_of_last_line = m_offset.

  endmethod.


  method ZIF_IO_MEMORY_WRITER~GET_RESULT_TYPE.

    result_type ?= cl_abap_typedescr=>describe_by_data_ref( m_table ).

  endmethod.

  METHOD ZIF_IO_ITAB_WRITER~BIND_RESULT_AREA.

    m_table = ref #( table ).

  ENDMETHOD.

ENDCLASS.
