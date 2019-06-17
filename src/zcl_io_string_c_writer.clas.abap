"! <p class="shorttext synchronized" lang="en">Character string writer</p>
"!
CLASS zcl_io_string_c_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_memory_c_writer
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_writer .

  PUBLIC SECTION.

    INTERFACES zif_io_string_writer .

    METHODS constructor
      IMPORTING
        !str TYPE string OPTIONAL .

    METHODS bind_result_area
      CHANGING
        str TYPE string.

    METHODS get_result_string
      RETURNING
        VALUE(str) TYPE string .

    METHODS get_result
        REDEFINITION .

    METHODS get_result_type
        REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA m_str TYPE string .
    DATA m_ref_str TYPE REF TO string .

    METHODS write_internal
      IMPORTING
        !data TYPE string .
ENDCLASS.



CLASS ZCL_IO_STRING_C_WRITER IMPLEMENTATION.


  METHOD bind_result_area.

    m_ref_str = REF #( str ).

  ENDMETHOD.


  METHOD constructor.

    CALL METHOD super->constructor.
    GET REFERENCE OF str INTO m_ref_str.

  ENDMETHOD.


  METHOD get_result.

    DATA: lo_rtti     TYPE REF TO cl_abap_typedescr,
          l_type_kind TYPE string.

    IF cl_abap_typedescr=>describe_by_data( result ) <> cl_abap_elemdescr=>get_string( ).
      lo_rtti = cl_abap_typedescr=>describe_by_data( result ).
      l_type_kind = lo_rtti->type_kind.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          parameter = `RESULT`
          type      = l_type_kind.
    ENDIF.
    result = m_str.

  ENDMETHOD.


  METHOD get_result_string.

    str = m_str.

  ENDMETHOD.


  METHOD get_result_type.

    result_type = cl_abap_elemdescr=>get_string( ).

  ENDMETHOD.


  METHOD write_internal.

    IF m_ref_str IS BOUND.
      CONCATENATE m_ref_str->* data INTO m_ref_str->*.
    ELSE.
      CONCATENATE m_str data INTO m_str.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
