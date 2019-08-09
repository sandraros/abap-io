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

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter str | <p class="shorttext synchronized" lang="en"></p>
    METHODS bind_result_area
      EXPORTING
        !str TYPE string .

    "! <p class="shorttext synchronized" lang="en">GET_RESULT_STRING</p>
    "!
    "! @parameter str | <p class="shorttext synchronized" lang="en">STR</p>
    METHODS get_result_string
      RETURNING
        VALUE(str) TYPE string .

    METHODS zif_io_memory_writer~get_result
        REDEFINITION .
    METHODS zif_io_memory_writer~get_result_type
        REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA m_str TYPE string .
    DATA m_ref_str TYPE REF TO string .

    METHODS write_internal
      IMPORTING
        !data TYPE string .
ENDCLASS.



CLASS zcl_io_string_c_writer IMPLEMENTATION.


  METHOD bind_result_area.

    m_ref_str = REF #( str ).

  ENDMETHOD.


  METHOD zif_io_memory_writer~get_result.

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


  METHOD zif_io_memory_writer~get_result_type.

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
