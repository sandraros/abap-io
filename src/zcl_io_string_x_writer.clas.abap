"! <p class="shorttext synchronized" lang="en">Byte string writer</p>
"!
CLASS zcl_io_string_x_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_memory_x_writer
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_x_writer .

  PUBLIC SECTION.

    INTERFACES zif_io_string_writer .

    METHODS bind_result_area
      EXPORTING
        !str TYPE xstring .

    METHODS get_result_string
      RETURNING
        VALUE(str) TYPE xstring .

    METHODS zif_io_memory_writer~get_result
        REDEFINITION .

    METHODS zif_io_memory_writer~get_result_type
        REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA m_str TYPE xstring .
    DATA m_ref_str TYPE REF TO xstring .

    METHODS write_internal
      IMPORTING
        !data TYPE xstring .
ENDCLASS.



CLASS zcl_io_string_x_writer IMPLEMENTATION.


  METHOD bind_result_area.

    m_ref_str = REF #( str ).

  ENDMETHOD.


  METHOD get_result_string.

    str = m_str.

  ENDMETHOD.


  METHOD zif_io_memory_writer~get_result.

    IF m_ref_str IS BOUND.
      RAISE EXCEPTION TYPE zcx_io_stream_error.
*        EXPORTING
*          text = 'GET_RESULT cannot be used if BIND_RESULT_AREA has been used'(001).
    ENDIF.
    IF cl_abap_typedescr=>describe_by_data( result ) <> cl_abap_elemdescr=>get_xstring( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          parameter = `RESULT`
          type      = cl_abap_typedescr=>describe_by_data( result )->type_kind.
    ENDIF.
    result = m_str.

  ENDMETHOD.


  METHOD zif_io_memory_writer~get_result_type.

    result_type = cl_abap_elemdescr=>get_xstring( ).

  ENDMETHOD.


  METHOD write_internal.

    IF m_ref_str IS BOUND.
      CONCATENATE m_ref_str->* data INTO m_ref_str->* IN BYTE MODE.
    ELSE.
      CONCATENATE m_str data INTO m_str IN BYTE MODE.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
