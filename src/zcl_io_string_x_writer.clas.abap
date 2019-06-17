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

    METHODS constructor
      IMPORTING
        !xstr TYPE xstring OPTIONAL .
    METHODS bind_result_area
      CHANGING
        str TYPE xstring.
    METHODS get_result_string
      RETURNING
        VALUE(str) TYPE xstring .
    METHODS get_result
        REDEFINITION .
    METHODS get_result_type
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA m_str TYPE xstring .
    DATA m_ref_xstr TYPE REF TO xstring .

    METHODS write_internal
      IMPORTING
        !data TYPE xstring .
ENDCLASS.



CLASS zcl_io_string_x_writer IMPLEMENTATION.


  METHOD constructor.

    CALL METHOD super->constructor.
    GET REFERENCE OF xstr INTO m_ref_xstr.

  ENDMETHOD.


  METHOD get_result.

    DATA lo_rtti TYPE REF TO cl_abap_typedescr.
    DATA l_type_kind TYPE string.
    IF cl_abap_typedescr=>describe_by_data( result ) <> cl_abap_elemdescr=>get_xstring( ).
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

    result_type = cl_abap_elemdescr=>get_xstring( ).

  ENDMETHOD.


  METHOD write_internal.

    IF m_ref_xstr IS BOUND.
      CONCATENATE m_ref_xstr->* data INTO m_ref_xstr->* IN BYTE MODE.
    ELSE.
      CONCATENATE m_str data INTO m_str IN BYTE MODE.
    ENDIF.

  ENDMETHOD.

  METHOD bind_result_area.

    m_ref_xstr = REF #( str ).

  ENDMETHOD.

ENDCLASS.
