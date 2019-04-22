class ZCL_IO_STRING_X_WRITER definition
  public
  inheriting from ZCL_IO_MEMORY_X_WRITER
  final
  create public

  global friends ZCL_IO_X_WRITER .

public section.

  interfaces ZIF_IO_STRING_WRITER .

  methods CONSTRUCTOR
    importing
      !XSTR type XSTRING optional .
  methods GET_RESULT_STRING
    returning
      value(STR) type XSTRING .

  methods GET_RESULT
    redefinition .
  methods GET_RESULT_TYPE
    redefinition .
protected section.
private section.

  data M_STR type XSTRING .
  data M_REF_XSTR type ref to XSTRING .

  methods WRITE_INTERNAL
    importing
      !DATA type XSTRING .
ENDCLASS.



CLASS ZCL_IO_STRING_X_WRITER IMPLEMENTATION.


  method CONSTRUCTOR.

    CALL METHOD super->constructor.
    GET REFERENCE OF xstr INTO m_ref_xstr.

  endmethod.


  method GET_RESULT.

    DATA lo_rtti TYPE REF TO cl_abap_typedescr.
    DATA l_type_kind TYPE string.
    IF cl_abap_typedescr=>describe_by_data( result ) <> cl_abap_elemdescr=>get_xstring( ).
      lo_rtti = cl_abap_typedescr=>describe_by_data( result ).
      l_type_kind = lo_rtti->type_kind.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
            EXPORTING
              parameter = `RESULT`
              type = l_type_kind.
    ENDIF.
    result = m_str.

  endmethod.


  method GET_RESULT_STRING.

    str = m_str.

  endmethod.


  method GET_RESULT_TYPE.

    result_type = cl_abap_elemdescr=>get_xstring( ).

  endmethod.


  method WRITE_INTERNAL.

    IF m_ref_xstr IS BOUND.
      CONCATENATE m_ref_xstr->* data INTO m_ref_xstr->* IN BYTE MODE.
    ELSE.
      CONCATENATE m_str data INTO m_str IN BYTE MODE.
    ENDIF.

  endmethod.
ENDCLASS.
