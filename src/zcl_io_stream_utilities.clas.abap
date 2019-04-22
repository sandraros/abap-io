class ZCL_IO_STREAM_UTILITIES definition
  public
  create public .

public section.

  class-methods CHECK_DATA_TYPE_IS_STRING
    importing
      !DATA type ANY
    raising
      ZCX_IO_PARAMETER_INVALID_TYPE .
  class-methods CHECK_DATA_TYPE_IS_XSTRING
    importing
      !DATA type ANY
    raising
      ZCX_IO_PARAMETER_INVALID_TYPE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IO_STREAM_UTILITIES IMPLEMENTATION.


  method CHECK_DATA_TYPE_IS_STRING.

    DATA type TYPE REF TO cl_abap_typedescr.
    DATA l_name TYPE string.
    type = cl_abap_typedescr=>describe_by_data( data ).
    IF type <> cl_abap_elemdescr=>get_string( ).
      l_name = type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `READ_DATA`
          type      = l_name.
    ENDIF.
*    IF zif_io_reader~is_x_reader( ) = abap_false.
** convert binary to character
*    ENDIF.

  endmethod.


  method CHECK_DATA_TYPE_IS_XSTRING.

    DATA type TYPE REF TO cl_abap_typedescr.
    DATA l_name TYPE string.
    type = cl_abap_typedescr=>describe_by_data( data ).
    IF type <> cl_abap_elemdescr=>get_xstring( ).
      l_name = type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `READ_DATA`
          type      = l_name.
    ENDIF.
*    IF zif_io_reader~is_x_reader( ) = abap_false.
** convert binary to character
*    ENDIF.

  endmethod.
ENDCLASS.
