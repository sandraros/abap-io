"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_io_utilities DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_data_type_is_string
      IMPORTING
        !data TYPE any
      RAISING
        zcx_io_parameter_invalid_type .
    CLASS-METHODS check_data_type_is_xstring
      IMPORTING
        !data TYPE any
      RAISING
        zcx_io_parameter_invalid_type .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_utilities IMPLEMENTATION.


  METHOD check_data_type_is_string.

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

  ENDMETHOD.


  METHOD check_data_type_is_xstring.

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

  ENDMETHOD.
ENDCLASS.
