"! <p class="shorttext synchronized" lang="en">Utilities</p>
"!
CLASS zcl_io_utilities DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .

    CLASS-METHODS copy_c_stream
      IMPORTING
        !from         TYPE REF TO zif_io_c_reader
        !to           TYPE REF TO zif_io_c_writer
        !package_size TYPE numeric DEFAULT 1000
        !close        TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_io_parameter_invalid_type
        zcx_io_resource_already_closed
        zcx_io_stream_error .

    CLASS-METHODS copy_x_stream
      IMPORTING
        !from         TYPE REF TO zif_io_x_reader
        !to           TYPE REF TO zif_io_x_writer
        !package_size TYPE numeric DEFAULT 1000
        !close        TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_io_parameter_invalid_type
        zcx_io_resource_already_closed
        zcx_io_stream_error .

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

    CLASS-METHODS is_unicode_program
      IMPORTING
        !progname     TYPE csequence
      RETURNING
        VALUE(result) TYPE abap_bool .

    CLASS-DATA:
      sys_uccheck TYPE c LENGTH 1 READ-ONLY .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_utilities IMPLEMENTATION.


  METHOD class_constructor.

    CALL 'C_SAPGPARAM'
    ID 'NAME'  FIELD 'abap/unicode_check'
    ID 'VALUE' FIELD sys_uccheck.

  ENDMETHOD.


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


  METHOD copy_c_stream.

    WHILE abap_true = from->data_available( ).
      DATA(buffer) = from->read( length = package_size ).
      to->write( buffer ).
    ENDWHILE.

    IF close = abap_true.
      from->close( ).
      to->close( ).
    ENDIF.

  ENDMETHOD.


  METHOD copy_x_stream.

    WHILE abap_true = from->data_available( ).
      DATA(buffer) = from->read( length = package_size ).
      to->write( buffer ).
    ENDWHILE.

    IF close = abap_true.
      from->close( ).
      to->close( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_unicode_program.

    DATA l_uccheck TYPE c LENGTH 1.
    IF sys_uccheck = 'on'.
      result = abap_true.
    ELSE.
      SELECT SINGLE uccheck FROM trdir INTO l_uccheck
            WHERE name = progname.
      IF l_uccheck = 'X'.
        result = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
