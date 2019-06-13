"! <p class="shorttext synchronized" lang="en">Byte to Character reader (given a code page)</p>
CLASS zcl_io_filter_x2c_c_reader DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_close_resource .
    INTERFACES zif_io_c_reader .
    INTERFACES zif_io_reader .

    ALIASES close
      FOR zif_io_c_reader~close .
    ALIASES data_available
      FOR zif_io_c_reader~data_available .
    ALIASES delete_mark
      FOR zif_io_c_reader~delete_mark .
    ALIASES is_closed
      FOR zif_io_c_reader~is_closed .
    ALIASES is_mark_supported
      FOR zif_io_c_reader~is_mark_supported .
    ALIASES is_reset_supported
      FOR zif_io_c_reader~is_reset_supported .
    ALIASES is_x_reader
      FOR zif_io_c_reader~is_x_reader .
    ALIASES read
      FOR zif_io_c_reader~read .
    ALIASES reset
      FOR zif_io_reader~reset .
    ALIASES reset_to_mark
      FOR zif_io_c_reader~reset_to_mark .
    ALIASES set_mark
      FOR zif_io_c_reader~set_mark .
    ALIASES skip
      FOR zif_io_c_reader~skip .

    METHODS constructor
      IMPORTING
        !io_x_reader TYPE REF TO zif_io_x_reader
        "! code page
        !i_encoding  TYPE abap_encoding .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA aio_x_reader TYPE REF TO zif_io_x_reader .
    DATA ai_encoding TYPE abap_encoding .
ENDCLASS.



CLASS zcl_io_filter_x2c_c_reader IMPLEMENTATION.


  METHOD constructor.

    aio_x_reader = io_x_reader.
    ai_encoding  = i_encoding.

  ENDMETHOD.


  METHOD zif_io_close_resource~close.
  ENDMETHOD.


  METHOD zif_io_close_resource~is_closed.
  ENDMETHOD.


  METHOD zif_io_c_reader~read.
  ENDMETHOD.


  METHOD zif_io_reader~data_available.
  ENDMETHOD.


  METHOD zif_io_reader~delete_mark.
  ENDMETHOD.


  METHOD zif_io_reader~is_mark_supported.
  ENDMETHOD.


  METHOD zif_io_reader~is_reset_supported.
  ENDMETHOD.


  METHOD zif_io_reader~is_x_reader.
  ENDMETHOD.


  METHOD zif_io_reader~read.


  ENDMETHOD.


  METHOD zif_io_reader~reset.
  ENDMETHOD.


  METHOD zif_io_reader~reset_to_mark.
  ENDMETHOD.


  METHOD zif_io_reader~set_mark.
  ENDMETHOD.


  METHOD zif_io_reader~skip.
  ENDMETHOD.
ENDCLASS.
