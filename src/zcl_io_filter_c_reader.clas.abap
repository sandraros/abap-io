"! <p class="shorttext synchronized" lang="en">Abstract filter character reader (delegation)</p>
CLASS zcl_io_filter_c_reader DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_c_reader.

    METHODS constructor
      IMPORTING
        reader TYPE REF TO zif_io_c_reader.

    ALIASES close
      FOR zif_io_close_resource~close .
    ALIASES data_available
      FOR zif_io_reader~data_available .
    ALIASES delete_mark
      FOR zif_io_reader~delete_mark .
    ALIASES is_closed
      FOR zif_io_close_resource~is_closed .
    ALIASES is_mark_supported
      FOR zif_io_reader~is_mark_supported .
    ALIASES is_reset_supported
      FOR zif_io_reader~is_reset_supported .
    ALIASES is_x_reader
      FOR zif_io_reader~is_x_reader .
    ALIASES read
      FOR zif_io_c_reader~read .
    ALIASES reset
      FOR zif_io_reader~reset .
    ALIASES reset_to_mark
      FOR zif_io_reader~reset_to_mark .
    ALIASES set_mark
      FOR zif_io_reader~set_mark .
    ALIASES skip
      FOR zif_io_reader~skip .

    DATA mo_reader TYPE REF TO zif_io_c_reader READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_io_filter_c_reader IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_reader = reader.
  ENDMETHOD.


  METHOD zif_io_close_resource~close.
    mo_reader->close( ).
  ENDMETHOD.


  METHOD zif_io_reader~data_available.
    available = mo_reader->data_available( ).
  ENDMETHOD.

  METHOD zif_io_reader~delete_mark.
    mo_reader->delete_mark( ).
  ENDMETHOD.

  METHOD zif_io_close_resource~is_closed.
    closed = mo_reader->is_closed( ).
  ENDMETHOD.

  METHOD zif_io_reader~is_mark_supported.
    res = mo_reader->is_mark_supported( ).
  ENDMETHOD.

  METHOD zif_io_reader~is_reset_supported.
    result = mo_reader->is_reset_supported( ).
  ENDMETHOD.

  METHOD zif_io_reader~is_x_reader.
    result = mo_reader->is_x_reader( ).
  ENDMETHOD.

  METHOD zif_io_reader~read.
    read_data = mo_reader->read( length ).
  ENDMETHOD.

  METHOD zif_io_reader~reset.
    mo_reader->reset( ).
  ENDMETHOD.

  METHOD zif_io_reader~reset_to_mark.
    mo_reader->reset_to_mark( ).
  ENDMETHOD.

  METHOD zif_io_reader~set_mark.
    mo_reader->set_mark( ).
  ENDMETHOD.

  METHOD zif_io_reader~skip.
    mo_reader->skip( length ).
  ENDMETHOD.

  METHOD zif_io_reader~is_auto_close_enabled.
    result = mo_reader->zif_io_reader~is_auto_close_enabled( ).
  ENDMETHOD.

  METHOD zif_io_c_reader~read.
    result = mo_reader->read( length ).
  ENDMETHOD.

ENDCLASS.
