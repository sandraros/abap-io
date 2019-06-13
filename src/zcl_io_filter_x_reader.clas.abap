"! <p class="shorttext synchronized" lang="en">Z</p>
CLASS zcl_io_filter_x_reader DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_x_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_close_resource .
    INTERFACES zif_io_reader .
    INTERFACES zif_io_x_reader .

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
      FOR zif_io_x_reader~read .
    ALIASES reset
      FOR zif_io_reader~reset .
    ALIASES reset_to_mark
      FOR zif_io_reader~reset_to_mark .
    ALIASES set_mark
      FOR zif_io_reader~set_mark .
    ALIASES skip
      FOR zif_io_reader~skip .

    METHODS constructor
      IMPORTING
        !io_x_reader TYPE REF TO zif_io_x_reader .
  PROTECTED SECTION.

    DATA aoo_x_reader TYPE REF TO zif_io_x_reader .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_filter_x_reader IMPLEMENTATION.


  METHOD constructor.

    aoo_x_reader    = io_x_reader.

  ENDMETHOD.


  METHOD zif_io_close_resource~close.
  ENDMETHOD.


  METHOD zif_io_close_resource~is_closed.
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

    zcl_io_utilities=>check_data_type_is_xstring( read_data ).
    read_data = aoo_x_reader->read( length ).

  ENDMETHOD.


  METHOD zif_io_reader~reset.
  ENDMETHOD.


  METHOD zif_io_reader~reset_to_mark.
  ENDMETHOD.


  METHOD zif_io_reader~set_mark.
  ENDMETHOD.


  METHOD zif_io_reader~skip.
  ENDMETHOD.


  METHOD zif_io_x_reader~read.
  ENDMETHOD.
ENDCLASS.
