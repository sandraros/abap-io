"! <p class="shorttext synchronized" lang="en">Z</p>
CLASS zcl_io_x_reader DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_close_resource
      ALL METHODS FINAL .
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

    METHODS constructor .
  PROTECTED SECTION.

    DATA aov_data_available TYPE abap_bool .
    DATA aov_closed TYPE abap_bool .
    DATA:
      aov_next_byte TYPE x LENGTH 1 .
  PRIVATE SECTION.

    DATA closed TYPE abap_bool VALUE abap_false ##NO_TEXT.
ENDCLASS.



CLASS zcl_io_x_reader IMPLEMENTATION.


  METHOD constructor.

    aov_closed = abap_false.

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
    CALL METHOD me->('READ_INTERNAL')
        EXPORTING length = length
        RECEIVING result = RESULT.
  ENDMETHOD.
ENDCLASS.
