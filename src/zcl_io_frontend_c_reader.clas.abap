"! <p class="shorttext synchronized" lang="en">Front-end file character reader</p>
"!
CLASS zcl_io_frontend_c_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_file_c_reader
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_frontend_reader .

    METHODS constructor
      IMPORTING
        !io_file TYPE REF TO zcl_io_frontend
      RAISING
        zcx_io_parameter_invalid .

    METHODS delete_mark
        REDEFINITION .
    METHODS is_mark_supported
        REDEFINITION .
    METHODS is_reset_supported
        REDEFINITION .
    METHODS reset
        REDEFINITION .
    METHODS reset_to_mark
        REDEFINITION .
    METHODS set_mark
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA aio_file TYPE REF TO zcl_io_frontend .
    DATA m_mark TYPE abap_msize VALUE -1 ##NO_TEXT.
    DATA m_position TYPE abap_msize VALUE 0 ##NO_TEXT.
    DATA m_str TYPE string .
    DATA aio_string_c_reader TYPE REF TO zcl_io_string_c_reader .

    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        VALUE(length) TYPE abap_msize
      RETURNING
        VALUE(result) TYPE string .
ENDCLASS.



CLASS zcl_io_frontend_c_reader IMPLEMENTATION.


  METHOD constructor.

    DATA ls_attr TYPE dset_attributes.
    DATA lx_root TYPE REF TO cx_root.
    DATA lt_buffer TYPE TABLE OF string.
    DATA l_filelength TYPE i.

    CALL METHOD super->constructor.
    aio_file = io_file.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = io_file->filename
        filetype                = 'ASC'
*       has_field_separator     = io_file->has_field_separator
*       header_length           = io_file->header_length
        read_by_line            = abap_false
*       dat_mode                = io_file->dat_mode
        codepage                = io_file->codepage
*       ignore_cerr             = io_file->ignore_cerr
*       replacement             = io_file->replacement
*       virus_scan_profile      = io_file->virus_scan_profile
      IMPORTING
        filelength              = l_filelength
*       header                  = me->header
      CHANGING
        data_tab                = lt_buffer
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_io_stream_position_error
        EXPORTING
          textid = zcx_io_stream_position_error=>zcx_io_mark_not_set.
    ENDIF.
    READ TABLE lt_buffer INDEX 1 INTO m_str.
    CREATE OBJECT aio_string_c_reader
      EXPORTING
        str = m_str.

  ENDMETHOD.


  METHOD data_available_internal.

    available = aio_string_c_reader->data_available_internal( ).

  ENDMETHOD.


  METHOD delete_mark.


  ENDMETHOD.


  METHOD is_mark_supported.

    res = is_mark_supported( ).

  ENDMETHOD.


  METHOD is_reset_supported.


  ENDMETHOD.


  METHOD read_internal.

    result = aio_string_c_reader->read_internal( length ).

  ENDMETHOD.


  METHOD reset.


  ENDMETHOD.


  METHOD reset_to_mark.


  ENDMETHOD.


  METHOD set_mark.


  ENDMETHOD.
ENDCLASS.
