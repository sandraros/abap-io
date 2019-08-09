"! <p class="shorttext synchronized" lang="en">Back-end file character reader</p>
CLASS zcl_io_backend_c_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_file_c_reader
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_backend_reader .

    METHODS constructor
      IMPORTING
        !io_file TYPE REF TO zcl_io_backend
      RAISING
        zcx_io_parameter_invalid .

    METHODS zif_io_reader~delete_mark
        REDEFINITION .
    METHODS zif_io_reader~is_mark_supported
        REDEFINITION .
    METHODS zif_io_reader~is_reset_supported
        REDEFINITION .
    METHODS zif_io_reader~reset
        REDEFINITION .
    METHODS zif_io_reader~reset_to_mark
        REDEFINITION .
    METHODS zif_io_reader~set_mark
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA aio_file TYPE REF TO zcl_io_backend .
    DATA m_mark TYPE abap_msize VALUE -1.
    DATA m_position TYPE abap_msize VALUE 0.
    DATA available TYPE flag VALUE abap_true ##NO_TEXT.

    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        VALUE(length) TYPE abap_msize
      RETURNING
        VALUE(result) TYPE string .
ENDCLASS.



CLASS zcl_io_backend_c_reader IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    aio_file = io_file.

    CASE io_file->attr-fixed-access_type.
      WHEN dset_input
        OR dset_output
        OR dset_update.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_io_parameter_invalid
          EXPORTING
            parameter = `IO_FILE`.
    ENDCASE.

    " If the program is UNICODE, reading characters in LEGACY BINARY mode is not allowed
    IF abap_true = zcl_io_utilities=>is_unicode_program( sy-repid ) AND io_file->attr-fixed-mode = dset_legacy_binary_mode.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid EXPORTING parameter = `IO_FILE`.
    ENDIF.

  ENDMETHOD.


  METHOD data_available_internal.

    available = me->available.

  ENDMETHOD.


  METHOD zif_io_reader~delete_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_mark = -1.

  ENDMETHOD.


  METHOD zif_io_reader~is_mark_supported.

    res = abap_true.

  ENDMETHOD.


  METHOD zif_io_reader~is_reset_supported.

    result = abap_true.

  ENDMETHOD.


  METHOD read_internal.

    DATA: l_characters TYPE i,
          l_pos        TYPE i,
          l_length     TYPE i,
          l_string     TYPE string,
          l_length2    TYPE i,
          off          TYPE i,
          len          TYPE i,
          l_subrc      TYPE sysubrc.

    " Allocate space (performance)
    CLEAR result.
    SHIFT result RIGHT BY length PLACES.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.

    l_length = length.

    DO.

      IF l_length <= 0.
        EXIT.
      ENDIF.

      GET DATASET aio_file->filename POSITION l_pos.

      TRY.
          READ DATASET aio_file->filename INTO l_string MAXIMUM LENGTH l_length
                LENGTH l_characters.

        CATCH cx_sy_conversion_codepage INTO DATA(lx_sy_conversion_codepage).
          IF aio_file->attr-changeable-conv_errors IS INITIAL.
            RAISE EXCEPTION lx_sy_conversion_codepage.
          ENDIF.
        CATCH cx_root INTO DATA(lx_root).
          RAISE EXCEPTION lx_root.
      ENDTRY.

      l_subrc = sy-subrc.
      REPLACE SECTION OFFSET off LENGTH l_characters OF result WITH l_string IN CHARACTER MODE.
      ADD l_characters TO off.
      IF l_subrc <> 0.
        " End of file is reached
        available = abap_false.
        EXIT.
      ELSEIF off >= length.
        " Maximum number or characters is reached
        EXIT.
      ENDIF.

*      " Add character at end of line.
*      " Note: no need to use CRLF because Windows considers LF as end of line.
*      REPLACE SECTION OFFSET off LENGTH 1 OF result
*            WITH cl_abap_char_utilities=>newline IN CHARACTER MODE.
*      ADD 1 TO off.

      " Calculate number or characters left to read.
      SUBTRACT 1 FROM l_length.
      SUBTRACT l_characters FROM l_length.

    ENDDO.

    " Cut space characters initially allocated but not used.
    " (note: don't delete all trailing spaces)
    len = length - off.
    IF len > 0.
      REPLACE SECTION OFFSET off LENGTH len OF result WITH `` IN CHARACTER MODE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_io_reader~reset.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    SET DATASET aio_file->filename POSITION zcl_io_backend=>cs_dset-position-begin_of_file.
    available = abap_true.

  ENDMETHOD.


  METHOD zif_io_reader~reset_to_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF m_mark = -1.
      RAISE EXCEPTION TYPE zcx_io_stream_position_error
        EXPORTING
          textid = zcx_io_stream_position_error=>zcx_io_mark_not_set.
    ENDIF.
    SET DATASET aio_file->filename POSITION m_mark.
    available = abap_true.

  ENDMETHOD.


  METHOD zif_io_reader~set_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    GET DATASET aio_file->filename POSITION m_mark.

  ENDMETHOD.

ENDCLASS.
