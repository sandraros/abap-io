"! <p class="shorttext synchronized" lang="en">Back-end file byte reader</p>
"!
CLASS zcl_io_backend_x_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_file_x_reader
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_x_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_backend_reader .

    METHODS constructor
      IMPORTING
        !io_file TYPE REF TO zcl_io_backend
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

    DATA aio_file TYPE REF TO zcl_io_backend .
    DATA m_mark TYPE abap_msize VALUE -1 ##NO_TEXT.
    DATA m_position TYPE abap_msize VALUE 0 ##NO_TEXT.

    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        VALUE(length) TYPE abap_msize
      RETURNING
        VALUE(result) TYPE xstring .
ENDCLASS.



CLASS zcl_io_backend_x_reader IMPLEMENTATION.


  METHOD constructor.

    DATA ls_attr TYPE dset_attributes.
    DATA lx_root TYPE REF TO cx_root.

    CALL METHOD super->constructor.
    aio_file = io_file.
    TRY.
        io_file->open( ).
        GET DATASET io_file->filename ATTRIBUTES ls_attr.
      CATCH cx_root INTO lx_root.
        RAISE EXCEPTION TYPE zcx_io_parameter_invalid
          EXPORTING
            previous  = lx_root
            parameter = `IO_FILE`.
    ENDTRY.
    CASE ls_attr-fixed-access_type.
      WHEN dset_input
        OR dset_output
        OR dset_update.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_io_parameter_invalid
          EXPORTING
            parameter = `IO_FILE`.
    ENDCASE.
    CASE ls_attr-fixed-mode.
      WHEN dset_binary_mode
        OR dset_legacy_binary_mode.
      WHEN OTHERS.
* Si le programme est non-unicode alors lire des octets en legacy text est permis
        IF abap_false = zcl_io_program=>is_unicode_program( sy-repid ) AND ls_attr-fixed-mode = dset_legacy_text_mode.
        ELSE.
          RAISE EXCEPTION TYPE zcx_io_parameter_invalid
            EXPORTING
              parameter = `IO_FILE`.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD data_available_internal.

    available = abap_true. "TODO

  ENDMETHOD.


  METHOD delete_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_mark = -1.

  ENDMETHOD.


  METHOD is_mark_supported.

    res = abap_true.

  ENDMETHOD.


  METHOD is_reset_supported.

    result = abap_true.

  ENDMETHOD.


  METHOD read_internal.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
*  get dataset aio_file->filename POSITION l_pos.
*  aio_file->CLOSE( ).
*  aio_file->open( ).
*  set dataset aio_file->filename POSITION l_pos.
    READ DATASET aio_file->filename INTO result MAXIMUM LENGTH length.

  ENDMETHOD.


  METHOD reset.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    SET DATASET aio_file->filename POSITION zcl_io_backend=>cs_position-begin_of_file.

  ENDMETHOD.


  METHOD reset_to_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF m_mark = -1.
      RAISE EXCEPTION TYPE zcx_io_stream_position_error
        EXPORTING
          textid = zcx_io_stream_position_error=>zcx_io_mark_not_set.
    ENDIF.
    SET DATASET aio_file->filename POSITION m_mark.

  ENDMETHOD.


  METHOD set_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    GET DATASET aio_file->filename POSITION m_mark.

  ENDMETHOD.
ENDCLASS.
