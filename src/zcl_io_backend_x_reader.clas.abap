class ZCL_IO_BACKEND_X_READER definition
  public
  inheriting from ZCL_IO_FILE_X_READER
  create public

  global friends ZCL_IO_X_READER .

public section.

  interfaces ZIF_IO_BACKEND_READER .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZCL_IO_BACKEND
    raising
      ZCX_IO_PARAMETER_INVALID .

  methods DELETE_MARK
    redefinition .
  methods IS_MARK_SUPPORTED
    redefinition .
  methods IS_RESET_SUPPORTED
    redefinition .
  methods RESET
    redefinition .
  methods RESET_TO_MARK
    redefinition .
  methods SET_MARK
    redefinition .
protected section.
private section.

  data AIO_FILE type ref to ZCL_IO_BACKEND .
  data M_MARK type ABAP_MSIZE value -1 ##NO_TEXT.
  data M_POSITION type ABAP_MSIZE value 0 ##NO_TEXT.

  methods DATA_AVAILABLE_INTERNAL
    returning
      value(AVAILABLE) type ABAP_BOOL .
  methods READ_INTERNAL
    importing
      value(LENGTH) type ABAP_MSIZE
    returning
      value(RESULT) type XSTRING .
ENDCLASS.



CLASS ZCL_IO_BACKEND_X_READER IMPLEMENTATION.


  method CONSTRUCTOR.

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

  endmethod.


  method DATA_AVAILABLE_INTERNAL.

    available = abap_true. "TODO

  endmethod.


  method DELETE_MARK.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_mark = -1.

  endmethod.


  method IS_MARK_SUPPORTED.

    res = abap_true.

  endmethod.


  method IS_RESET_SUPPORTED.

    result = abap_true.

  endmethod.


  method READ_INTERNAL.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
*  get dataset aio_file->filename POSITION l_pos.
*  aio_file->CLOSE( ).
*  aio_file->open( ).
*  set dataset aio_file->filename POSITION l_pos.
    READ DATASET aio_file->filename INTO result MAXIMUM LENGTH length.

  endmethod.


  method RESET.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    SET DATASET aio_file->filename POSITION zcl_io_backend=>cs_position-begin_of_file.

  endmethod.


  method RESET_TO_MARK.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF m_mark = -1.
      RAISE EXCEPTION TYPE zcx_io_stream_position_error
        EXPORTING
          textid = zcx_io_stream_position_error=>zcx_io_mark_not_set.
    ENDIF.
    SET DATASET aio_file->filename POSITION m_mark.

  endmethod.


  method SET_MARK.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    GET DATASET aio_file->filename POSITION m_mark.

  endmethod.
ENDCLASS.
