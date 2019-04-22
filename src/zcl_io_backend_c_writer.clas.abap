class ZCL_IO_BACKEND_C_WRITER definition
  public
  inheriting from ZCL_IO_FILE_C_WRITER
  create public

  global friends ZCL_IO_C_WRITER .

public section.

  interfaces ZIF_IO_BACKEND_WRITER .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZCL_IO_BACKEND
    raising
      ZCX_IO_PARAMETER_INVALID .
  methods WRITE_INTERNAL
    importing
      !DATA type STRING .
protected section.
private section.

  data AIO_FILE type ref to ZCL_IO_BACKEND .
ENDCLASS.



CLASS ZCL_IO_BACKEND_C_WRITER IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA ls_attr TYPE dset_attributes.

    CALL METHOD super->constructor.
    aio_file = io_file.

    GET DATASET io_file->filename ATTRIBUTES ls_attr.

    IF abap_true = zcl_io_program=>is_unicode_program( sy-repid ).
      CASE ls_attr-fixed-access_type.
        WHEN dset_appending
          OR dset_output.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_io_parameter_invalid.
      ENDCASE.
    ENDIF.

    CASE ls_attr-fixed-mode.
      WHEN dset_text_mode
        OR dset_legacy_text_mode.
      WHEN OTHERS.
* Si le programme est non-unicode alors lire des caractères en legacy binary est permis
        IF abap_false = zcl_io_program=>is_unicode_program( sy-repid )
              AND ls_attr-fixed-mode = dset_legacy_binary_mode.
        ELSE.
          RAISE EXCEPTION TYPE zcx_io_parameter_invalid
           EXPORTING
             parameter = `IO_FILE`.
        ENDIF.
    ENDCASE.

  endmethod.


  method WRITE_INTERNAL.

    TRANSFER data TO aio_file->filename NO END OF LINE.

  endmethod.
ENDCLASS.
