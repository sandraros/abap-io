"! <p class="shorttext synchronized" lang="en">Back-end file character writer</p>
CLASS zcl_io_backend_c_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_file_c_writer
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_writer .

  PUBLIC SECTION.

    INTERFACES zif_io_backend_writer .

    METHODS constructor
      IMPORTING
        !io_file TYPE REF TO zcl_io_backend
      RAISING
        zcx_io_parameter_invalid .
    METHODS write_internal
      IMPORTING
        !data TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA aio_file TYPE REF TO zcl_io_backend .
ENDCLASS.



CLASS zcl_io_backend_c_writer IMPLEMENTATION.


  METHOD constructor.

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

  ENDMETHOD.


  METHOD write_internal.

    TRANSFER data TO aio_file->filename NO END OF LINE.

  ENDMETHOD.
ENDCLASS.
