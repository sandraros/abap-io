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

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_file TYPE REF TO zcl_io_backend .

    METHODS write_internal
      IMPORTING
        !data TYPE string .

    METHODS CLOSE_internal.

ENDCLASS.



CLASS zcl_io_backend_c_writer IMPLEMENTATION.


  METHOD constructor.

    DATA ls_attr TYPE dset_attributes.

    CALL METHOD super->constructor.
    mo_file = io_file.
    close_managed_internally = abap_true.

*    GET DATASET io_file->filename ATTRIBUTES ls_attr.

    IF abap_true = zcl_io_utilities=>is_unicode_program( sy-repid ).
      CASE io_file->attr-fixed-access_type.
        WHEN dset_appending
          OR dset_output.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_io_parameter_invalid.
      ENDCASE.
    ENDIF.

    CASE io_file->attr-fixed-mode.
      WHEN dset_text_mode
        OR dset_legacy_text_mode.
      WHEN OTHERS.
        " If the program is UNICODE, LEGACY BINARY is allowed.
        IF abap_false = zcl_io_utilities=>is_unicode_program( sy-repid )
              AND io_file->attr-fixed-mode = dset_legacy_binary_mode.
        ELSE.
          RAISE EXCEPTION TYPE zcx_io_parameter_invalid
            EXPORTING
              parameter = `IO_FILE`.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD write_internal.

    TRY.
    TRANSFER data TO mo_file->filename NO END OF LINE.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD close_internal.

    TRY.
    close dataset mo_file->filename.
    IF sy-subrc <> 0.
    " TODO
    ENDIF.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
