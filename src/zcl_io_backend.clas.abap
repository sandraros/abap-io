"! <p class="shorttext synchronized" lang="en">Back-end file</p>
"!
CLASS zcl_io_backend DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_file
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_backend.

    METHODS constructor
      IMPORTING
        filename TYPE string
        attr     TYPE dset_attributes
        attr2    TYPE zif_io_backend=>ty_attr2
      RAISING
        zcx_io_file_error .

    METHODS open
        REDEFINITION .

    METHODS close
        REDEFINITION .

    ALIASES get_line_end_marker FOR zif_io_backend~get_line_end_marker.
    ALIASES filename FOR zif_io_backend~filename.
    ALIASES msg      FOR zif_io_backend~msg.
    ALIASES attr     FOR zif_io_backend~attr.
    ALIASES attr2    FOR zif_io_backend~attr2.
    ALIASES cs_dset  FOR zif_io_backend~cs_dset.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: open_sy_subrc TYPE syst-subrc.

    METHODS open_for_input
      RAISING
        zcx_io_file_error.

    METHODS open_for_output
      RAISING
        zcx_io_file_error.

    METHODS open_for_appending
      RAISING
        zcx_io_file_error.

    METHODS open_for_update
      RAISING
        zcx_io_file_error.

    METHODS check_arguments
      RAISING
        zcx_io_file_error.

    METHODS complete_open
      RAISING
        cx_sy_file_access_error.

ENDCLASS.



CLASS ZCL_IO_BACKEND IMPLEMENTATION.


  METHOD check_arguments.

    CASE attr-fixed-access_type.
      WHEN dset_input
          OR dset_output
          OR dset_update
          OR dset_appending.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = 'Invalid mode "&1"'(002).
    ENDCASE.

    CASE attr-fixed-mode.
      WHEN dset_text_mode
          OR dset_binary_mode
          OR dset_legacy_text_mode
          OR dset_legacy_binary_mode.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = 'Invalid mode "&1"'(002).
    ENDCASE.

    CASE attr2-process_utf8_bom.
      WHEN abap_true
          OR abap_false.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = 'Invalid UTF-8 BOM processing code "&1"'(002).
    ENDCASE.

    " The addition "WITH BYTE-ORDER MARK" is only permitted with "FOR OUTPUT".
    IF attr2-process_utf8_bom = abap_true AND attr-fixed-access_type <> dset_output.
      RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = 'The addition "WITH BYTE-ORDER MARK" is only permitted with "FOR OUTPUT"'(001).
    ENDIF.

  ENDMETHOD.


  METHOD close.

    TRY.
        CLOSE DATASET filename.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = 'File could not be closed'(002).
        ENDIF.
      CATCH cx_sy_file_access_error INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = lx->get_text( ) previous = lx.
    ENDTRY.

  ENDMETHOD.


  METHOD complete_open.

    CLEAR attr-changeable.

    IF attr-changeable-repl_char IS NOT INITIAL.
      attr-changeable-indicator-repl_char = dset_significant.
      attr-changeable-repl_char = attr-changeable-repl_char.
    ENDIF.

    IF attr-changeable-conv_errors IS NOT INITIAL.
      attr-changeable-indicator-conv_errors = dset_significant.
      attr-changeable-conv_errors = attr-changeable-conv_errors.
    ENDIF.

    IF attr-changeable-code_page IS NOT INITIAL.
      attr-changeable-indicator-code_page = dset_significant.
      attr-changeable-code_page = attr-changeable-code_page.
    ENDIF.

    IF attr-changeable-endian IS NOT INITIAL.
      attr-changeable-indicator-endian = dset_significant.
      attr-changeable-endian = attr-changeable-endian.
    ENDIF.

    IF attr-changeable-linefeed_mode IS NOT INITIAL.
      attr-changeable-indicator-linefeed_mode = dset_significant.
      attr-changeable-linefeed_mode = attr-changeable-linefeed_mode.
    ENDIF.

    IF attr-changeable IS NOT INITIAL.
      IF attr2-indicator-position = abap_true.
        SET DATASET filename POSITION attr2-position ATTRIBUTES attr-changeable.
      ELSE.
        SET DATASET filename ATTRIBUTES attr-changeable.
      ENDIF.
    ELSEIF attr2-indicator-position = abap_true.
      SET DATASET filename POSITION attr2-position.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CALL METHOD super->constructor.
    me->filename = filename.
    me->attr = attr.
    me->attr2 = attr2.

  ENDMETHOD.


  METHOD open.

    check_arguments( ).

    CASE attr-fixed-access_type.
      WHEN dset_input.     open_for_input( ).
      WHEN dset_output.    open_for_output( ).
      WHEN dset_update.    open_for_update( ).
      WHEN dset_appending. open_for_appending( ).
    ENDCASE.

    IF open_sy_subrc <> 0.
      RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = 'File could not be opened'(002).
    ENDIF.

    complete_open( ).

  ENDMETHOD.


  METHOD open_for_appending.

    CASE attr-fixed-mode.
      WHEN dset_text_mode.
        CASE attr-fixed-encoding.
          WHEN cs_dset-encoding-default.
            IF attr-fixed-filter IS INITIAL.
              OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                    FOR APPENDING
                    TYPE attr2-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                    FOR APPENDING
                    TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
            ENDIF.
          WHEN cs_dset-encoding-non_unicode.
            IF attr-fixed-filter IS INITIAL.
              OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                    FOR APPENDING
                    TYPE attr2-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                    FOR APPENDING
                    TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
            ENDIF.
          WHEN cs_dset-encoding-utf_8.
            IF attr-fixed-filter IS INITIAL.
              OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                    FOR APPENDING
                    TYPE attr2-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                    FOR APPENDING
                    TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
            ENDIF.
        ENDCASE.
      WHEN dset_binary_mode.
        IF attr-fixed-filter IS INITIAL.
          OPEN DATASET filename IN BINARY MODE
                FOR APPENDING
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN BINARY MODE
                FOR APPENDING
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
      WHEN dset_legacy_text_mode.
        IF attr-fixed-filter IS INITIAL.
          OPEN DATASET filename IN LEGACY TEXT MODE
                FOR APPENDING
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN LEGACY TEXT MODE
                FOR APPENDING
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
      WHEN dset_legacy_binary_mode.
        IF attr-fixed-filter IS INITIAL.
          OPEN DATASET filename IN LEGACY BINARY MODE
                FOR APPENDING
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN LEGACY BINARY MODE
                FOR APPENDING
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
    ENDCASE.

    open_sy_subrc = sy-subrc.

  ENDMETHOD.


  METHOD open_for_input.

    CASE attr-fixed-mode.
      WHEN dset_text_mode.
        CASE attr-fixed-encoding.
          WHEN cs_dset-encoding-default.
            IF attr-fixed-indicator-filter = abap_false.
              OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                    FOR INPUT
                    TYPE attr2-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                    FOR INPUT
                    TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
            ENDIF.
          WHEN cs_dset-encoding-non_unicode.
            IF attr-fixed-indicator-filter = abap_false.
              OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                    FOR INPUT
                    TYPE attr2-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                    FOR INPUT
                    TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
            ENDIF.
          WHEN cs_dset-encoding-utf_8.
            CASE attr2-process_utf8_bom.
              WHEN abap_true.
                RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = 'The addition "WITH BYTE-ORDER MARK" is only permitted with "FOR OUTPUT"'(001).
              WHEN abap_false.
                IF attr-fixed-indicator-filter = abap_false.
                  OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                        FOR INPUT
                        TYPE attr2-type MESSAGE msg.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                        FOR INPUT
                        TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
                ENDIF.
            ENDCASE.
        ENDCASE.
      WHEN dset_binary_mode.
        IF attr-fixed-indicator-filter = abap_false.
          OPEN DATASET filename IN BINARY MODE
                FOR INPUT
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN BINARY MODE
                FOR INPUT
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
      WHEN dset_legacy_text_mode.
        IF attr-fixed-indicator-filter = abap_false.
          OPEN DATASET filename IN LEGACY TEXT MODE
                FOR INPUT
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN LEGACY TEXT MODE
                FOR INPUT
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
      WHEN dset_legacy_binary_mode.
        IF attr-fixed-indicator-filter = abap_false.
          OPEN DATASET filename IN LEGACY BINARY MODE
                FOR INPUT
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN LEGACY BINARY MODE
                FOR INPUT
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
    ENDCASE.

    open_sy_subrc = sy-subrc.

  ENDMETHOD.


  METHOD open_for_output.

    CASE attr-fixed-mode.
      WHEN dset_text_mode.
        CASE attr-fixed-encoding.
          WHEN cs_dset-encoding-default.
            IF attr-fixed-indicator-filter = abap_false.
              OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                FOR OUTPUT
                    TYPE attr2-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                FOR OUTPUT
                    TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
            ENDIF.
          WHEN cs_dset-encoding-non_unicode.
            IF attr-fixed-indicator-filter = abap_false.
              OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                FOR OUTPUT
                    TYPE attr2-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                FOR OUTPUT
                    TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
            ENDIF.
          WHEN cs_dset-encoding-utf_8.
            CASE attr2-process_utf8_bom.
              WHEN abap_true.
                IF attr-fixed-indicator-filter = abap_false.
                  OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                        WITH BYTE-ORDER MARK
                        FOR OUTPUT
                        TYPE attr2-type MESSAGE msg.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                        WITH BYTE-ORDER MARK
                        FOR OUTPUT
                        TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
                ENDIF.
              WHEN abap_false.
                IF attr-fixed-indicator-filter = abap_false.
                  OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                        FOR OUTPUT
                        TYPE attr2-type MESSAGE msg.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                        FOR OUTPUT
                        TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
                ENDIF.
            ENDCASE.
        ENDCASE.
      WHEN dset_binary_mode.
        IF attr-fixed-indicator-filter = abap_false.
          OPEN DATASET filename IN BINARY MODE
                FOR OUTPUT
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN BINARY MODE
                FOR OUTPUT
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
      WHEN dset_legacy_text_mode.
        IF attr-fixed-indicator-filter = abap_false.
          OPEN DATASET filename IN LEGACY TEXT MODE
                FOR OUTPUT
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN LEGACY TEXT MODE
                FOR OUTPUT
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
      WHEN dset_legacy_binary_mode.
        IF attr-fixed-indicator-filter = abap_false.
          OPEN DATASET filename IN LEGACY BINARY MODE
                FOR OUTPUT
                TYPE attr2-type MESSAGE msg.
        ELSE.
          OPEN DATASET filename IN LEGACY BINARY MODE
                FOR OUTPUT
                TYPE attr2-type FILTER attr-fixed-filter MESSAGE msg.
        ENDIF.
    ENDCASE.

    open_sy_subrc = sy-subrc.

  ENDMETHOD.


  METHOD open_for_update.

    CASE attr-fixed-mode.
      WHEN dset_text_mode.
        "================
        "  TEXT
        "================
        CASE attr-fixed-encoding.
          WHEN cs_dset-encoding-default.
            OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                  FOR UPDATE
                  TYPE attr2-type MESSAGE msg.
          WHEN cs_dset-encoding-non_unicode.
            OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                  FOR UPDATE
                  TYPE attr2-type MESSAGE msg.
          WHEN cs_dset-encoding-utf_8.
            OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                  FOR UPDATE
                  TYPE attr2-type MESSAGE msg.
        ENDCASE.
      WHEN dset_binary_mode.
        "================
        "  BINARY
        "================
        OPEN DATASET filename IN BINARY MODE
              FOR UPDATE
              TYPE attr2-type MESSAGE msg.

      WHEN dset_legacy_text_mode.
        "================
        "  LEGACY TEXT
        "================
        OPEN DATASET filename IN LEGACY TEXT MODE
              FOR UPDATE
              TYPE attr2-type MESSAGE msg.

      WHEN dset_legacy_binary_mode.
        "================
        "  LEGACY BINARY
        "================
        OPEN DATASET filename IN LEGACY BINARY MODE
              FOR UPDATE
              TYPE attr2-type MESSAGE msg.
    ENDCASE.

    open_sy_subrc = sy-subrc.

  ENDMETHOD.


  METHOD zif_io_backend~delete.

    TRY.
        DELETE DATASET filename.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = 'File could not be deleted'(002).
        ENDIF.
      CATCH cx_sy_file_access_error INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_io_file_error EXPORTING text = lx->get_text( ) previous = lx.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_io_backend~get_line_end_marker.

    IF sy-opsys NP 'Windows*'.
      result = cl_abap_char_utilities=>newline.
    ELSE.
      DATA ntfmode TYPE c.
      CALL 'C_SAPGPARAM'
            ID 'NAME'  FIELD 'abap/NTFmode'
            ID 'VALUE' FIELD ntfmode.
      IF ntfmode = 'b'.
        result = cl_abap_char_utilities=>newline.
      ELSE.
        result = cl_abap_char_utilities=>cr_lf.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
