class ZCL_IO_BACKEND definition
  public
  inheriting from ZCL_IO_FILE
  create public .

public section.
  type-pools DSET .

  constants:
    BEGIN OF cs_mode,
                  text          TYPE dset_fixed_attributes-mode VALUE dset_text_mode, "'T',
                  binary        TYPE dset_fixed_attributes-mode VALUE dset_binary_mode,
                  legacy_text   TYPE dset_fixed_attributes-mode VALUE dset_legacy_text_mode,
                  legacy_binary TYPE dset_fixed_attributes-mode VALUE dset_legacy_binary_mode,
                END OF cs_mode .
  constants:
    BEGIN OF cs_position,
                  begin_of_file TYPE i VALUE 0,
                  end_of_file   TYPE i VALUE -1,
                END OF cs_position .
  constants:
    BEGIN OF cs_access_type,
                  input     TYPE dset_fixed_attributes-access_type VALUE dset_input,
                  output    TYPE dset_fixed_attributes-access_type VALUE dset_output,
                  update    TYPE dset_fixed_attributes-access_type VALUE dset_update,
                  appending TYPE dset_fixed_attributes-access_type VALUE dset_appending,
                END OF cs_access_type .
  constants:
    BEGIN OF cs_encoding,
                  none        TYPE dset_fixed_attributes-encoding VALUE '',
                  default     TYPE dset_fixed_attributes-encoding VALUE 'DEFAULT',
                  non_unicode TYPE dset_fixed_attributes-encoding VALUE 'NON-UNICODE',
                  utf_8       TYPE dset_fixed_attributes-encoding VALUE 'UTF-8',
                END OF cs_encoding .
  data FILENAME type STRING read-only .
  data MSG type MSG read-only .
  data:
    BEGIN OF aus_attr READ-ONLY,
            mode              TYPE dset_fixed_attributes-mode,
            access_type       TYPE dset_fixed_attributes-access_type,
            encoding          TYPE dset_fixed_attributes-encoding,
            process_utf8_bom  TYPE abap_bool,
            repl_char         TYPE dset_changeable_attributes-repl_char,
            conv_errors       TYPE dset_changeable_attributes-conv_errors,
            code_page         TYPE dset_changeable_attributes-code_page,
            endian            TYPE dset_changeable_attributes-endian,
            linefeed_mode     TYPE dset_changeable_attributes-linefeed_mode,
            type              TYPE string,
            filter            TYPE dset_fixed_attributes-filter,
          END OF aus_attr .

  methods CONSTRUCTOR
    importing
      !FILENAME type STRING
      !MODE type DSET_FIXED_ATTRIBUTES-MODE default 'T'
      !ACCESS_TYPE type DSET_FIXED_ATTRIBUTES-ACCESS_TYPE default DSET_INPUT
      !ENCODING type DSET_FIXED_ATTRIBUTES-ENCODING optional
      !PROCESS_UTF8_BOM type ABAP_BOOL optional
      !REPL_CHAR type DSET_CHANGEABLE_ATTRIBUTES-REPL_CHAR optional
      !CONV_ERRORS type DSET_CHANGEABLE_ATTRIBUTES-CONV_ERRORS optional
      !CODE_PAGE type DSET_CHANGEABLE_ATTRIBUTES-CODE_PAGE optional
      !ENDIAN type DSET_CHANGEABLE_ATTRIBUTES-ENDIAN optional
      !LINEFEED_MODE type DSET_CHANGEABLE_ATTRIBUTES-LINEFEED_MODE optional
      !TYPE type CSEQUENCE optional
      !FILTER type DSET_FIXED_ATTRIBUTES-FILTER optional
    raising
      ZCX_IO_FILE_OPEN_ERROR .
  methods GET_LINE_END_MARKER
    returning
      value(RESULT) type STRING .

  methods OPEN
    redefinition .
  methods CLOSE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IO_BACKEND IMPLEMENTATION.


  method CLOSE.
"
    CLOSE DATASET filename.

  endmethod.


  method CONSTRUCTOR.

    CALL METHOD super->constructor.
    me->filename = filename.
    aus_attr-mode         = mode        .
    aus_attr-access_type  = access_type .
    aus_attr-encoding     = encoding    .
    aus_attr-process_utf8_bom = process_utf8_bom.
    aus_attr-repl_char    = repl_char   .
    aus_attr-conv_errors  = conv_errors .
    aus_attr-code_page    = code_page   .
    aus_attr-endian       = endian      .
    aus_attr-linefeed_mode = linefeed_mode.
    aus_attr-type         = type        .
    aus_attr-filter       = filter      .

  endmethod.


  method GET_LINE_END_MARKER.

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

  endmethod.


  method OPEN.

    TYPE-POOLS dset.
    DATA attr TYPE dset_attributes.

    CASE aus_attr-access_type.

      WHEN dset_input.
        CASE aus_attr-mode.
          WHEN dset_text_mode.
            CASE aus_attr-encoding.
              WHEN cs_encoding-default.
                IF aus_attr-filter IS INITIAL.
                  OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                        FOR INPUT
                        TYPE aus_attr-type MESSAGE msg.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                        FOR INPUT
                        TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                ENDIF.
              WHEN cs_encoding-non_unicode.
                IF aus_attr-filter IS INITIAL.
                  OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                        FOR INPUT
                        TYPE aus_attr-type MESSAGE msg.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                        FOR INPUT
                        TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                ENDIF.
              WHEN cs_encoding-utf_8.
                CASE aus_attr-process_utf8_bom.
                  WHEN abap_true.
                    IF aus_attr-filter IS INITIAL.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                            SKIPPING BYTE-ORDER MARK
                            FOR INPUT
                            TYPE aus_attr-type MESSAGE msg.
                    ELSE.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                            SKIPPING BYTE-ORDER MARK
                            FOR INPUT
                            TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                    ENDIF.
                  WHEN abap_false.
                    IF aus_attr-filter IS INITIAL.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                            FOR INPUT
                            TYPE aus_attr-type MESSAGE msg.
                    ELSE.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                            FOR INPUT
                            TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                    ENDIF.
                ENDCASE.
            ENDCASE.
          WHEN dset_binary_mode.
            IF aus_attr-filter IS INITIAL.
              OPEN DATASET filename IN BINARY MODE
                    FOR INPUT
                    TYPE aus_attr-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN BINARY MODE
                    FOR INPUT
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
          WHEN dset_legacy_text_mode.
            IF aus_attr-filter IS INITIAL.
              OPEN DATASET filename IN LEGACY TEXT MODE
                    FOR INPUT
                    TYPE aus_attr-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN LEGACY TEXT MODE
                    FOR INPUT
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
          WHEN dset_legacy_binary_mode.
            IF aus_attr-filter IS INITIAL.
              OPEN DATASET filename IN LEGACY BINARY MODE
                    FOR INPUT
                    TYPE aus_attr-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN LEGACY BINARY MODE
                    FOR INPUT
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
        ENDCASE.

      WHEN dset_output.
        CASE aus_attr-mode.
          WHEN dset_text_mode.
            CASE aus_attr-encoding.
              WHEN cs_encoding-default.
                IF aus_attr-filter IS INITIAL.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                    FOR OUTPUT
                        TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                ENDIF.
              WHEN cs_encoding-non_unicode.
                IF aus_attr-filter IS INITIAL.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                    FOR OUTPUT
                        TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                ENDIF.
              WHEN cs_encoding-utf_8.
                CASE aus_attr-process_utf8_bom.
                  WHEN abap_true.
                    IF aus_attr-filter IS INITIAL.
                    ELSE.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                            WITH BYTE-ORDER MARK
                            FOR OUTPUT
                            TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                    ENDIF.
                  WHEN abap_false.
                    IF aus_attr-filter IS INITIAL.
                    ELSE.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                            FOR OUTPUT
                            TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                    ENDIF.
                ENDCASE.
            ENDCASE.
          WHEN dset_binary_mode.
            IF aus_attr-filter IS INITIAL.
            ELSE.
              OPEN DATASET filename IN BINARY MODE
                    FOR OUTPUT
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
          WHEN dset_legacy_text_mode.
            IF aus_attr-filter IS INITIAL.
            ELSE.
              OPEN DATASET filename IN LEGACY TEXT MODE
                    FOR OUTPUT
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
          WHEN dset_legacy_binary_mode.
            IF aus_attr-filter IS INITIAL.
            ELSE.
              OPEN DATASET filename IN LEGACY BINARY MODE
                    FOR OUTPUT
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
        ENDCASE.

      WHEN dset_update.
        CASE aus_attr-mode.
          WHEN dset_text_mode.
            CASE aus_attr-encoding.
              WHEN cs_encoding-default.
                OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                  FOR UPDATE
                      TYPE aus_attr-type MESSAGE msg.
              WHEN cs_encoding-non_unicode.
                OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                  FOR UPDATE
                      TYPE aus_attr-type MESSAGE msg.
              WHEN cs_encoding-utf_8.
                CASE aus_attr-process_utf8_bom.
                  WHEN abap_true.
                    OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                          SKIPPING BYTE-ORDER MARK
                          FOR UPDATE
                          TYPE aus_attr-type MESSAGE msg.
                  WHEN abap_false.
                    OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                          FOR UPDATE
                          TYPE aus_attr-type MESSAGE msg.
                ENDCASE.
            ENDCASE.
          WHEN dset_binary_mode.
            OPEN DATASET filename IN BINARY MODE
                  FOR UPDATE
                  TYPE aus_attr-type MESSAGE msg.
          WHEN dset_legacy_text_mode.
            OPEN DATASET filename IN LEGACY TEXT MODE
                  FOR UPDATE
                  TYPE aus_attr-type MESSAGE msg.
          WHEN dset_legacy_binary_mode.
            OPEN DATASET filename IN LEGACY BINARY MODE
                  FOR UPDATE
                  TYPE aus_attr-type MESSAGE msg.
        ENDCASE.

      WHEN dset_appending.
        CASE aus_attr-mode.
          WHEN dset_text_mode.
            CASE aus_attr-encoding.
              WHEN cs_encoding-default.
                IF aus_attr-filter IS INITIAL.
                  OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                        FOR APPENDING
                        TYPE aus_attr-type MESSAGE msg.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING DEFAULT
                        FOR APPENDING
                        TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                ENDIF.
              WHEN cs_encoding-non_unicode.
                IF aus_attr-filter IS INITIAL.
                  OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                        FOR APPENDING
                        TYPE aus_attr-type MESSAGE msg.
                ELSE.
                  OPEN DATASET filename IN TEXT MODE ENCODING NON-UNICODE
                        FOR APPENDING
                        TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                ENDIF.
              WHEN cs_encoding-utf_8.
                CASE aus_attr-process_utf8_bom.
                  WHEN abap_true.
                    IF aus_attr-filter IS INITIAL.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                          FOR APPENDING
                          TYPE aus_attr-type MESSAGE msg.
                    ELSE.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                          FOR APPENDING
                          TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                    ENDIF.
                  WHEN abap_false.
                    IF aus_attr-filter IS INITIAL.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                            FOR APPENDING
                            TYPE aus_attr-type MESSAGE msg.
                    ELSE.
                      OPEN DATASET filename IN TEXT MODE ENCODING UTF-8
                            FOR APPENDING
                            TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
                    ENDIF.
                ENDCASE.
            ENDCASE.
          WHEN dset_binary_mode.
            IF aus_attr-filter IS INITIAL.
              OPEN DATASET filename IN BINARY MODE
                    FOR APPENDING
                    TYPE aus_attr-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN BINARY MODE
                    FOR APPENDING
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
          WHEN dset_legacy_text_mode.
            IF aus_attr-filter IS INITIAL.
              OPEN DATASET filename IN LEGACY TEXT MODE
                    FOR APPENDING
                    TYPE aus_attr-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN LEGACY TEXT MODE
                    FOR APPENDING
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
          WHEN dset_legacy_binary_mode.
            IF aus_attr-filter IS INITIAL.
              OPEN DATASET filename IN LEGACY BINARY MODE
                    FOR APPENDING
                    TYPE aus_attr-type MESSAGE msg.
            ELSE.
              OPEN DATASET filename IN LEGACY BINARY MODE
                    FOR APPENDING
                    TYPE aus_attr-type FILTER aus_attr-filter MESSAGE msg.
            ENDIF.
        ENDCASE.
    ENDCASE.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_io_file_open_error
        EXPORTING
          text = 'File could not be opened'(002).
    ENDIF.

    GET DATASET filename ATTRIBUTES attr.

    CLEAR attr-changeable.

    IF aus_attr-repl_char IS NOT INITIAL.
      attr-changeable-indicator-repl_char = dset_significant.
      attr-changeable-repl_char = aus_attr-repl_char.
    ENDIF.

    IF aus_attr-conv_errors IS NOT INITIAL.
      attr-changeable-indicator-conv_errors = dset_significant.
      attr-changeable-conv_errors = aus_attr-conv_errors.
    ENDIF.

    IF aus_attr-code_page IS NOT INITIAL.
      attr-changeable-indicator-code_page = dset_significant.
      attr-changeable-code_page = aus_attr-code_page.
    ENDIF.

    IF aus_attr-endian IS NOT INITIAL.
      attr-changeable-indicator-endian = dset_significant.
      attr-changeable-endian = aus_attr-endian.
    ENDIF.

    IF aus_attr-linefeed_mode IS NOT INITIAL.
      attr-changeable-indicator-linefeed_mode = dset_significant.
      attr-changeable-linefeed_mode = aus_attr-linefeed_mode.
    ENDIF.

    IF attr-changeable IS NOT INITIAL.
      SET DATASET filename ATTRIBUTES attr-changeable.
    ENDIF.


  endmethod.
ENDCLASS.
