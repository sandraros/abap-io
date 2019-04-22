*&---------------------------------------------------------------------*
*& Report zzsro_stream
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzsro_stream.

CLASS zcx_io_freetext DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA text TYPE string.
    METHODS constructor
      IMPORTING
        text TYPE clike
        previous TYPE REF TO cx_root OPTIONAL.
    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.
ENDCLASS.

CLASS zcx_io_freetext IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
          EXPORTING
            previous = previous.
    me->text = text.
  ENDMETHOD.
  METHOD get_text.
    DATA lo_freetext_message TYPE REF TO cl_freetext_message.
    CREATE OBJECT lo_freetext_message
      EXPORTING
        the_raw_text = text
        the_subject  = me.
    result = lo_freetext_message->get_text( ).
  ENDMETHOD.
  METHOD get_longtext.
* result = 'No details as the message is built as a "free text"'(001).
    DATA lo_t100_message TYPE REF TO cl_t100_message.
    CREATE OBJECT lo_t100_message
      EXPORTING
        the_msg_class  = 'Z1' "message which explains the free text exception concept
        the_msg_number = '001'.
    result = lo_t100_message->get_longtext( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_file_open_error DEFINITION INHERITING FROM zcx_io_freetext.
ENDCLASS.

*----------------------------------------------------------------------*
INTERFACE zif_io_file.
  METHODS open.
  METHODS close.
ENDINTERFACE.                    "if_io_file DEFINITION

*----------------------------------------------------------------------*
CLASS zcl_io_file DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES zif_io_file.
    ALIASES open  FOR zif_io_file~open.
    ALIASES close FOR zif_io_file~close.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_file IMPLEMENTATION.
  METHOD open.
  ENDMETHOD.
  METHOD close.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_frontend DEFINITION INHERITING FROM zcl_io_file.
  PUBLIC SECTION.
    METHODS constructor
          IMPORTING
            filename TYPE string
            confirm_overwrite TYPE abap_bool DEFAULT abap_true.
    METHODS open  REDEFINITION.
    METHODS close REDEFINITION.
    DATA filename TYPE string READ-ONLY.
    DATA confirm_overwrite TYPE abap_bool READ-ONLY.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_frontend IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    me->filename = filename.
    me->confirm_overwrite = confirm_overwrite.
  ENDMETHOD.
  METHOD open.
  ENDMETHOD.
  METHOD close.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_backend DEFINITION INHERITING FROM zcl_io_file.
  PUBLIC SECTION.
    TYPE-POOLS dset.
    CONSTANTS : BEGIN OF cs_mode,
                  text          TYPE dset_fixed_attributes-mode VALUE dset_text_mode, "'T',
                  binary        TYPE dset_fixed_attributes-mode VALUE dset_binary_mode,
                  legacy_text   TYPE dset_fixed_attributes-mode VALUE dset_legacy_text_mode,
                  legacy_binary TYPE dset_fixed_attributes-mode VALUE dset_legacy_binary_mode,
                END OF cs_mode.
    CONSTANTS : BEGIN OF cs_position,
                  begin_of_file TYPE i VALUE 0,
                  end_of_file   TYPE i VALUE -1,
                END OF cs_position.
    CONSTANTS : BEGIN OF cs_access_type,
                  input     TYPE dset_fixed_attributes-access_type VALUE dset_input,
                  output    TYPE dset_fixed_attributes-access_type VALUE dset_output,
                  update    TYPE dset_fixed_attributes-access_type VALUE dset_update,
                  appending TYPE dset_fixed_attributes-access_type VALUE dset_appending,
                END OF cs_access_type.
    CONSTANTS : BEGIN OF cs_encoding,
                  none        TYPE dset_fixed_attributes-encoding VALUE '',
                  default     TYPE dset_fixed_attributes-encoding VALUE 'DEFAULT',
                  non_unicode TYPE dset_fixed_attributes-encoding VALUE 'NON-UNICODE',
                  utf_8       TYPE dset_fixed_attributes-encoding VALUE 'UTF-8',
                END OF cs_encoding.

    METHODS constructor
          IMPORTING
            filename    TYPE string
            mode        TYPE dset_fixed_attributes-mode DEFAULT 'T' "dset_text_mode
            access_type TYPE dset_fixed_attributes-access_type DEFAULT dset_input
            encoding    TYPE dset_fixed_attributes-encoding OPTIONAL
            process_utf8_bom TYPE abap_bool OPTIONAL
            repl_char   TYPE dset_changeable_attributes-repl_char OPTIONAL
            conv_errors TYPE dset_changeable_attributes-conv_errors OPTIONAL
            code_page   TYPE dset_changeable_attributes-code_page OPTIONAL
            endian      TYPE dset_changeable_attributes-endian OPTIONAL
            linefeed_mode TYPE dset_changeable_attributes-linefeed_mode OPTIONAL
            type        TYPE csequence OPTIONAL
            filter      TYPE dset_fixed_attributes-filter OPTIONAL
          RAISING
            zcx_io_file_open_error.

    METHODS open  REDEFINITION.
    METHODS close REDEFINITION.
    METHODS get_line_end_marker RETURNING value(result) TYPE string.

    DATA filename TYPE string READ-ONLY.
    DATA msg      TYPE msg READ-ONLY.

    DATA: BEGIN OF aus_attr READ-ONLY,
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
          END OF aus_attr.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_backend IMPLEMENTATION.
  METHOD constructor.
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
  ENDMETHOD.
  METHOD open.
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

  ENDMETHOD.

  METHOD close."
    CLOSE DATASET filename.
  ENDMETHOD.

  METHOD get_line_end_marker.
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

*----------------------------------------------------------------------*
CLASS zcl_io_program DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-METHODS is_unicode_program
          IMPORTING
            progname TYPE csequence
          RETURNING
            value(result) TYPE abap_bool.
    CLASS-DATA sys_uccheck TYPE c LENGTH 1 READ-ONLY.
ENDCLASS.
*----------------------------------------------------------------------*
CLASS zcl_io_program IMPLEMENTATION.
  METHOD class_constructor.
    CALL 'C_SAPGPARAM'
    ID 'NAME'  FIELD 'abap/unicode_check'
    ID 'VALUE' FIELD sys_uccheck.
  ENDMETHOD.
  METHOD is_unicode_program.
    DATA l_uccheck TYPE c LENGTH 1.
    IF sys_uccheck = 'on'.
      result = abap_true.
    ELSE.
      SELECT SINGLE uccheck FROM trdir INTO l_uccheck
            WHERE name = progname.
      IF l_uccheck = 'X'.
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

* <type>
*     C
*     X
* <direction>
*     READER
*     WRITER
* <resource>
*     MEMORY
*       ITAB
*       STRING
*     FILE
*       BACKEND
*       FRONTEND
*     HTTP
*     FTP (TYPE A/TYPE I, GET/PUT)
*     DB (CLOB et BLOB) -> utilisable seulement à partir de 7.02/7.11
*
* CREATE OBJECT lo_file EXPORTING filename = '...'
* CREATE OBJECT backend EXPORTING io_file = lo_file
* CREATE OBJECT istream EXPORTING io_stream = backend.
* METHODS process_stream
*       IMPORTING
*         io_istream TYPE REF TO zif_io_reader.
* METHOD process_stream.
*   chunk = io_istream->read( 10 ).
* ENDMETHOD.
*

*----------------------------------------------------------------------*
* zcx_io_STREAM_ERROR
*----------------------------------------------------------------------*
CLASS zcx_io_stream_error DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

**----------------------------------------------------------------------*
*CLASS zcx_io_end_of_istream DEFINITION INHERITING FROM zcx_io_stream_error.
*ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_close_resource_error DEFINITION INHERITING FROM zcx_io_stream_error.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_stream_position_error DEFINITION INHERITING FROM zcx_io_stream_error.
  PUBLIC SECTION.
    CONSTANTS zcx_io_stream_position_error TYPE sotr_conc VALUE '001560AA0E0802DB8CED7F7BF73D822C'."#EC NOTEXT
    CONSTANTS zcx_io_mark_not_supported TYPE sotr_conc VALUE '001560AA0E0802DB8CED980D364E426D'."#EC NOTEXT
    CONSTANTS zcx_io_reset_not_supported TYPE sotr_conc VALUE '001560AA0E0802EB9A9AFE4CA53E01C1'."#EC NOTEXT
    CONSTANTS zcx_io_mark_not_set TYPE sotr_conc VALUE '001560AA0E0802DBB9FF94E921E8C7EB'."#EC NOTEXT
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_resource_already_closed DEFINITION INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    CONSTANTS zcx_io_resource_already_closed TYPE sotr_conc VALUE '000F206A92371DECB18030FE4A64F060'."#EC NOTEXT
    DATA resource TYPE REF TO object.
    METHODS constructor
      IMPORTING
        textid    LIKE textid OPTIONAL
        previous  LIKE previous OPTIONAL
        resource  TYPE REF TO object OPTIONAL .
ENDCLASS.

*----------------------*
CLASS zcx_io_resource_already_closed IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = zcx_io_resource_already_closed .
    ENDIF.
    me->resource = resource .
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_stream_state_error DEFINITION INHERITING FROM zcx_io_stream_error.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_parameter_invalid DEFINITION INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.

    CONSTANTS zcx_io_parameter_invalid TYPE sotr_conc VALUE '06690F3C8163FF17E10000000A11447B' .
    DATA parameter TYPE string .

    METHODS constructor
      IMPORTING
        !textid LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        value(parameter) TYPE string OPTIONAL .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_parameter_invalid IMPLEMENTATION.

  METHOD constructor .
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid .
    ENDIF.
    me->parameter = parameter .
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_parameter_invalid_range DEFINITION INHERITING FROM zcx_io_parameter_invalid.
  PUBLIC SECTION.

    CONSTANTS zcx_io_parameter_invalid_range TYPE sotr_conc VALUE '9FB7E23B75B7157FE10000000A11447B' .
    DATA value TYPE string .

    METHODS constructor
      IMPORTING
        !textid LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        value(parameter) TYPE string OPTIONAL
        value(value) TYPE string OPTIONAL .

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_parameter_invalid_range IMPLEMENTATION.

  METHOD constructor .
    CALL METHOD super->constructor
      EXPORTING
        textid    = textid
        previous  = previous
        parameter = parameter.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid_range .
    ENDIF.
    me->value = value .
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcx_io_parameter_invalid_type DEFINITION INHERITING FROM zcx_io_parameter_invalid.
  PUBLIC SECTION.
    CONSTANTS zcx_io_parameter_invalid_type TYPE sotr_conc VALUE '0017206A92371DECB18030FE4A64F060'."#EC NOTEXT
    DATA type TYPE string.
    METHODS constructor
      IMPORTING
        textid    LIKE textid OPTIONAL
        previous  LIKE previous OPTIONAL
        parameter TYPE string OPTIONAL
        type      TYPE string OPTIONAL.
ENDCLASS.

*----------------------*
CLASS zcx_io_parameter_invalid_type IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid    = textid
        previous  = previous
        parameter = parameter.
    IF textid IS INITIAL.
      me->textid = zcx_io_parameter_invalid_type .
    ENDIF.
    me->type = type .
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*
* Input and output streams (more or less same as future cl_abap_*READER
* and *WRITER°
*
*----------------------------------------------------------------------*

INTERFACE zif_io_close_resource.
  METHODS close RAISING zcx_io_close_resource_error.
  METHODS is_closed
        RETURNING value(closed) TYPE abap_bool.
ENDINTERFACE.                    "if_io_close_resource DEFINITION

*----------------------------------------------------------------------*
*
* zif_io_<direction> = if_abap_<direction>
*
*----------------------------------------------------------------------*

INTERFACE zif_io_reader.
  INTERFACES zif_io_close_resource.
  ALIASES close FOR zif_io_close_resource~close.
  ALIASES is_closed FOR zif_io_close_resource~is_closed.

  METHODS skip
    IMPORTING
      length          TYPE abap_msize
    RAISING
      zcx_io_parameter_invalid_range
      zcx_io_resource_already_closed
      zcx_io_stream_error.
  METHODS is_reset_supported "NEW
    RETURNING
      value(result) TYPE abap_bool.
  METHODS set_mark
    RAISING
      zcx_io_stream_position_error
      zcx_io_resource_already_closed .
  METHODS delete_mark
    RAISING
      zcx_io_resource_already_closed .
  METHODS is_mark_supported
    RETURNING
      value(res)  TYPE abap_bool .
  METHODS reset_to_mark
    RAISING
      zcx_io_stream_position_error
      zcx_io_resource_already_closed .
  METHODS data_available
    RETURNING
      value(available) TYPE abap_bool
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_error.
  METHODS read "NEW
    IMPORTING
      length          TYPE abap_msize
    EXPORTING
      read_data       TYPE any
    RAISING
      zcx_io_parameter_invalid_range
      zcx_io_parameter_invalid_type
      zcx_io_resource_already_closed
      zcx_io_stream_error.
  METHODS is_x_reader
    RETURNING
      value(result) TYPE abap_bool
    RAISING
      zcx_io_stream_state_error.
  METHODS reset "NEW
    RAISING
      zcx_io_resource_already_closed
      zcx_io_stream_position_error.
ENDINTERFACE.                    "if_io_reader DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_writer.
  INTERFACES zif_io_close_resource.
  ALIASES close FOR zif_io_close_resource~close.
  ALIASES is_closed FOR zif_io_close_resource~is_closed.
  METHODS write
        IMPORTING
          data TYPE any
        RAISING
          zcx_io_parameter_invalid_type
          zcx_io_resource_already_closed
          zcx_io_stream_error.
  METHODS flush
        RAISING
          zcx_io_resource_already_closed
          zcx_io_stream_error.
  METHODS is_x_writer
        RETURNING
          value(result) TYPE abap_bool.
ENDINTERFACE.                    "if_io_reader DEFINITION

*----------------------------------------------------------------------*
*
* zif_io_<resource>_<direction> = if_abap_<resource>_<direction>
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
INTERFACE zif_io_file_reader.
  INTERFACES zif_io_reader.
ENDINTERFACE.                    "if_io_file_writer DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_file_writer.
  INTERFACES zif_io_writer.
ENDINTERFACE.                    "if_io_file_writer DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_memory_writer.
  INTERFACES zif_io_writer.
  METHODS get_result
    EXPORTING
      result              TYPE any
      length_of_last_line TYPE i
    RAISING
      zcx_io_parameter_invalid_type.
  METHODS get_result_type
    EXPORTING
      result_type         TYPE REF TO cl_abap_datadescr.
ENDINTERFACE.                    "if_io_memory_writer DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_memory_reader.
  INTERFACES zif_io_reader.
ENDINTERFACE.                    "if_io_memory_reader DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_string_writer.
  INTERFACES zif_io_memory_writer.
  ALIASES get_result FOR zif_io_memory_writer~get_result.
  ALIASES get_result_type FOR zif_io_memory_writer~get_result_type.
ENDINTERFACE.                    "if_io_string_writer DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_string_reader.
  INTERFACES zif_io_memory_reader.
ENDINTERFACE.                    "if_io_string_reader IMPLEMENTATION

*----------------------------------------------------------------------*
INTERFACE zif_io_itab_writer.
  INTERFACES zif_io_memory_writer.
  ALIASES get_result FOR zif_io_memory_writer~get_result.
  ALIASES get_result_type FOR zif_io_memory_writer~get_result_type.
  METHODS get_result_table
        EXPORTING
          table TYPE STANDARD TABLE
          length_of_last_line TYPE i.
  METHODS get_max_line_length
        EXPORTING
          line_length TYPE i.
ENDINTERFACE.                    "if_io_itab_writer IMPLEMENTATION

*----------------------------------------------------------------------*
INTERFACE zif_io_itab_reader.
  INTERFACES zif_io_memory_reader.
ENDINTERFACE.                    "if_io_itab_reader IMPLEMENTATION

*----------------------------------------------------------------------*
INTERFACE zif_io_backend_reader.
  INTERFACES zif_io_file_reader.
ENDINTERFACE.                    "if_io_backend_reader DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_backend_writer.
  INTERFACES zif_io_file_writer.
ENDINTERFACE.                    "if_io_backend_writer IMPLEMENTATION

*----------------------------------------------------------------------*
INTERFACE zif_io_frontend_reader.
  INTERFACES zif_io_file_reader.
ENDINTERFACE.                    "if_io_frontend_reader DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_frontend_writer.
  INTERFACES zif_io_file_writer.
ENDINTERFACE.                    "if_io_frontend_writer DEFINITION


*----------------------------------------------------------------------*
*
* zif_io_<type>_<direction> = if_abap_<type>_<direction>
*
*----------------------------------------------------------------------*

INTERFACE zif_io_c_reader.
  INTERFACES zif_io_reader.

  ALIASES data_available  FOR zif_io_reader~data_available.
  ALIASES is_x_reader     FOR zif_io_reader~is_x_reader.
  ALIASES close           FOR zif_io_reader~close.
  ALIASES is_closed       FOR zif_io_reader~is_closed.
  ALIASES set_mark        FOR zif_io_reader~set_mark.
  ALIASES delete_mark     FOR zif_io_reader~delete_mark.
  ALIASES is_mark_supported FOR zif_io_reader~is_mark_supported.
  ALIASES is_reset_supported FOR zif_io_reader~is_reset_supported.
  ALIASES reset_to_mark   FOR zif_io_reader~reset_to_mark.
  ALIASES reset           FOR zif_io_reader~reset.
  ALIASES skip            FOR zif_io_reader~skip.

  METHODS read
        IMPORTING length TYPE numeric
        RETURNING value(result) TYPE string
        RAISING
          zcx_io_parameter_invalid_range
          zcx_io_resource_already_closed
          zcx_io_stream_error.
ENDINTERFACE.                    "if_io_c_reader DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_c_writer.
  INTERFACES zif_io_writer.
  ALIASES close FOR zif_io_writer~close.
  ALIASES is_closed FOR zif_io_writer~is_closed.
  ALIASES flush FOR zif_io_writer~flush.
  ALIASES is_x_writer FOR zif_io_writer~is_x_writer.
  METHODS write
        IMPORTING
          data TYPE string
        RAISING
          zcx_io_resource_already_closed
          zcx_io_stream_error.
ENDINTERFACE.                    "if_io_c_writer DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_x_reader.
  INTERFACES zif_io_reader.
  ALIASES data_available  FOR zif_io_reader~data_available.
  ALIASES is_x_reader     FOR zif_io_reader~is_x_reader.
  ALIASES close           FOR zif_io_reader~close.
  ALIASES is_closed       FOR zif_io_reader~is_closed.
  ALIASES set_mark        FOR zif_io_reader~set_mark.
  ALIASES delete_mark     FOR zif_io_reader~delete_mark.
  ALIASES is_mark_supported FOR zif_io_reader~is_mark_supported.
  ALIASES is_reset_supported FOR zif_io_reader~is_reset_supported.
  ALIASES reset_to_mark   FOR zif_io_reader~reset_to_mark.
  ALIASES reset           FOR zif_io_reader~reset.
  ALIASES skip            FOR zif_io_reader~skip.

  METHODS read
        IMPORTING length TYPE numeric
        RETURNING value(result) TYPE xstring
        RAISING
          zcx_io_parameter_invalid_range
          zcx_io_resource_already_closed
          zcx_io_stream_error.
ENDINTERFACE.                    "if_io_x_reader DEFINITION

*----------------------------------------------------------------------*
INTERFACE zif_io_x_writer.
  INTERFACES zif_io_writer.
  ALIASES close FOR zif_io_writer~close.
  ALIASES is_closed FOR zif_io_writer~is_closed.

  METHODS write
        IMPORTING
          data TYPE xstring
        RAISING
          zcx_io_resource_already_closed
          zcx_io_stream_error.
ENDINTERFACE.                    "if_io_x_writer DEFINITION

*----------------------------------------------------------------------*
CLASS zcl_io_stream_utilities DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS check_data_type_is_string
          IMPORTING
            data   TYPE any
          RAISING
            zcx_io_parameter_invalid_type.
    CLASS-METHODS check_data_type_is_xstring
          IMPORTING
            data   TYPE any
          RAISING
            zcx_io_parameter_invalid_type.
ENDCLASS.

*----------------------------------------------------------------------*
*
* Definitions
* of zcl_io_<type>_<direction> = cl_abap_<type>_<direction>
*
*----------------------------------------------------------------------*

CLASS zcl_io_c_reader DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES zif_io_close_resource ALL METHODS FINAL .
    ALIASES close               FOR zif_io_close_resource~close .
    ALIASES is_closed           FOR zif_io_close_resource~is_closed .

    INTERFACES zif_io_reader
        FINAL METHODS data_available
                      is_x_reader
                      read
                      skip .
    ALIASES is_reset_supported  FOR zif_io_reader~is_reset_supported .
    ALIASES reset               FOR zif_io_reader~reset .

    INTERFACES zif_io_c_reader FINAL METHODS read .
    ALIASES data_available      FOR zif_io_c_reader~data_available .
    ALIASES delete_mark         FOR zif_io_c_reader~delete_mark .
    ALIASES is_mark_supported   FOR zif_io_c_reader~is_mark_supported .
    ALIASES is_x_reader         FOR zif_io_c_reader~is_x_reader .
    ALIASES read                FOR zif_io_c_reader~read .
    ALIASES reset_to_mark       FOR zif_io_c_reader~reset_to_mark .
    ALIASES set_mark            FOR zif_io_c_reader~set_mark .
    ALIASES skip                FOR zif_io_c_reader~skip .

    METHODS constructor .

  PROTECTED SECTION.
    TYPE-POOLS abap .
    DATA ao_data_available TYPE abap_bool .

  PRIVATE SECTION.
    DATA closed TYPE abap_bool.

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_c_writer DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_io_c_writer       ALL METHODS FINAL .
    INTERFACES zif_io_close_resource FINAL METHODS is_closed. "STDCH close peut être redéfini
    INTERFACES zif_io_writer         ALL METHODS FINAL.
    ALIASES close       FOR zif_io_close_resource~close.
    ALIASES is_closed   FOR zif_io_close_resource~is_closed.
    ALIASES write       FOR zif_io_c_writer~write.
    ALIASES flush       FOR zif_io_c_writer~flush.
    ALIASES is_x_writer FOR zif_io_c_writer~is_x_writer.
    METHODS constructor.
  PRIVATE SECTION.
    DATA closed TYPE abap_bool .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_x_reader DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES zif_io_close_resource
        ALL METHODS FINAL .
    INTERFACES zif_io_reader
        FINAL METHODS data_available
                      is_x_reader
                      read
                      skip .
    INTERFACES zif_io_x_reader
        FINAL METHODS read .

    ALIASES close               FOR zif_io_close_resource~close .
    ALIASES data_available      FOR zif_io_reader~data_available .
    ALIASES delete_mark         FOR zif_io_reader~delete_mark .
    ALIASES is_closed           FOR zif_io_close_resource~is_closed .
    ALIASES is_mark_supported   FOR zif_io_reader~is_mark_supported .
    ALIASES is_reset_supported  FOR zif_io_reader~is_reset_supported .
    ALIASES is_x_reader         FOR zif_io_reader~is_x_reader .
    ALIASES read                FOR zif_io_x_reader~read .
    ALIASES reset               FOR zif_io_reader~reset .
    ALIASES reset_to_mark       FOR zif_io_reader~reset_to_mark .
    ALIASES set_mark            FOR zif_io_reader~set_mark .
    ALIASES skip                FOR zif_io_reader~skip .
    METHODS constructor.

  PROTECTED SECTION.
    DATA aov_data_available TYPE abap_bool.
    DATA aov_closed TYPE abap_bool.
    DATA aov_next_byte TYPE x LENGTH 1.

  PRIVATE SECTION.
    DATA closed TYPE abap_bool VALUE abap_false.          "#EC NOTEXT .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_x_writer DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_io_close_resource ALL METHODS FINAL .
    INTERFACES zif_io_writer         ALL METHODS FINAL .
    INTERFACES zif_io_x_writer       ALL METHODS FINAL .

    ALIASES close FOR zif_io_close_resource~close.
    ALIASES flush FOR zif_io_writer~flush .
    ALIASES is_closed FOR zif_io_close_resource~is_closed.
    ALIASES is_x_writer FOR zif_io_writer~is_x_writer .
    ALIASES write FOR zif_io_x_writer~write.
    METHODS constructor.
ENDCLASS.


*----------------------------------------------------------------------*
*
* Definitions of
* of zcl_io_<resource>_<type>_<direction> = cl_abap_<resource>_<type>_<direction>
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
CLASS zcl_io_memory_c_writer DEFINITION ABSTRACT INHERITING FROM zcl_io_c_writer.
  PUBLIC SECTION.
    INTERFACES zif_io_memory_writer
        ABSTRACT METHODS get_result
                         get_result_type .
    ALIASES get_result
      FOR zif_io_memory_writer~get_result .
    ALIASES get_result_type
      FOR zif_io_memory_writer~get_result_type .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_memory_x_writer DEFINITION
  INHERITING FROM zcl_io_x_writer
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES zif_io_memory_writer
        ABSTRACT METHODS get_result
                         get_result_type .
    ALIASES get_result
      FOR zif_io_memory_writer~get_result .
    ALIASES get_result_type
      FOR zif_io_memory_writer~get_result_type .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_memory_c_reader DEFINITION
  INHERITING FROM zcl_io_c_reader
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_io_memory_reader .

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_memory_x_reader DEFINITION
  INHERITING FROM zcl_io_x_reader
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_io_memory_reader .

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_string_c_reader DEFINITION
  INHERITING FROM zcl_io_memory_c_reader
  FINAL
  FRIENDS zcl_io_c_reader.

  PUBLIC SECTION.
    INTERFACES zif_io_string_reader .

    METHODS constructor
      IMPORTING
        str TYPE string .

    METHODS delete_mark REDEFINITION .
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

  PRIVATE SECTION.
    DATA m_str TYPE string .
    TYPE-POOLS abap .
    DATA m_offset TYPE abap_msize VALUE 0.                "#EC NOTEXT .
    DATA m_mark TYPE abap_msize VALUE -1.                 "#EC NOTEXT .

    METHODS data_available_internal                         "#EC WARNOK
      RETURNING
        value(available) TYPE abap_bool .
    METHODS read_internal                                   "#EC WARNOK
      IMPORTING
        length TYPE numeric
      RETURNING
        value(result) TYPE string .

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_string_c_writer DEFINITION FINAL
      INHERITING FROM zcl_io_memory_c_writer
      FRIENDS zcl_io_c_writer.
* en standard, la STRING est interne, et récupérée à la fin en
* appelant GET_RESULT_STRING.
* En spécifique, on offre la possibilité de construire l'instance sur
* une STRING externe.
  PUBLIC SECTION.
    INTERFACES zif_io_string_writer.
    METHODS constructor
          IMPORTING
            str TYPE string OPTIONAL. "hors standard
    METHODS get_result      REDEFINITION.
    METHODS get_result_type REDEFINITION.
    METHODS get_result_string
      RETURNING
        value(str) TYPE string .
  PRIVATE SECTION.
    DATA m_str TYPE string .
    DATA m_ref_str TYPE REF TO string . "hors standard
    METHODS write_internal                                  "#EC WARNOK
      IMPORTING
        data TYPE string .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_string_x_reader DEFINITION FINAL
      INHERITING FROM zcl_io_memory_x_reader
      FRIENDS zcl_io_x_reader.
  PUBLIC SECTION.
    INTERFACES zif_io_string_reader.
    METHODS constructor
          IMPORTING
            str TYPE string "must contain hexadecimal digits, ex: `FF0080`
            xstr TYPE xsequence OPTIONAL. "hors standard
  PRIVATE SECTION.
    DATA aiv_stream_bytes TYPE i.
    DATA aiv_offset TYPE i.
    DATA m_str TYPE xstring .
    DATA m_ref_xstr TYPE REF TO data. "hors standard
    DATA m_offset TYPE abap_msize VALUE 0.                "#EC NOTEXT .
    DATA m_mark TYPE abap_msize VALUE -1.                 "#EC NOTEXT .
    METHODS data_available_internal                         "#EC WARNOK
      RETURNING
        value(available) TYPE abap_bool .
    METHODS read_internal                                   "#EC WARNOK
      IMPORTING
        length TYPE abap_msize
      RETURNING
        value(result) TYPE xstring .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_string_x_writer DEFINITION FINAL
      INHERITING FROM zcl_io_memory_x_writer
      FRIENDS zcl_io_x_writer.
  PUBLIC SECTION.
    INTERFACES zif_io_string_writer.

    METHODS constructor
          IMPORTING
            xstr TYPE xstring OPTIONAL. "hors standard
    METHODS get_result_string
      RETURNING
        value(str) TYPE xstring .
    METHODS get_result REDEFINITION .
    METHODS get_result_type REDEFINITION .
  PRIVATE SECTION.
    DATA m_str TYPE xstring .
    DATA m_ref_xstr TYPE REF TO xstring . "hors standard

    METHODS write_internal                                  "#EC WARNOK
      IMPORTING
        !data TYPE xstring .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_itab_c_reader DEFINITION
  INHERITING FROM zcl_io_memory_c_reader
  FRIENDS zcl_io_c_reader.

  PUBLIC SECTION.

    INTERFACES zif_io_itab_reader .

    METHODS constructor
      IMPORTING
        itab TYPE STANDARD TABLE
      RAISING
        zcx_io_parameter_invalid_type .

    METHODS delete_mark        REDEFINITION .
    METHODS is_mark_supported  REDEFINITION .
    METHODS is_reset_supported REDEFINITION .
    METHODS reset              REDEFINITION .
    METHODS reset_to_mark      REDEFINITION .
    METHODS set_mark           REDEFINITION .

  PRIVATE SECTION.

    DATA m_itab TYPE REF TO data .
    DATA m_line_index TYPE i VALUE 1.                     "#EC NOTEXT .
    DATA m_position TYPE i VALUE 0.                       "#EC NOTEXT .
    DATA m_line_index_mark TYPE i VALUE 0.                "#EC NOTEXT .
    DATA m_pos_mark TYPE i VALUE -1.                      "#EC NOTEXT .
    DATA m_line_type TYPE REF TO cl_abap_datadescr .
    TYPE-POOLS abap .
    DATA m_data_available TYPE abap_bool VALUE abap_true. "#EC NOTEXT .
    DATA m_line_type_is_string TYPE abap_bool .
    DATA m_line_length TYPE i .

    METHODS data_available_internal
      RETURNING
        value(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        value(length) TYPE abap_msize
      RETURNING
        value(result) TYPE string .
    METHODS find_first .

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_itab_c_writer DEFINITION
  INHERITING FROM zcl_io_memory_c_writer
  FINAL
  FRIENDS zcl_io_c_writer.

  PUBLIC SECTION.

    INTERFACES zif_io_itab_writer .

    ALIASES get_max_line_length
      FOR zif_io_itab_writer~get_max_line_length .
    ALIASES get_result_table
      FOR zif_io_itab_writer~get_result_table .

    TYPE-POOLS abap .
    CLASS cl_abap_typedescr DEFINITION LOAD .
    METHODS constructor
      IMPORTING
        !line_length TYPE i DEFAULT 255
        !line_type TYPE abap_typecategory DEFAULT cl_abap_typedescr=>typekind_string
      RAISING
        zcx_io_parameter_invalid_range .

    METHODS zif_io_memory_writer~get_result
      REDEFINITION .
    METHODS zif_io_memory_writer~get_result_type
      REDEFINITION .

  PROTECTED SECTION.

    DATA m_table TYPE REF TO data .
    DATA m_line_index TYPE i .
    DATA m_line_length TYPE i .

  PRIVATE SECTION.

    DATA m_offset TYPE i .
    DATA m_line_type TYPE REF TO cl_abap_elemdescr .
    TYPE-POOLS abap .
    DATA m_line_type_is_string TYPE abap_bool .

    METHODS write_internal
      IMPORTING
        data TYPE string .

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_file_c_reader DEFINITION ABSTRACT INHERITING FROM zcl_io_c_reader.
  PUBLIC SECTION.
    INTERFACES zif_io_file_reader.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_file_c_writer DEFINITION ABSTRACT INHERITING FROM zcl_io_c_writer.
  PUBLIC SECTION.
    INTERFACES zif_io_file_writer.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_file_x_reader DEFINITION ABSTRACT INHERITING FROM zcl_io_x_reader.
  PUBLIC SECTION.
    INTERFACES zif_io_file_reader.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_file_x_writer DEFINITION ABSTRACT INHERITING FROM zcl_io_x_writer.
  PUBLIC SECTION.
    INTERFACES zif_io_file_writer.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_backend_c_reader DEFINITION
      INHERITING FROM zcl_io_file_c_reader
      FRIENDS zcl_io_c_reader.

  PUBLIC SECTION.
    INTERFACES zif_io_backend_reader.
    METHODS constructor
          IMPORTING
            io_file TYPE REF TO zcl_io_backend
          RAISING
            zcx_io_parameter_invalid.

    METHODS delete_mark        REDEFINITION .
    METHODS is_mark_supported  REDEFINITION .
    METHODS is_reset_supported REDEFINITION .
    METHODS reset              REDEFINITION .
    METHODS reset_to_mark      REDEFINITION .
    METHODS set_mark           REDEFINITION .
*    METHODS close              REDEFINITION .

  PRIVATE SECTION.
    DATA aio_file TYPE REF TO zcl_io_backend.
    DATA m_mark TYPE abap_msize VALUE -1.
    DATA m_position TYPE abap_msize VALUE 0.
    DATA available TYPE flag VALUE abap_true.

    METHODS data_available_internal
      RETURNING
        value(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        value(length) TYPE abap_msize
      RETURNING
        value(result) TYPE string .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_backend_x_reader DEFINITION
      INHERITING FROM zcl_io_file_x_reader
      FRIENDS zcl_io_x_reader.

  PUBLIC SECTION.
    INTERFACES zif_io_backend_reader.
    METHODS constructor
          IMPORTING
            io_file TYPE REF TO zcl_io_backend
          RAISING
            zcx_io_parameter_invalid.

    METHODS delete_mark        REDEFINITION .
    METHODS is_mark_supported  REDEFINITION .
    METHODS is_reset_supported REDEFINITION .
    METHODS reset              REDEFINITION .
    METHODS reset_to_mark      REDEFINITION .
    METHODS set_mark           REDEFINITION .

  PRIVATE SECTION.
    DATA aio_file TYPE REF TO zcl_io_backend.
    DATA m_mark TYPE abap_msize VALUE -1.
    DATA m_position TYPE abap_msize VALUE 0.

    METHODS data_available_internal
      RETURNING
        value(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        value(length) TYPE abap_msize
      RETURNING
        value(result) TYPE xstring .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_backend_c_writer DEFINITION INHERITING FROM zcl_io_file_c_writer
      FRIENDS zcl_io_c_writer.
  PUBLIC SECTION.
    INTERFACES zif_io_backend_writer.
    METHODS constructor
          IMPORTING
            io_file TYPE REF TO zcl_io_backend
          RAISING
            zcx_io_parameter_invalid.
    METHODS write_internal                                  "#EC WARNOK
      IMPORTING
        data TYPE string .
  PRIVATE SECTION.
    DATA aio_file TYPE REF TO zcl_io_backend.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_frontend_c_reader DEFINITION INHERITING FROM zcl_io_file_c_reader
      FRIENDS zcl_io_c_reader.

  PUBLIC SECTION.
    INTERFACES zif_io_frontend_reader.
    METHODS constructor
          IMPORTING
            io_file TYPE REF TO zcl_io_frontend
          RAISING
            zcx_io_parameter_invalid.

    METHODS delete_mark        REDEFINITION .
    METHODS is_mark_supported  REDEFINITION .
    METHODS is_reset_supported REDEFINITION .
    METHODS reset              REDEFINITION .
    METHODS reset_to_mark      REDEFINITION .
    METHODS set_mark           REDEFINITION .

  PRIVATE SECTION.
    DATA aio_file TYPE REF TO zcl_io_frontend.
    DATA m_mark TYPE abap_msize VALUE -1.
    DATA m_position TYPE abap_msize VALUE 0.
    DATA m_str TYPE string.
    DATA aio_string_c_reader TYPE REF TO zcl_io_string_c_reader.

    METHODS data_available_internal
      RETURNING
        value(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        value(length) TYPE abap_msize
      RETURNING
        value(result) TYPE string .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_frontend_c_writer DEFINITION INHERITING FROM zcl_io_file_c_writer
      FRIENDS zcl_io_c_writer.
  PUBLIC SECTION.
    INTERFACES zif_io_frontend_writer.
    METHODS constructor
          IMPORTING
            io_file       TYPE REF TO zcl_io_frontend
            i_buffer_size TYPE i DEFAULT 0
          RAISING
            zcx_io_parameter_invalid.
  PRIVATE SECTION.
    DATA aio_file TYPE REF TO zcl_io_frontend.
    DATA ai_buffer_size TYPE i.
    DATA m_str TYPE string.
    METHODS close_internal.
    METHODS write_internal                                  "#EC WARNOK
      IMPORTING
        data TYPE string .
ENDCLASS.

*----------------------------------------------------------------------*
*
*  DEFINITIONS
*  of zcl_io_FILTER_<type>_<direction> = cl_abap_FILTER_<type>_<direction>
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* XSTREAM -> UNGZIP -> {CLASSE} -> XSTREAM
*----------------------------------
*    CREATE OBJECT lo_gzip_x_reader
*      EXPORTING
*        io_x_reader     = lo_frontend_x_reader
*        i_gzip_buffer   = 500
*        i_stream_buffer = 1000.
*    bytes = lo_gzip_x_reader->read( 100 ). "lire 100 octets décompressés
* READ_INTERNAL:
*   1) Tant que le buffer ne contient pas encore 100 octets, et que
*      abap_true = io_x_reader->is_available( ), lire 1000 octets de
*      IO_X_READER, puis décompresser (chaque décompression peut ou non
*      alimenter le buffer, selon que la décompression a produit un certain
*      quota d'octets)
*   2) Retourner 100 octets du buffer (ou tout le buffer s'il fait moins
*      de 100 octets), et enlever ces octets du buffer
* USE_OUT_BUF:
*   - est appelé par SAP lorsque, après décompression, on obtient
*     au moins 500 octets, ou que la fin est atteinte.
*   - Ces octets sont rajoutés au buffer.
CLASS zcl_io_filter_gzip_x_reader DEFINITION
      FRIENDS zcl_io_x_reader.
  PUBLIC SECTION.
    INTERFACES zif_io_x_reader.
    INTERFACES if_abap_ungzip_binary_handler.
    ALIASES close               FOR zif_io_close_resource~close .
    ALIASES data_available      FOR zif_io_reader~data_available .
    ALIASES delete_mark         FOR zif_io_reader~delete_mark .
    ALIASES is_closed           FOR zif_io_close_resource~is_closed .
    ALIASES is_mark_supported   FOR zif_io_reader~is_mark_supported .
    ALIASES is_reset_supported  FOR zif_io_reader~is_reset_supported .
    ALIASES is_x_reader         FOR zif_io_reader~is_x_reader .
    ALIASES read                FOR zif_io_x_reader~read .
    ALIASES reset               FOR zif_io_reader~reset .
    ALIASES reset_to_mark       FOR zif_io_reader~reset_to_mark .
    ALIASES set_mark            FOR zif_io_reader~set_mark .
    ALIASES skip                FOR zif_io_reader~skip .
    METHODS constructor
          IMPORTING
            io_x_reader     TYPE REF TO zif_io_x_reader
            i_gzip_buffer   TYPE i OPTIONAL
            i_stream_buffer TYPE i OPTIONAL
          RAISING
            zcx_io_parameter_invalid_range.

  PRIVATE SECTION.
    TYPES : BEGIN OF ty_is_ungzip_stream,
              o_ungzip_stream TYPE REF TO cl_abap_ungzip_binary_stream,
              o_x_reader      TYPE REF TO zif_io_x_reader,
            END OF ty_is_ungzip_stream.
    CLASS-DATA kit_ungzip_stream TYPE HASHED TABLE OF ty_is_ungzip_stream
          WITH UNIQUE KEY o_ungzip_stream.
    DATA aio_ungzip_bin TYPE REF TO cl_abap_ungzip_binary_stream.
    DATA ai_gzip_buffer TYPE i.
    DATA ai_stream_buffer TYPE i.
    DATA ai_buffer TYPE xstring.
    DATA aio_x_reader TYPE REF TO zif_io_x_reader.
    METHODS read_internal                                   "#EC WARNOK
      IMPORTING
        length TYPE numeric
      RETURNING
        value(result) TYPE xstring .
ENDCLASS.

*----------------------------------------------------------------------*
*    CREATE OBJECT lo_string_x_writer
*      EXPORTING
*        str = l_xstring.
*    CREATE OBJECT lo_gzip_x_writer
*      EXPORTING
*        io_x_writer = lo_string_x_writer
*        i_buffer_size = 1000.
*    lo_gzip_x_writer->write( '55FF00AA' ).
* WRITE fait une compression, et ça se stocke dans le buffer, qui,
* lorsqu'il est plein (1000 octets compressés), est envoyé à io_x_writer
*    ASSERT l_xstring = ''. "valeur compressée
*----------------------------------
CLASS zcl_io_filter_gzip_x_writer DEFINITION
      FRIENDS zcl_io_x_writer.
  PUBLIC SECTION.
    INTERFACES zif_io_x_writer.
    INTERFACES if_abap_gzip_binary_handler.
    ALIASES close       FOR zif_io_close_resource~close.
    ALIASES flush       FOR zif_io_writer~flush .
    ALIASES is_closed   FOR zif_io_close_resource~is_closed.
    ALIASES is_x_writer FOR zif_io_writer~is_x_writer .
    ALIASES write       FOR zif_io_x_writer~write.
    METHODS constructor
          IMPORTING
            io_x_writer     TYPE REF TO zif_io_x_writer
            compress_level  TYPE i DEFAULT 6
            i_buffer_size   TYPE i OPTIONAL
          RAISING
            zcx_io_parameter_invalid_range.

  PRIVATE SECTION.
    TYPES : BEGIN OF ty_is_gzip_stream,
              o_gzip_stream TYPE REF TO cl_abap_gzip_binary_stream,
              o_x_writer    TYPE REF TO zif_io_x_writer,
            END OF ty_is_gzip_stream.
    CLASS-DATA kit_gzip_stream TYPE HASHED TABLE OF ty_is_gzip_stream
          WITH UNIQUE KEY o_gzip_stream.
    DATA aio_gzip_bin TYPE REF TO cl_abap_gzip_binary_stream.
    DATA ai_buffer_size TYPE i.
    DATA ai_buffer TYPE xstring.
    METHODS write_internal                                  "#EC WARNOK
      IMPORTING
        data TYPE xsequence.
ENDCLASS.

*----------------------------------------------------------------------*
*    CREATE OBJECT lo_gzip_c_reader
*      EXPORTING
*        io_x_reader = lo_frontend_x_reader
*        i_buffer_size = 1000.
*    text = lo_gzip_c_reader->read( 100 ). "lire 100 caractères décompressés
*----------------------------------
CLASS zcl_io_filter_gzip_c_reader DEFINITION
      FRIENDS zcl_io_c_reader.
  PUBLIC SECTION.
    INTERFACES zif_io_c_reader.
    INTERFACES if_abap_ungzip_text_handler.
    ALIASES close               FOR zif_io_close_resource~close .
    ALIASES data_available      FOR zif_io_reader~data_available .
    ALIASES delete_mark         FOR zif_io_reader~delete_mark .
    ALIASES is_closed           FOR zif_io_close_resource~is_closed .
    ALIASES is_mark_supported   FOR zif_io_reader~is_mark_supported .
    ALIASES is_reset_supported  FOR zif_io_reader~is_reset_supported .
    ALIASES is_x_reader         FOR zif_io_reader~is_x_reader .
    ALIASES read                FOR zif_io_c_reader~read .
    ALIASES reset               FOR zif_io_reader~reset .
    ALIASES reset_to_mark       FOR zif_io_reader~reset_to_mark .
    ALIASES set_mark            FOR zif_io_reader~set_mark .
    ALIASES skip                FOR zif_io_reader~skip .
    METHODS constructor
          IMPORTING
            io_x_reader     TYPE REF TO zif_io_x_reader
            i_gzip_buffer   TYPE i OPTIONAL
            i_stream_buffer TYPE i OPTIONAL
          RAISING
            zcx_io_parameter_invalid_range.

  PRIVATE SECTION.
    TYPES : BEGIN OF ty_is_ungzip_stream,
              o_ungzip_stream TYPE REF TO cl_abap_ungzip_text_stream,
              o_c_reader      TYPE REF TO zif_io_c_reader,
            END OF ty_is_ungzip_stream.
    CLASS-DATA kit_ungzip_stream TYPE HASHED TABLE OF ty_is_ungzip_stream
          WITH UNIQUE KEY o_ungzip_stream.
    DATA aio_ungzip_text TYPE REF TO cl_abap_ungzip_text_stream.
    DATA ai_gzip_buffer TYPE i.
    DATA ai_stream_buffer TYPE i.
    DATA ai_buffer TYPE string.
    DATA aio_x_reader TYPE REF TO zif_io_x_reader.
    METHODS read_internal                                   "#EC WARNOK
      IMPORTING
        length TYPE numeric
      RETURNING
        value(result) TYPE string .
ENDCLASS.

*----------------------------------------------------------------------*
*    CREATE OBJECT lo_gzip_c_writer
*      EXPORTING
*        io_x_writer   = lo_string_x_writer
*        i_gzip_buffer = 1000.
*    lo_gzip_c_writer->write( 'WWW' ).
* WRITE:
*   1) compresse les caractères
*   2) lorsque la taille totale des compressions atteint 1000 octets,
*      ou que la fin du stream est déclarée (CLOSE), alors
*      SAP appelle USE_OUT_BUF.
* USE_OUT_BUF:
*   1) le buffer est envoyé à IO_X_WRITER
*----------------------------------
* C -> C_WRITER -> GZIP -> X_WRITER -> X
*----------------------------------
CLASS zcl_io_filter_gzip_c_writer DEFINITION
      FRIENDS zcl_io_c_writer.
  PUBLIC SECTION.
    INTERFACES zif_io_c_writer.
    INTERFACES if_abap_gzip_text_handler.
    ALIASES close       FOR zif_io_close_resource~close.
    ALIASES flush       FOR zif_io_writer~flush .
    ALIASES is_closed   FOR zif_io_close_resource~is_closed.
    ALIASES is_x_writer FOR zif_io_writer~is_x_writer .
    ALIASES write       FOR zif_io_c_writer~write.
    METHODS constructor
          IMPORTING
            io_x_writer     TYPE REF TO zif_io_x_writer
            compress_level  TYPE i DEFAULT 6
            i_buffer_size   TYPE i OPTIONAL
          RAISING
            zcx_io_parameter_invalid_range.

  PRIVATE SECTION.
    TYPES : BEGIN OF ty_is_gzip_stream,
              o_gzip_stream TYPE REF TO cl_abap_gzip_text_stream,
              o_c_writer    TYPE REF TO zcl_io_filter_gzip_c_writer,
            END OF ty_is_gzip_stream.
    CLASS-DATA kit_gzip_stream TYPE HASHED TABLE OF ty_is_gzip_stream
          WITH UNIQUE KEY o_gzip_stream.
    DATA aio_gzip_text TYPE REF TO cl_abap_gzip_text_stream.
    DATA ai_buffer_size TYPE i.
    DATA ai_buffer TYPE xstring.
    DATA aio_x_writer TYPE REF TO zif_io_x_writer.
    METHODS write_internal                                  "#EC WARNOK
      IMPORTING
        data TYPE clike.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_filter_x_reader DEFINITION ABSTRACT FRIENDS zcl_io_x_reader.
  PUBLIC SECTION.
    INTERFACES zif_io_x_reader.
    ALIASES close               FOR zif_io_close_resource~close .
    ALIASES data_available      FOR zif_io_reader~data_available .
    ALIASES delete_mark         FOR zif_io_reader~delete_mark .
    ALIASES is_closed           FOR zif_io_close_resource~is_closed .
    ALIASES is_mark_supported   FOR zif_io_reader~is_mark_supported .
    ALIASES is_reset_supported  FOR zif_io_reader~is_reset_supported .
    ALIASES is_x_reader         FOR zif_io_reader~is_x_reader .
    ALIASES read                FOR zif_io_x_reader~read .
    ALIASES reset               FOR zif_io_reader~reset .
    ALIASES reset_to_mark       FOR zif_io_reader~reset_to_mark .
    ALIASES set_mark            FOR zif_io_reader~set_mark .
    ALIASES skip                FOR zif_io_reader~skip .
    METHODS constructor
          IMPORTING
            io_x_reader     TYPE REF TO zif_io_x_reader.
*            i_buffer_size   TYPE i DEFAULT 10000
*          RAISING
*            zcx_io_parameter_invalid_range.

  PROTECTED SECTION.
*    DATA ai_buffer_size TYPE i.
    DATA aoo_x_reader TYPE REF TO zif_io_x_reader.
*    DATA ai_data_available TYPE abap_bool VALUE abap_true."#EC NOTEXT .
*    DATA m_mark TYPE i VALUE -1.

*    METHODS read_internal                                   "#EC WARNOK
*      IMPORTING
*        length TYPE numeric
*      RETURNING
*        value(result) TYPE xstring .
ENDCLASS.

*----------------------------------------------------------------------*
* zcl_io_FILTER_BUFF_X_READER
*--------------------------
* Lit le stream en entrée par blocs de 10000 octets, les bufférise, et
* les retransmet au fil de l'eau.
*** Exemple: lire un fichier XML et retourner un tableau de combinaisons
*** { XPath ; position exacte dans le fichier }; la complexité est la
*** présence des caractères de saut de ligne -> impossible d'ouvrir , car c'est
*** le kernel qui ???
**  CREATE OBJECT lo_backend_x_reader
**        EXPORTING
**          io_file = lo_file.
**  CREATE OBJECT lo_filter_buff_x_reader
**        EXPORTING
**          io_x_reader = lo_backend_x_reader
**          i_buffer_size = 100000.
**  CREATE OBJECT lo_filter_x2c_c_reader
**        EXPORTING
**          io_x_reader = lo_filter_buff_x_reader.
*----------------------------------------------------------------------*
CLASS zcl_io_filter_buff_x_reader DEFINITION INHERITING FROM zcl_io_filter_x_reader.
  PUBLIC SECTION.
    METHODS constructor
          IMPORTING
            io_x_reader     TYPE REF TO zif_io_x_reader
            i_buffer_size   TYPE i DEFAULT 10000
          RAISING
            zcx_io_parameter_invalid_range.
    DATA buffer TYPE xstring READ-ONLY.
    DATA offset TYPE i READ-ONLY.
*    INTERFACES zif_io_x_reader.
*    ALIASES close               FOR zif_io_close_resource~close .
*    ALIASES data_available      FOR zif_io_reader~data_available .
*    ALIASES delete_mark         FOR zif_io_reader~delete_mark .
*    ALIASES is_closed           FOR zif_io_close_resource~is_closed .
*    ALIASES is_mark_supported   FOR zif_io_reader~is_mark_supported .
*    ALIASES is_reset_supported  FOR zif_io_reader~is_reset_supported .
*    ALIASES is_x_reader         FOR zif_io_reader~is_x_reader .
*    ALIASES read                FOR zif_io_x_reader~read .
*    ALIASES reset               FOR zif_io_reader~reset .
*    ALIASES reset_to_mark       FOR zif_io_reader~reset_to_mark .
*    ALIASES set_mark            FOR zif_io_reader~set_mark .
*    ALIASES skip                FOR zif_io_reader~skip .
*    METHODS read                REDEFINITION.
*    METHODS close               REDEFINITION.
*    METHODS delete_mark         REDEFINITION.
*    METHODS is_mark_supported   REDEFINITION.
*    METHODS is_reset_supported  REDEFINITION.
*    METHODS reset               REDEFINITION.
*    METHODS reset_to_mark       REDEFINITION.
*    METHODS set_mark            REDEFINITION.

  PRIVATE SECTION.
    DATA ai_buffer_size TYPE i.
*    DATA aio_x_reader TYPE REF TO zif_io_x_reader.
    DATA ai_data_available TYPE abap_bool VALUE abap_true."#EC NOTEXT .
    DATA m_mark TYPE i VALUE -1.

*    METHODS data_available_internal                         "#EC WARNOK
*      RETURNING
*        value(available) TYPE abap_bool .
    METHODS read_internal                                   "#EC WARNOK
      IMPORTING
        length TYPE abap_msize
      RETURNING
        value(result) TYPE xstring .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_misc_xc_reader DEFINITION
      FRIENDS zcl_io_x_reader.
  PUBLIC SECTION.
    INTERFACES zif_io_x_reader.
    TYPES ty_u_utf_bom TYPE i.
    CONSTANTS : BEGIN OF utf_bom,
                  not_found TYPE ty_u_utf_bom VALUE 1,
                  utf_8     TYPE ty_u_utf_bom VALUE 2,
                  utf_16_le TYPE ty_u_utf_bom VALUE 3,
                  utf_16_be TYPE ty_u_utf_bom VALUE 4,
                END OF utf_bom.

    METHODS constructor
          IMPORTING
            io_x_reader     TYPE REF TO zif_io_x_reader
            i_encoding      TYPE cpcodepage OPTIONAL "abap_encoding
            i_utf_bom       TYPE abap_bool DEFAULT abap_true
            i_repl_char     TYPE char1     DEFAULT '#'
            i_linefeed_mode TYPE dset_changeable_attributes-linefeed_mode OPTIONAL.
    METHODS set_utf_bom
          IMPORTING
            i_utf_bom  TYPE abap_bool.
    METHODS set_encoding
          IMPORTING
            i_encoding  TYPE cpcodepage. "abap_encoding
    METHODS set_repl_char
          IMPORTING
            i_repl_char  TYPE char1.
    METHODS set_linefeed_mode
          IMPORTING
            i_linefeed_mode TYPE dset_changeable_attributes-linefeed_mode.
    METHODS check_bom.
    METHODS get_byte_offset
          IMPORTING
            i_string    TYPE csequence
            i_offset    TYPE i
            i_codepage  TYPE cpcodepage
          RETURNING
            value(result) TYPE i.

    METHODS read_chars
          IMPORTING length TYPE numeric
          RETURNING value(result) TYPE string
          RAISING
            zcx_io_parameter_invalid_range
            zcx_io_resource_already_closed
            zcx_io_stream_error.
    ALIASES read_bytes          FOR zif_io_x_reader~read .

    EVENTS utf_bom_information
          EXPORTING
            value(utf_bom) TYPE ty_u_utf_bom.

    ALIASES close               FOR zif_io_x_reader~close .
    ALIASES data_available      FOR zif_io_x_reader~data_available .
    ALIASES delete_mark         FOR zif_io_x_reader~delete_mark .
    ALIASES is_closed           FOR zif_io_x_reader~is_closed .
    ALIASES is_mark_supported   FOR zif_io_x_reader~is_mark_supported .
    ALIASES is_reset_supported  FOR zif_io_x_reader~is_reset_supported .
    ALIASES is_x_reader         FOR zif_io_x_reader~is_x_reader .
    ALIASES reset               FOR zif_io_x_reader~reset .
    ALIASES reset_to_mark       FOR zif_io_x_reader~reset_to_mark .
    ALIASES set_mark            FOR zif_io_x_reader~set_mark .
    ALIASES skip                FOR zif_io_x_reader~skip .

  PRIVATE SECTION.
    DATA aio_x_reader     TYPE REF TO zcl_io_filter_buff_x_reader.
    DATA ai_codepage      TYPE cpcodepage.
    DATA ai_offset        TYPE i.
    DATA ai_buffer_c      TYPE string.
    DATA ai_buffer_size   TYPE i VALUE 10000.
    DATA aio_conv_obj     TYPE REF TO cl_abap_conv_obj.    "octets -> caractères
    DATA aio_conv_out     TYPE REF TO cl_abap_conv_out_ce. "caractères -> code points
    DATA ai_utf_bom       TYPE abap_bool.
    DATA ai_repl_char     TYPE char1.
    DATA ai_linefeed_mode TYPE dset_changeable_attributes-linefeed_mode.
*    DATA ai_encoding  TYPE abap_encoding.
*            i_encoding      TYPE abap_encoding OPTIONAL
*
*    METHODS read_internal                                   "#EC WARNOK
*      IMPORTING
*        length TYPE numeric
*      RETURNING
*        value(result) TYPE string .
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_filter_x2c_c_reader DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_io_c_reader.
    METHODS constructor
          IMPORTING
            io_x_reader TYPE REF TO zif_io_x_reader
            i_encoding  TYPE abap_encoding.
    ALIASES data_available    FOR zif_io_c_reader~data_available.
    ALIASES is_x_reader       FOR zif_io_c_reader~is_x_reader.
    ALIASES close             FOR zif_io_c_reader~close.
    ALIASES is_closed         FOR zif_io_c_reader~is_closed.
    ALIASES set_mark          FOR zif_io_c_reader~set_mark.
    ALIASES delete_mark       FOR zif_io_c_reader~delete_mark.
    ALIASES is_mark_supported FOR zif_io_c_reader~is_mark_supported.
    ALIASES is_reset_supported FOR zif_io_c_reader~is_reset_supported.
    ALIASES reset_to_mark     FOR zif_io_c_reader~reset_to_mark.
    ALIASES reset             FOR zif_io_reader~reset.
    ALIASES skip              FOR zif_io_c_reader~skip.
    ALIASES read              FOR zif_io_c_reader~read.
  PRIVATE SECTION.
    DATA aio_x_reader TYPE REF TO zif_io_x_reader.
    DATA ai_encoding  TYPE abap_encoding.
ENDCLASS.

*---------------------------------------------------------------------
CLASS zcl_io_msdos DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:   to_date IMPORTING       date TYPE d RETURNING value(msdos_date) TYPE i.
    CLASS-METHODS:   to_time IMPORTING       time TYPE t RETURNING value(msdos_time) TYPE i.
    CLASS-METHODS: from_date IMPORTING msdos_date TYPE i RETURNING value(date)       TYPE d.
    CLASS-METHODS: from_time IMPORTING msdos_time TYPE i RETURNING value(time)       TYPE t.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zip_parse_error DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_filter_zip DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_file,
             name TYPE string,
             date TYPE d,
             time TYPE t,
             size TYPE i,
           END OF t_file .
    TYPES:
      t_files TYPE STANDARD TABLE OF t_file WITH DEFAULT KEY.
    CLASS-METHODS get_files
      IMPORTING
        io_x_reader TYPE REF TO zif_io_x_reader
      RETURNING
        value(files) TYPE t_files
      EXCEPTIONS
        zip_parse_error .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_ext,
             min_extract_version TYPE i,
             gen_flags           TYPE i,
             compressed          TYPE i,
             compsize            TYPE i,
             crc32(4)            TYPE x,
             filename_len        TYPE i,
             filename            TYPE xstring,
             extra_len           TYPE i,
             extra               TYPE xstring,
             content             TYPE xstring,
           END OF t_ext .
    TYPES:
      t_exts TYPE TABLE OF t_ext .

    CLASS-DATA exts TYPE t_exts .
ENDCLASS.
















*----------------------------------------------------------------------*
CLASS zcl_io_stream_utilities IMPLEMENTATION.
  METHOD check_data_type_is_string.
    DATA type TYPE REF TO cl_abap_typedescr.
    DATA l_name TYPE string.
    type = cl_abap_typedescr=>describe_by_data( data ).
    IF type <> cl_abap_elemdescr=>get_string( ).
      l_name = type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `READ_DATA`
          type      = l_name.
    ENDIF.
*    IF zif_io_reader~is_x_reader( ) = abap_false.
** convert binary to character
*    ENDIF.
  ENDMETHOD.
  METHOD check_data_type_is_xstring.
    DATA type TYPE REF TO cl_abap_typedescr.
    DATA l_name TYPE string.
    type = cl_abap_typedescr=>describe_by_data( data ).
    IF type <> cl_abap_elemdescr=>get_xstring( ).
      l_name = type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `READ_DATA`
          type      = l_name.
    ENDIF.
*    IF zif_io_reader~is_x_reader( ) = abap_false.
** convert binary to character
*    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*
*  IMPLEMENTATIONS
*  of zcl_io_<type>_<direction> = cl_abap_<type>_<direction>
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
CLASS zcl_io_c_reader IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD data_available.
    CALL METHOD me->('DATA_AVAILABLE_INTERNAL')
      RECEIVING
        available = available.
  ENDMETHOD.
  METHOD read.
    CALL METHOD me->('READ_INTERNAL')
      EXPORTING
        length = length
      RECEIVING
        result = result.
  ENDMETHOD.
  METHOD close.
    closed = abap_true.
  ENDMETHOD.
  METHOD is_closed.
    closed = me->closed.
  ENDMETHOD.
  METHOD is_x_reader.
    result = abap_false.
  ENDMETHOD.
  METHOD skip.
    read( length ).
  ENDMETHOD.
  METHOD set_mark.
  ENDMETHOD.
  METHOD delete_mark.
    IF closed IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.
  METHOD reset_to_mark.
  ENDMETHOD.
  METHOD is_mark_supported.
    res = abap_false.
  ENDMETHOD.
  METHOD zif_io_reader~is_reset_supported.
    result = abap_false.
  ENDMETHOD.
  METHOD zif_io_reader~reset.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_io_stream_position_error
      EXPORTING
        textid = zcx_io_stream_position_error=>zcx_io_reset_not_supported.
  ENDMETHOD.
  METHOD zif_io_reader~read.
    zcl_io_stream_utilities=>check_data_type_is_string( read_data ).
    read_data = read( length ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_x_reader IMPLEMENTATION.
  METHOD constructor.
    aov_closed = abap_false.
  ENDMETHOD.
  METHOD zif_io_reader~read.
    zcl_io_stream_utilities=>check_data_type_is_string( read_data ).
    read_data = read( length ).
  ENDMETHOD.
  METHOD read.
    CALL METHOD me->('READ_INTERNAL')
      EXPORTING
        length = length
      RECEIVING
        result = result.
  ENDMETHOD.
  METHOD close.
    DATA lx_root TYPE REF TO cx_root.
    aov_closed = abap_true.
    TRY.
        CALL METHOD me->('CLOSE_INTERNAL').
      CATCH cx_sy_dyn_call_illegal_method INTO lx_root.
    ENDTRY.
  ENDMETHOD.
  METHOD is_closed.
    closed = aov_closed.
    DATA lx_root TYPE REF TO cx_root.
    TRY.
        CALL METHOD me->('IS_CLOSED_INTERNAL')
          RECEIVING
            closed = closed.
      CATCH cx_sy_dyn_call_illegal_method INTO lx_root.
        closed = me->closed.
    ENDTRY.
  ENDMETHOD.
  METHOD data_available.
    CALL METHOD me->('DATA_AVAILABLE_INTERNAL')
      RECEIVING
        available = available.
  ENDMETHOD.
  METHOD is_x_reader.
  ENDMETHOD.
  METHOD skip.
    read( length ).
  ENDMETHOD.
  METHOD set_mark.
  ENDMETHOD.
  METHOD delete_mark.
  ENDMETHOD.
  METHOD reset_to_mark.
  ENDMETHOD.
  METHOD is_mark_supported.
  ENDMETHOD.
  METHOD reset.
  ENDMETHOD.
  METHOD is_reset_supported.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_c_writer IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD write.
    CALL METHOD me->('WRITE_INTERNAL')
      EXPORTING
        data = data.
  ENDMETHOD.
  METHOD flush.
  ENDMETHOD.
  METHOD close.
    DATA lx_root TYPE REF TO cx_root.
    TRY.
        CALL METHOD me->('CLOSE_INTERNAL').
      CATCH cx_sy_dyn_call_illegal_method INTO lx_root.
    ENDTRY.
  ENDMETHOD.
  METHOD is_closed.
    DATA lx_root TYPE REF TO cx_root.
    TRY.
        CALL METHOD me->('IS_CLOSED_INTERNAL')
          RECEIVING
            closed = closed.
      CATCH cx_sy_dyn_call_illegal_method INTO lx_root.
        closed = me->closed.
    ENDTRY.
  ENDMETHOD.
  METHOD is_x_writer.
    result = abap_false.
  ENDMETHOD.
  METHOD zif_io_writer~write.
    DATA type TYPE REF TO cl_abap_typedescr.
    DATA l_name TYPE string.
    type = cl_abap_typedescr=>describe_by_data( data ).
    IF type = cl_abap_elemdescr=>get_string( ).
      write( data ).
    ELSE.
      l_name = type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
         EXPORTING
           textid = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
           parameter = `DATA`
           type = l_name.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_x_writer IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD close.
  ENDMETHOD.
  METHOD is_closed.
  ENDMETHOD.
  METHOD zif_io_writer~write.
    DATA type TYPE REF TO cl_abap_typedescr.
    DATA l_name TYPE string.
    type = cl_abap_typedescr=>describe_by_data( data ).
    IF type = cl_abap_elemdescr=>get_xstring( ).
      write( data ).
    ELSE.
      l_name = type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
         EXPORTING
           textid = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
           parameter = `DATA`
           type = l_name.
    ENDIF.
  ENDMETHOD.
  METHOD write.
    CALL METHOD me->('WRITE_INTERNAL')
      EXPORTING
        data = data.
  ENDMETHOD.
  METHOD flush.
  ENDMETHOD.
  METHOD is_x_writer.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*
*  IMPLEMENTATIONS
*  of zcl_io_<resource>_<type>_<direction> = cl_abap_<resource>_<type>_<direction>
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
CLASS zcl_io_memory_c_reader IMPLEMENTATION.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_memory_c_writer IMPLEMENTATION.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_memory_x_reader IMPLEMENTATION.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_memory_x_writer IMPLEMENTATION.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_string_c_reader IMPLEMENTATION.

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

  METHOD zif_io_reader~reset.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_offset = 0.
    m_mark = -1.
  ENDMETHOD.

  METHOD zif_io_reader~reset_to_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF m_mark = -1.
      RAISE EXCEPTION TYPE zcx_io_stream_position_error
        EXPORTING textid = zcx_io_stream_position_error=>zcx_io_mark_not_set.
    ENDIF.
    m_offset = m_mark.
  ENDMETHOD.

  METHOD zif_io_reader~set_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_mark = m_offset.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    m_str = str.
  ENDMETHOD.

  METHOD data_available_internal.
    IF m_offset < STRLEN( m_str ).
      available = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD read_internal.
    DATA l_dummy TYPE i.
    l_dummy = length + m_offset.
    IF l_dummy > STRLEN( m_str ).
      result = m_str+m_offset(*).
    ELSE.
      result = m_str+m_offset(length).
    ENDIF.
    m_offset = l_dummy.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_string_c_writer IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    GET REFERENCE OF str INTO m_ref_str.
  ENDMETHOD.
  METHOD write_internal.
    IF m_ref_str IS BOUND.
      CONCATENATE m_ref_str->* data INTO m_ref_str->*.
    ELSE.
      CONCATENATE m_str data INTO m_str.
    ENDIF.
  ENDMETHOD.
  METHOD get_result.
    DATA lo_rtti TYPE REF TO cl_abap_typedescr.
    DATA l_type_kind TYPE string.
    IF cl_abap_typedescr=>describe_by_data( result ) <> cl_abap_elemdescr=>get_string( ).
      lo_rtti = cl_abap_typedescr=>describe_by_data( result ).
      l_type_kind = lo_rtti->type_kind.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          parameter = `RESULT`
          type      = l_type_kind.
    ENDIF.
    result = m_str.
  ENDMETHOD.
  METHOD get_result_type.
    result_type = cl_abap_elemdescr=>get_string( ).
  ENDMETHOD.
  METHOD get_result_string.
    str = m_str.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* STRING X READER
*----------------------------------------------------------------------*
CLASS zcl_io_string_x_reader IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    m_str = str.
    aiv_stream_bytes = XSTRLEN( m_str ).
    aiv_offset = 0.
    aov_data_available = abap_true.
    IF aiv_stream_bytes = 0.
      aov_data_available = abap_false.
    ENDIF.
    IF xstr IS SUPPLIED. "hors standard
      GET REFERENCE OF xstr INTO m_ref_xstr.
    ENDIF.
  ENDMETHOD.
  METHOD data_available_internal.
    available = aov_data_available.
  ENDMETHOD.
  METHOD read_internal.
    DATA lv_bytes TYPE i.
    FIELD-SYMBOLS <lv_xsequence> TYPE xsequence.
    IF data_available( ) = abap_false.
      lv_bytes = 0.
      CLEAR result.
      "RAISE EXCEPTION TYPE zcx_io_end_of_istream.
    ELSE.
      lv_bytes = aiv_stream_bytes - aiv_offset.
      IF length >= lv_bytes.
        aov_data_available = abap_false.
      ELSE.
        lv_bytes = length.
      ENDIF.
      result = m_str+aiv_offset(lv_bytes).
      ADD lv_bytes TO aiv_offset.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_string_x_writer IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    GET REFERENCE OF xstr INTO m_ref_xstr.
  ENDMETHOD.
  METHOD write_internal.
    IF m_ref_xstr IS BOUND.
      CONCATENATE m_ref_xstr->* data INTO m_ref_xstr->* IN BYTE MODE.
    ELSE.
      CONCATENATE m_str data INTO m_str IN BYTE MODE.
    ENDIF.
  ENDMETHOD.
  METHOD get_result.
    DATA lo_rtti TYPE REF TO cl_abap_typedescr.
    DATA l_type_kind TYPE string.
    IF cl_abap_typedescr=>describe_by_data( result ) <> cl_abap_elemdescr=>get_xstring( ).
      lo_rtti = cl_abap_typedescr=>describe_by_data( result ).
      l_type_kind = lo_rtti->type_kind.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
            EXPORTING
              parameter = `RESULT`
              type = l_type_kind.
    ENDIF.
    result = m_str.
  ENDMETHOD.

  METHOD get_result_type.
    result_type = cl_abap_elemdescr=>get_xstring( ).
  ENDMETHOD.

  METHOD get_result_string.
    str = m_str.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_itab_c_reader IMPLEMENTATION.

  METHOD zif_io_reader~delete_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE cx_resource_already_closed.
    ENDIF.
    m_line_index_mark = 0.
    m_pos_mark = -1.
  ENDMETHOD.

  METHOD zif_io_reader~is_mark_supported.
    res = abap_true.
  ENDMETHOD.

  METHOD zif_io_reader~is_reset_supported.
    result = abap_true.
  ENDMETHOD.

  METHOD zif_io_reader~reset.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE cx_resource_already_closed.
    ENDIF.
    m_line_index = 1.
    m_position = 0.
    find_first( ).
    m_line_index_mark = 0.
    m_pos_mark = -1.
  ENDMETHOD.

  METHOD zif_io_reader~reset_to_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE cx_resource_already_closed.
    ENDIF.
    m_line_index = m_line_index_mark.
    m_position = m_pos_mark.
  ENDMETHOD.

  METHOD zif_io_reader~set_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE cx_resource_already_closed.
    ENDIF.
    m_line_index_mark = m_line_index.
    m_pos_mark = m_position.
  ENDMETHOD.

  METHOD constructor.
    FIELD-SYMBOLS <input> TYPE STANDARD TABLE.
    DATA itab_desc TYPE REF TO cl_abap_tabledescr .
    DATA l_name TYPE string.

    super->constructor( ).
    " Check type
    itab_desc ?= cl_abap_typedescr=>describe_by_data( itab ).
    m_line_type = itab_desc->get_table_line_type( ).
    IF m_line_type->type_kind <> cl_abap_typedescr=>typekind_char AND
       m_line_type->type_kind <> cl_abap_typedescr=>typekind_string.
      l_name = m_line_type->get_relative_name( ).
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `ITAB`
          type      = l_name.
    ENDIF.
    IF m_line_type->type_kind = cl_abap_typedescr=>typekind_string.
      m_line_type_is_string = abap_true.
    ELSE.
      m_line_length = m_line_type->length.
    ENDIF.
    CREATE DATA m_itab LIKE itab.
    ASSIGN m_itab->* TO <input> CASTING LIKE itab.
    <input> = itab.
    find_first( ).
  ENDMETHOD.

  METHOD data_available_internal.
    available = m_data_available.
  ENDMETHOD.

  METHOD find_first.
    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line> TYPE csequence.
    DATA line TYPE REF TO data.
    ASSIGN m_itab->* TO <input>.
    DO.
      READ TABLE <input> INDEX m_line_index REFERENCE INTO line.
      IF sy-subrc <> 0.
        m_data_available = abap_false.
        RETURN.
      ENDIF.
      ASSIGN line->* TO <line> CASTING TYPE HANDLE m_line_type.
      IF <line> IS NOT INITIAL.
        EXIT.
      ENDIF.
      m_line_index = m_line_index + 1.
    ENDDO.
  ENDMETHOD.

  METHOD read_internal. "BY KERNEL MODULE ab_km_ItabCReadInternal.
    FIELD-SYMBOLS: <input> TYPE STANDARD TABLE,
                   <line> TYPE csequence.
    DATA line TYPE REF TO data.
    DATA l_take TYPE i.
    DATA l_remain_eol TYPE i.
    DATA l_remain TYPE i.

    IF abap_true = data_available_internal( ) AND length > 0.
      l_remain = length.
      ASSIGN m_itab->* TO <input>.
      DO.
        READ TABLE <input> INDEX m_line_index REFERENCE INTO line.
        IF sy-subrc <> 0.
          m_data_available = abap_false.
          EXIT.
        ENDIF.
        ASSIGN line->* TO <line> CASTING TYPE HANDLE m_line_type.
        IF m_line_type_is_string = abap_true.
          l_remain_eol = STRLEN( <line> ) - m_position.
        ELSE.
          l_remain_eol = m_line_length - m_position.
        ENDIF.
        IF l_remain_eol > l_remain.
          l_take = l_remain.
        ELSE.
          l_take = l_remain_eol.
        ENDIF.
        IF l_take > 0.
          CONCATENATE result <line>+m_position(l_take) INTO result.

          IF l_take = l_remain_eol.
            ADD 1 TO m_line_index.
            m_position = 0.
          ELSE.
            ADD l_take TO m_position.
          ENDIF.

          SUBTRACT l_take FROM l_remain.
          IF l_remain = 0.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_itab_c_writer IMPLEMENTATION.

  METHOD zif_io_memory_writer~get_result.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN m_table->* TO <table>.
    ASSERT sy-subrc = 0.
    result = <table>.
    length_of_last_line = m_offset.
  ENDMETHOD.

  METHOD zif_io_memory_writer~get_result_type.
    result_type ?= cl_abap_typedescr=>describe_by_data_ref( m_table ).
  ENDMETHOD.

  METHOD zif_io_itab_writer~get_max_line_length.
    line_length = m_line_length.
  ENDMETHOD.

  METHOD zif_io_itab_writer~get_result_table.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN m_table->* TO <table>.
    ASSERT sy-subrc = 0.
    table = <table>.
    length_of_last_line = m_offset.
  ENDMETHOD.

  METHOD constructor.
    DATA table_type TYPE REF TO cl_abap_tabledescr.
    DATA str TYPE string.
    super->constructor( ).
    IF line_type <> cl_abap_typedescr=>typekind_string AND
       line_type <> cl_abap_typedescr=>typekind_char.
      str = line_type.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_range
        EXPORTING
          parameter = `LINE_TYPE`
          value     = str.
    ENDIF.
    IF line_length <= 0.
      str = line_length.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_range
        EXPORTING
          parameter = `LINE_LENGTH`
          value     = str.
    ENDIF.
    IF line_type = cl_abap_typedescr=>typekind_string.
      m_line_type = cl_abap_elemdescr=>get_string( ).
      m_line_type_is_string = abap_true.
    ELSE.
      m_line_type = cl_abap_elemdescr=>get_c( line_length ).
    ENDIF.
    table_type = cl_abap_tabledescr=>create( m_line_type ).
    CREATE DATA m_table TYPE HANDLE table_type.
    m_line_length = line_length.
    m_line_index = 1.
  ENDMETHOD.

  METHOD write_internal. "BY KERNEL MODULE ab_km_ItabCWRiteInternal.
    "TODO
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_file_c_reader IMPLEMENTATION.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_file_x_reader IMPLEMENTATION.
ENDCLASS.

*----------------------------------------------------------------------*
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

  METHOD data_available_internal.
    available = abap_true. "TODO
  ENDMETHOD.

  METHOD is_mark_supported.
    res = abap_true.
  ENDMETHOD.
  METHOD set_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    GET DATASET aio_file->filename POSITION m_mark.
  ENDMETHOD.
  METHOD delete_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_mark = -1.
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
  METHOD is_reset_supported.
    result = abap_true.
  ENDMETHOD.
  METHOD reset.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    SET DATASET aio_file->filename POSITION zcl_io_backend=>cs_position-begin_of_file.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_backend_c_reader IMPLEMENTATION.
  METHOD constructor.
    DATA ls_attr TYPE dset_attributes.
    DATA lx_root TYPE REF TO cx_root.

    CALL METHOD super->constructor.
*          EXPORTING
*            io_file = io_file.
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
      WHEN dset_text_mode
        OR dset_legacy_text_mode.
      WHEN OTHERS.
* Si le programme est non-unicode alors lire des caractères en legacy binary est permis
        IF abap_false = zcl_io_program=>is_unicode_program( sy-repid ) AND ls_attr-fixed-mode = dset_legacy_binary_mode.
        ELSE.
          RAISE EXCEPTION TYPE zcx_io_parameter_invalid
           EXPORTING
             parameter = `IO_FILE`.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD read_internal.
    DATA l_characters TYPE i.
    DATA l_pos TYPE i.
    DATA l_length TYPE i.
    DATA l_string TYPE string.
    DATA l_length2 TYPE i.
    DATA off TYPE i.
    DATA len TYPE i.
    DATA l_subrc TYPE sysubrc.

* réserver l'espace (performance)
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

          DATA lx_root TYPE REF TO cx_root.
          DATA lx_sy_conversion_codepage TYPE REF TO cx_sy_conversion_codepage.
        CATCH cx_sy_conversion_codepage INTO lx_sy_conversion_codepage.
          IF aio_file->aus_attr-conv_errors IS INITIAL.
            RAISE EXCEPTION lx_sy_conversion_codepage.
          ENDIF.
        CATCH cx_root INTO lx_root.
          RAISE EXCEPTION lx_root.
      ENDTRY.

      l_subrc = sy-subrc.
      REPLACE SECTION OFFSET off LENGTH l_characters OF result WITH l_string IN CHARACTER MODE.
      ADD l_characters TO off.
      IF l_subrc <> 0.
        available = abap_false.
        EXIT. "on a atteint la fin du fichier
      ELSEIF off >= length. "code retour du READ DATASET
        EXIT. "on a atteint le nombre de caractères demandé
      ENDIF.
* ajout du caractère de fin de ligne
* note: inutile de se complexifier la vie avec CRLF car Windows considère un
* LF simple tout de même comme un retour chariot!
      REPLACE SECTION OFFSET off LENGTH 1 OF result
            WITH cl_abap_char_utilities=>newline IN CHARACTER MODE.
      ADD 1 TO off.
* calculer le nombre de caractères restant à lire
      SUBTRACT 1 FROM l_length.
      SUBTRACT l_characters FROM l_length.
    ENDDO.
* couper les caractères initialement réservés, mais non alimentés
* (note: il ne faut pas supprimer les espaces à la fin, on risquerait
* de supprimer ceux qui ont réellement été lus)
    len = length - off.
    IF len > 0.
      REPLACE SECTION OFFSET off LENGTH len OF result WITH `` IN CHARACTER MODE.
    ENDIF.
  ENDMETHOD.

  METHOD data_available_internal.
    available = me->available.
  ENDMETHOD.

  METHOD is_mark_supported.
    res = abap_true.
  ENDMETHOD.

  METHOD set_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    GET DATASET aio_file->filename POSITION m_mark.
  ENDMETHOD.

  METHOD delete_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    m_mark = -1.
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
    available = abap_true.
  ENDMETHOD.

  METHOD is_reset_supported.
    result = abap_true.
  ENDMETHOD.

  METHOD reset.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    SET DATASET aio_file->filename POSITION zcl_io_backend=>cs_position-begin_of_file.
    available = abap_true.
  ENDMETHOD.

*  METHOD close.
*    super->close( ).
*    aio_file->close( ).
*  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
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
ENDCLASS."

*----------------------------------------------------------------------*
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
*        has_field_separator     = io_file->has_field_separator
*        header_length           = io_file->header_length
        read_by_line            = abap_false
*        dat_mode                = io_file->dat_mode
*        codepage                = io_file->codepage
*        ignore_cerr             = io_file->ignore_cerr
*        replacement             = io_file->replacement
*        virus_scan_profile      = io_file->virus_scan_profile
      IMPORTING
        filelength              = l_filelength
*        header                  = me->header
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

  METHOD read_internal.
    result = aio_string_c_reader->read_internal( length ).
  ENDMETHOD.

  METHOD data_available_internal.
    available = aio_string_c_reader->data_available_internal( ).
  ENDMETHOD.

  METHOD is_mark_supported.
    res = is_mark_supported( ).
  ENDMETHOD.
  METHOD set_mark.
  ENDMETHOD.
  METHOD delete_mark.
  ENDMETHOD.
  METHOD reset_to_mark.
  ENDMETHOD.
  METHOD is_reset_supported.
  ENDMETHOD.
  METHOD reset.
  ENDMETHOD.
ENDCLASS."

*----------------------------------------------------------------------*
CLASS zcl_io_frontend_c_writer IMPLEMENTATION.
  METHOD constructor.
    DATA ls_attr TYPE dset_attributes.

    CALL METHOD super->constructor.
    aio_file = io_file.
    ai_buffer_size = i_buffer_size.
  ENDMETHOD.
  METHOD write_internal.
    DATA lt_data TYPE TABLE OF string.
    IF ai_buffer_size > 0.
      CONCATENATE m_str data INTO m_str.
    ENDIF.
    IF ai_buffer_size = 0 OR STRLEN( m_str ) >= ai_buffer_size.
      IF ai_buffer_size = 0.
        APPEND data TO lt_data.
      ELSE.
        APPEND m_str(ai_buffer_size) TO lt_data.
        IF STRLEN( m_str ) = ai_buffer_size.
          CLEAR m_str.
        ELSE.
          m_str = m_str+ai_buffer_size.
        ENDIF.
      ENDIF.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = aio_file->filename
          filetype                = 'ASC'
          append                  = abap_false
          write_lf                = abap_false
          confirm_overwrite       = aio_file->confirm_overwrite
        CHANGING
          data_tab                = lt_data
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24.
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD close_internal.
    DATA lt_data TYPE TABLE OF string.
    IF ai_buffer_size > 0 AND m_str IS NOT INITIAL.
      APPEND m_str TO lt_data.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = aio_file->filename
          filetype                = 'ASC'
          write_lf                = abap_false
          confirm_overwrite       = aio_file->confirm_overwrite
        CHANGING
          data_tab                = lt_data
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24.
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_filter_gzip_x_reader IMPLEMENTATION.
  METHOD constructor.
    DATA l_buf_len TYPE i.
    DATA ls_ungzip_stream TYPE ty_is_ungzip_stream.
    CREATE OBJECT aio_ungzip_bin
      EXPORTING
        output_handler = me.
    l_buf_len = -1.
    CALL METHOD aio_ungzip_bin->set_out_buf
      IMPORTING
        out_buf     = ai_buffer
        out_buf_len = l_buf_len.
    aio_x_reader = io_x_reader.
    ai_gzip_buffer = i_gzip_buffer.
    ai_stream_buffer = i_stream_buffer.
    ls_ungzip_stream-o_ungzip_stream = aio_ungzip_bin.
    ls_ungzip_stream-o_x_reader = me.
    INSERT ls_ungzip_stream INTO TABLE kit_ungzip_stream.
  ENDMETHOD.

  METHOD if_abap_ungzip_binary_handler~use_out_buf.
    DATA ls_ungzip_stream TYPE ty_is_ungzip_stream.
    DATA lo_gzip_x_reader TYPE REF TO zcl_io_filter_gzip_x_reader.
* méthode appelée tous les 1000 octets (cf plus loin, à cause de SET_OUT_BUF), et à la fin
* OUT_BUF       XSEQUENCE   Output Buffer
* OUT_BUF_LEN   I           Length of Output Buffer
* PART          I           Segment (compteur à partir de 1, qui s'incrémente de 1 à chaque appel)
* GZIP_STREAM   Type Ref To cl_abap_UNGZIP_BINARY_STREAM
    READ TABLE kit_ungzip_stream INTO ls_ungzip_stream
          WITH TABLE KEY o_ungzip_stream = gzip_stream.
    IF sy-subrc = 0.
      lo_gzip_x_reader ?= ls_ungzip_stream-o_x_reader.
      lo_gzip_x_reader->read( length = lo_gzip_x_reader->ai_gzip_buffer ).
*    if part = cl_abap_unGZIP_BINARY_STREAM=>last.
    ENDIF.
  ENDMETHOD.

  METHOD read_internal.
    DATA l_xstring TYPE xstring.
    WHILE XSTRLEN( ai_buffer ) < length AND abap_false = aio_x_reader->data_available( ).
      l_xstring = aio_x_reader->read( ai_stream_buffer ).
      CALL METHOD aio_ungzip_bin->decompress_binary_stream
        EXPORTING
          gzip_in     = l_xstring
          gzip_in_len = -1.
    ENDWHILE.
  ENDMETHOD.

  METHOD close.
    CALL METHOD aio_ungzip_bin->decompress_binary_stream_end
      EXPORTING
        gzip_in     = ai_buffer
        gzip_in_len = -1.
  ENDMETHOD.

  METHOD is_closed.
  ENDMETHOD.

  METHOD is_x_reader.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD zif_io_reader~read.
  ENDMETHOD.

  METHOD skip.
  ENDMETHOD.

  METHOD data_available.
  ENDMETHOD.

  METHOD delete_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.

  METHOD is_mark_supported.
    res = abap_true.
  ENDMETHOD.

  METHOD is_reset_supported.
    result = abap_true.
  ENDMETHOD.

  METHOD reset.
  ENDMETHOD.

  METHOD reset_to_mark.
  ENDMETHOD.

  METHOD set_mark.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_filter_gzip_x_writer IMPLEMENTATION.
  METHOD constructor.
    DATA l_buf_len TYPE i.
    DATA ls_gzip_stream TYPE ty_is_gzip_stream.
    CREATE OBJECT aio_gzip_bin
      EXPORTING
        compress_level = compress_level
        output_handler = me.
    l_buf_len = -1.
    CALL METHOD aio_gzip_bin->set_out_buf
      IMPORTING
        out_buf     = ai_buffer
        out_buf_len = l_buf_len.
    ai_buffer_size = i_buffer_size.
    ls_gzip_stream-o_gzip_stream = aio_gzip_bin.
    ls_gzip_stream-o_x_writer = me.
    INSERT ls_gzip_stream INTO TABLE kit_gzip_stream.
  ENDMETHOD.

  METHOD if_abap_gzip_binary_handler~use_out_buf.
    DATA ls_gzip_stream TYPE ty_is_gzip_stream.
* méthode appelée tous les 1000 octets (cf plus loin, à cause de SET_OUT_BUF), et à la fin
* OUT_BUF       XSEQUENCE   Output Buffer
* OUT_BUF_LEN   I           Length of Output Buffer
* PART          I           Segment (compteur à partir de 1, qui s'incrémente de 1 à chaque appel)
* GZIP_STREAM   Type Ref To cl_abap_GZIP_BINARY_STREAM
    READ TABLE kit_gzip_stream INTO ls_gzip_stream
          WITH TABLE KEY o_gzip_stream = gzip_stream.
    IF sy-subrc = 0.
      ls_gzip_stream-o_x_writer->write( out_buf(out_buf_len) ).
*    if part = cl_abap_GZIP_BINARY_STREAM=>last.
    ENDIF.
  ENDMETHOD.

  METHOD write_internal.
    CALL METHOD aio_gzip_bin->compress_binary_stream
      EXPORTING
        raw_in = data.
  ENDMETHOD.

  METHOD close.
    CALL METHOD aio_gzip_bin->compress_binary_stream_end
      EXPORTING
        raw_in = ai_buffer.
  ENDMETHOD.

  METHOD is_closed.
  ENDMETHOD.

  METHOD flush.
  ENDMETHOD.

  METHOD is_x_writer.
  ENDMETHOD.

  METHOD write.
  ENDMETHOD.

  METHOD zif_io_writer~write.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_filter_gzip_c_reader IMPLEMENTATION.
  METHOD constructor.
    DATA l_buf_len TYPE i.
    DATA ls_ungzip_stream TYPE ty_is_ungzip_stream.
    CREATE OBJECT aio_ungzip_text
      EXPORTING
        output_handler = me.
    l_buf_len = -1.
    CALL METHOD aio_ungzip_text->set_out_buf
      IMPORTING
        out_buf     = ai_buffer
        out_buf_len = l_buf_len.
    aio_x_reader = io_x_reader.
    ai_gzip_buffer = i_gzip_buffer.
    ai_stream_buffer = i_stream_buffer.
    ls_ungzip_stream-o_ungzip_stream = aio_ungzip_text.
    ls_ungzip_stream-o_c_reader = me.
    INSERT ls_ungzip_stream INTO TABLE kit_ungzip_stream.
  ENDMETHOD.

  METHOD if_abap_ungzip_text_handler~use_out_buf.
    DATA ls_ungzip_stream TYPE ty_is_ungzip_stream.
    DATA lo_gzip_c_reader TYPE REF TO zcl_io_filter_gzip_c_reader.
* méthode appelée tous les 1000 octets (à cause de SET_OUT_BUF), et à la fin
* OUT_BUF       XSEQUENCE   Output Buffer
* OUT_BUF_LEN   I           Length of Output Buffer
* PART          I           Segment (compteur à partir de 1, qui s'incrémente de 1 à chaque appel)
* GZIP_STREAM   Type Ref To cl_abap_UNGZIP_BINARY_STREAM
    READ TABLE kit_ungzip_stream INTO ls_ungzip_stream
          WITH TABLE KEY o_ungzip_stream = gzip_stream.
    IF sy-subrc = 0.
      lo_gzip_c_reader ?= ls_ungzip_stream-o_c_reader.
      lo_gzip_c_reader->read( length = lo_gzip_c_reader->ai_gzip_buffer ).
*    if part = cl_abap_unGZIP_BINARY_STREAM=>last.
    ENDIF.
  ENDMETHOD.

  METHOD read_internal.
    DATA l_xstring TYPE xstring.
    WHILE STRLEN( ai_buffer ) < length AND abap_true = aio_x_reader->data_available( ).
      l_xstring = aio_x_reader->read( ai_stream_buffer ).
      IF  abap_true = aio_x_reader->data_available( ).
        CALL METHOD aio_ungzip_text->decompress_text_stream
          EXPORTING
            gzip_in     = l_xstring
            gzip_in_len = -1.
      ELSE.
        CALL METHOD aio_ungzip_text->decompress_text_stream_end
          EXPORTING
            gzip_in     = l_xstring
            gzip_in_len = -1.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD close.
  ENDMETHOD.

  METHOD is_closed.
  ENDMETHOD.

  METHOD is_x_reader.
    result = abap_false.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD zif_io_reader~read.
  ENDMETHOD.

  METHOD skip.
  ENDMETHOD.

  METHOD data_available.
  ENDMETHOD.

  METHOD delete_mark.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.

  METHOD is_mark_supported.
    res = abap_true.
  ENDMETHOD.

  METHOD is_reset_supported.
    result = abap_true.
  ENDMETHOD.

  METHOD reset.
  ENDMETHOD.

  METHOD reset_to_mark.
  ENDMETHOD.

  METHOD set_mark.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_filter_gzip_c_writer IMPLEMENTATION.
  METHOD constructor.
    DATA l_buf_len TYPE i.
    DATA ls_gzip_stream TYPE ty_is_gzip_stream.
    CREATE OBJECT aio_gzip_text
      EXPORTING
        compress_level = compress_level
        output_handler = me.
    l_buf_len = -1.
    CALL METHOD aio_gzip_text->set_out_buf
      IMPORTING
        out_buf     = ai_buffer
        out_buf_len = l_buf_len.
    ai_buffer_size = i_buffer_size.
    aio_x_writer = io_x_writer.
    ls_gzip_stream-o_gzip_stream = aio_gzip_text.
    ls_gzip_stream-o_c_writer = me.
    INSERT ls_gzip_stream INTO TABLE kit_gzip_stream.
  ENDMETHOD.

  METHOD if_abap_gzip_text_handler~use_out_buf.
    DATA ls_gzip_stream TYPE ty_is_gzip_stream.
* méthode appelée tous les 1000 octets (cf plus loin, à cause de SET_OUT_BUF), et à la fin
* OUT_BUF       XSEQUENCE   Output Buffer
* OUT_BUF_LEN   I           Length of Output Buffer
* PART          I           Segment (compteur à partir de 1, qui s'incrémente de 1 à chaque appel)
* GZIP_STREAM   Type Ref To cl_abap_GZIP_BINARY_STREAM
    READ TABLE kit_gzip_stream INTO ls_gzip_stream
          WITH TABLE KEY o_gzip_stream = gzip_stream.
    IF sy-subrc = 0.
      ls_gzip_stream-o_c_writer->aio_x_writer->write( out_buf(out_buf_len) ).
*    if part = cl_abap_GZIP_BINARY_STREAM=>last.
    ENDIF.
  ENDMETHOD.

  METHOD write_internal.
    CALL METHOD aio_gzip_text->compress_text_stream
      EXPORTING
        text_in     = data
        text_in_len = -1.
  ENDMETHOD.

  METHOD close.
    DATA l_string TYPE string.
    CALL METHOD aio_gzip_text->compress_text_stream_end
      EXPORTING
        text_in     = l_string
        text_in_len = 0.
  ENDMETHOD.

  METHOD is_closed.
  ENDMETHOD.

  METHOD flush.
  ENDMETHOD.

  METHOD is_x_writer.
  ENDMETHOD.

  METHOD write.
  ENDMETHOD.

  METHOD zif_io_writer~write.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_filter_x_reader IMPLEMENTATION.

  METHOD constructor.
    aoo_x_reader    = io_x_reader.
  ENDMETHOD.
*
*  METHOD read_internal.
*    DATA l_xstring TYPE xstring.
*    l_xstring = aio_x_reader->read_internal( length ).
*    CONCATENATE buffer l_xstring INTO buffer IN BYTE MODE.
*  ENDMETHOD.

  METHOD close.
    aoo_x_reader->close( ).
  ENDMETHOD.

  METHOD is_closed.
    closed = aoo_x_reader->is_closed( ).
  ENDMETHOD.

  METHOD is_x_reader.
    result = abap_true.
  ENDMETHOD.

  METHOD zif_io_reader~read.
    zcl_io_stream_utilities=>check_data_type_is_xstring( read_data ).
    read_data = aoo_x_reader->read( length ).
  ENDMETHOD.

  METHOD read.
    result = aoo_x_reader->read( length ).
  ENDMETHOD.

  METHOD skip.
    aoo_x_reader->skip( length ).
  ENDMETHOD.

  METHOD data_available.
    available = aoo_x_reader->data_available( ).
  ENDMETHOD.

  METHOD delete_mark.
    aoo_x_reader->delete_mark( ).
  ENDMETHOD.

  METHOD is_mark_supported.
    res = aoo_x_reader->is_mark_supported( ).
  ENDMETHOD.

  METHOD is_reset_supported.
    result = aoo_x_reader->is_reset_supported( ).
  ENDMETHOD.

  METHOD reset.
    aoo_x_reader->reset( ).
  ENDMETHOD.

  METHOD reset_to_mark.
    aoo_x_reader->reset_to_mark( ).
  ENDMETHOD.

  METHOD set_mark.
    aoo_x_reader->set_mark( ).
  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
CLASS zcl_io_filter_buff_x_reader IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        io_x_reader = io_x_reader.
    ai_buffer_size = i_buffer_size.
  ENDMETHOD.

  METHOD read_internal.
    DATA l_new_offset TYPE i.
    DATA l_xstring TYPE xstring.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    IF length <= 0.
      "exception
    ENDIF.
    l_new_offset = offset + length.
    IF l_new_offset > XSTRLEN( buffer ).
      "cas où le buffer ne contient pas le nombre d'octets demandé
      l_xstring = aoo_x_reader->read( ai_buffer_size ).
      CONCATENATE buffer l_xstring INTO buffer IN BYTE MODE.
    ENDIF.
    IF l_new_offset > XSTRLEN( buffer ).
      "la lecture n'a pas permis d'en trouver assez
      ai_data_available = abap_false.
      result = buffer+offset. "retourner tout ce qui reste
    ELSE.
      result = buffer+offset(length).
    ENDIF.
    IF m_mark = -1.
      "s'il n'y a pas de marqueur, alors enlever la partie inutile du
      "buffer et le pointeur reste au début du buffer
      IF l_new_offset = XSTRLEN( buffer ).
        CLEAR buffer.
      ELSE.
        buffer = buffer+l_new_offset.
      ENDIF.
      offset = 0.
    ELSE.
      "s'il y a un marqueur, alors conserver tout le buffer
      "et avancer le pointeur
      ADD length TO offset.
    ENDIF.

  ENDMETHOD.

*  METHOD delete_mark.
*    IF is_closed( ) = abap_true.
*      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
*    ENDIF.
*    m_mark = -1.
*    IF offset > 0.
*      buffer = buffer+offset.
*      CLEAR offset.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD is_mark_supported.
*    res = abap_true.
*  ENDMETHOD.
*
*  METHOD is_reset_supported.
*    result = aio_x_reader->is_reset_supported( ).
*  ENDMETHOD.
*
*  METHOD reset.
**    IF abap_false = aoo_x_reader->is_reset_supported( ).
**      "RAISE EXCEPTION TYPE ...
**    ENDIF.
*    aoo_x_reader->reset( ).
*    CLEAR buffer.
*    offset = 0.
*  ENDMETHOD.
*
*  METHOD reset_to_mark.
*    offset = 0.
*  ENDMETHOD.
*
*  METHOD set_mark.
*    m_mark = 0.
*    IF ai_buffer_offset > 0.
*      ai_buffer = ai_buffer+ai_buffer_offset.
*      ai_buffer_offset = 0.
*    ENDIF.
*  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_misc_xc_reader IMPLEMENTATION.
  METHOD constructor.
*    aio_x_reader = io_x_reader.
    CREATE OBJECT aio_x_reader
      EXPORTING
        io_x_reader   = io_x_reader
        i_buffer_size = 100.
*    ai_buffer_size = i_buffer_size.

    set_utf_bom( i_utf_bom ).
    set_encoding( i_encoding ).
    set_linefeed_mode( i_linefeed_mode ).
    set_repl_char( i_repl_char ).
  ENDMETHOD.

  METHOD set_utf_bom.
    ai_utf_bom = i_utf_bom.
  ENDMETHOD.

  METHOD set_encoding.
    DATA l_codepage TYPE cpcodepage.
    DATA l_codepage2 TYPE cpcodepage.

    ai_codepage = i_encoding.
    l_codepage = i_encoding.
    l_codepage2 = '0000'. "page de codes du serveur d'application
    CREATE OBJECT aio_conv_obj
      EXPORTING
        incode  = l_codepage
        outcode = l_codepage2
        broken  = 'R'. "Conversion is canceled before any doubtful byte sequence
  ENDMETHOD.

  METHOD set_linefeed_mode.
    ai_linefeed_mode = i_linefeed_mode.
  ENDMETHOD.

  METHOD set_repl_char.
    ai_repl_char = i_repl_char.
  ENDMETHOD.

  METHOD zif_io_reader~read.
  ENDMETHOD.

  METHOD check_bom.
    DATA bytes TYPE x LENGTH 3.

    IF ai_utf_bom = abap_true AND ai_offset = 0.
      set_mark( ).
      bytes = read_bytes( 3 ).
      IF 0 = 1.
      ELSEIF bytes(2) = cl_abap_char_utilities=>byte_order_mark_big.
        reset_to_mark( ).
        skip( 2 ).
        set_encoding( '4102' ).
      ELSEIF bytes(2) = cl_abap_char_utilities=>byte_order_mark_little.
        reset_to_mark( ).
        skip( 2 ).
        set_encoding( '4103' ).
      ELSEIF bytes(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
        set_encoding( '4110' ).
      ELSE.
        reset_to_mark( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_byte_offset.
    DATA: l_string    TYPE xstring,
          l_xstring   TYPE xstring.

    IF i_offset IS INITIAL.
      result = 0.
    ELSE.
      l_string = i_string(i_offset).
      aio_conv_out->convert( EXPORTING data = l_string
                  IMPORTING buffer = l_xstring ).
      result = XSTRLEN( l_xstring ).
    ENDIF.
  ENDMETHOD.

  METHOD read_chars.
    DATA l_bytes TYPE abap_msize.
    DATA l_string TYPE string.
    DATA l_xstring TYPE xstring.
    DATA l_xstring2 TYPE xstring.
    DATA i TYPE i.
    DATA l_remaining_length TYPE i.
    DATA l_inused TYPE i.

    check_bom( ).

    l_remaining_length = length.

    DO. "jusqu'à ce que le nombre de caractères demandés soit lu

* extraire les caractères du buffer de caractères
      IF STRLEN( ai_buffer_c ) < l_remaining_length.
*       cas où le buffer ne contient pas assez de caractères

*       1) ajouter au résultat tous les caractères du buffer
        CONCATENATE result ai_buffer_c INTO result.
        CLEAR ai_buffer_c.
*       2) re-remplir le buffer de caractères en complétant le
*         buffer d'octets et les convertir en caractères
        IF abap_false = aio_x_reader->data_available( ).
*         zut il n'y a plus rien!
          EXIT.
        ENDIF.
        l_xstring = read_bytes( ai_buffer_size ).
        l_bytes = XSTRLEN( l_xstring ).
        ADD l_bytes TO ai_offset.
*         Convertir en caractères
        CLEAR l_string.
        CALL METHOD aio_conv_obj->convert
          EXPORTING
            inbuff    = l_xstring
            outbufflg = length "nombre de caractères à obtenir
          IMPORTING
            outbuff   = l_string "caractères décodés
            inused    = l_inused. "combien d'octets correspondent aux caractères décodés
        CONCATENATE ai_buffer_c l_string INTO ai_buffer_c.

      ELSE.
*       cas où le buffer contient assez de caractères
        CONCATENATE result ai_buffer_c(l_remaining_length) INTO result.
*       enlever ces caractères du buffer
        IF STRLEN( ai_buffer_c ) = l_remaining_length.
          CLEAR ai_buffer_c.
        ELSE.
          ai_buffer_c = ai_buffer_c+l_remaining_length.
        ENDIF.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD read_bytes.
    check_bom( ).
    result = aio_x_reader->read( length ).
  ENDMETHOD.

*  METHOD read_internal.
*  ENDMETHOD.

  METHOD close.
    aio_x_reader->close( ).
  ENDMETHOD.
  METHOD is_closed.
    closed = aio_x_reader->is_closed( ).
  ENDMETHOD.
  METHOD data_available.
    available = aio_x_reader->data_available( ).
  ENDMETHOD.
  METHOD is_x_reader.
    result = abap_false.
  ENDMETHOD.
  METHOD set_mark.
    aio_x_reader->set_mark( ).
  ENDMETHOD.
  METHOD delete_mark.
    aio_x_reader->delete_mark( ).
  ENDMETHOD.
  METHOD is_mark_supported.
    res = aio_x_reader->is_mark_supported( ).
  ENDMETHOD.
  METHOD is_reset_supported.
    result = aio_x_reader->is_reset_supported( ).
  ENDMETHOD.
  METHOD reset_to_mark.
    aio_x_reader->reset_to_mark( ).
  ENDMETHOD.
  METHOD skip.
    aio_x_reader->skip( length ).
  ENDMETHOD.
  METHOD reset.
    aio_x_reader->reset( ).
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------
CLASS zcl_io_filter_x2c_c_reader IMPLEMENTATION.
  METHOD constructor.
    aio_x_reader = io_x_reader.
    ai_encoding  = i_encoding.
  ENDMETHOD.
  METHOD zif_io_reader~read.
  ENDMETHOD.
  METHOD read.
    DATA l_bytes TYPE abap_msize.
    DATA l_xstring TYPE xstring.
    l_bytes = length * cl_abap_char_utilities=>charsize.
    l_xstring = aio_x_reader->read( l_bytes ).
  ENDMETHOD.
  METHOD close.
    aio_x_reader->close( ).
  ENDMETHOD.
  METHOD is_closed.
    aio_x_reader->is_closed( ).
  ENDMETHOD.
  METHOD data_available.
    aio_x_reader->data_available( ).
  ENDMETHOD.
  METHOD is_x_reader.
    result = abap_false.
  ENDMETHOD.
  METHOD set_mark.
    aio_x_reader->set_mark( ).
  ENDMETHOD.
  METHOD delete_mark.
    aio_x_reader->delete_mark( ).
  ENDMETHOD.
  METHOD is_mark_supported.
    aio_x_reader->is_mark_supported( ).
  ENDMETHOD.
  METHOD is_reset_supported.
    aio_x_reader->is_reset_supported( ).
  ENDMETHOD.
  METHOD reset_to_mark.
    aio_x_reader->reset_to_mark( ).
  ENDMETHOD.
  METHOD skip.
    aio_x_reader->skip( length ).
  ENDMETHOD.
  METHOD reset.
    aio_x_reader->reset( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_msdos IMPLEMENTATION.

  METHOD from_date. " IMPORTING msdos_date TYPE i RETURNING value(date) TYPE d

*   MS-DOS format for date:
*     Bits 15:9 = year - 1980
*     Bits 8:5 = month of year
*     Bits 4:0 = day of month

    CONSTANTS: mfe00(2) TYPE x VALUE 'FE00',
               m01e0(2) TYPE x VALUE '01E0',
               m001f(2) TYPE x VALUE '001F'.

    DATA: x(2)  TYPE x,
          year  TYPE i,
          month TYPE i,
          day   TYPE i,
          c4(4) TYPE c,
          str   TYPE string.

*   Bits 15:9 = year - 1980
    x     = msdos_date.
    x     = x BIT-AND mfe00.
    x     = x DIV 512.                                      " >> 9
    x     = x BIT-AND m001f.
    year  = x.
    year  = year + 1980.
    WRITE year TO c4 USING EDIT MASK 'RR____'.
    CONCATENATE str c4 INTO str.

*     Bits 8:5 = month of year
    x     = msdos_date.
    x     = x BIT-AND m01e0.
    x     = x DIV 32.                                       " >> 5
    x     = x BIT-AND m001f.
    month = x.
    WRITE month TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*     Bits 4:0 = day of month
    x     = msdos_date.
    x     = x BIT-AND m001f.
    day   = x.
    WRITE day TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Build date
    TRANSLATE str USING ' 0'.
    date = str.

  ENDMETHOD.


  METHOD from_time. " IMPORTING msdos_time TYPE i RETURNING value(time) TYPE t.

*   MS-DOS format for time:
*     Bits 15:11 = hour   (24-hour clock)
*     Bits 10:5 = minute
*     Bits 4:0 = second/2

    CONSTANTS: mf100(2) TYPE x VALUE 'F100',
               m07e0(2) TYPE x VALUE '07E0',
               m003f(2) TYPE x VALUE '003F',
               m001f(2) TYPE x VALUE '001F'.

    DATA: x(2)  TYPE x,
          hour  TYPE i,
          min   TYPE i,
          c4(4) TYPE c,
          str   TYPE string.

*   Bits 15:11 = hour (24-hour clock)
    x     = msdos_time.
    x     = x BIT-AND mf100.
    x     = x DIV 2048.                                     " >> 11
    x     = x BIT-AND m001f.
    hour  = x.
    WRITE hour TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Bits 10:5 = minute
    x     = msdos_time.
    x     = x BIT-AND m07e0.
    x     = x DIV 32.                                       " >> 5
    x     = x BIT-AND m003f.
    min   = x.
    WRITE min TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Bits 4:0 = second/2
    CONCATENATE str '00' INTO str.

*   Build time
    TRANSLATE str USING ' 0'.
    time = str.

  ENDMETHOD.


  METHOD to_date. " IMPORTING date TYPE d RETURNING value(msdos_date) TYPE i.

*   MS-DOS format for date:
*     Bits 15:9 = year - 1980
*     Bits 8:5 = month of year
*     Bits 4:0 = day of month

    DATA: xdate(2) TYPE x,
          x(2)     TYPE x,
          year     TYPE i,
          month    TYPE i,
          day      TYPE i.

*   Bits 15:9 = year - 1980
    year  = date+0(4).
    x     = year - 1980.
    x     = x * 512.                                        " << 9
    xdate = xdate BIT-OR x.

*   Bits 8:5 = month of year
    month = date+4(2).
    x     = month.
    x     = x * 32.                                         " << 5
    xdate = xdate BIT-OR x.

*   Bits 4:0 = day of month
    day   = date+6(2).
    x     = day.
    xdate = xdate BIT-OR x.

    msdos_date = xdate.

  ENDMETHOD.


  METHOD to_time. " IMPORTING time TYPE t RETURNING value(msdos_time) TYPE i.

*   MS-DOS format for time:
*     Bits 15:11 = hour   (24-hour clock)
*     Bits 10:5 = minute
*     Bits 4:0 = second/2

    DATA: xtime(2) TYPE x,
          x(2)     TYPE x,
          hour     TYPE i,
          min      TYPE i,
          sec      TYPE i.

*   Bits 15:11 = hour (24-hour clock)
    hour  = time+0(2).
    x     = hour.
    x     = x * 2048.                                       " << 11
    xtime = xtime BIT-OR x.

*   Bits 10:5 = minute
    min   = time+2(2).
    x     = min.
    x     = x * 32.                                         " << 5
    xtime = xtime BIT-OR x.

*   Bits 4:0 = seconds
    sec   = time+4(2).
    x     = sec / 2.
    xtime = xtime BIT-OR x.

    msdos_time = xtime.

  ENDMETHOD.


ENDCLASS.



*----------------------------------------------------------------------*
CLASS zcl_io_filter_zip IMPLEMENTATION.
  METHOD get_files.
    DATA lo_buff TYPE REF TO zcl_io_filter_buff_x_reader.
    CREATE OBJECT lo_buff
      EXPORTING
        io_x_reader   = io_x_reader
        i_buffer_size = 0. "buffer infini

* Documentation from: http://www.pkware.com/company/standards/appnote/appnote.txt

* Start to decode new ZIP file
    CLEAR:   files, exts.
    REFRESH: files, exts.

* Global offset for moving through file
    DATA: offset  TYPE i.

    DEFINE next.   " move offset
      offset = offset + &1.
    END-OF-DEFINITION.

* DATA: l1(1) TYPE x, h1(1) TYPE x, l2(1) TYPE x, h2(1) TYPE x, xstr TYPE xstring.
    DATA: w2(2) TYPE x, w4(4) TYPE x, xstr TYPE xstring.
    DEFINE read2.  " read two bytes as integer and move offset
*    l1 = zip+offset(1). offset = offset + 1.
*    h1 = zip+offset(1). offset = offset + 1.
*    CONCATENATE h1 l1 INTO xstr IN BYTE MODE.
      buff_stream 2.
      w2     = lo_buff->buffer+offset(2).
      offset = offset + 2.
      concatenate w2+1(1) w2+0(1) into xstr in byte mode.
      &1     = xstr.
    END-OF-DEFINITION.

    DEFINE read4.  " read four bytes as integer and move offset
*    l1 = zip+offset(1). offset = offset + 1.
*    h1 = zip+offset(1). offset = offset + 1.
*    l2 = zip+offset(1). offset = offset + 1.
*    h2 = zip+offset(1). offset = offset + 1.
*    CONCATENATE h2 l2 h1 l1 INTO xstr IN BYTE MODE.
      buff_stream 4.
      w4     = lo_buff->buffer+offset(4).
      offset = offset + 4.
      concatenate w4+3(1) w4+2(1) w4+1(1) w4+0(1) into xstr in byte mode.
      &1     = xstr.
    END-OF-DEFINITION.
    DEFINE buff_stream.
      lo_buff->read( &1 ).
    END-OF-DEFINITION.

* We convert all names from xstring into string
    DATA: conv TYPE REF TO cl_abap_conv_in_ce.
    conv = cl_abap_conv_in_ce=>create( ).

* The maximum length of the ZIP file for scanning.
    DATA: max_length TYPE i.
    DATA l_ref TYPE REF TO i. "(car l_highest_integer est de type i)
    l_ref ?= cl_abap_exceptional_values=>get_max_value( in = max_length ).
    max_length = l_ref->*.
*  max_length = XSTRLEN( zip ) - 4.

* Extract information about all files.
    DATA: msdos_date TYPE i, msdos_time TYPE i, file_no TYPE i VALUE 0.
    FIELD-SYMBOLS:  <file> TYPE t_file,
                    <ext>  TYPE t_ext.

    buff_stream 4.
    WHILE offset < max_length AND lo_buff->buffer+offset(4) = '504B0304'.  " local file header signature

      file_no = file_no + 1.
      APPEND INITIAL LINE TO files ASSIGNING <file>.
      APPEND INITIAL LINE TO exts  ASSIGNING <ext>.

      next  4.                          " local file header signature
      read2 <ext>-min_extract_version.  " version needed to extract = 2.0 - File is compressed using Deflate
      read2 <ext>-gen_flags.            " general purpose bit flag
      read2 <ext>-compressed.           " compression method: deflated
      read2 msdos_time.                 " last mod file time
      read2 msdos_date.                 " last mod file date
      read4 <ext>-crc32.                                    " crc-32
      read4 <ext>-compsize.             " compressed size
      read4 <file>-size.                " uncompressed size
      read2 <ext>-filename_len.         " file name length
      read2 <ext>-extra_len.            " extra field length

      buff_stream <ext>-filename_len.
      <ext>-filename = lo_buff->buffer+offset(<ext>-filename_len).
      conv->convert( EXPORTING input = <ext>-filename IMPORTING data = <file>-name ).
      next <ext>-filename_len.

      buff_stream <ext>-extra_len.
      <ext>-extra = lo_buff->buffer+offset(<ext>-extra_len).
      next <ext>-extra_len.

      IF <ext>-gen_flags <> 8.

        buff_stream <ext>-compsize.
        <ext>-content = lo_buff->buffer+offset(<ext>-compsize).
        next <ext>-compsize.

      ELSE.

        DATA   result_tab TYPE match_result_tab.
        FIELD-SYMBOLS <match> LIKE LINE OF result_tab.
* pas le choix il faut lire d'avance tout le fichier zip!
        lo_buff->read( max_length ).
        FIND ALL OCCURRENCES OF <ext>-filename IN lo_buff->buffer RESULTS result_tab IN BYTE MODE.
*     Loop till the end of the result_tab to get the entry from the Central Directory
        LOOP AT result_tab ASSIGNING <match>.
        ENDLOOP .
        DATA: cached_offset TYPE i. cached_offset = offset. offset = <match>-offset - 30.

        read4 <ext>-crc32.
        read4 <ext>-compsize.
        read4 <file>-size.
        next 18.
        offset = cached_offset.
        buff_stream <ext>-compsize.
        <ext>-content = lo_buff->buffer+offset(<ext>-compsize).
        next <ext>-compsize.
        next 16.                                            " I032850

      ENDIF.

      <file>-time = zcl_io_msdos=>from_time( msdos_time ).
      <file>-date = zcl_io_msdos=>from_date( msdos_date ).

      CONSTANTS: gen_flags_encrypted(2) TYPE x VALUE '0001'.
      DATA:      gen_flags(2) TYPE x.
      gen_flags = <ext>-gen_flags.
      gen_flags = gen_flags BIT-AND gen_flags_encrypted.

      IF NOT ( <ext>-min_extract_version <= 20 )
*   OR NOT ( <ext>-gen_flags = 0  OR <ext>-gen_flags = 2 OR <ext>-gen_flags = 8 )
      OR     ( gen_flags = gen_flags_encrypted )
      OR NOT ( <ext>-compressed = 0 OR <ext>-compressed = 8 ).
        RAISE zip_parse_error.                            "#EC RAISE_OK
      ENDIF.

      buff_stream 4.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.

CLASS zcl_io_test DEFINITION FOR TESTING      "#AU Risk_Level Harmless
      INHERITING FROM cl_aunit_assert.     "#AU Duration   Short
  PUBLIC SECTION.
    METHODS test_reader
          IMPORTING
            io_reader TYPE REF TO zif_io_reader.
    METHODS test_writer
          IMPORTING
            io_writer TYPE REF TO zif_io_writer.
    METHODS test_c_reader
          IMPORTING
            io_c_reader TYPE REF TO zif_io_c_reader.
    METHODS test_c_writer
          IMPORTING
            io_c_writer TYPE REF TO zif_io_c_writer.
    METHODS test_x_reader
          IMPORTING
            io_x_reader TYPE REF TO zif_io_x_reader.
    METHODS test_x_writer
          IMPORTING
            io_x_writer TYPE REF TO zif_io_x_writer.

    METHODS test_string_c   FOR TESTING.
    METHODS test_backend_c  FOR TESTING.
    METHODS test_gzip       FOR TESTING.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS zcl_io_test IMPLEMENTATION.

  METHOD test_c_writer.
    DO 10 TIMES.
      io_c_writer->write( 'a' ).
    ENDDO.
    io_c_writer->close( ).
  ENDMETHOD.

  METHOD test_c_reader.
    DATA snippet TYPE c LENGTH 1.
    io_c_reader->skip( 3 ).
    IF abap_true = io_c_reader->is_mark_supported( ).
    ENDIF.
    IF abap_true = io_c_reader->is_reset_supported( ).
    ENDIF.
    WHILE abap_true = io_c_reader->data_available( ).
      snippet = io_c_reader->read( 1 ).
      cl_aunit_assert=>assert_equals( act = snippet exp = 'a' ).
    ENDWHILE.
    io_c_reader->close( ).
  ENDMETHOD.

  METHOD test_x_reader.
  ENDMETHOD.

  METHOD test_x_writer.
  ENDMETHOD.

  METHOD test_reader.
    DATA lo_c_reader TYPE REF TO zif_io_c_reader.
    DATA lo_x_reader TYPE REF TO zif_io_x_reader.
    IF abap_true = io_reader->is_x_reader( ).
      lo_x_reader ?= io_reader.
      CALL METHOD test_x_reader
        EXPORTING
          io_x_reader = lo_x_reader.
    ELSE.
      lo_c_reader ?= io_reader.
      CALL METHOD test_c_reader
        EXPORTING
          io_c_reader = lo_c_reader.
    ENDIF.
  ENDMETHOD.

  METHOD test_writer.
  ENDMETHOD.

  METHOD test_string_c.
    DATA lo_c_writer TYPE REF TO zcl_io_string_c_writer.
    DATA lo_c_reader TYPE REF TO zcl_io_string_c_reader.
    DATA l_string TYPE string.
    DATA l_dummy TYPE string.

    CREATE OBJECT lo_c_writer
      EXPORTING
        str = l_string.
    CALL METHOD test_writer
      EXPORTING
        io_writer = lo_c_writer.
    l_dummy = lo_c_writer->get_result_string( ).
    cl_aunit_assert=>assert_equals( act = l_dummy exp = l_string ).

    CREATE OBJECT lo_c_reader
      EXPORTING
        str = l_string.
    CALL METHOD test_reader
      EXPORTING
        io_reader = lo_c_reader.
  ENDMETHOD.

  METHOD test_backend_c.
    DATA lo_file TYPE REF TO zcl_io_backend.
    DATA lo_c_writer TYPE REF TO zcl_io_backend_c_writer.
    DATA lo_c_reader TYPE REF TO zcl_io_backend_c_reader.
    DATA l_string TYPE string.

" write file
    CREATE OBJECT lo_file
      EXPORTING
        filename    = 'test.txt'
        mode        = zcl_io_backend=>cs_mode-text
        access_type = zcl_io_backend=>cs_access_type-output.

    CREATE OBJECT lo_c_writer "cela exécute d'abord: io_file->open( )
          EXPORTING
            io_file = lo_file.
    CALL METHOD test_c_writer "close( ) du stream exécute close( ) du fichier
          EXPORTING
            io_c_writer = lo_c_writer.

" read file
    CREATE OBJECT lo_file
      EXPORTING
        filename    = 'test.txt'
        mode        = zcl_io_backend=>cs_mode-text
        access_type = zcl_io_backend=>cs_access_type-input.
    CREATE OBJECT lo_c_reader
      EXPORTING
        io_file = lo_file.
    CALL METHOD test_c_reader
      EXPORTING
        io_c_reader = lo_c_reader.

  ENDMETHOD.

  METHOD test_gzip.
    DATA lo_gzip_x_writer TYPE REF TO zcl_io_filter_gzip_x_writer.
    DATA lo_gzip_x_reader TYPE REF TO zcl_io_filter_gzip_x_reader.
    DATA lo_gzip_c_writer TYPE REF TO zcl_io_filter_gzip_c_writer.
    DATA lo_gzip_c_reader TYPE REF TO zcl_io_filter_gzip_c_reader.
    DATA lo_string_x_writer TYPE REF TO zcl_io_string_x_writer.
    DATA lo_string_x_reader TYPE REF TO zcl_io_string_x_reader.
    DATA l_string TYPE string.
    DATA l_xstring TYPE xstring.

    CREATE OBJECT lo_string_x_writer
      EXPORTING
        xstr = l_xstring.

    CREATE OBJECT lo_gzip_x_writer
      EXPORTING
        io_x_writer = lo_string_x_writer
        i_buffer_size = 1000.
    CALL METHOD test_writer
      EXPORTING
        io_writer = lo_gzip_x_writer.
    ASSERT l_xstring = ''. "valeur zippée

    CREATE OBJECT lo_gzip_c_writer
      EXPORTING
        io_x_writer = lo_string_x_writer
        i_buffer_size = 1000.
    lo_gzip_c_writer->write( 'AAA' ).


    CREATE OBJECT lo_string_x_reader
      EXPORTING
        str = ``
        xstr = l_xstring.
    CREATE OBJECT lo_gzip_x_reader
      EXPORTING
        io_x_reader = lo_string_x_reader "containing GZIPped data
        i_gzip_buffer = 1000.
    l_xstring = lo_gzip_x_reader->read( 100 ). "lire 100 octets décompressés

    CREATE OBJECT lo_gzip_c_reader
      EXPORTING
        io_x_reader = lo_string_x_reader
        i_gzip_buffer = 1000.
    l_string = lo_gzip_c_reader->read( 100 ). "lire 100 caractères décompressés

  ENDMETHOD.

ENDCLASS.
