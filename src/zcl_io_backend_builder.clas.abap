"! <p class="shorttext synchronized" lang="en">Back-end file builder</p>
"!
CLASS zcl_io_backend_builder DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA filename TYPE string READ-ONLY .
    DATA attr TYPE dset_attributes READ-ONLY .
    DATA attr2 TYPE zif_io_backend=>ty_attr2 READ-ONLY .

    METHODS constructor
      IMPORTING
        !filename TYPE string .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter backend | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_io_file_error | <p class="shorttext synchronized" lang="en"></p>
    METHODS open
      RETURNING
        VALUE(backend) TYPE REF TO zcl_io_backend
      RAISING
        zcx_io_file_error .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS for_input
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS for_output
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS for_update
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS for_appending
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter encoding |<ul>
    "!                      <li>DEFAULT</li>
    "!                      <li>UTF-8</li>
    "!                      <li>NON-UNICODE</li>
    "!                      </ul>
    "!                      <p class="shorttext synchronized" lang="en"></p>
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS encoding
      IMPORTING
        !encoding      TYPE abap_encod
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS in_text_mode
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS in_binary_mode
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS in_legacy_text_mode
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS in_legacy_binary_mode
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS with_native_linefeed
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS with_smart_linefeed
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS with_unix_linefeed
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS with_windows_linefeed
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS big_endian
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS little_endian
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter position | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter end_of_file | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS at_position
      IMPORTING
        !position      TYPE numeric DEFAULT 0
        !end_of_file   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter code_page | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS code_page
      IMPORTING
        !code_page     TYPE csequence
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter filter | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS filter
      IMPORTING
        !filter        TYPE csequence
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter replacement_character | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS replacement_character
      IMPORTING
        !replacement_character TYPE csequence
      RETURNING
        VALUE(builder)         TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS ignoring_conversion_errors
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS type
      IMPORTING
        !type          TYPE csequence
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS skipping_byte_order_mark
      IMPORTING
        !type          TYPE csequence
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter type | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter builder | <p class="shorttext synchronized" lang="en"></p>
    METHODS with_byte_order_mark
      IMPORTING
        !type          TYPE csequence
      RETURNING
        VALUE(builder) TYPE REF TO zcl_io_backend_builder .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_backend_builder IMPLEMENTATION.


  METHOD constructor.

    CALL METHOD super->constructor.
    me->filename = filename.
    attr-fixed-encoding = zif_io_backend=>cs_dset-encoding-default.

  ENDMETHOD.


  METHOD open.

    backend = NEW zcl_io_backend(
        filename = filename
        attr     = attr
        attr2    = attr2 ).
    backend->open( ).

  ENDMETHOD.


  METHOD at_position.
    attr2-indicator-position = abap_true.
    IF end_of_file = abap_true.
      attr2-position = -1.
    ELSE.
      attr2-position = position.
    ENDIF.
    builder = me.
  ENDMETHOD.


  METHOD big_endian.
    attr-changeable-indicator-endian = abap_true.
    attr-changeable-endian = dset_big_endian.
    builder = me.
  ENDMETHOD.


  METHOD code_page.
    attr-changeable-code_page = abap_true.
    attr-changeable-code_page = code_page.
    builder = me.
  ENDMETHOD.


  METHOD encoding.
    CASE encoding.
      WHEN zif_io_backend=>cs_dset-encoding-default
        OR zif_io_backend=>cs_dset-encoding-utf_8
        OR zif_io_backend=>cs_dset-encoding-non_unicode
        OR zif_io_backend=>cs_dset-encoding-none.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_io_parameter_invalid_range
          EXPORTING
            parameter = `ENCODING`
            value     = CONV #( encoding ).
    ENDCASE.
    attr-fixed-indicator-encoding = abap_true.
    attr-fixed-encoding = encoding.
    builder = me.
  ENDMETHOD.


  METHOD filter.
    attr-fixed-indicator-filter = abap_true.
    attr-fixed-filter = filter.
    builder = me.
  ENDMETHOD.


  METHOD for_appending.
    attr-fixed-indicator-access_type = abap_true.
    attr-fixed-access_type = dset_appending.
    builder = me.
  ENDMETHOD.


  METHOD for_input.
    attr-fixed-indicator-access_type = abap_true.
    attr-fixed-access_type = dset_input.
    builder = me.
  ENDMETHOD.


  METHOD for_output.
    attr-fixed-indicator-access_type = abap_true.
    attr-fixed-access_type = dset_output.
    builder = me.
  ENDMETHOD.


  METHOD for_update.
    attr-fixed-indicator-access_type = abap_true.
    attr-fixed-access_type = dset_update.
    builder = me.
  ENDMETHOD.


  METHOD ignoring_conversion_errors.
    attr-changeable-indicator-conv_errors = abap_true.
    attr-changeable-conv_errors = dset_ignore_conv_errors.
    builder = me.
  ENDMETHOD.


  METHOD in_binary_mode.
    attr-fixed-indicator-mode = abap_true.
    attr-fixed-mode = dset_binary_mode.
    builder = me.
  ENDMETHOD.


  METHOD in_legacy_binary_mode.
    attr-fixed-indicator-mode = abap_true.
    attr-fixed-mode = dset_legacy_binary_mode.
    builder = me.
  ENDMETHOD.


  METHOD in_legacy_text_mode.
    attr-fixed-indicator-mode = abap_true.
    attr-fixed-mode = dset_legacy_text_mode.
    builder = me.
  ENDMETHOD.


  METHOD in_text_mode.
    attr-fixed-indicator-mode = abap_true.
    attr-fixed-mode = dset_text_mode.
    builder = me.
  ENDMETHOD.


  METHOD little_endian.
    attr-changeable-indicator-endian = abap_true.
    attr-changeable-endian = dset_little_endian.
    builder = me.
  ENDMETHOD.


  METHOD replacement_character.
    attr-changeable-indicator-repl_char = abap_true.
    attr-changeable-repl_char = replacement_character.
    builder = me.
  ENDMETHOD.


  METHOD type.
    attr2-indicator-type = abap_true.
    attr2-type = type.
    builder = me.
  ENDMETHOD.


  METHOD with_native_linefeed.
    attr-changeable-indicator-linefeed_mode = abap_true.
    attr-changeable-linefeed_mode = dset_native_linefeed.
    builder = me.
  ENDMETHOD.


  METHOD with_smart_linefeed.
    attr-changeable-indicator-linefeed_mode = abap_true.
    attr-changeable-linefeed_mode = dset_smart_linefeed.
    builder = me.
  ENDMETHOD.


  METHOD with_unix_linefeed.
    attr-changeable-indicator-linefeed_mode = abap_true.
    attr-changeable-linefeed_mode = dset_unix_linefeed.
    builder = me.
  ENDMETHOD.


  METHOD with_windows_linefeed.
    attr-changeable-indicator-linefeed_mode = abap_true.
    attr-changeable-linefeed_mode = dset_windows_linefeed.
    builder = me.
  ENDMETHOD.


  METHOD skipping_byte_order_mark.
    attr2-indicator-process_utf8_bom = abap_true.
    attr2-process_utf8_bom = abap_true.
    builder = me.
  ENDMETHOD.


  METHOD with_byte_order_mark.
    attr2-indicator-process_utf8_bom = abap_true.
    attr2-process_utf8_bom = abap_false.
    builder = me.
  ENDMETHOD.


ENDCLASS.
