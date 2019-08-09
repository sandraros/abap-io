"! <p class="shorttext synchronized" lang="en">Back-end file</p>
INTERFACE zif_io_backend
  PUBLIC .

  INTERFACES zif_io_file.

  TYPE-POOLS dset .

  TYPES: BEGIN OF ty_attr2_indicator,
           position         TYPE abap_bool,
           type             TYPE abap_bool,
           process_utf8_bom TYPE abap_bool,
         END OF ty_attr2_indicator,
         BEGIN OF ty_attr2,
           indicator        TYPE ty_attr2_indicator,
           position         TYPE decfloat16,
           type             TYPE string,
           process_utf8_bom TYPE abap_bool,
         END OF ty_attr2.

  METHODS get_line_end_marker
    RETURNING
      VALUE(result) TYPE string .

  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  METHODS delete
    RAISING
      zcx_io_file_error.

  CONSTANTS:
    BEGIN OF cs_dset,
      BEGIN OF mode,
        text          TYPE dset_fixed_attributes-mode VALUE dset_text_mode,
        binary        TYPE dset_fixed_attributes-mode VALUE dset_binary_mode,
        legacy_text   TYPE dset_fixed_attributes-mode VALUE dset_legacy_text_mode,
        legacy_binary TYPE dset_fixed_attributes-mode VALUE dset_legacy_binary_mode,
      END OF mode,
      BEGIN OF position,
        begin_of_file TYPE i VALUE 0,
        end_of_file   TYPE i VALUE -1,
      END OF position,
      BEGIN OF access_type,
        input     TYPE dset_fixed_attributes-access_type VALUE dset_input,
        output    TYPE dset_fixed_attributes-access_type VALUE dset_output,
        update    TYPE dset_fixed_attributes-access_type VALUE dset_update,
        appending TYPE dset_fixed_attributes-access_type VALUE dset_appending,
      END OF access_type,
      BEGIN OF encoding,
        none        TYPE dset_fixed_attributes-encoding VALUE '',
        default     TYPE dset_fixed_attributes-encoding VALUE 'DEFAULT',
        non_unicode TYPE dset_fixed_attributes-encoding VALUE 'NON-UNICODE',
        utf_8       TYPE dset_fixed_attributes-encoding VALUE 'UTF-8',
      END OF encoding,
    END OF cs_dset.

  DATA filename TYPE string READ-ONLY .
  DATA msg TYPE msg READ-ONLY .
  DATA attr TYPE dset_attributes READ-ONLY.
  DATA attr2 TYPE ty_attr2 READ-ONLY.

ENDINTERFACE.
