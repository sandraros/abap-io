"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_io_string_x_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_memory_x_reader
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_x_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_string_reader .

    METHODS constructor
      IMPORTING
*        !str  TYPE string
        !xstr TYPE xsequence OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA aiv_stream_bytes TYPE i .
    DATA aiv_offset TYPE i .
    DATA m_str TYPE xstring .
    DATA m_ref_xstr TYPE REF TO data .
    DATA m_offset TYPE abap_msize VALUE 0 ##NO_TEXT.
    DATA m_mark TYPE abap_msize VALUE -1 ##NO_TEXT.

    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        !length       TYPE abap_msize
      RETURNING
        VALUE(result) TYPE xstring .
ENDCLASS.



CLASS zcl_io_string_x_reader IMPLEMENTATION.


  METHOD constructor.

    CALL METHOD super->constructor.
    m_str = xstr.
    aiv_stream_bytes = xstrlen( m_str ).
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
