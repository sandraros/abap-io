"! <p class="shorttext synchronized" lang="en">back-end file character reader</p>
CLASS zcl_io_backend_c_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_file_c_reader
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_backend_reader .

    METHODS constructor
      IMPORTING
        !io_file TYPE REF TO zcl_io_backend
      RAISING
        zcx_io_parameter_invalid .

    METHODS zif_io_reader~delete_mark
        REDEFINITION .
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
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA aio_file TYPE REF TO zcl_io_backend .
    DATA m_mark TYPE abap_msize VALUE -1.
    DATA m_position TYPE abap_msize VALUE 0.
    DATA available TYPE flag VALUE abap_true ##NO_TEXT.

    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        VALUE(length) TYPE abap_msize
      RETURNING
        VALUE(result) TYPE string .
ENDCLASS.



CLASS zcl_io_backend_c_reader IMPLEMENTATION.


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
      WHEN dset_text_mode
        OR dset_legacy_text_mode.
      WHEN OTHERS.
        " If non-Unicode program, read characters in LEGACY BINARY is allowed
        IF abap_false = zcl_io_program=>is_unicode_program( sy-repid ) AND ls_attr-fixed-mode = dset_legacy_binary_mode.
        ELSE.
          RAISE EXCEPTION TYPE zcx_io_parameter_invalid
            EXPORTING
              parameter = `IO_FILE`.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD data_available_internal.

    available = me->available.

  ENDMETHOD.


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


  METHOD read_internal.

    DATA l_characters TYPE i.
    DATA l_pos TYPE i.
    DATA l_length TYPE i.
    DATA l_string TYPE string.
    DATA l_length2 TYPE i.
    DATA off TYPE i.
    DATA len TYPE i.
    DATA l_subrc TYPE sysubrc.

    " réserver l'espace (performance)
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
        EXIT. "on a atteint le nombre de caractÃ¨res demandÃ©
      ENDIF.

" ajout character de fin de ligne
" note: inutile de se complexifier la vie avec CRLF car Windows considÃ¨re un
" LF simple tout de mÃªme comme un retour chariot!
      REPLACE SECTION OFFSET off LENGTH 1 OF result
            WITH cl_abap_char_utilities=>newline IN CHARACTER MODE.
      ADD 1 TO off.
" calculer le nombre de caractÃ¨res restant Ã  lire
      SUBTRACT 1 FROM l_length.
      SUBTRACT l_characters FROM l_length.

    ENDDO.

" couper les caractÃ¨res initialement rÃ©servÃ©s, mais non alimentÃ©s
" (note: il ne faut pas supprimer les espaces Ã  la fin, on risquerait
" de supprimer ceux qui ont rÃ©ellement Ã©tÃ© lus)
    len = length - off.
    IF len > 0.
      REPLACE SECTION OFFSET off LENGTH len OF result WITH `` IN CHARACTER MODE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_io_reader~reset.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    SET DATASET aio_file->filename POSITION zcl_io_backend=>cs_position-begin_of_file.
    available = abap_true.

  ENDMETHOD.


  METHOD zif_io_reader~reset_to_mark.

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


  METHOD zif_io_reader~set_mark.

    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    GET DATASET aio_file->filename POSITION m_mark.

  ENDMETHOD.

ENDCLASS.
