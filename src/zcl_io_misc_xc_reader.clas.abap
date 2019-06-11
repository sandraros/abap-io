"! <p class="shorttext synchronized" lang="en">Mix of byte and character reader</p>
CLASS zcl_io_misc_xc_reader DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_x_reader .

  PUBLIC SECTION.

    INTERFACES zif_io_close_resource .
    INTERFACES zif_io_reader .
    INTERFACES zif_io_x_reader .

    ALIASES close
      FOR zif_io_x_reader~close .
    ALIASES data_available
      FOR zif_io_x_reader~data_available .
    ALIASES delete_mark
      FOR zif_io_x_reader~delete_mark .
    ALIASES is_closed
      FOR zif_io_x_reader~is_closed .
    ALIASES is_mark_supported
      FOR zif_io_x_reader~is_mark_supported .
    ALIASES is_reset_supported
      FOR zif_io_x_reader~is_reset_supported .
    ALIASES is_x_reader
      FOR zif_io_x_reader~is_x_reader .
    ALIASES read_bytes
      FOR zif_io_x_reader~read .
    ALIASES reset
      FOR zif_io_x_reader~reset .
    ALIASES reset_to_mark
      FOR zif_io_x_reader~reset_to_mark .
    ALIASES set_mark
      FOR zif_io_x_reader~set_mark .
    ALIASES skip
      FOR zif_io_x_reader~skip .

    TYPES ty_u_utf_bom TYPE i .

    CONSTANTS:
      BEGIN OF utf_bom,
        not_found TYPE ty_u_utf_bom VALUE 1,
        utf_8     TYPE ty_u_utf_bom VALUE 2,
        utf_16_le TYPE ty_u_utf_bom VALUE 3,
        utf_16_be TYPE ty_u_utf_bom VALUE 4,
      END OF utf_bom .

    EVENTS utf_bom_information
      EXPORTING
        VALUE(utf_bom) TYPE ty_u_utf_bom .

    METHODS constructor
      IMPORTING
        !io_x_reader     TYPE REF TO zif_io_x_reader
        !i_encoding      TYPE cpcodepage OPTIONAL
        !i_utf_bom       TYPE abap_bool DEFAULT abap_true
        !i_repl_char     TYPE char1 DEFAULT '#'
        !i_linefeed_mode TYPE dset_changeable_attributes-linefeed_mode OPTIONAL .

    METHODS set_utf_bom
      IMPORTING
        !i_utf_bom TYPE abap_bool .

    METHODS set_encoding
      IMPORTING
        !i_encoding TYPE cpcodepage .

    METHODS set_repl_char
      IMPORTING
        !i_repl_char TYPE char1 .

    METHODS set_linefeed_mode
      IMPORTING
        !i_linefeed_mode TYPE dset_changeable_attributes-linefeed_mode .

    METHODS check_bom .

    METHODS get_byte_offset
      IMPORTING
        !i_string     TYPE csequence
        !i_offset     TYPE i
        !i_codepage   TYPE cpcodepage
      RETURNING
        VALUE(result) TYPE i .

    METHODS read_chars
      IMPORTING
        !length       TYPE numeric
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_io_parameter_invalid_range
        zcx_io_resource_already_closed
        zcx_io_stream_error .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA aio_x_reader TYPE REF TO zcl_io_filter_buff_x_reader .
    DATA ai_codepage TYPE cpcodepage .
    DATA ai_offset TYPE i .
    DATA ai_buffer_c TYPE string .
    DATA ai_buffer_size TYPE i VALUE 10000 ##NO_TEXT.
    DATA aio_conv_obj TYPE REF TO cl_abap_conv_obj .
    DATA aio_conv_out TYPE REF TO cl_abap_conv_out_ce .
    DATA ai_utf_bom TYPE abap_bool .
    DATA ai_repl_char TYPE char1 .
    DATA ai_linefeed_mode TYPE dset_changeable_attributes-linefeed_mode .
ENDCLASS.



CLASS zcl_io_misc_xc_reader IMPLEMENTATION.


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


  METHOD get_byte_offset.

    DATA: l_string  TYPE xstring,
          l_xstring TYPE xstring.

    IF i_offset IS INITIAL.
      result = 0.
    ELSE.
      l_string = i_string(i_offset).
      aio_conv_out->convert( EXPORTING data = l_string
                  IMPORTING buffer = l_xstring ).
      result = xstrlen( l_xstring ).
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

    DO. "jusqu'Ã  ce que le nombre de caractÃ¨res demandÃ©s soit lu

* extraire les caractÃ¨res du buffer de caractÃ¨res
      IF strlen( ai_buffer_c ) < l_remaining_length.
*       cas oÃ¹ le buffer ne contient pas assez de caractÃ¨res

*       1) ajouter au rÃ©sultat tous les caractÃ¨res du buffer
        CONCATENATE result ai_buffer_c INTO result.
        CLEAR ai_buffer_c.
*       2) re-remplir le buffer de caractÃ¨res en complÃ©tant le
*         buffer d'octets et les convertir en caractÃ¨res
        IF abap_false = aio_x_reader->data_available( ).
*         zut il n'y a plus rien!
          EXIT.
        ENDIF.
        l_xstring = read_bytes( ai_buffer_size ).
        l_bytes = xstrlen( l_xstring ).
        ADD l_bytes TO ai_offset.
*         Convertir en caractÃ¨res
        CLEAR l_string.
        CALL METHOD aio_conv_obj->convert
          EXPORTING
            inbuff    = l_xstring
            outbufflg = length "nombre de caractÃ¨res Ã  obtenir
          IMPORTING
            outbuff   = l_string "caractÃ¨res dÃ©codÃ©s
            inused    = l_inused. "combien d'octets correspondent aux caractÃ¨res dÃ©codÃ©s
        CONCATENATE ai_buffer_c l_string INTO ai_buffer_c.

      ELSE.
*       cas oÃ¹ le buffer contient assez de caractÃ¨res
        CONCATENATE result ai_buffer_c(l_remaining_length) INTO result.
*       enlever ces caractÃ¨res du buffer
        IF strlen( ai_buffer_c ) = l_remaining_length.
          CLEAR ai_buffer_c.
        ELSE.
          ai_buffer_c = ai_buffer_c+l_remaining_length.
        ENDIF.
        EXIT.
      ENDIF.
    ENDDO.

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


  METHOD set_utf_bom.

    ai_utf_bom = i_utf_bom.

  ENDMETHOD.


  METHOD zif_io_close_resource~close.
  ENDMETHOD.


  METHOD zif_io_close_resource~is_closed.
  ENDMETHOD.


  METHOD zif_io_reader~data_available.
  ENDMETHOD.


  METHOD zif_io_reader~delete_mark.
  ENDMETHOD.


  METHOD zif_io_reader~is_mark_supported.
  ENDMETHOD.


  METHOD zif_io_reader~is_reset_supported.
  ENDMETHOD.


  METHOD zif_io_reader~is_x_reader.
  ENDMETHOD.


  METHOD zif_io_reader~read.


  ENDMETHOD.


  METHOD zif_io_reader~reset.
  ENDMETHOD.


  METHOD zif_io_reader~reset_to_mark.
  ENDMETHOD.


  METHOD zif_io_reader~set_mark.
  ENDMETHOD.


  METHOD zif_io_reader~skip.
  ENDMETHOD.


  METHOD zif_io_x_reader~read.
  ENDMETHOD.
ENDCLASS.
