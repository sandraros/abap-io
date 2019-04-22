class ZCL_IO_MISC_XC_READER definition
  public
  create public

  global friends ZCL_IO_X_READER .

public section.

  interfaces ZIF_IO_CLOSE_RESOURCE .
  interfaces ZIF_IO_READER .
  interfaces ZIF_IO_X_READER .

  aliases CLOSE
    for ZIF_IO_X_READER~CLOSE .
  aliases DATA_AVAILABLE
    for ZIF_IO_X_READER~DATA_AVAILABLE .
  aliases DELETE_MARK
    for ZIF_IO_X_READER~DELETE_MARK .
  aliases IS_CLOSED
    for ZIF_IO_X_READER~IS_CLOSED .
  aliases IS_MARK_SUPPORTED
    for ZIF_IO_X_READER~IS_MARK_SUPPORTED .
  aliases IS_RESET_SUPPORTED
    for ZIF_IO_X_READER~IS_RESET_SUPPORTED .
  aliases IS_X_READER
    for ZIF_IO_X_READER~IS_X_READER .
  aliases READ_BYTES
    for ZIF_IO_X_READER~READ .
  aliases RESET
    for ZIF_IO_X_READER~RESET .
  aliases RESET_TO_MARK
    for ZIF_IO_X_READER~RESET_TO_MARK .
  aliases SET_MARK
    for ZIF_IO_X_READER~SET_MARK .
  aliases SKIP
    for ZIF_IO_X_READER~SKIP .

  types TY_U_UTF_BOM type I .

  constants:
    BEGIN OF utf_bom,
                  not_found TYPE ty_u_utf_bom VALUE 1,
                  utf_8     TYPE ty_u_utf_bom VALUE 2,
                  utf_16_le TYPE ty_u_utf_bom VALUE 3,
                  utf_16_be TYPE ty_u_utf_bom VALUE 4,
                END OF utf_bom .

  events UTF_BOM_INFORMATION
    exporting
      value(UTF_BOM) type TY_U_UTF_BOM .

  methods CONSTRUCTOR
    importing
      !IO_X_READER type ref to ZIF_IO_X_READER
      !I_ENCODING type CPCODEPAGE optional
      !I_UTF_BOM type ABAP_BOOL default ABAP_TRUE
      !I_REPL_CHAR type CHAR1 default '#'
      !I_LINEFEED_MODE type DSET_CHANGEABLE_ATTRIBUTES-LINEFEED_MODE optional .
  methods SET_UTF_BOM
    importing
      !I_UTF_BOM type ABAP_BOOL .
  methods SET_ENCODING
    importing
      !I_ENCODING type CPCODEPAGE .
  methods SET_REPL_CHAR
    importing
      !I_REPL_CHAR type CHAR1 .
  methods SET_LINEFEED_MODE
    importing
      !I_LINEFEED_MODE type DSET_CHANGEABLE_ATTRIBUTES-LINEFEED_MODE .
  methods CHECK_BOM .
  methods GET_BYTE_OFFSET
    importing
      !I_STRING type CSEQUENCE
      !I_OFFSET type I
      !I_CODEPAGE type CPCODEPAGE
    returning
      value(RESULT) type I .
  methods READ_CHARS
    importing
      !LENGTH type NUMERIC
    returning
      value(RESULT) type STRING
    raising
      ZCX_IO_PARAMETER_INVALID_RANGE
      ZCX_IO_RESOURCE_ALREADY_CLOSED
      ZCX_IO_STREAM_ERROR .
protected section.
private section.

  data AIO_X_READER type ref to ZCL_IO_FILTER_BUFF_X_READER .
  data AI_CODEPAGE type CPCODEPAGE .
  data AI_OFFSET type I .
  data AI_BUFFER_C type STRING .
  data AI_BUFFER_SIZE type I value 10000 ##NO_TEXT.
  data AIO_CONV_OBJ type ref to CL_ABAP_CONV_OBJ .
  data AIO_CONV_OUT type ref to CL_ABAP_CONV_OUT_CE .
  data AI_UTF_BOM type ABAP_BOOL .
  data AI_REPL_CHAR type CHAR1 .
  data AI_LINEFEED_MODE type DSET_CHANGEABLE_ATTRIBUTES-LINEFEED_MODE .
ENDCLASS.



CLASS ZCL_IO_MISC_XC_READER IMPLEMENTATION.


  method CHECK_BOM.

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

  endmethod.


  method CONSTRUCTOR.

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

  endmethod.


  method GET_BYTE_OFFSET.

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

  endmethod.


  method READ_CHARS.

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

  endmethod.


  method SET_ENCODING.

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

  endmethod.


  method SET_LINEFEED_MODE.

    ai_linefeed_mode = i_linefeed_mode.

  endmethod.


  method SET_REPL_CHAR.

    ai_repl_char = i_repl_char.

  endmethod.


  method SET_UTF_BOM.

    ai_utf_bom = i_utf_bom.

  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~CLOSE.
  endmethod.


  method ZIF_IO_CLOSE_RESOURCE~IS_CLOSED.
  endmethod.


  method ZIF_IO_READER~DATA_AVAILABLE.
  endmethod.


  method ZIF_IO_READER~DELETE_MARK.
  endmethod.


  method ZIF_IO_READER~IS_MARK_SUPPORTED.
  endmethod.


  method ZIF_IO_READER~IS_RESET_SUPPORTED.
  endmethod.


  method ZIF_IO_READER~IS_X_READER.
  endmethod.


  method ZIF_IO_READER~READ.


  endmethod.


  method ZIF_IO_READER~RESET.
  endmethod.


  method ZIF_IO_READER~RESET_TO_MARK.
  endmethod.


  method ZIF_IO_READER~SET_MARK.
  endmethod.


  method ZIF_IO_READER~SKIP.
  endmethod.


  method ZIF_IO_X_READER~READ.
  endmethod.
ENDCLASS.
