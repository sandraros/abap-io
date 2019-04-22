class ZCL_IO_FILTER_ZIP definition
  public
  create public .

public section.

  types:
    BEGIN OF t_file,
             name TYPE string,
             date TYPE d,
             time TYPE t,
             size TYPE i,
           END OF t_file .
  types:
    t_files TYPE STANDARD TABLE OF t_file WITH DEFAULT KEY .

  class-methods GET_FILES
    importing
      !IO_X_READER type ref to ZIF_IO_X_READER
    returning
      value(FILES) type T_FILES
    exceptions
      ZIP_PARSE_ERROR .
protected section.
private section.

  types:
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
  types:
    t_exts TYPE TABLE OF t_ext .

  class-data EXTS type T_EXTS .
ENDCLASS.



CLASS ZCL_IO_FILTER_ZIP IMPLEMENTATION.


  method GET_FILES.

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


  endmethod.
ENDCLASS.
