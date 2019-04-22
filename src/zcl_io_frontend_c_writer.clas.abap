class ZCL_IO_FRONTEND_C_WRITER definition
  public
  inheriting from ZCL_IO_FILE_C_WRITER
  create public

  global friends ZCL_IO_C_WRITER .

public section.

  interfaces ZIF_IO_FRONTEND_WRITER .

  methods CONSTRUCTOR
    importing
      !IO_FILE type ref to ZCL_IO_FRONTEND
      !I_BUFFER_SIZE type I default 0
    raising
      ZCX_IO_PARAMETER_INVALID .
protected section.
private section.

  data AIO_FILE type ref to ZCL_IO_FRONTEND .
  data AI_BUFFER_SIZE type I .
  data M_STR type STRING .

  methods CLOSE_INTERNAL .
  methods WRITE_INTERNAL
    importing
      !DATA type STRING .
ENDCLASS.



CLASS ZCL_IO_FRONTEND_C_WRITER IMPLEMENTATION.


  method CLOSE_INTERNAL.

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

  endmethod.


  method CONSTRUCTOR.

    DATA ls_attr TYPE dset_attributes.

    CALL METHOD super->constructor.
    aio_file = io_file.
    ai_buffer_size = i_buffer_size.

  endmethod.


  method WRITE_INTERNAL.

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

  endmethod.
ENDCLASS.
