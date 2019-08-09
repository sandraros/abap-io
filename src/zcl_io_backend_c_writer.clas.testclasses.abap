*"* use this source file for your ABAP unit test classes
*  METHOD test_backend_c.
*
*    DATA l_string TYPE string.
*
*    "==================
*    " write file
*    "==================
*    DATA(lo_file) = NEW zcl_io_backend(
*        filename    = 'test.txt'
*        mode        = zcl_io_backend=>cs_mode-text
*        access_type = zcl_io_backend=>cs_access_type-output ).
*
*    " it first executes IO_FILE->OPEN( )
*    DATA(lo_c_writer) = NEW zcl_io_backend_c_writer( lo_file ).
*
*    " stream->close( ) does a close( ) of the file
*    test_c_writer( lo_c_writer ).
*
*    "==================
*    " read file
*    "==================
*    lo_file = NEW zcl_io_backend(
*        filename    = 'test.txt'
*        mode        = zcl_io_backend=>cs_mode-text
*        access_type = zcl_io_backend=>cs_access_type-input ).
*
*    DATA(lo_c_reader) = NEW zcl_io_backend_c_reader( lo_file ).
*
*    test_c_reader( lo_c_reader ).
*
*  ENDMETHOD.
