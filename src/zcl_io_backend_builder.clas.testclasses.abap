*"* use this source file for your ABAP unit test classes

class ltc_main definition
      for testing
      duration short
      risk level harmless
      inheriting from cl_aunit_assert.
  private section.
    methods test for testing.
*    class-methods class_setup.
*    class-methods class_teardown.
*    methods setup.
*    methods teardown.
endclass.
class ltc_main implementation.
  method test.
  new zcl_io_backend_builder( filename = '' )->in_text_mode( )->for_input( ).
  endmethod.
endclass.
