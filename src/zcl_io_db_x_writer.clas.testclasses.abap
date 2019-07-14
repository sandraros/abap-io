*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test.
  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD test.

    DATA: demo_lob_table TYPE demo_lob_table WRITER FOR COLUMNS blob1.

    demo_lob_table = VALUE #( idx = 1 ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.

    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 2 act = sy-subrc ).

    test_x_writer( NEW zcl_io_db_x_writer( demo_lob_table-blob1 ) ).

    ROLLBACK WORK.

  ENDMETHOD.

ENDCLASS.
