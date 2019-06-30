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

    DATA(demo_lob_table) = VALUE demo_lob_table( idx = 1 clob1 = sy-abcde ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).
    DATA std_reader TYPE REF TO cl_abap_db_c_reader.
    SELECT SINGLE clob1 FROM demo_lob_table INTO std_reader WHERE idx = 1.

    test_c_reader( NEW zcl_io_db_c_reader( std_reader ) ).

    ROLLBACK WORK.
  ENDMETHOD.
ENDCLASS.
