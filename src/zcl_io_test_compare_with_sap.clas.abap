"! <p class="shorttext synchronized" lang="en">Compare IO classes with SAP standard streaming classes</p>
"! <strong>Usage example:</strong><br/>
"! REPORT.<br/>
"! <br/>
"! CLASS lcl_app DEFINITION CREATE PUBLIC INHERITING FROM zcl_io_test_compare_with_sap.<br/>
"!   PROTECTED SECTION.<br/>
"!     METHODS: write_line REDEFINITION.<br/>
"! ENDCLASS.<br/>
"!<br/>
"! CLASS lcl_app IMPLEMENTATION.<br/>
"!   METHOD write_line.<br/>
"!     PERFORM write_line USING equiv_class.<br/>
"!   ENDMETHOD.<br/>
"! ENDCLASS.<br/>
"!<br/>
"! DATA equiv_class TYPE zcl_io_test_compare_with_sap=>ty_equiv_class.<br/>
"!<br/>
"! START-OF-SELECTION.<br/>
"!   DATA(APP) = NEW lcl_app( ).<br/>
"!   APP->write_list( ).<br/>
"!<br/>
"! FORM write_line USING i_equiv_class TYPE zcl_io_test_compare_with_sap=>ty_equiv_class.<br/>
"!   equiv_class = i_equiv_class.<br/>
"!   WRITE : / equiv_class-cus_clsname, equiv_class-std_clsname.<br/>
"!   HIDE equiv_class.<br/>
"! ENDFORM.<br/>
"!<br/>
"! AT LINE-SELECTION.<br/>
"!   APP->at_line_selection( equiv_class ).<br/>
"!
CLASS zcl_io_test_compare_with_sap DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.
    TYPES : BEGIN OF ty_equiv_class,
              cus_clsname TYPE seoclsname,
              std_clsname TYPE seoclsname,
              count_i     TYPE i,
              count_d     TYPE i,
              count_u     TYPE i,
              count_total TYPE i,
            END OF ty_equiv_class,
            ty_equiv_classes TYPE STANDARD TABLE OF ty_equiv_class WITH EMPTY KEY.
    TYPES : BEGIN OF ty_sources,
              cus_lines TYPE rswsourcet,
              std_lines TYPE rswsourcet,
            END OF ty_sources.

    METHODS write_list.
    METHODS at_line_selection IMPORTING equiv_class TYPE ty_equiv_class.

  PROTECTED SECTION.
    METHODS write_line ABSTRACT IMPORTING equiv_class TYPE ty_equiv_class.

  PRIVATE SECTION.
    METHODS show_diff_string_tables
      IMPORTING
        sources TYPE ty_sources.
    METHODS create_report
      IMPORTING
        source              TYPE rswsourcet
      RETURNING
        VALUE(program_name) TYPE syrepid.
    METHODS diff
      CHANGING
        equiv_class TYPE zcl_io_test_compare_with_sap=>ty_equiv_class.
    METHODS get_sources
      IMPORTING
        equiv_class    TYPE zcl_io_test_compare_with_sap=>ty_equiv_class
      RETURNING
        VALUE(sources) TYPE ty_sources
      RAISING
        cx_oo_clif_not_exists.
ENDCLASS.



CLASS zcl_io_test_compare_with_sap IMPLEMENTATION.


  METHOD at_line_selection.

    DATA(sources) = get_sources( equiv_class ).

    show_diff_string_tables( sources ).

  ENDMETHOD.


  METHOD create_report.
    CONSTANTS temp_program_prefix TYPE c LENGTH 5 VALUE 'ZTMP_'.
    DATA: l_guid_16 TYPE guid_16,
          l_guid_25 TYPE rssg_uni_idc25,
          ls_trdir  TYPE trdir.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = l_guid_16.
    CALL FUNCTION 'RSS_UNIQUE_CONVERT_TO_ID25'
      EXPORTING
        i_uni_idx         = l_guid_16
      IMPORTING
        e_uni_idc25       = l_guid_25
      EXCEPTIONS
        invalid_unique_id = 1
        OTHERS            = 2.
    CONCATENATE temp_program_prefix l_guid_25 INTO program_name.
    ls_trdir-name = program_name.
    ls_trdir-subc = 'I'.
    INSERT REPORT program_name FROM source.
  ENDMETHOD.


  METHOD diff.
    DATA: abaptext_old      TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY,
          abaptext_new      TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY,
          lt_trdirtab_old   TYPE TABLE OF trdir,
          lt_trdirtab_new   TYPE TABLE OF trdir,
          lt_trdir_delta    TYPE TABLE OF xtrdir,
          lt_abaptext_delta TYPE TABLE OF vxabapt255.

    DATA(sources) = get_sources( equiv_class ).

    abaptext_old = sources-cus_lines.
    abaptext_new = sources-std_lines.
    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      EXPORTING
        compare_mode            = 1
        ignore_case_differences = 'X'
      TABLES
        texttab_old             = abaptext_old
        texttab_new             = abaptext_new
        trdirtab_old            = lt_trdirtab_old
        trdirtab_new            = lt_trdirtab_new
        trdir_delta             = lt_trdir_delta
        text_delta              = lt_abaptext_delta.

    LOOP AT lt_abaptext_delta ASSIGNING FIELD-SYMBOL(<text_delta>).
      CASE <text_delta>-vrsflag.
        WHEN 'D'.
          " line only in TEXTTAB_OLD
          ADD 1 TO equiv_class-count_d.
        WHEN 'I'.
          " line only in TEXTTAB_NEW
          ADD 1 TO equiv_class-count_i.
        WHEN 'U'.
          " line changed
          ADD 1 TO equiv_class-count_u.
      ENDCASE.
    ENDLOOP.

    equiv_class-count_total = lines( abaptext_old ).

  ENDMETHOD.


  METHOD get_sources.

    DATA(settings) = cl_oo_clif_source_settings=>create_instance( ).

    DATA(cus) = cl_oo_clif_source=>create_instance(
        clif_name = equiv_class-cus_clsname
        version   = if_oo_clif_source=>co_version_active
        settings  = settings ).
    cus->get_source( IMPORTING source = sources-cus_lines ).

    DATA(std) = cl_oo_clif_source=>create_instance(
        clif_name = equiv_class-std_clsname
        version   = if_oo_clif_source=>co_version_active
        settings  = settings ).
    std->get_source( IMPORTING source = sources-std_lines ).

    REPLACE ALL OCCURRENCES OF REGEX '^ *"!.*$' IN TABLE sources-cus_lines WITH ``.
    DELETE sources-cus_lines WHERE table_line IS INITIAL.

    CONCATENATE LINES OF sources-cus_lines INTO DATA(cus_lines_as_string) SEPARATED BY cl_abap_char_utilities=>newline.
    CONCATENATE LINES OF sources-std_lines INTO DATA(std_lines_as_string) SEPARATED BY cl_abap_char_utilities=>newline.

    REPLACE ALL OCCURRENCES OF REGEX 'ZCX_IO' IN cus_lines_as_string WITH 'CX' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX 'Z(..)_IO' IN cus_lines_as_string WITH '$1_ABAP' IGNORING CASE.

    FIND ALL OCCURRENCES OF REGEX 'METHOD\s+(\S+)\s+BY KERNEL MODULE'
          IN std_lines_as_string IGNORING CASE RESULTS DATA(matches).
    SORT matches BY offset DESCENDING.
    LOOP AT matches ASSIGNING FIELD-SYMBOL(<match>).
      DATA(submatch) = <match>-submatches[ 1 ].
      DATA(method_name) = std_lines_as_string+submatch-offset(submatch-length).
      REPLACE REGEX 'METHOD\s+' && method_name && '\s(?:(?!ENDMETHOD).)+ENDMETHOD\s*.'
          IN cus_lines_as_string WITH '' IGNORING CASE.
      REPLACE REGEX 'METHOD\s+' && method_name && '\s(?:(?!ENDMETHOD).)+ENDMETHOD\s*.'
          IN std_lines_as_string WITH '' IGNORING CASE.
    ENDLOOP.

    SPLIT cus_lines_as_string AT cl_abap_char_utilities=>newline INTO TABLE sources-cus_lines.
    SPLIT std_lines_as_string AT cl_abap_char_utilities=>newline INTO TABLE sources-std_lines.

    DELETE sources-std_lines WHERE table_line IS INITIAL OR table_line CO space.

  ENDMETHOD.


  METHOD show_diff_string_tables.

    DATA(l_name_left) = create_report( source = sources-cus_lines ).
    DATA(l_name_right) = create_report( source = sources-std_lines ).

    COMMIT WORK.

    SUBMIT rsvrsrs3
          WITH objnam2 = l_name_right
          WITH objname = l_name_left
          WITH objtyp1 = 'REPS'
          WITH objtyp2 = 'REPS'
          WITH versno1 = 0
          WITH versno2 = 0
          AND RETURN.

    DELETE REPORT l_name_left.
    DELETE REPORT l_name_right.
    COMMIT WORK.
  ENDMETHOD.


  METHOD write_list.
    SELECT clsname FROM seoclassdf INTO TABLE @DATA(seoclassdf_s) WHERE clsname LIKE 'ZCL_IO_%' OR clsname LIKE 'ZIF_IO_%'.


    DATA(equiv_classes) = VALUE ty_equiv_classes( ).
    LOOP AT seoclassdf_s INTO DATA(seoclassdf).
      equiv_classes = VALUE #( BASE equiv_classes
      ( cus_clsname = seoclassdf-clsname
        std_clsname = CONV sy-repid( replace( val = seoclassdf-clsname regex = 'Z(..)_IO' with = '$1_ABAP' ) ) ) ).
    ENDLOOP.

    SELECT clsname FROM seoclassdf
    FOR ALL ENTRIES IN @equiv_classes
    WHERE clsname = @equiv_classes-std_clsname
    INTO TABLE @seoclassdf_s.

    LOOP AT equiv_classes ASSIGNING FIELD-SYMBOL(<equiv_class>).
      IF line_exists( seoclassdf_s[ clsname = <equiv_class>-std_clsname ] ).
        diff( CHANGING equiv_class = <equiv_class> ).
        write_line( <equiv_class> ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
