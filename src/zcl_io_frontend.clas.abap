"! <p class="shorttext synchronized" lang="en">Frontend file</p>
"!
CLASS zcl_io_frontend DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_file
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: filename          TYPE string READ-ONLY,
          confirm_overwrite TYPE abap_bool READ-ONLY,
          codepage          TYPE abap_encoding READ-ONLY.

    METHODS constructor
      IMPORTING
        !filename          TYPE string
        !confirm_overwrite TYPE abap_bool DEFAULT abap_true
        !codepage          TYPE abap_encoding OPTIONAL.

    METHODS open
        REDEFINITION .
    METHODS close
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_frontend IMPLEMENTATION.


  METHOD close.


  ENDMETHOD.


  METHOD constructor.

    CALL METHOD super->constructor.
    me->filename = filename.
    me->confirm_overwrite = confirm_overwrite.
    me->codepage = codepage.

  ENDMETHOD.


  METHOD open.


  ENDMETHOD.
ENDCLASS.
