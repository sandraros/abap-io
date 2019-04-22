class ZCX_IO_FREETEXT definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  data TEXT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXT type CLIKE
      !PREVIOUS type ref to CX_ROOT optional .

  methods GET_TEXT
    redefinition .
  methods GET_LONGTEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_IO_FREETEXT IMPLEMENTATION.


  method CONSTRUCTOR.

    CALL METHOD super->constructor
          EXPORTING
            previous = previous.
    me->text = text.

  endmethod.


  method GET_LONGTEXT.

* result = 'No details as the message is built as a "free text"'(001).
    DATA lo_t100_message TYPE REF TO cl_t100_message.
    CREATE OBJECT lo_t100_message
      EXPORTING
        the_msg_class  = 'Z1' "message which explains the free text exception concept
        the_msg_number = '001'.
    result = lo_t100_message->get_longtext( ).

  endmethod.


  method GET_TEXT.

    DATA lo_freetext_message TYPE REF TO cl_freetext_message.
    CREATE OBJECT lo_freetext_message
      EXPORTING
        the_raw_text = text
        the_subject  = me.
    result = lo_freetext_message->get_text( ).

  endmethod.
ENDCLASS.
