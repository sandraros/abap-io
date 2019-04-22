class ZCL_IO_MSDOS definition
  public
  final
  create public .

public section.

  class-methods TO_DATE
    importing
      !DATE type D
    returning
      value(MSDOS_DATE) type I .
  class-methods TO_TIME
    importing
      !TIME type T
    returning
      value(MSDOS_TIME) type I .
  class-methods FROM_DATE
    importing
      !MSDOS_DATE type I
    returning
      value(DATE) type D .
  class-methods FROM_TIME
    importing
      !MSDOS_TIME type I
    returning
      value(TIME) type T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IO_MSDOS IMPLEMENTATION.


  method FROM_DATE.
 " IMPORTING msdos_date TYPE i RETURNING value(date) TYPE d

*   MS-DOS format for date:
*     Bits 15:9 = year - 1980
*     Bits 8:5 = month of year
*     Bits 4:0 = day of month

    CONSTANTS: mfe00(2) TYPE x VALUE 'FE00',
               m01e0(2) TYPE x VALUE '01E0',
               m001f(2) TYPE x VALUE '001F'.

    DATA: x(2)  TYPE x,
          year  TYPE i,
          month TYPE i,
          day   TYPE i,
          c4(4) TYPE c,
          str   TYPE string.

*   Bits 15:9 = year - 1980
    x     = msdos_date.
    x     = x BIT-AND mfe00.
    x     = x DIV 512.                                      " >> 9
    x     = x BIT-AND m001f.
    year  = x.
    year  = year + 1980.
    WRITE year TO c4 USING EDIT MASK 'RR____'.
    CONCATENATE str c4 INTO str.

*     Bits 8:5 = month of year
    x     = msdos_date.
    x     = x BIT-AND m01e0.
    x     = x DIV 32.                                       " >> 5
    x     = x BIT-AND m001f.
    month = x.
    WRITE month TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*     Bits 4:0 = day of month
    x     = msdos_date.
    x     = x BIT-AND m001f.
    day   = x.
    WRITE day TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Build date
    TRANSLATE str USING ' 0'.
    date = str.


  endmethod.


  method FROM_TIME.
 " IMPORTING msdos_time TYPE i RETURNING value(time) TYPE t.

*   MS-DOS format for time:
*     Bits 15:11 = hour   (24-hour clock)
*     Bits 10:5 = minute
*     Bits 4:0 = second/2

    CONSTANTS: mf100(2) TYPE x VALUE 'F100',
               m07e0(2) TYPE x VALUE '07E0',
               m003f(2) TYPE x VALUE '003F',
               m001f(2) TYPE x VALUE '001F'.

    DATA: x(2)  TYPE x,
          hour  TYPE i,
          min   TYPE i,
          c4(4) TYPE c,
          str   TYPE string.

*   Bits 15:11 = hour (24-hour clock)
    x     = msdos_time.
    x     = x BIT-AND mf100.
    x     = x DIV 2048.                                     " >> 11
    x     = x BIT-AND m001f.
    hour  = x.
    WRITE hour TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Bits 10:5 = minute
    x     = msdos_time.
    x     = x BIT-AND m07e0.
    x     = x DIV 32.                                       " >> 5
    x     = x BIT-AND m003f.
    min   = x.
    WRITE min TO c4 USING EDIT MASK 'RR__'.
    CONCATENATE str c4 INTO str.

*   Bits 4:0 = second/2
    CONCATENATE str '00' INTO str.

*   Build time
    TRANSLATE str USING ' 0'.
    time = str.


  endmethod.


  method TO_DATE.
 " IMPORTING date TYPE d RETURNING value(msdos_date) TYPE i.

*   MS-DOS format for date:
*     Bits 15:9 = year - 1980
*     Bits 8:5 = month of year
*     Bits 4:0 = day of month

    DATA: xdate(2) TYPE x,
          x(2)     TYPE x,
          year     TYPE i,
          month    TYPE i,
          day      TYPE i.

*   Bits 15:9 = year - 1980
    year  = date+0(4).
    x     = year - 1980.
    x     = x * 512.                                        " << 9
    xdate = xdate BIT-OR x.

*   Bits 8:5 = month of year
    month = date+4(2).
    x     = month.
    x     = x * 32.                                         " << 5
    xdate = xdate BIT-OR x.

*   Bits 4:0 = day of month
    day   = date+6(2).
    x     = day.
    xdate = xdate BIT-OR x.

    msdos_date = xdate.


  endmethod.


  method TO_TIME.
 " IMPORTING time TYPE t RETURNING value(msdos_time) TYPE i.

*   MS-DOS format for time:
*     Bits 15:11 = hour   (24-hour clock)
*     Bits 10:5 = minute
*     Bits 4:0 = second/2

    DATA: xtime(2) TYPE x,
          x(2)     TYPE x,
          hour     TYPE i,
          min      TYPE i,
          sec      TYPE i.

*   Bits 15:11 = hour (24-hour clock)
    hour  = time+0(2).
    x     = hour.
    x     = x * 2048.                                       " << 11
    xtime = xtime BIT-OR x.

*   Bits 10:5 = minute
    min   = time+2(2).
    x     = min.
    x     = x * 32.                                         " << 5
    xtime = xtime BIT-OR x.

*   Bits 4:0 = seconds
    sec   = time+4(2).
    x     = sec / 2.
    xtime = xtime BIT-OR x.

    msdos_time = xtime.


  endmethod.
ENDCLASS.
