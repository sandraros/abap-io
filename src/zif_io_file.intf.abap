"! <p class="shorttext synchronized" lang="en">I/O File interface</p>
"!
INTERFACE zif_io_file
  PUBLIC .


  METHODS open
    RAISING
      zcx_io_file_error.
  METHODS close
    RAISING
      zcx_io_file_error.
ENDINTERFACE.
