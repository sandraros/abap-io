"! <p class="shorttext synchronized" lang="en">Close Resource</p>
"!
INTERFACE zif_io_close_resource
  PUBLIC .

  TYPE-POOLS abap .

  METHODS close
    RAISING
      zcx_io_close_resource_error .
  METHODS is_closed
    RETURNING
      VALUE(closed) TYPE abap_bool .
ENDINTERFACE.
