CLASS zcl_insert_data_a7 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_insert_data_a7 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA ls_customer TYPE ztcustomer_a7.
    DATA ls_technician TYPE zttechnician_a7.

*    ls_status = VALUE #( status_code = 'PE'
*                        status_description = 'Pending' ).
*    INSERT  ztstatus_a08 FROM @ls_status.
*
*    ls_status = VALUE #( status_code = 'CO'
*                         status_description = 'Completed' ).
*    INSERT  ztstatus_a08 FROM @ls_status.
*
*    ls_priority = VALUE #( priority_code = 'A'
*                         priority_description = 'High' ).
*    INSERT  ztpriority_a08 FROM @ls_priority.

*    ls_priority = VALUE #( priority_code = 'B'
*                         priority_description = 'Low' ).
*    INSERT  ztpriority_a08 FROM @ls_priority.

    ls_customer = VALUE ztcustomer_a7(
  customer_id = '1'
  name        = 'Laura Fernández'
  address     = 'Calle 45 #18-22'
  phone       = '3105678932' ).
    INSERT ztcustomer_a7 FROM @ls_customer.

    ls_customer = VALUE #(
      customer_id = '2'
      name        = 'Carlos Mejía'
      address     = 'Av. Las Palmas #102-33'
      phone       = '3001122334' ).
    INSERT ztcustomer_a7 FROM @ls_customer.

    ls_customer = VALUE #(
      customer_id = '3'
      name        = 'Diana Gómez'
      address     = 'Cl. 10 Sur #55-12'
      phone       = '3119988776' ).
    INSERT ztcustomer_a7 FROM @ls_customer.

    ls_customer = VALUE #(
      customer_id = '4'
      name        = 'Andrés Rodríguez'
      address     = 'Transv. 93 #50-60'
      phone       = '3214455667' ).
    INSERT ztcustomer_a7 FROM @ls_customer.

*    ls_technician = VALUE #( technician_id = '1'
*                         name = 'Jose Ortiz'
*                         phone = '3124197663' ).
*    INSERT  zttechnician_a7 FROM @ls_technician.

*    IF sy-subrc = 0.
*      out->write( ' record inserted correctly' ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
