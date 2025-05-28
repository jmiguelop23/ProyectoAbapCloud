CLASS zcl_work_order_crud_handler_a7 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      create_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_a7
                                  iv_customer_id   TYPE zde_customer_id_a7
                                  iv_technician_id TYPE zde_technician_id_a7
                                  iv_priority      TYPE zde_priority_a7
                                  iv_status        TYPE zde_status_a7
                                  iv_description   TYPE string
                                  iv_creation_date TYPE d
                        EXPORTING rv_valid         TYPE abap_bool
                                  rv_message       TYPE string,
      read_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_a7
                      EXPORTING rv_valid         TYPE abap_bool
                                rv_message       TYPE string
                                rv_ls_work_order TYPE ztwork_order_a7,
      update_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_a7
                                  iv_customer_id   TYPE zde_customer_id_a7
                                  iv_technician_id TYPE zde_technician_id_a7
                                  iv_priority      TYPE zde_priority_a7
                                  iv_status        TYPE zde_status_a7
                                  iv_description   TYPE string
                                  iv_creation_date TYPE d
                        EXPORTING rv_valid         TYPE abap_bool
                                  rv_message       TYPE string,
      delete_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_a7
                        EXPORTING rv_valid         TYPE abap_bool
                                  rv_message       TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_handler_a7 IMPLEMENTATION.


  METHOD create_work_order.

    DATA: ls_work_order    TYPE ztwork_order_a7,
          lv_error_message TYPE string.

    DATA(lo_instance) = NEW zcl_work_order_validator_a7( ).

    "Ejecutamos las validaciones correspondientes
    IF lo_instance->validate_create_order( EXPORTING iv_customer_id   = iv_customer_id
                                           iv_technician_id = iv_technician_id
                                           iv_priority      = iv_priority
                                           IMPORTING ev_error_message = lv_error_message ) = abap_false.

      rv_message =  lv_error_message.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    "En caso de superar las validaciones creamos el registro
    ls_work_order = VALUE #( work_order_id = iv_work_order_id
                            customer_id   = iv_customer_id
                            technician_id = iv_technician_id
                            priority      = iv_priority
                            status        = iv_status
                            description   = iv_description
                            creation_date = iv_creation_date ).

    INSERT ztwork_order_a7 FROM @ls_work_order.

    IF sy-subrc = 0.
      rv_message =  | Orden de Trabajo { iv_work_order_id } creada correctamente|  .
      rv_valid = abap_true.
    ELSE.
      rv_message =  | Orden de Trabajo { iv_work_order_id } no fue creada|  .
      rv_valid = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD read_work_order.

    IF iv_work_order_id IS INITIAL.
      rv_message = 'ID de orden de trabajo no proporcionado.'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM ztwork_order_a7
    FIELDS *
    WHERE work_order_id = @iv_work_order_id
    INTO @rv_ls_work_order.

    IF sy-subrc = 0.
      rv_message =  | Orden de Trabajo { iv_work_order_id } consultada con exito|  .
      rv_valid = abap_true.

    ELSE.
      rv_message =  | Orden de Trabajo { iv_work_order_id } no existe |  .
      rv_valid = abap_false.
    ENDIF.


  ENDMETHOD.

  METHOD update_work_order.

    DATA: ls_work_order         TYPE ztwork_order_a7,
          lv_error_message      TYPE string,
          lv_change_description TYPE string.

    DATA(lo_instance) = NEW zcl_work_order_validator_a7( ).
    DATA(lo_wrk_ord_his_crud_han) = NEW zcl_wrk_ord_his_crud_han_a7( ).


    "Ejecutamos las validaciones correspondientes
    IF lo_instance->validate_update_order( EXPORTING iv_work_order_id   = iv_work_order_id
                                           IMPORTING ev_error_message = lv_error_message ) = abap_false.

      rv_message =  lv_error_message.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM ztwork_order_a7
    FIELDS *
    WHERE work_order_id = @iv_work_order_id
    INTO @ls_work_order.

    IF sy-subrc = 0.
      IF ls_work_order-customer_id <> iv_customer_id.
        lv_change_description = | / Actualización de id del cliente: { ls_work_order-customer_id }->{ iv_customer_id }|.
        ls_work_order-customer_id   = iv_customer_id.
      ENDIF.

      IF ls_work_order-technician_id <> iv_technician_id.
        lv_change_description = lv_change_description && | / Actualización de id del técnico: { ls_work_order-technician_id }->{ iv_technician_id }|.
        ls_work_order-technician_id = iv_technician_id.
      ENDIF.

      IF ls_work_order-priority <> iv_priority.
        lv_change_description = lv_change_description && | / Actualización de id del técnico: { ls_work_order-priority }->{ iv_priority }|.
        ls_work_order-priority      = iv_priority.
      ENDIF.

      IF ls_work_order-status <> iv_status.
        lv_change_description = lv_change_description && | / Actualización de estado: { ls_work_order-status }->{ iv_status }|.
        ls_work_order-status        = iv_status.
      ENDIF.

      IF ls_work_order-description <> iv_description.
        lv_change_description = lv_change_description && | / Actualización de la descripción: { ls_work_order-description }->{ iv_description }|.
        ls_work_order-description   = iv_description.
      ENDIF.

      IF ls_work_order-creation_date <> iv_creation_date.
        lv_change_description = lv_change_description && | / Actualización de la fecha de creación: { ls_work_order-creation_date }->{ iv_creation_date }|.
        ls_work_order-creation_date = iv_creation_date.
      ENDIF.
    ENDIF.

*    ls_work_order = VALUE #( work_order_id = iv_work_order_id
*                            customer_id   = iv_customer_id
*                            technician_id = iv_technician_id
*                            priority      = iv_priority
*                            status        = iv_status
*                            description   = iv_description
*                            creation_date = iv_creation_date ).

    UPDATE ztwork_order_a7 FROM @ls_work_order.

    IF sy-subrc = 0.

      lo_wrk_ord_his_crud_han->create_wrk_ord_his( EXPORTING iv_work_order_id = iv_work_order_id
                                 iv_modification_date = cl_abap_context_info=>get_system_date( )
                                 iv_change_description = lv_change_description
                       IMPORTING rv_valid = rv_valid
                                 rv_message = rv_message ).

      IF rv_valid = abap_false.
        RETURN.
      ENDIF.

      rv_message =  | Orden de Trabajo { iv_work_order_id } actualizada correctamente|  .
      rv_valid = abap_true.
    ELSE.
      rv_message =  | Orden de Trabajo { iv_work_order_id } no fue actualizada|  .
      rv_valid = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD delete_work_order.

    DATA: ls_work_order    TYPE ztwork_order_a7,
          lv_error_message TYPE string.

    DATA(lo_instance) = NEW zcl_work_order_validator_a7( ).

    "Ejecutamos las validaciones correspondientes
    IF lo_instance->validate_delete_order( EXPORTING iv_work_order_id   = iv_work_order_id
                                           IMPORTING ev_error_message = lv_error_message ) = abap_false.
      rv_message =  lv_error_message.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    "En caso de superar las validaciones eliminamos el registro

    ls_work_order = VALUE ztwork_order_a7( work_order_id = iv_work_order_id ).

    DELETE ztwork_order_a7 FROM @ls_work_order.

    IF sy-subrc = 0.
      rv_message =  | Orden de Trabajo { iv_work_order_id } eliminada correctamente|  .
      rv_valid = abap_true.
    ELSE.
      rv_message =  | Orden de Trabajo { iv_work_order_id } no pudo ser eliminada|  .
      rv_valid = abap_false.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
