    CLASS zcl_work_order_validator_a7 DEFINITION
      PUBLIC
      FINAL
      CREATE PUBLIC .

      PUBLIC SECTION.
        METHODS:
          validate_create_order IMPORTING iv_customer_id   TYPE zde_customer_id_a7
                                          iv_technician_id TYPE zde_technician_id_a7
                                          iv_priority      TYPE zde_priority_a7
                                EXPORTING ev_error_message TYPE string
                                RETURNING VALUE(rv_valid)  TYPE abap_bool,
          validate_update_order IMPORTING iv_work_order_id TYPE zde_work_order_id_a7
                                EXPORTING ev_error_message TYPE string
                                RETURNING VALUE(rv_valid)  TYPE abap_bool,
          validate_delete_order IMPORTING iv_work_order_id TYPE zde_work_order_id_a7
                                EXPORTING ev_error_message TYPE string
                                RETURNING VALUE(rv_valid)  TYPE abap_bool.
      PROTECTED SECTION.
      PRIVATE SECTION.
        METHODS:
          check_customer_exists IMPORTING iv_customer_id   TYPE zde_customer_id_a7
                                RETURNING VALUE(rv_exists) TYPE abap_bool,
          check_technician_exists IMPORTING iv_technician_id TYPE zde_technician_id_a7
                                  RETURNING VALUE(rv_exists) TYPE abap_bool,
          check_priority IMPORTING iv_priority_id  TYPE zde_priority_a7
                         RETURNING VALUE(rv_valid) TYPE abap_bool,
          check_order_exists IMPORTING iv_work_order_id TYPE zde_work_order_id_a7
                             RETURNING VALUE(rv_exists) TYPE abap_bool,
          check_order_pending IMPORTING iv_work_order_id  TYPE zde_work_order_id_a7
                              RETURNING VALUE(rv_pending) TYPE abap_bool,
          check_order_history_exists IMPORTING iv_work_order_id TYPE zde_work_order_id_a7
                                     RETURNING VALUE(rv_exists) TYPE abap_bool.
    ENDCLASS.



    CLASS zcl_work_order_validator_a7 IMPLEMENTATION.

      METHOD validate_create_order. "Valida que el cliente, el técnico y la prioridad sean correctos antes de permitir la creación de una orden de trabajo

        " Validar que el CUSTOMER_ID (ID del cliente) esté presente
        IF iv_customer_id IS INITIAL.
          rv_valid = abap_false.
          ev_error_message = 'ID del cliente no proporcionado.'.
          RETURN.
        ENDIF.

        " Validar que corresponda a un cliente existente en la tabla ZT_CUSTOMER
        IF check_customer_exists( iv_customer_id ) = abap_false.
          rv_valid = abap_false.
          ev_error_message = 'Cliente no existe.'.
          RETURN.
        ENDIF.

        " Validar que el TECHNICIAN_ID (ID del técnico) esté presente
        IF iv_technician_id IS INITIAL.
          rv_valid = abap_false.
          ev_error_message = 'ID del técnico no proporcionado.'.
          RETURN.
        ENDIF.

        " Validar que corresponda a un técnico existente en la tabla ZT_TECHNICIAN
        IF check_technician_exists( iv_technician_id ) = abap_false.
          rv_valid = abap_false.
          ev_error_message = 'Técnico no existe.'.
          RETURN.
        ENDIF.

        " Validar que el PRIORITY (Prioridad) sea un valor válido
        IF check_priority( iv_priority ) = abap_false.
          rv_valid = abap_false.
          ev_error_message = 'Prioridad inválida.'.
          RETURN.
        ENDIF.

        rv_valid = abap_true.
      ENDMETHOD.

      METHOD validate_update_order. "Asegura que una orden de trabajo pueda ser actualizada solo si existe y su estado es válido para la modificación

        " Verificar que la orden de trabajo existe en la base de datos antes de realizar cualquier modificación
        IF check_order_exists( iv_work_order_id ) = abap_false.
          rv_valid = abap_false.
          ev_error_message = 'Orden de trabajo no existe.'.
          RETURN.
        ENDIF.

        "Comprobar que solo se pueden actualizar las órdenes cuyo estado (STATUS) esté en un estado editable
        IF check_order_pending( iv_work_order_id ) = abap_false.
          rv_valid = abap_false.
          ev_error_message = 'El estado de la orden de trabajo no permite actualizaciones.'.
          RETURN.
        ENDIF.

        rv_valid = abap_true.
      ENDMETHOD.

      METHOD validate_delete_order. "Asegura que una orden de trabajo pueda ser actualizada solo si existe y su estado es válido para la modificación

        " Verificar que la orden de trabajo existe en la base de datos
        IF check_order_exists( iv_work_order_id ) = abap_false.
          rv_valid = abap_false.
          ev_error_message = 'Orden de trabajo no existe.'.
          RETURN.
        ENDIF.

        " Comprobar que el estado de la orden sea "PE" (Pendiente) antes de permitir la eliminación.
        IF check_order_pending( iv_work_order_id ) = abap_false.
          rv_valid = abap_false.
          ev_error_message = 'El estado de la orden de trabajo no permite eliminarla.'.
          RETURN.
        ENDIF.

        " Verificar que la orden no tenga entradas en el historial.
        IF check_order_history_exists( iv_work_order_id ) = abap_true.
          rv_valid = abap_false.
          ev_error_message = 'La orden de trabajo solo podrá eliminarse si no existen registros asociados en el historial de actualizaciones.'.
          RETURN.
        ENDIF.

        rv_valid = abap_true.
      ENDMETHOD.

      METHOD check_customer_exists.
        SELECT SINGLE FROM ztcustomer_a7
            FIELDS customer_id
            WHERE customer_id = @iv_customer_id
            INTO @DATA(ls_customer).

        IF sy-subrc = 0.
          rv_exists = abap_true.
        ELSE.
          rv_exists = abap_false.
        ENDIF.
      ENDMETHOD.

      METHOD check_technician_exists.
        SELECT SINGLE FROM zttechnician_a7
          FIELDS technician_id
          WHERE technician_id = @iv_technician_id
          INTO @DATA(ls_technician).

        IF sy-subrc = 0.
          rv_exists = abap_true.
        ELSE.
          rv_exists = abap_false.
        ENDIF.
      ENDMETHOD.

      METHOD check_priority.
        IF iv_priority_id = 'A' OR iv_priority_id = 'B'.
          rv_valid = abap_true.
        ELSE.
          rv_valid = abap_false.
        ENDIF.
      ENDMETHOD.

      METHOD check_order_exists.
        SELECT SINGLE FROM ztwork_order_a7
          FIELDS work_order_id
          WHERE work_order_id = @iv_work_order_id
          INTO @DATA(ls_work_order).

        IF sy-subrc = 0.
          rv_exists = abap_true.
        ELSE.
          rv_exists = abap_false.
        ENDIF.
      ENDMETHOD.

      METHOD check_order_pending.
        SELECT SINGLE FROM ztwork_order_a7
          FIELDS status
          WHERE work_order_id = @iv_work_order_id
          INTO @DATA(lv_status).

        IF sy-subrc = 0 AND lv_status = 'PE'.
          rv_pending = abap_true.
        ELSE.
          rv_pending = abap_false.
        ENDIF.
      ENDMETHOD.

      METHOD check_order_history_exists.
        SELECT SINGLE FROM ztwrk_ord_his_a7
          FIELDS history_id
          WHERE work_order_id = @iv_work_order_id
          INTO @DATA(ls_wrk_ord_his).

        IF sy-subrc = 0.
          rv_exists = abap_true.
        ELSE.
          rv_exists = abap_false.
        ENDIF.
      ENDMETHOD.

    ENDCLASS.
