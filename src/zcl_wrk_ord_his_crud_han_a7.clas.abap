CLASS zcl_wrk_ord_his_crud_han_a7 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      create_wrk_ord_his IMPORTING iv_work_order_id      TYPE zde_work_order_id_a7
                                   iv_modification_date  TYPE d
                                   iv_change_description TYPE string
                         EXPORTING rv_valid              TYPE abap_bool
                                   rv_message            TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_wrk_ord_his_crud_han_a7 IMPLEMENTATION.
  METHOD create_wrk_ord_his.

    DATA: ls_wrk_ord_his    TYPE ztwrk_ord_his_a7,
          lv_error_message TYPE string,
          lv_count type i.

    SELECT COUNT(*) FROM ztwrk_ord_his_a7 INTO @lv_count.

    lv_count = lv_count + 1.

    ls_wrk_ord_his = VALUE #( history_id = lv_count
                            work_order_id   = iv_work_order_id
                            modification_date = iv_modification_date
                            change_description      = iv_change_description ).

    INSERT ztwrk_ord_his_a7 FROM @ls_wrk_ord_his.

    IF sy-subrc = 0.
      rv_message =  | Historico de orden de trabajo { lv_count } creado correctamente|  .
      rv_valid = abap_true.
    ELSE.
      rv_message =  | Historico de orden de trabajo { lv_count } no fue creado|  .
      rv_valid = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
