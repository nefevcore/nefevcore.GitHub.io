# 工单关闭

## 技术性完成（TECO）生产订单

<details>
<summary>示例代码</summary>

```ABAP

DATA:
lt_orders        TYPE STANDARD TABLE OF bapi_order_key,
lt_detail_return TYPE STANDARD TABLE OF bapi_order_return,
ls_return        TYPE bapiret2.

CLEAR lt_orders.
CLEAR lt_detail_return.
CLEAR ls_return.

INSERT VALUE #( order_number = ls_input-aufnr ) INTO TABLE lt_orders.

" 技术性完成生产订单
CALL FUNCTION 'BAPI_PRODORD_COMPLETE_TECH'
IMPORTING
    return        = ls_return
TABLES
    orders        = lt_orders
    detail_return = lt_detail_return.

IF ls_return-type = 'E'.
l_mtype = 'E'.
MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
    WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
    INTO l_msg.
CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
l_mtype = 'S'.
l_msg = |已处理|.
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
    wait = 'X'.
ENDIF.

```

</details>
