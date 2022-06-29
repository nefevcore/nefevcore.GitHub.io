# 生产报工

## 示例代码

使用 ***BAPI_PRODORDCONF_GET_TT_PROP*** 查询工单信息，使用 ***BAPI_PRODORDCONF_CREATE_TT*** 创建或维护报工。

### 新建

<details>
<summary>新建生产报工</summary>

```ABAP

" 处理状态
DATA l_mtype TYPE bapi_mtype.
DATA l_msg TYPE bapi_msg.

DATA:
  lt_timetickets        TYPE STANDARD TABLE OF bapi_pp_timeticket,
  ls_timetickets        TYPE bapi_pp_timeticket,
  ls_bapiret1           TYPE bapiret1.

CLEAR lt_timetickets.
LOOP AT lt_input INTO ls_input.
  CLEAR ls_timetickets.
  ls_timetickets-orderid    = ls_input-aufnr. " 生产订单号
  ls_timetickets-operation  = ls_input-posnr. " 工序
  ls_timetickets-postg_date = ls_input-budat. " 日期
  ls_timetickets-yield      = ls_input-gmnga. " 生产数量
  APPEND ls_timetickets TO lt_timetickets.
ENDLOOP.

" 报工
CLEAR ls_bapiret1.
CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
  IMPORTING
    return      = ls_bapiret1
  TABLES
    timetickets = lt_timetickets.
IF ls_bapiret1-type CA 'AEX'.
  l_mtype = 'E'.
  MESSAGE ID ls_bapiret1-id TYPE ls_bapiret1-type NUMBER ls_bapiret1-number
     WITH ls_bapiret1-message_v1 ls_bapiret1-message_v2 ls_bapiret1-message_v3 ls_bapiret1-message_v4
     INTO l_msg.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  l_mtype = 'S'.
  l_msg = |报工已处理|.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDIF.

```

</details>

### 修改

<details>
<summary>修改生产报工</summary>

```ABAP


```

</details>
