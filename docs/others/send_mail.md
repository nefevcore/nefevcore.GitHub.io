# 邮件发送

SBWP，发送测试

SOST，邮箱管理

SCOT，节点配置

<details>
<summary>邮件发送代码</summary>

```ABAP

DATA: send_request TYPE REF TO cl_bcs,
    document     TYPE REF TO cl_document_bcs,
    fail         TYPE REF TO cx_bcs,
    recipient    TYPE REF TO if_recipient_bcs.

DATA: ls        TYPE string,
    mailto    TYPE ad_smtpadr,
    main_text TYPE bcsy_text,
    title     TYPE so_obj_des.

ls = '该邮件用于测试演示程序'.

APPEND ls TO main_text.
title = '邮件发送测试'.
mailto = 'xxx@xxx.xx'.

TRY.
    " 创建发送请求
    send_request = cl_bcs=>create_persistent( ).
    " 创建整理发送内容
    document = cl_document_bcs=>create_document(
        i_type = 'RAW'
        i_text = main_text
        i_subject = title ).
    " 添加邮件内容到发送请求
    send_request->set_document( document ).
    " 邮件地址转换
    recipient = cl_cam_address_bcs=>create_internet_address( mailto ).
    " 添加邮件地址到发送请求
    send_request->add_recipient( recipient ).
    " 正式发送并提交作业
    send_request->send( i_with_error_screen = 'X' ).
    COMMIT WORK AND WAIT.

CATCH cx_bcs INTO fail.
ENDTRY.

```

</details>
