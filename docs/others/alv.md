# ALV

整理下ALV用到的两种方法，以及可能遇到的问题

## LVC

LVC好在不用画屏幕，直接调用即可。

<details>
    <summary>INCLUDE_LVC</summary>

    ```ABAP
        
    *&---------------------------------------------------------------------*
    *& Form frm_display
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM frm_display TABLES ct_data TYPE STANDARD TABLE.

        DATA ls_layout TYPE lvc_s_layo.
        DATA lt_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat.

        PERFORM frm_set_layout CHANGING ls_layout.
        PERFORM frm_set_fieldcat TABLES lt_fieldcat.

        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
            EXPORTING
            i_callback_program       = sy-repid
            i_callback_pf_status_set = 'FRM_PF_STATUS'
            i_callback_user_command  = 'FRM_USER_COMMAND'
            is_layout_lvc            = ls_layout
            it_fieldcat_lvc          = lt_fieldcat
            i_default                = abap_true
            i_save                   = 'A'
            TABLES
            t_outtab                 = ct_data[]
            EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_set_layout
    *&---------------------------------------------------------------------*
    *& 报表布局设置
    *&---------------------------------------------------------------------*
    FORM frm_set_layout CHANGING cs_layout TYPE lvc_s_layo.

        CLEAR cs_layout.
        cs_layout-zebra = abap_true. " 斑马线
        cs_layout-cwidth_opt = abap_true. " 自动调整ALVL列宽
        cs_layout-sel_mode = 'A'. " 选择模式
        *  CS_LAYOUT-box_fname  = 'CHECKBOX'. " 选择框
        *  CS_LAYOUT-ctab_fname = 'COLTAB'. " 单元格颜色设置

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_set_fieldcat
    *&---------------------------------------------------------------------*
    *& 字段目录设置
    *&---------------------------------------------------------------------*
    FORM frm_set_fieldcat TABLES ct_fieldcat TYPE lvc_t_fcat.

        DATA ls_fieldcat TYPE lvc_s_fcat.

        DEFINE _init_fieldcat.
            CLEAR ls_fieldcat.
            ls_fieldcat-fieldname = &1.
            ls_fieldcat-tooltip =
            ls_fieldcat-coltext =
            ls_fieldcat-seltext =
            ls_fieldcat-scrtext_l =
            ls_fieldcat-scrtext_m =
            ls_fieldcat-scrtext_s = &2.
            ls_fieldcat-ref_table = &3.
            ls_fieldcat-ref_field = &4.
            INSERT ls_fieldcat INTO TABLE ct_fieldcat.
        END-OF-DEFINITION.

        _init_fieldcat 'ZSEL' '选择项' '' ''.

        _init_fieldcat 'MTYPE  ' '处理状态' '' ''.
        _init_fieldcat 'MSG    ' '处理消息' '' ''.

        " 个性化自己输出数据格式
        LOOP AT ct_fieldcat REFERENCE INTO DATA(lr_fieldcat).
            " 字段显示属性设置
            CASE lr_fieldcat->fieldname .
            WHEN 'ZSEL'.
                lr_fieldcat->checkbox = abap_true.
                lr_fieldcat->edit = abap_true.
                lr_fieldcat->hotspot = abap_true.
            WHEN 'MENGE'.
                lr_fieldcat->qfieldname = 'MEINS'.
            WHEN OTHERS.
            ENDCASE.
        ENDLOOP.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_pf_status
    *&---------------------------------------------------------------------*
    *& 设置GUI状态
    *&---------------------------------------------------------------------*
    FORM frm_pf_status USING ct_extab TYPE slis_t_extab.
        SET PF-STATUS 'STATUS'.
        SET TITLEBAR 'TITLE'.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_user_command
    *&---------------------------------------------------------------------*
    *& 功能响应
    *&---------------------------------------------------------------------*
    FORM frm_user_command USING cv_ucomm LIKE sy-ucomm
                                cs_selfield TYPE slis_selfield.

        " 刷新屏幕数据到内表
        DATA: lo_grid TYPE REF TO cl_gui_alv_grid.
        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
            IMPORTING
            e_grid = lo_grid.
        CALL METHOD lo_grid->check_changed_data.

        PERFORM frm_get_data_selection.

        " 按钮功能实现
        CASE cv_ucomm.
            WHEN '&IC1'. " 双击

            WHEN OTHERS.
        ENDCASE.

        " 刷新ALV 显示值
        cs_selfield-refresh = abap_true .
        cs_selfield-row_stable = abap_true .
        cs_selfield-col_stable = abap_true .

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_get_data_selection
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM frm_get_data_selection.

        " 刷新屏幕数据到内表
        DATA: lo_grid TYPE REF TO cl_gui_alv_grid.
        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
            IMPORTING
            e_grid = lo_grid.

        " 获取ALV选取行
        DATA lt_rows TYPE lvc_t_row.
        CALL METHOD lo_grid->get_selected_rows
            IMPORTING
            et_index_rows = lt_rows.

        LOOP AT lt_rows INTO DATA(ls_row).
            READ TABLE gt_data REFERENCE INTO DATA(lr_data) INDEX ls_row-index.
            IF sy-subrc = 0.
            lr_data->zsel = abap_true.
            ENDIF.
        ENDLOOP.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_reset_data_selection
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM frm_reset_data_selection.

        LOOP AT gt_data REFERENCE INTO DATA(lr_data).
            CLEAR lr_data->zsel.
        ENDLOOP.

    ENDFORM.

    ```

</details>

## ALV GRID

<details>
    <summary>INCLUDE_ALV_GRID</summary>

    ```ABAP

    CLASS lcl_event_handler DEFINITION DEFERRED.

    DATA go_container TYPE REF TO cl_gui_container.
    DATA go_alv_grid TYPE REF TO cl_gui_alv_grid.
    DATA go_handler TYPE REF TO lcl_event_handler.

    *&---------------------------------------------------------------------*
    *& Form lcl_event_handler
    *&---------------------------------------------------------------------*
    CLASS lcl_event_handler DEFINITION.
    PUBLIC SECTION.
        " 数据变更事件
        METHODS on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING
            e_modified
            et_good_cells.

        " 工具栏调整
        METHODS on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_interactive.

        " 响应工具栏事件
        " 点击按钮之类的事件
        METHODS on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm.
    ENDCLASS.
    *&---------------------------------------------------------------------*
    *& Form lcl_event_handler
    *&---------------------------------------------------------------------*
    CLASS lcl_event_handler IMPLEMENTATION.
    *&---------------------------------------------------------------------*
    *& on_data_changed_finished
    *&---------------------------------------------------------------------*
    METHOD on_data_changed_finished.

        CHECK e_modified = abap_true.

        LOOP AT et_good_cells INTO DATA(ls_cell).
        READ TABLE gt_data REFERENCE INTO DATA(lr_data) INDEX ls_cell-row_id.
        IF sy-subrc = 0.

        ENDIF.
        ENDLOOP.

        PERFORM frm_refresh_display.

    ENDMETHOD.

    *&---------------------------------------------------------------------*
    *& on_toolbar
    *&---------------------------------------------------------------------*
    METHOD on_toolbar.

        " 移除编辑相关的按钮
        DELETE e_object->mt_toolbar WHERE function CS '&LOCAL&'.

        " 新增按钮
        DATA ls_button LIKE LINE OF e_object->mt_toolbar.
        CLEAR ls_button.
        ls_button-function = 'Z10'.
        ls_button-icon = icon_system_copy.
        ls_button-quickinfo =
        ls_button-text      = ''.
        INSERT ls_button INTO TABLE e_object->mt_toolbar.

    ENDMETHOD.

    *&---------------------------------------------------------------------*
    *& on_user_command
    *&---------------------------------------------------------------------*
    METHOD on_user_command.

        " 获取ALV选取行
        DATA lt_rows TYPE lvc_t_row.
        DATA ls_row TYPE lvc_s_row.
        CALL METHOD go_alv_grid->get_selected_rows
        IMPORTING
            et_index_rows = lt_rows.

        CASE e_ucomm.
        WHEN 'Z10'.

        WHEN OTHERS.
        ENDCASE.

        PERFORM frm_refresh_display.

    ENDMETHOD.

    ENDCLASS.

    *&---------------------------------------------------------------------*
    *& Form frm_display
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM frm_display TABLES ct_data TYPE STANDARD TABLE.

        IF go_container IS NOT BOUND.
            go_container = NEW cl_gui_custom_container( 'CON_9000' ).
        ENDIF.

        IF go_alv_grid IS BOUND.
            PERFORM frm_refresh_display.
            RETURN.
        ENDIF.

        " 新建对象
        go_alv_grid = NEW cl_gui_alv_grid( go_container ).

        DATA ls_layout TYPE lvc_s_layo.
        DATA lt_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat.

        PERFORM frm_set_layout CHANGING ls_layout.
        PERFORM frm_set_fieldcat TABLES lt_fieldcat.
        PERFORM frm_set_event.
        PERFORM frm_set_style.
        PERFORM frm_set_color.

        " 展示
        CALL METHOD go_alv_grid->set_table_for_first_display
            EXPORTING
            is_layout                     = ls_layout
            CHANGING
            it_outtab                     = ct_data[]
            it_fieldcatalog               = lt_fieldcat
            EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_display
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM frm_refresh_display.

        PERFORM frm_set_style.
        PERFORM frm_set_color.

        go_alv_grid->refresh_table_display( is_stable = CONV #( 'XX' ) ).

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_set_layout
    *&---------------------------------------------------------------------*
    *& 报表布局设置
    *&---------------------------------------------------------------------*
    FORM frm_set_layout CHANGING cs_layout TYPE lvc_s_layo.

        CLEAR cs_layout.
        cs_layout-zebra = abap_true. " 斑马线
        cs_layout-cwidth_opt = abap_true. " 自动调整ALVL列宽
        cs_layout-sel_mode = 'A'. " 选择模式
        cs_layout-ctab_fname = 'T_SCOL'. " 单元格颜色设置
        cs_layout-stylefname = 'T_STYL'. " 单元格控制

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_set_fieldcat
    *&---------------------------------------------------------------------*
    *& 字段目录设置
    *&---------------------------------------------------------------------*
    FORM frm_set_fieldcat TABLES ct_fieldcat TYPE lvc_t_fcat.

        DATA ls_fieldcat TYPE lvc_s_fcat.

        DEFINE _init_fieldcat.
            CLEAR ls_fieldcat.
            ls_fieldcat-fieldname = &1.
            ls_fieldcat-tooltip =
            ls_fieldcat-coltext =
            ls_fieldcat-seltext =
            ls_fieldcat-scrtext_l =
            ls_fieldcat-scrtext_m =
            ls_fieldcat-scrtext_s = &2.
            ls_fieldcat-ref_table = &3.
            ls_fieldcat-ref_field = &4.
            INSERT ls_fieldcat INTO TABLE ct_fieldcat.
        END-OF-DEFINITION.

        _init_fieldcat 'MTYPE' '检查状态' '' ''.
        _init_fieldcat 'MSG  ' '检查消息' '' ''.

        " 个性化自己输出数据格式
        LOOP AT ct_fieldcat REFERENCE INTO DATA(lr_fieldcat).
            " 编辑状态，默认可编辑
            lr_fieldcat->edit = abap_true.
            CASE lr_fieldcat->fieldname .
            WHEN 'MTYPE'
                OR 'MSG'
                .
                lr_fieldcat->edit = abap_false.
            WHEN OTHERS.
            ENDCASE.
        ENDLOOP.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_set_event
    *&---------------------------------------------------------------------*
    *& 绑定ALV事件
    *&---------------------------------------------------------------------*
    FORM frm_set_event.

        IF go_handler IS BOUND.
            RETURN.
        ENDIF.

        go_handler = NEW lcl_event_handler( ).

        " 数据变更
        go_alv_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
        SET HANDLER go_handler->on_data_changed_finished FOR go_alv_grid.

        " 工具栏相关
        SET HANDLER go_handler->on_toolbar FOR go_alv_grid.
        SET HANDLER go_handler->on_user_command FOR go_alv_grid.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_set_style
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM frm_set_style.

        DATA ls_styl TYPE lvc_s_styl.

        LOOP AT gt_data REFERENCE INTO DATA(lr_data).
            CLEAR lr_data->t_styl.

            " 设置ALV字段或行的状态
            " 常用有禁止编辑CL_GUI_ALV_GRID=>MC_STYLE_DISABLED
            " 禁止删除行CL_GUI_ALV_GRID=>MC_STYLE_NO_DELETE_ROW

        ENDLOOP.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form frm_set_color
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM frm_set_color.

        DATA ls_scol TYPE lvc_s_scol.

        LOOP AT gt_data REFERENCE INTO DATA(lr_data).
            CLEAR lr_data->t_scol.

            IF lr_data->mtype = 'E'.
            CLEAR ls_scol.
            ls_scol-fname = 'MTYPE'.
            ls_scol-color = VALUE #( col = '6' ).
            INSERT ls_scol INTO TABLE lr_data->t_scol.

            CLEAR ls_scol.
            ls_scol-fname = 'MSG'.
            ls_scol-color = VALUE #( col = '6' ).
            INSERT ls_scol INTO TABLE lr_data->t_scol.
            ENDIF.

        ENDLOOP.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Module STATUS_9000 OUTPUT
    *&---------------------------------------------------------------------*
    *&
    *&---------------------------------------------------------------------*
    MODULE status_9000 OUTPUT.

        PERFORM frm_display TABLES gt_data.

        SET PF-STATUS 'STATUS'.
        SET TITLEBAR 'TITLE'.

    ENDMODULE.
    *&---------------------------------------------------------------------*
    *&      Module  USER_COMMAND_9000  INPUT
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    MODULE user_command_9000 INPUT.

        CASE sy-ucomm.
            WHEN '&F03' OR '&F15' OR '&F12'.
            LEAVE TO SCREEN 0.
            WHEN OTHERS.
        ENDCASE.
        CLEAR sy-ucomm.

    ENDMODULE.

    ```

</details>
