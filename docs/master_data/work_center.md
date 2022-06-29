# 工作中心

使用 ***CL_PP_WORK_CENTER_BO*** 导入工作中心

> 上面类通过封装 ***CRAP_WORKCENTER_CREATE*** 实现，并多了返回信息

<details>
  <summary>工作中心导入</summary>

    ```ABAP

    TYPES:
      BEGIN OF ty_data,
        " 创建工作中心必填字段
        arbpl TYPE crhd-arbpl, " 工作中心
        werks TYPE crhd-werks, " 工厂
        kostl TYPE crco-kostl, " 成本中心
        " 处理返回
        mtype TYPE bapi_mtype,
        msg   TYPE bapi_msg,
      END OF ty_data.
    DATA lt_data TYPE STANDARD TABLE OF ty_data.

    DATA ls_crhd_api01 TYPE crhd_api01.
    DATA ls_crhd_api02 TYPE crhd_api02.
    DATA ls_crhd_api03 TYPE crhd_api03.
    DATA ls_crhd_api05 TYPE crhd_api05.

    DATA lt_kapa_api01 TYPE STANDARD TABLE OF kapa_api01.
    DATA lt_kapa_api02 TYPE STANDARD TABLE OF kapa_api02.
    DATA lt_crhd_api04 TYPE STANDARD TABLE OF crhd_api04.
    DATA lt_crco_api01 TYPE STANDARD TABLE OF crco_api01.

    DATA ls_kapa_api01 TYPE kapa_api01.
    DATA ls_kapa_api02 TYPE kapa_api02.
    DATA ls_crhd_api04 TYPE crhd_api04.
    DATA ls_crco_api01 TYPE crco_api01.

    DATA lt_return TYPE bapiret2_tab.

    DATA l_mtype TYPE bapi_mtype.
    DATA l_msg TYPE bapi_msg.

    " 查找工厂对应日历
    IF lt_data[] IS NOT INITIAL.
      SELECT
        werks,
        fabkl
        FROM t001w
        FOR ALL ENTRIES IN @lt_data[]
        WHERE werks = @lt_data-werks
        INTO TABLE @DATA(lt_t001w).
      SORT lt_t001w BY werks.
      DELETE ADJACENT DUPLICATES FROM lt_t001w COMPARING werks.
    ENDIF.

    " 分组处理
    LOOP AT lt_data INTO DATA(ls_data) WHERE mtype <> 'E'
      GROUP BY (
        arbpl = ls_data-arbpl
        werks = ls_data-werks
        group_size = GROUP SIZE
        ) INTO DATA(ls_data_grp).

      " 重复代码，退出时写入处理结果
      DATA lr_data LIKE REF TO ls_data.
      DEFINE _contiune.
        CLEAR l_mtype.
        CLEAR l_msg.
        l_mtype = &1.
        l_msg = &2.
        LOOP AT GROUP ls_data_grp REFERENCE INTO lr_data.
          lr_data->mtype = l_mtype.
          lr_data->msg = l_msg.
        ENDLOOP.
        CONTINUE.
      END-OF-DEFINITION.

      " 取工厂相关数据
      READ TABLE lt_t001w INTO DATA(ls_t001w) WITH KEY werks = ls_data_grp-werks BINARY SEARCH.
      IF sy-subrc <> 0.
        l_msg = |工厂{ ls_data_grp-werks }不存在|.
        _contiune 'E' l_msg.
      ENDIF.

      CLEAR ls_crhd_api01.
      ls_crhd_api01-arbpl = ls_data_grp-arbpl.
      ls_crhd_api01-werks = ls_data_grp-werks.
      ls_crhd_api01-verwe = '0001'.
      ls_crhd_api01-ktext = |{ ls_data_grp-arbpl }-{ ls_data_grp-werks }|. " 工作中心文本

      CLEAR ls_crhd_api02.
      ls_crhd_api02-planv = '009'.
      ls_crhd_api02-veran = 'DF'.
      ls_crhd_api02-vgwts = 'DFDL'.

      CLEAR ls_crhd_api03.
      ls_crhd_api03-steus = 'DF01'.
      ls_crhd_api03-vge01 = 'STD'. " h - 内码
      ls_crhd_api03-vge02 = 'STD'. " h - 内码
      ls_crhd_api03-vge03 = 'STD'. " h - 内码
      ls_crhd_api03-vge04 = 'KG'. " kg - 内码
      ls_crhd_api03-vge05 = 'STD'. " h - 内码
      ls_crhd_api03-vge06 = 'KG'. " kg - 内码

      CLEAR ls_crhd_api05.
      ls_crhd_api05-kapart = '001'.
      ls_crhd_api05-fort2 = 'DF_PP'.

      CLEAR lt_kapa_api01.
      CLEAR ls_kapa_api01.
      ls_kapa_api01-canum = '1'.
      ls_kapa_api01-kapart = '001'.
      " 这个不填，下面FM才会会自动创建能力
      " 或者手工调用CRAP_CAPACITY_CREATE创建能力
      " ls_kapa_api01-kapname = '机器'.
      ls_kapa_api01-werks = ls_data_grp-werks.
      ls_kapa_api01-ktext = '机器'.
      ls_kapa_api01-kapid = ls_t001w-fabkl. " 使用工厂日历
      INSERT ls_kapa_api01 INTO TABLE lt_kapa_api01.

      CLEAR lt_kapa_api02.
      CLEAR ls_kapa_api02.
      ls_kapa_api02-canum = '1'.
      ls_kapa_api02-planr = 'DF'.
      ls_kapa_api02-kalid = ls_t001w-fabkl.
      ls_kapa_api02-meins = 'STD'. " h - 内码
      ls_kapa_api02-begzt = '000000'.
      ls_kapa_api02-endzt = '240000'.
      ls_kapa_api02-ngrad = '100'.
      ls_kapa_api02-aznor = ls_data_grp-group_size. " 有多少行项目
      INSERT ls_kapa_api02 INTO TABLE lt_kapa_api02.

      CLEAR lt_crhd_api04.
      CLEAR ls_crhd_api04.
      ls_crhd_api04-canum = '1'.
      ls_crhd_api04-fork2 = 'DF_PP'.
      INSERT ls_crhd_api04 INTO TABLE lt_crhd_api04.

      CLEAR lt_crco_api01.
      LOOP AT GROUP ls_data_grp INTO ls_data.
        CLEAR ls_crco_api01.
        ls_crco_api01-kostl = ls_data-kostl.
        ls_crco_api01-begda = sy-datum.
        ls_crco_api01-endda = '99991231'.

        ls_crco_api01-lstar1 = '1000'.
        ls_crco_api01-lstar_ref1 = 'X'.
        ls_crco_api01-leinh1 = 'STD'. " h - 内码
        ls_crco_api01-forml1 = 'DF_CO1'.

        ls_crco_api01-lstar2 = '2000'.
        ls_crco_api01-lstar_ref2 = 'X'.
        ls_crco_api01-leinh2 = 'STD'. " h - 内码
        ls_crco_api01-forml2 = 'DF_CO2'.

        ls_crco_api01-lstar3 = '3000'.
        ls_crco_api01-lstar_ref3 = 'X'.
        ls_crco_api01-leinh3 = 'STD'. " h - 内码
        ls_crco_api01-forml3 = 'DF_CO3'.

        ls_crco_api01-lstar4 = '4000'.
        ls_crco_api01-lstar_ref4 = 'X'.
        ls_crco_api01-leinh4 = 'KG'. " kg - 内码
        ls_crco_api01-forml4 = 'DF_CO4'.

        ls_crco_api01-lstar5 = '5000'.
        ls_crco_api01-lstar_ref5 = 'X'.
        ls_crco_api01-leinh5 = 'STD'. " h - 内码
        ls_crco_api01-forml5 = 'DF_CO5'.

        ls_crco_api01-lstar6 = '6000'.
        ls_crco_api01-lstar_ref6 = 'X'.
        ls_crco_api01-leinh6 = 'KG'. " kg - 内码
        ls_crco_api01-forml6 = 'DF_CO6'.

        INSERT ls_crco_api01 INTO TABLE lt_crco_api01.
      ENDLOOP.

      " 可能不同行只是设备码不一样，而成本中心是一致的
      SORT lt_crco_api01 BY kostl.
      DELETE ADJACENT DUPLICATES FROM lt_crco_api01 COMPARING kostl.

    * CALL FUNCTION 'CRAP_WORKCENTER_CREATE'
    * EXPORTING
    * in_crhd_api01 = ls_crhd_api01
    * in_crhd_api02 = ls_crhd_api02
    * in_crhd_api03 = ls_crhd_api03
    * in_crhd_api05 = ls_crhd_api05
    * test          = i_test
    * TABLES
    * in_kapa_api01 = lt_kapa_api01
    * in_kapa_api02 = lt_kapa_api02
    * in_crhd_api04 = lt_crhd_api04
    * in_crco_api01 = lt_crco_api01.

      " 换个标准带返回的，方便展示消息
      DATA(lo_work_center) = cl_pp_work_center_bo=>get_instance( ).
      CLEAR lt_return.
      lo_work_center->create_work_center(
        EXPORTING
          is_crhd_api01 = ls_crhd_api01
          is_crhd_api02 = ls_crhd_api02
          is_crhd_api03 = ls_crhd_api03
          it_crhd_api04 = lt_crhd_api04
          is_crhd_api05 = ls_crhd_api05
          it_kapa_api01 = lt_kapa_api01
          it_kapa_api02 = lt_kapa_api02
          it_crco_api01 = lt_crco_api01
        IMPORTING
          et_return     = lt_return
      ).

      CLEAR l_mtype.
      CLEAR l_msg.
      LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'EA'.
        l_msg = |{ l_msg }{ ls_return-message };|.
      ENDLOOP.
      IF sy-subrc = 0.
        _contiune 'E' l_msg.
      ELSE.
        _contiune 'S' '工作中心导入成功'.
      ENDIF.

    ENDLOOP.

    ```

</details>
