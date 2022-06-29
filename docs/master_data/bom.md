# BOM

见物料清单类别 ***STLTY*** 值域，有不少类型，我目前只用到两种。

## 变更号

使用 ***CCAP_ECN_CREATE*** 创建变更号。

<details>
  <summary>变更号创建函数</summary>

  ``` ABAP
    
  FUNCTION zfm_ecn_check
    IMPORTING
      i_datuv TYPE datuv
      i_verid TYPE verid
    EXPORTING
      e_change_no TYPE aennr
      e_mtype TYPE bapi_mtype
      e_msg TYPE bapi_msg.



    IF i_datuv IS INITIAL OR i_datuv = ''.
      e_mtype = 'E'.
      e_msg = |[版本启用时间]为空|.
      RETURN.
    ENDIF.

    IF i_verid IS INITIAL.
      e_mtype = 'E'.
      e_msg = |[生产版本]为空|.
      RETURN.
    ENDIF.

    " 更改号可以随意编写，一般用日期+版本生成
    DATA l_change_no TYPE aenr_api01-change_no.
    l_change_no = |{ i_datuv }{ i_verid }|.

    CALL FUNCTION 'CONVERSION_EXIT_AENNR_INPUT'
      EXPORTING
        input        = l_change_no
      IMPORTING
        output       = l_change_no
      EXCEPTIONS
        length_error = 1.

    " 检查更改号是否存在
    SELECT SINGLE COUNT(*) FROM aenr WHERE aennr = @l_change_no.
    IF sy-subrc = 0.
      e_change_no = l_change_no.
      e_mtype = 'S'.
      RETURN.
    ENDIF.

    " 不存在则创建
    DATA:
      ls_header         TYPE aenr_api01,
      ls_object_bom     TYPE aenv_api01,
      ls_object_bom_mat TYPE aenv_api01,
      ls_object_bom_psp TYPE aenv_api01,
      ls_object_tlist_n TYPE aenv_api01,
      ls_object_tlist_q TYPE aenv_api01,
      ls_object_char    TYPE aenv_api01,
      ls_object_cls     TYPE aenv_api01,
      lv_number         TYPE aenrb-aennr.

    ls_header-change_no = l_change_no.
    ls_header-status = '01'.
    ls_header-valid_from = |{ i_datuv DATE = USER }|. " 下面FM需要外部格式
    ls_header-descript = |BOM/工艺数据维护|.

    ls_object_bom-active = 'X'.

    ls_object_bom_mat-active = 'X'.
    ls_object_bom_mat-obj_requ = 'X'.
    ls_object_bom_mat-mgtrec_gen = 'X'.

    ls_object_bom_psp-active = 'X'.
    ls_object_bom_psp-obj_requ = 'X'.
    ls_object_bom_psp-mgtrec_gen = 'X'.

    ls_object_tlist_n-active = 'X'.
    ls_object_tlist_n-obj_requ = 'X'.
    ls_object_tlist_n-mgtrec_gen = 'X'.

    ls_object_tlist_q-active = 'X'.
    ls_object_tlist_q-obj_requ = 'X'.
    ls_object_tlist_q-mgtrec_gen = 'X'.

    ls_object_char-active = 'X'.

    ls_object_cls-active = 'X'.

    SET UPDATE TASK LOCAL.

    " 创建ECN
    CLEAR l_change_no.
    CALL FUNCTION 'CCAP_ECN_CREATE'
      EXPORTING
        change_header            = ls_header
        object_bom               = ls_object_bom
        object_bom_mat           = ls_object_bom_mat
        object_bom_psp           = ls_object_bom_psp
        object_tlist_n           = ls_object_tlist_n
        object_tlist_q           = ls_object_tlist_q
        object_char              = ls_object_char
        object_cls               = ls_object_cls
        fl_commit_and_wait       = 'X'
      IMPORTING
        change_no                = l_change_no
      EXCEPTIONS
        change_no_already_exists = 1
        error                    = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      e_mtype = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO e_msg.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    e_change_no = l_change_no.
    e_mtype = 'S'.

  ENDFUNCTION.
  
  ```

</details>

## (M) 物料BOM

物料BOM用 ***CSAP_MAT_BOM_READ*** 读取，***CSAP_MAT_BOM_CREATE*** 创建，***CSAP_MAT_BOM_MAINTAIN*** 维护。

> 实践发现，创建和维护的函数皆不支持备选清单(STLAL)扩展，如需扩展，使用 [***BAPI_MATERIAL_BOM_GROUP_CREATE***](#m-bom_1) 。

<details>
  <summary>物料BOM维护函数</summary>

  ``` abap
  FUNCTION zfm_mat_bom_maintain
    IMPORTING
      i_mast TYPE mast
      i_stko TYPE stko
    EXPORTING
      e_mtype TYPE bapi_mtype
      e_msg TYPE bapi_msg
      et_bapiret2 TYPE bapiret2_t
    TABLES
      it_stpo LIKE stpo.





    " 需要注意，CSAP_MAT_BOM_MAINTAIN要求参数都是外部格式

    DATA l_mtype TYPE bapi_mtype.
    DATA l_msg TYPE bapi_msg.

    DATA:
      ls_csap_mbom  TYPE csap_mbom,
      ls_stko_api01 TYPE stko_api01,
      ls_stko_api02 TYPE stko_api02,
      lt_stpo_api01 TYPE STANDARD TABLE OF stpo_api01,
      ls_stpo_api01 TYPE stpo_api01.

    CLEAR ls_csap_mbom.
    ls_csap_mbom-matnr = i_mast-matnr.
    ls_csap_mbom-werks = i_mast-werks.
    ls_csap_mbom-stlan = i_mast-stlan.
    ls_csap_mbom-stlal = i_mast-stlal.
    ls_csap_mbom-datuv = |{ i_stko-datuv DATE = USER }|. " 外部日期格式
    ls_csap_mbom-aennr = i_stko-aennr.

    CLEAR ls_stko_api01.
    IF i_stko-bmeng IS NOT INITIAL.
      ls_stko_api01-base_quan = |{ i_stko-bmeng NUMBER = USER }|.
    ELSE.
      ls_stko_api01-base_quan = 1.
    ENDIF.
    ls_stko_api01-alt_text = i_stko-stktx.
    ls_stko_api01-bom_status = i_stko-stlst.

    LOOP AT it_stpo INTO DATA(ls_stpo).
      CLEAR ls_stpo_api01.
      ls_stpo_api01-item_no   = ls_stpo-posnr. " 行号
      ls_stpo_api01-item_categ = 'L'.
      ls_stpo_api01-component = ls_stpo-idnrk. " 组件物料
      ls_stpo_api01-comp_qty  = ls_stpo-menge. " 组件数量
  *    ls_stpo_api03-comp_unit = ls_stpo-meins. " 组件计量单位
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = ls_stpo-meins
        IMPORTING
          output         = ls_stpo_api01-comp_unit
        EXCEPTIONS
          unit_not_found = 1.
      ls_stpo_api01-valid_from = ls_stpo-datuv. " 有效期从
      ls_stpo_api01-change_no = ls_stpo-aennr. " 变更号
      ls_stpo_api01-sortstring = ls_stpo-sortf. " 工序（排序字段）
      ls_stpo_api01-rel_prod = 'X'.
      ls_stpo_api01-rel_cost = 'X'.

      " 增强字段
      ls_stpo_api01-zspec1 = ls_stpo-zspec1.
      ls_stpo_api01-zspec3 = ls_stpo-zspec2.
      ls_stpo_api01-zspec2 = ls_stpo-zspec3.

      INSERT ls_stpo_api01 INTO TABLE lt_stpo_api01.
    ENDLOOP.

  *  " CSAP_MAT_BOM_MAINTAIN限制，无法扩展备选BOM
  *  " 跳转到另一个方法，通过BAPI_MATERIAL_BOM_GROUP_CREATE创建
  *  IF ls_csap_mbom-stlal <> '01'.
  *    CALL FUNCTION 'ZFM_MAT_BGR_CREATE'
  *      EXPORTING
  *        i_mast  = i_mast
  *        i_stko  = i_stko
  *      IMPORTING
  *        e_mtype = e_mtype
  *        e_msg   = e_msg
  *      TABLES
  *        it_stpo = it_stpo[].
  *    IF e_mtype = 'E'.
  *      RETURN.
  *    ENDIF.
  *  ENDIF.

    " 检查是否已存在
    DATA:
      lt_stko_api02_old TYPE STANDARD TABLE OF stko_api02,
      lt_stpo_api02_old TYPE STANDARD TABLE OF stpo_api02.
    CALL FUNCTION 'CSAP_MAT_BOM_READ'
      EXPORTING
        material    = ls_csap_mbom-matnr
        plant       = ls_csap_mbom-werks
        bom_usage   = ls_csap_mbom-stlan
        alternative = ls_csap_mbom-stlal
        valid_from  = ls_csap_mbom-datuv
        change_no   = ls_csap_mbom-aennr
      TABLES
        t_stpo      = lt_stpo_api02_old
        t_stko      = lt_stko_api02_old
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      READ TABLE lt_stko_api02_old INTO DATA(ls_stko_api02_upd) INDEX 1.
      " 见LCSDIFEH中的rc29k_stko_compare子例程，抬头只有部分字段可修改
      " 数量、单位、抬头文本都不在修改范围内
  *    ls_stko_api02_upd-alt_text = ls_stko_api01-alt_text.
      ls_stko_api01 = CORRESPONDING #( ls_stko_api02_upd ).

      DATA lt_stpo_api03 TYPE STANDARD TABLE OF stpo_api03.

      SORT lt_stpo_api01 BY item_no.
      SORT lt_stpo_api02_old BY item_no.

      " 修改
      LOOP AT lt_stpo_api01 INTO ls_stpo_api01.
        READ TABLE lt_stpo_api02_old INTO DATA(ls_stpo_api02_upd) WITH KEY item_no = ls_stpo_api01-item_no BINARY SEARCH.
        IF sy-subrc = 0.
          DELETE lt_stpo_api02_old INDEX sy-tabix.
          ls_stpo_api02_upd-item_no    = ls_stpo_api01-item_no    .
          ls_stpo_api02_upd-item_categ = ls_stpo_api01-item_categ .
          ls_stpo_api02_upd-component  = ls_stpo_api01-component  .
          ls_stpo_api02_upd-comp_qty   = ls_stpo_api01-comp_qty   .
          ls_stpo_api02_upd-comp_unit  = ls_stpo_api01-comp_unit  .
          ls_stpo_api02_upd-valid_from = ls_stpo_api01-valid_from .
          ls_stpo_api02_upd-change_no  = ls_stpo_api01-change_no  .
          ls_stpo_api02_upd-rel_prod   = ls_stpo_api01-rel_prod   .
          ls_stpo_api02_upd-rel_cost   = ls_stpo_api01-rel_cost   .
          ls_stpo_api02_upd-zspec1     = ls_stpo_api01-zspec1     .
          ls_stpo_api02_upd-zspec2     = ls_stpo_api01-zspec2     .
          ls_stpo_api02_upd-zspec3     = ls_stpo_api01-zspec3     .
          INSERT CORRESPONDING #( ls_stpo_api02_upd ) INTO TABLE lt_stpo_api03.
        ELSE.
          INSERT CORRESPONDING #( ls_stpo_api01 ) INTO TABLE lt_stpo_api03.
        ENDIF.
      ENDLOOP.

      " 删除多余的旧项目
      LOOP AT lt_stpo_api02_old INTO DATA(ls_stpo_api02_del).
        ls_stpo_api02_del-fldelete = abap_true.
        INSERT CORRESPONDING #( ls_stpo_api02_del ) INTO TABLE lt_stpo_api03.
      ENDLOOP.

      SORT lt_stpo_api03 BY item_no.

      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
        EXPORTING
          material      = ls_csap_mbom-matnr
          plant         = ls_csap_mbom-werks
          bom_usage     = ls_csap_mbom-stlan
          alternative   = ls_csap_mbom-stlal
          valid_from    = ls_csap_mbom-datuv
          change_no     = ls_csap_mbom-aennr
          i_stko        = ls_stko_api01
          fl_bom_create = 'X'
          fl_new_item   = 'X'
        IMPORTING
          o_stko        = ls_stko_api02
        TABLES
          t_stpo        = lt_stpo_api03
        EXCEPTIONS
          error         = 1
          OTHERS        = 2.
    ELSE.
      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'CSAP_MAT_BOM_CREATE'
        EXPORTING
          material    = ls_csap_mbom-matnr
          plant       = ls_csap_mbom-werks
          bom_usage   = ls_csap_mbom-stlan
          alternative = ls_csap_mbom-stlal
          valid_from  = ls_csap_mbom-datuv
          change_no   = ls_csap_mbom-aennr
          i_stko      = ls_stko_api01
        IMPORTING
          bom_no      = ls_stko_api02-bom_no
        TABLES
          t_stpo      = lt_stpo_api01
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.
    ENDIF.

    IF sy-subrc <> 0.
      e_mtype = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO e_msg.
      MESSAGE e_msg TYPE 'S' DISPLAY LIKE 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      e_mtype = 'S'.
      e_msg = |已处理|.
      MESSAGE e_msg TYPE 'S'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDFUNCTION.

  ```
  
</details>

## (M) 物料BOM组

通过 ***BAPI_MATERIAL_BOM_GROUP_CREATE*** 实现多个备选BOM的创建。

<details>
  <summary>物料BOM组创建函数</summary>

  ``` abap

  FUNCTION zfm_mat_bgr_create
    IMPORTING
      i_mast TYPE mast
      i_stko TYPE stko
    EXPORTING
      e_mtype TYPE bapi_mtype
      e_msg TYPE bapi_msg
      et_bapiret2 TYPE bapiret2_t
    TABLES
      it_stpo LIKE stpo.




    DATA l_mtype TYPE bapi_mtype.
    DATA l_msg TYPE bapi_msg.

    DATA:
      testrun              TYPE bapiflag,
      lt_bomgroup          TYPE STANDARD TABLE OF bapi1080_bgr_c,
      ls_bomgroup          TYPE bapi1080_bgr_c,
      lt_variants          TYPE STANDARD TABLE OF bapi1080_bom_c,
      ls_variants          TYPE bapi1080_bom_c,
      lt_materialrelations TYPE STANDARD TABLE OF bapi1080_mbm_c,
      ls_materialrelations TYPE bapi1080_mbm_c,
      lt_items             TYPE STANDARD TABLE OF bapi1080_itm_c,
      ls_items             TYPE bapi1080_itm_c,
      lt_itemassignments   TYPE STANDARD TABLE OF bapi1080_rel_itm_bom_c,
      ls_itemassignments   TYPE bapi1080_rel_itm_bom_c,
      lt_bomsubitems       TYPE STANDARD TABLE OF bapi1080_sui_c,
      lt_bomsubitemas      TYPE STANDARD TABLE OF bapi1080_rel_sui_itm_c,
      lt_texts             TYPE STANDARD TABLE OF bapi1080_txt_c.

    " 标识
    DATA l_uuid TYPE sysuuid_c32.
    TRY.
        l_uuid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        l_uuid = sy-datum && sy-uzeit && sy-uname.
    ENDTRY.

    " 新增/维护
    DATA l_funciton TYPE cs_function.
    SELECT SINGLE COUNT(*) FROM mast
      WHERE matnr = @i_mast-matnr
        AND werks = @i_mast-werks
        AND stlan = @i_mast-stlan
        AND stlal = @i_mast-stlal.
    IF sy-subrc = 0.
      " 修改用CSAP_MAT_BOM_MAINTAIN，这个方法是为了扩展备选BOM
      e_mtype = 'S'.
      e_msg = '已处理'.
      RETURN.
    ELSE.
      l_funciton = 'NEW'.
    ENDIF.

    " OBJECT_TYPE的域值，也备注下，免得看不懂
    " BGR - Bill of material group
    " BOM - Material BOM assembly variant/alternative
    " ITM - BOM item
    " SUI - BOM Sub-Item

    " OBJECT ID
    DATA l_head_object_id TYPE cs_object_id.
    l_head_object_id = 'HEAD'.

    CLEAR ls_bomgroup.
    ls_bomgroup-bom_group_identification = l_uuid.
    ls_bomgroup-object_type      = 'BGR'.
    ls_bomgroup-object_id        = l_head_object_id.
    ls_bomgroup-bom_usage        = i_mast-stlan. " BOM用途
    ls_bomgroup-created_in_plant = i_mast-werks. " 工厂
    INSERT ls_bomgroup INTO TABLE lt_bomgroup.

    " ？？？为什么没有STLTY参数
    CLEAR ls_variants.
    ls_variants-bom_group_identification = l_uuid.
    ls_variants-object_type     = 'BOM'.
    ls_variants-object_id       = l_head_object_id.
    ls_variants-alternative_bom = i_mast-stlal. " 备选物料清单
    ls_variants-alt_text        = i_stko-stktx. " 文本
    ls_variants-bom_status      = i_stko-stlst. " BOM状态
    ls_variants-change_no       = i_stko-aennr. " 变更号
    ls_variants-valid_from_date = i_stko-datuv.
    ls_variants-function        = l_funciton.
    INSERT ls_variants INTO TABLE lt_variants.

    CLEAR ls_materialrelations.
    ls_materialrelations-bom_group_identification = l_uuid.
    ls_materialrelations-material          =
    ls_materialrelations-material_guid     =
    ls_materialrelations-material_external =
    ls_materialrelations-material_long     = i_mast-matnr. " 物料编码
    ls_materialrelations-plant             = i_mast-werks. " 工厂
    ls_materialrelations-bom_usage         = i_mast-stlan. " BOM用途
    ls_materialrelations-alternative_bom   = i_mast-stlal. " 备选物料清单
    INSERT ls_materialrelations INTO TABLE lt_materialrelations.

    LOOP AT it_stpo INTO DATA(ls_stpo).
      DATA(l_tabix) = sy-tabix * 10.

      " OBJECT ID
      DATA l_item_object_id TYPE cs_object_id.
      l_item_object_id = |ITEM{ l_tabix }|.

      CLEAR ls_items.
      ls_items-bom_group_identification = l_uuid.
      ls_items-object_type     = 'ITM'.
      ls_items-object_id       = l_item_object_id.
      ls_items-item_no         = |{ ls_stpo-posnr ALPHA = IN }|. " 行号
      ls_items-component       = ls_stpo-idnrk. " 组件物料
      ls_items-comp_qty        = ls_stpo-menge. " 组件数量
      ls_items-comp_unit       = ls_stpo-meins. " 组件计量单位
      ls_items-ltxt_lang       = '1'.
      ls_items-valid_from_date = ls_stpo-datuv. " 有效起始日期
      ls_items-change_no       = ls_stpo-aennr. " 变更号
      INSERT ls_items INTO TABLE lt_items.

      CLEAR ls_itemassignments.
      ls_itemassignments-bom_group_identification = l_uuid.
      ls_itemassignments-sub_object_type   = 'ITM'.
      ls_itemassignments-sub_object_id     = l_item_object_id.
      ls_itemassignments-super_object_type = 'BOM'.
      ls_itemassignments-super_object_id   = l_head_object_id.
      ls_itemassignments-valid_from_date   = sy-datum.
      ls_itemassignments-function = l_funciton.
      INSERT ls_itemassignments INTO TABLE lt_itemassignments.
    ENDLOOP.

    CALL FUNCTION 'BAPI_MATERIAL_BOM_GROUP_CREATE'
      EXPORTING
        all_error         = 'X'
      TABLES
        bomgroup          = lt_bomgroup
        variants          = lt_variants
        items             = lt_items
        materialrelations = lt_materialrelations
        itemassignments   = lt_itemassignments
        return            = et_bapiret2[].

    CLEAR l_mtype.
    CLEAR l_msg.
    LOOP AT et_bapiret2 INTO DATA(ls_return) WHERE type CA 'AXE'.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
      WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
      INTO l_msg.
      e_msg = |{ e_msg }{ l_msg };|.
    ENDLOOP.

    IF sy-subrc = 0.
      e_mtype = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO e_msg.
      MESSAGE e_msg TYPE 'S' DISPLAY LIKE 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      e_mtype = 'S'.
      e_msg = |已处理|.
      MESSAGE e_msg TYPE 'S'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDFUNCTION.

  ```

</details>

## (P) WBS BOM

WBS BOM用 ***CSAP_BOM_READ*** 读取，***CSAP_BOM_CREATE*** 创建， ***CSAP_BOM_MAINTAIN*** 维护

<details>
  <summary>WBS BOM维护函数</summary>

  ``` abap

  FUNCTION zfm_wbs_bom_maintain
    IMPORTING
      i_prps TYPE prps
      i_prst TYPE prst
      i_stko TYPE stko
    EXPORTING
      e_mtype TYPE bapi_mtype
      e_msg TYPE bapi_msg
      et_bapiret2 TYPE bapiret2_t
    TABLES
      it_stpo LIKE stpo.



    DATA:
      ls_ecsin TYPE csin,
      ls_stzub TYPE stzub,
      lt_stkob TYPE STANDARD TABLE OF stkob,
      ls_stkob TYPE stkob,
      lt_stpob TYPE STANDARD TABLE OF stpob,
      ls_stpob TYPE stpob.

    CLEAR ls_ecsin.
    ls_ecsin-matnr = i_prst-matnr.
    ls_ecsin-werks = i_prst-werks.
    ls_ecsin-pspnr = i_prps-pspnr. " WBS编号
    ls_ecsin-stlty = i_stko-stlty. " BOM类型
    ls_ecsin-stlan = i_prst-stlan. " BOM用途
    " 数量
    IF i_stko-bmeng IS NOT INITIAL.
      ls_ecsin-emeng = i_stko-bmeng.
    ELSE.
      ls_ecsin-emeng = 1.
    ENDIF.
    ls_ecsin-aennr = i_prps-aennr. " 变更号

    CLEAR ls_stkob.
    ls_stkob-stlty = i_stko-stlty. " BOM类型
    ls_stkob-bmeng = i_stko-bmeng.
    ls_stkob-stktx = i_stko-stktx.

    CLEAR lt_stpob.
    LOOP AT it_stpo INTO DATA(ls_stpo).
      CLEAR ls_stpob.
      ls_stpob-posnr = ls_stpo-posnr.
      ls_stpob-datuv = ls_stpo-datuv.
      ls_stpob-stlty = ls_stpo-stlty.
      ls_stpob-postp = 'L'.
      ls_stpob-idnrk = ls_stpo-idnrk.
      ls_stpob-menge = ls_stpo-menge.
      ls_stpob-meins = ls_stpo-meins.
      ls_stpob-aennr = ls_stpo-aennr.
      ls_stpob-sortf = ls_stpo-sortf. " 工序

      ls_stpob-zspec1 = ls_stpo-zspec1.
      ls_stpob-zspec2 = ls_stpo-zspec2.
      ls_stpob-zspec3 = ls_stpo-zspec3.

      " 供应商/牌号/产地不为空的时候
      IF ls_stpo-zspec1 IS NOT INITIAL
      OR ls_stpo-zspec2 IS NOT INITIAL
      OR ls_stpo-zspec3 IS NOT INITIAL
      .
        ls_stpob-dspst = '1'.
      ENDIF.

      INSERT ls_stpob INTO TABLE lt_stpob.
    ENDLOOP.

    " 检查是否已存在
    DATA:
      lt_stkob_old TYPE STANDARD TABLE OF stkob,
      lt_stpob_old TYPE STANDARD TABLE OF stpob.
    CALL FUNCTION 'CSAI_BOM_READ'
      EXPORTING
        ecsin   = ls_ecsin
      TABLES
        t_stkob = lt_stkob_old
        t_stpob = lt_stpob_old
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

    IF sy-subrc = 0.
      " 抬头只有描述允许被修改
      READ TABLE lt_stkob_old INTO DATA(ls_stkob_old) INDEX 1.
      ls_stkob_old-stktx = ls_stkob-stktx.
      ls_stkob = ls_stkob_old.

      SORT lt_stpob BY posnr.
      SORT lt_stpob_old BY posnr.

      " 修改
      LOOP AT lt_stpob REFERENCE INTO DATA(lr_stpob).
        lr_stpob->vbkz = lr_stpob->mvbkz  = 'I'. " 新增
        READ TABLE lt_stpob_old INTO DATA(ls_stpob_upd) WITH KEY posnr = lr_stpob->posnr BINARY SEARCH.
        IF sy-subrc = 0.
          DELETE lt_stpob_old INDEX sy-tabix.
          ls_stpob_upd-vbkz = ls_stpob_upd-mvbkz = 'U'. " 修改
          ls_stpob_upd-posnr  = lr_stpob->posnr .
          ls_stpob_upd-stlty  = lr_stpob->stlty .
          ls_stpob_upd-datuv  = lr_stpob->datuv .
          ls_stpob_upd-postp  = lr_stpob->postp .
          ls_stpob_upd-idnrk  = lr_stpob->idnrk .
          ls_stpob_upd-menge  = lr_stpob->menge .
          ls_stpob_upd-meins  = lr_stpob->meins .
          ls_stpob_upd-aennr  = lr_stpob->aennr .
          ls_stpob_upd-sortf  = lr_stpob->sortf .
          ls_stpob_upd-zspec1 = lr_stpob->zspec1.
          ls_stpob_upd-zspec2 = lr_stpob->zspec2.
          ls_stpob_upd-zspec3 = lr_stpob->zspec3.
          ls_stpob_upd-dspst  = lr_stpob->dspst .
          lr_stpob->* = ls_stpob_upd.
  *        " 用子项目，而不是直接修改
  *        ls_stpob_upd-vgknt = ls_stpob_upd-stlkn.
  *        ls_stpob_upd-vgpzl = ls_stpob_upd-stpoz.
  *        CLEAR ls_stpob_upd-stlkn.
  *        CLEAR ls_stpob_upd-stpoz.
        ENDIF.
      ENDLOOP.

      " 删除多余的旧项目
      LOOP AT lt_stpob_old INTO DATA(ls_stpob_del).
        ls_stpob_del-lkenz = abap_true.
        ls_stpob_del-vbkz  = ls_stpob_upd-mvbkz  = 'D' . " 删除标识
        INSERT ls_stpob_del INTO TABLE lt_stpob.
      ENDLOOP.

      SORT lt_stpob BY posnr.

      " 维护BOM
      CALL FUNCTION 'CSAI_BOM_MAINTAIN'
        EXPORTING
          ecsin   = ls_ecsin
          estkob  = ls_stkob
          estzub  = ls_stzub
        IMPORTING
          astzub  = ls_stzub
        TABLES
          t_stpob = lt_stpob
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.
    ELSE.
      " 新增BOM
      CALL FUNCTION 'CSAI_BOM_CREATE'
        EXPORTING
          ecsin   = ls_ecsin
          estkob  = ls_stkob
          estzub  = ls_stzub
        IMPORTING
          astlnr  = ls_stzub-stlnr
        TABLES
          t_stpob = lt_stpob
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.
    ENDIF.

    IF sy-subrc <> 0 OR sy-msgty = 'E'.
      e_mtype = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO e_msg
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      e_mtype = 'S'.
      e_msg = |已处理|.
      MESSAGE e_msg TYPE 'S'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDFUNCTION.

  ```

</details>
