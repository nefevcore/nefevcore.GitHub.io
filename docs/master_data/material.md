# 物料

## 物料维护

- ***BAPI_MATERIAL_SAVEDATA***，每次只能扩展一个工厂。

- ***BAPI_MATERIAL_SAVEREPLICA***，允许扩展多个工厂，下面使用该函数维护物料。

### 更新失败的情况

单独更新计划价格1（成本核算视图2）失败，由于

### 代码

<details>
<summary>函数形式</summary>

```ABAP
FUNCTION zfm_demo_material_save_2
 " You can use the template 'functionModuleParameter' to add here the signature!
.


*&---------------------------------------------------------------------*
*& 批导数据
*&---------------------------------------------------------------------*
  " 演示用，假设这是程序的入参和出参
  TYPES:
    BEGIN OF ty_data,
      matnr TYPE mara-matnr,
      mbrsh TYPE mara-mbrsh,
      mtart TYPE mara-mtart,
      maktx TYPE makt-maktx,
      werks TYPE werks_d,
      vkorg TYPE vkorg,
      mtype TYPE bapi_mtype,
      msg   TYPE bapi_msg,
    END OF ty_data.
  TYPES tt_data TYPE STANDARD TABLE OF ty_data.

  DATA lt_data TYPE tt_data.
  DATA l_matnr TYPE matnr.
  DATA l_mtype TYPE bapi_mtype.
  DATA l_msg TYPE bapi_msg.

*&---------------------------------------------------------------------*
*& BAPI数据声明
*&---------------------------------------------------------------------*
  DATA l_global_data TYPE bapie1global_data.
  DATA:
    lt_headdata             TYPE STANDARD TABLE OF bapie1matheader,
    lt_clientdata           TYPE STANDARD TABLE OF bapie1mara,
    lt_clientdatax          TYPE STANDARD TABLE OF bapie1marax,
    lt_plantdata            TYPE STANDARD TABLE OF bapie1marc,
    lt_plantdatax           TYPE STANDARD TABLE OF bapie1marcx,
    lt_forecastparameters   TYPE STANDARD TABLE OF bapie1mpop,
    lt_forecastparametersx  TYPE STANDARD TABLE OF bapie1mpopx,
    lt_planningdata         TYPE STANDARD TABLE OF bapie1mpgd,
    lt_planningdatax        TYPE STANDARD TABLE OF bapie1mpgdx,
    lt_storagelocationdata  TYPE STANDARD TABLE OF bapie1mard,
    lt_storagelocationdatax TYPE STANDARD TABLE OF bapie1mardx,
    lt_valuationdata        TYPE STANDARD TABLE OF bapie1mbew,
    lt_valuationdatax       TYPE STANDARD TABLE OF bapie1mbewx,
    lt_warehousenumberdata  TYPE STANDARD TABLE OF bapie1mlgn,
    lt_warehousenumberdatax TYPE STANDARD TABLE OF bapie1mlgnx,
    lt_salesdata            TYPE STANDARD TABLE OF bapie1mvke,
    lt_salesdatax           TYPE STANDARD TABLE OF bapie1mvkex,
    lt_storagetypedata      TYPE STANDARD TABLE OF bapie1mlgt,
    lt_storagetypedatax     TYPE STANDARD TABLE OF bapie1mlgtx,
    lt_materialdescription  TYPE STANDARD TABLE OF bapie1makt,
    lt_unitsofmeasure       TYPE STANDARD TABLE OF bapie1marm,
    lt_unitsofmeasurex      TYPE STANDARD TABLE OF bapie1marmx,
    lt_internationalartnos  TYPE STANDARD TABLE OF bapie1mean,
    lt_materiallongtext     TYPE STANDARD TABLE OF bapie1mltx,
    lt_taxclassifications   TYPE STANDARD TABLE OF bapie1mlan,
    lt_prtdata              TYPE STANDARD TABLE OF bapie1mfhm,
    lt_prtdatax             TYPE STANDARD TABLE OF bapie1mfhmx,
    lt_extensionin          TYPE STANDARD TABLE OF bapie1parex,
    lt_extensioninx         TYPE STANDARD TABLE OF bapie1parexx,
    lt_forecastvalues       TYPE STANDARD TABLE OF bapie1mprw,
    lt_unplndconsumption    TYPE STANDARD TABLE OF bapie1mveu,
    lt_totalconsumption     TYPE STANDARD TABLE OF bapie1mveg.
  DATA:
    ls_headdata             TYPE bapie1matheader,
    ls_clientdata           TYPE bapie1mara,
    ls_clientdatax          TYPE bapie1marax,
    ls_plantdata            TYPE bapie1marc,
    ls_plantdatax           TYPE bapie1marcx,
    ls_forecastparameters   TYPE bapie1mpop,
    ls_forecastparametersx  TYPE bapie1mpopx,
    ls_planningdata         TYPE bapie1mpgd,
    ls_planningdatax        TYPE bapie1mpgdx,
    ls_storagelocationdata  TYPE bapie1mard,
    ls_storagelocationdatax TYPE bapie1mardx,
    ls_valuationdata        TYPE bapie1mbew,
    ls_valuationdatax       TYPE bapie1mbewx,
    ls_warehousenumberdata  TYPE bapie1mlgn,
    ls_warehousenumberdatax TYPE bapie1mlgnx,
    ls_salesdata            TYPE bapie1mvke,
    ls_salesdatax           TYPE bapie1mvkex,
    ls_storagetypedata      TYPE bapie1mlgt,
    ls_storagetypedatax     TYPE bapie1mlgtx,
    ls_materialdescription  TYPE bapie1makt,
    ls_unitsofmeasure       TYPE bapie1marm,
    ls_unitsofmeasurex      TYPE bapie1marmx,
    ls_internationalartnos  TYPE bapie1mean,
    ls_materiallongtext     TYPE bapie1mltx,
    ls_taxclassifications   TYPE bapie1mlan,
    ls_prtdata              TYPE bapie1mfhm,
    ls_prtdatax             TYPE bapie1mfhmx,
    ls_extensionin          TYPE bapie1parex,
    ls_extensioninx         TYPE bapie1parexx,
    ls_forecastvalues       TYPE bapie1mprw,
    ls_unplndconsumption    TYPE bapie1mveu,
    ls_totalconsumption     TYPE bapie1mveg.
  DATA:
    lt_return TYPE STANDARD TABLE OF bapie1ret2,
    ls_return TYPE bapiret2.

*&---------------------------------------------------------------------*
*& 分批处理
*&---------------------------------------------------------------------*
  LOOP AT lt_data INTO DATA(ls_data)
    GROUP BY (
      matnr = ls_data-matnr
    ) INTO DATA(ls_data_grp).

    " 任取一行作为抬头
    LOOP AT GROUP ls_data_grp INTO DATA(ls_data_head).
      EXIT.
    ENDLOOP.

*&---------------------------------------------------------------------*
*& 物料号
*&---------------------------------------------------------------------*
    " 大多都是外部指定物料号，如需流水可自行调整
    CLEAR l_matnr.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = ls_data_head-matnr
      IMPORTING
        output       = l_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

*&---------------------------------------------------------------------*
*& 设置需要导入的物料和视图
*&---------------------------------------------------------------------*
    CLEAR ls_headdata.
    ls_headdata-material_long   = l_matnr. " 物料
    ls_headdata-ind_sector      = ls_data_head-mbrsh. " 行业领域
    ls_headdata-matl_type       = ls_data_head-mtart. " 物料类型
    ls_headdata-basic_view      = abap_true.
    ls_headdata-sales_view      = abap_true.
    ls_headdata-purchase_view   = abap_true.
    ls_headdata-mrp_view        = abap_true.
    ls_headdata-forecast_view   = abap_true.
    ls_headdata-work_sched_view = abap_true.
    ls_headdata-prt_view        = abap_true.
    ls_headdata-storage_view    = abap_true.
    ls_headdata-warehouse_view  = abap_true.
    ls_headdata-quality_view    = abap_true.
    ls_headdata-account_view    = abap_true.
    ls_headdata-cost_view       = abap_true.
    INSERT ls_headdata INTO TABLE lt_headdata.

*&---------------------------------------------------------------------*
*& 基础视图
*&---------------------------------------------------------------------*
    DATA ls_mara_ga TYPE mara.
    DATA ls_bapi_mara_ga TYPE bapi_mara_ga.

    CLEAR ls_mara_ga.
    CLEAR ls_bapi_mara_ga.
    ls_mara_ga = CORRESPONDING #( ls_data_head ).
    CALL FUNCTION 'MAP2E_MARA_TO_BAPI_MARA_GA'
      EXPORTING
        mara         = ls_mara_ga
      CHANGING
        bapi_mara_ga = ls_bapi_mara_ga.

    CLEAR ls_clientdata.
    ls_clientdata = CORRESPONDING #( ls_bapi_mara_ga ).
    ls_clientdata-material_long = l_matnr.

    CLEAR ls_clientdatax.
    PERFORM map_data_to_datax USING 'BAPIE1MARAX' ls_clientdata CHANGING ls_clientdatax.

    INSERT ls_clientdata INTO TABLE lt_clientdata.
    INSERT ls_clientdatax INTO TABLE lt_clientdatax.

*&---------------------------------------------------------------------*
*& 工厂视图
*&---------------------------------------------------------------------*
    DATA ls_marc_ga TYPE marc.
    DATA ls_bapi_marc_ga TYPE bapi_marc_ga.

    LOOP AT GROUP ls_data_grp INTO ls_data.
      CLEAR ls_marc_ga.
      CLEAR ls_bapi_marc_ga.
      ls_marc_ga = CORRESPONDING #( ls_data ).
      CALL FUNCTION 'MAP2E_MARC_TO_BAPI_MARC_GA'
        EXPORTING
          marc                         = ls_marc_ga
          h_waers                      = 'CNY'
        CHANGING
          bapi_marc_ga                 = ls_bapi_marc_ga
        EXCEPTIONS
          error_converting_curr_amount = 1.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CLEAR ls_plantdata.
      ls_plantdata = CORRESPONDING #( ls_bapi_marc_ga ).
      ls_plantdata-material_long = l_matnr.

      CLEAR ls_plantdatax.
      PERFORM map_data_to_datax USING 'BAPIE1MARCX' ls_plantdata CHANGING ls_plantdatax.

      INSERT ls_plantdata INTO TABLE lt_plantdata.
      INSERT ls_plantdatax INTO TABLE lt_plantdatax.
    ENDLOOP.

    SORT lt_plantdata BY plant.
    SORT lt_plantdatax BY plant.
    DELETE ADJACENT DUPLICATES FROM lt_plantdata COMPARING plant.
    DELETE ADJACENT DUPLICATES FROM lt_plantdatax COMPARING plant.

**&---------------------------------------------------------------------*
**& MPOP
**&---------------------------------------------------------------------*
*  DATA ls_mpop_ga TYPE mpop.
*  DATA ls_bapi_mpop_ga TYPE bapi_mpop_ga.
*
*  LOOP AT GROUP ls_data_grp INTO ls_data.
*    CLEAR ls_mpop_ga.
*    CLEAR ls_bapi_mpop_ga.
*    ls_mpop_ga = CORRESPONDING #( ls_data ).
*    CALL FUNCTION 'MAP2E_MPOP_TO_BAPI_MPOP_GA'
*      EXPORTING
*        mpop         = ls_mpop_ga
*      CHANGING
*        bapi_mpop_ga = ls_bapi_mpop_ga.
*
*    CLEAR ls_forecastparameters.
*    ls_forecastparameters = CORRESPONDING #( ls_bapi_mpop_ga ).
*    ls_forecastparameters-material_long = l_matnr.
*
*    CLEAR ls_forecastparametersx.
*    PERFORM map_data_to_datax USING 'BAPIE1MPOPX' ls_forecastparameters CHANGING ls_forecastparametersx.
*
*    INSERT ls_forecastparameters INTO TABLE lt_forecastparameters.
*    INSERT ls_forecastparametersx INTO TABLE lt_forecastparametersx.
*  ENDLOOP.
*
*  SORT lt_forecastparameters BY plant.
*  SORT lt_forecastparametersx BY plant.
*  DELETE ADJACENT DUPLICATES FROM lt_forecastparameters COMPARING plant.
*  DELETE ADJACENT DUPLICATES FROM lt_forecastparametersx COMPARING plant.

**&---------------------------------------------------------------------*
**& MPGD
**&---------------------------------------------------------------------*
*  DATA ls_mpgd_ga TYPE mpgd.
*  DATA ls_bapi_mpgd_ga TYPE bapi_mpgd_ga.
*
*  LOOP AT GROUP ls_data_grp INTO ls_data.
*    CLEAR ls_mpgd_ga.
*    CLEAR ls_bapi_mpgd_ga.
*    ls_mpgd_ga = CORRESPONDING #( ls_data ).
*    CALL FUNCTION 'MAP2E_MPGD_TO_BAPI_MPGD_GA'
*      EXPORTING
*        mpgd         = ls_mpgd_ga
*      CHANGING
*        bapi_mpgd_ga = ls_bapi_mpgd_ga.
*
*    CLEAR ls_planningdata.
*    ls_planningdata = CORRESPONDING #( ls_bapi_mpgd_ga ).
*    ls_planningdata-material_long = l_matnr.
*
*    CLEAR ls_planningdatax.
*    PERFORM map_data_to_datax USING 'BAPIE1MPGDX' ls_planningdata CHANGING ls_planningdatax.
*
*    INSERT ls_planningdata INTO TABLE lt_planningdata.
*    INSERT ls_planningdatax INTO TABLE lt_planningdatax.
*  ENDLOOP.
*
*  SORT lt_planningdata BY plant.
*  SORT lt_planningdatax BY plant.
*  DELETE ADJACENT DUPLICATES FROM lt_planningdata COMPARING plant.
*  DELETE ADJACENT DUPLICATES FROM lt_planningdatax COMPARING plant.

*&---------------------------------------------------------------------*
*& 库位视图
*&---------------------------------------------------------------------*
    DATA ls_mard_ga TYPE mard.
    DATA ls_bapi_mard_ga TYPE bapi_mard_ga.

    LOOP AT GROUP ls_data_grp INTO ls_data.
      CLEAR ls_mard_ga.
      CLEAR ls_bapi_mard_ga.
      ls_mard_ga = CORRESPONDING #( ls_data ).
      CALL FUNCTION 'MAP2E_MARD_TO_BAPI_MARD_GA'
        EXPORTING
          mard         = ls_mard_ga
        CHANGING
          bapi_mard_ga = ls_bapi_mard_ga.

      CLEAR ls_storagelocationdata.
      ls_storagelocationdata = CORRESPONDING #( ls_bapi_mard_ga ).
      ls_storagelocationdata-material_long = l_matnr.

      CLEAR ls_storagelocationdatax.
      PERFORM map_data_to_datax USING 'BAPIE1MARDX' ls_storagelocationdata CHANGING ls_storagelocationdatax.

      INSERT ls_storagelocationdata INTO TABLE lt_storagelocationdata.
      INSERT ls_storagelocationdatax INTO TABLE lt_storagelocationdatax.
    ENDLOOP.

    SORT lt_storagelocationdata BY plant stge_loc.
    SORT lt_storagelocationdatax BY plant stge_loc.
    DELETE ADJACENT DUPLICATES FROM lt_storagelocationdata COMPARING plant stge_loc.
    DELETE ADJACENT DUPLICATES FROM lt_storagelocationdatax COMPARING plant stge_loc.

*&---------------------------------------------------------------------*
*& 评估视图
*&---------------------------------------------------------------------*
    DATA ls_mbew_ga TYPE mbew.
    DATA ls_bapi_mbew_ga TYPE bapi_mbew_ga.

    LOOP AT GROUP ls_data_grp INTO ls_data.
      CLEAR ls_mbew_ga.
      CLEAR ls_bapi_mbew_ga.
      ls_mbew_ga = CORRESPONDING #( ls_data ).
      CALL FUNCTION 'MAP2E_MBEW_TO_BAPI_MBEW_GA'
        EXPORTING
          mbew                         = ls_mbew_ga
          h_waers                      = 'CNY'
        CHANGING
          bapi_mbew_ga                 = ls_bapi_mbew_ga
        EXCEPTIONS
          error_converting_curr_amount = 1.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR ls_valuationdata.
      ls_valuationdata = CORRESPONDING #( ls_bapi_mbew_ga ).
      ls_valuationdata-material_long = l_matnr.

      CLEAR ls_valuationdatax.
      PERFORM map_data_to_datax USING 'BAPIE1MBEWX' ls_valuationdata CHANGING ls_valuationdatax.

      INSERT ls_valuationdata INTO TABLE lt_valuationdata.
      INSERT ls_valuationdatax INTO TABLE lt_valuationdatax.
    ENDLOOP.

    SORT lt_valuationdata BY val_area val_type.
    SORT lt_valuationdatax BY val_area val_type.
    DELETE ADJACENT DUPLICATES FROM lt_valuationdata COMPARING val_area val_type.
    DELETE ADJACENT DUPLICATES FROM lt_valuationdatax COMPARING val_area val_type.

*&---------------------------------------------------------------------*
*& MLGN
*&---------------------------------------------------------------------*
    DATA ls_mlgn_ga TYPE mlgn.
    DATA ls_bapi_mlgn_ga TYPE bapi_mlgn_ga.

    LOOP AT GROUP ls_data_grp INTO ls_data.
      CLEAR ls_mlgn_ga.
      CLEAR ls_bapi_mlgn_ga.
      ls_mlgn_ga = CORRESPONDING #( ls_data ).
      CALL FUNCTION 'MAP2E_MLGN_TO_BAPI_MLGN_GA'
        EXPORTING
          mlgn         = ls_mlgn_ga
        CHANGING
          bapi_mlgn_ga = ls_bapi_mlgn_ga.

      CLEAR ls_plantdata.
      ls_plantdata = CORRESPONDING #( ls_bapi_mlgn_ga ).
      ls_plantdata-material_long = l_matnr.

      CLEAR ls_plantdatax.
      PERFORM map_data_to_datax USING 'BAPIE1MLGNX' ls_plantdata CHANGING ls_plantdatax.

      INSERT ls_plantdata INTO TABLE lt_plantdata.
      INSERT ls_plantdatax INTO TABLE lt_plantdatax.
    ENDLOOP.

    SORT lt_plantdata BY plant.
    SORT lt_plantdatax BY plant.
    DELETE ADJACENT DUPLICATES FROM lt_plantdata COMPARING plant.
    DELETE ADJACENT DUPLICATES FROM lt_plantdatax COMPARING plant.

*&---------------------------------------------------------------------*
*& 销售视图
*&---------------------------------------------------------------------*
    DATA ls_mvke_ga TYPE mvke.
    DATA ls_bapi_mvke_ga TYPE bapi_mvke_ga.

    LOOP AT GROUP ls_data_grp INTO ls_data.
      CLEAR ls_mvke_ga.
      CLEAR ls_bapi_mvke_ga.
      ls_mvke_ga = CORRESPONDING #( ls_data ).
      CALL FUNCTION 'MAP2E_MVKE_TO_BAPI_MVKE_GA'
        EXPORTING
          mvke         = ls_mvke_ga
        CHANGING
          bapi_mvke_ga = ls_bapi_mvke_ga.

      CLEAR ls_salesdata.
      ls_salesdata = CORRESPONDING #( ls_bapi_mvke_ga ).
      ls_salesdata-material_long = l_matnr.

      CLEAR ls_salesdatax.
      PERFORM map_data_to_datax USING 'BAPIE1MVKEX' ls_salesdata CHANGING ls_salesdatax.

      INSERT ls_salesdata INTO TABLE lt_salesdata.
      INSERT ls_salesdatax INTO TABLE lt_salesdatax.
    ENDLOOP.

    SORT lt_salesdata BY sales_org distr_chan.
    SORT lt_salesdatax BY sales_org distr_chan.
    DELETE ADJACENT DUPLICATES FROM lt_salesdata COMPARING sales_org distr_chan.
    DELETE ADJACENT DUPLICATES FROM lt_salesdatax COMPARING sales_org distr_chan.

*&---------------------------------------------------------------------*
*& MLGT
*&---------------------------------------------------------------------*
    DATA ls_mlgt_ga TYPE mlgt.
    DATA ls_bapi_mlgt_ga TYPE bapi_mlgt_ga.

    LOOP AT GROUP ls_data_grp INTO ls_data.
      CLEAR ls_mlgt_ga.
      CLEAR ls_bapi_mlgt_ga.
      ls_mlgt_ga = CORRESPONDING #( ls_data ).
      CALL FUNCTION 'MAP2E_MLGT_TO_BAPI_MLGT_GA'
        EXPORTING
          mlgt         = ls_mlgt_ga
        CHANGING
          bapi_mlgt_ga = ls_bapi_mlgt_ga.

      CLEAR ls_storagetypedata.
      ls_storagetypedata = CORRESPONDING #( ls_bapi_mlgt_ga ).
      ls_storagetypedata-material_long = l_matnr.

      CLEAR ls_storagetypedatax.
      PERFORM map_data_to_datax USING 'BAPIE1MLGTX' ls_storagetypedata CHANGING ls_storagetypedatax.

      INSERT ls_storagetypedata INTO TABLE lt_storagetypedata.
      INSERT ls_storagetypedatax INTO TABLE lt_storagetypedatax.
    ENDLOOP.

    SORT lt_storagetypedata BY whse_no stge_type.
    SORT lt_storagetypedatax BY whse_no stge_type.
    DELETE ADJACENT DUPLICATES FROM lt_storagetypedata COMPARING whse_no stge_type.
    DELETE ADJACENT DUPLICATES FROM lt_storagetypedatax COMPARING whse_no stge_type.

*&---------------------------------------------------------------------*
*& 物料描述
*&---------------------------------------------------------------------*
    CLEAR ls_materialdescription.
    ls_materialdescription-material_long = l_matnr.
    ls_materialdescription-langu = '1'.
    ls_materialdescription-matl_desc = ls_data_head-maktx.
    INSERT ls_materialdescription INTO TABLE lt_materialdescription.

    SORT lt_materialdescription BY langu.
    DELETE ADJACENT DUPLICATES FROM lt_materialdescription COMPARING langu.

**&---------------------------------------------------------------------*
**& 税
**&---------------------------------------------------------------------*
*    CLEAR ls_taxclassifications.
*    ls_taxclassifications-material_long = l_matnr.
*    ls_taxclassifications-depcountry = ls_data_head-aland.
*    ls_taxclassifications-tax_ind = ls_data_head-taxim.
*
*    IF ls_data_head-taxm1 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_1 = ls_data_head-taxm1.
*      ls_taxclassifications-taxclass_1 = ls_data_head-taxim.
*    ENDIF.
*
*    IF ls_data_head-taxm2 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_2 = ls_data_head-taxm2.
*      ls_taxclassifications-taxclass_2 = ls_data_head-taxim.
*    ENDIF.
*
*    IF ls_data_head-taxm3 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_3 = ls_data_head-taxm3.
*      ls_taxclassifications-taxclass_3 = ls_data_head-taxim.
*    ENDIF.
*
*    IF ls_data_head-taxm4 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_4 = ls_data_head-taxm4.
*      ls_taxclassifications-taxclass_4 = ls_data_head-taxim.
*    ENDIF.
*
*    IF ls_data_head-taxm5 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_5 = ls_data_head-taxm5.
*      ls_taxclassifications-taxclass_5 = ls_data_head-taxim.
*    ENDIF.
*
*    IF ls_data_head-taxm6 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_6 = ls_data_head-taxm6.
*      ls_taxclassifications-taxclass_6 = ls_data_head-taxim.
*    ENDIF.
*
*    IF ls_data_head-taxm7 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_7 = ls_data_head-taxm7.
*      ls_taxclassifications-taxclass_7 = ls_data_head-taxim.
*    ENDIF.
*
*    IF ls_data_head-taxm8 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_8 = ls_data_head-taxm8.
*      ls_taxclassifications-taxclass_8 = ls_data_head-taxim.
*    ENDIF.
*
*    IF ls_data_head-taxm9 IS NOT INITIAL.
*      ls_taxclassifications-tax_type_9 = ls_data_head-taxm9.
*      ls_taxclassifications-taxclass_9 = ls_data_head-taxim.
*    ENDIF.
*
*    INSERT ls_taxclassifications INTO TABLE lt_taxclassifications.

**&---------------------------------------------------------------------*
**& MARM
**&---------------------------------------------------------------------*
*  DATA ls_marm_ga TYPE marm.
*  DATA ls_bapi_marm_ga TYPE bapi_marm_ga.
*
*  LOOP AT GROUP ls_data_grp INTO ls_data.
*    CLEAR ls_marm_ga.
*    CLEAR ls_unitsofmeasure.
*    ls_marm_ga = CORRESPONDING #( ls_data ).
*    CALL FUNCTION 'MAP2E_MARM_TO_BAPIE1MARM'
*      EXPORTING
*        marm       = ls_marm_ga
*      CHANGING
*        bapie1marm = ls_unitsofmeasure.
*    ls_unitsofmeasure-material_long = l_matnr.
*
*    CLEAR ls_unitsofmeasurex.
*    PERFORM map_data_to_datax USING 'BAPIE1MARMX' ls_unitsofmeasure CHANGING ls_unitsofmeasurex.
*
*    INSERT ls_unitsofmeasure INTO TABLE lt_unitsofmeasure.
*    INSERT ls_unitsofmeasurex INTO TABLE lt_unitsofmeasurex.
*  ENDLOOP.
*
*  SORT lt_unitsofmeasure BY alt_unit alt_unit_iso.
*  SORT lt_unitsofmeasurex BY alt_unit alt_unit_iso.
*  DELETE ADJACENT DUPLICATES FROM lt_unitsofmeasure COMPARING alt_unit alt_unit_iso.
*  DELETE ADJACENT DUPLICATES FROM lt_unitsofmeasurex COMPARING alt_unit alt_unit_iso.

*&---------------------------------------------------------------------*
*& 导入物料
*&---------------------------------------------------------------------*
    CLEAR lt_return.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEREPLICA'
      EXPORTING
        noappllog            = l_global_data-no_appl_log
        nochangedoc          = l_global_data-no_change_doc
        testrun              = l_global_data-testrun
        inpfldcheck          = l_global_data-inp_fld_check
      TABLES
        headdata             = lt_headdata
        clientdata           = lt_clientdata
        clientdatax          = lt_clientdatax
        plantdata            = lt_plantdata
        plantdatax           = lt_plantdatax
        forecastparameters   = lt_forecastparameters
        forecastparametersx  = lt_forecastparametersx
        planningdata         = lt_planningdata
        planningdatax        = lt_planningdatax
        storagelocationdata  = lt_storagelocationdata
        storagelocationdatax = lt_storagelocationdatax
        valuationdata        = lt_valuationdata
        valuationdatax       = lt_valuationdatax
        warehousenumberdata  = lt_warehousenumberdata
        warehousenumberdatax = lt_warehousenumberdatax
        salesdata            = lt_salesdata
        salesdatax           = lt_salesdatax
        storagetypedata      = lt_storagetypedata
        storagetypedatax     = lt_storagetypedatax
        materialdescription  = lt_materialdescription
        unitsofmeasure       = lt_unitsofmeasure
        unitsofmeasurex      = lt_unitsofmeasurex
*       internationalartnos  = lt_internationalartnos
*       materiallongtext     = lt_materiallongtext
        taxclassifications   = lt_taxclassifications
*       prtdata              = lt_prtdata
*       prtdatax             = lt_prtdatax
        extensionin          = lt_extensionin
        extensioninx         = lt_extensioninx
*       forecastvalues       = lt_forecastvalues
*       unplndconsumption    = lt_unplndconsumption
*       totalconsumption     = lt_totalconsumption
        returnmessages       = lt_return.

    CLEAR l_mtype.
    CLEAR l_msg.
    LOOP AT lt_return INTO ls_return WHERE type CA 'AXE'.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
          WITH ls_return-message_v1
               ls_return-message_v2
               ls_return-message_v3
               ls_return-message_v4
          INTO ls_return-message.
      l_msg = |{ l_msg }{ ls_return-message }|.
    ENDLOOP.
    IF sy-subrc = 0.
      l_mtype = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      l_matnr = lt_headdata[ 1 ]-material_long.
      l_mtype = 'S'.
      l_msg = '已处理'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

    " 回写消息
    LOOP AT GROUP ls_data_grp REFERENCE INTO DATA(lr_data).
      lr_data->mtype = l_mtype.
      lr_data->msg = l_msg.
    ENDLOOP.

  ENDLOOP.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*& 同名BAPIUPDATE字段赋值
*&---------------------------------------------------------------------*
FORM map_data_to_datax USING i_name TYPE tabname
                             i_data TYPE data
                       CHANGING c_datax TYPE data.

  FIELD-SYMBOLS <wa> TYPE any.
  FIELD-SYMBOLS <wax> TYPE any.

  SELECT
    fieldname,
    rollname
    FROM dd03l
    WHERE tabname = @i_name
    INTO TABLE @DATA(lt_field).

  LOOP AT lt_field INTO DATA(ls_field).
    ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE i_data TO <wa>.
    ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE c_datax TO <wax>.
    IF <wa> IS ASSIGNED AND <wax> IS ASSIGNED.
      IF ls_field-rollname = 'BAPIUPDATE'.
        IF <wa> IS NOT INITIAL AND <wa> <> ''.
          <wax> = abap_true.
        ENDIF.
      ELSE.
        <wax> = <wa>.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

```

</details>

## 物料分类视图

- ***BAPI_OBJCL_CREATE***，创建物料分类视图

- ***BAPI_OBJCL_CHANGE***，修改物料分类视图

- ***BAPI_OBJCL_EXISTENCECHECK***，检查物料是否存在分类视图

- ***BAPI_OBJCL_GETDETAIL***，获取物料分类特征值

<details>
<summary>物料分类视图维护</summary>

```ABAP

```

</details>

## IDOC物料同步

SAP系统间迁移或同步物料，推荐使用IDOC实现，下面讲述实现过程。

<details>
<summary>IDOC同步物料</summary>

```ABAP

```

</details>
