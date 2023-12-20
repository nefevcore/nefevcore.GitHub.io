sap.ui.define([
    "sap/ui/Device"
], function (Device) {
    "use strict";

    return {
        /**
         * 返回值
         * @typedef ValueResult
         * @property {Object[]} aSelectedItems
         * @property {Token[]} aTokens
         */

        /**
         * 打开搜索帮助弹窗，并获取返回值
         * @param {Object} oParameter
         * @param {sap.ui.core.mvc.Controller} oParameter.controller 
         * @param {sap.ui.model.odata.v2.ODataModel} oParameter.model
         * @param {string} oParameter.entitySet entitySet
         * @param {Object} [oParameter.filterData] checkout FAQ.7 (https://sapui5.hana.ondemand.com/sdk/#/api/sap.ui.comp.smartfilterbar.SmartFilterBar%23faq)
         * @param {Object|sap.ui.comp.valuehelpdialog.$ValueHelpDialogSettings} [oParameter.valueHelpDialog]
         * @param {Object|sap.ui.comp.smartfilterbar.$SmartFilterBarSettings} [oParameter.smartFilterBar]
         * @param {Object|sap.ui.comp.smarttable.$SmartTableSettings} [oParameter.smartTable]
         * @returns {Promise<ValueResult>}
         */
        openAsync: function (oParameter) {
            // 每次调用生成新的ID
            this._count = this._count || 0;
            this._count++;
            this._sSmartFilterBarID = "id__CustomValueHelpSmartFilterBar" + this._count;
            this._sSmartTableID = "id__CustomValueHelpSmartTable" + this._count;
            // 
            this._oParameter = oParameter || {};
            this._setValueHelpDialog();
            this._setSmartFilterBar();
            this._setSmartTable();
            this._oValueHelpDialog.open();
            var that = this;
            return new Promise(function (resolve) {
                that._oParameter.resolve = resolve;
            });
        },

        /**
         * Create ValueHelpDialog Contorl
         *
         * @private
         */
        _setValueHelpDialog: function () {
            /** @type {sap.ui.comp.valuehelpdialog.$ValueHelpDialogSettings} */
            var oSetting = this._oParameter.valueHelpDialog || {};
            oSetting.ok = this._onValueHelpOkPress.bind(this);
            oSetting.cancel = this._onValueHelpCancelPress.bind(this);
            oSetting.afterClose = this._onValueHelpAfterClose.bind(this);
            this._oValueHelpDialog = new sap.ui.comp.valuehelpdialog.ValueHelpDialog(oSetting);
            this._oValueHelpDialog.setModel(this._oParameter.model);
            this._oValueHelpDialog.addStyleClass("sapUiSizeCompact");
            if (oSetting.supportMultiselect && this._oParameter.tokens)
                this._oValueHelpDialog.setTokens(this._oParameter.tokens);
            this._oParameter.controller.getView().addDependent(this._oValueHelpDialog);
        },

        /**
         * Create SmartFilterBar Contorl
         *
         * @private
         */
        _setSmartFilterBar: function () {
            /** @type {sap.ui.comp.smartfilterbar.$SmartFilterBarSettings} */
            var oSetting = this._oParameter.smartFilterBar || {};
            oSetting.entitySet = this._oParameter.entitySet;
            oSetting.enableBasicSearch ??= true; // 默认启动基础搜索
            oSetting.advancedMode ??= Device.system.phone;
            this._oSmartFilterBar = new sap.ui.comp.smartfilterbar.SmartFilterBar(this._sSmartFilterBarID, oSetting);
            this._oSmartFilterBar.attachInitialized(function (oEvent) {
                if (this._oParameter.filterData)
                    this._oSmartFilterBar.setFilterData(this._oParameter.filterData, true);
                if (oSetting.initialized)
                    oSetting.initialized(oEvent)
            }.bind(this));
            this._oValueHelpDialog.setFilterBar(this._oSmartFilterBar);
        },

        /**
         * Create SmartTable Contorl
         *
         * @private
         */
        _setSmartTable: function () {
            /** @type {sap.ui.comp.smarttable.$SmartTableSettings} */
            var oSetting = this._oParameter.smartTable || {};
            oSetting.entitySet = this._oParameter.entitySet;
            oSetting.smartFilterId = this._sSmartFilterBarID;
            oSetting.tableType ??= Device.system.phone ? "ResponsiveTable" : "Table";
            oSetting.useExportToExcel ??= false;
            oSetting.useVariantManagement ??= false;
            oSetting.useTablePersonalisation ??= false;
            oSetting.enableAutoBinding ??= true;
            oSetting.placeToolbarInTable ??= false;
            // 对齐标准，不允许进行过滤操作
            var fnInitialise = oSetting.initialise;
            oSetting.initialise = function (oEvent) {
                oEvent.getSource().getTable().getColumns().forEach(function (oColumn) {
                    oColumn.setProperty("filterProperty", "");
                });
                if (fnInitialise)
                    fnInitialise(oEvent);
            }
            this._oSmartTable = new sap.ui.comp.smarttable.SmartTable(this._sSmartTableID, oSetting);
            this._oSmartTable.getToolbar().setVisible(false);
            this._oSmartTable.setLayoutData(new sap.m.FlexItemData({ growFactor: 1 }));
            if (Device.system.desktop)
                this._oSmartTable.getTable().setSelectionBehavior("Row");
            this._oValueHelpDialog.setTable(this._oSmartTable);
        },

        /**
         * Press Ok
         *
         * @private
         */
        _onValueHelpOkPress: function (oEvent) {
            if (this._oParameter.resolve) {
                var aTokens = oEvent.getParameter("tokens");
                this._oValueHelpDialog.getTableAsync().then(function (oTable) {
                    var aIndices = oTable.getSelectedIndices();
                    var aSelectedItems = [];
                    for (let i = 0; i < aIndices.length; i++)
                        aSelectedItems.push(oTable.getContextByIndex(aIndices[i]).getObject());
                    this._oParameter.resolve({ aSelectedItems, aTokens });
                    this._oValueHelpDialog.close();
                }.bind(this));
            } else {
                this._oValueHelpDialog.close();
            }
        },

        /**
         * Press Cancel
         *
         * @private
         */
        _onValueHelpCancelPress: function (oEvent) {
            this._oValueHelpDialog.close();
        },

        /**
         * After Dialog close
         *
         * @private
         */
        _onValueHelpAfterClose: function (oEvent) {
            this._oValueHelpDialog.destroy();
        },

        /**
         * 回写值到控件对象
         *
         * @param {sap.ui.base.EventProvider} oControl
         * @param {Object} oParamaters
         */
        writeBackInputControl: function (oControl, oParamaters) {
            if (oControl instanceof sap.m.MultiInput) {
                /** @type {sap.m.MultiInput} */
                var oMultiInput = oControl;
                oMultiInput.removeAllTokens();
                for (var i = 0; i < oParamaters.aTokens.length; i++)
                    oMultiInput.addToken(oParamaters.aTokens[i]);
            } else if (oControl instanceof sap.m.Input) {
                /** @type {sap.m.Input} */
                var oInput = oControl;
                oInput.setValue(oParamaters.aTokens[0].getKey());
            }
        }
    };
});