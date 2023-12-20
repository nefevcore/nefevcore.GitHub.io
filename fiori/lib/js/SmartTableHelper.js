sap.ui.define([
], function () {
    "use strict";

    return {
        /**
         * dataReceived的代码现在移动到beforeRebindTable事件里
         * 设置起来还有点麻烦，封装一下
         * @param {(sap.ui.comp.smarttable.SmartTable | sap.ui.base.ManagedObject.ObjectBindingInfo.events)} obj
         * @param {(data: []) => void} fn
         */
        attachDataReceived: function (obj, fn) {
            if (obj instanceof sap.ui.comp.smarttable.SmartTable) {
                var that = this;
                obj.attachBeforeRebindTable(function (oEvent) {
                    that._addEventListener.addEventListener(oEvent.getParameter("bindingParams").events, "dataReceived", fn);
                });
            } else {
                this._addEventListener.addEventListener(obj, "dataReceived", fn);
            }
        },

        /**
         * 修改SmartTable全部列绑定的模型对象
         * 需要注意，通过Entity代理生成的列，只有展示出来才能修改
         * @param {sap.ui.comp.smarttable.SmartTable} oSmartTable
         * @param {String} sModel 
         */
        changeBindingModel: function (oSmartTable, sModel) {
            var that = this;
            this.eachColumns(oSmartTable, function (oColumn) {
                that._changeTemplateBindingModel(oColumn.getTemplate(), sModel);
            })
        },

        /**
         * 修改列模板绑定的模型对象
         * @param {sap.ui.core.Control} oTemplate 
         * @param {String} sModel 
         * @private
         */
        _changeTemplateBindingModel: function (oTemplate, sModel) {
            for (var property in oTemplate.mBindingInfos) {
                if (Object.hasOwnProperty.call(oTemplate.mBindingInfos, property)) {
                    var oBindingInfo = oTemplate.mBindingInfos[property];
                    if (oBindingInfo.parts) {
                        for (var i = 0; i < oBindingInfo.parts.length; i++) {
                            oBindingInfo.parts[i].model = sModel;
                        }
                    }
                }
            }
            // 模板可能包含多个控件，通过递归处理
            if (oTemplate.getItems) {
                var oChildTemplates = oTemplate.getItems();
                for (var i = 0; i < oChildTemplates.length; i++)
                    this._changeTemplateBindingModel(oChildTemplates[i], sModel);
            }
        },

        /**
         * 遍历SmartTable列
         * 需要注意，如果列根据EntitySet推导获取，那么没展示的列不会被选取
         * @param {sap.ui.comp.smarttable.SmartTable} oSmartTable
         * @param {(oColumn: sap.ui.table.Column, sColumnKey: String) => void} fn
         */
        eachColumns: function (oSmartTable, fn) {
            var oTable = oSmartTable.getTable();
            var aColumns = oTable.getColumns();
            for (var i = 0; i < aColumns.length; i++) {
                var sColumnKey = aColumns[i].data("p13nData").columnKey;
                fn(aColumns[i], sColumnKey);
            }
        },

        /**
         * 遍历勾选行
         * @param {sap.ui.comp.smarttable.SmartTable} oSmartTable
         * @param {*} fn 
         */
        eachSelected: function (oSmartTable, fn) {
            if (oSmartTable) {
                var oTable = oSmartTable.getTable();
                oTable.getSelectedIndices().forEach(function (i) {
                    var oItemContext = oTable.getContextByIndex(i)
                    fn(oItemContext);
                });
            }
        },

        /**
         * 获取SmartTable勾选行
         * @param {sap.ui.comp.smarttable.SmartTable} oSmartTable
         * @param {object} [oParamaters]
         * @param {boolean} [oParamaters.all]
         * @param {boolean} [oParamaters.deleteMetedata]
         */
        getSelected: function (oSmartTable, oParamaters) {
            var oSelect = {
                aContexts: [],
                aObjects: []
            }
            var oParamaters = oParamaters || {}
            if (oParamaters.all)
                this.selecteAll(oSmartTable)
            this.eachSelected(oSmartTable, function (oItemContext) {
                var oItem = oItemContext.getModel().getObject(oItemContext.getPath());
                if (oParamaters.deleteMetedata)
                    delete oItem.__metadata;
                oSelect.aContexts.push(oItemContext);
                oSelect.aObjects.push(oItem);
            })
            return oSelect;
        },

        /**
         * 获取SmartTable全部行
         * @param {sap.ui.comp.smarttable.SmartTable} oSmartTable
         */
        hasSelected: function (oSmartTable) {
            return oSmartTable.getTable().getSelectedIndices().length > 0;
        },

        /**
         * 获取SmartTable全部行
         * @param {sap.ui.comp.smarttable.SmartTable} oSmartTable
         */
        selecteAll: function (oSmartTable) {
            oSmartTable.getTable().selectAll();
        },

        /**
         * addEventListener这个名称不太准确，核心代码是装饰模式
         * @private
         */
        _addEventListener: function (obj, event, fn) {
            if (obj[event]) {
                var fnOri = obj[event];
                obj[event] = function () {
                    fnOri.apply(this, arguments);
                    if (fn) fn.apply(this, arguments); // 放fnOri前后是个问题
                }.bind(obj);
                obj[event]["_fnOri"] = fnOri;
            } else {
                obj[event] = fn;
            }
        },

        /**
         * addEventListener的逆向操作
         * @private
         */
        _removeEventListener: function (obj, event) {
            if (obj[event]) {
                if (obj[event]["_fnOri"]) {
                    obj[event] = obj[event]["_fnOri"]
                } else {
                    delete obj[event];
                }
            }
        }
    };
});