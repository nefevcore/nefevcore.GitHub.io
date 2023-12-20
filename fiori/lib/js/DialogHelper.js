sap.ui.define([
    "sap/ui/core/Fragment"
], function (Fragment) {
    "use strict";

    var i18n = sap.ui.getCore().getLibraryResourceBundle("sap.m")

    return {
        /**
         * 弹窗
         * @param {Object} oParameter
         * @param {sap.ui.core.mvc.Controller} oParameter.controller
         * @param {string} [oParameter.fragment]
         * @param {(oControl) => void} [oParameter.fragmentLoad]
         * @param {(oEvent: sap.ui.base.Event) => void} [oParameter.ok]
         * @param {(oEvent: sap.ui.base.Event) => void} [oParameter.close]
         * @param {sap.ui.core.Control} [oParameter.control]
         * @param {sap.m.$DialogSettings} [oParameter.dialog]
         */
        open: function (oParameter) {
            this._oParameter = oParameter;

            var oPromise;
            if (!!(oParameter.fragment)) {
                oPromise = Fragment.load({
                    name: oParameter.fragment,
                    controller: oParameter.controller
                }).then(function (oControl) {
                    if (that._oParameter.fragmentLoad)
                        that._oParameter.fragmentLoad(oControl)
                    return oControl;
                });
            }
            if (!!(oParameter.control)) {
                oPromise = Promise.resolve(oParameter.control)
            }

            var that = this;
            oPromise.then(function (oControl) {
                // Fragment里面直接写Dialog的情况
                if (oControl instanceof sap.m.Dialog)
                    return oControl;
                var oDialog = that._createDialog();
                oDialog.addContent(oControl);
                return oDialog;
            }).then(function (oDialog) {
                // 关闭后摧毁，没必要考虑那点性能问题
                oDialog.attachAfterClose(function (oEvent) {
                    oEvent.getSource().destroy();
                })
                that._oParameter.controller.getView().addDependent(oDialog);
                return oDialog;
            }).then(function (oDialog) {
                oDialog.open();
            });
        },

        /**
         * 
         */
        _createDialog: function () {
            // 默认OK和关闭两个按钮
            var oSetting = this._oParameter.dialog || {}
            oSetting.beginButton = this.createOkButton(this._oParameter.ok);
            oSetting.endButton = this.createCloseButton(this._oParameter.close);
            return new sap.m.Dialog(oSetting);
        },

        /**
         * 
         * @param {(any)=>void} [fn]
         * @param {Object} [oButtonSetting] 
         */
        createOkButton: function (fn, oButtonSetting) {
            var oButtonSetting = oButtonSetting || {};
            oButtonSetting.type = oButtonSetting.type || "Emphasized";
            oButtonSetting.text = oButtonSetting.text || i18n.getText("MSGBOX_OK");
            oButtonSetting.press = function (oEvent) {
                if (fn)
                    fn(oEvent);
                if (oEvent.bPreventDefault)
                    return;
                oEvent.getSource().getParent().close();
            }
            return new sap.m.Button(oButtonSetting)
        },

        /**
         * 
         * @param {(any)=>void} [fn]
         * @param {Object} [oButtonSetting] 
         */
        createCloseButton: function (fn, oButtonSetting) {
            var oButtonSetting = oButtonSetting || {};
            oButtonSetting.text = oButtonSetting.text || i18n.getText("MSGBOX_CLOSE");
            oButtonSetting.press = function (oEvent) {
                if (fn)
                    fn(oEvent)
                if (oEvent.bPreventDefault)
                    return;
                oEvent.getSource().getParent().close();
            }
            return new sap.m.Button(oButtonSetting)
        }
    };
});