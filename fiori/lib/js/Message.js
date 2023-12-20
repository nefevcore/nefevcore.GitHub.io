sap.ui.define([
    "sap/m/MessageBox",
    "sap/ui/util/XMLHelper"
], function (MessageBox, XMLHelper) {
    "use strict";

    var rbSapM = sap.ui.getCore().getLibraryResourceBundle("sap.m")

    return {
        /**
         * toast
         * @param {String} sMsg 
         * @param {Object} [oParameter] 
         */
        toast: function (sMsg, oParameter) {
            sap.m.MessageToast.show(sMsg, oParameter);
        },

        /**
         * @param {String} sMsg 
         * @param {any} [actions] 
         * @returns {Promise}
         */
        success: function (sMsg, actions) {
            return this.showMsgAsync(sMsg, "success", actions);
        },

        /**
         * @param {String} sMsg 
         * @param {any} [actions]
         * @returns {Promise}
         */
        error: function (sMsg, actions) {
            return this.showMsgAsync(sMsg, "error", actions);
        },

        /**
         * @param {String} sMsg 
         * @param {any} [actions] 
         * @returns {Promise}
         */
        warning: function (sMsg, actions) {
            return this.showMsgAsync(sMsg, "warning", actions);
        },

        /**
         * @param {String} sMsg 
         * @param {any} [actions] 
         * @returns {Promise}
         */
        confirm: function (sMsg, actions) {
            var that = this;
            var sActionOK = sap.m.MessageBox.Action.OK;
            if (actions && actions.length > 0)
                sActionOK = actions[0];
            return new Promise(function (resolve, reject) {
                that.showMsgAsync(sMsg, "confirm", actions).then(function (sAction) {
                    if (sAction === sActionOK)
                        resolve();
                    else
                        reject();
                });
            });
        },

        /**
         * 异步弹窗
         * @param {String} sType 弹窗类型
         * @param {String} sMsg 消息文本
         * @param {any} [actions] 消息文本
         * @returns {Promise}
         */
        showMsgAsync: function (sMsg, sType, actions) {
            return new Promise(function (resolve) {
                MessageBox[sType || "show"](sMsg, {
                    actions: actions,
                    onClose: function (sAction) {
                        resolve(sAction);
                    }
                });
            });
        },

        /** 
         * 解析并显示后端错误信息
         * @param {Object} 调用odata服务返回的错误消息
         * @returns {Promise}
         **/
        showOdataError: function (error) {
            return this.error(this.parseOdataError(error));
        },

        /**
         * 解析odata错误信息
         * @param {Object} 调用odata服务返回的错误消息
         * @returns {String}
         */
        parseOdataError: function (error) {
            var sErr;
            try {
                var oJsonData = JSON.parse(error.responseText);
                // 通过JSONModel对象操作
                // var oJsonModel = new sap.ui.model.json.JSONModel();
                // oJsonModel.setData(oJsonData);
                // sErr = oJsonModel.getProperty("/error/message/value");
                // 直接操作JSON对象
                sErr = oJsonData.error.message.value;
            } catch (e) {
                // 500错误返回XML格式报文
                var oXMLDocument = XMLHelper.parse(error.responseText);
                // 通过XMLModel操作
                // var oXmlModel = new sap.ui.model.xml.XMLModel();
                // oXmlModel.setData(oXMLDocument);
                // sErr = oXmlModel.getProperty("/message")
                // 直接操作Document对象
                // document -> [error] -> [code, message] -> [text]
                sErr = oXMLDocument.childNodes[0].childNodes[1].childNodes[0].nodeValue;
            }
            return sErr;
        },

        /**
         * 展示消息列表
         * 
         * @param {object} oParamaters 
         * @param {any[]} [oParamaters.data] 
         * @param {sap.m.$MessageViewSettings} [oParamaters.messageView] 
         * @param {sap.m.$MessageItemSettings} [oParamaters.messageItem] 
         * @param {sap.m.$DialogSettings} [oParamaters.dialog] 
         */
        showMsgTable: function (oParamaters) {
            var oMessageItemSetting = oParamaters.messageItem || {};
            var oMessageItem = new sap.m.MessageItem(oMessageItemSetting);

            var oMessageViewSetting = oParamaters.messageView || {};
            oMessageViewSetting.items = {
                path: "/",
                template: oMessageItem
            }
            var oMessageView = new sap.m.MessageView(oMessageViewSetting);
            oMessageView.setModel(new sap.ui.model.json.JSONModel(oParamaters.data || []));

            var oDialogSetting = oParamaters.dialog || {};
            oDialogSetting.content = oMessageView;
            oDialogSetting.contentHeight ??= "60%";
            oDialogSetting.contentWidth ??= "80%";
            oDialogSetting.resizable ??= true;
            oDialogSetting.verticalScrolling ??= false;
            oDialogSetting.endButton = new sap.m.Button({
                text: rbSapM.getText("MSGBOX_CLOSE"),
                press: function (oEvent) {
                    oEvent.getSource().getParent().close();
                }
            })
            oDialogSetting.afterClose = function (oEvent) {
                oEvent.getSource().destroy();
                if (oDialogSetting.resolve)
                    oDialogSetting.resolve();
            }
            var oDialog = new sap.m.Dialog(oDialogSetting);
            oDialog.open();

            return new Promise(function (resolve) {
                oDialogSetting.resolve = resolve;
            });
        }
    };
});