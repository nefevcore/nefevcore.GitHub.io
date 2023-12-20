sap.ui.define([
], function (
) {
    "use strict";

    /**
     * 基本上都是重复代码，主要是加了等待和弹窗报错
     */
    return {
        /**
         * 读取EntitySet
         * @param {any} oDataModel
         * @param {string} sEntitySet
         * @param {object} [oParamters]
         */
        read: function (oDataModel, sEntitySet, oParamters) {
            var that = this;
            return new Promise(function (resolve, reject) {
                that.showBusyIndicator(60000, 0); // 等待1分钟
                oParamters = oParamters || {}
                var fnSuccess = oParamters.success || undefined
                var fnError = oParamters.error || undefined
                oParamters.success = function (oData) {
                    that.hideBusyIndicator();
                    if (fnSuccess)
                        fnSuccess(oData)
                    resolve(oData);
                }
                oParamters.error = function (oError) {
                    that.hideBusyIndicator();
                    sap.m.MessageBox.error(that.parseOdataError(oError));
                    if (fnError)
                        fnError(oError)
                    reject(oError);
                }
                oDataModel.read(sEntitySet, oParamters);
            }.bind(this));
        },

        /**
         * 调用后端方法
         * @param {any} oDataModel
         * @param {string} sEntitySet
         * @param {object} [oRequestData]
         * @param {object} [oParamters]
         * @returns 
         */
        request: function (oDataModel, sEntitySet, oRequestData, oParamters) {
            var that = this;
            return new Promise(function (resolve, reject) {
                that.showBusyIndicator(60000, 0); // 等待1分钟
                oParamters = oParamters || {}
                var fnSuccess = oParamters.success || undefined
                var fnError = oParamters.error || undefined
                oParamters.success = function (oData) {
                    that.hideBusyIndicator();
                    if (fnSuccess)
                        fnSuccess(oData)
                    resolve(oData);
                }
                oParamters.error = function (oError) {
                    that.hideBusyIndicator();
                    sap.m.MessageBox.error(that.parseOdataError(oError));
                    if (fnError)
                        fnError(oError)
                    reject(oError);
                }
                oDataModel.create(sEntitySet, oRequestData, oParamters);
            });
        },

        /**
         * 转圈
         * @param {Number} iDuration 
         * @param {Number} iDelay 
         */
        showBusyIndicator: function (iDuration, iDelay) {
            sap.ui.core.BusyIndicator.show(iDelay);
            if (iDuration && iDuration > 0) {
                if (this._sTimeoutId) {
                    clearTimeout(this._sTimeoutId);
                    this._sTimeoutId = null;
                }

                this._sTimeoutId = setTimeout(function () {
                    this.hideBusyIndicator();
                }.bind(this), iDuration);
            }
        },

        /**
         * 转圈
         * @param {Number} iDuration 
         */
        hideBusyIndicator: function (iDuration) {
            if (iDuration && iDuration > 0) {
                if (this._sTimeoutId) {
                    clearTimeout(this._sTimeoutId);
                    this._sTimeoutId = null;
                }

                this._sTimeoutId = setTimeout(function () {
                    sap.ui.core.BusyIndicator.hide();
                }.bind(this), iDuration);
            } else {
                sap.ui.core.BusyIndicator.hide();
            }
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
         * 文件流转blob对象下载
         * @param {any} data 
         * @param {any} type 
         * @param {any} fileName 
         */
        downloadFile: function (data, type, fileName) {
            if (!data) return;
            var blob = new Blob([data], { type: "application/" + type + ";charset=utf-8" });
            // 获取heads中的filename文件名
            var downloadElement = document.createElement('a');
            // 创建下载的链接
            var href = window.URL.createObjectURL(blob);
            downloadElement.href = href;
            // 下载后文件名
            downloadElement.download = fileName;
            document.body.appendChild(downloadElement);
            // 点击下载
            downloadElement.click();
            // 下载完成移除元素
            document.body.removeChild(downloadElement);
            // 释放掉blob对象
            window.URL.revokeObjectURL(href);
        },

        // 16进制字符串转换成整型数组
        hexToBytes: function (hexStr) {
            var bytes = [];
            for (var c = 0; c < hexStr.length; c += 2) {
                bytes.push(parseInt(hexStr.substr(c, 2), 16));
            }
            return bytes;
        }
    };
});