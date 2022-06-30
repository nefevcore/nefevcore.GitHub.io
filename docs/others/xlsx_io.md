# XLSX上传下载

项目实施少不了批导，这里整理下上传下载会用到的工具。

## OLE

上古工具，建议放弃

## DOI

没用过

## TEXT_CONVERT_XLS_TO_SAP

TODO

## ALSM_EXCEL_TO_INTERNAL_TABLE

看代码可以看出，是OLE操作复制上传区域，然后动态解析剪切板，优点是方便，但受上传文件格式影响较大。

TODO

## ABAP2XLSX

第三方工具，使用ABAPGIT安装。

通过解析XLSX中的XML文件，实现高速读取数据。相比[CL_EHFND_XLSX](#cl_ehfnd_xlsx)，提供了更齐全的EXCEL设置。

程序搜 ***Z\*ABAP2XLSX\**** 可见官方示例。

## CL_EHFND_XLSX

SAP提供的标准方法，通过解析XLSX中的XML文件，实现高速读取数据，笔者首推。

TODO

## XLSX WORKBANCH

第三方工具，适合导出EXCEL。

类似SMARTFORMS，预先设置好EXCEL模板，并提供输入参数。
