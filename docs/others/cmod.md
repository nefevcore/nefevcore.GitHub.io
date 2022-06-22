# CMOD增强

## 查找CMOD增强

1. 在程序中搜索 ***CUSTOMER-FUNCTION*** 找到后面的3位数字编号，出口函数名的规则为EXIT_<***程序名***>_<3位数字>，然后通过找到的出口函数名到 ***MODSAP*** 表里查找所对应的出口对象
（即增强点）

2. 通过调试系统相关函数：MODX_FUNCTION_ACTIVE_CHECK

3. 代码找增强
    - E类：***MODX_FUNCTION_ACTIVE_CHECK***
（检查功能出口类用户出口是否被激活）
    - C类：***MODX_MENUENTRY_ACTIVE_CHECK***
（检查菜单关键字类增强激活状况）
    - S类：***MODX_SUBSCREEN_ACTIVE_CHECK***
（检查屏幕类增强激活状况）

        SAP的任何一个事务码 对应的标注程序都留下了大量的用户出口，正是SAP灵活的配置和强大的用户出口，才使其产品轻松应对各种复杂需求成为可能，系统还为能快速找到和激活这些增强进行了有效组织，各类增强被记录在table中并且提供了相关检查函数，从而更方便系统实施过程 。
