# 文本取值工具

报表部分字段需要取文本，这里将频率较高的字段整理出来，以后就不用重复写SQL查表了。

<details>
<summary>ZCL_TEXT</summary>

  ``` abap

  class ZCL_TEXT definition
    public
    final
    create private .

  public section.

    class-data BUKRS type ref to ZCL_TEXT read-only .
    class-data BP type ref to ZCL_TEXT read-only .
    class-data MATNR type ref to ZCL_TEXT read-only .

    class-methods CLASS_CONSTRUCTOR .
    class-methods CREATE
      importing
        !I_NAME type STRING
      returning
        value(RO_TEXT) type ref to ZCL_TEXT .
    class-methods CREATE_DOMAIN
      importing
        !I_DOMNAME type DOMNAME
      returning
        value(RO_TEXT) type ref to ZCL_TEXT .
    class-methods CREATE_AUSP
      importing
        !I_CUOBJ type INOB-CUOBJ optional
        !I_KLART type INOB-KLART default '023'
        !I_OBTAB type INOB-OBTAB default 'MARA'
        !I_OBJEK type INOB-OBJEK
      returning
        value(RO_TEXT) type ref to ZCL_TEXT .
    methods GET
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    PROTECTED SECTION.
  private section.

    types:
      BEGIN OF ty_text,
          key   TYPE string,
          value TYPE string,
        END OF ty_text .
    types:
      tt_text TYPE SORTED TABLE OF ty_text WITH NON-UNIQUE KEY key .
    types:
      BEGIN OF ty_text_instance,
          name   TYPE string,
          o_text TYPE REF TO zcl_text,
        END OF ty_text_instance .
    types:
      tt_text_instance TYPE SORTED TABLE OF ty_text_instance WITH NON-UNIQUE KEY name .
    types:
      BEGIN OF ty_domains,
          domname TYPE domname,
          o_text  TYPE REF TO zcl_text,
        END OF ty_domains .
    types:
      tt_domnames TYPE SORTED TABLE OF ty_domains WITH NON-UNIQUE KEY domname .
    types:
      BEGIN OF ty_ausp,
          object_key TYPE ausp-objek,
          o_text     TYPE REF TO zcl_text,
        END OF ty_ausp .
    types:
      tt_ausp TYPE SORTED TABLE OF ty_ausp WITH NON-UNIQUE KEY object_key .

    data MT_TEXT type TT_TEXT .
    data M_NAME type STRING .
    data M_METHOD type ABAP_METHNAME .
    class-data MT_TEXT_INSTANCE type TT_TEXT_INSTANCE .
    class-data MT_DOMAINS type TT_DOMNAMES .
    class-data MT_AUSP type TT_AUSP .
    class-data MO_OBJECTDESCR type ref to CL_ABAP_OBJECTDESCR .

    methods CONSTRUCTOR
      importing
        !I_NAME type STRING .
    methods _BUKRS
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _BP
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _MATNR
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _WERKS
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _LGORT
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _PRCTR
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _WAERS
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _DOMAIN
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _AUSP
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
    methods _NOT_FOUND
      importing
        !I_KEY type DATA
      returning
        value(R_VALUE) type STRING .
  ENDCLASS.



  CLASS ZCL_TEXT IMPLEMENTATION.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Static Public Method ZCL_TEXT=>CLASS_CONSTRUCTOR
  * +-------------------------------------------------------------------------------------------------+
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD class_constructor.

      " 分析自身，获取可以提供的文本处理方法
      mo_objectdescr ?= cl_abap_typedescr=>describe_by_name( 'ZCL_TEXT' ).

      " 设置几个常用的
      bukrs = create( 'BUKRS' ).
      bp = create( 'BP' ).
      matnr = create( 'MATNR' ).

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->CONSTRUCTOR
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_NAME                         TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD constructor.

      m_name = i_name.
      m_method = |_{ m_name }|.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Static Public Method ZCL_TEXT=>CREATE
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_NAME                         TYPE        STRING
  * | [<-()] RO_TEXT                        TYPE REF TO ZCL_TEXT
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD create.

      " V3，相比之前，精简为单文件，方便搬运
      DATA(l_name) = to_upper( i_name ).

      " 检查缓存记录
      READ TABLE mt_text_instance INTO DATA(ls_text_instance) WITH KEY name = l_name BINARY SEARCH.
      IF sy-subrc <> 0.
        " 没有就新建一条记录缓存
        CLEAR ls_text_instance.
        ls_text_instance-name = l_name.

        " 检查是否存在相应处理方法
        DATA l_method TYPE abap_methname.
        l_method = |_{ l_name }|.
        READ TABLE mo_objectdescr->methods TRANSPORTING NO FIELDS WITH KEY name = l_method BINARY SEARCH.
        IF sy-subrc <> 0.
          l_name = 'NOT_FOUND'.
        ENDIF.
        ls_text_instance-o_text = NEW zcl_text( l_name ).

        INSERT ls_text_instance INTO TABLE mt_text_instance.
      ENDIF.

      ro_text = ls_text_instance-o_text.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Static Public Method ZCL_TEXT=>CREATE_AUSP
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_CUOBJ                        TYPE        INOB-CUOBJ(optional)
  * | [--->] I_KLART                        TYPE        INOB-KLART (default ='023')
  * | [--->] I_OBTAB                        TYPE        INOB-OBTAB (default ='MARA')
  * | [--->] I_OBJEK                        TYPE        INOB-OBJEK
  * | [<-()] RO_TEXT                        TYPE REF TO ZCL_TEXT
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD CREATE_AUSP.

      DATA l_object_key TYPE ausp-objek.

      IF i_cuobj IS NOT INITIAL.
        l_object_key = i_cuobj.
      ELSE.
        SELECT SINGLE cuobj
          FROM inob
          WHERE klart = @i_klart
            AND obtab = @i_obtab
            AND objek = @i_objek
          INTO @l_object_key.
      ENDIF.

      READ TABLE mt_ausp INTO DATA(ls_ausp) WITH KEY object_key = l_object_key BINARY SEARCH.
      IF sy-subrc <> 0.
        CLEAR ls_ausp.
        ls_ausp-object_key = l_object_key.
        ls_ausp-o_text = NEW zcl_text( 'AUSP' ).

        SELECT
          atinn AS key,
          atwrt AS value
          FROM ausp
          WHERE objek = @i_objek
          INTO TABLE @ls_ausp-o_text->mt_text.

        INSERT ls_ausp INTO TABLE mt_ausp.
      ENDIF.
      ro_text = ls_ausp-o_text.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Static Public Method ZCL_TEXT=>CREATE_DOMAIN
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_DOMNAME                      TYPE        DOMNAME
  * | [<-()] RO_TEXT                        TYPE REF TO ZCL_TEXT
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD CREATE_DOMAIN.

      READ TABLE mt_domains INTO DATA(ls_domain) WITH KEY domname = i_domname BINARY SEARCH.
      IF sy-subrc <> 0.
        CLEAR ls_domain.
        ls_domain-domname = i_domname.
        ls_domain-o_text = NEW zcl_text( 'DOMAIN' ).

        SELECT
          domvalue_l AS key,
          ddtext AS value
          FROM dd07t
          WHERE domname = @i_domname
            AND ddlanguage = '1'
            AND as4local = 'A'
          INTO TABLE @ls_domain-o_text->mt_text.

        INSERT ls_domain INTO TABLE mt_domains.
      ENDIF.
      ro_text = ls_domain-o_text.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Public Method ZCL_TEXT->GET
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD get.

      CHECK i_key IS NOT INITIAL.
      CHECK m_name <> 'NOT_FOUND'.

      " 特征值特殊处理，无需缓存
      IF m_name = 'AUSP'.
        CALL METHOD me->_ausp
          EXPORTING
            i_key   = i_key
          RECEIVING
            r_value = r_value.
        RETURN.
      ENDIF.

      " 检查缓存
      READ TABLE mt_text INTO DATA(ls_text) WITH KEY key = i_key.
      IF sy-subrc = 0.
        r_value = ls_text-value.
        RETURN.
      ENDIF.

      TRY.
          " 跳转到对应方法处理
          CALL METHOD me->(m_method)
            EXPORTING
              i_key   = i_key
            RECEIVING
              r_value = r_value.
        CATCH cx_root.
      ENDTRY.

      " 缓存记录
      CLEAR ls_text.
      ls_text-key = i_key.
      ls_text-value = r_value.
      INSERT ls_text INTO TABLE mt_text.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_AUSP
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _ausp.

      DATA l_atinn TYPE ausp-atinn.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = i_key
        IMPORTING
          output = l_atinn.

      READ TABLE mt_text INTO DATA(ls_text) WITH KEY key = l_atinn.
      r_value = ls_text-value.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_BP
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _bp.

      SELECT SINGLE
        concat( name_org1, name_org2 ) AS name
        FROM but000
        WHERE partner = @i_key
        INTO @r_value.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_BUKRS
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _bukrs.

      SELECT SINGLE butxt FROM t001 WHERE bukrs = @i_key INTO @r_value.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_DOMAIN
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _DOMAIN.
    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_LGORT
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _lgort.

      SELECT SINGLE lgobe FROM t001l WHERE lgort = @i_key INTO @r_value.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_MATNR
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _matnr.

      SELECT SINGLE maktx FROM makt WHERE matnr = @i_key AND spras = '1' INTO @r_value.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_NOT_FOUND
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _not_found.
    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_PRCTR
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _prctr.

      SELECT SINGLE ltext FROM cepct WHERE spras = '1' AND prctr = @i_key INTO @r_value.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_WAERS
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _waers.

      SELECT SINGLE ltext FROM tcurt WHERE spras = '1' AND waers = @i_key INTO @r_value.

    ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method ZCL_TEXT->_WERKS
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] I_KEY                          TYPE        DATA
  * | [<-()] R_VALUE                        TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD _werks.

      SELECT SINGLE name1 FROM t001w WHERE werks = @i_key INTO @r_value.

    ENDMETHOD.
  ENDCLASS.

  ```
  
</details>
