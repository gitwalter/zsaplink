class ZSAPLINK definition
  public
  abstract
  create public .

public section.

  data NUGGET_LEVEL type INT4 read-only value 0 ##NO_TEXT.

  class-methods GETOBJECTINFOFROMIXMLDOC
    importing
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
    exporting
      !OBJTYPENAME type STRING
      !OBJNAME type STRING .
  class-methods CONVERTSTRINGTOIXMLDOC
    importing
      value(XMLSTRING) type STRING
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT .
  class-methods CONVERTIXMLDOCTOSTRING
    importing
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
    returning
      value(XMLSTRING) type STRING .
  methods CREATEOBJECTFROMIXMLDOC
  abstract
    importing
      !IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
      !DEVCLASS type DEVCLASS default '$TMP'
      !OVERWRITE type FLAG optional
    returning
      value(NAME) type STRING
    raising
      ZCX_SAPLINK .
  methods CREATEIXMLDOCFROMOBJECT
  abstract
    returning
      value(IXMLDOCUMENT) type ref to IF_IXML_DOCUMENT
    raising
      ZCX_SAPLINK .
  methods CREATESTRINGFROMOBJECT
    returning
      value(STRING) type STRING
    raising
      ZCX_SAPLINK .
  methods CONSTRUCTOR
    importing
      !NAME type STRING .
  methods UPLOADXML
  final
    importing
      !XMLDATA type STRING .
  class-methods GETPLUGINS
    changing
      value(OBJECTTABLE) type TABLE .
  methods CHECKEXISTS
  abstract
    returning
      value(EXISTS) type FLAG .
  methods VALUEHELP
    importing
      !I_OBJTYPE type STRING
    returning
      value(E_OBJNAME) type STRING .
  class-methods CHECKOBJECT
    importing
      !I_IXMLDOCUMENT type ref to IF_IXML_DOCUMENT
    exporting
      !E_OBJTYPE type STRING
      !E_OBJNAME type STRING
      !E_PLUGINEXISTS type FLAG
      !E_OBJECTEXISTS type FLAG
      !E_TARGETOBJECT type ref to ZSAPLINK .
protected section.

  data OBJNAME type STRING .
  data IXML type ref to IF_IXML .
  data XMLDOC type ref to IF_IXML_DOCUMENT .

  methods DELETEOBJECT
  abstract
    raising
      ZCX_SAPLINK .
  class-methods SETATTRIBUTESFROMSTRUCTURE
    importing
      !NODE type ref to IF_IXML_ELEMENT
      !STRUCTURE type DATA .
  class-methods GETSTRUCTUREFROMATTRIBUTES
    importing
      !NODE type ref to IF_IXML_ELEMENT
      !PRESERVEVERSION type FLAG optional
    changing
      !STRUCTURE type DATA .
  methods CREATEXMLSTRING
  final
    returning
      value(XML) type STRING .
  class-methods BUILDTABLEFROMSTRING
    importing
      !SOURCE type STRING
    returning
      value(SOURCETABLE) type TABLE_OF_STRINGS .
  class-methods BUILDSOURCESTRING
    importing
      !SOURCETABLE type RSWSOURCET optional
      !PAGETABLE type O2PAGELINE_TABLE optional
    returning
      value(SOURCESTRING) type STRING .
  methods GETOBJECTTYPE
  abstract
    returning
      value(OBJECTTYPE) type STRING .
  methods CREATEOTRFROMNODE
    importing
      value(NODE) type ref to IF_IXML_ELEMENT
      !DEVCLASS type DEVCLASS default '$TMP'
    exporting
      !CONCEPT type SOTR_TEXT-CONCEPT
    raising
      ZCX_SAPLINK .
  methods CREATENODEFROMOTR
    importing
      !OTRGUID type SOTR_CONC
    returning
      value(NODE) type ref to IF_IXML_ELEMENT .
private section.

  types:
    BEGIN OF t_objecttable,
           classname TYPE string,
           object TYPE ko100-object,
           text TYPE ko100-text,
         END OF t_objecttable .

  data STREAMFACTORY type ref to IF_IXML_STREAM_FACTORY .
  data XMLDATA type STRING .
  data:
    objecttable TYPE TABLE OF t_objecttable .
ENDCLASS.



CLASS ZSAPLINK IMPLEMENTATION.


method BUILDSOURCESTRING.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
data sTemp type string.
data pageLine type O2PAGELINE.

  if sourceTable is not initial.
    loop at sourceTable into sTemp.
      concatenate sourceString sTemp CL_ABAP_CHAR_UTILITIES=>NEWLINE
        into sourceString.
    endloop.
  elseif pageTable is not initial.
    loop at pageTable into pageLine.
      concatenate sourceString pageLine-line
        CL_ABAP_CHAR_UTILITIES=>NEWLINE
        into sourceString.
    endloop.
  endif.

* remove extra newline characters for conversion comparison consistency
  shift sourceString left deleting leading
    CL_ABAP_CHAR_UTILITIES=>NEWLINE.
  shift sourceString right deleting trailing
    CL_ABAP_CHAR_UTILITIES=>NEWLINE.
  shift sourceString left deleting leading space.
endmethod.


method BUILDTABLEFROMSTRING.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  split source at CL_ABAP_CHAR_UTILITIES=>NEWLINE
    into table sourceTable.
endmethod.


method CHECKOBJECT.
  DATA l_objtable LIKE objecttable.
  DATA l_objline  LIKE LINE OF objecttable.

  CLEAR: e_objtype, e_objname, e_pluginexists, e_objectexists.
  CALL METHOD zsaplink=>getobjectinfofromixmldoc
    EXPORTING
      ixmldocument = i_ixmldocument
    IMPORTING
      objtypename  = e_objtype
      objname      = e_objname.

  CALL METHOD zsaplink=>getplugins( CHANGING objecttable = l_objtable ).

  READ TABLE l_objtable INTO l_objline WITH KEY object = e_objtype.

  IF sy-subrc = 0.
    e_pluginexists = 'X'.
    CREATE OBJECT e_targetobject
      TYPE
      (l_objline-classname)
      EXPORTING
        name = e_objname.

    e_objectexists = e_targetobject->checkexists( ).
  ENDIF.

endmethod.


method CONSTRUCTOR.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

*  data meTypeDescr type ref to CL_ABAP_TYPEDESCR.
*  clear className.
*
*  objName = name.
*  meTypeDescr = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_OBJECT_REF( me ).
*  className = meTypeDescr->get_relative_name( ).

  objName = name.
  translate objName to upper case.

  ixml = cl_ixml=>create( ).
  xmlDoc = ixml->create_document( ).
  streamFactory = ixml->CREATE_STREAM_FACTORY( ).
endmethod.


method CONVERTIXMLDOCTOSTRING.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data _ixml type ref to if_ixml.
  data _encoding   type ref to if_ixml_encoding.
  data _streamFactory type ref to IF_IXML_STREAM_FACTORY.
  data _outputStream type ref to IF_IXML_OSTREAM.
  data _renderer type ref to IF_IXML_RENDERER.
  data _tempString  type string.
  data _tempStringx type xstring.
  data _printXMLDoc type ref to cl_xml_document.
  data _rc type sysubrc.

  _ixml = cl_ixml=>create( ).
  _encoding = _ixml->create_encoding(
      byte_order    = if_ixml_encoding=>co_none
      character_set = 'utf-8' ).
  _streamFactory = _ixml->CREATE_STREAM_FACTORY( ).
  _outputStream = _streamFactory->create_ostream_xstring( _tempStringx ).
  _outputstream->set_encoding( encoding = _encoding ).
  _renderer = _ixml->CREATE_RENDERER(
                DOCUMENT = ixmlDocument
                OSTREAM  = _outputStream
              ).
  _renderer->SET_NORMALIZING( ).
  _rc = _renderer->render( ).
  create object _printXMLDoc.
  _rc = _printXMLDoc->parse_string( _tempString ).

  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
    EXPORTING
      im_xstring  = _tempstringx
      im_encoding = 'UTF-8'
    IMPORTING
      ex_string   = _tempstring.

  xmlString = _tempString.
endmethod.


method CONVERTSTRINGTOIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data ixml type ref to if_ixml.
  data streamFactory type ref to IF_IXML_STREAM_FACTORY.
  data iStream type ref to if_ixml_istream.
  data ixmlParser type ref to if_ixml_parser.
  data xmlDoc type ref to if_ixml_document.

  " Make sure to convert Windows Line Break to Unix as
  " this linebreak is used to get a correct import
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
    IN xmlString WITH cl_abap_char_utilities=>newline.

  ixml = cl_ixml=>create( ).
  xmlDoc = ixml->create_document( ).
  streamFactory = ixml->CREATE_STREAM_FACTORY( ).
  iStream = streamFactory->CREATE_ISTREAM_STRING( xmlString ).
  iXMLParser = iXML->create_parser(  stream_factory = streamFactory
                                     istream        = iStream
                                     document       = xmlDoc ).
  iXMLParser->parse( ).
  ixmlDocument = xmlDoc.
endmethod.


method CREATENODEFROMOTR.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA rootnode TYPE REF TO if_ixml_element.
  DATA txtnode TYPE REF TO if_ixml_element.
  DATA rc TYPE sysubrc.

  DATA sotrheader TYPE sotr_head.
  DATA sotrtextline TYPE sotr_text.
  DATA sotrtexttable TYPE TABLE OF sotr_text.

  DATA _ixml TYPE REF TO if_ixml.
  DATA _xmldoc TYPE REF TO if_ixml_document.

  CALL FUNCTION 'SOTR_GET_CONCEPT'
    EXPORTING
      concept        = otrguid
    IMPORTING
      header         = sotrheader
    TABLES
      entries        = sotrtexttable
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  sotrheader-paket = '$TMP'. "change devclass to $TMP for exports

* Create xml doc
*  _ixml = cl_ixml=>create( ).
*  _xmldoc = _ixml->create_document( ).
*  streamfactory = _ixml->create_stream_factory( ).

* Create parent node
  rootnode = xmldoc->create_element( zsaplink_oo=>c_xml_key_sotr ). "OTR object type
  CLEAR sotrheader-concept.                                 "ewH:33
  setattributesfromstructure( node = rootnode structure = sotrheader ).

* Create nodes for texts
  LOOP AT sotrtexttable INTO sotrtextline.
    txtnode = xmldoc->create_element( zsaplink_oo=>c_xml_key_sotrtext ).
    CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
    setattributesfromstructure(
      node = txtnode structure = sotrtextline ).
    rc = rootnode->append_child( txtnode ).
  ENDLOOP.

  node = rootnode.

endmethod.


method CREATEOTRFROMNODE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA txtnode TYPE REF TO if_ixml_element.
  DATA filter TYPE REF TO if_ixml_node_filter.
  DATA iterator TYPE REF TO if_ixml_node_iterator.

  DATA sotrheader TYPE sotr_head.
  DATA sotrtextline TYPE sotr_text.
  DATA sotrtexttable TYPE TABLE OF sotr_text.
  DATA sotrpaket TYPE sotr_pack.

* get OTR header info
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = node
    CHANGING
      structure = sotrheader.

* get OTR text info
  filter = node->create_filter_name( zsaplink_oo=>c_xml_key_sotrText ).
  iterator = node->create_iterator_filtered( filter ).
  txtnode ?= iterator->get_next( ).

  WHILE txtnode IS NOT INITIAL.
    CLEAR sotrtextline.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = txtnode
      CHANGING
        structure = sotrtextline.
    CLEAR: sotrtextline-concept, sotrtextline-object.       "ewH:33
    APPEND sotrtextline TO sotrtexttable.
    txtnode ?= iterator->get_next( ).
  ENDWHILE.

* ewH:issue 33--> in 6.40 and above, you cannot pass a default concept
*  (otr) guid, so we will always create new
*  CALL FUNCTION 'SOTR_GET_CONCEPT'
*    EXPORTING
*      concept              = sotrHeader-concept
**   IMPORTING
**     HEADER               =
**   TABLES
**     ENTRIES              =
*   EXCEPTIONS
*     NO_ENTRY_FOUND       = 1
*     OTHERS               = 2
*            .
*  IF sy-subrc <> 1.
**   delete OTR if exists already
*    CALL FUNCTION 'SOTR_DELETE_CONCEPT'
*      EXPORTING
*        concept                     = sotrHeader-concept
*     EXCEPTIONS
*       NO_AUTHORIZATION            = 1
*       NO_ENTRY_FOUND              = 2. "who cares
**       CONCEPT_USED                = 3
**       NO_MASTER_LANGUAGE          = 4
**       NO_SOURCE_SYSTEM            = 5
**       NO_TADIR_ENTRY              = 6
**       ERROR_IN_CORRECTION         = 7
**       USER_CANCELLED              = 8
**       OTHERS                      = 9
**              .
*    if sy-subrc = 1.
*      raise exception type zcx_saplink
*        exporting textid = zcx_saplink=>not_authorized.
*    endif.
*  ENDIF.


  DATA objecttable TYPE sotr_objects.
  DATA objecttype TYPE LINE OF sotr_objects.
* Retrieve object type of OTR
  CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
    EXPORTING
      object_vector    = sotrheader-objid_vec
    IMPORTING
      OBJECTS          = objecttable
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  READ TABLE objecttable INTO objecttype INDEX 1.

* create OTR
  sotrpaket-paket = devclass.
  CALL FUNCTION 'SOTR_CREATE_CONCEPT'
    EXPORTING
      paket                               = sotrpaket
      crea_lan                            = sotrheader-crea_lan
      alias_name                          = sotrheader-alias_name
*      CATEGORY                            =
      object                              = objecttype
      entries                             = sotrtexttable
*     FLAG_CORRECTION_ENTRY               =
*     IN_UPDATE_TASK                      =
*      CONCEPT_DEFAULT                     = sotrHeader-concept "ewH:33
    IMPORTING
      concept                             = concept         "ewH:33
    EXCEPTIONS
      package_missing                     = 1
      crea_lan_missing                    = 2
      object_missing                      = 3
      paket_does_not_exist                = 4
      alias_already_exist                 = 5
      object_type_not_found               = 6
      langu_missing                       = 7
      identical_context_not_allowed       = 8
      text_too_long                       = 9
      error_in_update                     = 10
      no_master_langu                     = 11
      error_in_concept_id                 = 12
      alias_not_allowed                   = 13
      tadir_entry_creation_failed         = 14
      internal_error                      = 15
      error_in_correction                 = 16
      user_cancelled                      = 17
      no_entry_found                      = 18
      OTHERS                              = 19
            .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

endmethod.


method CREATESTRINGFROMOBJECT.
endmethod.


method CREATEXMLSTRING.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
data streamFactory type ref to IF_IXML_STREAM_FACTORY.
data outputStream type ref to IF_IXML_OSTREAM.
data renderer type ref to IF_IXML_RENDERER.
data tempString type string.
data printXMLDoc type ref to cl_xml_document.
data rc type sysubrc.

  streamFactory = ixml->CREATE_STREAM_FACTORY( ).
  outputStream = streamFactory->CREATE_OSTREAM_CSTRING( tempString ).
  renderer = ixml->CREATE_RENDERER(
    DOCUMENT = xmlDoc OSTREAM = outputStream ).
  rc = renderer->render( ).
  create object printXMLDoc.
  rc = printXMLDoc->parse_string( tempString ).
  xml = tempString.
endmethod.


method GETOBJECTINFOFROMIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
data rootNode type ref to IF_IXML_NODE.
data rootAttr type ref to IF_IXML_NAMED_NODE_MAP.
data AttrNode type ref to IF_IXML_NODE.
data nodeName type string.

  rootNode ?= ixmlDocument->GET_ROOT_ELEMENT( ).

* get object type
  objTypeName = rootNode->GET_NAME( ).
  translate objTypeName to upper case.

* get object name
  rootAttr = rootNode->GET_ATTRIBUTES( ).
  AttrNode = rootAttr->GET_ITEM( 0 ).
  objName = AttrNode->GET_VALUE( ).
endmethod.


method GETPLUGINS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA classlist TYPE seo_inheritances.
  DATA classline TYPE vseoextend.
  DATA classobject TYPE REF TO zsaplink.
  DATA objectline TYPE t_objecttable.
  DATA tabletypeline TYPE ko105.
  DATA tabletypesin TYPE TABLE OF ko105.
  DATA tabletypesout TYPE tr_object_texts.
  DATA tabletypeoutline TYPE ko100.
  DATA clsname TYPE string.
  DATA objtype TYPE trobjtype.

  REFRESH objecttable.

  SELECT * FROM vseoextend INTO TABLE classlist
    WHERE refclsname like 'ZSAPLINK%'
    AND version = '1'.

  LOOP AT classlist INTO classline.
    clsname = classline-clsname.
    TRY.
        CREATE OBJECT classobject
          TYPE
            (clsname)
          EXPORTING
            name      = 'foo'.
        objtype = classobject->getobjecttype( ).
      CATCH cx_root.
        CONTINUE.
    ENDTRY.
    CLEAR tabletypeline.
    REFRESH tabletypesin.

    tabletypeline-object = objtype.
    APPEND tabletypeline TO tabletypesin.

    CALL FUNCTION 'TRINT_OBJECT_TABLE'
      TABLES
        tt_types_in  = tabletypesin
        tt_types_out = tabletypesout.

    LOOP AT tabletypesout INTO tabletypeoutline.
      objectline-classname = clsname.
      MOVE-CORRESPONDING tabletypeoutline TO objectline.
      APPEND objectline TO objecttable.
    ENDLOOP.
  ENDLOOP.
endmethod.


method GETSTRUCTUREFROMATTRIBUTES.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
data attributeList type ref to IF_IXML_NAMED_NODE_MAP.
data nodeIterator type ref to IF_IXML_NODE_ITERATOR.
data attributeNode type ref to if_ixml_node.
data value type string.
data name type string.
field-symbols <value> type any.

  clear structure.
  attributeList = node->GET_ATTRIBUTES( ).
  nodeIterator = attributeList->create_iterator( ).
  attributeNode = nodeIterator->get_next( ).
  while attributeNode is not initial.
    name = attributeNode->get_name( ).
    if name = 'VERSION' and preserveVersion is initial. "ewh:issue 45
*    if name = 'VERSION'.
      value = '0'.
    else.
      value = attributeNode->get_value( ).
    endif.
    assign component name of structure structure to <value>.
    if sy-subrc = 0.
      <value> = value.
    endif.
    attributeNode = nodeIterator->get_next( ).
  endwhile.















*    .-"-.
*  .'=^=^='.
* /=^=^=^=^=\
*:^=SAPLINK=^;
*|^ EASTER  ^|
*:^=^EGG^=^=^:
* \=^=^=^=^=/
*  `.=^=^=.'
*    `~~~`
* Don't like the way we did something?
* Help us fix it!  Tell us what you think!
* http://saplink.org
endmethod.


method SETATTRIBUTESFROMSTRUCTURE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  data int type i.
  int = int.
  data structDescr type ref to cl_abap_structDescr.
  data aComponent type abap_compdescr.
  field-symbols <fieldValue> type any.
  data rc type sysubrc.
  data sName type string.
  data sValue type string.

  structDescr ?= cl_abap_structDescr=>describe_by_data( structure ).
  loop at structDescr->components into aComponent.
    assign component aComponent-name of structure
      structure to <fieldValue>.
    if sy-subrc = 0.
      sName = aComponent-name.
*      sValue = <fieldValue>.
*     for certain attributes, set to a standard for exporting
      case sName.
*        when 'VERSION'. "version should always export as inactive
*          sValue = '0'. "commented by ewH: issue 45
        when 'DEVCLASS'. "development class should always be $TMP
          sValue = '$TMP'.
        " Developer, Date and Time Metadata has to be removed to
        " not clutter diffs
        "
        " Meta Attributes for DDIC Types
        when 'AS4USER'.
          clear sValue.
        when 'AS4DATE'.
          clear sValue.
        when 'AS4TIME'.
          clear sValue.
        " Meta Attributes for PROG
        when 'CNAM'.
          clear sValue.
        when 'CDAT'.
          clear sValue.
        when 'UNAM'.
          clear sValue.
        when 'UDAT'.
          clear sValue.
        when 'VERN'.
          clear sValue.
        when 'SDATE'.
          clear sValue.
        when 'STIME'.
          clear sValue.
        when 'IDATE'.
          clear sValue.
        when 'ITIME'.
          clear sValue.
        " Meta Attributes for CLAS
        when 'AUTHOR'.
          clear sValue.
        when 'CREATEDON'.
          clear sValue.
        when 'CHANGEDBY'.
          clear sValue.
        when 'CHANGEDON'.
          clear sValue.
        when 'CHANGEDON'.
          clear sValue.
        when 'CHGDANYON'.
          clear sValue.
        when 'R3RELEASE'.
          clear sValue.
        when 'UUID'.
          clear sValue.
        " SOTR
        when 'CREA_NAME'.
          clear sValue.
        when 'CHAN_NAME'.
          clear sValue.
        when 'CREA_TSTUT'.
          clear sValue.
        when 'CHAN_TSTUT'.
          clear sValue.
        " MSAG
        when 'LASTUSER'.
          clear sValue.
        when 'LDATE'.
          clear sValue.
        when 'LTIME'.
          clear sValue.
        when others.
          sValue = <fieldValue>.
      endcase.
      if sValue is not initial.
        rc = Node->set_attribute( name = sName value = sValue ).
      endif.
    else.
* WHAT?>!??
    endif.
  endloop.
endmethod.


method UPLOADXML.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
data iStream type ref to if_ixml_istream.
data ixmlParser type ref to if_ixml_parser.

  iStream = streamFactory->CREATE_ISTREAM_STRING( xmlData ).
  iXMLParser = iXML->create_parser(  stream_factory = streamFactory
                                     istream        = iStream
                                     document       = XMLdoc ).
  iXMLParser->parse( ).

endmethod.


method VALUEHELP.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
DATA l_object_type type  euobj-id.
data objname(40) type c.

l_object_type = i_objType.


  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
     EXPORTING
       object_type           = l_object_type
       object_name           = objname
       suppress_selection    = 'X'
       use_alv_grid          = ''
       without_personal_list = ''
     IMPORTING
       object_name_selected  = objname
     EXCEPTIONS
       cancel                = 1.

  e_objname = objname.
endmethod.
ENDCLASS.
