@AbapCatalog.sqlViewName: 'ZICOREINFOOBJT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Text for InfoObjects'

@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'InfoObject'

define view ZI_CORE_InfoObjectT as select from rsdiobjt 
  association [0..1] to I_Language as _Language on $projection.Language = _Language.Language
{
     @Semantics.language: true
     key langu as Language,
     @ObjectModel.text.element: 'Description'
     key iobjnm as InfoObject,
     @Semantics.text: true
     txtlg as Description,
     
     _Language
}
  where objvers = 'X'
     
