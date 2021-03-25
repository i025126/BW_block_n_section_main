@AbapCatalog.sqlViewName: 'ZICORECLUSTER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'DW Clusters'
define view ZI_CORE_Cluster as 
   select distinct from rsdarea inner join zcore_setup on zcore_setup.customizid = 'BW'
   
 association[0..*] to ZI_CORE_ClusterT as _Text on _Text.DocCluster = $projection.DocCluster
 
{
     @ObjectModel.text.association: '_Text'
     @EndUserText.label: 'Data Warehouse Cluster'
     key left(infoarea,1) as DocCluster,
     @EndUserText.label: 'Cluster InfoArea'
     infoarea as ClusterInfoArea,
     _Text
}
where
  objvers    = 'A' and
  infoarea_p = zcore_setup.infoarea_cluster
group by 
  infoarea
