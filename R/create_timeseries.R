#'  create_timeseries
#' 
#' @name create_timeseries
#' 
#' @description  Denne funksjonen lager et datasett med plassering av en plugg fra forsiden for en konkret periode med gitte tidsintervaller
#' 
#' 
#' @param email Epost til konto du ønsker å spørre etter dataene med
#' @param fra Date(time) for starttidspunkt
#' @param til Date(time) for sluttidspunkt
#' @param period Nivået på periodene du ønsker å aggregere på. Tillater MICROSECOND , MILLISECOND , SECOND , MINUTE , HOUR eller DAY
#' @param amount Angi størrelsen på intervallene i perioden. Hvis period er satt til SECOND og amount til 20, vil vi lage et datasett med 20 sekunds intervaller
#' @param type Angi hva du ønsker å bruke som identifikator - contentId eller url.
#' @param ID Angi contentID for pluggen du ønsker å hente data for. ContentID behøver polypoly prefix'er og URL'er trenger å bli enkapsulert med %
#' @return Et datasett som viser etasje og rom for en gitt artikkel på bestemte tidspunkter
#' @examples
#' create_timeseries("eirik.brautaset(at)nrk.no","2022-04-03 00:00:00","2022-04-03 23:59:59",1,"SECONDS","pp:1.15918927")
#' create_timeseries("eirik.brautaset(at)nrk.no","2022-04-03 00:00:00","2022-04-03 23:59:59",1,"SECONDS",type="url", "%paskenotter%")
#' 
#' @export
create_timeseries = function(email,fra,til,period="SECOND",amount=60,type="contentId",contentId){
  require(bigrquery)
  
  
  if(type == "contentId"){
    get_df(paste0(
      "with secs AS (select * 
  from UNNEST(GENERATE_TIMESTAMP_ARRAY('",fra,"', '",til,"', INTERVAL ",amount," ",period,")) AS secs),
  kurator AS (  SELECT startTime,  endTime,contentId,floorNumber,roomNumber
  FROM `nrk-datahub.consumer_facing_views.kurator_time_periods_detailed` 
  WHERE contentId = '",ID,"'  AND publishedDate between '",as.Date(from),"'  AND '",as.Date(to),"')  
  select * from secs s left join kurator k on s.secs between k.startTime and k.endTime order by 1")
      
      ,email)
  
  }
  if(type == "url"){
    get_df(paste0(
      "with secs AS (select * 
  from UNNEST(GENERATE_TIMESTAMP_ARRAY('",fra,"', '",til,"', INTERVAL ",amount," ",period,")) AS secs),
  kurator AS (  
 
  SELECT * FROM `nrk-datahub.test_tom.kurator_url_and_placement_periods` WHERE url like ",ID," AND (
  ",fra," >= startTime AND ",til," <= endTime
)  )  
  select * from secs s left join kurator k on s.secs between k.startTime and k.endTime order by 1")
      ,email)
  }
  
  
  
  

}








