#'  create_timeseries
#' 
#' @name create_timeseries
#' 
#' @description  Denne funksjonen lager et datasett med plassering av en plugg fra forsiden for en konkret periode med gitte tidsintervaller
#' 
#' 
#' @param email Epost til konto du ønsker å spørre etter dataene med
#' @param from Date(time) for starttidspunkt
#' @param to Date(time) for sluttidspunkt
#' @param period Nivået på periodene du ønsker å aggregere på. Tillater MICROSECOND , MILLISECOND , SECOND , MINUTE , HOUR eller DAY
#' @param amount Angi størrelsen på intervallene i perioden. Hvis period er satt til SECOND og amount til 20, vil vi lage et datasett med 20 sekunds intervaller
#' @param contentId Angi contentID for pluggen du ønsker å hente data for. Vi støtter p.t. ikke å legge inn flere contentID'er
#' @return Et datasett som viser etasje og rom for en gitt artikkel på bestemte tidspunkter
#' @examples
#' create_timeseries("eirik.brautaset(at)nrk.no","2022-04-03 00:00:00","2022-04-03 23:59:59",1,"SECONDS","pp:1.15918927")
#' 
#' @export
create_timeseries = function(email,from,to,period="SECOND",amount=60,contentId){
  require(bigrquery)
  
  get_df(paste0(
  "with secs AS (select * 
  from UNNEST(GENERATE_TIMESTAMP_ARRAY('",from,"', '",to,"', INTERVAL ",amount," ",period,")) AS secs),
  kurator AS (  SELECT startTime,  endTime,contentId,floorNumber,roomNumber
  FROM `nrk-datahub.consumer_facing_views.kurator_time_periods_detailed` 
  WHERE contentId = '",contentId,"'  AND publishedDate between '",as.Date(from),"'  AND '",as.Date(to),"')  
  select * from secs s left join kurator k on s.secs between k.startTime and k.endTime order by 1")

  ,email)

}








