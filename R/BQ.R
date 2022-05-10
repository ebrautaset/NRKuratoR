#'  get_df
#' 
#' @name get_df
#' 
#' @description  Forenklet funksjon for å hente data fra BigQuery
#' 
#' 
#' @param email Epost til konto du ønsker å spørre etter dataene med
#' @param sql legg inn spørringen din her
#' @return Datasett fra BigQuery basert på spørringen du gjør i parameteret 'sql'
#' @examples
#' get_df("SELECT * FROM 'nrk-datahub.consumer_facing_views.article_demography_total LIMIT' 1000","eirik.brautaset(at)nrk.no")
#' 
#' @export
get_df = function(sql,epost){
  require(bigrquery)
  require(lubridate)
  
  evaluering = function(d,p) names(d)[grep(p,names(d))]

  bq_auth(
    email = epost,
    path = NULL,
    scopes = c("https://www.googleapis.com/auth/bigquery",
               "https://www.googleapis.com/auth/cloud-platform"),
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL
  )
  
  ## 
  id= ""
  
  if(grepl("nrk-scratchbook",sql, fixed = FALSE)) {
    
    id = "nrk-scratchbook"
  } else if(grepl("nrk-datahub",sql, fixed = FALSE)) {
    id = "nrk-datahub" 
  } else if(grepl("nrk-midas-event",sql, fixed = FALSE)){
    id = "nrk-midas-event" 
  } else if(grepl("nrk-ark",sql, fixed = FALSE)){
    id = "nrk-ark"
    
  } else if(grepl("nrk-tv-d4402",sql, fixed = FALSE)) {
    id = "nrk-tv-d4402"
    
  } else {print("ID'en er ikke tilgjengeliggjort - kontakt eirik.brautaset@nrk.no")}
  
  df = bq_table_download(bq_project_query(id,sql))
  
  ## Fikse tidssoner
  
  
  
  for(i in names(df)){
    
    if(is.POSIXct(df[i][[1]])){
      
      df[i][[1]] = lubridate::with_tz(df[i][[1]], tz="Europe/Oslo")
    }
      
      
      
      
    
  }

  df
  
}

