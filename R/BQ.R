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
  } else {
    id = "nrk-datahub" 
  }
  
  df = bq_table_download(bq_project_query(id,sql))
  
  df
  
}

