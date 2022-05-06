#'  Get cut-off
#' 
#' @name get_cutoff
#' 
#' @description  Denne funksjonen tar imot et datasett med Kuratordata og beregner ny secondsVisible basert på et program sin sendetid
#' Funksjonen aksepterer p.t. bare hele timer
#' 
#' @param df Datasettet du ønsker evaluert. Her kan du benytte funksjonen get_df()
#' @param start starttidspunktet for programmets sendetid
#' @param slutt sluttidspunktet for programmets sendetid
#' @return Et nytt datasett hvor all data i datasettet bqdata beregner ny secondsActive for spesifisert tidsintervall
#' @examples
#' cutOffDF = get_cutoff(bqdata,17,23)
#' cutOffDF = get_cutoff(get_df(sql,email),17,23)
#' @export
get_cutoff = function(df,start,slutt){
  require(dplyr)
  dfr = df
  
  ## Endre tidssone for datovariabler til Oslotid
  
  dfr$startTime = format(dfr$startTime, tz="Europe/Oslo",usetz=TRUE)
  dfr$startTime = as.POSIXct(dfr$startTime)
  
  dfr$endTime = format(dfr$endTime, tz="Europe/Oslo",usetz=TRUE)
  dfr$endTime = as.POSIXct(dfr$endTime)
  
  
  ## Evaluerer om et snapshot er innenfor sendetiden
  
  dfr$Issendetid = ""

  
  for(i in 1:nrow(dfr)){
    
    if(is.na(dfr$startTime[i])) {
      if(
        
        ( #Er starttidspunktet innenfor intervallet?
          
          (as.integer(format(as.POSIXct(dfr$startTime[i]), format = "%H")) >= start)
          
          &&
          
          (as.integer(format(as.POSIXct(dfr$startTime[i]), format = "%H")) < slutt)
          
          
        ) || (#Er sluttidspunktet innenfor intervallet?
          
          as.integer(format(as.POSIXct(dfr$endTime[i]), format = "%H")) >= start
          
          &&
          
          as.integer(format(as.POSIXct(dfr$endTime[i]), format = "%H")) < slutt
        )
        
        
      ){
        dfr$Issendetid[i] = TRUE
        
      } else {
        dfr$Issendetid[i] = FALSE
      }
      
    }
    
    } else {print(paste0("exception on row ",i) )}
    

  
  
  dfr$Issendetid = as.logical(dfr$Issendetid)
  
  
  ## Sette en cut off date hvor end eller start time er utenfor sendetiden + beregne ny secondsVisible
  
  
  dfr$endTimeCut = ""
  dfr$startTimeCut = ""
  
  
  
  for(i in 1:nrow(dfr)){ 
    if(dfr$Issendetid[i] == TRUE){
      
      if(as.integer(format(dfr$startTime[i], "%H")) < start ) {
        
        dfr$startTimeCut[i] = paste0(format(dfr$startTime[i], "%Y-%m-%d")," ",start,":00:00 CEST")
        
      } else {
        
        dfr$startTimeCut[i] = paste0(as.character(dfr$startTime[i])," CEST")
      }
      
      if(as.integer(format(dfr$endTime[i], "%H")) >= slutt){
        
        dfr$endTimeCut[i] = paste0(format(dfr$endTime[i], "%Y-%m-%d")," ",slutt,":00:00 CEST")
        
        
      } else {
        
        dfr$endTimeCut[i] = paste0(as.character(dfr$endTime[i])," CEST")
        
      }
      
      
    }
    
  }
  
  
  
  ## Regne om secondsVisible basert på cutOffTime
  
  dfr$secondsVisibleCutted = as.integer(as.POSIXct(dfr$endTimeCut, format="%Y-%m-%d %H:%M:%S")-as.POSIXct(dfr$startTimeCut, format="%Y-%m-%d %H:%M:%S"))
  
  dfr
}

