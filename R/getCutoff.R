#'  Get cut-off
#' 
#' @name get_cutoff
#' 
#' @description  Denne funksjonen tar imot et datasett med Kuratordata og beregner ny secondsVisible basert på et program sin sendetid
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
  require(lubridate)
  
  
  ## Funksjon for å gjøre tid om til numeric
  
  time_to_numeric = function(tidspunkt){
    
    ledd1 = ""
    ledd2 = ""
    
    if(grepl(".", tidspunkt, fixed = TRUE)){
      
     
      
      # Første ledd
    
      ledd1 = strsplit(tidspunkt,"[.]")[[1]][1]
      # Andre ledd
      
      if(is.na(strsplit(tidspunkt,"[.]")[[1]][2])) {
        
        ledd2 = 0
      } else {
        ledd2 = ((as.integer(strsplit(tidspunkt,"[.]")[[1]][2])/60)*100)

      }
    
      
    } else if(grepl(":", tidspunkt, fixed = TRUE)) {

      # Første ledd
      ledd1 = strsplit(tidspunkt,":")[[1]][1]
      
      #Andre ledd
      if(is.na(strsplit(tidspunkt,":")[[1]][2])) {
        ledd2 = 0
      } else {
        ledd2 = ((as.integer(strsplit(tidspunkt,":")[[1]][2])/60)*100) 
      }
     
       
    } else {
      
      print("FEIL: Sørg for at du har med ':' eller '.' som separator mellom time og minutt. eks: 17.00 eller 17:30")
      }
    
   # Ferdig resultat
    as.numeric(paste0(ledd1,".",round(ledd2,0)))
    
     
  }
  
  
  ## Endre separator for fra og til tidspunkt fra . til : slik at det kan reformatteres til full dato
  
  make_timepart = function(klokka){
    
   
    
    if(grepl(".", klokka, fixed = TRUE)) {
      
      paste0(strsplit(klokka,"[.]")[[1]][1],":",strsplit(klokka,"[.]")[[1]][2])
       
    } else {
      

      klokka
      
      }
    }
    
  
  

  tempdf = df
  
  ## Endre tidssone for datovariabler til Oslotid
  
  
  #dfr$startTime = format(dfr$startTime, tz="Europe/Oslo",usetz=TRUE)
  #dfr$startTime = as.POSIXct(dfr$startTime)
  #dfr$endTime = format(dfr$endTime, tz="Europe/Oslo",usetz=TRUE)
  #dfr$endTime = as.POSIXct(dfr$endTime)
  
  tempdf$starTime = lubridate::with_tz(tempdf$startTime, tz="Europe/Oslo")
  tempdf$endTime = lubridate::with_tz(tempdf$endTime, tz="Europe/Oslo")
  
  
  ## Behandle starttidspunkt for programmet
  ## parse integer om til datetime
  
  
  ## Evaluerer om et snapshot er innenfor sendetiden
  
  tempdf$Issendetid = ""
  
  for(i in 1:nrow(tempdf)){
    
    print(paste0("starter (sendetid): ",i))
    
    if(!is.na(tempdf$startTime[i])) {
      if(
        
        ( #Er starttidspunktet innenfor intervallet?
         
          (time_to_numeric(format(as.POSIXct(tempdf$startTime[i]), format = "%H.%M")) >= time_to_numeric(start))
          
          &&
          
          (time_to_numeric(format(as.POSIXct(tempdf$startTime[i]), format = "%H.%M")) < time_to_numeric(slutt))
          
          
        ) || (#Er sluttidspunktet innenfor intervallet?
          
          time_to_numeric(format(as.POSIXct(tempdf$endTime[i]), format = "%H.%M")) >= time_to_numeric(start)
          
          &&
          
          time_to_numeric(format(as.POSIXct(tempdf$endTime[i]), format = "%H.%M")) < time_to_numeric(slutt)
        )
        
        
      ){
        tempdf$Issendetid[i] = TRUE
        
      } else {
        tempdf$Issendetid[i] = FALSE
      }
      
    }  else {print(paste0("exception (isSendetid) on row ",i) )}
    
     
    }
    


  tempdf$Issendetid = as.logical(tempdf$Issendetid)
  
  
  ## Sette en cut off date hvor end eller start time er utenfor sendetiden + beregne ny secondsVisible
  

  tempdf$endTimeCut = ""
  tempdf$startTimeCut = ""
  
  
  for(i in 1:nrow(tempdf)){ 
    
    
    
    if(tempdf$Issendetid[i] == TRUE){
      
      if(
        
        
        time_to_numeric(format(tempdf$startTime[i], "%H.%M")) < time_to_numeric(start)
        
        
        ) {
        
        tempdf$startTimeCut[i] = paste0(format(tempdf$startTime[i], "%Y-%m-%d")," ", make_timepart(start) ,":00 CEST")
        
      } else {
        
        tempdf$startTimeCut[i] = paste0(as.character(tempdf$startTime[i])," CEST")
      }
      
      if(
        
        
        time_to_numeric(format(tempdf$endTime[i], "%H.%M")) >= time_to_numeric(slutt)){
        
        tempdf$endTimeCut[i] = paste0(format(tempdf$endTime[i], "%Y-%m-%d")," ",make_timepart(slutt),":00 CEST")
        
        
      } else {
        
        
        tempdf$endTimeCut[i] = paste0(as.character(tempdf$endTime[i])," CEST")
        
      }
      
    }
    
    
    
  }
  
  ## Regne om secondsVisible basert på cutOffTime
  
  tempdf$secondsVisibleCutted = as.integer(as.POSIXct(tempdf$endTimeCut, format="%Y-%m-%d %H:%M:%S")-as.POSIXct(tempdf$startTimeCut, format="%Y-%m-%d %H:%M:%S"))
  
  tempdf
}

