## set up ========================================================
baseurl <- "https://clone.psi-mis.org/"
base = substr(baseurl, 9,25)
username <- keyringr::get_kc_account(base, type = 'internet')
password <- keyringr::decrypt_kc_pw(base, type = "internet")

source("/Users/isaiahnyabuto/Documents/PSI Workspace/functions.R")

loginDHIS2(baseurl,username,password)

r <- GET(paste0(baseurl,"api/events?program=H0ARClgSPpF&paging=false"))
d <- fromJSON(content(r,"text"))

all_events <- as_tibble(d$events)


#'
#'Get ougUnits with UIDs
#'
#'Begin with Cambodia
ous_r <- GET(paste0(baseurl,"api/29/organisationUnits/NC3WdxGafv5?fields=id,name&includeDescendants=true"))
ous_d <- fromJSON(content(ous_r,"text"))$organisationUnits
#'
#'filter out events from cambodia
cambodia <- dplyr::filter(all_events, orgUnit %in% ous_d$id)




#'
#'Function to get a list of orgUnits
#'@param baseurl server url
#'@return a df with OrgUnits nameand Ids
ous_all <- function(baseurl, ou_id = ""){
  ous_r <- httr::GET(paste0(baseurl,"api/29/organisationUnits/",ou_id,"?fields=id,name&includeDescendants=true"))
  ous_d <- jsonlite::fromJSON(content(ous_r,"text"))$organisationUnits
  return(ous_d)
}



#'
#'Get a list of data lement in CEI program 

cei_des_r <- GET(paste0(baseurl,"api/programStages/ftFE8kmzbTn?fields=name,programStageDataElements[id,name,dataElement[id,name]]"))
cei_des <- fromJSON(content(cei_des_r,"text"))$programStageDataElements

#' Translate event dataElements UIDs to names
#' @param x a df with dataElemeents uuids
#' @param cei_des a df with program dataElements
#' @return a translated df
dv_wide <- function(x,cei_des){
  #x <- x %>% dplyr::select(c("dataElement","value"))
  x$dataElement <- plyr::mapvalues(x$dataElement, from = cei_des$dataElement$id, to = cei_des$dataElement$name, warn_missing = F)
  # reshape to wide 
  x <- tidyr::spread(x,dataElement,value,fill = "") 
  x <- tibble::as_tibble(lapply(x,paste0,collapse=""))
  x <- x %>% dplyr::select(-c(lastUpdated,storedBy,created,providedElsewhere))
  return(x)
}




#'
#'A function to convert transform event Data values to a df 
#'Convert the cost of service 
#'@param dv A list of events data values
#'@param country Name of the country
#'@param rate Exchange rate
#'@param cei_des a data fram with CEI data elements and uids
#'@source dv_wd() 
#'@return a dataframe with dataValues
cei_country_dv <- function(dv, country = "", rate = 0.00025, cei_des){
  dv <- lapply(dv, function(x) dv_wide(x,cei_des))
  dv <- plyr::ldply(dv,data.frame)
  dv$country = rep(country,nrow(dv))
  #'convert to usd
  dv <- dv %>% dplyr::mutate(cost_of_service = as.numeric(CEI.SRH.FP.TRK...How.much.did.you.pay.out.of.pocket.for.your.services.today.))
  dv$cost_of_service <- sapply(dv$cost_of_service, function(x) x * 1/rate)
  return(dv)
  
}


cambodia <- cei_country_dv(cambodia$dataValues,country = "Cambodia", cei_des = cei_des, rate = 3970.42)
# Get BI ous 
benin_ous <- ous_all(baseurl,ou_id = "ydpjItzzx42")
benin <- dplyr::filter(all_events, orgUnit %in% benin_ous$id)
benin <- cei_country_dv(benin$dataValues,country = "Benin", cei_des = cei_des,rate = 624.63)

# Get Cameroon ous 
cameroon_ous <- ous_all(baseurl, ou_id = "RUVBtgS12zl")
cameroon <- dplyr::filter(all_events, orgUnit %in% cameroon_ous$id)
cameroon <- cei_country_dv(cameroon$dataValues, country = "Cameroon", cei_des = cei_des, rate = 624.75)

#' Get Niger OUS
niger_ous <- ous_all(baseurl, ou_id = "fLUjAswfjNy")
niger <- dplyr::filter(all_events, orgUnit %in% niger_ous$id)
niger <- cei_country_dv(niger$dataValues, country = "Niger", cei_des = cei_des, rate = 624.63)

#' Get Nepal OUS 
nepal_ous <- ous_all(baseurl, ou_id = "G4bzdahEfGk")
nepal <- dplyr::filter(all_events, orgUnit %in% nepal_ous$id)
#' remove null 
nepal <-  nepal[sapply(nepal, function(x) dim(x)[1] > 0)]
nepal <- cei_country_dv(nepal, country = "Nepal", cei_des = cei_des, rate = 107.288)

#' Get El Salvador ous
el_ous  <- ous_all(baseurl,ou_id = "YypjoDjI7Zg")
el <- dplyr::filter(all_events, orgUnit %in% el_ous$id)
el <- cei_country_dv(el$dataValues, country = "El Salvador", cei_des = cei_des, rate = 1)

#' Get Guatamala ous
#'@param ou_id Id of the orgUnit
guatemala_ous <- ous_all(baseurl, ou_id = "NxKJFhKFGbN")
guatemala <- dplyr::filter(all_events, orgUnit %in% guatemala_ous$id)
guatemala <- cei_country_dv(guatemala$dataValues, country = "Guatemala", cei_des = cei_des, rate = 7.351)

nicaragua_ous <- ous_all(baseurl, ou_id = "qy3vInGGyDB")
nicaragua <- dplyr::filter(all_events, orgUnit %in% nicaragua_ous$id)
nicaragua <- cei_country_dv(nicaragua$dataValues, country = "Nicaragua", cei_des = cei_des, rate = 28.83)

#' Get Burundi OUS 
burundi_ous <- ous_all(baseurl, ou_id = "DgKFsDCpY7X")
burundi <- dplyr::filter(all_events, orgUnit %in% burundi_ous$id)
burundi <- cei_country_dv(burundi$dataValues, country = "Burundi", cei_des = cei_des, rate = 1650.35)


cei_events <- dplyr::bind_rows(burundi,nicaragua,guatemala,el,nepal,niger,cameroon,benin,cambodia)

write.csv(cei_events,"./cei_events.csv", row.names = F)



#' Get Nicaragua ous