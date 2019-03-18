#'
#' Extract events to converts to USD

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
cambodia_ous <- ous_all(baseurl, ou_id = "NC3WdxGafv5")

#'filter out events from cambodia
cambodia <- dplyr::filter(all_events, orgUnit %in% cambodia_ous$id)

#'
#'Tranform the ammount to USD
cambodia_transformed <- cei_transform(cambodia,rate = 3970.42, cei_des = cei_des)

# prepare the payload 
cambodia_payload <- cei_payload(cambodia_transformed, program_id = "H0ARClgSPpF")
# break the payload into a chunck of 100
cambodia_payload <- split(cambodia_payload, (as.numeric(row.names(cambodia_payload)) - 1) %/% 100)


# send the payload
pb <- txtProgressBar(min = 0, max = length(cambodia_payload), initial = 0, style = 3)
for (i in seq_along(1:length(cambodia_payload))) {
  d <- POST(paste0(baseurl,"api/events?importStrategy=UPDATE"),
            body = toJSON(list(events = cambodia_payload[[i]]), auto_unbox = T),
            content_type_json())
  Sys.sleep(60)
  loginDHIS2(baseurl,username,password)
  setTxtProgressBar(pb,i)
}
close(pb)












# Get items not imported

cei_d <- fromJSON(content(d,"text"))$response$importSummaries
cei_not_imported <- filter(cei_d, status != "SUCCESS")
# filter the events from the payload
cambodia_to_reupdate <- filter(cambodia_payload, event %in% cei_not_imported$reference)

d2 <- POST(paste0(baseurl, "api/events?importStrategy=UPDATE"),
           body = toJSON(list(events = cambodia_to_reupdate), auto_unbox = T),
           content_type_json())

cambodia_to_reupdate_js <- toJSON(list(events = cambodia_to_reupdate), auto_unbox = T)

write_json(cambodia_to_reupdate_js,"cambodia_to_reupdate.json")





test <- GET(paste0(baseurl,"api/events?program=H0ARClgSPpF&paging=false&lastUpdated=2019-03-05"))



d <- POST(paste0(baseurl,"api/events?importStrategy=UPDATE"),
          body = toJSON(list(events = cambodia_payload), auto_unbox = T),
          content_type_json())


# ==========================
# 

#'Benin
benin_ous <- ous_all(baseurl, ou_id = "ydpjItzzx42")
#filter events 
benin <- dplyr::filter(all_events, orgUnit %in% benin_ous$id)

# Transform the amount to usd
benin_transformed <- cei_transform(benin, rate = 624.63, cei_des = cei_des)

# prepare the payload 
benin_payload <- cei_payload(benin_transformed, program_id = "H0ARClgSPpF")


















#' Function to update the dataValues 
#' @param x events 
#' @param rate country usd rate
#' @param cei_des client exit interviews data elements to remap
#' @return the update events 
cei_transform <- function(x,rate,cei_des){
  
  # convert to wide and remap
  x$dataValues <- lapply(x$dataValues, function(x) dv_wide(x,cei_des = cei_des))
  
  # covert the currency
  x$dataValues <- lapply(x$dataValues, function(x) cei_events_usd(x,rate = rate))
  
  # reshape the data to long
  x$dataValues <- lapply(x$dataValues, function(x) eventDataValues(x))
  
  # remap the DES names to uids
  x$dataValues <- lapply(x$dataValues, function(x) cei_des_map(x, cei_des = cei_des))
  
  return(x)
  
}











#'
#'A function to convert transform event Data values to a df 
#'Convert the cost of service 
#'@param dv A list of events data values
#'@param rate Exchange rate
#'@param cei_des a data fram with CEI data elements and uids
#'@source dv_wd() 
#'@return a dataframe with dataValues
cei_events_usd <- function(dv, rate = 1){
  tryCatch({
    dv$`CEI SRH FP TRK - How much did you pay out of pocket for your services today?` <- as.numeric(dv$`CEI SRH FP TRK - How much did you pay out of pocket for your services today?`)
    #'convert to usd
    dv$`CEI SRH FP TRK - Cost of Service (USD)` <- dv$`CEI SRH FP TRK - How much did you pay out of pocket for your services today?` * 1/rate
    # convert the values to characters. 
    dv$`CEI SRH FP TRK - Cost of Service (USD)` <- as.character(dv$`CEI SRH FP TRK - Cost of Service (USD)`)
    
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  return(dv)
}


#'A function to convert the events to datavaleues
#'@param sp_df a dataframe with wide events
#'@retun a dataframe with two columns dataElement, value
eventDataValues <- function(sp_df){
  #gather 
  sp_df <- tidyr::gather(sp_df,dataElement,value,
                         1:length(names(sp_df)))
  
  
  return(sp_df)
}

#'
#'A function to generate the payload
#'@param sp_transformed a df
#'@param program_id string
#'@retun events 
cei_payload <- function(sp_transformed, program_id){
  
  payload <- data.frame(program = rep(program_id), nrow(sp_transformed),
                        event = sp_transformed$event,
                        orgUnit = sp_transformed$orgUnit,
                        eventDate = sp_transformed$eventDate, stringsAsFactors = F)

  payload <- payload %>% select(-nrow.sp_transformed.)
  payload$dataValues <- sp_transformed$dataValues
  return(payload)
  
}

#'
#'@param df a df
#'@param cei_des cei_data elements 
cei_des_map <- function(df,cei_des){
  df$dataElement <- plyr::mapvalues(df$dataElement, 
                                    from = cei_des$dataElement$name,
                                    to = cei_des$dataElement$id,
                                    warn_missing = F)
  return(df)
}




  
  


