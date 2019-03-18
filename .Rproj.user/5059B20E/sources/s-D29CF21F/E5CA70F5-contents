
#'
#'Generate client exit interviews payload 
#'@param baseurl baseurl of the data server
#'@param ou_id id of orgUnts to update
#'@param all_events a df with all CEI events
#'@param rate USD rate
#'@param cei_des a df with CEI dataElements
#'@param program_id id of the program to update
events_payload <- function(baseurl, ou_id, all_events, rate, cei_des, program_id){
  #'Benin ous
  country_ous <- ous_all(baseurl, ou_id = ou_id)
  #filter events 
  country_events <- dplyr::filter(all_events, orgUnit %in% country_ous$id)
  
  #verify that the dataValues are not null
  country_events <- subset(country_events, sapply(country_events$dataValues, function(x) dim(x)[1] > 0))
  
  # Transform the amount to usd
  country_transformed <- cei_transform(country_events, rate = rate, cei_des = cei_des)
  
  # prepare the payload 
  country_payload <- cei_payload(country_transformed, program_id = program_id)
  
  return(country_payload)
}



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
