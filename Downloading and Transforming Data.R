packages <- c("httr", "jsonlite", "tidyverse")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
    }
)


IDs <- c("balancing_energy_prices",    # 1
         "countertrade",               # 2
         "cross_zonal_capacities",     # 3
         "current_balancing_state",    # 4
         "direction_of_balancing",     # 5
         "er_mfrr_bid_prices",         # 6
         "er_mfrr_bid_volumes",        # 7
         "imbalance_prices",           # 8 
         "imbalance_volumes",          # 9
         "mfrr_bid_prices",            # 10              
         "mfrr_bid_volumes",           # 11
         "neutrality",                 # 12
         "normal_activations_er_mfrr", # 13
         "normal_activations_mfrr",    # 14
         "normal_activations_total",   # 15
         "special_activations_total",  # 16  
         "system_services",            # 17
         "unavailable_mfrr_bid_volumes") # 18


fn_fetching_data <- function(id){
  
  print(id) 
 
  from <- "2023-09-01T00%3A42"
  to <- "2023-10-01T00%3A42"
  
  url <- paste("https://api-baltic.transparency-dashboard.eu/api/v1/export?id=",
               id,
               "&start_date=", from,
               "&end_date=", to,
               "&output_time_zone=EET&output_format=json&json_header_groups=0",
               sep ="")
  
  get.data <- GET(url)
  
  content.data <-content(get.data, as = "text", encoding = "UTF-8")
  
  data <- fromJSON(content.data, flatten = TRUE)
  
  data.column.names <- paste(id,
                             data[["data"]][["columns"]][,3], # Upward/Downward
                             data[["data"]][["columns"]][,4]) # Country
  
  DF <-as.data.frame(data[["data"]][["timeseries"]][["values"]])
  
  DF <- as.data.frame(t(DF)) # transposing (rows to columns)
  
  names(DF) <- data.column.names
  DF[,c(length(DF)+1,length(DF)+2)] <- data[["data"]][["timeseries"]][,c(1,2)]
  
  row.names(DF) <- NULL
  
  DF <- DF %>% relocate(from, to)
  
  
  return(DF)
  
} 


DF.list <-lapply(IDs, fn_fetching_data) #  "current_balancing_state" # 4 is 1 second resolution

df_list <- DF.list[c(1:3, 5:18)] # "current_balancing_state" will be left separate


Data_Merged <- Reduce(function(x, y) merge(x, y, by = c("from", "to")), df_list)
current_balancing_state <- DF.list[[4]]

write.csv(Data_Merged , file = "Data_Merged.csv")




