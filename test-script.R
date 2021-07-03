adstock_build=function(data=NA, channel=NA, metric=NA, adstock_rate=NULL, max_memory=NULL) {
  
  or_ser <- #original series
    data %>%
    dplyr::filter(channel == {{channel}}) %>% # filter only relevant channel
    dplyr::select(matches({{metric}})) # select only relevant metric
  
  input <- 
    data %>%
    dplyr::filter(channel == {{channel}}) %>% # filter only relevant channel
    dplyr::select(matches("Date") | matches({{metric}})) %>% # select only relevant metric
    dplyr::mutate(week_start = lubridate::floor_date(as.Date(Date,"%d/%m/%Y"), "week", week_start = 1)) %>% # transform days into week starting on mondays
    dplyr::select(-Date) %>% # delete original Date
    dplyr::relocate(week_start) %>% # pull week_start to the front of the data frame
    dplyr::group_by(week_start) %>% 
    dplyr::summarise(metric = sum(c_across(where(is.numeric)))) %>% # metric grouped by week
    #dplyr::rename({{metric}} = metric)
    dplyr::select(-week_start) # keep only the metric
  
  input_vec <- pull(input, metric)
  
  input_new <- data.frame(matrix(ncol = {{max_memory}}+1, nrow = length(input_vec)))
  
  for(i in 0:{{max_memory}}) {
    input_new[,i+1] <- lag(input_vec, n=i, default = 0) * ({{adstock_rate}}^(i))
  }

  input_new$row_tot <- rowSums(input_new)
  # return(input_new)
  
  output <- pull(input_new, row_tot)
  
  lt <- list(original_series = or_ser
             , transformed_series = output
             , channel_name = {{channel}}
             , metric_name = {{metric}}
             , ad_stock = {{adstock_rate}}
             , max_memory = {{max_memory}}
            )
  return(lt) #original series, transformed series, name of channel, name of metric, ad stock and max memory
}

result <- 
  adstock_build(data,'Display','Impressions',0.1,4)

class(result)


data_2_week <- 
  data_2 %>%
  dplyr::mutate(week_start = lubridate::floor_date(as.Date(date,"%Y-%m-%d"), "week", week_start = 1)) %>% # transform days into week starting on mondays
  dplyr::select(-date) %>% # delete original Date
  dplyr::relocate(week_start) %>% # pull week_start to the front of the data frame
  dplyr::group_by(week_start) %>% 
  dplyr::summarise(orders = sum(c_across(where(is.numeric)))) #%>% # orders grouped by week
