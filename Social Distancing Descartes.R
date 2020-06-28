PullExample <- function(data_frame_input, FIPS = NULL){
  fipslist = unique(data_frame_input$fips)
  
  if (is.null(FIPS)){
    FIPS <- sample(fipslist,1)
  }
    timeseries <- subset(data_frame_input, fips == FIPS)
  # timeseries <- timeseries[order(timeseries$date), c("m50_index")]
  
  return(list(timeseries[order(timeseries$date), c("m50_index")], timeseries))
}


spline_smoother <- function(param = 0.25, FIPS = NULL, DataFrameInput = NULL){
  Section <- PullExample(data_frame_input = DataFrameInput, FIPS)
  timeseries <- Section[[1]]
  Section <- Section[[2]]
  
  timeseries <- prepend(timeseries, timeseries[1],before = 1)
  
  if (length(timeseries) < 10){
    return()
  }
  
  y_data <- timeseries[-1] 
  
  mod = smooth.spline(seq_along(y_data), y_data, df = 4)
  Spline = mod$y
  
  y_pred = (1-param)*Spline+param*y_data
  
  gdata = cbind(y_pred, y_data, Spline)
  colnames(gdata) <- c('y_pred','y_data', 'spline')
  
  ggplot(data=melt(gdata), aes(x=Var1, y=value, colour=Var2)) + geom_line()
  
  Section$y_pred <- y_pred  
  
  Start_Date <- Section %>% 
    group_by(fips) %>%
    subset(y_pred < 60) %>%
    summarise(mindate=min(date))
  colnames(Start_Date)[2] <- 'SD_Start' 
  
  Section <- merge.data.frame(Section, Start_Date, by = c("fips"), all.x = TRUE, all.y = FALSE)
  
  Avg_After <- Section %>%
    subset(date >= SD_Start) %>%
    group_by(fips) %>%
    mutate(Avg_After = mean(m50_index)) %>%
    select(c("fips", 'Avg_After')) %>%
    unique()
  
  Section <- merge.data.frame(Section, Avg_After, by = c("fips"), all.x = TRUE, all.y = FALSE)
  
  Section <- mutate(Section,Mean = mean(m50_index))
  
  Section <- Section %>% select(c(fips,SD_Start,Avg_After,Mean)) %>% unique()
  
  return(Section)
}

socialdist_descartes <- function(){
  SD_descartes <- read.csv("~/Covid Project Data/Intervention Data/Descartes Labs/DL-us-mobility-daterow.csv")
  SD_descartes$date <- anytime(SD_descartes$date)
  
  Start_data = spline_smoother(param = 0.25, 1, SD_descartes) 
  fipslist = unique(SD_descartes$fips)[-1]
  for (i in 1:length(fipslist)){
    if (is.na(fipslist[i]) == FALSE){
      New_data = spline_smoother(param = 0.25, FIPS = fipslist[i], SD_descartes)
      Start_data <- rbind(Start_data,New_data)      
    }
  }
  Start_data <- select(Start_data,c(fips,SD_Start,Avg_After,Mean))
  
  return(Start_data)
}
