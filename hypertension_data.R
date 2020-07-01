hypertension_data <- function(){
  Hypertension = paste(Health_path,"IHME_USA_HYPERTENSION_BY_COUNTY_2001_2009.csv", sep = "")
  Hypertension <- data.frame(read.csv(Hypertension))
  
  Hypertension <- subset(filter(Hypertension, Race == "all"), select = -c(Race))
  temp <- melt(Hypertension, id.vars = c('State', 'FIPS', 'County'))
  
  temp %>% mutate_if(is.factor, as.character) -> temp
  temp$variable[substr(temp$variable,1,13) == 'Self.reported'] <- 
    paste('SelfReported',substr(temp$variable,14,1000), sep = '')[substr(temp$variable,1,13) == 'Self.reported']
  temp <- filter(separate(data = temp, col = variable, into = c("val1","val2","val3"), remove = TRUE,
                          convert = FALSE, extra = "merge", fill = "warn"),
                 val1=='Total' & val3 == '2009')
  
  Hypertension <- dcast(temp,State+FIPS+County ~ val2, value.var = c('value'))
  colnames(Hypertension)[4:5] <- c('Female_2009', 'Male_2009')
  remove(temp)
  
  return(select(Hypertension, -c(State, County)))
}