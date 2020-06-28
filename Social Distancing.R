socialdist = function(){
  SD_google <- melt(read.xlsx("~/Covid Project Data/Intervention Data/Google Mobility/mobility_report_US.xlsx"), 
               id.vars = c('county', 'date', 'state'))
  colnames(SD_google)[4] <- 'category'
  
  SD_google$date <- anytime(SD_google$date)
  SD_google %>% mutate_if(is.factor, as.character) -> SD_google
  
  LIST <- c("retail", "grocery.and.pharmacy", "parks", "transit.stations", "workplaces", "residential")
  
  StartDate <- function(i){
    SD_google %>% filter(category == LIST[i]) -> test
    test[order(test$category, test$state, test$county, test$date),] -> test
    Start_Date <- test %>% 
      group_by(category, county, state) %>% 
      mutate(mo_avg_5 = rollmean(value, k = 5, fill = NA))  %>%
      subset(mo_avg_5 < -20) %>%
      summarise(mindate=min(date))
    
    colnames(Start_Date)[4] <- 'SD_Start'   
    
    test <- merge.data.frame(test, Start_Date, by = c("category", "county", "state"), all.x = TRUE, all.y = FALSE) 
    
    Avg_After <- test %>%
      subset(date >= SD_Start) %>%
      group_by(category, county, state) %>%
      mutate(Avg_After = mean(value, na.rm = TRUE)) %>%
      select(c("state", "county", "category", "Avg_After")) %>% unique()
  
    output = merge.data.frame(test, Avg_After, by = c("category", "county", "state"), all.x = TRUE, all.y = FALSE)  
    return(output %>% select(c("state", "county", "category", "Avg_After", "SD_Start", "Avg_After")) %>% unique())
  }
  
  for (i in 1:6){assign(paste('Out', i, sep = ""), StartDate(i))}
  
  return(rbind(Out1, Out2, Out3, Out4, Out5, Out6))
}
