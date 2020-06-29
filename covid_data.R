covid_data <- function(){
  
  Covid_path <- "C:/Users/russe/Documents/Covid Project Data/Covid Infection Data/us-counties.csv"
  
  covid_by_county <- data.frame(read.csv(Covid_path)) %>% filter(county != "New York City")
  
  Melt <- melt(covid_by_county, c("county",'state', 'fips', 'cases', "deaths"))
  Wide <- dcast(Melt, state+county+fips ~ value, value.var = "cases")
  Cases_Long <- melt(Wide, id.vars = c('state', "county", 'fips'))
  names(Cases_Long)[names(Cases_Long) == 'value'] <- 'cases'
  Wide <- dcast(Melt, state+county+fips ~ value, value.var = "deaths")
  Deaths_Long <- melt(Wide, id.vars = c('state', "county", 'fips'))
  names(Deaths_Long)[names(Deaths_Long) == 'value'] <- 'deaths'
  covid_by_county <- merge(Cases_Long, Deaths_Long, by=c('state', "county", 'fips', 'variable'))
  names(covid_by_county)[names(covid_by_county) == 'variable'] <- 'day'
  
  remove(Cases_Long,Deaths_Long, Melt, Wide)
  
  covid_by_county$cases[is.na(covid_by_county$cases)] <- 0
  covid_by_county$deaths[is.na(covid_by_county$deaths)] <- 0
  
  return(covid_by_county)
  
}  