voting_data <- function(){
  
  Politics_path <- "C:/Users/russe/Documents/Covid Project Data/Political Data/election-context-2018.csv"
  Voting_data <- data.frame(read.csv(Politics_path))
  Demographics <- Voting_data[c('fips', "total_population", "cvap", "white_pct", "black_pct",
                                "hispanic_pct", "nonwhite_pct", "foreignborn_pct", "female_pct", "age29andunder_pct",     
                                "age65andolder_pct", "median_hh_inc", "clf_unemploy_pct", "lesshs_pct", "lesscollege_pct",       
                                "lesshs_whites_pct", "lesscollege_whites_pct", "rural_pct", "ruralurban_cc")]
  
  Voting_data <- subset(Voting_data, select = -c(total_population, cvap, white_pct, black_pct,
                                                 hispanic_pct, nonwhite_pct, foreignborn_pct, female_pct, age29andunder_pct,     
                                                 age65andolder_pct, median_hh_inc, clf_unemploy_pct, lesshs_pct, lesscollege_pct,       
                                                 lesshs_whites_pct, lesscollege_whites_pct, rural_pct, ruralurban_cc))
  
  Voting_data[, c('trump16', 'clinton16', 'otherpres16')] <- Voting_data[, c('trump16', 'clinton16', 'otherpres16')]/
    rowSums(subset(Voting_data, select = c(trump16, clinton16, otherpres16)), na.rm = TRUE) 
  
  Voting_data[, c('romney12', 'obama12', 'otherpres12')] <- Voting_data[, c('romney12', 'obama12', 'otherpres12')]/
    rowSums(subset(Voting_data, select = c(romney12, obama12, otherpres12)), na.rm = TRUE) 
  
  Voting_data[, c('demsen16', 'repsen16', 'othersen16')] <- Voting_data[, c('demsen16', 'repsen16', 'othersen16')]/
    rowSums(subset(Voting_data, select = c(demsen16, repsen16, othersen16)), na.rm = TRUE) 
  
  Voting_data[, c('demhouse16', 'rephouse16', 'otherhouse16')] <- Voting_data[, c('demhouse16', 'rephouse16', 'otherhouse16')]/
    rowSums(subset(Voting_data, select = c(demhouse16, rephouse16, otherhouse16)), na.rm = TRUE) 
  
  Voting_data[, c('demgov16', 'repgov16', 'othergov16')] <- Voting_data[, c('demgov16', 'repgov16', 'othergov16')]/
    rowSums(subset(Voting_data, select = c(demgov16, repgov16, othergov16)), na.rm = TRUE) 
  
  Voting_data[, c('demgov14', 'repgov14', 'othergov14')] <- Voting_data[, c('demgov14', 'repgov14', 'othergov14')]/
    rowSums(subset(Voting_data, select = c(demgov14, repgov14, othergov14)), na.rm = TRUE) 
  
  
  # features <- subset(Voting_data, select = -c(state, county, fips))
  # features[is.na(features)] <- 0
  # 
  # # 1D AutoEncoder here using Tensorflow instead of H2o
  # num_inputs = 18
  # num_hidden = 1
  # num_outputs = num_inputs # Must be true for an autoencoder!
  # learning_rate = 0.001
  # 
  # sess = tf$Session()
  # X = tf$placeholder(tf$float32, shape=shape(NULL, num_inputs), name='x')
  # W = tf$Variable(tf$truncated_normal(shape=shape(num_inputs, num_hidden), stddev=0.1, seed = 1))
  # b = tf$Variable(tf$truncated_normal(shape=shape(1, num_hidden), stddev=0.1, seed = 1))
  # 
  # hidden = tf$nn$tanh( tf$matmul(X, W)+b) 
  # Output = tf$matmul(hidden-b,tf$transpose(W)) 
  # 
  # loss = tf$reduce_mean(tf$square(Output-X))
  # 
  # optimizer = tf$train$AdamOptimizer(learning_rate = learning_rate)
  # train = optimizer$minimize(loss)
  # 
  # init = tf$global_variables_initializer()
  # 
  # sess$run(init)
  # for (i in 1:2000){
  #   sess$run(train, feed_dict=dict(X=data.matrix(features)))
  # }
  # components = data.frame(sess$run(hidden, feed_dict=dict(X=data.matrix(features))))
  # 
  # Max = max(Voting_data[,c("repsen16")] %>% na.omit())
  # Min = min(Voting_data[,c("repsen16")] %>% na.omit())
  
  D <- Voting_data[, c("repsen16", "romney12", "rephouse16", "repgov16", "repgov14")] 
  D[is.na(D)] <- 0
  T <- is.na(Voting_data[, c("repsen16", "romney12", "rephouse16", "repgov16", "repgov14")]) == FALSE 
  T <- T*1

  # components = rowSums(D)/rowSums(T)
    
  components = Voting_data$romney12
  
  # components = (-components - min(-components))/(max(-components) - min(-components))
  # components = components*(Max-Min)+Min
   
  
  Voting_data <- cbind(Voting_data,components)
  colnames(Voting_data)[22] <- c('Republican_Factor')
  cormat <- round(cor(subset(Voting_data, select = -c(state, county, fips)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0, limit = c(-1,1))
  
  # ggplot(data = Voting_data, aes(x = trump16, y= demsen16))+geom_point()
  
  Voting_data <- subset(Voting_data, select = c(state, county, fips, trump16, Republican_Factor))

  return(list(Voting_data,Demographics))
}
