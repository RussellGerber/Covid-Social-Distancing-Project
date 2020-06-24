smoking_data <- function(){
  Health_path <- "C:/Users/russe/Documents/Covid Project Data/Global Health Data/"
  
  ### Smoking
  smoking = paste(Health_path,"IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.csv", sep = "")
  smoking_data <- data.frame(read.csv(smoking))
  remove(smoking)
  smoking_data = subset(smoking_data, select = -c(total_lb,total_ub, daily_lb, X.daily_ub))
  names(smoking_data)[names(smoking_data) == 'total_mean'] <- 'All_Smokers_Mean'
  names(smoking_data)[names(smoking_data) == 'daily_mean'] <- 'Daily_Smokers_Mean'
  names(smoking_data)[names(smoking_data) == 'County'] <- 'county'
  
  all_smoking_data <- dcast(
    melt(
      subset(
        filter(
          smoking_data, sex == 'Both')
        , select = -c(Daily_Smokers_Mean))
      , id.vars = c("state","county","sex","year")), 
    state+county+sex ~ variable+year, value.var = c("value"))
  
  
  features <- subset(all_smoking_data, select = -c(state, county, sex))
  features[is.na(features)] <- 0
  
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 17
  num_hidden = 1
  num_outputs = num_inputs # Must be true for an autoencoder!
  learning_rate = 0.001
  
  sess = tf$Session()
  X = tf$placeholder(tf$float32, shape=shape(NULL, num_inputs), name='x')
  W = tf$Variable(tf$truncated_normal(shape=shape(num_inputs, num_hidden), stddev=0.1, seed = 1))
  b = tf$Variable(tf$truncated_normal(shape=shape(1, num_hidden), stddev=0.1, seed = 1))
  
  hidden = tf$nn$relu( tf$matmul(X, W)+b) 
  Output = tf$matmul(hidden-b,tf$transpose(W)) 
  
  loss = tf$reduce_mean(tf$square(Output-X))
  
  optimizer = tf$train$AdamOptimizer(learning_rate = learning_rate)
  train = optimizer$minimize(loss)
  
  init = tf$global_variables_initializer()
  
  sess$run(init)
  for (i in 1:2000){
    sess$run(train, feed_dict=dict(X=data.matrix(features)))
  }
  components = data.frame(sess$run(hidden, feed_dict=dict(X=data.matrix(features))))
  
  colnames(components) <- c('All_Smoking_Factor')
  all_smoking_data <- cbind(all_smoking_data,components)
  cormat <- round(cor(subset(all_smoking_data, select = -c(state, county, sex)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
  all_smoking_data <- subset(all_smoking_data, select = c(state, county, sex, All_Smokers_Mean_2012, All_Smoking_Factor))
  remove(components, sess, init, train, optimizer, loss, hidden, Output, X, W, b, num_inputs, num_outputs, num_hidden, learning_rate,
         cormat)
  
  
  daily_smoking_data <- dcast(
    melt(
      subset(
        filter(
          smoking_data, sex == 'Both')
        , select = -c(All_Smokers_Mean))
      , id.vars = c("state","county","sex","year")), 
    state+county+sex ~ variable+year, value.var = c("value"))
  
  
  features <- subset(daily_smoking_data, select = -c(state, county, sex))
  features[is.na(features)] <- 0
  
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 17
  num_hidden = 1
  num_outputs = num_inputs # Must be true for an autoencoder!
  learning_rate = 0.001
  
  sess = tf$Session()
  X = tf$placeholder(tf$float32, shape=shape(NULL, num_inputs), name='x')
  W = tf$Variable(tf$truncated_normal(shape=shape(num_inputs, num_hidden), stddev=0.1, seed = 1))
  b = tf$Variable(tf$truncated_normal(shape=shape(1, num_hidden), stddev=0.1, seed = 1))
  
  hidden = tf$nn$relu( tf$matmul(X, W)+b) 
  Output = tf$matmul(hidden-b,tf$transpose(W)) 
  
  loss = tf$reduce_mean(tf$square(Output-X))
  
  optimizer = tf$train$AdamOptimizer(learning_rate = learning_rate)
  train = optimizer$minimize(loss)
  
  init = tf$global_variables_initializer()
  
  sess$run(init)
  for (i in 1:2000){
    sess$run(train, feed_dict=dict(X=data.matrix(features)))
  }
  components = data.frame(sess$run(hidden, feed_dict=dict(X=data.matrix(features))))
  
  colnames(components) <- c('Daily_Smoking_Factor')
  daily_smoking_data <- cbind(daily_smoking_data,components)
  cormat <- round(cor(subset(daily_smoking_data, select = -c(state, county, sex)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
  daily_smoking_data <- subset(daily_smoking_data, select = c(state, county, sex, Daily_Smokers_Mean_2012, Daily_Smoking_Factor))
  
  smoking_data = cbind(all_smoking_data, daily_smoking_data[,4:5])
  
  return(select(smoking_data, -c(sex)))
  
}