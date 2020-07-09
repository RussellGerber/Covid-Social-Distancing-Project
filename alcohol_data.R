alcohol_data <- function(){
  alcohol = paste(Health_path,"IHME_USA_COUNTY_ALCOHOL_USE_PREVALENCE_2002_2012_NATIONAL_Y2015M04D23.xlsx", sep = "")
  alcohol_heavy <- data.frame(read.xlsx(xlsxFile = alcohol,sheet = 'Heavy'))
  alcohol_binge <- data.frame(read.xlsx(xlsxFile = alcohol,sheet = 'Binge'))
  
  temp <- filter(melt(alcohol_heavy, id.vars = c('State', 'Location')), substr(variable,1,1) == "X")
  temp['year'] = substr(temp$variable,2,5)
  temp['gender'] = substr(temp$variable,6,1000)
  temp <- filter(temp, gender == '.Both.Sexes')
  
  alcohol_heavy <- dcast(
    select(temp, c('State', 'Location', 'value', 'year')),
    State+Location ~ year, value.var = c("value"))
  colnames(alcohol_heavy)[2] <- c("County")
  
  features <- subset(alcohol_heavy, select = -c(State, County))
  features[is.na(features)] <- 0
  
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 8
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
  
  colnames(components) <- c('Heavy_Alcohol_Factor')
  alcohol_heavy <- cbind(alcohol_heavy,components)
  cormat <- round(cor(subset(alcohol_heavy, select = -c(State, County)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  colnames(alcohol_heavy)[10] = 'Heavy_Alcohol_2012'
  
  alcohol_heavy <- subset(alcohol_heavy, select = c(State, County, Heavy_Alcohol_2012, Heavy_Alcohol_Factor))
  
  temp <- filter(melt(alcohol_binge, id.vars = c('State', 'Location')), substr(variable,1,1) == "X")
  temp['year'] = substr(temp$variable,2,5)
  temp['gender'] = substr(temp$variable,6,1000)
  temp <- filter(temp, gender == '.Both.Sexes')
  
  alcohol_binge <- dcast(
    select(temp, c('State', 'Location', 'value', 'year')),
    State+Location ~ year, value.var = c("value"))
  colnames(alcohol_binge)[2] <- c("County")
  
  features <- subset(alcohol_binge, select = -c(State, County))
  features[is.na(features)] <- 0
  
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 11
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
  
  colnames(components) <- c('Binge_Alcohol_Factor')
  alcohol_binge <- cbind(alcohol_binge,components)
  cormat <- round(cor(subset(alcohol_binge, select = -c(State, County)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  colnames(alcohol_binge)[10] = 'Binge_Alcohol_2012'
  
  alcohol_binge <- subset(alcohol_binge, select = c(State, County, Binge_Alcohol_2012, Binge_Alcohol_Factor))
  
  alcohol_data <- cbind(alcohol_heavy, subset(alcohol_binge, select = c(Binge_Alcohol_2012, Binge_Alcohol_Factor)))
  
  remove(components, sess, init, train, optimizer, loss, hidden, Output, X, W, b, num_inputs, num_outputs, num_hidden, learning_rate,
         cormat, features, alcohol_binge, alcohol_heavy)
  
  return(alcohol_data)
}