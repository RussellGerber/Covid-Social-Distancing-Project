obesity_data <- function(){
  Obesity <- paste(Health_path,"IHME_USA_OBESITY_PHYSICAL_ACTIVITY_2001_2011.csv", sep = "")
  Obesity <- data.frame(read.csv(Obesity))
  Obesity <- melt(
                  subset(Obesity, Outcome == 'Obesity', select = -c(merged_fips, Outcome)),
                  id.vars = c('State', 'County', 'fips', 'Sex')
                 )
  Obesity$Year <- substr(Obesity$variable, 12, 15)
  Obesity <- Obesity[substr(Obesity$variable,1,1) != "D",]
  Obesity <- Obesity[substr(Obesity$variable,20,30) != "er.bound...", c('State', 'County', 'fips', 'Sex', 'value', 'Year')]
  
  Obesity <- dcast(Obesity, State+County+fips+Sex ~ Year, value.var = c('value'))
  
  Obesity_Male <- Obesity[Obesity$Sex == "Male",c(1,2,3,5:15)]
    
  features <- subset(Obesity_Male, select = -c(fips, State, County))
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
  
  # hidden = tf$matmul(X, W)+b
  hidden = tf$nn$relu(tf$matmul(X, W)+b)
  Output = tf$matmul(hidden-b,tf$transpose(W)) 
  
  loss = tf$reduce_mean(tf$square(Output-X))
  
  optimizer = tf$train$AdamOptimizer(learning_rate = learning_rate)
  train = optimizer$minimize(loss)
  
  init = tf$global_variables_initializer()
  
  sess$run(init)
  for (i in 1:1000){
    sess$run(train, feed_dict=dict(X=data.matrix(features)))
  }
  components = data.frame(sess$run(hidden, feed_dict=dict(X=data.matrix(features))))
  

  colnames(components) <- c('Obesity_Male_Factor')
  Obesity_Male <- cbind(Obesity_Male,components)
  cormat <- round(cor(subset(Obesity_Male, select = -c(fips, State, County)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
  colnames(Obesity_Male)[14] = 'Obesity_Male_2011'
  
  Obesity_Male <- subset(Obesity_Male, select = c(fips, State, County, Obesity_Male_2011, Obesity_Male_Factor))

  
  Obesity_Female <- Obesity[Obesity$Sex != "Male",c(1,2,3,5:15)]
  
  features <- subset(Obesity_Female, select = -c(fips, State, County))
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
  
  # hidden = tf$matmul(X, W)+b
  hidden = tf$nn$relu(tf$matmul(X, W)+b)
  Output = tf$matmul(hidden-b,tf$transpose(W)) 
  
  loss = tf$reduce_mean(tf$square(Output-X))
  
  optimizer = tf$train$AdamOptimizer(learning_rate = learning_rate)
  train = optimizer$minimize(loss)
  
  init = tf$global_variables_initializer()
  
  sess$run(init)
  for (i in 1:1000){
    sess$run(train, feed_dict=dict(X=data.matrix(features)))
  }
  components = data.frame(sess$run(hidden, feed_dict=dict(X=data.matrix(features))))
  
  
  colnames(components) <- c('Obesity_Female_Factor')
  Obesity_Female <- cbind(Obesity_Female,components)
  cormat <- round(cor(subset(Obesity_Female, select = -c(fips, State, County)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
  colnames(Obesity_Female)[14] = 'Obesity_Female_2011'
  
  Obesity_Female <- subset(Obesity_Female, select = c(fips, State, County, Obesity_Female_2011, Obesity_Female_Factor))
                     
  Obesity_data <- select(cbind(Obesity_Female, Obesity_Male[,4:5]), -c(State, County))
  colnames(Obesity_data)[1] = "FIPS"
  return(Obesity_data)
}
