life_expectancy <- function(){
  
  Life_Expectancy <- paste(Health_path,"IHME_USA_LIFE_EXPECTANCY_1985_2010.csv", sep = "")
  Life_Expectancy <- data.frame(read.csv(Life_Expectancy))
  Life_Expectancy <- subset(Life_Expectancy, select = 
                              -c(Female.life.expectancy..state..years., 
                                 Male.life.expectancy..state..years., 
                                 Female.life.expectancy..national..years., 
                                 Male.life.expectancy..national..years.))
  
  Life_Expectancy_Male <- dcast(
    select(
      melt(
        subset(Life_Expectancy, select = -c(Female.life.expectancy..years.)),
        id.vars = c('State', 'County', 'fips', 'Year')
      ),-c(variable)
    ), State+County+fips~Year, value.var = c("value")
  )
  
  features <- subset(Life_Expectancy_Male, select = -c(fips, State, County))
  features[is.na(features)] <- 0
  
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 26
  num_hidden = 1
  num_outputs = num_inputs # Must be true for an autoencoder!
  learning_rate = 0.001
  
  sess = tf$Session()
  X = tf$placeholder(tf$float32, shape=shape(NULL, num_inputs), name='x')
  W = tf$Variable(tf$truncated_normal(shape=shape(num_inputs, num_hidden), stddev=0.05, seed = 1))
  b = tf$Variable(tf$truncated_normal(shape=shape(1, num_hidden), stddev=0.05, seed = 1))
  
  #hidden = tf$matmul(X, W)+b
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
  
  colnames(components) <- c('Life_Exp_Male_Factor')
  Life_Expectancy_Male <- cbind(Life_Expectancy_Male,components)
  cormat <- round(cor(subset(Life_Expectancy_Male, select = -c(fips, State, County)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(-1,1))
  
  colnames(Life_Expectancy_Male)[29] = 'Life_Exp_Male_2010'
  
  Life_Expectancy_Male <- subset(Life_Expectancy_Male, select = c(fips, State, County, Life_Exp_Male_2010, Life_Exp_Male_Factor))
  
  remove(components, sess, init, train, optimizer, loss, hidden, Output, X, W, b, num_inputs, num_outputs, num_hidden, learning_rate,
         cormat, features)
  
  
  Life_Expectancy_Female <- dcast(
    select(
      melt(
        subset(Life_Expectancy, select = -c(Male.life.expectancy..years.)),
        id.vars = c('State', 'County', 'fips', 'Year')
      ),-c(variable)
    ), State+County+fips~Year, value.var = c("value")
  )
  
  features <- subset(Life_Expectancy_Female, select = -c(fips, State, County))
  features[is.na(features)] <- 0
  
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 26
  num_hidden = 1
  num_outputs = num_inputs # Must be true for an autoencoder!
  learning_rate = 0.001
  
  sess = tf$Session()
  X = tf$placeholder(tf$float32, shape=shape(NULL, num_inputs), name='x')
  W = tf$Variable(tf$truncated_normal(shape=shape(num_inputs, num_hidden), stddev=0.05, seed = 1))
  b = tf$Variable(tf$truncated_normal(shape=shape(1, num_hidden), stddev=0.05, seed = 1))
  
  #hidden = tf$matmul(X, W)+b
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
  
  colnames(components) <- c('Life_Exp_Female_Factor')
  Life_Expectancy_Female <- cbind(Life_Expectancy_Female,components)
  cormat <- round(cor(subset(Life_Expectancy_Female, select = -c(fips, State, County)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(-1,1))
  
  colnames(Life_Expectancy_Female)[29] = 'Life_Exp_Female_2010'
  
  Life_Expectancy_Female <- subset(Life_Expectancy_Female, select = c(fips, State, County, Life_Exp_Female_2010, Life_Exp_Female_Factor))
  
  Life_Expectancy <- select(
                        cbind(Life_Expectancy_Male, Life_Expectancy_Female[,4:5]), -c(State, County))
  colnames(Life_Expectancy)[1] = "FIPS"  

  
  return(Life_Expectancy)
}