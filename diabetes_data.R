diabetes_data <- function(){
  
  diabetes = paste(Health_path,"IHME_USA_COUNTY_DIABETES_PREVALENCE_1999_2012_NATIONAL_Y2016M08D23.xlsx", sep = "")
  #Total The proportion of adults age 20+ who report a previous diabetes diagnosis and/or have high FPG/A1C* (age-standardized)
  diabetes_total <- data.frame(read.xlsx(xlsxFile = diabetes,sheet = 'Total', startRow = 2))
  
  temp <- filter(melt(diabetes_total, id.vars = c('Location', 'FIPS')), substr(variable,19,1000) == "Both.Sexes")
  temp['year'] = substr(temp$variable,13,16)
  
  diabetes_total <- dcast(
    select(temp, c('FIPS', 'Location', 'value', 'year')),
    FIPS+Location ~ year, value.var = c("value"))
  
  features <- subset(diabetes_total, select = -c(FIPS, Location))
  features[is.na(features)] <- 0
  
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 14
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
  
  colnames(components) <- c('Diabetes_Total_Factor')
  diabetes_total <- cbind(diabetes_total,components)
  cormat <- round(cor(subset(diabetes_total, select = -c(FIPS, Location)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
  colnames(diabetes_total)[16] = 'Diabetes_Total_2012'
  
  diabetes_total <- subset(diabetes_total, select = c(FIPS, Location, Diabetes_Total_2012, Diabetes_Total_Factor))
  
  remove(components, sess, init, train, optimizer, loss, hidden, Output, X, W, b, num_inputs, num_outputs, num_hidden, learning_rate,
         cormat, features)
  
  
  # Awareness	The proportion of adults age 20+ with a previous diabetes diagnosis and/or high FPG/A1C* who have received a diagnosis (age-standardized)
  diabetes_aware <- data.frame(read.xlsx(xlsxFile = diabetes,sheet = 'Awareness', startRow = 2))
  
  temp <- filter(melt(diabetes_aware, id.vars = c('Location', 'FIPS')), substr(variable,19,1000) == "Both.Sexes")
  temp['year'] = substr(temp$variable,13,16)
  
  diabetes_aware <- dcast(
    select(temp, c('FIPS', 'Location', 'value', 'year')),
    FIPS+Location ~ year, value.var = c("value"))
  
  features <- subset(diabetes_aware, select = -c(FIPS, Location))
  features[is.na(features)] <- 0
  
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 14
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
  
  colnames(components) <- c('Diabetes_Aware_Factor')
  diabetes_aware <- cbind(diabetes_aware,components)
  cormat <- round(cor(subset(diabetes_aware, select = -c(FIPS, Location)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
  colnames(diabetes_aware)[16] = 'Diabetes_Aware_2012'
  
  diabetes_aware <- subset(diabetes_aware, select = c(FIPS, Location, Diabetes_Aware_2012, Diabetes_Aware_Factor))
  
  Diabetes_Data <- cbind(diabetes_total, diabetes_aware[,3:4])
  
  
  return(select(Diabetes_Data,-c(Location)))
}