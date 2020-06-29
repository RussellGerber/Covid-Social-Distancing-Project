cardiovascular_data <- function(){
  Mortality = paste(Health_path,"IHME_USA_COUNTY_MORTALITY_RATES_1980_2014_NATIONAL_Y2016M12D13.xlsx", sep = "")
  Cardiovascular_Mortality <- data.frame(read.xlsx(xlsxFile = Mortality, sheet = 'Cardiovascular diseases', startRow = 2))
  
  temp <- melt(Cardiovascular_Mortality, id.vars = c('Location', 'FIPS'))
  temp['year'] = substr(temp$variable,17,20)
  
  temp <- subset(separate(data = temp, col = value, into = c("val1","val2"), sep = " ", remove = TRUE,
                          convert = TRUE, extra = "merge", fill = "warn"), select = -c(val2))
  
  Cardiovascular_Mortality <- dcast(
    select(temp, c('FIPS', 'Location', 'val1', 'year')),
    FIPS+Location ~ year, value.var = c("val1"))
  Cardiovascular_Mortality <- within(Cardiovascular_Mortality, rm(tali)) 
  
  features <- subset(Cardiovascular_Mortality, select = -c(FIPS, Location))
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
  
  colnames(components) <- c('Cardiovascular_Mort_Factor')
  Cardiovascular_Mortality <- cbind(Cardiovascular_Mortality,components)
  cormat <- round(cor(subset(Cardiovascular_Mortality, select = -c(FIPS, Location)), use = 'pairwise.complete.obs'),2)
  ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(-1,1))
  
  colnames(Cardiovascular_Mortality)[10] = 'Cardiovascular_Mort_2014'
  
  Cardiovascular_Mortality <- subset(Cardiovascular_Mortality, select = c(FIPS, Location, Cardiovascular_Mort_2014, Cardiovascular_Mort_Factor))
  
  
  return(select(Cardiovascular_Mortality, -c(Location)))
} 

