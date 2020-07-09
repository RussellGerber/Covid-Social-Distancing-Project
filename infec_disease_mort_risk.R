autoencode <- function(dataset, features, name){
  # 1D AutoEncoder here using Tensorflow instead of H2o
  num_inputs = 35
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
  
  colnames(components) <- c(paste(name,"_Factor", sep = ''))
  dataset <- cbind(dataset,components)
  cormat <- round(cor(subset(dataset, select = -c(FIPS, location_name)), use = 'pairwise.complete.obs'),2)
  plot <- ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
  
  colnames(dataset)[ncol(dataset)-1] <- c(paste(name,"_2014", sep = ''))
  
  return(list(dataset[,c(1,2,ncol(dataset)-1,ncol(dataset))], plot))
}

Process <- function(DATA){
  features <- subset(DATA, select = -c(FIPS, location_name))
  features[is.na(features)] <- 0
  FileName = deparse(substitute(DATA))
  outlist <- autoencode(dataset = DATA, features = features, name = FileName)  
  return(outlist)
}  


infec_disease_mort_risk <- function(){
  ID_Path <- paste(Health_path,"INFECT_DIS_MORT/", sep = "")
  ID_Files = list.files(path = ID_Path, pattern = NULL, all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  file <- ID_Files[1]
  ID_Mort_Risk <- data.frame(read.csv(paste(ID_Path,file, sep = "")))
  L = length(ID_Files)-6
  for(i in 2:L) {
    file <- ID_Files[i]
    # print(file)
    temp <- data.frame(read.csv(paste(ID_Path,file, sep = "")))
    ID_Mort_Risk <- rbind.data.frame(ID_Mort_Risk,temp)
  }
  remove(L, file, temp, ID_Path, ID_Files)
  
  Tuberculosis_Male <- dcast(subset(ID_Mort_Risk, ID_Mort_Risk$cause_name == 'Tuberculosis' & sex == 'Male', 
            select = -cbind(measure_id, measure_name, location_id, sex_id, sex, age_id, age_name, metric, upper, lower, cause_id, cause_name)), 
                            location_name+FIPS ~ year_id, value.var = c('mx'))
  outlist <- Process(Tuberculosis_Male)
  outlist[[2]]
  Tuberculosis_Male <- outlist[[1]]  
  
  
  Tuberculosis_Female <- dcast(subset(ID_Mort_Risk, ID_Mort_Risk$cause_name == 'Tuberculosis' & sex == 'Female', 
            select = -cbind(measure_id, measure_name, location_id, sex_id, sex, age_id, age_name, metric, upper, lower, cause_id, cause_name)), 
                             location_name+FIPS ~ year_id, value.var = c('mx'))
  outlist <- Process(Tuberculosis_Female)
  outlist[[2]]
  Tuberculosis_Female <- outlist[[1]]  
  
  
  Lower_Resp_Male <- dcast(subset(ID_Mort_Risk, ID_Mort_Risk$cause_name == 'Lower respiratory infections' & sex == 'Male', 
             select = -cbind(measure_id, measure_name, location_id, sex_id, sex, age_id, age_name, metric, upper, lower, cause_id, cause_name)), 
                             location_name+FIPS ~ year_id, value.var = c('mx'))
  outlist <- Process(Lower_Resp_Male)
  outlist[[2]]
  Lower_Resp_Male <- outlist[[1]]  
  
  
  Lower_Resp_Female <- dcast(subset(ID_Mort_Risk, ID_Mort_Risk$cause_name == 'Lower respiratory infections' & sex == 'Female', 
            select = -cbind(measure_id, measure_name, location_id, sex_id, sex, age_id, age_name, metric, upper, lower, cause_id, cause_name)), 
                               location_name+FIPS ~ year_id, value.var = c('mx'))
  outlist <- Process(Lower_Resp_Female)
  outlist[[2]]
  Lower_Resp_Female <- outlist[[1]]  
  
  ID_Mort_Risk <- cbind(Tuberculosis_Male,
                           Tuberculosis_Female[,3:4],
                           Lower_Resp_Male[,3:4],
                           Lower_Resp_Female[,3:4])
  return(select(ID_Mort_Risk, -c(location_name)))
}