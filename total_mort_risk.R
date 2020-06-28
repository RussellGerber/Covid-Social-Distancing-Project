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



total_mort_risk <- function(){
  
  TM_Path <- paste(Health_path,"LE_MORTALITY/", sep = "")
  TM_Files = list.files(path = TM_Path, pattern = NULL, all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  file <- TM_Files[1]
  Total_Mort_Risk <- data.frame(read.csv(paste(TM_Path,file, sep = "")))
  L = length(TM_Files)-5
  for(i in 2:L) {
    file <- TM_Files[i]
    # print(file)
    temp <- data.frame(read.csv(paste(TM_Path,file, sep = "")))
    Total_Mort_Risk <- rbind.data.frame(Total_Mort_Risk,temp)
  }
  remove(L, file, temp, TM_Path, TM_Files)
  Total_Mort_Risk <- subset(Total_Mort_Risk,Total_Mort_Risk$measure_name == 'Mortality risk', 
                            select = -cbind(measure_id, measure_name, location_id, sex_id, age_id, metric, upper, lower))
  
  Total_Mort_Risk_25_45_male <- dcast(subset(Total_Mort_Risk, age_name == "25 to 45" & sex == "Male", select = -c(age_name, sex)), 
                                      location_name+FIPS ~ year_id, value.var = c('val'))
  outlist <- Process(Total_Mort_Risk_25_45_male)
  outlist[[2]]
  Total_Mort_Risk_25_45_male <- outlist[[1]]  
  
    
  Total_Mort_Risk_25_45_female <- dcast(subset(Total_Mort_Risk, age_name == "25 to 45" & sex == "Female", select = -c(age_name, sex)), 
                                        location_name+FIPS ~ year_id, value.var = c('val'))
  outlist <- Process(Total_Mort_Risk_25_45_female)
  outlist[[2]]
  Total_Mort_Risk_25_45_female <- outlist[[1]]
  
  
  Total_Mort_Risk_45_65_male <- dcast(subset(Total_Mort_Risk, age_name == "45 to 65" & sex == "Male", select = -c(age_name, sex)), 
                                      location_name+FIPS ~ year_id, value.var = c('val'))
  outlist <- Process(Total_Mort_Risk_45_65_male)
  outlist[[2]]
  Total_Mort_Risk_45_65_male <- outlist[[1]]
  
  
  Total_Mort_Risk_45_65_female <- dcast(subset(Total_Mort_Risk, age_name == "45 to 65" & sex == "Female", select = -c(age_name, sex)), 
                                        location_name+FIPS ~ year_id, value.var = c('val'))
  outlist <- Process(Total_Mort_Risk_45_65_female)
  outlist[[2]]
  Total_Mort_Risk_45_65_female <- outlist[[1]]
  
   
  Total_Mort_Risk_65_85_male <- dcast(subset(Total_Mort_Risk, age_name == "65 to 85" & sex == "Male", select = -c(age_name, sex)), 
                                        location_name+FIPS ~ year_id, value.var = c('val'))
  outlist <- Process(Total_Mort_Risk_65_85_male)
  outlist[[2]]
  Total_Mort_Risk_65_85_male <- outlist[[1]]
  
  
  Total_Mort_Risk_65_85_female <- dcast(subset(Total_Mort_Risk, age_name == "65 to 85" & sex == "Female", select = -c(age_name, sex)), 
                                        location_name+FIPS ~ year_id, value.var = c('val'))
  outlist <- Process(Total_Mort_Risk_65_85_female)
  outlist[[2]]
  Total_Mort_Risk_65_85_female <- outlist[[1]]  

  Total_Mort_Risk <- cbind(Total_Mort_Risk_25_45_male,
                           Total_Mort_Risk_25_45_female[,3:4],
                           Total_Mort_Risk_45_65_male[,3:4],
                           Total_Mort_Risk_45_65_female[,3:4],
                           Total_Mort_Risk_65_85_male[,3:4],
                           Total_Mort_Risk_65_85_female[,3:4])
    
  return(select(Total_Mort_Risk, -c(location_name)))
}





  