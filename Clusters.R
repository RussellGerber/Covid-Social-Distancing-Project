library(openxlsx)
library(tensorflow)
library(reshape2)
library(tidyverse)


Factor_Maker <- function(features){
    jiggy <- as.data.frame(complete.cases(features))
    jiggy[jiggy == TRUE] <- 1
    jiggy[jiggy == 0] <- NA
    features <- features[complete.cases(features),]
    # features[is.na(features)] <- 0
    
    # 1D AutoEncoder here using Tensorflow instead of H2o
    num_inputs = dim(features)[2]
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
    for (i in 1:2000){
      sess$run(train, feed_dict=dict(X=data.matrix(features)))
    }
    components = data.frame(sess$run(hidden, feed_dict=dict(X=data.matrix(features))))

    colnames(components) <- c('Factor') 
    features <- cbind(features,components)
    cormat <- round(cor(features, use = 'pairwise.complete.obs'),2) 
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1)) 
    
    # components$Factor <- (components$Factor-min(components$Factor))/max(components$Factor)-min(components$Factor)  
    components$Factor <- (components$Factor-mean(components$Factor))/sd(components$Factor)
    jiggy[is.na(jiggy) == FALSE,] <- components 
    
    return(jiggy)
}

MakeOutput <- function(){ 
  County_Data <- read.xlsx("C:/Users/russe/Documents/Covid Project Data/County Data.xlsx")
  
  # Correlation matrix for Smoking shows extremely high positive interrelation between variables. 
    cormat <- round(cor(select(County_Data, "All_Smokers_Mean_2012",               
                             "All_Smoking_Factor",            
                             "Daily_Smokers_Mean_2012",            
                             "Daily_Smoking_Factor"), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
    
    select(County_Data, "All_Smokers_Mean_2012",               
           "All_Smoking_Factor",            
           "Daily_Smokers_Mean_2012",            
           "Daily_Smoking_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
            Factor_Maker() -> Smoking
    
  
  
  # Correlation matrix for Alcohol shows very high positive interrelation between variables.
    cormat <- round(cor(select(County_Data, "Heavy_Alcohol_2012",                  
                             "Heavy_Alcohol_Factor",               
                             "Binge_Alcohol_2012" ,                 
                             "Binge_Alcohol_Factor"), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
    select(County_Data, "Heavy_Alcohol_2012",                  
           "Heavy_Alcohol_Factor",               
           "Binge_Alcohol_2012" ,                 
           "Binge_Alcohol_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
      Factor_Maker() -> Alcohol
    
    
    
  # Correlation matrix for Diabetes shows high positive interrelation between variables.
    cormat <- round(cor(select(County_Data, "Diabetes_Total_2012",                 
                             "Diabetes_Total_Factor",               
                             "Diabetes_Aware_2012",              
                             "Diabetes_Aware_Factor"), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
    select(County_Data, "Diabetes_Total_2012",                 
           "Diabetes_Total_Factor",               
           "Diabetes_Aware_2012",              
           "Diabetes_Aware_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
            Factor_Maker() -> Diabetes
    
    
  
  # Correlation matrix for Life Expectancy shows very high positive interrelation between variables.
    cormat <- round(cor(select(County_Data, "Life_Exp_Male_2010",              
                             "Life_Exp_Male_Factor",   
                             "Life_Exp_Female_2010",
                             "Life_Exp_Female_Factor"), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
    select(County_Data, "Life_Exp_Male_2010",              
           "Life_Exp_Male_Factor",   
           "Life_Exp_Female_2010",
           "Life_Exp_Female_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
            Factor_Maker() -> Life_Expectancy
    
    
    
  # Correlation matrix for Cardio+Respiratory shows meh positive interrelation between variables.
    cormat <- round(cor(select(County_Data, "Cardiovascular_Mort_2014",           
                             "Cardiovascular_Mort_Factor",
                             "Chron_Resp_Mort_2014",         
                             "Chron_Resp_Mort_Factor"), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
    select(County_Data, "Cardiovascular_Mort_2014",           
           "Cardiovascular_Mort_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
            Factor_Maker() -> Cardio
    
    select(County_Data, "Chron_Resp_Mort_2014",         
           "Chron_Resp_Mort_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
            Factor_Maker() -> Chron_Resp
    
    
  
  # Correlation matrix for Obesity shows fairly positive interrelation between variables, but males/female obesity is disjoint.
    cormat <- round(cor(select(County_Data, "Obesity_Female_2011",                
                             "Obesity_Female_Factor",               
                             "Obesity_Male_2011",                
                             "Obesity_Male_Factor"), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
    select(County_Data, "Obesity_Female_2011",                
           "Obesity_Female_Factor",               
           "Obesity_Male_2011",                
           "Obesity_Male_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
            Factor_Maker() -> Obesity
    
    
    
  # Correlation matrix for Total Mortality Risk shows two groupings, each with very high positive interrelation between variables in the 
  # grouping. Those older than 65 and those younger than 65 form the two groups.
    cormat <- round(cor(select(County_Data, starts_with("Total_M")), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
      select(County_Data, "Total_Mort_Risk_25_45_male_2014", "Total_Mort_Risk_25_45_male_Factor", 
                                    "Total_Mort_Risk_25_45_female_2014", "Total_Mort_Risk_25_45_female_Factor", 
                                    "Total_Mort_Risk_45_65_male_2014", "Total_Mort_Risk_45_65_male_Factor", 
                                    "Total_Mort_Risk_45_65_female_2014", 
                                    "Total_Mort_Risk_45_65_female_Factor") %>% 
                                    apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
                                    Factor_Maker() -> Total_Mortality_25_65
    
    select(County_Data, "Total_Mort_Risk_65_85_male_2014", "Total_Mort_Risk_65_85_male_Factor", 
                                    "Total_Mort_Risk_65_85_female_2014", 
                                    "Total_Mort_Risk_65_85_female_Factor") %>% 
                                    apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
                                    Factor_Maker() -> Total_Mortality_65_85
    
    
    
  # Correlation matrix for Tuberculosis shows high positive interrelation between variables.
    cormat <- round(cor(select(County_Data, "Tuberculosis_Male_2014",              
                             "Tuberculosis_Male_Factor",           
                             "Tuberculosis_Female_2014",         
                             "Tuberculosis_Female_Factor"), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
    select(County_Data, "Tuberculosis_Male_2014",              
           "Tuberculosis_Male_Factor",           
           "Tuberculosis_Female_2014",         
           "Tuberculosis_Female_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
            Factor_Maker() -> Tuberculosis
    
    
  
  # Correlation matrix for Lower Respiratory shows high positive interrelation between variables.
    cormat <- round(cor(select(County_Data, "Lower_Resp_Male_2014",                
                             "Lower_Resp_Male_Factor",            
                             "Lower_Resp_Female_2014",            
                             "Lower_Resp_Female_Factor"), use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.5, limit = c(0,1))
  
    select(County_Data, "Lower_Resp_Male_2014",                
           "Lower_Resp_Male_Factor",            
           "Lower_Resp_Female_2014",            
           "Lower_Resp_Female_Factor") %>% apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))) %>% 
            Factor_Maker() -> Lower_Resp
    
    
  Health_Vars <- select(County_Data, "Life_Exp_Male_2010", "Life_Exp_Male_Factor", "Life_Exp_Female_2010",
           "Life_Exp_Female_Factor", "Heavy_Alcohol_2012", "Heavy_Alcohol_Factor", "Binge_Alcohol_2012",                 
           "Binge_Alcohol_Factor", "All_Smokers_Mean_2012", "All_Smoking_Factor", "Daily_Smokers_Mean_2012",            
           "Daily_Smoking_Factor", "Diabetes_Total_2012", "Diabetes_Total_Factor", "Diabetes_Aware_2012",              
           "Diabetes_Aware_Factor", "Obesity_Female_2011", "Obesity_Female_Factor", "Obesity_Male_2011",                
           "Obesity_Male_Factor", "Cardiovascular_Mort_2014", "Cardiovascular_Mort_Factor", "Chron_Resp_Mort_2014",         
           "Chron_Resp_Mort_Factor",  starts_with("Total_M"), "Tuberculosis_Male_2014", "Tuberculosis_Male_Factor",           
           "Tuberculosis_Female_2014", "Tuberculosis_Female_Factor", "Lower_Resp_Male_2014", "Lower_Resp_Male_Factor",            
           "Lower_Resp_Female_2014", "Lower_Resp_Female_Factor")
  
  
    
  # Correlation matrix for all health background variables.
    cormat <- round(cor(Health_Vars, use = 'pairwise.complete.obs'),2)
    ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", midpoint = 0.0, limit = c(-1,1))
  
  # PCA for All 44 Health variables 
    Test <- prcomp(Health_Vars  %>% na.omit() %>% 
                   apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))), scale = FALSE)
    PCA_Loadings <- Test[['rotation']]
    
    Melted <- melt(PCA_Loadings)
    Melted$value <- Melted$value/(max(Melted$value)-min(Melted$value)) 
    
    ggplot(data = Melted, aes(x=Var2, y=Var1, fill=value)) + 
      geom_tile()+scale_fill_gradient2(high = "blue", low = "red", mid = "white", 
                                       midpoint = 0.0, limit = c(-1,1))
    
    #### OUTPUTS #### 
    Correlation_Matrix <- melt(cormat)
    # Melted
    Scree = Test[['sdev']]^2/sum(Test[['sdev']]^2)
    
    write.xlsx(Correlation_Matrix, file = "C:/Users/russe/Documents/Covid Project Data/Correlation_Matrix_Data.xlsx")
    write.xlsx(Melted, file = "C:/Users/russe/Documents/Covid Project Data/PCA_Loadings_Data.xlsx")
    write.xlsx(Scree, file = "C:/Users/russe/Documents/Covid Project Data/PCA_Scree_Data.xlsx")
    
    
  # K-Means Clustering for All 44 Health Variables
    make_cluster <- function(numclusters){
      HealthCluster <- kmeans(Health_Vars %>% na.omit() %>% 
                  apply(MARGIN = 2, FUN = function(X) (X - min(X, na.rm = TRUE))/diff(range(X,na.rm = TRUE))),
                centers = numclusters, nstart = 20)
      return(HealthCluster$cluster)
      }
    Clusters = make_cluster(2) %>% as.data.frame()
    for (i in 3:9){
      Clusters <- cbind(Clusters, make_cluster(i))
    } 
   colnames(Clusters) <- c("Health 2 Cats", "Health 3 Cats", "Health 4 Cats", "Health 5 Cats", "Health 6 Cats", "Health 7 Cats", 
                           "Health 8 Cats", "Health 9 Cats")
    
   Cluster_Graph_Data <- cbind(County_Data$FIPS, County_Data$state.x, Life_Expectancy, Alcohol, Smoking, Diabetes, Cardio, Chron_Resp, Obesity, Total_Mortality_25_65, 
         Total_Mortality_65_85, Tuberculosis, Lower_Resp)
   colnames(Cluster_Graph_Data) <- c("FIPS", 'state.x', 'Life_Expectancy', 'Alcohol', 'Smoking', 'Diabetes', 'Cardio', 'Chron_Resp', 'Obesity', 
                                     'Total_Mortality_25_65', 'Total_Mortality_65_85', 'Tuberculosis', 'Lower_Resp')
            
   Cluster_Graph_Data <- merge.data.frame(Cluster_Graph_Data, Clusters, by=0, all.x = TRUE) %>% select(-c(Row.names))
   
   write.xlsx(Cluster_Graph_Data, file = "C:/Users/russe/Documents/Covid Project Data/Health_Cluster_Graph_Data.xlsx") 
} 
 
 
MakeOutput()  
 
 
 
 