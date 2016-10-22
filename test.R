#By Mona Hadibarhaghtalab & Mina Amirzade





output.res <- function(result){
  last.res <- matrix(NA, nrow = 10000 , ncol = 2)
  current.disp.id <- result[1,1]
  ad.list<-c()
  index <- 0
  flag <- 0
  for (i in 1:nrow(result)) {
    if(result[i, 1] == current.disp.id & result[i, 3] == 1 ){
      if(flag == 0){
        index <- index + 1
        flag <- 1
      }
      
      ad.list<-rbind(ad.list, result[i, 2])
      
      
      
    }
    else if(result[i, 1] > current.disp.id){
      if(length(ad.list) > 0){
        
        last.res[index, 2] <- paste(sort(ad.list, decreasing = FALSE), collapse = " ")
        last.res[index, 1]<- result[(i - 1), 1]
      }
      
      #last.index <- index
      
      ad.list <- c()
      flag <- 0
      current.disp.id <- result[i , 1]
      if(result[i, 3] == 1){
        flag <- 1
        ad.list <- rbind(ad.list, result[i, 2])
        index <- index + 1
      }
    }
    
    
    
  }
  
  if(length(ad.list) > 0){
    last.res[index, 2] <- paste(sort(ad.list, decreasing = FALSE), collapse = " ")
    last.res[index, 1]<- result[i, 1]
  }
  
  
  
  last.res <- last.res[(1:index), ]
  colnames(last.res) <- c("display_id", "ad_id")
  
  #write in file 
  write.csv(last.res, file = "result.csv", row.names = FALSE)
}


library(rpart)

train.data = read.csv("clicks_train_t.csv")
test.data = read.csv("clicks_test_t.csv")
#define a constant
FIRST.VALUE <- 16874594 - 1



glm.res<-glm(clicked ~ ad_id, data=train.data, family=binomial())
clicked <- ifelse(predict(glm.res, newdata=test.data, type="response")>0.193,1,0);
result <- cbind(test.data, clicked)




#function sort data by special method
output.res(result = result)

