#By Mona Hadibarhaghtalab
#9531228


library(rpart)

library(ada)
#library(party)
#library(randomForest)

train.data = read.csv("clicks_train_t.csv")
test.data = read.csv("clicks_test_t.csv")
#define a constant
FIRST.VALUE <- 16874594 - 1



#decision tree
#sample.data = read.csv("sample_submission.csv")
#decision.tree<-rpart(clicked ~ ad_id , data=train.data, method = "class")
#glm.res<-glm(clicked ~ ad_id, data=train.data, family=binomial())
#table.decision <- table(predict(decision.tree, newdata=test.data),test.data$clicked)

ada.res <- ada(clicked~ad_id, data = train.data,  type = "discrete",  iter = 70)
ada.res <- addtest(ada.res, test.data, test.data)

clicked <- predict(ada.res, newdata=test.data);




result <- cbind(test.data, clicked)


last.res <- matrix(NA, nrow = 10000 , ncol = 2)



#function sort data by special method
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
  


#random forest
#rf <- randomForest(clicked ~ ad_id + display_id,data=train.data,ntree=100,proximity=TRUE)
#table.forest <-table(predict(rf, newdata=test.data),test.data$Species)
#print(predict(rf, newdata=test.data))


#add column name
#last.res <- cbind(test.data,clicked)
last.res <- last.res[(1:index), ]
colnames(last.res) <- c("display_id", "ad_id")

#write in file 
write.csv(last.res, file = "result.csv", row.names = FALSE)


#draw plot
#splot(decision.tree)