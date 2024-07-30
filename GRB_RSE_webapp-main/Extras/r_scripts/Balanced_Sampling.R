
balancing <- function(set,response,ll){
  
  #print(hist(response))
  H <- hist(response,breaks=seq(min(response),max(response),l=ll+1))
  
  balance_sample <- cbind(response,set) # COMBINING THE PREDICTORS AND RESPONSE
  new_balance_sample <- balance_sample
  mean_count <- mean(H$counts)
  
  for (i in 1:length(H$counts)) {
    #print(H$breaks[i])
    # FOR iTH COUNT THE RESPONSE LIMITS ARE i AND i+1
    
    #print(nrow(balance_sample[(balance_sample$response >= H$breaks[i])&
     #                         (balance_sample$response <= H$breaks[i+1])
      #                        ,]
       #        ))
    
    temp <- balance_sample[(balance_sample$response >= H$breaks[i])&
                             (balance_sample$response <= H$breaks[i+1])
                           ,] 
    # temp HERE CONTAINS THE DATA POINTS WHICH FALL WITHIN
    # A PARTICULAR BIN OF THE DISTRIBUTION
    
    # IF THE NUMBER OF DATA POINTS IS LESS THAN
    # 75% OF THE MEAN COUNT OF THE DISTRIBUTION
    # THEN THOSE DATA POINTS BELOW TO THE MINORITY
    # THEY NEED TO BE INCREASED IN NUMBER.
    # FOR THAT THEY ARE REPEAT BY THE RATIO OF MEAN COUNT TO NUMBER OF POINTS IN THAT BIN
    # THE floor() FUNCTION IS USED TO ENSURE THE COPYING OF THE DATA POINTS 
    # DOESNT INCREASE BEYOND THE MEAN COUNT
    if(nrow(temp) < mean_count*0.75){
      #print(nrow(temp))
        #print(round(mean_count/nrow(temp)))
      for (j in 1:floor(mean_count/nrow(temp))) {
          #print(j)
          new_balance_sample <- rbind(new_balance_sample,temp)
      }  
      
    }
    remove(temp)
  }
  
  #print(hist(new_balance_sample$response,breaks=seq(min(new_balance_sample$response),max(new_balance_sample$response),l=10+1)))
  #plot(hist(new_balance_sample$response,breaks=seq(min(new_balance_sample$response),max(new_balance_sample$response),l=10+1)))
  #plot(H,add=T)
  
  ################# FOR THE GRBS CASE ONLY ########
  #train_set <- new_balance_sample[,-1]
  #invZtrain <- new_balance_sample[,1]
  #return()
  #################################################
  
  return(new_balance_sample)
}