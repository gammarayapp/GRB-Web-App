# NEW GAM FORMULA GENERATION
# THIS USES THE combn FUNCTION
# WITH THE PARAMETER m LOOPED OVER FROM
# 1 TO 9
# THIS SHOULD GENERATE MANY COMBINATIONS

####Function to add second order data to the input data set and return this modified data set######
SqrTermGen <- function(inputData){
  
  indVar = colnames(inputData)
 
  for (i in 1:length(indVar)){ #Loop over all variables in indVar
    for (j in i:(length(indVar))){ #Loop over all variables at index i and greater than index i
      #If i and j correspond to the same varaible call the variable varSqr
      if (indVar[i] == indVar[j]){
        inputData[[paste(indVar[i],"Sqr",sep="")]] <- inputData[,indVar[i]]*inputData[,indVar[j]]
      }#else{
       # inputData[[paste(indVar[i],indVar[j],sep="")]] <- inputData[,indVar[i]]*inputData[,indVar[j]]
       #}
    }
  }
  
  return(inputData) 
}

quadTermGen <- function(inputData){
  #indVar = c("LogFluence","LogT90","LogPeak","PhotonIndex","Alpha","LogTa","LogNH","LogFlux","Gamma")
  #indVar = c("LogFlux1.100m", "LogEnergy_Flux100", "Frac_Variability", "LogHighest_Energy", "PL_Index", "LogPivot_Energy", "LP_Index", "LP_beta","Gaia_G_Magnitude","w4","Lognufnu","Lognu")
  #indVar = NumOvar[c(-1,-2)]
  #indVar = NumOvar[c(-1,-2,-3,-4)]
  indVar = colnames(inputData)
  #print(indVar)
  #indVar
  for (i in 1:length(indVar)){ #Loop over all variables in indVar
    for (j in i:(length(indVar))){ #Loop over all variables at index i and greater than index i
      #If i and j correspond to the same varaible call the variable varSqr
      if (indVar[i] == indVar[j]){
        inputData[[paste(indVar[i],"Sqr",sep="")]] <- inputData[,indVar[i]]*inputData[,indVar[j]]
      }else{
        inputData[[paste(indVar[i],indVar[j],sep="")]] <- inputData[,indVar[i]]*inputData[,indVar[j]]
      }
    }
  }
  
  return(inputData)
}

#TrainData = read.csv("ServerTrainingData.csv",header = T,row.names = 1)
TrainData = read.csv(paste("OutputFiles/MEstimator","/grb_xray_m_est.csv",sep = ""),header = T,row.names = 1)

Response = TrainData$log10z

Predictor = subset(TrainData,select = lassovar)

O1Predictor_names = colnames(Predictor)

SQRPredictors = SqrTermGen(Predictor)

O2Predictors = quadTermGen(Predictor)

{
  O2=T
  if(O2){
    Predictor_names = colnames(SQRPredictors)
  }else{
    Predictor_names = colnames(Predictor)
  }

  term_matrix=c();
  
  for (i in 1:length(Predictor_names)) {
    term_matrix[[i]] <- combn(Predictor_names, m = i)
  }
 
  dim(term_matrix[[3]])[2]
 
  formula_list=vector()
  index=1
  gam_summary=c()

  for (m in 1:length(Predictor_names)) {

    j = dim(term_matrix[[m]])[2]
    print(j)

    for(j in 1:dim(term_matrix[[m]])[2] ){
      
      test1=c()
      for(i in 1:dim(term_matrix[[m]])[1]){
        
        if(dim(term_matrix[[m]])[1] != 1){
          test1 = paste( term_matrix[[m]][i,j], test1 ,sep="+")
        }else{
          test1 = paste( term_matrix[[m]][i,j],":",term_matrix[[m]][i,j],"+",sep="")
        }
      }  
      #print(Predictor_names[!(Predictor_names %in% term_matrix[[3]][,1])])
      
      term_to_add = Predictor_names[!(Predictor_names %in% term_matrix[[m]][,j])]
      
      if(dim(term_matrix[[m]])[1] != 1){
        test1 = paste("(",substr(test1,1,str_length(test1)-1),")^2",sep="") # str_length(xx) - 1 TAKES CARE OF THE + SYMBOL ADDED AT THE END
      }else{
        test1=substr(test1,1,str_length(test1)-1)
      }  
      
      for(ii in term_to_add){ # THIS ADDS THE TERMS NOT INCLUDED
        test1 = paste(test1,ii,sep="+") # SO WE ALWAYS HAVE THE 9 PREDICTORS
      }
      
      #final_formula = as.formula(paste("Response ~ ",test1))
      final_formula = paste("Response ~ ",test1)
      
      formula_list[index] <- final_formula
      
      # THE LINE BELOW IS FOR TESTING IF THE FORMULA IS WORKING OR NOT
      #gam_summary[[index]]=summary(mgcv::gam(final_formula,data = cbind(Response,SQRPredictors),family = gaussian()))

      index=index+1
      # A+B+C == A^2 +B+C == A:A +B+C
    }
  }

  if(O2){
    write.csv(formula_list,paste0("Formula_Generation/O2_formula_list"))
  }else{
    write.csv(formula_list,paste0("Formula_Generation/O1_formula_list"))
  }
}

{
  O2=F
  if(O2){
    Predictor_names = colnames(SQRPredictors)
  }else{
    Predictor_names = colnames(Predictor)
  }
  
  term_matrix=c();
  
  for (i in 1:length(Predictor_names)) {
    term_matrix[[i]] <- combn(Predictor_names, m = i)
  }
  
  dim(term_matrix[[3]])[2]
  
  
  formula_list=vector()
  index=1
  gam_summary=c()
  
  for (m in 1:length(Predictor_names)) {
    
    j = dim(term_matrix[[m]])[2]
    print(j)

    for(j in 1:dim(term_matrix[[m]])[2] ){
      
      test1=c()
      for(i in 1:dim(term_matrix[[m]])[1]){
        
        if(dim(term_matrix[[m]])[1] != 1){
          test1 = paste( term_matrix[[m]][i,j], test1 ,sep="+")
        }else{
          test1 = paste( term_matrix[[m]][i,j],":",term_matrix[[m]][i,j],"+",sep="")
        }
      }  
      #print(Predictor_names[!(Predictor_names %in% term_matrix[[3]][,1])])
      
      term_to_add = Predictor_names[!(Predictor_names %in% term_matrix[[m]][,j])]
      
      if(dim(term_matrix[[m]])[1] != 1){
        test1 = paste("(",substr(test1,1,str_length(test1)-1),")^2",sep="") # str_length(xx) - 1 TAKES CARE OF THE + SYMBOL ADDED AT THE END
      }else{
        test1=substr(test1,1,str_length(test1)-1)
      }  
      
      for(ii in term_to_add){ # THIS ADDS THE TERMS NOT INCLUDED
        test1 = paste(test1,ii,sep="+") # SO WE ALWAYS HAVE THE 9 PREDICTORS
      }
      
      #final_formula = as.formula(paste("Response ~ ",test1))
      final_formula = paste("Response ~ ",test1)
      
      formula_list[index] <- final_formula
      
      # THE LINE BELOW IS FOR TESTING IF THE FORMULA IS WORKING OR NOT
      #gam_summary[[index]]=summary(mgcv::gam(final_formula,data = cbind(Response,SQRPredictors),family = gaussian()))
      
      
      index=index+1
      # A+B+C == A^2 +B+C == A:A +B+C  
    }
  }
  
  
  # sink("Gam_summary.txt")
  # print(gam_summary)
  # sink()
  # 
  # sink("All_formula.txt")
  # print(formula_list)
  # sink()
  
  if(O2){
    write.csv(formula_list,paste0("Formula_Generation/O2_formula_list"))
  }else{
    write.csv(formula_list,paste0("Formula_Generation/O1_formula_list"))
  }
}

# rsq=matrix(nrow=index,ncol = 2)
# 
# for (i in 1:(index-1)) {
#   rsq[i,] = c(i,gam_summary[[i]]$r.sq)
# }
# 
# 
# gam_summary[[index]]=summary(mgcv::gam(final_formula,data = cbind(Response,SQRPredictors),family = gaussian()))
# summary(mgcv::gam(formula_list[[19]],data = cbind(Response,SQRPredictors),family = gaussian()))
# 
# read_formula_list=read.csv(paste0("O2_formula_list_",length(Predictor_names),"features.csv"),header = T,row.names = 1)
# read_formula_list=read.csv("O1_formula_list_",length(Predictor_names),"features.csv",header = T,row.names = 1)
# 
# read_formula_list[1,]
# 
# for (i in 1:100) {
#   tryCatch(
#     summary(mgcv::gam(as.formula(read_formula_list[i,]),data = cbind(Response,O2Predictors),family = gaussian()))
#     ,error = function(cond){message(paste("error encountered at",i));message(cond)}
#     
#            )
# }
# 
# tryCatch(summary(mgcv::gam(as.formula(read_formula_list[261971,]),data = cbind(Response,O2Predictors),family = gaussian()))
#          ,error=function(cond){message("error encountered");message(cond)}
#          )


############# FOR SMOOTH FUNCTION ###########

Predictor_names = colnames(Predictor)

term_matrix=c();

for (i in 1:length(Predictor_names)) {
  term_matrix[[i]] <- combn(Predictor_names, m = i)
}

dim(term_matrix[[3]])[2]


formula_list=vector()
index=1
gam_summary=c()

for (m in 1:length(Predictor_names)) {
  
  j = dim(term_matrix[[m]])[2]
  print(j)

  for(j in 1:dim(term_matrix[[m]])[2] ){
    
    test1=c()
    test2=c()
    for(i in 1:dim(term_matrix[[m]])[1]){
      #print(term_matrix[[m]][i,j])
      
      if(dim(term_matrix[[m]])[1] != 1){
        test1 = paste( term_matrix[[m]][i,j], test1 ,sep=",")
        #test1 = paste( "s(",term_matrix[[m]][i,j],")+",sep="")
        test2 = paste( "s(",term_matrix[[m]][i,j],") +",test2,sep="")
        #print(test1)
        #print(test2)
      }else{
        test1 = paste( " s(",term_matrix[[m]][i,j],") +",sep="")
        #print(test1)
        }
    }  
    #print(Predictor_names[!(Predictor_names %in% term_matrix[[3]][,1])])
    
    term_to_add = Predictor_names[!(Predictor_names %in% term_matrix[[m]][,j])]
    
    if(dim(term_matrix[[m]])[1] != 1){
      test1 = paste("s(",substr(test1,1,str_length(test1)-1),") ",sep="") # str_length(xx) - 1 TAKES CARE OF THE + SYMBOL ADDED AT THE END
      #print(test1)
      
      test2 = paste(substr(test2,1,str_length(test2)-1),sep="") # str_length(xx) - 1 TAKES CARE OF THE + SYMBOL ADDED AT THE END
      #print(test2)
    }else{
      test1=substr(test1,1,str_length(test1)-1)
      #print(test1)
    }
    
    for(ii in term_to_add){ # THIS ADDS THE TERMS NOT INCLUDED
      test1 = paste(test1,ii,sep=" + ") # SO WE ALWAYS HAVE THE 9 PREDICTORS
      test2 = paste(test2,ii,sep=" + ")
    }
    
    #final_formula = as.formula(paste("Response ~ ",test1))
    #print(test1)
    #print(test2)
    
    
    if(m==1){ # IF M=1 THEN ONLY STORE THE FORMULA IN TEST1 NOT IN TEST2
      final_formula = paste("Response ~ ",test1)
      formula_list[index] <- final_formula
      index=index+1
      #print(final_formula)
    }
    if(m==2){ # IF M==2 THEN STORE BOTH TEST1 AND TEST2 FORMULAS
      final_formula = paste("Response ~ ",test1)
      formula_list[index] <- final_formula
      index=index+1
      #print(final_formula)
      
      final_formula = paste("Response ~ ",test2)
      formula_list[index] <- final_formula
      index=index+1
      #print(final_formula)
    }
    if(m>=3){ # IF M>2 THEN ONLY STORE TEST2 BECAUSE WE DONT WANT SMOOTH OF MORE THAN 2 VARIABLES AT A TIME
      # THAT IS WE DONT WANT s(A,B,C,...). WE ONLY WANT s(A,B) max
      final_formula = paste("Response ~ ",test2)
      formula_list[index] <- final_formula
      index=index+1
      #print(final_formula)
    }
  }
}

#formula_list
write.csv(formula_list,paste0("Formula_Generation/SmoothedO1_formula_list"))

# source("Find_Best_GAM.R")