library(utils)
library(SuperLearner)

inflFunc <- function(all_data_scale_wo,s9){
	x= matrix(rep(as.numeric(all_data_scale_wo[1,]),500),nrow=500,byrow=T)
	nd <- x+matrix(rnorm(500*(ncol(all_data_scale_wo)),sd=0.2),500,(ncol(all_data_scale_wo)))
	colnames(nd)=names(all_data_scale_wo)
	nd1=as.data.frame(nd)
	#print(nd1)
	print("nd1 defined")
	prp <- predict(s9,newdata=nd1)$pred
	print("prp defined")
	mod <- lm(unlist(prp)~(nd)-1)
	influence = abs(mod$coefficients)/sum(abs(mod$coefficients))
	print("influence")
	 for(k in 2:nrow(all_data_scale_wo)){
	   x= matrix(rep(as.numeric(all_data_scale_wo[1,]),500),nrow=500,byrow=T)
	   nd <- x+matrix(rnorm(500*(ncol(all_data_scale_wo)),sd=0.2),500,(ncol(all_data_scale_wo)))
	   colnames(nd)=names(all_data_scale_wo)
	   nd1=as.data.frame(nd)
	   prp <- predict(s9,newdata=nd1)$pred
	   mod <- lm(unlist(prp)~(nd)-1)
	   influence = influence+abs(mod$coefficients)/sum(abs(mod$coefficients))

	}

	 inf <- influence/sum(influence)*100
	 #names(inf)=names(all_data_scale_wo[,-1])
	 names(inf)=names(all_data_scale_wo)
	 return(inf)
}
