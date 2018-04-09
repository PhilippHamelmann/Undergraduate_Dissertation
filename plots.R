require(ggplot2)
library(tidyr)

#base model

setwd("C:/Users/User/Documents/R/Dissertation/final_data")
data<-readRDS("data_base.RDS")

data$mults_gr_1<-0
data[which(data$mults>1),"mults_gr_1"]<-1
data$connected<-0
data[which(data$s_p_max!=Inf),"connected"]<-1

varlist<-c("homogamy","connected","mults_gr_1","mults","n","char_ratio", "sex_ratio","homophily","s_p_max","s_p_median","g_density","clustering")
data<-data[,varlist]
data[data==Inf]<-9*10^10

setwd("C:/Users/User/Documents/R/Dissertation/final_regressions")
res4<-readRDS("final_step.RDS")
res3<-readRDS("final_gen.RDS")

data$mults_gr_1<-as.factor(data$mults_gr_1)
data$connected<-as.factor(data$connected)
data<-data[complete.cases(data),]


{
ggplot(data=data,mapping = aes(x=homogamy))+geom_histogram(fill="dodgerblue1")+ scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1))
ggplot(data=data,mapping = aes(x=homophily))+geom_histogram(fill="dodgerblue1")+ scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1))
ggplot(data=data,mapping = aes(x=g_density))+geom_histogram(fill="dodgerblue1")+ scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1))+xlab("density")
ggplot(data=data,mapping = aes(x=clustering))+geom_histogram(fill="dodgerblue1")+ scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1))
ggplot(data=data[data$connected==1,],mapping = aes(x=s_p_max))+geom_histogram(fill="dodgerblue1")+ scale_x_continuous(breaks=c(1,5,10),limits = c(1,10))+xlab("diameter")
ggplot(data=data[data$connected==1,],mapping = aes(x=s_p_median))+geom_histogram(fill="dodgerblue1")+ scale_x_continuous(breaks=c(1,2,3,4,5),limits = c(1,5))+xlab("median shortest path")
} #histograms

{
#mults
plot_data_mults<-data[1:5000,]
plot_data_mults[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_mults[1:5000,"mults"]<-runif(5000,0,5)
plot_data_mults[1:5000,"mults_gr_1"]<-0
plot_data_mults[plot_data_mults$mults>=1,"mults_gr_1"]<-1
plot_data_mults[1:5000,"n"]<-100
plot_data_mults[1:5000,"char_ratio"]<-0.5
plot_data_mults[1:5000,"sex_ratio"]<-0.5
plot_data_mults[1:5000,"s_p_max"]<-4
plot_data_mults[1:5000,"s_p_median"]<-2
plot_data_mults[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_mults[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_mults[1:5000,"connected"]<-rbinom(5000,1,0.5)

prediction_mults_1<-predict(res4,plot_data_mults)
prediction_mults_2<-predict(res3,plot_data_mults)

ggplot(plot_data_mults,aes(x=plot_data_mults[1:5000,"mults"],y=prediction_mults_1[1:5000],colour=mults,shape=connected))+geom_point()+xlab("mults")+ylab("homogamy")+ labs(color='d')
ggplot(plot_data_mults,aes(x=plot_data_mults[1:5000,"mults"],y=prediction_mults_2[1:5000],colour=mults,shape=connected))+geom_point()+xlab("mults")+ylab("homogamy")+ labs(color='d')
}#mults

{
plot_data_homophily<-data[1:5000,]
plot_data_homophily[1:5000,"homophily"]<-runif(5000,0,1)
plot_data_homophily[1:2500,"mults"]<-runif(2500,0,1)
plot_data_homophily[2501:5000,"mults"]<-runif(2500,1,5)
plot_data_homophily[1:5000,"mults_gr_1"]<-0
plot_data_homophily[plot_data_homophily$mults>=1,"mults_gr_1"]<-1
plot_data_homophily[1:5000,"n"]<-100
plot_data_homophily[1:5000,"char_ratio"]<-mean(data$char_ratio)
plot_data_homophily[1:5000,"sex_ratio"]<-mean(data$sex_ratio)
plot_data_homophily[1:5000,"s_p_max"]<-median(data$s_p_max)
plot_data_homophily[1:5000,"s_p_median"]<-median(data$s_p_median)
plot_data_homophily[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_homophily[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_homophily[1:5000,"connected"]<-1

prediction_homophily_1<-predict(res4,plot_data_homophily)
prediction_homophily_2<-predict(res3,plot_data_homophily)

ggplot(plot_data_homophily,aes(x=plot_data_homophily[1:5000,"homophily"],y=prediction_homophily_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("homophily")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(-0.25,1)+ geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)
ggplot(plot_data_homophily,aes(x=plot_data_homophily[1:5000,"homophily"],y=prediction_homophily_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("homophily")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(-0.25,1)+ geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)
}#homophily


{
plot_data_density<-data[1:5000,]
plot_data_density[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_density[1:2500,"mults"]<-runif(2500,0,1)
plot_data_density[2501:5000,"mults"]<-runif(2500,1,5)
plot_data_density[1:5000,"mults_gr_1"]<-0
plot_data_density[plot_data_density$mults>=1,"mults_gr_1"]<-1
plot_data_density[1:5000,"n"]<-100
plot_data_density[1:5000,"char_ratio"]<-mean(data$char_ratio)
plot_data_density[1:5000,"sex_ratio"]<-mean(data$sex_ratio)
plot_data_density[1:5000,"s_p_max"]<-median(data$s_p_max)
plot_data_density[1:5000,"s_p_median"]<-median(data$s_p_median)
plot_data_density[1:5000,"g_density"]<-runif(5000,0,1)
plot_data_density[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_density[1:5000,"connected"]<-1

prediction_density_1<-predict(res4,plot_data_density)
prediction_density_2<-predict(res3,plot_data_density)

ggplot(plot_data_density,aes(x=plot_data_density[1:5000,"g_density"],y=prediction_density_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("density")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0,1.2)+ geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)
ggplot(plot_data_density,aes(x=plot_data_density[1:5000,"g_density"],y=prediction_density_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("density")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0,1.2)+ geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)
}#density


{
plot_data_clustering<-data[1:5000,]
plot_data_clustering[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_clustering[1:2500,"mults"]<-runif(2500,0,1)
plot_data_clustering[2501:5000,"mults"]<-runif(2500,1,5)
plot_data_clustering[1:5000,"mults_gr_1"]<-0
plot_data_clustering[plot_data_clustering$mults>=1,"mults_gr_1"]<-1
plot_data_clustering[1:5000,"n"]<-100
plot_data_clustering[1:5000,"char_ratio"]<-mean(data$char_ratio)
plot_data_clustering[1:5000,"sex_ratio"]<-mean(data$sex_ratio)
plot_data_clustering[1:5000,"s_p_max"]<-median(data$s_p_max)
plot_data_clustering[1:5000,"s_p_median"]<-median(data$s_p_median)
plot_data_clustering[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_clustering[1:5000,"clustering"]<-runif(5000,0,1)
plot_data_clustering[1:5000,"connected"]<-1

prediction_clustering_1<-predict(res4,plot_data_clustering)
prediction_clustering_2<-predict(res3,plot_data_clustering)

ggplot(plot_data_clustering,aes(x=plot_data_clustering[1:5000,"clustering"],y=prediction_clustering_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("clustering")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0,1.2)+ geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)
ggplot(plot_data_clustering,aes(x=plot_data_clustering[1:5000,"clustering"],y=prediction_clustering_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("clustering")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0,1.2)+ geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)
}#clustering


{
plot_data_s_p_max<-data[1:5000,]
plot_data_s_p_max[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_s_p_max[1:2500,"mults"]<-runif(2500,0,1)
plot_data_s_p_max[2501:5000,"mults"]<-runif(2500,1,5)
plot_data_s_p_max[1:5000,"mults_gr_1"]<-0
plot_data_s_p_max[plot_data_s_p_max$mults>=1,"mults_gr_1"]<-1
plot_data_s_p_max[1:5000,"n"]<-100
plot_data_s_p_max[1:5000,"char_ratio"]<-mean(data$char_ratio)
plot_data_s_p_max[1:5000,"sex_ratio"]<-mean(data$sex_ratio)
plot_data_s_p_max[1:5000,"s_p_max"]<-runif(5000,1,10)
plot_data_s_p_max[1:5000,"s_p_median"]<-median(data$s_p_median)
plot_data_s_p_max[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_s_p_max[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_s_p_max[1:5000,"connected"]<-1

prediction_s_p_max_1<-predict(res4,plot_data_s_p_max)
prediction_s_p_max_2<-predict(res3,plot_data_s_p_max)

ggplot(plot_data_s_p_max,aes(x=plot_data_s_p_max[1:5000,"s_p_max"],y=prediction_s_p_max_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("diameter")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0.7,0.8)
ggplot(plot_data_s_p_max,aes(x=plot_data_s_p_max[1:5000,"s_p_max"],y=prediction_s_p_max_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("diameter")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0.7,0.8)
}#s_p_max


{
plot_data_s_p_median<-data[1:5000,]
plot_data_s_p_median[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_s_p_median[1:2500,"mults"]<-runif(2500,0,1)
plot_data_s_p_median[2501:5000,"mults"]<-runif(2500,1,5)
plot_data_s_p_median[1:5000,"mults_gr_1"]<-0
plot_data_s_p_median[plot_data_s_p_median$mults>=1,"mults_gr_1"]<-1
plot_data_s_p_median[1:5000,"n"]<-100
plot_data_s_p_median[1:5000,"char_ratio"]<-mean(data$char_ratio)
plot_data_s_p_median[1:5000,"sex_ratio"]<-mean(data$sex_ratio)
plot_data_s_p_median[1:5000,"s_p_max"]<-median(data$s_p_max)
plot_data_s_p_median[1:5000,"s_p_median"]<-runif(5000,1,10)
plot_data_s_p_median[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_s_p_median[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_s_p_median[1:5000,"connected"]<-1

prediction_s_p_median_1<-predict(res4,plot_data_s_p_median)
prediction_s_p_median_2<-predict(res3,plot_data_s_p_median)

ggplot(plot_data_s_p_median,aes(x=plot_data_s_p_median[1:5000,"s_p_median"],y=prediction_s_p_median_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("median shortest path")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0.7,0.9)
ggplot(plot_data_s_p_median,aes(x=plot_data_s_p_median[1:5000,"s_p_median"],y=prediction_s_p_median_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("median shortest path")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0.7,0.9)
}#s_p_median


{
plot_data_char_ratio<-data[1:5000,]
plot_data_char_ratio[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_char_ratio[1:5000,"mults"]<-runif(5000,0,5)
plot_data_char_ratio[1:5000,"mults_gr_1"]<-0
plot_data_char_ratio[plot_data_char_ratio$mults>=1,"mults_gr_1"]<-1
plot_data_char_ratio[1:5000,"n"]<-100
plot_data_char_ratio[1:5000,"char_ratio"]<-runif(5000,0,1)
plot_data_char_ratio[1:5000,"sex_ratio"]<-0.5
plot_data_char_ratio[1:5000,"s_p_max"]<-4
plot_data_char_ratio[1:5000,"s_p_median"]<-2
plot_data_char_ratio[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_char_ratio[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_char_ratio[1:5000,"connected"]<-1

prediction_char_ratio_1<-predict(res4,plot_data_char_ratio)
prediction_char_ratio_2<-predict(res3,plot_data_char_ratio)

ggplot(plot_data_char_ratio,aes(x=plot_data_char_ratio[1:5000,"char_ratio"],y=prediction_char_ratio_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("char_ratio")+ylab("homogamy")+ labs(color='(d>1)')
ggplot(plot_data_char_ratio,aes(x=plot_data_char_ratio[1:5000,"char_ratio"],y=prediction_char_ratio_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("char_ratio")+ylab("homogamy")+ labs(color='(d>1)')
}#char ratio


{
plot_data_sex_ratio<-data[1:5000,]
plot_data_sex_ratio[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_sex_ratio[1:5000,"mults"]<-runif(5000,0,5)
plot_data_sex_ratio[1:5000,"mults_gr_1"]<-0
plot_data_sex_ratio[plot_data_mults$sex_ratio>=1,"mults_gr_1"]<-1
plot_data_sex_ratio[1:5000,"n"]<-100
plot_data_sex_ratio[1:5000,"char_ratio"]<-0.5
plot_data_sex_ratio[1:5000,"sex_ratio"]<-runif(5000,0,1)
plot_data_sex_ratio[1:5000,"s_p_max"]<-4
plot_data_sex_ratio[1:5000,"s_p_median"]<-2
plot_data_sex_ratio[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_sex_ratio[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_sex_ratio[1:5000,"connected"]<-rbinom(5000,1,0.5)

prediction_sex_ratio_1<-predict(res4,plot_data_sex_ratio)
prediction_sex_ratio_2<-predict(res3,plot_data_sex_ratio)

ggplot(plot_data_sex_ratio,aes(x=plot_data_sex_ratio[1:5000,"sex_ratio"],y=prediction_sex_ratio_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("sex_ratio")+ylab("homogamy")+ labs(color='(d>1)')
ggplot(plot_data_sex_ratio,aes(x=plot_data_sex_ratio[1:5000,"sex_ratio"],y=prediction_sex_ratio_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("sex_ratio")+ylab("homogamy")+ labs(color='(d>1)')
}#sex ratio


{
plot_data_n<-data[1:5000,]
plot_data_n[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_n[1:5000,"mults"]<-runif(5000,0,5)
plot_data_n[1:5000,"mults_gr_1"]<-0
plot_data_n[plot_data_n$mults>=1,"mults_gr_1"]<-1
plot_data_n[1:5000,"n"]<-runif(5000,10,200)
plot_data_n[1:5000,"char_ratio"]<-0.5
plot_data_n[1:5000,"sex_ratio"]<-0.5
plot_data_n[1:5000,"s_p_max"]<-4
plot_data_n[1:5000,"s_p_median"]<-2
plot_data_n[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_n[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_n[1:5000,"connected"]<-rbinom(5000,1,0.5)

prediction_n_1<-predict(res4,plot_data_n)
prediction_n_2<-predict(res3,plot_data_n)

ggplot(plot_data_n,aes(x=plot_data_n[1:5000,"n"],y=prediction_n_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("n")+ylab("homogamy")+ labs(color='(d>1)')
ggplot(plot_data_n,aes(x=plot_data_n[1:5000,"n"],y=prediction_n_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("n")+ylab("homogamy")+ labs(color='(d>1)')
}#n


{
  plot_data_internet<-data[1:50000,]
  plot_data_internet[1:50000,"homophily"]<-runif(50000,0,1)
  plot_data_internet[1:50000,"mults"]<-runif(50000,0,5)
  plot_data_internet[1:50000,"mults_gr_1"]<-0
  plot_data_internet[plot_data_internet$mults>=1,"mults_gr_1"]<-1
  plot_data_internet[1:50000,"n"]<-100
  plot_data_internet[1:50000,"char_ratio"]<-0.5
  plot_data_internet[1:50000,"sex_ratio"]<-0.5
  plot_data_internet[1:50000,"s_p_max"]<-4
  plot_data_internet[1:50000,"s_p_median"]<-2
  plot_data_internet[1:50000,"g_density"]<-runif(50000,0,1)
  plot_data_internet[1:50000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
  plot_data_internet[1:50000,"connected"]<-1
  
  prediction_internet_1<-predict(res4,plot_data_internet)
  prediction_internet_2<-predict(res3,plot_data_internet)
  
  plot_data_internet_1<-cbind(prediction_internet_1,plot_data_internet)
  plot_data_internet_2<-cbind(prediction_internet_2,plot_data_internet)
  
  colnames(plot_data_internet_1)<-c("homogamy","homogamy2","connected","d_gr_1" ,"mults","n","char_ratio", "sex_ratio",  "homophily",  "s_p_max","s_p_median" ,"density", "clustering")
  
  ggplot(plot_data_internet[which(plot_data_internet$mults_gr_1==1),],aes(x=plot_data_internet[which(plot_data_internet$mults_gr_1==1),"homophily"],y=prediction_internet_1[which(plot_data_internet$mults_gr_1==1)],colour=plot_data_internet[which(plot_data_internet$mults_gr_1==1),"g_density"]))+geom_point()+xlab("homophily")+ylab("homogamy")+ labs(color='density')+ylim(-1.5,1.5)+ theme(legend.position="bottom")
  ggplot(plot_data_internet[which(plot_data_internet$mults_gr_1==0),],aes(x=plot_data_internet[which(plot_data_internet$mults_gr_1==0),"homophily"],y=prediction_internet_1[which(plot_data_internet$mults_gr_1==0)],colour=plot_data_internet[which(plot_data_internet$mults_gr_1==0),"g_density"]))+geom_point()+xlab("homophily")+ylab("homogamy")+ labs(color='density')+ylim(-1.5,1.5)+ theme(legend.position="bottom")
  
  ggplot(plot_data_internet[which(plot_data_internet$mults_gr_1==1),],aes(x=plot_data_internet[which(plot_data_internet$mults_gr_1==1),"homophily"],y=prediction_internet_2[which(plot_data_internet$mults_gr_1==1)],colour=plot_data_internet[which(plot_data_internet$mults_gr_1==1),"g_density"]))+geom_point()+xlab("homophily")+ylab("homogamy")+ labs(color='density')+ylim(-1.5,1.5)+ theme(legend.position="bottom")
  ggplot(plot_data_internet[which(plot_data_internet$mults_gr_1==0),],aes(x=plot_data_internet[which(plot_data_internet$mults_gr_1==0),"homophily"],y=prediction_internet_2[which(plot_data_internet$mults_gr_1==0)],colour=plot_data_internet[which(plot_data_internet$mults_gr_1==0),"g_density"]))+geom_point()+xlab("homophily")+ylab("homogamy")+ labs(color='density')+ylim(-1.5,1.5)+ theme(legend.position="bottom")
} #internet



#extensions

setwd("C:/Users/User/Documents/R/Dissertation/final_data")

data<-readRDS("data_homophily_sex.RDS")
data$mults_gr_1<-0
data[which(data$mults>1),"mults_gr_1"]<-1
data$connected<-0
data[which(data$s_p_max!=Inf),"connected"]<-1

varlist<-c("homogamy","connected","mults_gr_1","mults","n","char_ratio", "sex_ratio","homophily","s_p_max","s_p_median","g_density","clustering","homophily_sex")
data<-data[,varlist]
data[data==Inf]<-9*10^10

setwd("C:/Users/User/Documents/R/Dissertation/final_regressions")
res4<-readRDS("homophily_sex_step.RDS")
res3<-readRDS("homophily_sex_gen.RDS")

data$mults_gr_1<-as.factor(data$mults_gr_1)
data$connected<-as.factor(data$connected)
data<-data[complete.cases(data),]

{
plot_data_homophily_sex<-data[1:5000,]
plot_data_homophily_sex[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_homophily_sex[1:2500,"mults"]<-runif(2500,0,1)
plot_data_homophily_sex[2501:5000,"mults"]<-runif(2500,1,5)
plot_data_homophily_sex[1:5000,"mults_gr_1"]<-0
plot_data_homophily_sex[plot_data_homophily_sex$mults>=1,"mults_gr_1"]<-1
plot_data_homophily_sex[1:5000,"n"]<-100
plot_data_homophily_sex[1:5000,"char_ratio"]<-mean(data$char_ratio)
plot_data_homophily_sex[1:5000,"sex_ratio"]<-mean(data$sex_ratio)
plot_data_homophily_sex[1:5000,"s_p_max"]<-median(data$s_p_max)
plot_data_homophily_sex[1:5000,"s_p_median"]<-median(data$s_p_median)
plot_data_homophily_sex[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_homophily_sex[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_homophily_sex[1:5000,"connected"]<-1
plot_data_homophily_sex[1:5000,"homophily_sex"]<-runif(5000,0,1)

prediction_homophily_sex_1<-predict(res4,plot_data_homophily_sex)
prediction_homophily_sex_2<-predict(res3,plot_data_homophily_sex)

ggplot(plot_data_homophily_sex,aes(x=plot_data_homophily_sex[1:5000,"homophily_sex"],y=prediction_homophily_sex_1[1:5000],colour=mults_gr_1))+geom_point()+xlab("homophily_sex")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0,1)+ geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)
ggplot(plot_data_homophily_sex,aes(x=plot_data_homophily_sex[1:5000,"homophily_sex"],y=prediction_homophily_sex_2[1:5000],colour=mults_gr_1))+geom_point()+xlab("homophily_sex")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0,1)+ geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)
} #homophily_sex




setwd("C:/Users/User/Documents/R/Dissertation/final_data")

data<-readRDS("data_decay.RDS")
data$mults_gr_1<-0
data[which(data$mults>1),"mults_gr_1"]<-1
data$connected<-0
data[which(data$s_p_max!=Inf),"connected"]<-1

varlist<-c("homogamy","connected","mults_gr_1","mults","n","char_ratio", "sex_ratio","homophily","s_p_max","s_p_median","g_density","clustering","decay")
data<-data[,varlist]
data[data==Inf]<-9*10^10

setwd("C:/Users/User/Documents/R/Dissertation/final_regressions")
res4<-readRDS("decay_step.RDS")
res3<-readRDS("decay_gen.RDS")

data$mults_gr_1<-as.factor(data$mults_gr_1)
data$connected<-as.factor(data$connected)
data<-data[complete.cases(data),]

{
plot_data_decay<-data[1:5000,]
plot_data_decay[1:5000,"homophily"]<-mean(data[!is.na(data$homophily),"homophily"])
plot_data_decay[1:2500,"mults"]<-runif(2500,0,1)
plot_data_decay[2501:5000,"mults"]<-runif(2500,1,5)
plot_data_decay[1:5000,"mults_gr_1"]<-0
plot_data_decay[plot_data_decay$mults>=1,"mults_gr_1"]<-1
plot_data_decay[1:5000,"n"]<-100
plot_data_decay[1:5000,"char_ratio"]<-mean(data$char_ratio)
plot_data_decay[1:5000,"sex_ratio"]<-mean(data$sex_ratio)
plot_data_decay[1:5000,"s_p_max"]<-median(data$s_p_max)
plot_data_decay[1:5000,"s_p_median"]<-median(data$s_p_median)
plot_data_decay[1:5000,"g_density"]<-mean(data[!is.na(data$g_density),"g_density"])
plot_data_decay[1:5000,"clustering"]<-mean(data[!is.na(data$clustering),"clustering"])
plot_data_decay[1:5000,"connected"]<-1
plot_data_decay[1:5000,"decay"]<-runif(5000,0,2)

plot_data_decay[,"mults_cat"]<-4
plot_data_decay[plot_data_decay$mults<3,"mults_cat"]<-3
plot_data_decay[plot_data_decay$mults<2,"mults_cat"]<-2
plot_data_decay[plot_data_decay$mults<1,"mults_cat"]<-1

plot_data_decay$mults_cat<-as.factor(plot_data_decay$mults_cat)

prediction_decay_1<-predict(res4,plot_data_decay)
prediction_decay_2<-predict(res3,plot_data_decay)

ggplot(plot_data_decay,aes(x=plot_data_decay[1:5000,"decay"],y=prediction_decay_1[1:5000],colour=mults_cat))+geom_point()+xlab("decay")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0.7,1)
ggplot(plot_data_decay,aes(x=plot_data_decay[1:5000,"decay"],y=prediction_decay_2[1:5000],colour=mults_cat))+geom_point()+xlab("decay")+ylab("homogamy")+ labs(color='(d>1)')+ theme(legend.position="bottom")+ylim(0.7,1)
} #decay


