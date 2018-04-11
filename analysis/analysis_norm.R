require(glmulti)
require(psych)
lm.forced_inclusion = function(formula, data, keep="", ...) {lm(as.formula(paste(deparse(formula), keep)), data=data, ...)} 


setwd("C:/Users/User/Documents/R/Dissertation/final_data")

data<-readRDS("data_norm.RDS")


data$mults_gr_1<-0
data[which(data$mults>1),"mults_gr_1"]<-1
data$mults_gr_1<-as.factor(data$mults_gr_1)

data$connected<-0
data[which(data$s_p_max!=Inf),"connected"]<-1
data$connected<-as.factor(data$connected)

data[data==Inf]<-9*10^10

polys<-4
varlist<-c("homogamy","mults_gr_1","mults","n","char_ratio", "sex_ratio","homophily","s_p_max","s_p_median","g_density","clustering","stand_dev")
vars_power<-paste0("I(",varlist[c(-1,-2)],"^",mapply(rep,c(2:polys),MoreArgs = list(times=length(varlist[c(-1,-2)]))),")")
interactions<-c(paste(expand.grid(varlist[-1],varlist[-1])[,1],expand.grid(varlist[-1],varlist[-1])[,2],sep = ":"))

data<-data[,varlist]

res1<-glmulti(y="homogamy",xr=colnames(data)[-1],data=data,level = 2,marginality = T,method = "g",confsetsize = 10,plotty = F,conseq = 10)
best_model1<-as.formula(summary(res1)$bestmodel)
rm(res1)
gc()

res2<-glmulti(y="homogamy",xr=vars_power,data=data,confsetsize = 10,conseq = 10, fitfunc=lm.forced_inclusion,crit = aic, level=1, method="g", keep= paste0("+",paste(attr(terms(best_model1),"term.labels"),collapse = "+")),plotty = F) 
best_model2<-update.formula(summary(res2)$bestmodel,paste0("~.+",paste(attr(terms(best_model1),"term.labels"),collapse = "+")))
res3<-lm(best_model2,data)
rm(res2)
gc()

max_model<-as.formula(paste0("homogamy~",paste(c(varlist[-1],vars_power,interactions),collapse = "+")))
res4<-step(lm(max_model,data))

