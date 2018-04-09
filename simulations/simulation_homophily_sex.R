require(doParallel) 
require(parallel)
require(sna)
require(igraph)
require(psych)
setwd("C:/Users/User/Documents/R/Dissertation")

#this data simulatiion is for regression data only, as it is too messy to plot

cl <- makeCluster(4, type='PSOCK')
registerDoParallel(cl)
{
  runs<-10000 # for test runs chose samll number of runs e.g. 10
  data<-data.frame(id=c(1:runs)) #creates data frame with idn
  data$mults<-runif(runs,0,5) 
  data$divs<-runif(runs,1,10)
  data$divs2<-runif(runs,1,10)
  data$Alpha<-runif(runs,0,1)
  data$Beta<-data$Alpha/data$divs
  data$A<-runif(runs,0,1)
  data$B<-data$A/data$divs2
  data$n<-sample(c(10:200),runs,replace = T)
  data$char_ratio<-runif(runs,0,1) #ratio of char=1
  data$sex_ratio<-runif(runs,0,1) # ratio of sex=1
}#variable set up
simulation<-function(run){
  require(sna)
  require(igraph)
  require(psych)
  sex<-rbinom(data[run,"n"],1,data[run,"sex_ratio"]) #vector of players' sex
  char<-rbinom(data[run,"n"],1,data[run,"char_ratio"])  #vector of players' chars
  search_times<-char*data[run,"mults"] #vector with time players' begin searching
  g<-outer(1:data[run,"n"],1:data[run,"n"],Vectorize(function(i,j,run=run){
    if(char[i]==char[j]&sex[i]==sex[j]){return(rbinom(1,1,(data[run,"Alpha"]*data[run,"A"])))}
    else if (char[i]==char[j]&sex[i]!=sex[j]){return(rbinom(1,1,(data[run,"Alpha"]*data[run,"B"])))}
    else if (char[i]!=char[j]&sex[i]==sex[j]){return(rbinom(1,1,(data[run,"Beta"]*data[run,"A"])))}
    else {return(rbinom(1,1,(data[run,"Beta"]*data[run,"B"])))}
  }))#creates fridnship network
  
  g<-symmetrize(g,"upper")
  g1<-graph_from_adjacency_matrix(g)
  geos<-geodist(g)$gdist
  
  Phi<-outer(1:data[run,"n"],1:data[run,"n"],Vectorize(function(i,j,run=run){
    if(sex[i]==sex[j]){return(Inf)}
    else{return(max(search_times[i],search_times[j]+geos[i,j]))}
  }))


  matches<-data.frame(i=numeric(0),j=numeric(0),char_i=numeric(0),char_j=numeric(0),sex_i=numeric(0),sex_j=numeric(0),same_char=numeric(0),same_sex=numeric(0))
  
  counter<-1
  while(min(Phi)!=Inf){
    
    mins<-which(Phi==min(Phi),arr.ind = T)
    
    who_prop_to<-lapply(1:data[run,"n"],function(i){
      if(length(mins[which(mins[,1]==i),2])==1) {return(mins[which(mins[,1]==i),2])}
      else if(length(mins[which(mins[,1]==i),2])>1) {return(sample(mins[which(mins[,1]==i),2],1))}
   })
    
    which_prop_acccept<-lapply(1:data[run,"n"],function(i){
      if(length(which(who_prop_to %in% i))==1){return(which(who_prop_to %in% i))}
      else if(length(which(who_prop_to %in% i))>1){return(sample(which(who_prop_to %in% i),1))}
    })
    
    chose_from<-which(unlist(lapply(which_prop_acccept,function(x){!is.null(x)}))==T)
    if(length(chose_from)==1){chose_1<-chose_from}
    else if(length(chose_from)>1){chose_1<-sample(chose_from,1)}
    matches[counter,"i"]<-chose_1
    if(length(which_prop_acccept[[chose_1]])==1){matches[counter,2]<-which_prop_acccept[[chose_1]]}
    else if (length(which_prop_acccept[[chose_1]])>1){matches[counter,2]<-sample(which_prop_acccept[[chose_1]],1)}

    Phi[c(matches[counter,1],matches[counter,2]),]<-Inf
    Phi[,c(matches[counter,1],matches[counter,2])]<-Inf    

    counter<-counter+1
  }
  if(dim(matches)[1]!=0){
  matches[,c("char_i","char_j","sex_i","sex_j")]<-t(matrix(unlist(lapply(c(1:dim(matches)[1]),function(i){
    return(c(char[matches[i,"i"]],char[matches[i,"j"]],sex[matches[i,"i"]],sex[matches[i,"j"]]))
  })),nrow = 4))
  matches[,c("same_char","same_sex")]<-t(matrix(unlist(lapply(c(1:dim(matches)[1]),function(i){
    if (matches[i,"char_i"]==matches[i,"char_j"]){temp1<-1}
    else if (matches[i,"char_i"]!=matches[i,"char_j"]){temp1<-0}
    if (matches[i,"sex_i"]==matches[i,"sex_j"]){temp2<-1}
    else if (matches[i,"sex_i"]!=matches[i,"sex_j"]){temp2<-0}
    return(c(temp1,temp2))
  })),nrow = 2))
  


  g[which(g==Inf)]<-NA
  comps<-count_components(g1)
  no_of_ties<-sum(g)/2
  no_of_ties_char_1<-sum(g[which(char==1),which(char==1)])/2
  no_of_ties_char_0<-sum(g[which(char==0),which(char==0)])/2
  homophily<-(no_of_ties_char_1+no_of_ties_char_0)/no_of_ties
  
  no_of_ties_sex_1<-sum(g[which(sex==1),which(sex==1)])/2
  no_of_ties_sex_0<-sum(g[which(sex==0),which(sex==0)])/2
  homophily_sex<-(no_of_ties_sex_1+no_of_ties_sex_0)/no_of_ties
  
  homogamy<-sum(matches$same_char==1)/sum(!is.na(matches$same_char))
  s_p_max<-describe(geos[upper.tri(geos)])$max
  s_p_median<-describe(geos[upper.tri(geos)])$median
  g_density<-graph.density(g1)
  clustering<-transitivity(g1)
  
  return(list(homophily,homophily_sex,homogamy,s_p_max,s_p_median,g_density, clustering))
  }
  else {return(rep(NA,20))}
  rm(Phi)
}

sim_out<-foreach(run=1:runs,.combine = rbind)%dopar% simulation(run)
name_list<-c("homophily","homophily_sex","homogamy","s_p_max","s_p_median",
             "g_density", "clustering")
colnames(sim_out)<-name_list
registerDoSEQ()

for(i in 1:runs){
  for(j in name_list){
    data[i,j]<-sim_out[i,j][[1]]
  }}

