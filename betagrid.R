#Betagrid function from http://rfunctions.blogspot.com/2015/08/calculating-beta-diversity-on-grid.html

betagrid<-function(gridshp, comp, xfeature, yfeature, radius, phylotree, phylobeta=F, index="sorensen"){
  data<-data.frame(gridshp[xfeature],gridshp[yfeature],comp)
  mean_turnover<-numeric(length(comp[,1]))
  mean_nestedness<-numeric(length(comp[,1]))
  mean_beta<-numeric(length(comp[,1]))
  for(i in 1:length(shape[[2]])){
    adj<-select.window(xf=data[i,1], yf=data[i,2], radius, xydata=data)[,-c(1,2)]
    if(phylobeta==F){
      res<-beta.pair(adj, index.family=index)
    }else if(phylobeta==T){
      res<-phylo.beta.pair(adj, phylotree, index.family=index)
    }
    mean_turnover[i]<-mean(as.matrix(res[[1]])[2:length(as.matrix(res[[1]])[,1]),1],na.rm=TRUE)
    mean_nestedness[i]<-mean(as.matrix(res[[2]])[2:length(as.matrix(res[[2]])[,1]),1],na.rm=TRUE)
    mean_beta[i]<-mean(as.matrix(res[[3]])[2:length(as.matrix(res[[3]])[,1]),1],na.rm=TRUE)
  }
  return(data.frame(cell=row.names(comp), mean_turnover, mean_nestedness, mean_beta))
}