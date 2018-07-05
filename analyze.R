library(dnar)

csvs<-list.files('.','.csv$')
tabs<-lapply(csvs,read.csv,stringsAsFactors=FALSE,skip=1)
names(tabs)<-sub(',.*','',sapply(csvs,readLines,n=1))
diffs<-lapply(tabs,function(xx){
  out<-data.frame(
    'diff'=xx$Mean-xx$Mean.1,
    'se'=sqrt(xx$SD^2/xx$Total+xx$SD.1^2/xx$Total.1),
    'df'=(xx$SD^2/xx$Total+xx$SD.1^2/xx$Total.1)^2/(xx$SD^4/xx$Total^2/(xx$Total-1)+xx$SD.1^4/xx$Total.1^2/(xx$Total.1-1))
  )
  out$weight<-out$se^2/sum(out$se^2)
  out$t<--qt(.025,out$df)
  out$lower<-out$diff-out$t*out$se
  out$upper<-out$diff+out$t*out$se
  out
})

plotForest<-function(tab,diff,xlab=''){
  displayCols<-c('Study','Mean','SD','Total','Mean.1','SD.1','Total.1','Difference','Weight')
  esc<-escalc(measure="MD", n1i=Total, n2i=Total.1, m1i=Mean, m2i=Mean.1, sd1i=SD, sd2i=SD.1, data=tab)
  rm<-metafor::rma(yi,vi,data=esc)
  tab[,grepl('Mean|SD',colnames(tab))]<-apply(tab[,grepl('Mean|SD',colnames(tab))],2,function(xx)sprintf('%0.2f',xx))
  #tab$Weight<-sprintf('%0.1f%%',diff$weight*100)
  tab$Weight<-sprintf('%0.1f%%',weights(rm))
  tab$Difference<-sprintf('%0.2f (%0.2f - %0.2f)',diff$diff,diff$lower,diff$upper)
  rePos<--.25
  start<-31
  par(mar=c(4,start,2.5,.5))
  plot(1,1,type='n',xlab=xlab,ylab='',yaxt='n',ylim=c(rePos-.2,nrow(tab)),xlim=range(pretty(c(diff$lower,diff$upper))),bty='n',mgp=c(2,.7,0),tcl=-.2)
  pos=start
  poss<-c()
  for(ii in displayCols){
    mtext(sub('\\.1','',ii),3,.5,at=convertLineToUser(pos,2),adj=0,cex=.9,font=2)
    axis(2,1:nrow(tab),tab[,ii],las=1,lty=0,mgp=c(0,pos,0),hadj=0,cex.axis=.7)
    poss[ii]<-pos
    pos<-pos-1+convertUserToLine(par('usr')[1]+max(c(strwidth(sub('\\.1','',ii)),strwidth(tab[,ii],cex=.7))),2)
  }
  axis(2,rePos,'Random effects model',las=1,lty=0,mgp=c(0,start,0),hadj=0,cex.axis=.9)
  axis(2,rePos,sprintf('%0.2f (%0.2f - %0.2f)',rm$b,rm$ci.lb,rm$ci.ub),las=1,lty=0,mgp=c(0,poss['Difference'],0),hadj=0,cex.axis=.7)
  points(diff$diff,1:nrow(diff),pch=22,bg='black',cex=sqrt(weights(rm))/3)
  segments(diff$lower,1:nrow(diff),diff$upper,1:nrow(diff))
  abline(h=par('usr')[4],xpd=NA)
  abline(v=0,lty=2)
  wmd<-sum(diff$diff*diff$weight)
  polygon(c(rm$ci.lb,rm$b,rm$ci.ub,rm$b),c(rePos,rePos-.2,rePos,rePos+.2))
  return(rm)
}
pdf('forests.pdf',width=8,height=5)
rmas<-mapply(plotForest,tabs,diffs,sprintf('Change in %s',names(tabs)),SIMPLIFY=FALSE)
dev.off()

print(rmas)


