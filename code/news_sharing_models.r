
rm(list=ls(all=TRUE))
#We set the working Directory

#path_tiago <- "C:/Users/Tiago Ventura/Dropbox/CalvoTiago/mlogitRedes"

#setwd(path_tiago)

list.files()

#load("Maldonado with equilibrium Results.RData")

#Clear all objects from memory
#rm(list=ls(all=TRUE))

#Call the library for Multi-Level modeling in R
library(arm)
library(igraph)

plot(V(net)$l1,V(net)$l2,col=V(net)$membership,cex=.3, pch=16) 

#Select a smaller sample for trial run
#my.sub<- 1:1000
my.sub<-sample(1:length(resultsNews1[,1]), 900, replace = FALSE, prob = NULL)

#Selection of a smaller sample of media links for trial run
#my.media<- c(1,3:17)
#my.media<- c(1:24)
  
  my.media<- c(1:24)
  
  S <- unname(resultsNews1[my.sub,])
  S <- S[,my.media]

  #Eliminate observations with no selected link
  my.sub<- my.sub[which(rowSums(S)!=0)]
  S <- unname(resultsNews1[my.sub,])

  Slag <- unname(resultsNews2[my.sub,])
  Slag <- Slag[,my.media]

  #Slag <- Slag[which.rows,]
  N = length(S[,1])
  K = length(S[1,])
  # l1<- V(net)$l1

  #Bolsonaro
  #l1<- V(net)$l2

  #Maldonado and travel ban
  l1<- V(net)$l1
  l1 = l1[my.sub]
  
  user.names <-V(net)$name
  user.names <- user.names[my.sub]
  
  position1 <- rep(0,24)
  for(i in 1:24){
    position1[i]<-sum(l1*(S[,i]/sum(S[,i])))
  }

position1lag <- rep(0,24)
  for(i in 1:24){
    position1lag[i]<-sum(l1*(resultsNews2[my.sub,i]/sum(resultsNews2[my.sub,i])))
  }

  
  D1<-position1[1:K]
  market<- colSums(S[,1:K])/sum(colSums(S[,1:K]))

  D1lag<-position1lag[1:K]
  marketLag<- colSums(resultsNews2[my.sub,1:K])/sum(colSums(resultsNews2[my.sub,1:K]))
  
  quant.1<- cut(l1, quantile(l1, probs = 0:10/10), labels = FALSE, include.lowest = TRUE)
  n <- rowSums(S)
  colSums(S)
 
 png("ideology by quantiles.png", w=1000, h=600)
 op <- par(mfcol=c(1,2))  
 plot(l1,jitter(quant.1), xlim=c(-100,100), pch=16, cex=.6, xlab="Ideology", ylab="Quantile(Ideology)")
 grid(5, 5, lwd = .5)
 hist(new.data[,2], 60, probability=TRUE, xlim=c(-100,100), ylab="Probability", xlab="Ideology", main="")
 par(op)
 dev.off()
   
  #POISSON MODEL
  ideoDist<- S
  ideoDistLag<- Slag
  for(i in 1:K){ideoDist[,i]<- (D1[i]-l1)^2}
  for(i in 1:K){ideoDistLag[,i]<- (D1lag[i]-l1)^2}
  new.data<- cbind(1:length(l1),l1,quant.1,ideoDist,S,n,Slag,ideoDistLag)
  colnames(new.data)<- c("id","l1","quant",paste("ideoDist",1:K,sep=""),paste("S",1:K,sep=""),"sumn",paste("Slag",1:K,sep=""),paste("ideoDistLag",1:K,sep=""))
  library(reshape2)
  library(tidyr)
  library(splitstackshape)
  head(new.data)
  new.long <- merged.stack(as.data.frame(new.data), id.vars = c("id","l1","quant","sumn"),var.stubs = c("ideoDist","S", "Slag","ideoDistLag"), sep = "var.stubs")
  new.long$groupR1<- paste("Q",new.long$quant,"M",new.long$.time_1,sep="")
  new.long$.time_1<- as.integer(new.long$.time_1)
  new.long <- new.long[order(id, .time_1),]
  
# time_1 is the outlet. Just how the merged.stack works. 
  
  
pois.lmer <- glmer(S ~ log(ideoDist) +  
                     (1+log(ideoDist)|quant) + 
                     (1|.time_1) + (1|.time_1:quant) + 
                     (1|id), nAGQ=0, family=poisson,data=new.long)
  
  #pois.lmer <- glmer(S ~ log(ideoDist) + (1|.time_1)*(1+log(ideoDist)|quant), nAGQ=0, family=poisson,data=new.long)
  
  summary(pois.lmer)
  
  coef(pois.lmer)

