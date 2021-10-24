# News by popular demand
# update: 12/10/2018

# In this code, I am generating the figures for the paper using the three cases: Bolsonaro, Travel Ban, and Maldonado


path <- "/home/venturat/Dropbox/CalvoTiago/mlogitRedes"
output <- "./output/Bolsonaro/"
 

#path_gradlab <- "C:/Users/venturat/Downloads"

setwd(path)

# Libraries

library(tidyverse)
library(igraph)
library(data.table)
library(arm)
library(broom)
library(RColorBrewer)
library(KernSmooth)

rm(list=ls(all=TRUE))

# Load

#load("Maldonado with equilibrium Results.RData")

# Functions ---------------------------------------------------------------

##PLOT FUNCTION
my.den.plot <- function(l=l,new.color=new.color, ind=ind, legend, title){
  #Numero efectivo de Comunidades
  ENCG<-round(1/sum(round(table(new.color)/sum(table(new.color)),3)^2),2)
  
  est <- bkde2D(l, bandwidth=c(10, 10))
  #temp<-as.numeric(factor(new.color))+15
  plot(l,cex=log(ind+1)/4, main=title, col=new.color, pch=16, xlim=c(-160,160),ylim=c(-160,140), xlab="", ylab="", axes=FALSE)
  contour(est$x1, est$x2, est$fhat,  col = gray(.2), add=TRUE)
  legend("topright", c(legend[1],legend[2]), pch = 17:18, col=c("#B2182B", "#2166AC"))
  #text(-15000,25000,"Gobierno", cex=1.2, srt=0)
  text(-140,115,paste("ENCG: ",ENCG,sep=""), cex=1, srt=0)
  #text(jitter(l.rotated[,1],100),jitter(l.rotated[,2],100),my.label2, pos=3, cex=.6, col=gray(.1))
  #segments(-5000,-40000,-5000,40000, col=2,lty=2)
  #segments(-40000,-5000,40000,-5000, col=2,lty=2)
} 


# FOF

fomfE<- function(var=var, adjV=adjV,adjE=adjE){
  stemp <- sapply(adjE, function(x) sum(var[x]))
  mstemp <- sapply(adjV, function(x) mean(stemp[x]))
  plot(log(stemp),log(mstemp),ylim=c(0,max(log(mstemp),na.rm=TRUE)), xlim=c(0,max(log(stemp),na.rm=TRUE)), pch=16, cex=.4, col=gray(.5), xlab="Friends", ylab="Friends of Friends")
  abline(0,1, col=2, lty=2)
  out<-cbind(stemp,mstemp)
}


# Plot Authorities

auth_chart <- function(data, x, y, color, destination) {
  x <- enquo(x)
  y <- enquo(y)
  
ggplot(data, aes(x=reorder(!!x,
                               !!y),
                     y=!!y)) + 
    geom_histogram(stat="identity", width=.5, color="black", 
                   fill=color) +
    theme_minimal(base_size = 22) + 
    coord_flip() +
    xlab("") + ylab("")
  
  
  ggsave(filename = destination,
      width = 12, height = 12, units = "in",
      pointsize = 22, bg = "white")
  
    
}



# Open the data -----------------------------------------------------------

memory.limit(size=50000)

path <- "C:/Users/Tiago Ventura/Dropbox/CalvoTiago/mlogitRedes"

#load("Bolsonaro with equlibrium Results.RData")

#load("Maldonado with equilibrium Results.RData")

#load("travelBan with  equilibrium Results.RData")


# Adjancent V and E -------------------------------------------------------

# This code is fromCWR_heterogenousnetwork by Ernesto

# res.al <- .Call(igraph:::C_R_igraph_get_adjlist, net, 3, PACKAGE = "igraph")
# al <- sapply(res.al, function(x) x+1)

#al<- get.adjlist.b(net)
# 
# res.el <- .Call(igraph:::C_R_igraph_get_adjedgelist, net, 3, PACKAGE = "igraph")
# el<-sapply(res.el, function(x) x+1)
# 
# rm(res.al,res.el)


# Summary Network ---------------------------------------------------------

# Main Graph

# Checking the communities
d <-  V(net)$membership %>% 
  as.tibble() %>% 
  
  count(value) 


# Brasil
# 1 : Haddad
# 2 : Bolsonaro

# Travel Ban
# 2: Government
# 4: Opposition

# Maldonado 
# 1: Pro Government
# 4: Anti Government

# Picking the colors

# My colors

my.color <- brewer.pal(n=9, "RdBu")
RColorBrewer::display.brewer.all()

# Building a empty containes
temp <- rep(1,length(V(net)$membership))
new.color <- data.frame(t(col2rgb(temp)/255))
new.color <- rgb(new.color, alpha=.05)

#new.color <- my.color
new.color[V(net)$membership==2] <- my.color[1] ####
#new.color[V(net)$membership==11] <- "gray" ####
new.color[V(net)$membership==4] <- my.color[9] ####
#new.color[V(net)$membership==3] <- my.color[4] ####
#new.color[V(net)$membership==37] <- "gold" ####

# Setting to the net
V(net)$new.color <- new.color

summary(net)

png(filename ="travelban_flipped.png", width = 8, height = 8, units = "in", pointsize = 12, bg = "white", res = 300)


my.den.plot(l=cbind(V(net)$l2,V(net)$l1),new.color=V(net)$new.color, ind=V(net)$ind, 
            legend = c("Pro-Travel Ban", "Anti-Travel Ban"), title="Travel Ban")


dev.off()

#plot(net, layout=layout_with_fr(net))

#plot(x,y) = l2 is the first dimension in Bolsonaro
#plot(x,y) = l1 is the first dimension in Maldonado



# The name of indegree is different for the travel ban


dev.off()



# Authorities in the network -------------------------------------------------------------
outd<-degree(net, mode="out")
ind<-degree(net, mode="in")
hubscore<-hub.score(net)$vector
authscore<-authority.score(net)$vector
membership <- V(net)$membership

# Build the data frame

d <- data_frame(names(outd), authscore, ind, membership) %>%
      filter(membership==1| membership==2 | membership==3 |membership==4 |membership==5) %>%
      split(.$membership) %>%
      map(~ arrange(., desc(ind))) %>%
      map(~ slice(., 1:30)) 


# Graphs 
# 2 pro trump
auth_chart(d[[4]], `names(outd)`, ind, color=my.color[9], 
           destination = "auth_anti_trump.png")

auth_chart(d[[2]], `names(outd)`, ind, color=my.color[1],
           destination = "auth_trump.png")

auth_chart(d[[4]], `names(outd)`, ind, color=my.color[9], 
           destination = "auth_anti_maldo.png")


# Check verified accounta
summary(net)
text <- E(net)$text
verified <- E(net)$verifiedT
auth <- E(net)$nameauth
verified2 <- E(net)$verifiedRT


# For Anti Bolsonaro
d_ver <- data_frame(verified, auth) %>%
      filter(auth %in% d[[1]]$`names(outd)`) %>%
      unique() %>%
      left_join(., d[[1]], by=c("auth"="names(outd)")) %>%
      arrange(desc(ind)) %>%
      slice(1:10)


sum(d_ver$verified==TRUE)

# For the opposition

d_ver <- data_frame(verified, auth) %>%
  filter(auth %in% d[[2]]$`names(outd)`) %>%
  unique() %>%
  left_join(., d[[2]], by=c("auth"="names(outd)")) %>%
  arrange(desc(ind)) %>%
  slice(1:10)

sum(d_ver$verified==TRUE)


# Activation per community ------------------------------------------------


# Bolsonaro

length(E(net)$text[V(net)$membership==2])


# Anti
length(E(net)$text[V(net)$membership==1])

f <- E(net)$text

#View(f)

# News FoF ----------------------------------------------------------------


summary(net)

keynews <- head(sort(table(unlist(E(net)$web.root)),decreasing=TRUE),24)

keynews.names <- names(keynews)

N<-length(keynews.names)
count.keynews<- array(0,dim=c(length(E(net)),N))

# Setting a time bar
pb <- txtProgressBar(min = 1, max = N, style = 3)
# Looping 
for(i in 1:N){
  
  temp<- grepl(keynews.names[i],E(net)$web.root, ignore.case = TRUE)
  #temp <- str_match(E(net)$text,'Arangur[A-Za-z]+[A-Za-z0-9_]+')
  count.keynews[temp==TRUE,i]<-1
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)

  }
close(pb)


# Setting the names of the media

colnames(count.keynews)<- keynews.names

#FIRST AND SECOND ORDER Keywords

tiff(filename = "Keynews_maldonado.tiff", width = 16, height = 8, units = "in", pointsize = 12, compression = c("lzw"), bg = "white", res = 100)
op <- par(mfcol=c(2,4))  
resultsNews1<- array(0,dim=c(length(V(net)),N))
resultsNews2<- array(0,dim=c(length(V(net)),N))
for(i in 1:N){
  #aa<- as.numeric(grepl(keynews[i],E(net)$text, ignore.case = TRUE))
  bb<-fomfE(count.keynews[,i],al,el)
  #dev.off()
  #title(keynews[i])
  #windows()
  bb[bb[,1]=="NaN"]<-0
  resultsNews1[,i]<- bb[,1]
  resultsNews2[,i]<- bb[,2]
}


colnames(resultsNews1)<- keynews.names
colnames(resultsNews2)<- keynews.names
dev.off()  

head(resultsNews1)

j=1
# Graph
for(j in 1:3){
  png(filename =paste0("EmbeddedNews",j,"maldonadocomplete.png"), width = 16, height = 8, units = "in", pointsize = 12, bg = "white", res = 100)
  op <- par(mfcol=c(2,4))  
  for(i in 1:8){
    plot(V(net)$l1,V(net)$l2,pch=16,  col=V(net)$new.color, cex=log(resultsNews1[,(8*(j-1))+i]+1)/3, ylim=c(-130,130),xlim=c(-130,130), xlab="", ylab="", main=colnames(resultsNews1)[(8*(j-1))+i])
    title(colnames(resultsNews1[(8*(j-1))+i]))
  }
  par(op)
  dev.off()
}



  

# Models ------------------------------------------------------------------


## Poisson -----------------------------------------------------------------


### By Community

id.name<-c(1:length(V(net)$name))

## Reducing to two communities
# Building a empty containes
temp <- rep(1,length(V(net)$membership))
new.color <- data.frame(t(col2rgb(temp)/255))
new.color <- rgb(new.color, alpha=.005)


#new.color <- my.color
my.color <- brewer.pal(n=9, "RdBu")

new.color[V(net)$membership==1] <- my.color[1] ####
new.color[V(net)$membership==2] <- my.color[9] ####

# Setting to the net
V(net)$new.color <- new.color





# Creating a data frame for reputation
for.reputation <- data.frame(id.name=id.name,name=V(net)$name,
                             l=cbind(V(net)$l2,V(net)$l1),
                             membership=V(net)$membership,
                             new.color=V(net)$new.color,
                             var=resultsNews1)


# Filtering rows withouth retweets
for.reputation <- for.reputation[which(rowSums(resultsNews1)!=0),]


# The Median for each comunnity

com.median1<-  tapply(for.reputation$l.1,for.reputation$new.color,median,na.rm=TRUE)
com.median2<-  tapply(for.reputation$l.2,for.reputation$new.color,median,na.rm=TRUE)


# Distance to the Median

for.reputation$dist.opo <- sqrt((for.reputation$l.1-com.median1[4])^2 + (for.reputation$l.2-com.median2[4])^2)
for.reputation$dist.gov <- sqrt((for.reputation$l.1-com.median1[2])^2 + (for.reputation$l.2-com.median2[2])^2)



for.reputation <- reshape(for.reputation, v.names = "count", idvar = "name", 
                          varying= list(7:30),direction = "long")

for.reputation <- within(for.reputation,
                         {sum_count = ave(count,id.name,FUN=sum)} )

for.reputation$not_count <- for.reputation$sum_count-for.reputation$count

#save(for.reputation, file="reputationdata_calvo.Rdata")

# The variables time here is the media from keynews. In this order?


###
### Reputation Reducido a una dimension
###


##MODELS OF EMBEDDING BY COMMUNITY

ind<-V(net)$ind
outd<-V(net)$outd

to.store <- vector("list",24)
for(i in 1:24){
  to.store[[i]]<-glm(resultsNews1[,i]~log(ind+1)+log(outd+1)+
                       factor(V(net)$new.color)+
                       log(outd+1):factor(V(net)$new.color)+
                       log(ind+1):factor(V(net)$new.color), 
                       family = poisson())
}

res <- to.store %>% map_df(., tidy) %>% 
  add_column(dependent_variable= rep(names(keynews), each=9))


names <- c("intercept", "In-Degree", "Out-Degree", "Pro-Bolsonaro", 
           "Anti-Bolsonaro",
           "Out-Degree Pro-Bolsonaro", "Out-Degree Anti-Bolsonaro",
           "In-Degree Pro-Bolsonaro", 
           "In-Degree Anti-Bolsonaro")

res<-  res %>% mutate(term=rep(names, 24), 
                      upper= estimate + 1.96*std.error, 
                      lower= estimate - 1.96*std.error)

View(res)
# Graphs 
pat <- c("www.|com.br|.com")

res %>% filter(term=="Pro-Bolsonaro" | term == "Anti-Bolsonaro") %>%
  mutate(dependent_variable_r=str_replace_all(dependent_variable, 
                                          "www.", "")) %>%
  
  filter(!dependent_variable_r=="f5.folha.uol.com.br") %>% 
  ggplot(., aes(y=estimate, x=dependent_variable_r,ymin=lower, ymax=upper, color=term)) + 
  geom_point(size=2) + 
  
  coord_flip() + 
  scale_color_manual(name="", 
                     values=c(my.color[1],my.color[9])) + 
  geom_errorbar(alpha=0.8, width=.5) + 
  geom_hline(yintercept = 0, col="gray", linetype="dashed") + 
  ylim(-12, 13) +
  labs(x="", y="", title="") + 
  theme_minimal() +
  geom_hline(yintercept = 0, col="lightcoral", linetype="dashed") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title=element_text(size=18, face="bold"),
        text = element_text(size=20))


ggsave(paste0(output, "web_embedding_in_Bolsonaro.png"), width = 12, height = 12, units = "in", pointsize = 22, bg = "white")

## Binomial : Ideology and Reputation

getwd()
load("./data/bolsonaro_data_weightedmean.Rdata")

dim(d_long)
head(d_long)

d_long$quant.1<- cut(d_long$V1, quantile(d_long$V1, probs = 0:10/10), labels = FALSE, include.lowest = TRUE)
d_long$quant.2<- cut(d_long$V2, quantile(d_long$V2, probs = 0:10/10), labels = FALSE, include.lowest = TRUE)
d_long$group<-paste(d_long$quant.1,d_long$quant.2, sep="")

key.groups<-unique(cbind(d_long$group,d_long$quant.1,d_long$quant.2))

unique(d_long$quant.1)

#ggplot(d_long, aes(y=V1, x=as.factor(quant.1))) + geom_point()

# The one we are using. 

media_binom.glm<-glm(cbind(count,not_count)~ -1+
                       dv + euclid:group, 
                     family = binomial(link="logit"), data=d_long)

# Tidy the results



res3 <- tidy(media_binom.glm) %>% 
  filter(str_detect(term, "euclid")) %>%
  mutate(term_rename= str_replace(term, "euclid:", ""),
         upper= estimate + 1.96*std.error, 
         lower= estimate - 1.96*std.error, 
         model="Binomial Count") %>% 
  arrange(desc(estimate))


res_heat <- res3 %>% dplyr::select(estimate, term_rename) %>% 
  mutate(xy= parse_number(term_rename)) %>% 
  arrange(xy) %>% 
  bind_cols(., as.data.frame(str_split_fixed(.$xy, 
                                             "",3))) %>%
  mutate_at(vars(starts_with("V")), as.character) %>%
  mutate(
    n_V1=ifelse(V2=="0", "10", V1),
    n_V2=ifelse(V3=="0", "10", V3),
    n_V2=ifelse(V3=="", V2, n_V2))

# Graph


# # I am not using this graph anymore, presenting the geom_tile instead
# ggplot(res3, aes(y=estimate,
#                  x=reorder(term_rename, -estimate),ymin=lower, 
#                  ymax=upper)) + 
#   geom_point(shape=21, color="black", size=2, fill='gold') + 
#   coord_flip() + 
#   scale_fill_manual(name="", 
#                     values=c("steelblue", "gold"))+
#   geom_errorbar( color="black", alpha=.8,  width=.5) + 
#   geom_hline(yintercept = 0, col="lightcoral", linetype="dashed") +
#   labs(x="", y="", title="Coefficients for ideological distance on news embedding (Binomial Model)") + 
#   theme_minimal(base_size = 10) 
# 
# ggsave("./output/ideology_binom_complete_nointercept.png", width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Heatmap

# Levelplot with ggplot2

ggplot(res_heat, aes(n_V2, n_V1, z= estimate)) +
  geom_tile(aes(fill = estimate)) + theme_minimal() +
  scale_fill_gradient(low="red", high="yellow") +
  scale_x_discrete(limits=unique(res_heat$n_V1)) +
  scale_y_discrete(limits=unique(res_heat$n_V2)) +
  labs(x="Quadrant L1", y="Quadrant L2", 
       title="Effects of ideological distance by location") + 
  theme_minimal() 

#or 

blups <- brewer.pal(n=9, "Blues")

ggplot(res_heat, aes(n_V2, n_V1, z= estimate)) +
  geom_tile(aes(fill = estimate)) + theme_minimal() +
  scale_fill_gradientn(colors=blups) +
  scale_x_discrete(limits=unique(res_heat$n_V1)) +
  scale_y_discrete(limits=unique(res_heat$n_V2)) +
  labs(x="Quadrant L1", y="Quadrant L2", 
       title="Effects of ideological distance by location") + 
  theme_minimal() 

#or 

display.brewer.all()

ggplot(res_heat, aes(n_V2, n_V1, z= estimate)) +
  geom_tile(aes(fill = estimate)) + theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd") +
  scale_x_discrete(limits=unique(res_heat$n_V1)) +
  scale_y_discrete(limits=unique(res_heat$n_V2)) +
  labs(x="Quadrant L1", y="Quadrant L2", 
       title="Effects of ideological distance by location") + 
  theme_minimal() 

ggsave(paste0(output, "ideology_grouped.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Valence -----------------------------------------------------------------

res <- tidy(media_binom.glm) %>% 
  filter(!str_detect(term, "euclid")) %>%
  mutate(term_rename= str_replace(term, "www.", ""),
         term_rename= str_replace(term_rename, "dv", ""), 
         upper= estimate + 1.96*std.error, 
         lower= estimate - 1.96*std.error, 
         model="Binomial Count") %>% 
  arrange(desc(estimate))


View(res)

# Graphs 

ggplot(res, aes(y=estimate,
                x=reorder(term_rename, estimate),ymin=lower, ymax=upper)) + 
  geom_point(shape=21, color="black", size=1, fill="darkblue") + 
  coord_flip() + 
  scale_fill_manual(name="", 
                    values=c("steelblue", "gold"))+
  geom_errorbar( color="black", alpha=.8,  width=.5) + 
  theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=14)) +
  labs(x="", y="", title="") + theme_minimal() 

ggsave(paste0(output, "valence_bolsonaro.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Heatmaps ----------------------------------------------------------------


# Making a long format and calculating the distance

#load("bolsonaro_data_weightedmean.Rdata")

head(d_long)

# Models


# The one we are using. 
my.sample<-sample(1:dim(d_long)[1], 100000, replace = FALSE, prob = NULL)

media_binom.glm <- glm(cbind(count,not_count)~ -1+
                       dv + euclid:group, 
                     family = binomial(link="logit"), data=d_long)

# Distances by group

distan<-tapply(d_long$euclid, paste(d_long$dv,d_long$group), mean)
dist.mat<-matrix(distan,100,24)
colnames(dist.mat)<-unlist(unique(lapply(names(distan), function(x) strsplit(x," ")[[1]][1])))
View(res_base)

res_base <- tidy(media_binom.glm) %>% 
  filter(str_detect(term, "euclid")) %>%
  mutate(term_rename= str_replace(term, "euclid:", ""),
         upper= estimate + 1.96*std.error, 
         lower= estimate - 1.96*std.error, 
         model="Binomial Count")


all.dat <- cbind(res_base,dist.mat)
all.dat <- arrange(all.dat, desc(estimate))

res_media <- tidy(media_binom.glm) %>% 
  filter(!str_detect(term, "euclid")) %>%
  mutate(term_rename= str_replace(term, "www.", ""),
         term_rename= str_replace(term_rename, "dv", ""), 
         upper= estimate + 1.96*std.error, 
         lower= estimate - 1.96*std.error, 
         model="Binomial Count") 


res_media$estimate[1] + rnorm(10, 0, 1)

#op<-par(mfrow = c(3, 4),ann=FALSE)
for(i in 1:24){
  
  all.dat$temp<- exp(res_media$estimate[i] + all.dat[,2]*all.dat[,9+i])/(1+exp(res_media$estimate[i]+all.dat[,2]*all.dat[,9+i]))
  
  res_heat <- all.dat %>% dplyr::select(estimate, temp, term_rename) %>% 
    mutate(xy= parse_number(term_rename)) %>% 
    arrange(xy) %>% 
    bind_cols(., as.data.frame(str_split_fixed(.$xy,"",3))) %>%
    mutate_at(vars(starts_with("V")), as.character) %>%
    mutate(
      n_V1=ifelse(V2=="0", "10", V1),
      n_V2=ifelse(V3=="0", "10", V3),
      n_V2=ifelse(V3=="", V2, n_V2))
  
  ggplot(res_heat, aes(n_V2, n_V1, z= temp)) +
    geom_tile(aes(fill = temp)) + theme_minimal() +
    scale_fill_distiller(palette= "YlOrRd") +
    scale_x_discrete(limits=unique(res_heat$n_V1)) +
    scale_y_discrete(limits=unique(res_heat$n_V2)) +
    labs(x="Quadrant L1", y="Quadrant L2", 
         title=paste("Predicted rate of embbed for ", res_media$term_rename[i],sep="")) + 
    theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=14)) +
    
  ggsave(paste0(output, "Media_bolsonaro",i,".png"), width = 10, height = 8, units = "in", pointsize = 12, bg = "white")
  

}

summary(net)

RColorBrewer::display.brewer.all()




# Summary of th network ---------------------------------------------------


# Numer of users who embedded

users <- length(d_long$id)/length(unique(d_long$dv))

# Share who embedded

users/length(V(net)$ind)





# New Model: Poisson Multinomial Approximation ----------------------------

path <- "C:/Users/venturat/Dropbox/CalvoTiago/mlogitRedes"
output <- "./output/Bolsonaro/"


setwd(path)

rm(list=ls(all=TRUE))

# Load

library(tidyverse)
library(igraph)
library(data.table)
library(arm)
library(broom)
library(RColorBrewer)
library(KernSmooth)
library(conflicted)


#load("./data/bolsonaroElectionWeek-Com-Web.RData")

load("Bolsonaro with equlibrium Results.RData")

# load("Maldonado with equilibrium Results.Rdata")

# load("travelBan with  equilibrium Results.Rdata")

memory.limit(size=50000)


summary(pois.lmer)

# Building a dataframe

final.positions<- as.data.frame(cbind(D1-xbar,market,equ.sim$median$nash-xbar,equ.sim$median$probp))
rownames(final.positions)<-colnames(resultsNews1)[my.media]
colnames(final.positions)<-c("Observed","MarketShare", "Equilibrium","OptimalShare")
final.positions$names <- colnames(resultsNews1)[my.media]

data_weights <- rbind(L.Alpha, -L.Attention,  -L.Reputation) %>% as.data.frame() %>%
                mutate(Weights=c("Ideology", "Attention",  "Reputation"))


# ggplot

library(ggrepel)

ggplot(final.positions) +
  geom_point(aes(y=names, x=Observed), 
             shape=21, color="black", fill="red", size=4) + 
  geom_segment(aes(y=names, yend=names,  x=Observed, xend=Equilibrium), 
               arrow = arrow(length = unit(0.2, "inches")), 
               size=.5) + 
  geom_vline(aes(xintercept=0), linetype="dashed", color="red") + 
  theme_minimal(base_size = 22) + labs(title="#Travel Ban") +
  xlim(-60, 60) + 
  theme(axis.title=element_blank(), 
        axis.text.y = element_text(size=22, face="bold"), 
        plot.title = element_text(size=30, face="bold")) +
  geom_segment(data=data_weights, aes(y=27, yend=24.5, x=0, xend=V1, color=Weights), 
               arrow=arrow(length = unit(0.2, "inches")),size=1,  linetype="twodash") +
  scale_color_manual(values = c("Green", "Red", "Blue"))
  

ggsave("equilibrium_in_travel_poster.png", width = 16, height = 12, units = "in", pointsize = 22, bg = "white")



# Correlation between the parameters --------------------------------------
# install.packages("merDeriv")
library(merDeriv)

vcov(pois.lmer, full = TRUE, ranpar = "var")

# Parameters by quantile --------------------------------------------------

# coef(pois.lmer)$quant[i,1] # attention

# coef(pois.lmer)$quant[i,2] # ideology

# a.R1<-matrix(coef(pois.lmer)[[2]][,1],10,K,byrow=F) # repu

# ideology
s <- VarCorr(pois.lmer)

ideology <-coef(pois.lmer)$quant[,2]
se_id<- se.coef(pois.lmer)$quant[,2]

# attention
attention <-coef(pois.lmer)$quant[,1]
se_att <- se.coef(pois.lmer)$quant[,1]



# Reputation
K=24

a.R1<-matrix(coef(pois.lmer)[[2]][,1],10,K,byrow=F) # repu
se.R1<-matrix(se.coef(pois.lmer)[[2]][,1],10,K,byrow=F) # repu

reputation <- apply(a.R1,1,median)
se_rep <- apply(se.R1, 1, median)


# Quantile
quantile <- row.names(coef(pois.lmer)$quant)


# data 

results_maldonado <- data_frame(ideology,  attention, reputation, quantile) %>%
                  gather(key=parameter, value=value, -quantile) %>% 
                  bind_cols(., data_frame(se=c(se_id, se_att, se_rep))) %>% 
                  mutate(lower= value - 1.96*se, 
                     upper=value + 1.96*se, 
                     quantile=parse_integer(quantile), 
                     case="Maldonado")


save(results_maldonado, file="parameters_maldonado.RData")

# plot
load("parameters_ban.RData")
load("parameters_maldo.RData")
load("parameters_bolsonaro.RData")


#bind the data
res_id <- bind_rows(results_bolsonaro, results_maldo, results_ban)

# 
# # graph
# 
# res_id %>% filter(parameter=="ideology") %>%
# ggplot(., aes(y=value, x=factor(quantile), fill=case))+ 
#   geom_point(alpha=1, size=2,  shape=21,
#              position=position_dodge(width=1)) +
#   geom_errorbar(aes(ymax=upper, ymin= lower), colour="black", 
#                 alpha=0.5, width=.5, 
#                 position=position_dodge(width=1)) + 
#   ylab("") + 
#   theme_minimal() +
#   theme(plot.title = element_text(size = 22, face = "bold"),
#         axis.title=element_text(size=22), 
#         legend.position = "bottom") +
# xlab("Quantiles") +
#   geom_hline(aes(yintercept = 0.0), color = 'grey') +
#   scale_fill_discrete(name="") +
#   ggtitle("Ideology")
# 
# 
# ggsave("parameters_op1.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")
# 
# res_id %>% filter(parameter=="ideology") %>%
#   ggplot(., aes(y=value, x=factor(quantile), fill=case))+ 
#   geom_point(alpha=1, size=2,  shape=21,
#              position=position_dodge(width=1)) +
#   geom_errorbar(aes(ymax=upper, ymin= lower), colour="black", 
#                 alpha=0.5, width=.5, 
#                 position=position_dodge(width=1)) + 
#   ylab("") + 
#   theme_bw() +
#   theme(plot.title = element_text(size = 22, face = "bold"),
#         axis.title=element_text(size=22), 
#         legend.position = "bottom", 
#         axis.title.x = element_text(hjust = 1)) +
#   xlab("Quantiles") +
#   geom_hline(aes(yintercept = 0.0), color = 'grey') +
#   scale_fill_discrete(name="") +
#   ggtitle("Ideology") +
#   facet_grid(.~case)
# 
# 
# ggsave("parameters_op2.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")
# 


res_id %>% filter(parameter=="ideology") %>%
  ggplot(., aes(y=value, x=factor(quantile), fill=case))+ 
  geom_point(alpha=1, size=4,  shape=21,
             position=position_dodge(width=1)) +
  geom_errorbar(aes(ymax=upper, ymin= lower), colour="black", 
                alpha=1, width=.5, 
                position=position_dodge(width=1)) + 
  ylab("") + 
  labs(title = "Ideology by quantile \n") +
  theme_bw() +
  theme(plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size=22),
        axis.title=element_text(size=16), 
        legend.position = "bottom", 
        axis.title.x = element_text(hjust = 1), 
        legend.text =  element_text(size=22), 
        strip.text.y = element_text(size = 16, face="bold")) +
  xlab("Quantiles") +
  geom_hline(aes(yintercept = 0.0), color = 'tomato2', linetype="dotted") +
  scale_fill_discrete(name="") +
  facet_grid(case~.) 

ggsave("parameters_ideo_poster.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")

summary(pois.lmer)

res_id %>% filter(parameter=="attention") %>%
  ggplot(., aes(y=value, x=factor(quantile), fill=case))+ 
  geom_point(alpha=1, size=4,  shape=21,
             position=position_dodge(width=1)) +
  geom_errorbar(aes(ymax=upper, ymin= lower), colour="black", 
                alpha=1, width=.5, 
                position=position_dodge(width=1)) + 
  ylab("") + 
  labs(title = "Attention by quantile \n") +
  theme_bw() +
  theme(plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size=22),
        axis.title=element_text(size=16), 
        legend.position = "bottom", 
        axis.title.x = element_text(hjust = 1), 
        legend.text =  element_text(size=22), 
        strip.text.y = element_text(size = 16, face="bold")) +
  xlab("Quantiles") +
  geom_hline(aes(yintercept = 0.0), color = 'tomato2', linetype="dotted") +
  scale_fill_discrete(name="") +
  facet_grid(case~.) 

ggsave("parameters_attention_poster.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")


# Reputation



res_id %>% filter(parameter=="reputation") %>%
  ggplot(., aes(y=value, x=factor(quantile), fill=case))+ 
  geom_point(alpha=1, size=2,  shape=21,
             position=position_dodge(width=1)) +
  geom_errorbar(aes(ymax=upper, ymin= lower), colour="black", 
                alpha=0.5, width=.5, 
                position=position_dodge(width=1)) + 
  ylab("") + 
  theme_bw() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title=element_text(size=16), 
        legend.position = "bottom", 
        axis.title.x = element_text(hjust = 1), 
        legend.text =  element_text(size=22)) +
  xlab("Quantiles") +
  geom_hline(aes(yintercept = 0.0), color = 'tomato2', linetype="dotted") +
  scale_fill_discrete(name="") +
  facet_grid(case~.)

ggsave("parameters_reputation.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")


# Reputation by Media

# Bolsonaro
load("Bolsonaro with equlibrium Results.RData")
pois.lmer_bolsonaro <- pois.lmer
resultsnews_bolsonaro <- resultsNews1


a.repu <- coef(pois.lmer_bolsonaro)$.time_1[,1]
se.rep <- se.coef(pois.lmer_bolsonaro)$.time_1[,1]
names <- colnames(resultsnews_bolsonaro)

results_reputation <- tibble(value=a.repu, se=se.rep, media=names) %>%
  mutate(upper=value + 1.96*se, 
         lower=value - 1.96*se) %>% 
  arrange(value) %>%
  mutate(media=fct_inorder(media)) 

results_reputation <- results_reputation  %>% filter(!(str_detect(media, "twitter.com|youtu.be")))

ggplot(results_reputation,
       aes(y=value, x=media, ymax=upper, ymin= lower)) + 
  geom_pointrange(color="steelblue", fill="white", shape=21, size=1) +
  ylab("") + 
  labs(title = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title=element_blank(), 
        axis.title.x = element_text(hjust = 1), 
        axis.text = element_text(size=14),
        legend.position = "bottom", 
        legend.text=element_text(size=14)) +
  geom_hline(aes(yintercept = median(results_reputation$value), 
                 color="Median Reputation"),
             linetype="twodash", size=1) +
  scale_fill_discrete(name="") +
  coord_flip() +
  scale_color_manual(name = "", 
                     values = c('Median Reputation'= "red"))


ggsave("parameters_reputation_bolsonaro.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")

# Maldonado
load("Maldonado with equilibrium Results.RData")
pois.lmer_m <- pois.lmer
resultsnews_m <- resultsNews1

a.repu <- coef(pois.lmer_m)$.time_1[,1]
se.rep <- se.coef(pois.lmer_m)$.time_1[,1]
names <- colnames(resultsnews_m)

results_reputation <- tibble(value=a.repu, se=se.rep, media=names) %>%
  mutate(upper=value + 1.96*se, 
         lower=value - 1.96*se) %>% 
  arrange(value) %>%
  mutate(media=fct_inorder(media)) 

results_reputation <- results_reputation  %>% filter(!(str_detect(media, "twitter.com|youtu.be|fb.me")))


ggplot(results_reputation,
       aes(y=value, x=media, ymax=upper, ymin= lower)) + 
  geom_pointrange(color="steelblue", fill="white", shape=21, size=1) +
  ylab("") + 
  labs(title = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title=element_blank(), 
        axis.title.x = element_text(hjust = 1), 
        axis.text = element_text(size=14),
        legend.position = "bottom", 
        legend.text=element_text(size=14)) +
  geom_hline(aes(yintercept = median(results_reputation$value), 
                 color="Median Reputation"),
             linetype="twodash", size=1) +
  scale_fill_discrete(name="") +
  coord_flip() +
  scale_color_manual(name = "", 
                     values = c('Median Reputation'= "red"))

ggsave("parameters_reputation_maldonado.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")

list.files()
load("travelBan with  equilibrium Results.RData")
pois.lmer_ban <- pois.lmer
resultsnews_ban <- resultsNews1

a.repu <- coef(pois.lmer_ban)$.time_1[,1]
se.rep <- se.coef(pois.lmer_ban)$.time_1[,1]
names <- colnames(resultsnews_ban)

results_reputation <- tibble(value=a.repu, se=se.rep, media=names) %>%
  mutate(upper=value + 1.96*se, 
         lower=value - 1.96*se) %>% 
  arrange(value) %>%
  mutate(media=fct_inorder(media)) 

results_reputation <- results_reputation  %>% filter(!(str_detect(media, "twitter.com|youtu.be|fb.me")))


ggplot(results_reputation,
       aes(y=value, x=media, ymax=upper, ymin= lower)) + 
  geom_pointrange(color="steelblue", fill="white", shape=21, size=1) +
  ylab("") + 
  labs(title = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title=element_blank(), 
        axis.title.x = element_text(hjust = 1), 
        axis.text = element_text(size=14),
        legend.position = "bottom", 
        legend.text=element_text(size=14)) +
  geom_hline(aes(yintercept = median(results_reputation$value), 
                 color="Median Reputation"),
             linetype="twodash", size=1) +
  scale_fill_discrete(name="") +
  coord_flip() +
  scale_color_manual(name = "", 
                     values = c('Median Reputation'= "red"))

ggsave("parameters_reputation_ban.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")



# Correlation

samples <- res_id  %>% 
  mutate(n=1000) %>%
  rowwise() %>% 
  mutate(dist=list(rnorm(n, value, se))) %>% 
  unnest(dist) %>%
  ungroup() %>%
  dplyr::select(quantile, case, parameter, dist) %>%
  pivot_wider(names_from=parameter, 
              values_from=dist) %>%
  unnest()

res_wider <- samples %>% 
                group_by(case) %>%
             mutate(re_ideology = -1*rescale(ideology), 
                     re_attention = rescale(attention)) %>%
              ungroup() %>%
              nest_by(case) %>%
              mutate(cor=list(cor(data$re_ideology,data$re_attention))) %>%
              unnest()
write.csv(res_wider, file="simulations.csv")
write.csv(res_id, file="parameters.csv")

cor <- res_wider %>% 
        dplyr::select(case, cor) %>% 
          distinct() %>% 
          mutate(cor=round(cor, 2))

res_wider %>% filter(case=="Bolsonaro") %>%
ggplot(., aes(x=re_ideology, y=re_attention,
                      fill=-2*re_ideology,
                      colour=re_ideology)) + 
  geom_point(alpha=.4, size=8, shape=21) +
  annotate(geom="label", x = -.75, y = .9, size=8,
           label = paste("Person Correlation =", cor$cor[1])) + 
  theme_bw() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title=element_text(size=20), 
        axis.title.x = element_text(hjust = 1), 
        legend.text =  element_text(size=18), 
        strip.text = element_text(size = 16, face="bold")) +
  scale_fill_distiller(palette="Blues", name="") +
  scale_color_gradient(low = "gray90", high = "gray10") +
  scale_shape_manual(values=c(21, 22, 23), 
                     name="") +
  xlab("") +
  ylab("") +
  guides(colour=FALSE,fill=FALSE) +
  facet_grid(~case) 


models <- res_wider %>%
  group_by(case) %>%
  nest() %>%
  mutate(model=map(data, ~ lm(re_ideology~re_attention, data=.x)), 
         res=map(model, tidy)) %>% 
  unnest(res) %>%
  mutate(estimate=round(estimate, digits = 4))


ggplot(res_wider, aes(x=re_ideology, y=re_attention,
                fill=re_ideology,
                colour=re_ideology)) + 
  geom_point(alpha=.4, size=8, shape=21) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title=element_text(size=20), 
        axis.title.x = element_text(hjust = 1),
        axis.text = element_text(size=14),
        legend.text =  element_text(size=18), 
        strip.text = element_text(size = 16, face="bold")) +
  scale_fill_gradient(low = "#0091ff", high = "#f0650e") +
scale_color_gradient(low = "gray90", high = "gray10") +
  scale_shape_manual(values=c(21, 22, 23), 
                     name="") +
  xlab("Ideology (Alpha)") +
  ylab("Attention") +
  guides(colour=FALSE,fill=FALSE) +
  facet_grid(~case) 

ggsave("corr_figure.png", width = 12, height = 12, units = "in", pointsize = 22, bg = "white")








