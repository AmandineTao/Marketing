########################################################
# Good Customers Marketing strategy
#######################################################


options(stringsAsFactors = FALSE)

data <- read.csv2(file="Final-datafiles_16102018.csv")


#number of active customer == nb of indiv who have numb purchase which are not NA
library(dplyr)

#active customer 
names(data)
nrow(dplyr::filter(data, !is.na(MONTANT_CUMULE))) #16512 
#unactive customer
nrow(dplyr::filter(data, is.na(MONTANT_CUMULE))) #19443


data$CA_MOY_VISITE <- as.numeric(data$CA_MOY_VISITE)
data$NB_PRDT_MOY_VISITE <- as.numeric(data$NB_PRDT_MOY_VISITE)


#RFM analysis
data[is.na(data$NB_VISITE),]$NB_VISITE <- 0
data[is.na(data$NB_VISITE),]$NB_VISITE <- 0
data[is.na(data$NB_VISITE),]$NB_VISITE <- 0


hist(data$NB_VISITE,breaks = 50)
hist(data$CA_MOY_VISITE)
hist(data$MONTANT_CUMULE)
hist(data$RECENCE)

#finding the cut for FREQUENCY (nb_visite) and MONETARY (montant_cumule)

data_active <- filter(data, !is.na(MONTANT_CUMULE))
quantile(data_active$NB_VISITE, probs = c(0.33, 0.66)) # 1 et 3

data_active$frequency <- ifelse(data_active$NB_VISITE<=1,"Weak","")
data_active$frequency <- ifelse(data_active$NB_VISITE>1 & data_active$NB_VISITE<=3,"Medium",data_active$frequency)
data_active$frequency <- ifelse(data_active$NB_VISITE>3,"High",data_active$frequency)



table(data_active$frequency)

quantile(data_active$MONTANT_CUMULE, probs = c(0.33, 0.66)) # 165 et 342

data_active$monetary <- ifelse(data_active$MONTANT_CUMULE<=165,"Weak","")
data_active$monetary <- ifelse(data_active$MONTANT_CUMULE>165 & data_active$MONTANT_CUMULE<=342,"Medium",data_active$monetary)
data_active$monetary <- ifelse(data_active$MONTANT_CUMULE>342,"High",data_active$monetary)

table(data_active$monetary)

data_active$FM <- ifelse((data_active$frequency=="Weak" & data_active$monetary=="Weak") |
                           (data_active$frequency=="Weak" & data_active$monetary=="Medium") |
                           (data_active$frequency=="Medium" & data_active$monetary=="Weak"),"Weak","")
data_active$FM <- ifelse((data_active$frequency=="High" & data_active$monetary=="Weak") |
                           (data_active$frequency=="Medium" & data_active$monetary=="Medium") |
                           (data_active$frequency=="Weak" & data_active$monetary=="High"),"Medium",data_active$FM)
data_active$FM <- ifelse((data_active$frequency=="High" & data_active$monetary=="High") |
                           (data_active$frequency=="High" & data_active$monetary=="Medium") |
                           (data_active$frequency=="Medium" & data_active$monetary=="High"),"High",data_active$FM)

table(data_active$monetary,data_active$frequency)
table(data_active$FM)

quantile(data_active$RECENCE, probs = c(0.33, 0.66)) # 87 et 277

data_active$recency <- ifelse(data_active$RECENCE>277,"Weak","")
data_active$recency <- ifelse(data_active$RECENCE<=277 & data_active$RECENCE>87,"Medium",data_active$recency)
data_active$recency <- ifelse(data_active$RECENCE<=87,"High",data_active$recency)

table(data_active$recency)



data_active$RFM <- ifelse(data_active$FM=="Weak" & data_active$recency=="Weak",1,0)
data_active$RFM <- ifelse(data_active$FM=="Medium" & data_active$recency=="Weak",4,data_active$RFM)
data_active$RFM <- ifelse(data_active$FM=="High" & data_active$recency=="Weak",7,data_active$RFM)
data_active$RFM <- ifelse(data_active$FM=="Weak" & data_active$recency=="Medium",2,data_active$RFM)
data_active$RFM <- ifelse(data_active$FM=="Medium" & data_active$recency=="Medium",5,data_active$RFM)
data_active$RFM <- ifelse(data_active$FM=="High" & data_active$recency=="Medium",8,data_active$RFM)
data_active$RFM <- ifelse(data_active$FM=="Weak" & data_active$recency=="High",3,data_active$RFM)
data_active$RFM <- ifelse(data_active$FM=="Medium" & data_active$recency=="High",6,data_active$RFM)
data_active$RFM <- ifelse(data_active$FM=="High" & data_active$recency=="High",9,data_active$RFM)

table(data_active$RFM)

#top customers  : gr 8 & 9 : 5465
#good customers : gr 6 & 5 : 2215
#Decelerations  : gr 4 & 7 : 1657
#small customers: gr 2 & 3 : 3334
#Weak customer  : gr 1     : 3841
#inactive

#new customer : 0 or 1 year of recency : using ANCIENNETE
data_active$TypoRFM = ifelse(data_active$RFM==6|data_active$RFM==5,'goodcustomers','')
data_active$TypoRFM = ifelse(data_active$RFM==8|data_active$RFM==9,'topcustomers',data_active$TypoRFM)
data_active$TypoRFM = ifelse(data_active$RFM==4|data_active$RFM==7,'deceleration',data_active$TypoRFM)
data_active$TypoRFM = ifelse(data_active$RFM==2|data_active$RFM==3,'smallcustomers',data_active$TypoRFM)
data_active$TypoRFM = ifelse(data_active$RFM==1,'weakcustomers',data_active$TypoRFM)

table(data_active$TypoRFM)
data_active$new_cust <- ifelse(data_active$ANCIENNETE<=1,1,0)
table(data_active$new_cust) #8923 more than half of active customers

#sur toute la bdd : des new customer en plus
data$new_cust <- ifelse(data$ANCIENNETE<=1,1,0)
table(data$new_cust) #9109 : some new customers are inactive 



#loyalty program sensibility

data_active$loyalty_sens <- ifelse(data_active$NB_CADEAUX==0,"insensible","")
data_active$loyalty_sens <- ifelse(data_active$NB_CADEAUX>0 & data_active$NB_CADEAUX<=3,"reactive",data_active$loyalty_sens)

data_active$loyalty_sens <- ifelse(data_active$NB_CADEAUX>3,"addict",data_active$loyalty_sens)
data_active$loyalty_sens <- ifelse(is.na(data_active$NB_CADEAUX)==TRUE,"empty",data_active$loyalty_sens)

table(data_active$loyalty_sens)

data_active$eclect <- ifelse(data_active$NB_LIGNE_DIFF==1,1,0)
data_active$eclect <- ifelse(data_active$NB_LIGNE_DIFF==2,2,data_active$eclect)
data_active$eclect <- ifelse(data_active$NB_LIGNE_DIFF==3,3,data_active$eclect)
data_active$eclect <- ifelse(is.na(data_active$NB_LIGNE_DIFF)==TRUE,0,data_active$eclect)

table(data_active$eclect)
#0 value is empty

data_active$diversif <- ifelse(data_active$NB_FAM_DIFF<=2,"basic","")
data_active$diversif <- ifelse(data_active$NB_FAM_DIFF>2 & data_active$NB_FAM_DIFF<=5,"diversified",data_active$diversif)
data_active$diversif <- ifelse(data_active$NB_FAM_DIFF>5,"addict",data_active$diversif)
data_active$diversif <- ifelse(is.na(data_active$NB_FAM_DIFF)==TRUE,"empty",data_active$diversif)

table(data_active$diversif)
table(data_active$NB_FAM_DIFF)

test <-count(data_active,PART_VIST_MAG_GEST)

data_active$store_attach <- ifelse(data_active$PART_VIST_MAG_GEST<0.7,"weak","")
data_active$store_attach <- ifelse(data_active$PART_VIST_MAG_GEST>=0.7 & data_active$PART_VIST_MAG_GEST<1,"medium",data_active$store_attach)
data_active$store_attach <- ifelse(data_active$PART_VIST_MAG_GEST>=1,"high",data_active$store_attach)
data_active$store_attach <- ifelse(is.na(data_active$PART_VIST_MAG_GEST)==TRUE,"empty",data_active$store_attach)

write.csv2(data_active[,c(1:28)],"data_loyalty_cards_indicateurs.csv",row.names = FALSE)

table(data_active$store_attach)
table(data_active$PART_VISIT_MAG_GEST)

#Debut
#cellup = read.csv2("data_loyalty_cards_indicateurs.csv")
cellup = data_active
View(cellup)

cellup$CENTRE_VILLE[cellup$CENTRE_VILLE=='Centre Co'] <- 'Centre Commercial'
cellup$CENTRE_VILLE[cellup$CENTRE_VILLE=='Centre Ville'] <- 'Centre ville'

table(cellup$TypoRFM)
good_customers = cellup[cellup$TypoRFM == "goodcustomers",]
top_customers = cellup[cellup$TypoRFM == "topcustomers",]
cellup = cellup[cellup$TypoRFM != "goodcustomers",]
summary(good_customers)
nb_good_cust = nrow(good_customers)
nb_good_cust
nb_good_cust/nrow(cellup)
good_customers

good_customers$MONTANT_CUMULE


total_income_gc  = sum(good_customers$MONTANT_CUMULE) 
total_income_gc

#Age
hist(good_customers$AGE)

good_customers[is.na(good_customers[,15]), 15] <- mean(good_customers[,15], na.rm = TRUE)
top_customers[is.na(top_customers[,15]), 15] <- mean(top_customers[,15], na.rm = TRUE)
cellup[is.na(cellup[,15]), 15] <- mean(cellup[,15], na.rm = TRUE)

dens=density(good_customers$AGE)
dens2=density(cellup$AGE)
dens3=density(top_customers$AGE)

par(bg=NA)
plot(dens$x,dens$y,type="l",xlab="Age",ylab="Probability estimate",
     col='#FA6E6E',lwd=2)
#lines(dens2$x,dens2$y,lwd=2,col='dark grey')
lines(dens3$x,dens3$y,lwd=2,col='light blue')
abline(v = mean(good_customers$AGE), col="#FA6E6E", lwd=1, lty=2)
text(x=mean(good_customers$AGE)*1.2,y=0,labels=paste('mean = ',round(mean(good_customers$AGE),0)),
     col = '#FA6E6E',cex=0.8)
legend('topright',legend=c('Good customers','Top customers'),col=c('#FA6E6E','light blue'),
       lty = c(1,1),box.lty=0,cex=0.8)

#Sexe
prop.table(table(good_customers$SEXE))    #2201 women and 25 men (1.12% H and 98.83% F)
prop.table(table(cellup$SEXE))    #16380 women and 124 men (0.751% H and 99.20% F)

#Montant cumule
dens=density(good_customers$MONTANT_CUMULE)
dens2=density(cellup$MONTANT_CUMULE)
dens3=density(top_customers$MONTANT_CUMULE)

par(bg=NA)
plot(dens$x,dens$y,type="l",xlab="Ammount spent",ylab="Probability estimate",xlim = c(0,1200),
     col='#FA6E6E',lwd=2)
#lines(dens2$x,dens2$y,lwd=2,col='dark grey')
lines(dens3$x,dens3$y,lwd=2,col='light blue')
abline(v = mean(good_customers$MONTANT_CUMULE), col="#FA6E6E", lwd=1, lty=2)
text(x=mean(good_customers$MONTANT_CUMULE)/1.25,y=0,labels=paste('mean = ',round(mean(good_customers$MONTANT_CUMULE),0)),
     col = '#FA6E6E',cex=0.8)
legend('topright',legend=c('Good customers','Top customers'),col=c('#FA6E6E','light blue'),
       lty = c(1,1),box.lty=0,cex=0.8)

plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",ylim=c(0,0.01),xlim=c(0,800),col='blue',lwd=2)
lines(dens2$x,dens2$y,lwd=2)
lines(dens3$x,dens3$y,lwd=2,col='red')
#They spend between 200 and 300 total

#Montant moyen par visite
dens=density(good_customers$CA_MOY_VISITE)
dens2=density(cellup$CA_MOY_VISITE)
dens3=density(top_customers$CA_MOY_VISITE)

par(bg=NA)
plot(dens$x,dens$y,type="l",xlab="Average spent per visit",ylab="Probability estimate",xlim = c(0,400),
     col='#FA6E6E',lwd=2)
#lines(dens2$x,dens2$y,lwd=2,col='dark grey')
lines(dens3$x,dens3$y,lwd=2,col='light blue')
abline(v = mean(good_customers$CA_MOY_VISITE), col="#FA6E6E", lwd=1, lty=2)
text(x=mean(good_customers$CA_MOY_VISITE)/1.25,y=0,labels=paste('mean = ',round(mean(good_customers$CA_MOY_VISITE),0)),
     col = '#FA6E6E',cex=0.8)
legend('topright',legend=c('Good customers','Top customers'),col=c('#FA6E6E','light blue'),
       lty = c(1,1),box.lty=0,cex=0.8)

plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",ylim=c(0,0.01),xlim=c(0,800),col='blue',lwd=2)
lines(dens2$x,dens2$y,lwd=2)
lines(dens3$x,dens3$y,lwd=2,col='red')

#Nb produits moyen par visite
dens=density(good_customers$NB_PRDT_MOY_VISITE)
dens2=density(cellup$NB_PRDT_MOY_VISITE)
dens3=density(top_customers$NB_PRDT_MOY_VISITE)

par(bg=NA)
plot(dens$x,dens$y,type="l",xlab="Products per visit",ylab="Probability estimate",xlim=c(0,10),
     col='#FA6E6E',lwd=2)
#lines(dens2$x,dens2$y,lwd=2,col='dark grey')
lines(dens3$x,dens3$y,lwd=2,col='light blue')
abline(v = mean(good_customers$NB_PRDT_MOY_VISITE), col="#FA6E6E", lwd=1, lty=2)
text(x=mean(good_customers$NB_PRDT_MOY_VISITE)/1.25,y=0,labels=paste('mean = ',round(mean(good_customers$NB_PRDT_MOY_VISITE),0)),
     col = '#FA6E6E',cex=0.8)
legend('topright',legend=c('Good customers','Top customers'),col=c('#FA6E6E','light blue'),
       lty = c(1,1),box.lty=0,cex=0.8)

plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",col='blue',lwd=2)
lines(dens2$x,dens2$y,lwd=2)
lines(dens3$x,dens3$y,lwd=2,col='red')

#Recence
dens=density(good_customers$RECENCE,from = 0)
dens2=density(cellup$RECENCE,from = 0)
dens3=density(top_customers$RECENCE,from = 0)
#hist(cellup$RECENCE,breaks = 100)

par(bg=NA)
plot(dens$x,dens$y,type="l",xlab="Recency",ylab="Probability estimate",xlim=c(0,500),ylim=c(0,0.011),
     col='#FA6E6E',lwd=2)
#lines(dens2$x,dens2$y,lwd=2,col='dark grey')
lines(dens3$x,dens3$y,lwd=2,col='light blue')
abline(v = mean(good_customers$RECENCE), col="#FA6E6E", lwd=1, lty=2)
text(x=mean(good_customers$RECENCE)/1.25,y=0,labels=paste('mean = ',round(mean(good_customers$RECENCE),0)),
     col = '#FA6E6E',cex=0.8)
legend('topright',legend=c('Good customers','Top customers'),col=c('#FA6E6E','light blue'),
       lty = c(1,1),box.lty=0,cex=0.8)

plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",ylim=c(0,0.01),col='blue',lwd=2)
lines(dens2$x,dens2$y,lwd=2)
lines(dens3$x,dens3$y,lwd=2,col='red')

#Anciennete
dens=density(good_customers$ANCIENNETE,from = 0)
dens2=density(cellup$ANCIENNETE,from = 0)
dens3=density(top_customers$ANCIENNETE,from = 0)

par(bg=NA)
plot(dens$x,dens$y,type="l",xlab="Seniority",ylab="Probability estimate",xlim=c(0,12),
     col='#FA6E6E',lwd=2)
#lines(dens2$x,dens2$y,lwd=2,col='dark grey')
lines(dens3$x,dens3$y,lwd=2,col='light blue')
abline(v = mean(good_customers$ANCIENNETE), col="#FA6E6E", lwd=1, lty=2)
text(x=mean(good_customers$ANCIENNETE)/1.25,y=0,labels=paste('mean = ',round(mean(good_customers$ANCIENNETE),0)),
     col = '#FA6E6E',cex=0.8)
legend('topright',legend=c('Good customers','Top customers'),col=c('#FA6E6E','light blue'),
       lty = c(1,1),box.lty=0,cex=0.8)


plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",col='blue',lwd=2)
lines(dens2$x,dens2$y,lwd=2)
lines(dens3$x,dens3$y,lwd=2,col='red')


#Nb visites
dens=density(good_customers$NB_VISITE,from = 0)
dens2=density(cellup$NB_VISITE,from = 0)
dens3=density(top_customers$NB_VISITE,from = 0)


par(bg=NA)
plot(dens$x,dens$y,type="l",xlab="Number of visits",ylab="Probability estimate",xlim=c(0,10),
     col='#FA6E6E',lwd=2)
#lines(dens2$x,dens2$y,lwd=2,col='dark grey')
lines(dens3$x,dens3$y,lwd=2,col='light blue')
abline(v = mean(good_customers$NB_VISITE), col="#FA6E6E", lwd=1, lty=2)
text(x=mean(good_customers$NB_VISITE)/1.25,y=0,labels=paste('mean = ',round(mean(good_customers$NB_VISITE),0)),
     col = '#FA6E6E',cex=0.8)
legend('topright',legend=c('Good customers','Top customers'),col=c('#FA6E6E','light blue'),
       lty = c(1,1),box.lty=0,cex=0.8)


plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",col='blue',lwd=2,xlim = c(0,10))
lines(dens2$x,dens2$y,lwd=2)
lines(dens3$x,dens3$y,lwd=2,col='red')

#Region
prop.table(table(good_customers$REGIONS))    #37.8% Paris and 62.2% Province
prop.table(table(cellup$REGIONS))    #35.5% Paris and 64.5% Province

#Nb ligne diff
prop.table(table(good_customers$NB_LIGNE_DIFF))    #Majorité: 2 cat
prop.table(table(cellup$NB_LIGNE_DIFF))    #Majorité: 1 cat
prop.table(table(top_customers$NB_LIGNE_DIFF)) #Majorité: 3 cat

#Centre ville
prop.table(table(good_customers$CENTRE_VILLE)) #72.1% centre ville
prop.table(table(cellup$CENTRE_VILLE)) #70.4% centre ville
prop.table(table(top_customers$CENTRE_VILLE)) #74.3% centre ville

#Civilite
prop.table(table(good_customers$CIVILITE))
prop.table(table(cellup$CIVILITE))
prop.table(table(top_customers$CIVILITE))

#Nb cadeaux
good_customers[is.na(good_customers[,8]), 8] <- 0
top_customers[is.na(top_customers[,8]), 8] <- 0
cellup[is.na(cellup[,8]), 8] <- 0

dens=density(good_customers$NB_CADEAUX,from = 0)
dens2=density(cellup$NB_CADEAUX,from = 0)
dens3=density(top_customers$NB_CADEAUX,from = 0)

par(bg=NA)
plot(dens$x,dens$y,type="l",xlab="Number of gifts",ylab="Probability estimate",xlim=c(0,10),
     col='#FA6E6E',lwd=2)
#lines(dens2$x,dens2$y,lwd=2,col='dark grey')
lines(dens3$x,dens3$y,lwd=2,col='light blue')
abline(v = mean(good_customers$NB_CADEAUX), col="#FA6E6E", lwd=1, lty=2)
text(x=mean(good_customers$NB_CADEAUX)/1.25,y=0,labels=paste('mean = ',round(mean(good_customers$NB_CADEAUX),0)),
     col = '#FA6E6E',cex=0.8)
legend('topright',legend=c('Good customers','Top customers'),col=c('#FA6E6E','light blue'),
       lty = c(1,1),box.lty=0,cex=0.8)

plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",col='blue',lwd=2,xlim = c(0,10))
lines(dens2$x,dens2$y,lwd=2)
lines(dens3$x,dens3$y,lwd=2,col='red')

names(cellup)