#loading libraries
library(dplyr)
library(ggplot2)
library(caTools)
library(data.table)
#loading the data
setwd("C:/Users/Utsav/Desktop/CreditCard_Proj")
#setwd("")
cust_data<-fread("cc_dbase.csv")
cust_data_backup<-cust_data
cust_data[1:6,1:6]
str(cust_data)
#removing unuseful columns
cust_data_subset<-cust_data[,-c("custid","age","birthmonth","ed","employ","income","lninc","creddebt","othdebt","spoused"
                                ,"pets_cats","pets_dogs","pets_birds","pets_reptiles","pets_small","pets_saltfish","pets_freshfish"
                                ,"address","carvalue","commute","commutetime","commutecar","commutecarpool","commutemotorcycle","commutecarpool"
                                ,"commutebus","commuterail","commutepublic","commutebike","commutewalk","commutenonmotor"
                                ,"telecommute","polview","polparty","polcontrib","vote","cardtenure","card2tenure","bfast","lnlongmon","lntollmon","lnlongten","lntollten","lnequipmon"
                                ,"lnequipten","lncardmon","lncardten","lnwiremon","lnwireten"
                          )]
#separating columns according to factors and numeric ones
cust_cat<-cust_data_subset[,-c("debtinc","lncreddebt","lnothdebt","reside","pets"
                               ,"cars","carditems","card2items","card2spent","cardspent",
                               "tenure","longmon","longten","tollmon","tollten","equipmon","equipten",
                               "cardmon","cardten","wiremon","wireten","hourstv")]

cust_num<-cust_data_subset[,c("debtinc","lncreddebt","lnothdebt","reside","pets"
                              ,"cars","carditems","card2items","card2spent","cardspent",
                              "tenure","longmon","longten","tollmon","tollten","equipmon","equipten",
                              "cardmon","cardten","wiremon","wireten","hourstv")]

#converting into factor
cust_cat[,1:62]<-lapply(cust_cat[,1:62],as.character)
cust_cat[,1:62]<-lapply(cust_cat[,1:62],as.factor)
str(cust_cat)
str(cust_num)

#combining again into one dataset
cust_data_sub<-cbind(cust_num,cust_cat)
str(cust_data_sub)
summary(cust_data_sub)

#checking for NAs
na.values<-View(cust_data_sub[!complete.cases(cust_data_sub)])

#reordering columns(not working)
col_names<-names(cust_data_subset)
class(cust_data_subset)
cust_data_sub<-cust_data_sub[,col_names]

#removing NAs for townsize
mean_reg_twn_1<-as.numeric(cust_data%>%filter(region=="1")%>%select(townsize)%>%summarize(mn=ceiling(median(townsize,na.rm=T))))
cust_data_sub[cust_data_sub$region=="1" & is.na(cust_data_sub$townsize),"townsize"]<-as.factor(mean_reg_twn_1)

mean_reg_twn_5<-as.numeric(cust_data%>%filter(region=="5")%>%select(townsize)%>%summarize(mn=ceiling(median(townsize,na.rm=T))))
cust_data_sub[cust_data_sub$region=="5" & is.na(cust_data_sub$townsize),"townsize"]<-as.factor(mean_reg_twn_5)

na.values 
#combining card 1 spent and card 2 spent
cust_data_sub$total_cardspent<-cust_data_sub$cardspent+cust_data_sub$card2spent
head(cust_data_sub[,c("cardspent","card2spent","total_cardspent")])
