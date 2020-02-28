library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)


#reorder the columns
colnames(fmarket)
col_names<-c("FMID","MarketName","Website","Facebook","Twitter", "Youtube","OtherMedia","street","city","County","State","zip","Season1Date","Season1Time","Season2Date","Season2Time","Season3Date","Season3Time","Season4Date","Season4Time","x","y","Location","updateTime","Credit","WIC","WICcash","SFMNP","SNAP","Organic","Bakedgoods","Cheese","Crafts","Flowers","Eggs","Seafood","Herbs","Vegetables","Honey","Jams","Maple","Meat","Nursery","Nuts","Plants","Poultry","Prepared","Soap","Trees","Wine","Coffee","Beans","Fruits","Grains","Juices","Mushrooms","PetFood","Tofu","WildHarvested")
fmarketorg<-fmarket[,col_names]
View(fmarketorg)

#converting the character to numerical variables
fmarketorg[,25:59]<-ifelse((fmarketorg[,25:59])=="Y",1,0)
View(fmarketorg)

#Product distribution 
temp3 <- fmarket[,29:58]
temp3.1 <- as.data.frame(ifelse(temp3[,2:30] == "Y","1","0"))
temp3.2 <- na.omit(temp3.1, cols = "Bakedgoods")
apply(is.na(temp3.2),2,sum)
temp3.2 <- as.data.frame(lapply(temp3.2,as.numeric))
temp3.2 <- as.data.frame(ifelse(temp3.2 == 2,1,0))
tail(temp3.2)
sumdata=data.frame(value=apply(temp3.2,2,sum))
sumdata
sumdata$key=rownames(sumdata)
all_products<- sumdata[order(-sumdata$value),][1:29,]
ggplot(data=all_products, aes(x=reorder(key,value), y=value, fill=key)) +
  geom_bar(position="Stack", colour="black", stat="identity") +  geom_text(aes(label=value),hjust = 1.5, size=2.9) + theme_minimal()  + labs(title = "Different Produts sold in Farmer's market") + xlab("Product") + ylab("count") +theme(legend.position = "none") + theme(plot.title=element_text(size=18, face="bold",color="black",lineheight=1.5, hjust = 0.4))  + theme(axis.title.x = element_text(color="white", size=14, face="bold"),
                                                                                                                                                                                                                                                                                                                                                                               axis.title.y = element_text(color="white", size=14, face="bold"))+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
#Product split by region
nest_sample<-fmarket[,c(11,30:50)]
nest_sample[nest_sample=="Y"] <-1

long_form<-gather(nest_sample, key="Typeofgoods",value=measurement,-State)
long_form
long_form$measurement<-as.numeric(long_form$measurement)

nested_example<-long_form %>%
  filter(measurement==1)%>%
  group_by(State,Typeofgoods)%>%
  summarise(Count=n())

long_form<-full_join(long_form,state_regions,by='State')
View(long_form)

nested_example<-long_form %>%
  filter(measurement==1)%>%
  group_by(State,Region,Typeofgoods)%>%
  drop_na() %>%
  summarise(Count=n())

nested_example

mw<-nested_example%>%filter(Region=="Midwest")%>%group_by("State")
mw
ne<-nested_example%>%filter(Region=="Northeast")%>%group_by("State")
sth<-nested_example%>%filter(Region=="South")%>%group_by("State")
wst<-nested_example%>%filter(Region=="West")%>%group_by("State")

q1<-ggplot(mw, aes(fill=Typeofgoods,reorder(x = State,-Count), y = Count)) + geom_bar(position="Stack", stat="identity")+
  ggtitle("Product Split in the Midwest")+
  xlab("Midwest States")+
  ylab("Count of Products")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) 
q2<-ggplot(ne, aes(fill=Typeofgoods,reorder(x = State,-Count), y = Count)) + geom_bar(position="Stack", stat="identity")+
  ggtitle("Product Split in the Northeast")+
  xlab("Northeast States")+
  ylab("Count of Products")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
q3<-ggplot(sth, aes(fill=Typeofgoods,reorder(x = State,-Count), y = Count)) + geom_bar(position="Stack", stat="identity")+
  ggtitle("Product Split in the South")+
  xlab("South States")+
  ylab("Count of Products")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) 
q4<-ggplot(wst, aes(fill=Typeofgoods,reorder(x = State,-Count), y = Count)) + geom_bar(position="Stack", stat="identity")+
  ggtitle("Product Split in the West")+
  xlab("West States")+
  ylab("Count of Products")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) 

q1
q2
q3
q4

#how do people pay
top<-fmarketorg %>%
  select(State,Credit,WIC,WICcash,SFMNP,SNAP)%>%
  na.omit()

D1<-melt(top,id.vars="State")
D1
lf_payment<-full_join(D1,state_regions,by='State')

#midwest
lf_payment_midwest<-lf_payment %>%
  filter(Region=="Midwest")%>%
  group_by(variable,State)%>%
  mutate(sum_total=sum(value), payment_type=variable)

ggplot(lf_payment_midwest, aes(x = State, y = sum_total, group=payment_type, color= payment_type)) + geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

#south
lf_payment_south<-lf_payment %>%
  filter(Region=="South")%>%
  group_by(variable,State)%>%
  mutate(sum_total=sum(value), payment_type=variable)

ggplot(lf_payment_south, aes(x = State, y = sum_total, group=payment_type, color= payment_type)) + geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

#northeast
lf_payment_northeast<-lf_payment %>%
  filter(Region=="Northeast")%>%
  group_by(variable,State)%>%
  mutate(sum_total=sum(value), payment_type=variable)

ggplot(lf_payment_northeast,aes(x = State, y = sum_total, group=payment_type, color= payment_type)) + geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

#west
lf_payment_west<-lf_payment %>%
  filter(Region=="West")%>%
  group_by(variable,State)%>%
  mutate(sum_total=sum(value), payment_type=variable)


ggplot(lf_payment_west, aes(reorder(x = State,-sum_total), y = sum_total, group=payment_type, color= payment_type)) + geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

par(mfrow=c(4,1))
p1<-ggplot(lf_payment_midwest, aes(x = State, y = sum_total, group=payment_type, color= payment_type)) + geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
p2<-ggplot(lf_payment_south, aes(x = State, y = sum_total, group=payment_type, color= payment_type)) + geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
p3<-ggplot(lf_payment_northeast,aes(x = State, y = sum_total, group=payment_type, color= payment_type)) + geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
p4<-ggplot(lf_payment_west, aes(x = State, y = sum_total, group=payment_type, color= payment_type)) + geom_line()+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)



#SFMNP
lf_payment_sfmnp<-D1 %>%
  filter(variable=="SFMNP")%>%
  filter(value==1)%>%
  group_by(State)%>%
  summarise(Count=n())

lf_payment_sfmnp

Sfmnp_top<-lf_payment_sfmnp %>%top_n(20)
Sfmnp_top
ggplot(Sfmnp_top, aes(x=reorder(State,-Count), y=Count)) + 
  geom_bar(stat = "identity",fill="lightblue") +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+geom_text(aes(label=Count),hjust = 1.1, size=4)+xlab("States")+ggtitle("Top 20 SFMNP Providing States ")

Sfmnp_bottom<-lf_payment_sfmnp %>%top_n(-20)
ggplot(Sfmnp_bottom, aes(x=reorder(State,Count), y=Count)) + 
  geom_bar(stat = "identity",fill="lightblue") +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+geom_text(aes(label=Count),hjust = 1.1, size=4)+xlab("States")+ggtitle("Bottom 20 SFMNP Providing States ")

#WIC
lf_payment_wic<-D1 %>%
  filter(variable=="WIC")%>%
  filter(value==1)%>%
  group_by(State)%>%
  summarise(Count=n())

lf_payment_wic

wic_top<-lf_payment_wic %>%top_n(20)
wic_top
ggplot(wic_top, aes(x=reorder(State,-Count), y=Count)) + 
  geom_bar(stat = "identity",fill="lightgreen") +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+geom_text(aes(label=Count),hjust = 1.1, size=4)+xlab("States")+ggtitle("Top 20 WIC Providing States ")

wic_bottom<-lf_payment_wic %>%top_n(-20)
ggplot(wic_bottom, aes(x=reorder(State,Count), y=Count)) + 
  geom_bar(stat = "identity",fill="lightgreen") +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+geom_text(aes(label=Count),hjust = 1.1, size=4)+xlab("States")+ggtitle("Bottom 20 WIC Providing States ")

#WICcash
lf_payment_wiccash<-D1 %>%
  filter(variable=="WICcash")%>%
  filter(value==1)%>%
  group_by(State)%>%
  summarise(Count=n())

lf_payment_wiccash

wiccash_top<-lf_payment_wiccash %>%top_n(20)
wiccash_top
ggplot(wiccash_top, aes(x=reorder(State,-Count), y=Count)) + 
  geom_bar(stat = "identity",fill="green") +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+geom_text(aes(label=Count),hjust = 1.1, size=4)+xlab("States")+ggtitle("Top 20 WICcash Providing States ")


wiccash_bottom<-lf_payment_wiccash %>%top_n(-20)
ggplot(wiccash_bottom, aes(x=reorder(State,Count), y=Count)) + 
  geom_bar(stat = "identity",fill="green") +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+geom_text(aes(label=Count),hjust = 1.1, size=4)+xlab("States")+ggtitle("Bottom 20 WICcash Providing States ")

#SNAP
lf_payment_SNAP<-D1 %>%
  filter(variable=="SNAP")%>%
  filter(value==1)%>%
  group_by(State)%>%
  summarise(Count=n())

lf_payment_SNAP

SNAP_top<-lf_payment_SNAP %>%top_n(20)
SNAP_top
ggplot(SNAP_top, aes(x=reorder(State,-Count), y=Count)) + 
  geom_bar(stat = "identity",fill="VIOLET") +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+geom_text(aes(label=Count),hjust = 1.1, size=4)+xlab("States")+ggtitle("Top 20 SNAP Providing States ")

SNAP_bottom<-lf_payment_SNAP %>%top_n(-20)
ggplot(SNAP_bottom, aes(x=reorder(State,Count), y=Count)) + 
  geom_bar(stat = "identity",fill="VIOLET") +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+geom_text(aes(label=Count),hjust = 1.1, size=4)+xlab("States")+ggtitle("Bottom 20 SNAP Providing States ")

#Credit
lf_payment_credit<-D1 %>%
  filter(variable=="Credit")%>%
  filter(value==1)%>%
  group_by(State)%>%
  summarise(Count=n())

lf_payment_credit

credit_top<-lf_payment_credit %>%top_n(10)
credit_top
ggplot(credit_top, aes(x=reorder(State,-Count), y=Count)) + 
  geom_bar(stat = "identity",fill="red") 





#products for SMNP


prod<-fmarketorg %>%
  select(State,Vegetables,Honey,Fruits,Herbs)%>%
  na.omit()


D11<-melt(prod,id.vars="State")
D12<-D11 %>%
  group_by(variable,State)%>%
  mutate(sum_total=sum(value), product_type=variable)



ggplot(D12, aes(fill=product_type,reorder(x = State,-sum_total), y = sum_total)) + geom_bar(position="dodge", stat="identity")+
  ggtitle("test") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

#products for SNAP
library(dplyr)
library(tidyr)
library(reshape2)
prodSNAP<-fmarketorg %>%
  select(State,Plants,Vegetables,Fruits,Poultry,Juices,Coffee,Eggs)%>%
  na.omit()


D13<-melt(prodSNAP,id.vars="State")
D13
D14<-D13 %>%
  group_by(variable,State)%>%
  mutate(sum_total=sum(value), product_type=variable)


ggplot(D14, aes(fill=product_type,reorder(x = State,-sum_total), y = sum_total)) + geom_bar(position="stack", stat="identity")+
  ggtitle("test") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))



#Region distribution of Framer's Market in the US
market_distribution<-fmarket[,8:11]
market_distribution

sum(is.na(market_distribution$County))
sum(is.na(market_distribution$city))
sum(is.na(market_distribution$State))
market_distribution$State <- as.character(market_distribution$State)
unique(market_distribution$State)

fmap<-full_join(market_distribution,state_regions,by='State')
View(fmap)

#Regional Distribution
temp1.1 <- plyr::count(fmap, "Region") %>% drop_na()

temp1.2 <- plyr::count(fmap, "State")                                                                               

colnames(temp1.2)[colnames(temp1.2)=="State"] <- "state"
library(usmap)
library(ggplot2)
plot_usmap(data = temp1.2, values = "freq", color = "purple",labels = T,label_color = "black")  +
  scale_fill_continuous( low = "lightblue", high = "red", name = "State Distribution", label = scales::comma ) +
  theme(legend.position = "right") + labs(title = "State Distribution of Farmer's Market")+
  theme(plot.title=element_text(size=16, face="bold",color="black",lineheight=2)) +
  theme(legend.position = c(0.9,0.1)) + theme(legend.title = element_text(colour="black", size=12, face = "bold" )) +
  theme(legend.background = element_rect(fill="lightsalmon", size=0.4, linetype="solid"))

ggplot(temp1.2, aes(x=reorder(factor(State), freq),y=freq, fill = State))+ geom_bar(stat="identity")  + coord_flip()+
  geom_text(aes(label=freq),hjust = 1.1, size=3) + theme_minimal()  + labs(title = "State distribution of Farmer's Market ") +
  xlab("States") + ylab("count") +theme(legend.position = "none")+
  theme(plot.title=element_text(size=14, face="bold",color="black",lineheight=1.5))+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))


ggplot(temp1.1, aes(x=reorder(factor(Region), freq),y=freq, fill = Region))+ geom_bar(stat="identity")  +
  geom_text(aes(label=freq),hjust = 1.1, size=4) + theme_minimal()  + labs(title = "Region distribution of Farmer's Market ") +
  xlab("Region") + ylab("count") +theme(legend.position = "none")+
  theme(plot.title=element_text(size=14, face="bold",color="black",lineheight=1.5))

#Divisional Distribution
temp1.3 <- plyr::count(fmap, "Division") %>% drop_na()

temp1.4 <- plyr::count(fmap, "State") 

ggplot(temp1.3, aes(x=reorder(factor(Division), freq),y=freq, fill = Division))+ geom_bar(stat="identity")  +
  geom_text(aes(label=freq),hjust = 1.1, size=4) + theme_minimal()  + labs(title = "Division distribution of Farmer's Market ") +
  xlab("Division") + ylab("count") +theme(legend.position = "none")+
  theme(plot.title=element_text(size=14, face="bold",color="black",lineheight=1.5))

