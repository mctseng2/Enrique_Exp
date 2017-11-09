library(dplyr)
library(ggplot2)
library(stringr)
library(plotrix)
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("stringr")
#install.packages("plotrix")

setwd("C:/Users/Meng-Chun/OneDrive - University of Illinois - Urbana/Thesis/uruguay/Enrique_Exp/analysis/Aug23/") #home
setwd("C:/Users/mctseng2/OneDrive - University of Illinois - Urbana/Thesis/uruguay/Enrique_Exp/analysis/Aug23/") #office


#read seprate data
Exp_parameter= read.csv("Data_all.csv", stringsAsFactors=FALSE)
Yield_data= read.csv("Yield_data.csv", stringsAsFactors=FALSE)
Pesticide_info = read.csv("Pesticide_info.csv")

#join together
combined_data = left_join(Yield_data,Exp_parameter)
#Joining, by = c("Location", "Trt", "Year")

#factor the first 4 groups
for(i in 1:4)
  combined_data[,i] = as.factor(combined_data[,i])

#calculate NUE
combined_data$NUE = combined_data$Yield*1000/combined_data$N_rate

#claculate the total amount of pesticide Use
note = c(H="Herbicide",I="Insecticide",F="Fungicide")

pesticide_quan <- data.frame(Herbicide=NA,Insecticide=NA,Fungicide=NA)
for(i in seq(nrow(combined_data))){
  yr = combined_data[i,]$Year
  loc = combined_data[i,]$Location
  trt = as.character(combined_data[i,]$Trt)
  
  for(j in c("H","F","I")){
    sub_Pest_info = filter(Pesticide_info,Year==yr,Location==loc,Type==j)
    if(nrow(sub_Pest_info)==0)
      pesticide_quan[i,note[j]] = NA
    else{
      sum_index = sapply(str_split(sub_Pest_info$Treatment,","),function(x){sum(x==trt)==1})
      total_quan = sum(sub_Pest_info[sum_index,"ai"])
      pesticide_quan[i,note[j]] = total_quan
    }
    }
  }

combined_data <- cbind(combined_data,pesticide_quan)

#calculate total pesticide risk based on treatments
pesticide_risk <- data.frame(Herbicide_risk=NA,Insecticide_risk=NA,Fungicide_risk=NA)
for(i in seq(nrow(combined_data))){
  yr = combined_data[i,]$Year
  loc = combined_data[i,]$Location
  trt = as.character(combined_data[i,]$Trt)
  
  for(j in c("H","F","I")){
    sub_Pest_info = filter(Pesticide_info,Year==yr,Location==loc,Type==j)
    if(nrow(sub_Pest_info)==0)
      pesticide_risk[i,paste0(note[j],"_risk")] = NA
    else{
      sum_index = sapply(str_split(sub_Pest_info$Treatment,","),function(x){sum(x==trt)==1})
      total_risk = sum(sub_Pest_info[sum_index,"Net_impact"])
      pesticide_risk[i,paste0(note[j],"_risk")] <-  total_risk
    }
  }
}

#combining data
combined_data <- cbind(combined_data,pesticide_risk)

#just to check for potential errors
write.table(combined_data,"test.csv",sep=",",row.names = F)




#entering energy and GHG conversion factors
#units are MJ/kg
Energy = c(Diesel=47.7,N=63.5,P=13.95,K=6.69,Herbicide=303.8,Insecticide=418.4,Fungicide=115,Seed=15.5,Yield=15.2)
#all units are CO2e/kg
CO2 = c(Diesel=3.2,N=5,P=1.77,K=0.69,Herbicide=303.8*.069,Insecticide=418.4*.069,Fungicide=115*.069,Seed=0.7)

save.image("RTR_1011.RData")

attach(combined_data)
  
energy= data.frame(
                  Seed_Energy = Seed_Rate*Energy["Seed"],
                  N_Energy = N_rate*Energy["N"],
                  P_Energy = P_rate*Energy["P"],
                  K_Energy = K_rate*Energy["K"],
                  Fungicide_Energy = Fungicide*Energy["Fungicide"],
                  Herbicide_Energy = Herbicide*Energy["Herbicide"],
                  Insecticide_Energy = Insecticide*Energy["Insecticide"],
                  Tillage_Energy = (Diesel_landPrep+Disel_pestcide+Disel_fertilizer)*Energy["Diesel"],
                  energy_yield = Yield*1000*Energy["Yield"])

CO2 = data.frame(
                  Seed_CO2 = Seed_Rate*CO2["Seed"],
                  N_CO2 = N_rate*CO2["N"],
                  P_CO2 = P_rate*CO2["P"],
                  K_CO2 = K_rate*CO2["K"],
                  Fungicide_CO2 = Fungicide*CO2["Fungicide"],
                  Herbicide_CO2 = Herbicide*CO2["Herbicide"],
                  Insecticide_CO2 = Insecticide*CO2["Insecticide"],
                  Tillage_CO2 = (Diesel_landPrep+Disel_pestcide+Disel_fertilizer)*CO2["Diesel"]
                )
detach(combined_data)

combined_data <- cbind(combined_data,CO2)
combined_data <- cbind(combined_data,energy)

save.image("RTR_1011.RData")

#calulate energy use efficiency
NEY <- (combined_data$energy_yield-rowSums(energy[,1:8]))/1e3  #GJ/Ha
hist(NEY)
EUE <- (combined_data$Yield*1000)/rowSums(energy[,1:8]) #kg_rice/MJ energy
hist(EUE)

#calculate total C footprint
total_c_footprint <- apply(CO2,1,sum)
hist(total_c_footprint)
summary(total_c_footprint)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   667.9   847.3   878.5   915.0  1018.0  1142.0      30 
yield_scaled_c_footprint <- total_c_footprint/(combined_data$Yield*1000)
hist(yield_scaled_c_footprint)
summary(yield_scaled_c_footprint)
# Min.    1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.05648 0.06518 0.07541 0.07784 0.08704 0.12690      30 
##########################################################################################

#calculate total AGC contamination risk
total_agc_risk <- apply(pesticide_risk,1,sum)
hist(total_agc_risk)
yield_scaled_agc_risk <- total_agc_risk/(combined_data$Yield*1000) #PAF m3/kg rice
hist(yield_scaled_agc_risk)



##calculate input related env & social cost
#include C02 (From Hill et al., 2009 PNAS) and Pesticide (Env, Dev and Sustainability (2005) 7: 229-252)
#carbon: $120/ton, pesticide: $20/kg

ES_cost<- cbind(pesticide_quan*20, CO2*0.12)  #usd20 per kg pesticide , USD 0.12/kg CO2
colnames(ES_cost) <- paste0(colnames(ES_cost),".cost")
Input_EnvSoc_cost <- rowSums(ES_cost)
yield_scaled_Input_EnvSoc_cost <- Input_EnvSoc_cost/(combined_data$Yield*1000)



#put all calculated indicators into a new df
output <- data.frame(Yield=combined_data$Yield,NUE=combined_data$NUE,EUE,NEY,
                     total_c_footprint,yield_scaled_c_footprint,total_agc_risk,
                     yield_scaled_agc_risk, Input_EnvSoc_cost, yield_scaled_Input_EnvSoc_cost)
#adding tags
output <- cbind(combined_data[,1:4],output)


#putting letters
levels(output$Location) = c("Ricon de Ramirez","Cebollati","India Muerta","Treinta y Tres/San Francisco")

colnames(output)
# [1] "Location"                       "Trt"                            "Block"                          "Year"                           "Yield"                         
# [6] "NUE"                            "EUE"                            "NEY"                            "total_c_footprint"              "yield_scaled_c_footprint"      
# [11] "total_agc_risk"                 "yield_scaled_agc_risk"          "Input_EnvSoc_cost"              "yield_scaled_Input_EnvSoc_cost"

write.table(output,"exp_indicators.csv",sep=",",row.names = F)
#aggrigate data by year, location and treatment
Exp_data_summary = group_by(output,Location,Trt,Year) %>%
  summarise(se_Yield=std.error(Yield), Yield=mean(Yield,na.rm=F), se_NUE=std.error(NUE), NUE=mean(NUE),
            se_EUE=std.error(EUE), EUE=mean(EUE), se_NEY=std.error(NEY), NEY=mean(NEY),
            se_total_c_footprint=std.error(total_c_footprint), total_c_footprint=mean(total_c_footprint,na.rm=F),
            se_yield_scaled_c_footprint=std.error(yield_scaled_c_footprint), yield_scaled_c_footprint=mean(yield_scaled_c_footprint,na.rm=F),
            se_total_agc_risk=std.error(total_agc_risk), total_agc_risk=mean(total_agc_risk,na.rm=F),
            se_yield_scaled_agc_risk=std.error(yield_scaled_agc_risk), yield_scaled_agc_risk=mean(yield_scaled_agc_risk, na.rm=F),
            se_Input_EnvSoc_cost=std.error(Input_EnvSoc_cost), Input_EnvSoc_cost=mean(Input_EnvSoc_cost, na.rm=F),
            se_yield_scaled_Input_EnvSoc_cost=std.error(yield_scaled_Input_EnvSoc_cost), yield_scaled_Input_EnvSoc_cost=mean(yield_scaled_Input_EnvSoc_cost, na.rm=F),
            n=n())

#################################
##stats and graphing

#grouping ratio
options(na.rm=T)
#overall across 4 loc, 2yr
Exp_Sum.all = group_by(output,Trt) %>%
  summarise(se_Yield=std.error(Yield),Yield=mean(Yield,na.rm=T),se_NUE=std.error(NUE),NUE=mean(NUE),
            se_EUE=std.error(EUE),EUE=mean(EUE,na.rm=T),se_NEY=std.error(NEY),NEY=mean(NEY,na.rm=T),
            se_total_c_footprint=std.error(total_c_footprint),total_c_footprint=mean(total_c_footprint,na.rm=T),
            se_yield_scaled_c_footprint=std.error(yield_scaled_c_footprint),yield_scaled_c_footprint=mean(yield_scaled_c_footprint,na.rm=T),
            se_total_agc_risk=std.error(total_agc_risk),total_agc_risk=mean(total_agc_risk,na.rm=T),
            se_yield_scaled_agc_risk=std.error(yield_scaled_agc_risk),yield_scaled_agc_risk=mean(yield_scaled_agc_risk,na.rm=T),
            se_Input_EnvSoc_cost=std.error(Input_EnvSoc_cost), Input_EnvSoc_cost=mean(Input_EnvSoc_cost, na.rm=T),
            se_yield_scaled_Input_EnvSoc_cost=std.error(yield_scaled_Input_EnvSoc_cost), yield_scaled_Input_EnvSoc_cost=mean(yield_scaled_Input_EnvSoc_cost, na.rm=T),
            n=n())

ref <- Exp_Sum.all[c(1,7),-grep("se",colnames(Exp_Sum.all))] #this is our baseline
Exp_Sum.all.nomalized <-Exp_Sum.all[,-grep("se",colnames(Exp_Sum.all))] #this is total data without se

for(i in seq(nrow(Exp_Sum.all))){
    for(j in 2:(ncol(Exp_Sum.all[c(1,7),-grep("se",colnames(Exp_Sum.all))]))) {
    Exp_Sum.all.nomalized[i,j] <- (Exp_Sum.all.nomalized[i,j]/ref[ifelse(i<=6,1,2),j])-1
   
    }} #normalization

colnames(Exp_Sum.all.nomalized) <- paste0(colnames(Exp_Sum.all.nomalized),"_change")
out <- cbind(Exp_Sum.all[,-grep("se",colnames(Exp_Sum.all))],Exp_Sum.all.nomalized)
out[,order(colnames(out))]%>%write.table("Means_all_yr_loc.csv",sep=",",row.names = F)


#year 1 and 2

Exp_Sum.yr1_2 = group_by(output,Trt,Year) %>%
  summarise(se_Yield=std.error(Yield),Yield=mean(Yield,na.rm=T),se_NUE=std.error(NUE),NUE=mean(NUE),
            se_EUE=std.error(EUE),EUE=mean(EUE,na.rm=T),se_NEY=std.error(NEY),NEY=mean(NEY,na.rm=T),
            se_total_c_footprint=std.error(total_c_footprint),total_c_footprint=mean(total_c_footprint,na.rm=T),
            se_yield_scaled_c_footprint=std.error(yield_scaled_c_footprint),yield_scaled_c_footprint=mean(yield_scaled_c_footprint,na.rm=T),
            se_total_agc_risk=std.error(total_agc_risk),total_agc_risk=mean(total_agc_risk,na.rm=T),
            se_yield_scaled_agc_risk=std.error(yield_scaled_agc_risk),yield_scaled_agc_risk=mean(yield_scaled_agc_risk,na.rm=T),
            se_Input_EnvSoc_cost=std.error(Input_EnvSoc_cost), Input_EnvSoc_cost=mean(Input_EnvSoc_cost, na.rm=F),
            n=n())

#output yr n

for(n in 1:2){
  Exp_Sum.yrn <- Exp_Sum.yr1_2[Exp_Sum.yr1_2$Year==n,]
  ref <- Exp_Sum.yrn [c(1,7),-grep("se",colnames(Exp_Sum.yrn))]
  Exp_Sum.yrn.nomalized <-Exp_Sum.yrn[,-grep("se",colnames(Exp_Sum.yrn))]
  
  
  for(i in seq(nrow(Exp_Sum.yrn))){
    for(j in 2:(ncol(Exp_Sum.yrn[,-grep("se",colnames(Exp_Sum.yr1_2))]))){
      Exp_Sum.yrn.nomalized[i,j] <- (Exp_Sum.yrn.nomalized[i,j]/ref[ifelse(i<=6,1,2),j])-1
      }}
  
  colnames(Exp_Sum.yrn.nomalized) <- paste0(colnames(Exp_Sum.yrn.nomalized),"_change")
  out <- cbind(Exp_Sum.yrn[,-grep("se",colnames(Exp_Sum.yrn))],Exp_Sum.yrn.nomalized)
  out[,order(colnames(out))]%>%write.table(paste0("Means_all_yr_loc_yr",n,".csv"),sep=",",row.names = F)

}




#######################################

for(i in ncol(Exp_Sum2[,-grep("se",colnames(Exp_Sum2))])){
  filter(Trt==1) %>%
            select(Location,Year,Yield) %>%
            rename(Ref_Yield=Yield) %>%
            right_join(Exp_data_summary) %>%
            mutate(Yield_percent=(Yield/Ref_Yield)*100-100) 
}
            

p = ggplot(Exp_Sum2,aes(x=Trt,y=Yield_percent,fill=Year))
p+geom_bar(stat="identity",position="dodge")


Exp_Sum2 = ungroup(Exp_data_summary) %>%
  filter(Trt==1) %>%
  select(Location,Year,Yield) %>%
  rename(Ref_Yield=Yield) %>%
  right_join(Exp_data_summary) %>%
  mutate(Yield_percent=(Yield/Ref_Yield)*100-100) 

p = ggplot(Exp_Sum2,aes(x=Trt,y=Yield_percent,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
    facet_wrap(~Location)

Exp_Sum2 = ungroup(Exp_data_summary) %>%
  filter(Trt==1) %>%
  select(Location,Year,NUE) %>%
  rename(Ref_NUE=NUE) %>%
  right_join(Exp_data_summary) %>%
  mutate(NUE_percent=(NUE/Ref_NUE)*100-100) 

p = ggplot(Exp_Sum2,aes(x=Trt,y=NUE_percent,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  facet_wrap(~Location)


Exp_Sum2 = ungroup(Exp_data_summary) %>%
  filter(Trt==1) %>%
  select(Location,Year,EUE) %>%
  rename(Ref_EUE=EUE) %>%
  right_join(Exp_data_summary) %>%
  mutate(EUE_percent=(EUE/Ref_EUE)*100-100) 

p = ggplot(Exp_Sum2,aes(x=Trt,y=EUE_percent,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  facet_wrap(~Location)



Exp_Sum2 = ungroup(Exp_data_summary) %>%
  filter(Trt==1) %>%
  select(Location,Year,yield_scaled_c_footprint) %>%
  rename(Ref_yield_scaled_c_footprint=yield_scaled_c_footprint) %>%
  right_join(Exp_data_summary) %>%
  mutate(yield_scaled_c_footprint_percent=(yield_scaled_c_footprint/Ref_yield_scaled_c_footprint)*100-100) 

p = ggplot(Exp_Sum2,aes(x=Trt,y=yield_scaled_c_footprint_percent,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  facet_wrap(~Location)



Exp_Sum2 = ungroup(Exp_data_summary) %>%
  filter(Trt==1) %>%
  select(Location,Year,yield_scaled_agc_risk) %>%
  rename(Ref_yield_scaled_agc_risk=yield_scaled_agc_risk) %>%
  right_join(Exp_data_summary) %>%
  mutate(yield_scaled_agc_risk_percent=(yield_scaled_agc_risk/Ref_yield_scaled_agc_risk)*100-100) 

p = ggplot(Exp_Sum2,aes(x=Trt,y=yield_scaled_agc_risk_percent,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  facet_wrap(~Location)



#Barplots

# Barplot yield
p = ggplot(Exp_data_summary,aes(x=Trt,y=Yield,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=Yield-se_Yield,ymax=Yield+se_Yield),position =position_dodge(0.9),width=.2)+
  facet_wrap(~Location)

# Barplot NUE
p = ggplot(Exp_data_summary,aes(x=Trt,y=NUE,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=NUE-se_NUE,ymax=NUE+se_NUE),position =position_dodge(0.9),width=.2)+
  facet_wrap(~Location)


# #N_rate
# p = ggplot(Exp_data_summary,aes(x=Trt,y=N_rate,fill=Year))
# p+geom_bar(stat="identity",position="dodge")+
#   facet_wrap(~Location)

# Barplot EUE
p = ggplot(Exp_data_summary,aes(x=Trt,y=EUE,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=EUE-se_EUE,ymax=EUE+se_EUE),position =position_dodge(0.9),width=.2)+
  facet_wrap(~Location)

# Barplot yield_scaled_c_footprint
p = ggplot(Exp_data_summary,aes(x=Trt,y=yield_scaled_c_footprint,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=yield_scaled_c_footprint-se_yield_scaled_c_footprint,ymax=yield_scaled_c_footprint+se_yield_scaled_c_footprint),position =position_dodge(0.9),width=.2)+
  facet_wrap(~Location)


#yield_scaled_agc_risk

p = ggplot(Exp_data_summary,aes(x=Trt,y=yield_scaled_agc_risk,fill=Year))
p+geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=yield_scaled_agc_risk-se_yield_scaled_agc_risk,ymax=yield_scaled_agc_risk+se_yield_scaled_agc_risk),position =position_dodge(0.9),width=.2)+
  facet_wrap(~Location)




