# leggo il file delle variabili ambientali

ENV <- read.csv("C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020-SEAwise\\__________________WP3\\Task 3.3\\ENV_data_GSA171819_(1987_2022).csv", sep=";")

# setto la cartella in cui salvo i risultati
wd <- "C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020-SEAwise\\__________________WP3\\Task 3.3"

# visualizzo i nomi delle colonne del data frame
colnames(ENV)

# visualizzo i dati della tabella
View(ENV)

# leggo la libbreria per effettuare le pivot
library(dplyr)


# riclassifico i dati di profondit? nella colonna 'strata'
ENV[ENV$depth >0 & ENV$depth <= 50, "strata"] <- "0-50"
ENV[ENV$depth >50 & ENV$depth <= 100, "strata"] <- "50-100"
ENV[ENV$depth >100 & ENV$depth <= 200, "strata"] <- "100-200"
ENV[ENV$depth >200 & ENV$depth <= 500, "strata"] <- "200-500"
ENV[ENV$depth >500 & ENV$depth <= 800, "strata"] <- "500-800"


# effettuo l'aggregazione dei dati ambientali a seconda delle esigenze (e.g. gsa, strata, year...)
tab1 <- ENV %>% group_by(GSA,strata,YEAR,MONTH) %>% summarise(sst=mean(sst,na.rm=TRUE),botT=mean(botT,na.rm=TRUE),so=mean(so,na.rm=TRUE),botso=mean(botso,na.rm=TRUE),nppv=mean(nppv,na.rm=TRUE))
# 
# 
# 
# 
# # hke17-18
# ENV2=ENV[ENV$GSA %in% c(17,18) & ENV$strata %in% c("50-100","100-200","200-500"),]
# 
# tab1 <- ENV2 %>% group_by(YEAR) %>% summarise(sst=mean(sst,na.rm=TRUE),botT=mean(botT,na.rm=TRUE),so=mean(so,na.rm=TRUE),botso=mean(botso,na.rm=TRUE),nppv=mean(nppv,na.rm=TRUE))

# salvo la tabella in un csv nella cartella dei risultati (wd)
#write.table(tab1,file.path(wd,"HKE1718_50-500_MONTH.csv"),sep=";",row.names=FALSE)




# dps17-18-19
# ENV2=ENV[ENV$GSA %in% c(17,18,19) & ENV$strata %in% c("50-100","100-200","200-500"),]
# 
# #ENV2=ENV[ENV$strata %in% c("50-100","100-200","200-500"),]
# 
# tab2 <- ENV2 %>% group_by(YEAR) %>% summarise(sst=mean(sst,na.rm=TRUE),botT=mean(botT,na.rm=TRUE),so=mean(so,na.rm=TRUE),botso=mean(botso,na.rm=TRUE),nppv=mean(nppv,na.rm=TRUE))

# salvo la tabella in un csv nella cartella dei risultati (wd)
#write.table(tab1,file.path(wd,"DPS171819_50-500_MONTH.csv"),sep=";",row.names=FALSE)



# HKE 19
ENV2=ENV[ENV$GSA==19 & ENV$strata %in% c("50-100","100-200","200-500"),]

env_var_HKE19 <- ENV2 %>% group_by(YEAR) %>% summarise(sst=mean(sst,na.rm=TRUE),botT=mean(botT,na.rm=TRUE),so=mean(so,na.rm=TRUE),botso=mean(botso,na.rm=TRUE),nppv=mean(nppv,na.rm=TRUE))

saveRDS(env_var_HKE19,"env_var_HKE19.RDS")


# MUT 19
ENV2=ENV[ENV$gsa==19 & ENV$strata %in% c("50-100","100-200","10-50"),]

env_var_MUT19 <- ENV2 %>% group_by(YEAR) %>% summarise(sst=mean(sst,na.rm=TRUE),botT=mean(botT,na.rm=TRUE),so=mean(so,na.rm=TRUE),botso=mean(botso,na.rm=TRUE),nppv=mean(nppv,na.rm=TRUE))

saveRDS(env_var_MUT19,"env_var_MUT19.RDS")


# ARS 18-19-20
ENV2=ENV[ENV$gsa %in% c(18,19,20) & ENV$strata %in% c("200-500","500-800"),]

env_var_ARSARA181920 <- ENV2 %>% group_by(YEAR) %>% summarise(sst=mean(sst,na.rm=TRUE),botT=mean(botT,na.rm=TRUE),so=mean(so,na.rm=TRUE),botso=mean(botso,na.rm=TRUE),nppv=mean(nppv,na.rm=TRUE))

saveRDS(env_var_ARSARA181920,"env_var_ARS_ARA181920.RDS")

