install.packages('rgbif')
install.packages('countrycode')
install.packages('sp')
install.packages('rgdal')
install.packages('CoordinateCleaner')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('elevatr')
install.packages('readr')
install.packages('magrittr')
install.packages('raster')
install.packages('maps')
install.packages('abjutils')
install.packages('flora')

#library(rgbif)
#library(countrycode)
library(sp)
library(rgdal)
#library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(tidyverse)
#library(elevatr)
library(readr)
#library(magrittr)
#library(raster)
#library(maps)
#library(abjutils)
library(flora)
library(vegan)

setwd('/Users/xbaroc/Desktop/Campos de altitude_papers/CA_January_2023/')

######################## Vouchers obtained in bacbonelists data ########################################

tot_2000<-read.csv("/Users/xbaroc/Desktop/Campos de altitude_papers/CA_January_2023/new_voucher2000.csv")
tot_2000$decimalLatitude=gsub(tot_2000$decimalLatitude, pattern = ',', replacement = '.')
tot_2000$decimalLongitude=gsub(tot_2000$decimalLongitude, pattern = ',', replacement = '.')

tot_1800<-read.csv("/Users/xbaroc/Desktop/Campos de altitude_papers/CA_January_2023/new_voucher1800.csv")
tot_1800$decimalLatitude=gsub(tot_1800$decimalLatitude, pattern = ',', replacement = '.')
tot_1800$decimalLongitude=gsub(tot_1800$decimalLongitude, pattern = ',', replacement = '.')

tot_1500<-read.csv("/Users/xbaroc/Desktop/Campos de altitude_papers/CA_January_2023/new_voucher1500.csv")
tot_1500$decimalLatitude=gsub(tot_1500$decimalLatitude, pattern = ',', replacement = '.')
tot_1500$decimalLongitude=gsub(tot_1500$decimalLongitude, pattern = ',', replacement = '.')

tot_1300<-read.csv("/Users/xbaroc/Desktop/Campos de altitude_papers/CA_January_2023/new_voucher1300.csv")
tot_1300$decimalLatitude=gsub(tot_1300$decimalLatitude, pattern = ',', replacement = '.')
tot_1300$decimalLongitude=gsub(tot_1300$decimalLongitude, pattern = ',', replacement = '.')



######################### Dividing papagaio into papagaio, mina and marins ###################################
papagaio_tot<-subset(tot_1800, grepl('PE Papagaio/Pedra da Mina/APA Mantiqueira', tot_1800$uc))

####Pedra furada pra cima e pra baixo
apsm<-data.frame(filter(papagaio_tot, papagaio_tot$decimalLatitude >= -22.365))

apsm_papagaio<-data.frame(filter(papagaio_tot, papagaio_tot$decimalLatitude < -22.365))
apsm_mina<-apsm[!(apsm$decimalLongitude > -45) ,]
apsm_marins<-apsm[!(apsm$decimalLongitude < -45) ,]

apsm_na<-subset(papagaio_tot, is.na(papagaio_tot$decimalLatitude))

apsm_na_papagaio<- subset(apsm_na, grepl("bocaina de minas|papagaio", apsm_na$locality, ignore.case = T))
apsm_papagaio<-rbind(apsm_papagaio,apsm_na_papagaio) 
apsm_papagaio$uc<-gsub(apsm_papagaio$uc, pattern='PE Papagaio/Pedra da Mina/APA Mantiqueira', replacement='PE Papagaio')

apsm_na_mina<- subset(apsm_na, grepl("mina|Pedra da mi|serra fina", apsm_na$locality, ignore.case = T))
apsm_mina<-rbind(apsm_mina,apsm_na_mina) 
apsm_mina$uc<-gsub(apsm_mina$uc, pattern='PE Papagaio/Pedra da Mina/APA Mantiqueira', replacement='Pedra da Mina')


apsm_na_marins<- subset(apsm_na, grepl("marins", apsm_na$locality, ignore.case = T))
apsm_marins<-rbind(apsm_marins,apsm_na_marins) 
apsm_marins$uc<-gsub(apsm_marins$uc, pattern='PE Papagaio/Pedra da Mina/APA Mantiqueira', replacement='Pico dos Marins')



################################### Verifying vouchers ############################################################
###### 2000
PNIT_tot<-subset(tot_2000, grepl('PARNA Itatiaia', tot_2000$uc))
PNCP_tot<-subset(tot_2000, grepl('PARNA Caparao', tot_2000$uc))

###### 1800
PNSO_tot<-subset(tot_1800, grepl('PARNA Serra dos Orgaos', tot_1800$uc))
PECJ_tot<-subset(tot_1800, grepl('PE Campos do Jordao', tot_1800$uc))
PETP_tot<-subset(tot_1800, grepl('PE Tres Picos', tot_1800$uc))
PESB_tot<-subset(tot_1800, grepl('PE Serra do Brigadeiro', tot_1800$uc))
PDMI_tot<- apsm_mina
PIMA_tot<- apsm_marins
PESP_tot<-apsm_papagaio

###### 1500
PESB_tot_1500<-subset(tot_1500, grepl('PE Serra do Brigadeiro', tot_1500$uc))
PNSB_tot<-subset(tot_1500, grepl('PARNA Bocaina', tot_1500$uc))
APAP_tot<-subset(tot_1500, grepl('APA Petropolis', tot_1500$uc))

###### 1300
PNSJ_tot<-subset(tot_1300, grepl('PARNA Sao Joaquim', tot_1300$uc))
PEPP_tot<-subset(tot_1300, grepl('PE Pico do Parana', tot_1300$uc))
ARAC_tot<-subset(tot_1300, grepl('Aracatuba', tot_1300$uc))


###EXTRAS
APAM_tot<-subset(tot_1500, grepl('APA Macae de Cima', tot_1500$uc))
PEDS_tot<-subset(tot_1500, grepl('PE Desengano', tot_1500$uc))

### species accumulation curve
total_vouchers<-rbind(APAP_tot,ARAC_tot,PDMI_tot,PECJ_tot,PEDS_tot,PEPP_tot,PESP_tot,PETP_tot,
                             PIMA_tot,PNCP_tot,PNIT_tot,PNSB_tot,PNSJ_tot,PNSO_tot,PESB_tot_1500)

total_vouchers_noPESB<-rbind(APAP_tot,ARAC_tot,PDMI_tot,PECJ_tot,PEDS_tot,PEPP_tot,PESP_tot,PETP_tot,
                             PIMA_tot,PNCP_tot,PNIT_tot,PNSB_tot,PNSJ_tot,PNSO_tot)

write.csv(total_vouchers_noPESB, file = "Vouchers_final.csv")
list1<-total_vouchers_noPESB[,c(11,34,54)]
str(list1)
list1$year<-as.factor(list1$year)
sac <- with(list1, specaccum(table(year, species), method = "random",permutations = 999))

plot(sac)
data <- data.frame(Sites=sac$sites, Richness=sac$richness, SD=sac$sd)

ggplot() +
  geom_point(data=data, aes(x=Sites, y=Richness)) +
  geom_line(data=data, aes(x=Sites, y=Richness)) +
  geom_ribbon(data=data ,aes(x=Sites,
                              ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2)

#plot total

pdf("SAC_total.pdf",width = 15,height = 15)
plot(sac, xlab = "Years", ylab = "Number of species", 
     main = "Total species-accumulation curve", xlim = c(0, 140), ylim = c(0, 2500))
dev.off()


# Split the data by "uc" column
data_by_uc <- split(list1, list1$uc)

# Apply specaccum() function to each group
sac_list <- lapply(data_by_uc, function(group) {
  specaccum(table(group$year, group$species), method = "random", permutations = 1000)
})

# Rename the list elements to match the "uc" values
names(sac_list) <- unique(list1$uc)

# Plot all curves on the same plot
colors_vec <- c("red", "gold", "navy", "orange", "purple", "brown", "salmon", "gray",
                     "green", "khaki", "tomato", "cyan", "rosybrown", "skyblue")
                     # Generate a gradient palette using viridis_pal()

pdf("SACs.pdf",width = 15,height = 15)
plot(sac_list[[1]], xlab = "Years", ylab = "Number of species", 
     main = "Species-accumulation curves", xlim = c(0, 140), ylim = c(-100, 800))
for (i in 2:length(sac_list)) {
  lines(sac_list[[i]], col = colors_vec[i])
}
legend("topleft", legend = names(sac_list), col = colors_vec[1:length(sac_list)], lty = 1, cex = 1)

dev.off()


