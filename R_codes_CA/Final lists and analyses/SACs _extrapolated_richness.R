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
library(iNEXT)

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

library(dplyr)

x<-total_vouchers_noPESB %>%
  group_by(uc) %>%
  summarise(num_species = n_distinct(species))









#Estimating richness
#Total

count_df <- total_vouchers%>%
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df <- count_df %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df) <- NULL
pa_df <- pa_df[,-1]


#get richness estimators (for each sample, cumulative)
poolaccum_tot <- poolaccum(pa_df)
specpool_tot <- specpool(pa_df)
specpool_PNIT
#plot all: obs richness and  estimators
plot(poolaccum_tot)

#build the species accumulation curve & rarefaction curve (expected)
specaccum_tot <- specaccum(pa_df,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_tot,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_tot2 <- specaccum(pa_df, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_tot,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_tot2, col="yellow", add=TRUE, pch="+")


#PNIT

count_df_PNIT <- total_vouchers %>%
  filter(uc == "PARNA Itatiaia") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PNIT <- count_df_PNIT %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PNIT) <- NULL
pa_df_PNIT <- pa_df_PNIT[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PNIT <- poolaccum(pa_df_PNIT)
specpool_PNIT <- specpool(pa_df_PNIT)

#plot all: obs richness and  estimators
plot(poolaccum_PNIT)
specpool_PNIT
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PNIT <- specaccum(pa_df_PNIT,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PNIT,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PNIT2 <- specaccum(pa_df_PNIT, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PNIT,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PNIT2, col="yellow", add=TRUE, pch="+")



#APAP

count_df_APAP <- total_vouchers %>%
  filter(uc == "APA Petropolis") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_APAP <- count_df_APAP %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_APAP) <- NULL
pa_df_APAP <- pa_df_APAP[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_APAP <- poolaccum(pa_df_APAP)
specpool_APAP <- specpool(pa_df_APAP)

#plot all: obs richness and  estimators
plot(poolaccum_APAP)
specpool_APAP
#build the species accumulation curve & rarefaction curve (expected)
specaccum_APAP <- specaccum(pa_df_APAP,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_APAP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_APAP2 <- specaccum(pa_df_APAP, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_APAP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_APAP2, col="yellow", add=TRUE, pch="+")






#ARAC

count_df_ARAC <- total_vouchers %>%
  filter(uc == "Serra do Aracatuba") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_ARAC <- count_df_ARAC %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_ARAC) <- NULL
pa_df_ARAC <- pa_df_ARAC[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_ARAC <- poolaccum(pa_df_ARAC)
specpool_ARAC <- specpool(pa_df_ARAC)

#plot all: obs richness and  estimators
plot(poolaccum_ARAC)
specpool_ARAC
#build the species accumulation curve & rarefaction curve (expected)
specaccum_ARAC <- specaccum(pa_df_ARAC,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_ARAC,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_ARAC2 <- specaccum(pa_df_ARAC, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_ARAC,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_ARAC2, col="yellow", add=TRUE, pch="+")



#PDMI

count_df_PDMI <- total_vouchers %>%
  filter(uc == "Pedra da Mina") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PDMI <- count_df_PDMI %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PDMI) <- NULL
pa_df_PDMI <- pa_df_PDMI[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PDMI <- poolaccum(pa_df_PDMI)
specpool_PDMI <- specpool(pa_df_PDMI)

#plot all: obs richness and  estimators
plot(poolaccum_PDMI)
specpool_PDMI
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PDMI <- specaccum(pa_df_PDMI,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PDMI,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PDMI2 <- specaccum(pa_df_PDMI, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PDMI,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PDMI2, col="yellow", add=TRUE, pch="+")



#PECJ

count_df_PECJ <- total_vouchers %>%
  filter(uc == "PE Campos do Jordao") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PECJ <- count_df_PECJ %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PECJ) <- NULL
pa_df_PECJ <- pa_df_PECJ[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PECJ <- poolaccum(pa_df_PECJ)
specpool_PECJ <- specpool(pa_df_PECJ)

#plot all: obs richness and  estimators
plot(poolaccum_PECJ)
specpool_PECJ
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PECJ <- specaccum(pa_df_PECJ,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PECJ,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PECJ2 <- specaccum(pa_df_PECJ, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PECJ,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PECJ2, col="yellow", add=TRUE, pch="+")



#PEDS

count_df_PEDS <- total_vouchers %>%
  filter(uc == "PE Desengano") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PEDS <- count_df_PEDS %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PEDS) <- NULL
pa_df_PEDS <- pa_df_PEDS[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PEDS <- poolaccum(pa_df_PEDS)
specpool_PEDS <- specpool(pa_df_PEDS)

#plot all: obs richness and  estimators
plot(poolaccum_PEDS)
specpool_PEDS
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PEDS <- specaccum(pa_df_PEDS,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PEDS,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PEDS2 <- specaccum(pa_df_PEDS, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PEDS,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PEDS2, col="yellow", add=TRUE, pch="+")



#PEPP

count_df_PEPP <- total_vouchers %>%
  filter(uc == "PE Pico do Parana") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PEPP <- count_df_PEPP %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PEPP) <- NULL
pa_df_PEPP <- pa_df_PEPP[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PEPP <- poolaccum(pa_df_PEPP)
specpool_PEPP <- specpool(pa_df_PEPP)

#plot all: obs richness and  estimators
plot(poolaccum_PEPP)
specpool_PEPP
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PEPP <- specaccum(pa_df_PEPP,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PEPP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PEPP2 <- specaccum(pa_df_PEPP, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PEPP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PEPP2, col="yellow", add=TRUE, pch="+")



#PESP

count_df_PESP <- total_vouchers %>%
  filter(uc == "PE Papagaio") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PESP <- count_df_PESP %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PESP) <- NULL
pa_df_PESP <- pa_df_PESP[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PESP <- poolaccum(pa_df_PESP)
specpool_PESP <- specpool(pa_df_PESP)

#plot all: obs richness and  estimators
plot(poolaccum_PESP)
specpool_PESP
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PESP <- specaccum(pa_df_PESP,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PESP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PESP2 <- specaccum(pa_df_PESP, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PESP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PESP2, col="yellow", add=TRUE, pch="+")



#PETP

count_df_PETP <- total_vouchers %>%
  filter(uc == "PE Tres Picos") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PETP <- count_df_PETP %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PETP) <- NULL
pa_df_PETP <- pa_df_PETP[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PETP <- poolaccum(pa_df_PETP)
specpool_PETP <- specpool(pa_df_PETP)

#plot all: obs richness and  estimators
plot(poolaccum_PETP)
specpool_PETP
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PETP <- specaccum(pa_df_PETP,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PETP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PETP2 <- specaccum(pa_df_PETP, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PETP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PETP2, col="yellow", add=TRUE, pch="+")



#PIMA

count_df_PIMA <- total_vouchers %>%
  filter(uc == "Pico dos Marins") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PIMA <- count_df_PIMA %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PIMA) <- NULL
pa_df_PIMA <- pa_df_PIMA[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PIMA <- poolaccum(pa_df_PIMA)
specpool_PIMA <- specpool(pa_df_PIMA)

#plot all: obs richness and  estimators
plot(poolaccum_PIMA)
specpool_PIMA
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PIMA <- specaccum(pa_df_PIMA,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PIMA,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PIMA2 <- specaccum(pa_df_PIMA, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PIMA,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PIMA2, col="yellow", add=TRUE, pch="+")



#PNCP

count_df_PNCP <- total_vouchers %>%
  filter(uc == "PARNA Caparao") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PNCP <- count_df_PNCP %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PNCP) <- NULL
pa_df_PNCP <- pa_df_PNCP[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PNCP <- poolaccum(pa_df_PNCP)
specpool_PNCP <- specpool(pa_df_PNCP)

#plot all: obs richness and  estimators
plot(poolaccum_PNCP)
specpool_PNCP
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PNCP <- specaccum(pa_df_PNCP,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PNCP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PNCP2 <- specaccum(pa_df_PNCP, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PNCP,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PNCP2, col="yellow", add=TRUE, pch="+")



#PNSB

count_df_PNSB <- total_vouchers %>%
  filter(uc == "PARNA Bocaina") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PNSB <- count_df_PNSB %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PNSB) <- NULL
pa_df_PNSB <- pa_df_PNSB[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PNSB <- poolaccum(pa_df_PNSB)
specpool_PNSB <- specpool(pa_df_PNSB)

#plot all: obs richness and  estimators
plot(poolaccum_PNSB)
specpool_PNSB
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PNSB <- specaccum(pa_df_PNSB,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PNSB,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PNSB2 <- specaccum(pa_df_PNSB, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PNSB,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PNSB2, col="yellow", add=TRUE, pch="+")



#PNSJ

count_df_PNSJ <- total_vouchers %>%
  filter(uc == "PARNA Sao Joaquim") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PNSJ <- count_df_PNSJ %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PNSJ) <- NULL
pa_df_PNSJ <- pa_df_PNSJ[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PNSJ <- poolaccum(pa_df_PNSJ)
specpool_PNSJ <- specpool(pa_df_PNSJ)

#plot all: obs richness and  estimators
plot(poolaccum_PNSJ)
specpool_PNSJ
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PNSJ <- specaccum(pa_df_PNSJ,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PNSJ,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PNSJ2 <- specaccum(pa_df_PNSJ, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PNSJ,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PNSJ2, col="yellow", add=TRUE, pch="+")



#PNSO

count_df_PNSO <- total_vouchers %>%
  filter(uc == "PARNA Serra dos Orgaos") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PNSO <- count_df_PNSO %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PNSO) <- NULL
pa_df_PNSO <- pa_df_PNSO[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PNSO <- poolaccum(pa_df_PNSO)
specpool_PNSO <- specpool(pa_df_PNSO)

#plot all: obs richness and  estimators
plot(poolaccum_PNSO)
specpool_PNSO
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PNSO <- specaccum(pa_df_PNSO,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PNSO,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PNSO2 <- specaccum(pa_df_PNSO, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PNSO,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PNSO2, col="yellow", add=TRUE, pch="+")



#PESB

count_df_PESB <- total_vouchers %>%
  filter(uc == "PE Serra do Brigadeiro") %>% 
  group_by(year, species) %>%
  summarize(count = n()) %>%
  ungroup()

# Reshape the data frame to create a presence/absence and species count matrix
pa_df_PESB <- count_df_PESB %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>%
  mutate(Total = rowSums(select(., -year)))

rownames(pa_df_PESB) <- NULL
pa_df_PESB <- pa_df_PESB[,-1]

#get richness estimators (for each sample, cumulative)
poolaccum_PESB <- poolaccum(pa_df_PESB)
specpool_PESB <- specpool(pa_df_PESB)

#plot all: obs richness and  estimators
plot(poolaccum_PESB)
specpool_PESB
#build the species accumulation curve & rarefaction curve (expected)
specaccum_PESB <- specaccum(pa_df_PESB,method = "rarefaction")

#plot the curve with some predefined settings
plot(specaccum_PESB,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
specaccum_PESB2 <- specaccum(pa_df_PESB, "random")
#plot both curves ("observed" vs "randomized")
plot(specaccum_PESB,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(specaccum_PESB2, col="yellow", add=TRUE, pch="+")




pdf("SACs_new.pdf",width = 25,height = 15)

par(mfrow=c(1,2)) # set up a 2-row, 1-column plot layout
plot(specaccum_tot,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",  xlab="Years", ylab="Number of species")
boxplot(specaccum_tot2, col="yellow", add=TRUE, pch="+")
plot(specaccum_PNIT,ci.type="poly", col="red", lwd=2, ci.lty=0, ci.col="pink",
     xlab="Years", ylab="Number of species")
plot(specaccum_PNSJ,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", add=TRUE)
plot(specaccum_PNSB,ci.type="poly", col="darkgreen", lwd=2, ci.lty=0, ci.col="lightgreen", add=TRUE)
plot(specaccum_PNCP,ci.type="poly", col="purple", lwd=2, ci.lty=0, ci.col="lavender", add=TRUE)
plot(specaccum_PEDS,ci.type="poly", col="orange", lwd=2, ci.lty=0, ci.col="lightcoral", add=TRUE)
plot(specaccum_PNSO,ci.type="poly", col="magenta", lwd=2, ci.lty=0, ci.col="plum", add=TRUE)
plot(specaccum_ARAC,ci.type="poly", col="darkcyan", lwd=2, ci.lty=0, ci.col="lightcyan", add=TRUE)
plot(specaccum_APAP,ci.type="poly", col="deeppink", lwd=2, ci.lty=0, ci.col="pink", add=TRUE)
plot(specaccum_PESP,ci.type="poly", col="darkorchid", lwd=2, ci.lty=0, ci.col="violet", add=TRUE)
plot(specaccum_PECJ,ci.type="poly", col="plum", lwd=2, ci.lty=0, ci.col="thistle", add=TRUE)
plot(specaccum_PDMI,ci.type="poly", col="turquoise", lwd=2, ci.lty=0, ci.col="paleturquoise", add=TRUE)
plot(specaccum_PIMA,ci.type="poly", col="darkgoldenrod", lwd=2, ci.lty=0, ci.col="gold", add=TRUE)
plot(specaccum_PEPP,ci.type="poly", col="chocolate", lwd=2, ci.lty=0, ci.col="burlywood", add=TRUE)
plot(specaccum_PETP,ci.type="poly", col="navy", lwd=2, ci.lty=0, ci.col="lightblue", add=TRUE)
plot(specaccum_PESB,ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="gray", add=TRUE)

legend("topleft", c("PNIT", "PNSJ", "PNSB", "PNCP", "PEDS", "PNSO", "ARAC", "APAP", "PESP", "PECJ", "PDMI", "PIMA", "PEPP", "PETP", "PESB"), 
       col=c("red", "blue", "green", "purple", "orange", "magenta", "darkcyan", "deeppink", "darkorchid", "plum", "turquoise", "darkgoldenrod", "chocolate", "navy", "black"), 
       lty=1, cex=0.8)

dev.off()


specpool_tot$Sites <- "All"
specpool_PNIT$Sites <- "PNIT"
specpool_APAP$Sites <- "APAP"
specpool_ARAC$Sites <- "ARAC"
specpool_PDMI$Sites <- "PDMI"
specpool_PECJ$Sites <- "PECJ"
specpool_PEDS$Sites <- "PEDS"
specpool_PEPP$Sites <- "PEPP"
specpool_PESP$Sites <- "PESP"
specpool_PETP$Sites <- "PETP"
specpool_PIMA$Sites <- "PIMA"
specpool_PNCP$Sites <- "PNCP"
specpool_PNSB$Sites <- "PNSB"
specpool_PNSJ$Sites <- "PNSJ"
specpool_PNSO$Sites <- "PNSO"
specpool_PESB$Sites <- "PESB"

specpool_final <- rbind(specpool_tot,
                        specpool_PNIT, specpool_APAP,
                        specpool_ARAC, specpool_PDMI, specpool_PECJ, specpool_PEDS,
                        specpool_PEPP, specpool_PESP, specpool_PETP, specpool_PIMA,
                        specpool_PNCP, specpool_PNSB, specpool_PNSJ, specpool_PNSO, specpool_PESB)

# Print the resulting table
specpool_final













