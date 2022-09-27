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

library(rgbif)
library(countrycode)
library(sp)
library(rgdal)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(elevatr)
library(readr)
library(magrittr)
library(raster)
library(rgbif)
library(maps)
library(abjutils)
library(flora)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs2')

######################## Vouchers obtained in bacbonelists data ########################################

tot_2000<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/vouchers_2000.csv', sep = ';')
tot_2000$decimalLatitude=gsub(tot_2000$decimalLatitude, pattern = ',', replacement = '.')
tot_2000$decimalLongitude=gsub(tot_2000$decimalLongitude, pattern = ',', replacement = '.')

tot_1800<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/vouchers_1800.csv', sep = ';')
tot_1800$decimalLatitude=gsub(tot_1800$decimalLatitude, pattern = ',', replacement = '.')
tot_1800$decimalLongitude=gsub(tot_1800$decimalLongitude, pattern = ',', replacement = '.')

tot_1500<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/vouchers_1500.csv', sep = ';')
tot_1500$decimalLatitude=gsub(tot_1500$decimalLatitude, pattern = ',', replacement = '.')
tot_1500$decimalLongitude=gsub(tot_1500$decimalLongitude, pattern = ',', replacement = '.')

tot_1300<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/vouchers_1300.csv', sep = ';')
tot_1300$decimalLatitude=gsub(tot_1300$decimalLatitude, pattern = ',', replacement = '.')
tot_1300$decimalLongitude=gsub(tot_1300$decimalLongitude, pattern = ',', replacement = '.')


####LSL list to be reference of the species not recognized in get.taxa

LSL<-read.csv("G:/Meu Drive/Campos de altitude_papers/AppS1.csv")

######################### Dividing papagaio into papagaio, mina and marins ###################################
papagaio_tot<-subset(tot_1800, grepl('PE Papagaio/Pedra da Mina/APA Mantiqueira', tot_1800$uc))


apsm<-data.frame(filter(papagaio_tot, papagaio_tot$decimalLatitude >= -22.365))

apsm_papagaio<-data.frame(filter(papagaio_tot, papagaio_tot$decimalLatitude < -22.365))
apsm_mina<-apsm[!(apsm$decimalLongitude > -45) ,]
apsm_marins<-apsm[!(apsm$decimalLongitude < -45) ,]

apsm_na<-subset(papagaio_tot, is.na(papagaio_tot$decimalLatitude))

apsm_na_papagaio<- subset(apsm_na, grepl("bocaina de minas|papagaio", apsm_na$locality, ignore.case = T))
apsm_papagaio<-rbind(apsm_papagaio,apsm_na_papagaio) 

apsm_na_mina<- subset(apsm_na, grepl("mina|Pedra da mi|serra fina", apsm_na$locality, ignore.case = T))
apsm_mina<-rbind(apsm_mina,apsm_na_mina) 

apsm_na_marins<- subset(apsm_na, grepl("marins", apsm_na$locality, ignore.case = T))
apsm_marins<-rbind(apsm_marins,apsm_na_marins) 



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

####### vouchers of collections if I need

######### New flora check: News only (PDMI, PIMA, PESP), PESB e PEDS ################################

################################# Pedra da Mina >1800m ##############################################

PDMI_tot$scientificName<-sapply(PDMI_tot$scientificName, remove.authors)
x<-unique(PDMI_tot$scientificName)
mina<-get.taxa(PDMI_tot$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)


mina_na <-mina[(is.na(mina$scientific.name)),]
mina_na2 <- mina_na[-c(1,2,7), ]
mina_na2 <-get.taxa(mina_na2$original.search,life.form = T, habitat = T, vegetation.type = T,
                    vernacular = T, states = T, establishment = T,domain = T, endemism = T, suggestion.distance = 0.5 )

mina<-mina[(!is.na(mina$scientific.name)),]
mina<-rbind(mina,mina_na2)

x<-unique(mina$scientific.name)
mina$life.form<-rm_accent(mina$life.form)
mina$habitat<-rm_accent(mina$habitat)
mina$vegetation.type<-rm_accent(mina$vegetation.type)
mina$vernacular.name<-rm_accent(mina$vernacular.name)
mina$domain<-rm_accent(mina$domain)
mina$endemism<-rm_accent(mina$endemism)
write.csv2(mina, file = 'pedra_da_mina_checklist.csv')

################################# Papagaio >1800m ##########################################

PESP_tot$scientificName<-sapply(PESP_tot$scientificName, remove.authors)
x<-unique(PESP_tot$scientificName)
papagaio<-get.taxa(PESP_tot$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)

papagaio_na <-papagaio[(is.na(papagaio$scientific.name)),]
papagaio_na2 <-get.taxa(papagaio_na$original.search,life.form = T, habitat = T, vegetation.type = T,
                    vernacular = T, states = T, establishment = T,domain = T, endemism = T, suggestion.distance = 0.5 )
papagaio_na2 <- papagaio_na2[-c(3), ]


papagaio<-papagaio[(!is.na(papagaio$scientific.name)),]
papagaio<-rbind(papagaio,papagaio_na2)

x<-unique(papagaio$scientific.name)
papagaio$life.form<-rm_accent(papagaio$life.form)
papagaio$habitat<-rm_accent(papagaio$habitat)
papagaio$vegetation.type<-rm_accent(papagaio$vegetation.type)
papagaio$vernacular.name<-rm_accent(papagaio$vernacular.name)
papagaio$domain<-rm_accent(papagaio$domain)
papagaio$endemism<-rm_accent(papagaio$endemism)
write.csv2(papagaio, file = 'papagaio_checklist.csv')

############################################### Marins >1800m ########################################################

PIMA_tot$scientificName<-sapply(PIMA_tot$scientificName, remove.authors)
x<-unique(PIMA_tot$scientificName)
marins<-get.taxa(PIMA_tot$scientificName,life.form = T, habitat = T, vegetation.type = T,
                   vernacular = T, states = T, establishment = T,domain = T, endemism = T)

marins_na <-marins[(is.na(marins$scientific.name)),]
marins_na2 <-get.taxa(marins_na$original.search,life.form = T, habitat = T, vegetation.type = T,
                        vernacular = T, states = T, establishment = T,domain = T, endemism = T, suggestion.distance = 0.5 )
marins_na2 <- marins_na2[c(1), ]

####inserting oxypetalum
oxy<-LSL[c(1435), ]
oxy<-oxy[,c(2:16) ]
oxy$accepted.name<-NA
oxy$search.str<-NA
oxy$original.search<-NA
oxy$notes<-NA
oxy$threat.status.mma<-NULL
colnames(oxy)
colnames(marins_na2)

marins<-marins[(!is.na(marins$scientific.name)),]
marins<-rbind(marins,marins_na2)
marins<-rbind(marins, oxy)

x<-unique(marins$scientific.name)
marins$life.form<-rm_accent(marins$life.form)
marins$habitat<-rm_accent(marins$habitat)
marins$vegetation.type<-rm_accent(marins$vegetation.type)
marins$vernacular.name<-rm_accent(marins$vernacular.name)
marins$domain<-rm_accent(marins$domain)
marins$endemism<-rm_accent(marins$endemism)
write.csv2(marins, file = 'marins_checklist.csv')


################################# Desengano >1500m ########################################################

PEDS_tot$scientificName<-sapply(PEDS_tot$scientificName, remove.authors)
x<-unique(PEDS_tot$scientificName)
desengano<-get.taxa(PEDS_tot$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
desengano_na <-desengano[(is.na(desengano$scientific.name)),]
desengano_na$original.search<-gsub(desengano_na$original.search, pattern = 'Paspalum densiflorum', replacement = 'Paspalum umbrosum')
desengano_na2 <-get.taxa(desengano_na$original.search,life.form = T, habitat = T, vegetation.type = T,
                        vernacular = T, states = T, establishment = T,domain = T, endemism = T, suggestion.distance = 0.5 )
desengano_na2 <- desengano_na2[-c(3,4,5,6,9), ]


desengano<-desengano[(!is.na(desengano$scientific.name)),]
desengano<-rbind(desengano,desengano_na2)

x<-unique(desengano$scientific.name)
desengano$life.form<-rm_accent(desengano$life.form)
desengano$habitat<-rm_accent(desengano$habitat)
desengano$vegetation.type<-rm_accent(desengano$vegetation.type)
desengano$vernacular.name<-rm_accent(desengano$vernacular.name)
desengano$domain<-rm_accent(desengano$domain)
desengano$endemism<-rm_accent(desengano$endemism)
write.csv2(desengano, file = 'desengano_checklist.csv')

################################# Serra do Brigadeiro >1500m ##########################################

PESB_tot_1500$scientificName<-sapply(PESB_tot_1500$scientificName, remove.authors)
x<-unique(PESB_tot_1500$scientificName)
brig<-get.taxa(PESB_tot_1500$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)

brig_na <-brig[(is.na(brig$scientific.name)),]
brig_na$original.search<-gsub(brig_na$original.search, pattern = 'Palicourea tomentosa', replacement = 'Psychotria tomentosa')

brig_na2 <-get.taxa(brig_na$original.search,life.form = T, habitat = T, vegetation.type = T,
                         vernacular = T, states = T, establishment = T,domain = T, endemism = T, suggestion.distance = 0.5 )


brig<-brig[(!is.na(brig$scientific.name)),]
brig<-rbind(brig,brig_na2)
x<-unique(brig$scientific.name)
brig$life.form<-rm_accent(brig$life.form)
brig$habitat<-rm_accent(brig$habitat)
brig$vegetation.type<-rm_accent(brig$vegetation.type)
brig$vernacular.name<-rm_accent(brig$vernacular.name)
brig$domain<-rm_accent(brig$domain)
brig$endemism<-rm_accent(brig$endemism)
write.csv2(brig, file = 'brigadeiro_checklist.csv')

################################# Generate separate lists #########################################################
########################################## Pedra da Mina  #########################################################

mina2<-subset(mina, grepl("Campo de Altitude", mina$vegetation.type))

mina<- mina[order(mina$scientific.name),]
mina<- mina[order(mina$family),]

mina2<- mina2[order(mina2$scientific.name),]
mina2<- mina2[order(mina2$family),]

mina1<-mina[,-c(9,10)]
mina1<- unique(mina1)
mina2<-mina2[,-c(9,10)]
mina2<- unique(mina2)

setwd('G:/Meu Drive/lists_UCs2')
write.csv2(mina1, file = 'Mina_igor_lato_sensu.csv')
write.csv2(mina2, file = 'Mina_igor_CA.csv')

########################################## Papagaio  #########################################################

papagaio2<-subset(papagaio, grepl("Campo de Altitude", papagaio$vegetation.type))

papagaio<- papagaio[order(papagaio$scientific.name),]
papagaio<- papagaio[order(papagaio$family),]

papagaio2<- papagaio2[order(papagaio2$scientific.name),]
papagaio2<- papagaio2[order(papagaio2$family),]

papagaio1<-papagaio[,-c(9,10)]
papagaio1<- unique(papagaio1)
papagaio2<-papagaio2[,-c(9,10)]
papagaio2<- unique(papagaio2)

setwd('G:/Meu Drive/lists_UCs2')
write.csv2(papagaio1, file = 'Mina_igor_lato_sensu.csv')
write.csv2(papagaio2, file = 'Mina_igor_CA.csv')

########################################## Marins  #########################################################

marins2<-subset(marins, grepl("Campo de Altitude", marins$vegetation.type))

marins<- marins[order(marins$scientific.name),]
marins<- marins[order(marins$family),]

marins2<- marins2[order(marins2$scientific.name),]
marins2<- marins2[order(marins2$family),]

marins1<-marins[,-c(9,10)]
marins1<- unique(marins1)
marins2<-marins2[,-c(9,10)]
marins2<- unique(marins2)

setwd('G:/Meu Drive/lists_UCs2')
write.csv2(marins1, file = 'Marins_igor_lato_sensu.csv')
write.csv2(marins2, file = 'Marins_igor_CA.csv')

########################################## Desengano  #########################################################

desengano2<-subset(desengano, grepl("Campo de Altitude", desengano$vegetation.type))

desengano<- desengano[order(desengano$scientific.name),]
desengano<- desengano[order(desengano$family),]

desengano2<- desengano2[order(desengano2$scientific.name),]
desengano2<- desengano2[order(desengano2$family),]

desengano1<-desengano[,-c(9,10)]
desengano1<- unique(desengano1)
desengano2<-desengano2[,-c(9,10)]
desengano2<- unique(desengano2)

setwd('G:/Meu Drive/lists_UCs2')
write.csv2(desengano1, file = 'desengano_igor_lato_sensu.csv')
write.csv2(desengano2, file = 'desengano_igor_CA.csv')


########################################## Brigadeiro  #########################################################

brig2<-subset(brig, grepl("Campo de Altitude", brig$vegetation.type))

brig<- brig[order(brig$scientific.name),]
brig<- brig[order(brig$family),]

brig2<- brig2[order(brig2$scientific.name),]
brig2<- brig2[order(brig2$family),]

brig1<-brig[,-c(9,10)]
brig1<- unique(brig1)
brig2<-brig2[,-c(9,10)]
brig2<- unique(brig2)

setwd('G:/Meu Drive/lists_UCs2')
write.csv2(brig1, file = 'brigadeiro_igor_lato_sensu.csv')
write.csv2(brig2, file = 'brigadeiro_igor_CA.csv')

##################################################################################################
########################################## Merging resutls with lists ucs ########################
##################################################################################################

itat_tot<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Itatiaia/itatiaia_leandro_igor_lato_sensu.csv', sep = ';')
itat_tot$X=NULL
itat_tot$threat.status.mma=NULL
itat_CA2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Itatiaia/itatiaia_publicada_igor_leandro_CA.csv', sep = ';')
itat_CA2$X=NULL
itat_CA2$threat.status.mma=NULL


cap_CA_tot<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Caparao/caparao_publicada_CA_igor_lato_sensu.csv', sep = ';')
cap_CA_tot$X=NULL
cap_CA_tot$threat.status.mma=NULL
cap_CA<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Caparao/caparao_publicada_igor_CA.csv', sep = ';')
cap_CA$X=NULL
cap_CA$threat.status.mma=NULL

parnaso<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/PARNASO/parnaso_igor_lato_sensu.csv', sep = ';')
parnaso$X=NULL
parnaso$threat.status.mma=NULL
parnaso2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/PARNASO/parnaso_igor_CA.csv', sep = ';')
parnaso2$X=NULL
parnaso2$threat.status.mma=NULL

sjoaquim<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Sao_joaquim/sao_joaquim_igor_lato_sensu.csv', sep = ';')
sjoaquim$X=NULL
sjoaquim$threat.status.mma=NULL
sjoaquim2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Sao_joaquim/sao_joaquim_igor_CA.csv', sep = ';')
sjoaquim2$X=NULL
sjoaquim2$threat.status.mma=NULL

cjordao<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Cjordao/cjordao_igor_lato_sensu.csv', sep = ';')
cjordao$X=NULL
cjordao$threat.status.mma=NULL
cjordao2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Cjordao/cjordao_igor_CA.csv', sep = ';')
cjordao2$X=NULL
cjordao2$threat.status.mma=NULL

bocaina<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Bocaina/bocaina_igor_lato_sensu.csv', sep = ';')
bocaina$X=NULL
bocaina$threat.status.mma=NULL
bocaina2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Bocaina/bocaina_igor_CA.csv', sep = ';')
bocaina2$X=NULL
bocaina2$threat.status.mma=NULL

pico_parana<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Pico_parana/pico_parana_igor_lato_sensu.csv', sep = ';')
pico_parana$X=NULL
pico_parana$threat.status.mma=NULL
pico_parana2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Pico_parana/pico_parana_igor_CA.csv', sep = ';')
pico_parana2$X=NULL
pico_parana2$threat.status.mma=NULL

aracatuba<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Aracatuba/aracatuba_igor_lato_sensu.csv', sep = ';')
aracatuba$X=NULL
aracatuba$threat.status.mma=NULL
aracatuba2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Aracatuba/aracatuba_igor_CA.csv', sep = ';')
aracatuba2$X=NULL
aracatuba2$threat.status.mma=NULL

tres_picos<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Tres_picos/tres_picos_igor_lato_sensu.csv', sep = ';')
tres_picos$X=NULL
tres_picos$threat.status.mma=NULL
tres_picos2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Tres_picos/tres_picos_igor_CA.csv', sep = ';')
tres_picos2$X=NULL
tres_picos2$threat.status.mma=NULL

apa_petropolis<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/APA_Petropolis/apa_petropolis_igor_lato_sensu.csv', sep = ';')
apa_petropolis$X=NULL
apa_petropolis$threat.status.mma=NULL
apa_petropolis2<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/APA_Petropolis/apa_petropolis_igor_CA.csv', sep = ';')
apa_petropolis2$X=NULL
apa_petropolis2$threat.status.mma=NULL


##################################################################################################
########################### Presence/absence matrix LSL ##########################################
##################################################################################################


parnaso$parnaso='1'
sjoaquim$sjoaquim='1'
itat_tot$itatiaia='1'
cap_CA_tot$caparao='1'
cjordao$cjordao='1'
bocaina$bocaina='1'
pico_parana$pico_parana='1'
aracatuba$aracatuba='1'
tres_picos$tres_picos='1'
apa_petropolis$apa_petropolis='1'
brig1$brigadeiro='1'
desengano1$desengano='1'
marins1$marins='1'
papagaio1$papagaio='1'
mina1$mina='1'

y<-merge(parnaso, sjoaquim,all.x = T, all.y = T)
y<-merge(y, itat_tot,all.x = T, all.y = T)
y<-merge(y, cap_CA_tot,all.x = T, all.y = T)
y<-merge(y, cjordao,all.x = T, all.y = T)
y<-merge(y, bocaina,all.x = T, all.y = T)
y<-merge(y, pico_parana,all.x = T, all.y = T)
y<-merge(y, aracatuba,all.x = T, all.y = T)
y<-merge(y, tres_picos,all.x = T, all.y = T)
y<-merge(y, apa_petropolis,all.x = T, all.y = T)
y<-merge(y, brig1,all.x = T, all.y = T)
y<-merge(y, desengano1,all.x = T, all.y = T)
y<-merge(y, marins1,all.x = T, all.y = T)
y<-merge(y, papagaio1,all.x = T, all.y = T)
y<-merge(y, mina1,all.x = T, all.y = T)

unique(y$scientific.name)


##################################################################################################
######## Correcting the missing name in flora, according WFO and Tropicos synonyms ##############
##################################################################################################

y_na <-y[(is.na(y$scientific.name)),]
y_na <-y_na[order(y_na$original.search),]
y_na$original.search=gsub(y_na$original.search,pattern = 'Oxypetalum insigne var. glabrum', replacement = 'Oxypetalum glabrum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Senna multijuga var. lindleyana', replacement = 'Senna multijuga subsp. lindleyana')
y_na$original.search=gsub(y_na$original.search,pattern = 'Abutilon vexillarium', replacement = 'Abutilon megapotamicum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Achyrocline candicans', replacement = 'Achyrocline satureioides')
y_na$original.search=gsub(y_na$original.search,pattern = 'Acisanthera variabilis var. glabriuscula', replacement = 'Acisanthera variabilis')
y_na$original.search=gsub(y_na$original.search,pattern = 'Aiouea hatschbachii', replacement = 'Cinnamomum hatschbachii')
y_na$original.search=gsub(y_na$original.search,pattern = 'Arenaria lanuginosa var. megalantha', replacement = 'Arenaria lanuginosa')
y_na$original.search=gsub(y_na$original.search,pattern = 'Axonopus siccus var. siccus', replacement = 'Axonopus siccus')
y_na$original.search=gsub(y_na$original.search,pattern = 'Baccharis microthamna', replacement = 'Baccharis selloi')
y_na$original.search=gsub(y_na$original.search,pattern = 'Campovassouria bupleurifolia', replacement = 'Campovassouria cruciata')
y_na$original.search=gsub(y_na$original.search,pattern = 'Centaurium umbellatum', replacement = 'Centaurium erythraea')
y_na$original.search=gsub(y_na$original.search,pattern = 'Centratherum punctatum subsp. punctatum', replacement = 'Centratherum punctatum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Chionolaena glomerata', replacement = 'Chionolaena latifolia')
y_na$original.search=gsub(y_na$original.search,pattern = 'Clematis denticulata', replacement = 'Clematis campestris')
y_na$original.search=gsub(y_na$original.search,pattern = 'Conyza sumatrensis var. floribunda', replacement = 'Conyza sumatrensis')
y_na$original.search=gsub(y_na$original.search,pattern = 'Cyphomandra corymbiflora subsp. mortoniana', replacement = 'Solanum corymbiflorum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Erigeron alpestre', replacement = 'Leptostelma maximum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Erigeron camposportoi', replacement = 'Leptostelma camposportoi')
y_na$original.search=gsub(y_na$original.search,pattern = 'Erigeron catharinensis', replacement = 'Leptostelma catharinense')
y_na$original.search=gsub(y_na$original.search,pattern = 'Erigeron maximus', replacement = 'Leptostelma maximum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Erythraea centaurium', replacement = 'Centaurium erythraea')
y_na$original.search=gsub(y_na$original.search,pattern = 'Eupatorium betoniciforme', replacement = 'Barrosoa betonicaeformis')
y_na$original.search=gsub(y_na$original.search,pattern = 'Eupatorium bupleurifolium', replacement = 'Campovassouria cruciata')
y_na$original.search=gsub(y_na$original.search,pattern = 'Eupatorium intermedium', replacement = 'Grazielia intermedia')
y_na$original.search=gsub(y_na$original.search,pattern = 'Eupatorium laetevirens', replacement = 'Austroeupatorium laetevirens')
y_na$original.search=gsub(y_na$original.search,pattern = 'Eupatorium serratum', replacement = 'Grazielia serrata')
y_na$original.search=gsub(y_na$original.search,pattern = 'Fuchsia regia var. affinis', replacement = 'Fuchsia regia subsp. serrae')
y_na$original.search=gsub(y_na$original.search,pattern = 'Gamochaeta brasiliana', replacement = 'Gamochaeta brasiliana')
y_na$original.search=gsub(y_na$original.search,pattern = 'Gamochaeta spicata', replacement = 'Gamochaeta americana')
y_na$original.search=gsub(y_na$original.search,pattern = 'Heterocondylus vauthierianus', replacement = 'Heterocondylus alatus')
y_na$original.search=gsub(y_na$original.search,pattern = 'Hypochaeris brasiliensis', replacement = 'Hypochaeris chillensis')
y_na$original.search=gsub(y_na$original.search,pattern = 'Leucopholis capitata', replacement = 'Chionolaena capitata')
y_na$original.search=gsub(y_na$original.search,pattern = 'Leucopholis latifolia', replacement = 'Chionolaena latifolia')
y_na$original.search=gsub(y_na$original.search,pattern = 'Lucilia flagelliformis', replacement = 'Lucilia lycopodioides')
y_na$original.search=gsub(y_na$original.search,pattern = 'Luehea divaricata Mart.', replacement = 'Luehea divaricata')
y_na$original.search=gsub(y_na$original.search,pattern = 'Mikania ligustrifolia var. subsessilis', replacement = 'Mikania ligustrifolia')
y_na$original.search=gsub(y_na$original.search,pattern = 'Mimosa scabrella subsp. scabrella', replacement = 'Mimosa scabrella')
y_na$original.search=gsub(y_na$original.search,pattern = 'Myrceugenia alpigena var. rufa', replacement = 'Myrceugenia alpigena')
y_na$original.search=gsub(y_na$original.search,pattern = 'Myrceugenia ovata var. gracilis', replacement = 'Myrceugenia regnelliana')
y_na$original.search=gsub(y_na$original.search,pattern = 'Neoregelia magdalenae var. magdalenae', replacement = 'Neoregelia magdalenae')
y_na$original.search=gsub(y_na$original.search,pattern = 'Panicum demissum', replacement = 'Dichanthelium sabulorum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Pleroma albiflorum', replacement = 'Pleroma hospitum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Prunus sellowii var. longifolia', replacement = 'Prunus myrtifolia')
y_na$original.search=gsub(y_na$original.search,pattern = 'Senecio bradei', replacement = 'Dendrophorbium bradei')
y_na$original.search=gsub(y_na$original.search,pattern = 'Senecio glaziovii', replacement = 'Dendrophorbium glaziovii')
y_na$original.search=gsub(y_na$original.search,pattern = 'Senecio itatiaiae', replacement = 'Graphistylis itatiaiae')
y_na$original.search=gsub(y_na$original.search,pattern = 'Senecio organensis', replacement = 'Graphistylis organensis')
y_na$original.search=gsub(y_na$original.search,pattern = 'Senecio pellucidinervis', replacement = 'Dendrophorbium pellucidinerve')
y_na$original.search=gsub(y_na$original.search,pattern = 'Senecio toledoi', replacement = 'Graphistylis toledoi')
y_na$original.search=gsub(y_na$original.search,pattern = 'Stevia tenuis', replacement = 'Stevia veronicae')
y_na$original.search=gsub(y_na$original.search,pattern = 'Tibouchina hospita var. albiflora', replacement = 'Pleroma hospitum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Tibouchina hospita var. minor', replacement = 'Pleroma hospitum')
y_na$original.search=gsub(y_na$original.search,pattern = 'Trixis verbascifolia subsp. verbascifolia', replacement = 'Trixis verbascifolia')
y_na$original.search=gsub(y_na$original.search,pattern = 'Vernonia nitidula', replacement = 'Vernonanthura montevidensis')
y_na$original.search=gsub(y_na$original.search,pattern = 'Vernonia oppositifolia', replacement = 'Critoniopsis stellata')
y_na$original.search=gsub(y_na$original.search,pattern = 'Vernonia puberula', replacement = 'Vernonanthura puberula')
y_na$original.search=gsub(y_na$original.search,pattern = 'Weinmannia itatiaia', replacement = 'Weinmannia humilis')

y_na2 <- get.taxa(y_na$original.search,life.form = T, habitat = T, vegetation.type = T,vernacular = T, states = T, establishment = T,domain = T, endemism = T, suggestion.distance = 0.5)
write.csv2(y_na2, file = 'y_na3.csv')
y_na2 <- y_na2[-c(1,5,7:9,12,13,14,18,20,25,27,28,29,32,33,40,45,48,49,51,52,53,56,57,59,61,63,65,67,76,77,87,88,95,99,101,103,104,106,108,109,110,112,113,115,116,119,121,122,123,126:128,131:133,136,140), ]
y_na <- y_na[-c(1,5,7:9,12,13,14,18,20,25,27,28,29,32,33,40,45,48,49,51,52,53,56,57,59,61,63,65,67,76,77,87,88,95,99,101,103,104,106,108,109,110,112,113,115,116,119,121,122,123,126:128,131:133,136,140), ]
y_na3<-y_na[,c(18:33)]
y_na4<-merge(y_na2,y_na3, by='original.search')
y_na4$threat.status.mma=NULL
y<-y[!(is.na(y$scientific.name)),]
y_final<-rbind(y,y_na4)
unique(y_final$id)

y_final<-y_final[,-c(3,7,17,18)]
unique(y_final$id)

##################################################################################################
################################## GENERATE THE FINAL LSL  #######################################
##################################################################################################

y_final2 <- y_final %>%
  group_by(scientific.name) %>%
  summarise_all(funs(na.omit(.)[1]))

write.csv2(y_final2, file = 'y_final2.csv')

#### Fix problems of the duplicated names in varieties and subsp, errors from flora package

y_final2$scientific.name[27]="Actinocephalus polyanthus (Bong.) Sano"
y_final2$scientific.name[49]="Agarista hispidula (DC.) Hook. ex Nied."
y_final2$scientific.name[51]="Agarista niederleinii (Sleumer) Judd"
y_final2$scientific.name[53]="Agarista oleifolia (Cham.) G.Don"
y_final2$scientific.name[61]="Agrostis longiberbis Hack. ex L.B.Sm."
y_final2$scientific.name[64]="Agrostis montevidensis Spreng. ex Nees"
y_final2$scientific.name[77]="Alstroemeria isabelleana Herb."
y_final2$scientific.name[292]="Billbergia distachia (Vell.) Mez"
y_final2$scientific.name[313]="Bromus brachyanthera Döll"
y_final2$scientific.name[315]="Brunfelsia brasiliensis (Spreng.) L.B.Sm. & Downs"
y_final2$scientific.name[349]="Cabralea canjerana (Vell.) Mart."
y_final2$scientific.name[413]="Cattleya cinnabarina (Bateman ex Lindl.) Van den Berg"
y_final2$scientific.name[465]="Chascolytrum juergensii (Hack.) L. Essi, Souza-Chies & Longhi-Wagner"
y_final2$scientific.name[512]="Chusquea mimosa McClure & L.B.Sm."
y_final2$scientific.name[739]="Drimys brasiliensis Miers"
y_final2$scientific.name[790]="Erechtites valerianifolius (Link ex Spreng.) DC."
y_final2$scientific.name[877]="Fuchsia regia (Vell.) Munz"
y_final2$scientific.name[894]="Gaultheria eriophylla (Pers.) Sleumer ex Burtt"
y_final2$scientific.name[913]="Galium hypocarpium (L.) Endl. ex Griseb."
y_final2$scientific.name[917]="Gaultheria serrata (Vell.) Sleumer ex Kin.-Gouv."
y_final2$scientific.name[925]="Gaylussacia brasiliensis (Spreng.) Meisn."
y_final2$scientific.name[931]="Gaylussacia densa Cham."
y_final2$scientific.name[952]="Glandularia lobata (Vell.) P.Peralta & Thode"
y_final2$scientific.name[1040]="Hedeoma crenata R.S.Irving"
y_final2$scientific.name[1042]="Hedyosmum brasiliense Mart. ex Miq."
y_final2$scientific.name[1107]="Hyptis radicans (Pohl) Harley & J.F.B.Pastore"
y_final2$scientific.name[1364]="Miconia cinerascens Miq."
y_final2$scientific.name[1380]="Miconia theizans (Bonpl.) Cogn."
y_final2$scientific.name[1419]="Mimosa aurivillus Mart."
y_final2$scientific.name[1436]="Mimosa monticola Dusén"
y_final2$scientific.name[1439]="Mimosa oblonga Benth."
y_final2$scientific.name[1714]="Pavonia schrankii Spreng."
y_final2$scientific.name[1746]="Peperomia tetraphylla (G.Forst.) Hook. & Arn."
y_final2$scientific.name[1821]="Pitcairnia flammea Lindl."
y_final2$scientific.name[1849]="Pleroma heteromallum (D.Don) D.Don"
y_final2$scientific.name[1967]="Qualea cryptantha (Spreng.) Warm."
y_final2$scientific.name[2027]="Roupala montana Aubl."
y_final2$scientific.name[2033]="Rubus rosifolius Sm."
y_final2$scientific.name[2038]="Rudgea parquioides (Cham.) Müll.Arg."
y_final2$scientific.name[2114]="Senecio pulcher Hook. & Arn."
y_final2$scientific.name[2126]="Senna organensis (Glaz. ex Harms) H.S.Irwin"
y_final2$scientific.name[2157]="Siphocampylus longipedunculatus Pohl"
y_final2$scientific.name[2360]="Tillandsia stricta Sol."
y_final2$scientific.name[2360]="Tillandsia stricta Sol."
y_final2$scientific.name[2462]="Vriesea billbergioides E.Morren ex Mez"
y_final2$scientific.name[2533]="Zygopetalum maculatum subsp. triste (Barb.Rodr.) Meneguzzo"
y_final2<-y_final2[-c(194,282,915,2404),]

#######################################################################################
############################# FINAL LSL list######################################
y_final2 <- y_final2 %>%
  group_by(scientific.name) %>%
  summarise_all(funs(na.omit(.)[1]))

##to check
z1<- data.frame(filter(y_final2, y_final2$domain == 'Cerrado'))
z2<- data.frame(filter(y_final2, y_final2$vegetation.type == 'Campo rupestre'))

y_final2<-y_final2[!(y_final2$domain %in% c('Amazonia','Cerrado','Caatinga', 'Pampa')), ]
y_final2<-y_final2[!(y_final2$vegetation.type %in% c('Campo rupestre')), ]
y_final2[,c(15:29)][is.na(y_final2[,c(15:29)])] <- '0'
LSL_final<-y_final2 
write.csv2(LSL_final, file = 'LSL_final.csv')

#######################################################################################
############################# FINAL SSL list ######################################
SSL_final <-subset(LSL_final, grepl("Campo de Altitude", LSL_final$vegetation.type))
write.csv2(SSL_final, file = 'SSL_final.csv')


