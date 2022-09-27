library(flora)
library(abjutils)
library(dplyr)

########################################## ITATIAIA ######################################################################

itatiaia<- read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/itatiaia_checklist.csv', sep = ';')
itatiaia2<-subset(itatiaia, grepl("Campo de Altitude", itatiaia$vegetation.type))
itatiaia2$X=NULL
itatiaia_leandro<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/Tests/lists_jabot_splink_hv/lista_espécies_campos_ita2.csv',sep = ';')
itatiaia_leandro$Species<-sapply(itatiaia_leandro$Species, remove.authors)
itatiaia_leandro$Species<-gsub(itatiaia_leandro$Species, pattern = '<ca>', replacement = ' ')
itatiaia_leandro$Species<-gsub(itatiaia_leandro$Species, pattern = '\xfc\xbe\x98\x96\x8c\xbc', replacement = ' ')
x<-unique(itatiaia_leandro$Species)
itat<-get.taxa(itatiaia_leandro$Species,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(itat$scientific.name)
itat$life.form<-rm_accent(itat$life.form)
itat$habitat<-rm_accent(itat$habitat)
itat$vegetation.type<-rm_accent(itat$vegetation.type)
itat$vernacular.name<-rm_accent(itat$vernacular.name)
itat$domain<-rm_accent(itat$domain)
itat$endemism<-rm_accent(itat$endemism)
itat2<-subset(itat, grepl("Campo de Altitude", itat$vegetation.type))

###SSL
itat_CA<-merge(itatiaia2, itat2, all.x = T, all.y = T)
itat_CA<- itat_CA[order(itat_CA$scientific.name),]
itat_CA<- itat_CA[order(itat_CA$family),]
itat_CA<-unique(itat_CA)


###### total list, our method + unpublished data Freitas (pers. comm)
itat_tot<-merge(itatiaia, itat,  all.x = T, all.y = T)
itat_tot<- itat_tot[order(itat_tot$scientific.name),]
itat_tot<- itat_tot[order(itat_tot$family),]
itat_tot<-unique(itat_tot)


####### published list Moreira et al. 2020
itatiaia_pub<- read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/Tests/lists_jabot_splink_hv/Dados_Catalogo_UCs_Brasil_itatiaia.csv', sep = ';')
itatiaia_pub$Species=gsub(".*ACEAE","",itatiaia_pub$T_n)
itatiaia_pub$Species<-sapply(itatiaia_pub$Species, remove.authors)

itat_pub<-get.taxa(itatiaia_pub$Species,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
itat_pub$life.form<-rm_accent(itat_pub$life.form)
itat_pub$habitat<-rm_accent(itat_pub$habitat)
itat_pub$vegetation.type<-rm_accent(itat_pub$vegetation.type)
itat_pub$vernacular.name<-rm_accent(itat_pub$vernacular.name)
itat_pub$domain<-rm_accent(itat_pub$domain)
itat_pub$endemism<-rm_accent(itat_pub$endemism)
itat_pub2<-subset(itat_pub, grepl("Campo de Altitude", itat_pub$vegetation.type))

itat_pub2<- itat_pub2[order(itat_pub2$scientific.name),]
itat_pub2<- itat_pub2[order(itat_pub2$family),]
itat_pub2<-unique(itat_pub2)

itat_CA2<-merge(itat_CA, itat_pub2, all.x = T, all.y = T)
itat_CA2<-itat_CA2[,c(1:9, 12:19)]
itat_CA2<-unique(itat_CA2)
itat_CA2<- itat_CA2[order(itat_CA2$scientific.name),]
itat_CA2<- itat_CA2[order(itat_CA2$family),]
itat_CA2<-unique(itat_CA2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/Itatiaia')
write.csv2(itat_CA, file = 'itatiaia_leandro_igor_CA.csv')
write.csv2(itat_tot, file = 'itatiaia_leandro_igor_lato_sensu.csv')
write.csv2(itat_pub2, file = 'itatiaia_publicada_CA.csv')
write.csv2(itat_CA2, file = 'itatiaia_publicada_igor_leandro_CA.csv')




########################################## CAPARAO ######################################################################

caparao<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/caparao_checklist.csv')
caparao2<-subset(caparao, grepl("Campo de Altitude", caparao$vegetation.type))
caparao2$X=NULL
caparao$X=NULL
caparao_pub<- read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/Tests/lists_jabot_splink_hv/oo_462550.txt',, sep="\t")
caparao_pub<-caparao_pub[,c(1:21)]
caparao_pub<-data.frame(filter(caparao_pub, caparao_pub$Groups =='Angiosperms'))

cap_pub<-get.taxa(caparao_pub$Species,life.form = T, habitat = T, vegetation.type = T,
                   vernacular = T, states = T, establishment = T,domain = T, endemism = T)
cap_pub$life.form<-rm_accent(cap_pub$life.form)
cap_pub$habitat<-rm_accent(cap_pub$habitat)
cap_pub$vegetation.type<-rm_accent(cap_pub$vegetation.type)
cap_pub$vernacular.name<-rm_accent(cap_pub$vernacular.name)
cap_pub$domain<-rm_accent(cap_pub$domain)
cap_pub$endemism<-rm_accent(cap_pub$endemism)

cap_pub2<-subset(cap_pub, grepl("Campo de Altitude", cap_pub$vegetation.type))
cap_pub2<- cap_pub2[order(cap_pub2$scientific.name),]
cap_pub2<- cap_pub2[order(cap_pub2$family),]

cap_CA<-merge(caparao2,cap_pub2, all.x = T, all.y = T)
cap_CA<-cap_CA[,c(1:9, 12:19)]
cap_CA<-unique(cap_CA)
cap_CA<- cap_CA[order(cap_CA$scientific.name),]
cap_CA<- cap_CA[order(cap_CA$family),]

cap_CA_tot<-merge(caparao, cap_pub2,all.x = T, all.y = T)
cap_CA_tot<- cap_CA_tot[order(cap_CA_tot$scientific.name),]
cap_CA_tot<- cap_CA_tot[order(cap_CA_tot$family),]
cap_CA_tot<- unique(cap_CA_tot)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/Caparao')
write.csv2(cap_pub2, file = 'caparao_publicada_CA.csv')
write.csv2(cap_CA, file = 'caparao_publicada_igor_CA.csv')
write.csv2(cap_CA_tot, file = 'caparao_publicada_CA_igor_lato_sensu.csv')


########################################## PARNASO ######################################################################

parnaso<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/parnaso_checklist.csv')
parnaso2<-subset(parnaso, grepl("Campo de Altitude", parnaso$vegetation.type))
parnaso2$X=NULL
parnaso$X=NULL

parnaso<- parnaso[order(parnaso$scientific.name),]
parnaso<- parnaso[order(parnaso$family),]

parnaso2<- parnaso2[order(parnaso2$scientific.name),]
parnaso2<- parnaso2[order(parnaso2$family),]
parnaso2<-parnaso2[,c(1:9, 12:19)]
parnaso2<- unique(parnaso2)
setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/PARNASO')
write.csv2(parnaso, file = 'parnaso_igor_lato_sensu.csv')
write.csv2(parnaso2, file = 'parnaso_igor_CA.csv')

########################################## PARNA Sao Joaquim ######################################################################

sjoaquim<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/sao_joaquim_checklist.csv')
sjoaquim2<-subset(sjoaquim, grepl("Campo de Altitude", sjoaquim$vegetation.type))
sjoaquim2$X=NULL
sjoaquim$X=NULL

sjoaquim<- sjoaquim[order(sjoaquim$scientific.name),]
sjoaquim<- sjoaquim[order(sjoaquim$family),]

sjoaquim2<- sjoaquim2[order(sjoaquim2$scientific.name),]
sjoaquim2<- sjoaquim2[order(sjoaquim2$family),]
sjoaquim2<-sjoaquim2[,c(1:9, 12:19)]
sjoaquim2<- unique(sjoaquim2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/Sao_joaquim')
write.csv2(sjoaquim, file = 'sao_joaquim_igor_lato_sensu.csv')
write.csv2(sjoaquim2, file = 'sao_joaquim_igor_CA.csv')

########################################## PE Campos do Jordao ######################################################################

cjordao<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/campos_do_jordao_checklist.csv')
cjordao2<-subset(cjordao, grepl("Campo de Altitude", cjordao$vegetation.type))
cjordao2$X=NULL
cjordao$X=NULL

cjordao<- cjordao[order(cjordao$scientific.name),]
cjordao<- cjordao[order(cjordao$family),]

cjordao2<- cjordao2[order(cjordao2$scientific.name),]
cjordao2<- cjordao2[order(cjordao2$family),]
cjordao2<-cjordao2[,c(1:9, 12:19)]
cjordao2<- unique(cjordao2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs')
write.csv2(cjordao, file = 'cjordao_igor_lato_sensu.csv')
write.csv2(cjordao2, file = 'cjordao_igor_CA.csv')

########################################## PARNA Bocaina ######################################################################

bocaina<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/bocaina_checklist.csv')
bocaina2<-subset(bocaina, grepl("Campo de Altitude", bocaina$vegetation.type))
bocaina2$X=NULL
bocaina$X=NULL

bocaina<- bocaina[order(bocaina$scientific.name),]
bocaina<- bocaina[order(bocaina$family),]

bocaina2<- bocaina2[order(bocaina2$scientific.name),]
bocaina2<- bocaina2[order(bocaina2$family),]
bocaina2<-bocaina2[,c(1:9, 12:19)]
bocaina2<- unique(bocaina2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs')
write.csv2(bocaina, file = 'bocaina_igor_lato_sensu.csv')
write.csv2(bocaina2, file = 'bocaina_igor_CA.csv')


########################## PE PAPAGAIO/ PEDRA DA MINA / APA MANTIQUEIRA ####################################

papagaio<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/papagaio_checklist.csv')
papagaio2<-subset(papagaio, grepl("Campo de Altitude", papagaio$vegetation.type))
papagaio2$X=NULL
papagaio$X=NULL

papagaio<- papagaio[order(papagaio$scientific.name),]
papagaio<- papagaio[order(papagaio$family),]

papagaio2<- papagaio2[order(papagaio2$scientific.name),]
papagaio2<- papagaio2[order(papagaio2$family),]
papagaio2<-papagaio2[,c(1:9, 12:19)]
papagaio2<- unique(papagaio2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs')
write.csv2(papagaio, file = 'papagaio_igor_lato_sensu.csv')
write.csv2(papagaio2, file = 'papagaio_igor_CA.csv')

########################################## Serra Brigadeiro ######################################################################

brigadeiro<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/brigadeiro_checklist.csv')
brigadeiro2<-subset(brigadeiro, grepl("Campo de Altitude", brigadeiro$vegetation.type))
brigadeiro2$X=NULL
brigadeiro$X=NULL

brigadeiro<- brigadeiro[order(brigadeiro$scientific.name),]
brigadeiro<- brigadeiro[order(brigadeiro$family),]

brigadeiro2<- brigadeiro2[order(brigadeiro2$scientific.name),]
brigadeiro2<- brigadeiro2[order(brigadeiro2$family),]
brigadeiro2<-brigadeiro2[,c(1:9, 12:19)]
brigadeiro2<- unique(brigadeiro2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs')
write.csv2(brigadeiro, file = 'brigadeiro_igor_lato_sensu.csv')
write.csv2(brigadeiro2, file = 'brigadeiro_igor_CA.csv')


########################################## Pico Parana ######################################################################

pico_parana<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/pico_parana_checklist.csv')
pico_parana2<-subset(pico_parana, grepl("Campo de Altitude", pico_parana$vegetation.type))
pico_parana2$X=NULL
pico_parana$X=NULL

pico_parana<- pico_parana[order(pico_parana$scientific.name),]
pico_parana<- pico_parana[order(pico_parana$family),]

pico_parana2<- pico_parana2[order(pico_parana2$scientific.name),]
pico_parana2<- pico_parana2[order(pico_parana2$family),]
pico_parana2<-pico_parana2[,c(1:9, 12:19)]
pico_parana2<- unique(pico_parana2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs')
write.csv2(pico_parana, file = 'pico_parana_igor_lato_sensu.csv')
write.csv2(pico_parana2, file = 'pico_parana_igor_CA.csv')


########################################## Aracatuba #########################################################

aracatuba<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/aracatuba_checklist.csv')
aracatuba2<-subset(aracatuba, grepl("Campo de Altitude", aracatuba$vegetation.type))
aracatuba2$X=NULL
aracatuba$X=NULL

aracatuba<- aracatuba[order(aracatuba$scientific.name),]
aracatuba<- aracatuba[order(aracatuba$family),]

aracatuba2<- aracatuba2[order(aracatuba2$scientific.name),]
aracatuba2<- aracatuba2[order(aracatuba2$family),]
aracatuba2<-aracatuba2[,c(1:9, 12:19)]
aracatuba2<- unique(aracatuba2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs')
write.csv2(aracatuba, file = 'aracatuba_igor_lato_sensu.csv')
write.csv2(aracatuba2, file = 'aracatuba_igor_CA.csv')

########################################## PE Tres Picos #########################################################
tres_picos<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/tres_picos_checklist.csv')
tres_picos2<-subset(tres_picos, grepl("Campo de Altitude", tres_picos$vegetation.type))
tres_picos2$X=NULL
tres_picos$X=NULL

tres_picos<- tres_picos[order(tres_picos$scientific.name),]
tres_picos<- tres_picos[order(tres_picos$family),]

tres_picos2<- tres_picos2[order(tres_picos2$scientific.name),]
tres_picos2<- tres_picos2[order(tres_picos2$family),]
tres_picos2<-tres_picos2[,c(1:9, 12:19)]
tres_picos2<- unique(tres_picos2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs')
write.csv2(tres_picos, file = 'tres_picos_igor_lato_sensu.csv')
write.csv2(tres_picos2, file = 'tres_picos_igor_CA.csv')

########################################## APA Petrópolis #########################################################
apa_petropolis<-read.csv2('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/apa_petropolis_checklist.csv')
apa_petropolis2<-subset(apa_petropolis, grepl("Campo de Altitude", apa_petropolis$vegetation.type))
apa_petropolis2$X=NULL
apa_petropolis$X=NULL

apa_petropolis<- apa_petropolis[order(apa_petropolis$scientific.name),]
apa_petropolis<- apa_petropolis[order(apa_petropolis$family),]

apa_petropolis2<- apa_petropolis2[order(apa_petropolis2$scientific.name),]
apa_petropolis2<- apa_petropolis2[order(apa_petropolis2$family),]
apa_petropolis2<-apa_petropolis2[,c(1:9, 12:19)]
apa_petropolis2<- unique(apa_petropolis2)

setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs')
write.csv2(apa_petropolis, file = 'apa_petropolis_igor_lato_sensu.csv')
write.csv2(apa_petropolis2, file = 'apa_petropolis_igor_CA.csv')
