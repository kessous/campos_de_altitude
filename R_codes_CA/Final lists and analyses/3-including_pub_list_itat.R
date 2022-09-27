##################################################################################################
########################### Presence/absence matrix LSL ##########################################
##################################################################################################
#####Including data from Moreira et al 2020 #######

library(flora)
colnames(itat_CA2)
colnames(itat_tot)

itat_tot<-itat_tot[,-c(9,10,19)]

itat_CA3<-merge(itat_CA2, itat_tot, all.x = T, all.y = T)
itat_tot<-unique(itat_CA3)

parnaso$parnaso=NULL
sjoaquim$sjoaquim=NULL
cap_CA_tot$caparao=NULL
cjordao$cjordao=NULL
bocaina$bocaina=NULL
pico_parana$pico_parana=NULL
aracatuba$aracatuba=NULL
tres_picos$tres_picos=NULL
apa_petropolis$apa_petropolis=NULL
brig1$brigadeiro=NULL
desengano1$desengano=NULL
marins1$marins=NULL
papagaio1$papagaio=NULL
mina1$mina=NULL

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


y_final2$scientific.name[194]
y_final2$scientific.name[282]
y_final2$scientific.name[915]
y_final2$scientific.name[2404]

y_final2<-y_final2[-c(194,282,915,2404),]

#######################################################################################
############################# FINAL LSL list!!!! ######################################
y_final2 <- y_final2 %>%
  group_by(scientific.name) %>%
  summarise_all(funs(na.omit(.)[1]))

##to check
z1<- data.frame(filter(y_final2, y_final2$domain == 'Cerrado'))
z2<- data.frame(filter(y_final2, y_final2$vegetation.type == 'Campo rupestre'))

##to do
#####rever pq que ta saindo NA no scientific name, talvez usar outro método.

y_final2<-y_final2[!(y_final2$domain %in% c('Amazonia','Cerrado','Caatinga', 'Pampa')), ]
y_final2<-y_final2[!(y_final2$vegetation.type %in% c('Campo rupestre')), ]
y_final2[,c(15:29)][is.na(y_final2[,c(15:29)])] <- '0'
LSL_final<-y_final2 
write.csv2(LSL_final, file = 'LSL_final.csv')
#######################################################################################
#grepl pega o valor no meio, dplyr é exato
############################# FINAL SSL list!!!! ######################################
SSL_final <-subset(LSL_final, grepl("Campo de Altitude", LSL_final$vegetation.type))
write.csv2(SSL_final, file = 'SSL_final.csv')

############################# Including Pub list of Itatiaia ######################################

itatiaia_SSL_clean<-data.frame(filter(SSL_final, SSL_final$itatiaia == 1))
itatiaia_SSL_clean<-subset(SSL_final, grepl('1', SSL_final$itatiaia))
itatiaia_LSL_clean<-subset(LSL_final, grepl('1', LSL_final$itatiaia))
itat_pub2<-read.csv("/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs/UCS/Itatiaia/itatiaia_leandro_igor_CA.csv", sep=";")
itat_pub2<-unique(itat_pub2)
itatiaia_SSL_clean$igor='1'
itat_pub2$pub='1'
colnames(itatiaia_SSL_clean)
colnames(itat_pub2)
itatiaia_SSL_clean<-itatiaia_SSL_clean[,c(1:14,30)]
itat_pub2<-itat_pub2[,c(2,3,5,6,7,9,11,12,13,14,15,16,17,18,19)]
itat_missing<-merge(itatiaia_SSL_clean, itat_pub2,all.x = T, all.y = T)
itat_missing2<-itat_missing[(is.na(itat_missing$igor) | is.na(itat_missing$pub)),]
itat_missing_pub<-itat_missing[(is.na(itat_missing$igor)),]
write.csv2(itat_missing_pub, file = "match_pub.csv")

library(dplyr)
itat_missing_pub2<-data.frame(filter(itat_missing_pub, itat_missing_pub$vegetation.type == 'Campo de Altitude'))
LSL_temp<-LSL_final[,c(1,17)]

LSL_final$itatiaia[5]=1
LSL_final$itatiaia[141]=1
LSL_final$itatiaia[173]=1
LSL_final$itatiaia[215]=1
LSL_final$itatiaia[275]=1
LSL_final$itatiaia[403]=1
LSL_final$itatiaia[463]=1
LSL_final$itatiaia[479]=1
LSL_final$itatiaia[495]=1
LSL_final$itatiaia[497]=1
LSL_final$itatiaia[528]=1
LSL_final$itatiaia[536]=1
LSL_final$itatiaia[584]=1
LSL_final$itatiaia[618]=1
LSL_final$itatiaia[658]=1
LSL_final$itatiaia[661]=1
LSL_final$itatiaia[742]=1
LSL_final$itatiaia[658]=1
LSL_final$itatiaia[784]=1
LSL_final$itatiaia[784]=1
LSL_final$itatiaia[811]=1
LSL_final$itatiaia[860]=1
LSL_final$itatiaia[966]=1
LSL_final$itatiaia[1031]=1
LSL_final$itatiaia[1042]=1
LSL_final$itatiaia[1064]=1
LSL_final$itatiaia[1088]=1
LSL_final$itatiaia[1161]=1
LSL_final$itatiaia[1176]=1
LSL_final$itatiaia[1182]=1
LSL_final$itatiaia[1235]=1
LSL_final$itatiaia[1266]=1
LSL_final$itatiaia[1297]=1
LSL_final$itatiaia[1307]=1
LSL_final$itatiaia[1327]=1
LSL_final$itatiaia[1363]=1
LSL_final$itatiaia[1364]=1
LSL_final$itatiaia[1366]=1
LSL_final$itatiaia[1368]=1
LSL_final$itatiaia[1412]=1
LSL_final$itatiaia[1414]=1
LSL_final$itatiaia[1486]=1
LSL_final$itatiaia[1700]=1
LSL_final$itatiaia[1734]=1
LSL_final$itatiaia[1775]=1
LSL_final$itatiaia[1797]=1
LSL_final$itatiaia[1818]=1
LSL_final$itatiaia[1872]=1
LSL_final$itatiaia[2050]=1
LSL_final$itatiaia[2074]=1
LSL_final$itatiaia[2180]=1
LSL_final$itatiaia[2202]=1
LSL_final$itatiaia[2249]=1
LSL_final$itatiaia[2274]=1
LSL_final$mina[623]=1
LSL_final<-LSL_final[-c(624),]

to_include<-read.csv("/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs2/to_include_itat_pub.csv", sep=";")
LSL_final2<-merge(LSL_final, to_include,all.x = T, all.y = T)
LSL_final2[,c(15:29)][is.na(LSL_final2[,c(15:29)])] <- '0'
write.csv2(LSL_final2, file = 'LSL_final2.csv')

SSL_final2 <-subset(LSL_final2, grepl("Campo de Altitude", LSL_final2$vegetation.type))
write.csv2(SSL_final2, file = 'SSL_final2.csv')

############################# Including Pub list of Itatiaia ######################################

itatiaia_SSL_clean<-data.frame(filter(SSL_final, SSL_final$itatiaia == 1))
itatiaia_SSL_clean<-subset(SSL_final, grepl('1', SSL_final$itatiaia))
itatiaia_LSL_clean<-subset(LSL_final, grepl('1', LSL_final$itatiaia))

itatiaia_SSL_clean$igor='1'
itat_CA2$pub='1'
colnames(itatiaia_SSL_clean)
colnames(itat_CA2)
itatiaia_SSL_clean<-itatiaia_SSL_clean[,c(1:14,30)]

itat_missing<-merge(itatiaia_SSL_clean, itat_CA2,all.x = T, all.y = T)
itat_missing2<-itat_missing[(is.na(itat_missing$igor) | is.na(itat_missing$pub)),]
itat_missing_pub<-itat_missing[(is.na(itat_missing$igor)),]
write.csv2(itat_missing_pub, file = "match_pub2.csv")

library(dplyr)
itat_missing_pub2<-data.frame(filter(itat_missing_pub, itat_missing_pub$vegetation.type == 'Campo de Altitude'))
LSL_temp<-LSL_final2[,c(1,17)]

LSL_final$itatiaia[5]=1
LSL_final$itatiaia[141]=1
LSL_final$itatiaia[173]=1
LSL_final$itatiaia[215]=1
LSL_final$itatiaia[275]=1
LSL_final$itatiaia[403]=1
LSL_final$itatiaia[463]=1
LSL_final$itatiaia[479]=1
LSL_final$itatiaia[495]=1
LSL_final$itatiaia[497]=1
LSL_final$itatiaia[528]=1
LSL_final$itatiaia[536]=1
LSL_final$itatiaia[584]=1
LSL_final$itatiaia[618]=1
LSL_final$itatiaia[658]=1
LSL_final$itatiaia[661]=1
LSL_final$itatiaia[742]=1
LSL_final$itatiaia[658]=1
LSL_final$itatiaia[784]=1
LSL_final$itatiaia[784]=1
LSL_final$itatiaia[811]=1
LSL_final$itatiaia[860]=1
LSL_final$itatiaia[966]=1
LSL_final$itatiaia[1031]=1
LSL_final$itatiaia[1042]=1
LSL_final$itatiaia[1064]=1
LSL_final$itatiaia[1088]=1
LSL_final$itatiaia[1161]=1
LSL_final$itatiaia[1176]=1
LSL_final$itatiaia[1182]=1
LSL_final$itatiaia[1235]=1
LSL_final$itatiaia[1266]=1
LSL_final$itatiaia[1297]=1
LSL_final$itatiaia[1307]=1
LSL_final$itatiaia[1327]=1
LSL_final$itatiaia[1363]=1
LSL_final$itatiaia[1364]=1
LSL_final$itatiaia[1366]=1
LSL_final$itatiaia[1368]=1
LSL_final$itatiaia[1412]=1
LSL_final$itatiaia[1414]=1
LSL_final$itatiaia[1486]=1
LSL_final$itatiaia[1700]=1
LSL_final$itatiaia[1734]=1
LSL_final$itatiaia[1775]=1
LSL_final$itatiaia[1797]=1
LSL_final$itatiaia[1818]=1
LSL_final$itatiaia[1872]=1
LSL_final$itatiaia[2050]=1
LSL_final$itatiaia[2074]=1
LSL_final$itatiaia[2180]=1
LSL_final$itatiaia[2202]=1
LSL_final$itatiaia[2249]=1
LSL_final$itatiaia[2274]=1
LSL_final$mina[623]=1
LSL_final<-LSL_final[-c(624),]

to_include<-read.csv("/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs2/to_include_itat_pub.csv", sep=";")
LSL_final2<-merge(LSL_final, to_include,all.x = T, all.y = T)
LSL_final2[,c(15:29)][is.na(LSL_final2[,c(15:29)])] <- '0'
write.csv2(LSL_final2, file = 'LSL_final2.csv')

SSL_final2 <-subset(LSL_final2, grepl("Campo de Altitude", LSL_final2$vegetation.type))
write.csv2(SSL_final2, file = 'SSL_final2.csv')


