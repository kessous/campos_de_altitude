################ Use the original 'backbone_list.R' to reach this step #####################################
############################# FILTER BY WORDS AND FIXED ELEVATION 1800 #####################################

ang_1800_cl=data.frame(filter(ang_1500_cl, ang_1500_cl$elev_high >=1800))

elev2<-data.frame(filter(sp_vr_occ_gbif2, sp_vr_occ_gbif2$elevation >=1800))
#CA<- subset(sp_vr_occ_gbif2, grepl("Campo de altitude", sp_vr_occ_gbif2$locality))
#CAs<- subset(sp_vr_occ_gbif2, grepl("Campos de altitude", sp_vr_occ_gbif2$locality))
#ca<- subset(sp_vr_occ_gbif2, grepl("campo de altitude", sp_vr_occ_gbif2$locality))
#cas<- subset(sp_vr_occ_gbif2, grepl("campos de altitude", sp_vr_occ_gbif2$locality))

CASS<-rbind(CA,CAs,ca,cas)
CASS<-CASS[(is.na(CASS$elevation) | CASS$elevation >= 1800),]

words2<-rbind(elev2,CASS)
words2<-words2[!(words2$stateProvince %in% c('Amazonas','amazonas','AM', 'Roraima','roraima','RR')), ]
words2<-unique(words2)

list_backbone_1800<-bind_rows(ang_1800_cl,words2)
list_backbone_1800<- list_backbone_1800[order(list_backbone_1800$uc),]
list_backbone_1800<- list_backbone_1800[order(list_backbone_1800$scientificName),]
xy_1800<-unique(list_backbone_1800$scientificName)
#write.csv2(list_backbone, file = 'list_backbone.csv')


############################# FINISH BACKBONE COORDINATES + NO COORDINATES #####################################


################################################################################################################
################################################################################################################


############################# CORRECTION OF THE BACKBONE LIST #################################################

unique(list_backbone_1800$stateProvince)
list_backbone_1800<-list_backbone_1800[!(list_backbone_1800$stateProvince %in% c('Bahia','bahia','BA', 'Ba','Goiás','Sergipe','Bom Retiro',
                                                                  'Distrito Federal', 'Df','Go','Rio Grande do Norte',
                                                                  'Mato Grosso do Sul','Tocantins','Pará','Mato Grosso','Goi s',
                                                                  'Acre','Pernambuco','Maranhão', 'Par','Paraíba','Rondônia','Diamantina',
                                                                  'Caeté','Piauí','Pinhão','Goias','Amapá','Alagoas','Fernando de Noronha', 'La Paz',
                                                                  'Giruá','Am','Curitiba','Rond nia', 'Peru', 'Río Negro', 'Estado da Bahia','State of Goiás',
                                                                  'Salta','Amazonas','São Joaquim',"Maranh o","Salvador","Estado Da Bahia","Santo André","amapa",
                                                                  "Ceará" ,"Planalto do Brasil" ,"Roraima (Terr.)" ,"Brasília Distrito Federal","Cundinamarca",
                                                                  "Mato Grosso (Former)","Serra dos Surucucus","Ro","Estado de Goias", "Ms","Santa Cruz","Estado de Roraima", 
                                                                  "Serra de Espinhaco","Province de Loja" ,"Campo Mourão" ,"Alto Paraíso de Goiás")), ]

unique(list_backbone_1800$stateProvince)

############## Conditionally exclude NAs  ##############
list_backbone2_1800<-list_backbone_1800[!(is.na(list_backbone_1800$locality) & is.na(list_backbone_1800$uc) & is.na(list_backbone_1800$stateProvince)),]
list_backbone3_1800<-list_backbone2_1800[!(is.na(list_backbone2_1800$locality) & is.na(list_backbone2_1800$uc)),]


##########different from others: 'bocaina de minas' included in APA Mantiqueira/papagaio
############## Excluding rows with selected keywords and coordinate ##############
l1<-list_backbone3_1800[!(is.na(list_backbone3_1800$uc)),]
list_backbone3_1800<-subset(list_backbone3_1800, is.na(list_backbone3_1800$uc))
x<-"aeroporto|arbusto|árvore|arvore|arbórea|arborea|bloqueado|o. pret.|canta galo|flor|fruto|campo rupestre|rupestre|afloramento|itaobim|medina|cachoeira que congela|jacupiranga|Abaíra|Serra do Ray|Small river |Arapiranga|corumba|corumbá|mucug|ouro preto|espomoso|espumoso|Caneorios|canoeiros|canoeirso|lenheiro|paracatu|paracatú|belo horizonte|curvelo|montes claros|serro|salinas|diamantina|diamantiana|alto paraiso|alto paraíso|bocaína funda|bocaina funda|capanema|Minas Geraës|pico das almas|rio de contas|ouro branco|itabirito|Rio Negro|Ibitipoca|ibitipoca|Gandarela| cipó| cipo| Cipó| Cipo|neblina|Neblina|itacolomi|Itacolomi|Itacolomy|itacolomí|espinhaço|espinhaco|Espinhaço|Espinhaco|caraça|caraca|Caraça|Caraca|itambé| itambe|Itambé|Itambe|Bahia|bahia|Goiás|Sergipe|Bom Retiro|Distrito Federal|Df|Rio Grande do Norte|Mato Grosso do Sul|Tocantins|Pará|Mato Grosso|Gois|Acre|Pernambuco|Maranhão|Rondônia|Diamantina|Caeté|Piauí|Pinhão|Goias|Amapá|Alagoas|Fernando de Noronha|La Paz|Giruá|Curitiba|Rondnia|Peru|Río Negro|Estado da Bahia|State of Goiás|Salta|Amazonas|São Joaquim|Maranho|Salvador|Estado Da Bahia|Santo André|Ceará|Planalto do Brasil|Roraima(Terr.)|Brasília Distrito Federal|Cundinamarca|Mato Grosso (Former)|Serra dos Surucucus|Estado de Goias|Ms|Santa Cruz|Estado de Roraima|Serra de Espinhaco|Province de Loja|Campo Mourão|Alto Paraíso de Goiás"
list_backbone3_1800<-list_backbone3_1800[!grepl(x,list_backbone3_1800$locality,ignore.case = T),]
list_backbone4_1800<-rbind(list_backbone3_1800,l1)

l1_1800<-list_backbone4_1800[!(is.na(list_backbone4_1800$decimalLatitude)),]
l1_1800<-data.frame(filter(l1_1800, l1_1800$decimalLatitude <= -19))
l1_1800<-data.frame(filter(l1_1800, l1_1800$decimalLongitude <= -37))
l1_1800<-data.frame(filter(l1_1800, l1_1800$decimalLongitude >= -52))
list_backbone4_1800<-subset(list_backbone4_1800, is.na(list_backbone4_1800$decimalLatitude))
list_backbone4_1800<-rbind(list_backbone4_1800,l1_1800)

#write.csv2(list_backbone4_1800, file = 'list2.csv')

############## Adding UCs into NAs (by keywords)  ##############


list_na_1800<-subset(list_backbone4_1800, is.na(list_backbone4_1800$uc))
list_non_na_1800<-subset(list_backbone4_1800, (!is.na(list_backbone4_1800$uc)))

x<- subset(list_na_1800, grepl("caparao|caparaó|capara|pico da bandeira|macieira|lajao|lajão|macierias|es, campo de altitude, comum|troqueira|troqnquiera|terreirao|terreirão", list_na_1800$locality, ignore.case = T))
x$uc=933
list_na_1800<-list_na_1800[!grepl("caparao|caparaó|capara|pico da bandeira|macieira|lajao|lajão|macierias|es, campo de altitude, comum|troqueira|troqnquiera|terreirao|terreirão",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x)

x<- subset(list_na_1800, grepl("itatia|itatiaia|itatiaya|agulhas negras|. negras|prateleira|itahaya|rebouças|reboucas|planalto", list_na_1800$locality, ignore.case = T))
x$uc=949
list_na_1800<-list_na_1800[!grepl("itatia|itatiaia|itatiaya|agulhas negras|. negras|prateleira|itahaya|rebouças|reboucas|planalto",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x)  
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("orgãos|orgaos|do sino|organ|órgãos|órgaos|Org. |orgais|das antas|dao antas", list_na_1800$locality, ignore.case = T))
x$uc=90
list_na_1800<-list_na_1800[!grepl("orgãos|orgaos|do sino|organ|órgãos|órgaos|Org. |orgais|das antas|dao antas",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("da mina|papagaio|serra fina", list_na_1800$locality, ignore.case = T))
x$uc=213
list_na_1800<-list_na_1800[!grepl("da mina|papagaio|serra fina",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("do baú|do bau|do paiol|bauzinho", list_na_1800$locality, ignore.case = T))
x$uc=389
list_na_1800<-list_na_1800[!grepl("do baú|do bau|do paiol|bauzinho",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("Jordào|jordao|jordão|fazenda da onça|fazenda do onça|são josé dos alpes|sao jose dos alpes|p. e. c. j.|pico do ataque|morro do ataque|itaguaré|taguaré|taguare", list_na_1800$locality, ignore.case = T))
x$uc=22
list_na_1800<-list_na_1800[!grepl("Jordào|jordao|jordão|fazenda da onça|fazenda do onça|são josé dos alpes|sao jose dos alpes|p. e. c. j.|pico do ataque|morro do ataque|itaguaré|taguaré|taguare",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("três picos|tres picos|3 picos|Caledônia|Caledônea|Caledonia|Caledonea", list_na_1800$locality, ignore.case = T))
x$uc=41
list_na_1800<-list_na_1800[!grepl("três picos|tres picos|3 picos|Caledônia|Caledônea|Caledonia|Caledonea",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("macae|macaé", list_na_1800$locality, ignore.case = T))
x$uc=42
list_na_1800<-list_na_1800[!grepl("macae|macaé",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("aracatuba|araçatuba", list_na_1800$locality, ignore.case = T))
x$uc=145
list_na_1800<-list_na_1800[!grepl("aracatuba|araçatuba",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("desengano|santa maria madalena|magdalena|alto da república|alto da republica", list_na_1800$locality, ignore.case = T))
x$uc=788
list_na_1800<-list_na_1800[!grepl("desengano|santa maria madalena|magdalena|alto da república|alto da republica",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("forno grande", list_na_1800$locality, ignore.case = T))
x$uc=790
list_na_1800<-list_na_1800[!grepl("forno grande",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("serra negra", list_na_1800$locality, ignore.case = T))
x$uc=881
list_na_1800<-list_na_1800[!grepl("serra negra",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("bocaina", list_na_1800$locality, ignore.case = T))
x$uc=910
list_na_1800<-list_na_1800[!grepl("bocaina",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("sao joaquim|são joaquim|da igreja|campo do padre|campo dos padres|campos dos padres|campos do padre|los padres|santa bárbara|urubici|pedra furada|corvo branco", list_na_1800$locality, ignore.case = T))
x$uc=937
list_na_1800<-list_na_1800[!grepl("sao joaquim|são joaquim|da igreja|campo do padre|campo dos padres|campos dos padres|campos do padre|los padres|santa bárbara|urubici|pedra furada|corvo branco",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("da farofa", list_na_1800$locality, ignore.case = T))
x$uc=1923
list_na_1800<-list_na_1800[!grepl("da farofa",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

####Differnt from others: bocaina de minas included
x<- subset(list_na_1800, grepl("bocaina de minas|dos marins", list_na_1800$locality, ignore.case = T))
x$uc=213
list_na_1800<-list_na_1800[!grepl("bocaina de minas|dos marins",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("brigadeiro|PESB", list_na_1800$locality, ignore.case = T))
x$uc="PE Serra do Brigadeiro"
list_na_1800<-list_na_1800[!grepl("brigadeiro|PESB",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("araras|A.P.A. Petrópolis| cuca", list_na_1800$locality, ignore.case = T))
x$uc="APA Petrópolis"
list_na_1800<-list_na_1800[!grepl("araras|A.P.A. Petrópolis| cuca",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

x<- subset(list_na_1800, grepl("pico do parana|pico do paraná|caratuva|caratuava", list_na_1800$locality, ignore.case = T))
x$uc="PE Pico do Paraná"
list_na_1800<-list_na_1800[!grepl("pico do parana|pico do paraná|caratuva|caratuava",list_na_1800$locality, ignore.case = T),] 
list_na_1800<-rbind(list_na_1800,x) 
unique(list_na_1800$uc)

short.na_1800<-list_na_1800[,c(13,17,18,22,23,53)]
short.na_1800<-subset(short.na_1800, is.na(short.na_1800$uc))
summary(is.na(short.na_1800$uc))

list_clean_1800<-rbind(list_non_na_1800,list_na_1800)
list_clean_1800$uc[list_clean_1800$uc == "1923"] <- "RPPN Serra da Farofa"
list_clean_1800$uc[list_clean_1800$uc == "949"] <- "PARNA Itatiaia"
list_clean_1800$uc[list_clean_1800$uc == "933"] <- "PARNA Caparaó"
list_clean_1800$uc[list_clean_1800$uc == "1019"] <- "Pedra de São Domingos"
list_clean_1800$uc[list_clean_1800$uc == "151"] <- "Pedra de São Domingos"
list_clean_1800$uc[list_clean_1800$uc == "213"] <- "PE Papagaio/Pedra da Mina/APA Mantiqueira"
list_clean_1800$uc[list_clean_1800$uc == "342"] <- "PE Campos do Jordão"
list_clean_1800$uc[list_clean_1800$uc == "389"] <- "MONA Pedra do Baú"
list_clean_1800$uc[list_clean_1800$uc == "937"] <- "PARNA São Joaquim"
list_clean_1800$uc[list_clean_1800$uc == "790"] <- "PE Forno Grande"
list_clean_1800$uc[list_clean_1800$uc == "90"] <- "PARNA Serra dos Órgãos"
list_clean_1800$uc[list_clean_1800$uc == "109"] <- "Serra da Bandeira"
list_clean_1800$uc[list_clean_1800$uc == "145"] <- "Serra de Araçatuba"
list_clean_1800$uc[list_clean_1800$uc == "788"] <- "PE Desengano"
list_clean_1800$uc[list_clean_1800$uc == "755"] <- "PE Campos do Jordão"
list_clean_1800$uc[list_clean_1800$uc == "910"] <- "PARNA Bocaina"
list_clean_1800$uc[list_clean_1800$uc == "881"] <- "Serra Negra"
list_clean_1800$uc[list_clean_1800$uc == "22"] <- "PE Campos do Jordão"
list_clean_1800$uc[list_clean_1800$uc == "41"] <- "PE Três Picos"
list_clean_1800$uc[list_clean_1800$uc == "42"] <- "APA Macaé de Cima"
list_clean_1800$uc[list_clean_1800$uc == "79"] <- "PARNA Itatiaia"
unique(list_clean_1800$uc)


######short lists
short.na2_1800<-unique(list_clean_1800[,c(8,13,53)])
#short.na2_1800<-unique(list_clean_1800[,c(8,10,13,17,18,22,23,33,38,39,40,45,51,53)])
short.na2_1800<- short.na2_1800[order(short.na2_1800$uc),]
short.na2_1800<- short.na2_1800[order(short.na2_1800$scientificName),]
short.na2_1800<- short.na2_1800[order(short.na2_1800$family),]
xy_1800<-unique(list_clean_1800$scientificName)

itatiaia2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =='PARNA Itatiaia'))
PARNASO2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =='PARNA Serra dos Órgãos'))
capara2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =='PARNA Caparaó'))
bocaina2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =='PARNA Bocaina'))
cjordao2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="PE Campos do Jordão"))
papagaio2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="PE Papagaio/Pedra da Mina/APA Mantiqueira"))
farofa2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="RPPN Serra da Farofa"))
sao_domingos2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="Pedra de São Domingos"))
bau2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="MONA Pedra do Baú"))
sao_joaquim2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="PARNA São Joaquim"))
f_grande2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="PE Forno Grande"))
brigadeiro2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="PE Serra do Brigadeiro"))
apa_petropolis2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="APA Petrópolis"))
parana2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="PE Pico do Paraná"))
bandeira2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="Serra da Bandeira"))
aracatuba2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="Serra de Araçatuba"))
desengano2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="PE Desengano"))
s_negra2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="Serra Negra"))
tres_picos2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="PE Três Picos"))
macae2<-data.frame(filter(short.na2_1800, short.na2_1800$uc =="APA Macaé de Cima"))

#########vouchers
list_clean_1800$uc<-rm_accent(list_clean_1800$uc)
list_clean_1800$locality<-rm_accent(list_clean_1800$locality)
list_clean_1800$recordedBy<-rm_accent(list_clean_1800$recordedBy)
list_clean_1800$stateProvince<-rm_accent(list_clean_1800$stateProvince)
setwd('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists')
write.csv2(list_clean_1800[,c(1,10,13,17,18,22,23,45,40,39,38,53)], file = 'vouchers_1800.csv')
