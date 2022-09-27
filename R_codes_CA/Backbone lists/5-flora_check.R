################# Generate lists with names according Flora do Brasil ###############
################# The objects came from "backbone_list ...." #######################################
library(flora)

#####Itatiaia >2000 m 

itatiaia3$scientificName<-sapply(itatiaia3$scientificName, remove.authors)
x<-unique(itatiaia3$scientificName)
itat<-get.taxa(itatiaia3$scientificName,life.form = T, habitat = T, vegetation.type = T,
vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(itat$scientific.name)
itat$life.form<-rm_accent(itat$life.form)
itat$habitat<-rm_accent(itat$habitat)
itat$vegetation.type<-rm_accent(itat$vegetation.type)
itat$vernacular.name<-rm_accent(itat$vernacular.name)
itat$domain<-rm_accent(itat$domain)
itat$endemism<-rm_accent(itat$endemism)
write.csv2(itat, file = 'itatiaia_checklist.csv')
itat_na <-itat[(is.na(itat$scientific.name)),]
itat_na2 <- itat_na[-c(1,3,5,6,10,11,12,15), ]
new_records_itat<- itat_na[c(1,3,5,6,10,11,12,15), ]
itat_na2 <- get.taxa(itat_na2$original.search,life.form = T, habitat = T, vegetation.type = T,
                    vernacular = T, states = T, establishment = T,domain = T, endemism = T, suggestion.distance = 0.5 )


##### Caparao >2000 m 

capara3$scientificName<-sapply(capara3$scientificName, remove.authors)
x<-unique(capara3$scientificName)
cap<-get.taxa(capara3$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(cap$scientific.name)
cap$life.form<-rm_accent(cap$life.form)
cap$habitat<-rm_accent(cap$habitat)
cap$vegetation.type<-rm_accent(cap$vegetation.type)
cap$vernacular.name<-rm_accent(cap$vernacular.name)
cap$domain<-rm_accent(cap$domain)
cap$endemism<-rm_accent(cap$endemism)
write.csv2(cap, file = 'caparao_checklist.csv')

##### PARNASO >1800m

PARNASO2$scientificName<-sapply(PARNASO2$scientificName, remove.authors)
x<-unique(PARNASO2$scientificName)
parn<-get.taxa(PARNASO2$scientificName,life.form = T, habitat = T, vegetation.type = T,
              vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(parn$scientific.name)
parn$life.form<-rm_accent(parn$life.form)
parn$habitat<-rm_accent(parn$habitat)
parn$vegetation.type<-rm_accent(parn$vegetation.type)
parn$vernacular.name<-rm_accent(parn$vernacular.name)
parn$domain<-rm_accent(parn$domain)
parn$endemism<-rm_accent(parn$endemism)
write.csv2(parn, file = 'parnaso_checklist.csv')

##### Campos do Jordão >1800m

cjordao2$scientificName<-sapply(cjordao2$scientificName, remove.authors)
x<-unique(cjordao2$scientificName)
cjor<-get.taxa(cjordao2$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(cjor$scientific.name)
cjor$life.form<-rm_accent(cjor$life.form)
cjor$habitat<-rm_accent(cjor$habitat)
cjor$vegetation.type<-rm_accent(cjor$vegetation.type)
cjor$vernacular.name<-rm_accent(cjor$vernacular.name)
cjor$domain<-rm_accent(cjor$domain)
cjor$endemism<-rm_accent(cjor$endemism)
write.csv2(cjor, file = 'campos_do_jordao_checklist.csv')


##### Bocaina >1500m

bocaina$scientificName<-sapply(bocaina$scientificName, remove.authors)
x<-unique(bocaina$scientificName)
boca<-get.taxa(bocaina$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(boca$scientific.name)
boca$life.form<-rm_accent(boca$life.form)
boca$habitat<-rm_accent(boca$habitat)
boca$vegetation.type<-rm_accent(boca$vegetation.type)
boca$vernacular.name<-rm_accent(boca$vernacular.name)
boca$domain<-rm_accent(boca$domain)
boca$endemism<-rm_accent(boca$endemism)
write.csv2(boca, file = 'bocaina_checklist.csv')

##### Sao joaquim >1300m

sao_joaquim4$scientificName<-sapply(sao_joaquim4$scientificName, remove.authors)
x<-unique(sao_joaquim4$scientificName)
joaq<-get.taxa(sao_joaquim4$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(joaq$scientific.name)
joaq$life.form<-rm_accent(joaq$life.form)
joaq$habitat<-rm_accent(joaq$habitat)
joaq$vegetation.type<-rm_accent(joaq$vegetation.type)
joaq$vernacular.name<-rm_accent(joaq$vernacular.name)
joaq$domain<-rm_accent(joaq$domain)
joaq$endemism<-rm_accent(joaq$endemism)
write.csv2(joaq, file = 'sao_joaquim_checklist.csv')

##### Pico do Parana >1300m

parana4$scientificName<-sapply(parana4$scientificName, remove.authors)
x<-unique(parana4$scientificName)
paran<-get.taxa(parana4$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(paran$scientific.name)
paran$life.form<-rm_accent(paran$life.form)
paran$habitat<-rm_accent(paran$habitat)
paran$vegetation.type<-rm_accent(paran$vegetation.type)
paran$vernacular.name<-rm_accent(paran$vernacular.name)
paran$domain<-rm_accent(paran$domain)
paran$endemism<-rm_accent(paran$endemism)
write.csv2(paran, file = 'pico_parana_checklist.csv')

##### Aracatuba >1300 m

aracatuba4$scientificName<-sapply(aracatuba4$scientificName, remove.authors)
x<-unique(aracatuba4$scientificName)
arac<-get.taxa(aracatuba4$scientificName,life.form = T, habitat = T, vegetation.type = T,
                vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(arac$scientific.name)
arac$life.form<-rm_accent(arac$life.form)
arac$habitat<-rm_accent(arac$habitat)
arac$vegetation.type<-rm_accent(arac$vegetation.type)
arac$vernacular.name<-rm_accent(arac$vernacular.name)
arac$domain<-rm_accent(arac$domain)
arac$endemism<-rm_accent(arac$endemism)
write.csv2(arac, file = 'aracatuba_checklist.csv')

##### APA Petropolis >1500 m

apa_petropolis$scientificName<-sapply(apa_petropolis$scientificName, remove.authors)
x<-unique(apa_petropolis$scientificName)
petrop<-get.taxa(apa_petropolis$scientificName,life.form = T, habitat = T, vegetation.type = T,
               vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(petrop$scientific.name)
petrop$life.form<-rm_accent(petrop$life.form)
petrop$habitat<-rm_accent(petrop$habitat)
petrop$vegetation.type<-rm_accent(petrop$vegetation.type)
petrop$vernacular.name<-rm_accent(petrop$vernacular.name)
petrop$domain<-rm_accent(petrop$domain)
petrop$endemism<-rm_accent(petrop$endemism)
write.csv2(petrop, file = 'apa_petropolis_checklist.csv')

##### APA Tres picos >1800 m

tres_picos2$scientificName<-sapply(tres_picos2$scientificName, remove.authors)
x<-unique(tres_picos2$scientificName)
t_picos<-get.taxa(tres_picos2$scientificName,life.form = T, habitat = T, vegetation.type = T,
                 vernacular = T, states = T, establishment = T,domain = T, endemism = T)
x<-unique(t_picos$scientific.name)
t_picos$life.form<-rm_accent(t_picos$life.form)
t_picos$habitat<-rm_accent(t_picos$habitat)
t_picos$vegetation.type<-rm_accent(t_picos$vegetation.type)
t_picos$vernacular.name<-rm_accent(t_picos$vernacular.name)
t_picos$domain<-rm_accent(t_picos$domain)
t_picos$endemism<-rm_accent(t_picos$endemism)
write.csv2(t_picos, file = 'tres_picos_checklist.csv')



