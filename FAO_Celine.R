library("stringr")
library("tidyverse")

# Read the folder of csv files
fichiers<-list.files(path = "C:/Users/celine/environnement_R_studio/FAO/fichiers_csv", pattern = NULL, all.files = FALSE,
                     full.names = FALSE, recursive = FALSE,
                     ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#put csv files in tables
for (i in 1:length(fichiers)){
  assign(x=str_sub(fichiers[i],1,-5),
         value=read.csv(paste0("C:/Users/celine/environnement_R_studio/FAO/fichiers_csv/",fichiers[i])))
}



#Concat√©nation des dataframe bilan_prod_animaux et bilan_vegetaux en ajoutant la colonne "origin"

bilan_prod_animaux$origin<-"animal"
temp<-bilan_prod_animaux

for (i in c('4','5','6','7')){
  df<-get(paste0("bilan_vegetaux_201",i))
  df$origin="vegetal"
  temp<-bind_rows(temp,df)
}


# cr√©ation d'un nouveau dataframe 
colnames(temp)
temp2<-temp%>%select(-c(1,2,5,9,13,14))


# On v√©rifie les unit√©s pour les diff√©rents champs de la variable Element de temp2 afin de pouvoir transposer notre table
l<-unique(temp2$Element)

for (i in c(1:13)){
  v<-temp2%>%filter(Element==l[i],Unit!="1000 tonnes")
  print(dim(v)[1])
}

# les colonnes Production,"Import Quantity","Stock Variation","Export Quantity","Domestic supply quantity"..."food"
#sont en milliers de tonnes.

#on transpose la table et on modifie les noms des colonnes
temp2<-temp2%>%select(-Unit)


prod_t<-temp2%>%pivot_wider(names_from = Element, values_from = Value)


prod_t<-prod_t%>%rename("country_code"=Area.Code,
                        "country"=Area,
                        "item_code"=Item.Code,
                        "item"=Item,
                        "year"=Year,
                        "production_1000t"=Production,
                        "import_qu_1000t"=`Import Quantity`,
                        "stock_var_1000t"=`Stock Variation`,
                        "export_qu_1000t"=`Export Quantity`,
                        "domestic_supply_qu_1000t"=`Domestic supply quantity`,
                        "feed_1000t"=Feed,
                        "seed_1000t"=Seed,
                        "losses_1000t"=Losses,
                        "processing"=Processing,
                        "other_uses"=`Other uses (non-food)`,
                        "tourist_consumption_1000t"=`Tourist consumption`,
                        "residuals_1000t"=Residuals,
                        "food_1000t"=Food,
                        "food_sup_qu_kg/cpt/yr"=`Food supply quantity (kg/capita/yr)`,
                        "food_sup_qu_kcal/cpt/day" = `Food supply (kcal/capita/day)`,
                        "protein_sup_qu_g/cpt/day"=`Protein supply quantity (g/capita/day)`,
                        "fat_sup_qu_g/cpt/day"=`Fat supply quantity (g/capita/day)`)

prod_t<-prod_t%>%filter(country_code!=351)

####################################

#Cr√©ation de la table population.

population_t<- population_data%>%
  rename('country'=Area,'country_code'=Area.Code,'year'=Year)%>%
  mutate(population=Value*1000)%>%
  select(country,country_code,year,population)

# population_t%>%filter(grepl("china",country,ignore.case=TRUE))%>%distinct(country,.keep_all = TRUE)


#taille de la population mondiale pour chaque ann√©e depuis 2014 : 
population_t%>%filter(country_code!=351)%>%group_by(year)%>%summarise(total=sum(population,na.rm=TRUE))

####Cr√©ation de la table dispo_alim

dispo_alim<-prod_t%>% mutate(food_sup_t=food_1000t*1000) %>%
  select(country,country_code,year,item,item_code, origin,`food_sup_qu_kcal/cpt/day`,
         food_sup_t,`protein_sup_qu_g/cpt/day`,`fat_sup_qu_g/cpt/day`)

#c<-dispo_alim%>%filter(grepl("china",country,ignore.case=TRUE))%>%distinct(country,.keep_all = TRUE)


#Cr√©ation de la table c√©r√©ales avec la colonne is_cereal

cereal<-dispo_alim%>%
  select(item,item_code) %>%
  filter(grepl("(cereals|wheat|rye|rice|barley|oats|corn|maize|millet|sorghum)",item,ignore.case = TRUE),
         grepl("oil",item,ignore.case = TRUE)==FALSE)%>%distinct()


prod_t<-prod_t%>%mutate(is_cereal=ifelse(item %in% cereal$item,TRUE,FALSE))

#calcul de la proportion de nourriture c√©r√©ali√®res destin√©e aux animaux depuis 2014.
#https://onedrive.live.com/view.aspx?resid=6C6D756296D4662!652089&ithint=file%2cdocx&authkey=!APSzmMiTAL9ZH0s

prod_t%>%filter(item %in% cereal$item)%>%group_by(year)%>%
  summarise(prop=sum(feed_1000t,na.rm=TRUE)/(sum(food_1000t,na.rm=TRUE)+sum(feed_1000t,na.rm=TRUE)))

###################‚
#Disponibilit√© alimentaire par pays et produit

# food_supply<-prod_t%>%
#              arrange(country_code,year)

jointure<-left_join(prod_t,population_t,by=c("country_code","country","year"))

#ratio energetique en kcal/kg
# 
# jointure<-jointure%>%mutate(food_sup_qu_kcal=population*365*`food_sup_qu_kcal/cpt/day`,
#                   protein_sup_kg=`protein_sup_qu_g/cpt/day`*population*365/1000)

jointure<-jointure%>%
  mutate(ratio_kcal_kg=ifelse(food_1000t!=0,round(`food_sup_qu_kcal/cpt/day`*365*population/(food_1000t*1000*1000),2),NA))


#ratio proteinique 

jointure<-jointure%>%mutate(ratio_protein_prct=ifelse(`food_sup_qu_kg/cpt/yr`!=0,
                                                 100*`protein_sup_qu_g/cpt/day`*365/(1000*`food_sup_qu_kg/cpt/yr`),
                                                 NA))


#les 20 aliments les plus riches en calories
jointure_clean<-jointure%>%filter(ratio_kcal_kg>20,ratio_kcal_kg<=9200)


foodmoy<- jointure_clean%>% 
  select(country,year,item,ratio_kcal_kg,ratio_protein_prct)%>%
  group_by(item)%>%
  summarise(moykcal_kg=mean(ratio_kcal_kg,na.rm=TRUE),moyprot=mean(ratio_protein_prct,na.rm=TRUE))%>%
  arrange(desc(moykcal_kg))%>%
  head(20)

foodmoy$moyprot<-formatC(foodmoy$moyprot,format="g", digits=5)

#les 20 aliments les plus riches en protÈines 


foodmoyprot<-jointure_clean%>%
  select(country,year,item,ratio_kcal_kg,ratio_protein_prct)%>%
  group_by(item)%>%
  summarise(moykcal_kg=mean(ratio_kcal_kg,na.rm=TRUE),moyprot=mean(ratio_protein_prct,na.rm=TRUE))%>%
  arrange(desc(moyprot))%>%
  head(20)

# 2.4 disponibilitÈ calorique mondiale

dispo_veg_kcal<-jointure_clean %>%filter(origin=="vegetal")%>%
                  mutate(dom_sup_kcal_country=domestic_supply_qu_1000t*1000000*ratio_kcal_kg)%>%
                  group_by(year)%>%
                  summarise(dom_sup_kcal=sum(dom_sup_kcal_country,na.rm=TRUE))

  #disponibilite proteique en kg


dispo_veg_prot<-jointure_clean%>%filter(origin=="vegetal")%>%
                 mutate(dom_sup_kg_prot_country=domestic_supply_qu_1000t*1000000*ratio_protein_prct*0.01)%>%
                 group_by(year)%>%
                 summarise(dom_sup_kg_prot=sum(dom_sup_kg_prot_country,na.rm=TRUE))
  
dispo_veg_prot$dom_sup_kg_prot<-as.numeric(formatC(dispo_veg_prot$dom_sup_kg_prot,format="e",digits=5))


ggplot(dispo_veg_kcal)+geom_point(aes(x=year,y=dom_sup_kcal))

ggplot(dispo_veg_prot)+ geom_point(aes(x=year,y=dom_sup_kg_prot))

#Combien d'humains pourraient Ítre nourris 
#si toute la disponibilitÈ intÈrieure mondiale de produits vÈgÈtaux Ètait utilisÈe pour de la nourriture
#2000 kcal par j/humain

a<-dispo_veg_kcal%>%mutate(nb_pers_nourries=(dom_sup_kcal/365)/2500)
                           
a$nb_pers_nourries<-formatC(a$nb_pers_nourries,format="e",digits=5)

a


b<-dispo_veg_prot%>%mutate(nb_pers_prot=as.numeric(dom_sup_kg_prot/365)/0.06)
b$nb_pers_prot<-formatC(b$nb_pers_prot,format="e",digits=5)

b


# 
# # Nombre de calories par jour et par personne
# NB_KCAL_PER_CAPITA_PER_DAY = 2500
# 
# # Poids moyen d'un humain : 62kg (https://en.wikipedia.org/wiki/Human_body_weight)
# # Besoin en protÈines moyens pour un humain : 0.9 g/kg/jour
# KG_PROT_PER_CAPITA_PER_DAY = 62 * .9 * .001
# YEAR = 2013
# 
# population = dom_sup.loc[YEAR, "dom_sup_kcal"] / 365 / NB_KCAL_PER_CAPITA_PER_DAY
# 
# 
#   