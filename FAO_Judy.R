library("tidyverse")

###############################################################################
### Loading csv files 
###############################################################################

animal <- read.csv("data/animal.csv")
vegetable <- read.csv("data/veg.csv")
cereal <- read.csv("data/cereals.csv")
malnut <- read.csv("data/malnut.csv")
pop <- read.csv("data/pop.csv")

###############################################################################
### Creation dataframe principale des bilans alimentaires
###############################################################################

# Adding origin column
animal <- mutate(animal, origin = "animal")
vegetable <- mutate(vegetable, origin = "vegetable")

#Adding the animal and vegetables tables together and renaming columns
temp <- rbind(animal, vegetable)
temp <- subset(temp, select = -c(Domain.Code, Domain))
temp <- temp %>% rename("x" = X,
                        "country_code" = Area.Code,
                        "country" = Area,
                        "element_code" = Element.Code,
                        "element" = Element,
                        "item_code" = Item.Code,
                        "item" = Item,
                        "year_code" = Year.Code,
                        "year" = Year,
                        "unit" = Unit,
                        "value" = Value,
                        "flag" = Flag,
                        "flag_desc" = Flag.Description)

# Unités différentes pour chaque "element" : suppression de la colonne unit, pivot de la table, colones renomées
temp2 <- temp %>% select(-c(x,unit,element_code,flag, flag_desc, year_code))
pivot <- temp2 %>% pivot_wider(names_from = element, values_from = value, values_fn = list(value = sum))
bilan <- pivot %>% rename("production"= Production,
                         "import_quantity" = `Import Quantity`,
                         "stock_variation" = `Stock Variation`,
                         "export_quantity" = `Export Quantity`,
                         "domestic_supply_quantity" = `Domestic supply quantity`,
                         "feed" = Feed,
                         "seed" = Seed,
                         "loss" = Losses,
                         "processing" = Processing,
                         "other_uses" = `Other uses (non-food)`,
                         "tourist_consumption" = `Tourist consumption`,
                         "residuals" = Residuals,
                         "food" = Food,
                         "food_supply_kgcapitayr" = `Food supply quantity (kg/capita/yr)`,
                         "food_supply_kcalcapitaday" = `Food supply (kcal/capita/day)`,
                         "protein_supply_gcapitaday" = `Protein supply quantity (g/capita/day)`,
                         "fat_supply_gcapitaday" = `Fat supply quantity (g/capita/day)`)


###############################################################################
### Creation dataframe population
###############################################################################

population <- pop %>% 
  select(c(Area.Code, Area, Element, Year, Value)) %>% 
  rename("country_code" = Area.Code, "country" = Area, "year" = Year) %>%
  mutate(Value = Value*1000) %>%
  pivot_wider(names_from = Element, values_from = Value) %>% 
  rename("total_population" = `Total Population - Both sexes`, "rural_population" = `Rural population`, "urban_population" = `Urban population`)

# Q.1.1 : Population mondiale par année
population %>% filter(country_code != 351) %>% group_by(year) %>% summarise("population mondiale" = sum(total_population))


###############################################################################
### Creation dataframe dispo_alim
###############################################################################

#Join population avec la table bilan sur le code pays
bilan_pop <- bilan %>% inner_join(population %>% select(c(country_code, year, total_population)), by=c("country_code","year"))

#Creation table dispo alimentaire : avec dispo_alim_tonnes = tonnes de nourriture disponible pour toute la population pour toute l'année
dispo_alim <- bilan_pop %>% mutate("dispo_alim_tonnes" = round(food_supply_kgcapitayr*total_population/1000)) %>%
                select(c(country, country_code, year, item, item_code, origin, dispo_alim_tonnes, food_supply_kcalcapitaday, protein_supply_gcapitaday, fat_supply_gcapitaday, total_population)) %>%
                  rename("dispo_alim_kcal_p_j" = food_supply_kcalcapitaday, 
                         "dispo_prot_gr_p_j" = protein_supply_gcapitaday,
                         "dispo_gr_p_j"= fat_supply_gcapitaday)

###############################################################################
### Céréales
###############################################################################

# Q.2.1.1 : Liste des cereales selon FAO
cereal %>% select(c(Item,Item.Code)) %>% rename("Produit" = Item, "Code Produit" = Item.Code)

# Creation is_cereal (True/False) column in bilan
bilan <- bilan %>% mutate("is_cereal" = ifelse(item_code %in% cereal$Item.Code, TRUE, FALSE))

# Q.2.1.2 : Proportion des céréales pour l???alimentation animale?
bilan %>% filter(is_cereal, country_code != 351) %>% 
  select(year, food, feed) %>% drop_na() %>% 
  mutate("feed_percent" =ifelse((food+feed) != 0, feed*100/(food+feed),NA)) %>% 
  drop_na(feed_percent) %>% group_by(year) %>% 
  summarise("feed_percent" = mean(feed_percent))

###############################################################################
### 2.2 Comment mesure-t-on la disponibilité alimentaire ?
###############################################################################




