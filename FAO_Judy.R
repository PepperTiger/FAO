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
data <- pivot %>% rename("production"= Production,
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
                         "food_supply_kgcapitaday" = `Food supply (kcal/capita/day)`,
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




