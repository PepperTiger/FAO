install.packages("sqldf")
library(sqldf)
options(scipen=999)
# 1. Les 10 pays ayant le plus haut ratio disponibilité alimentaire/habitant en
# termes de protéines (en kg) par habitant, puis en termes de kcal par habitant.

Q1_1_sql <- sqldf( 
  "SELECT country,round((sum([protein_sup_qu_g/cpt/day]/1000)),2) as dispo_prot_day_pers_kg
  FROM [jointure_clean]
  WHERE year = 2017
  GROUP BY country
  ORDER BY dispo_prot_day_pers_kg DESC 
  LIMIT 10;"
)

Q1_1_sql

Q1_2_sql <-sqldf( 
  "SELECT country,(sum([food_sup_qu_kcal/cpt/day])) as dispo_kcal_day_pers 
  FROM [jointure_clean]
  WHERE year = 2017
  GROUP BY country
  ORDER BY dispo_kcal_day_pers DESC 
  LIMIT 10;"
)

Q1_2_sql

#2. Pour chaque année disponible, les 10 pays ayant le plus faible ratio disponibilité
#alimentaire/habitant en termes de protéines (en kg) par habitant. Le nombre de
#lignes de la table renvoyée sera donc égal à 10 fois le nombre d'années disponibles.

Q2_sql <- sqldf("
SELECT * FROM (
  SELECT country, ratio_dispo_alim, year, ROW_NUMBER() OVER (
    PARTITION BY year 
    ORDER BY ratio_dispo_alim
    ) AS [ROW NUMBER]
  FROM (select country, round(avg([food_sup_qu_kcal/cpt/day]),2) as ratio_dispo_alim,year 
    FROM [dispo_alim_table]
    WHERE [food_sup_qu_kcal/cpt/day] > 0 
    GROUP BY country, year )
  ) groups
WHERE groups.[ROW NUMBER] <= 10
")

Q2_sql

### 3. La quantité totale (en kg) de produits perdus par pays et par année. La table
### renvoyée contiendra donc une ligne par couple (pays, année).

Q3_sql <- sqldf("
SELECT country, year, round(sum([losses_1000t]*1000000),2) AS waste_kg
FROM [prod_t]
WHERE losses_1000t is not null
GROUP BY country, year
ORDER BY waste_kg DESC
")

Q3_sql


#4. Les 10 pays pour lesquels la proportion de personnes sous-alimentées est la plus
#forte.

Q4_sql <- sqldf("
SELECT * FROM [pays_ss_nutrition_2017]
LIMIT 10
")

Q4_sql


#5. Les 10 produits pour lesquels le ratio Autres utilisations/Disponibilité intérieure
#est le plus élevé.

Q5_sql <- sqldf("
  SELECT item, round(([other_uses]/[domestic_supply_qu_1000t]),2) as ratio, country 
  FROM jointure_clean
  WHERE [other_uses] is not null AND year = 2017 AND [domestic_supply_qu_1000t] > 0
  ORDER BY ratio DESC
  LIMIT 10
  ")

Q5_sql
