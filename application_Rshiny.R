
library(shiny)

# Table totale du nombre d'hab par années
table_pop <- population_table%>%group_by(year)%>%summarise(total=sum(population,na.rm=TRUE))

pop2 <- population_table %>% arrange(desc(population))

top_hab <- population_table%>%
  group_by(country)%>%  
  summarise(moy_hab = mean(population)) %>% 
  arrange(desc(moy_hab))%>% distinct(country,moy_hab) 

top_hab <- top_hab %>% mutate(pct=round(moy_hab*100/sum(moy_hab),2), rang=rank(desc(moy_hab))) 
top_hab %>% head() %>% data.frame %>% arrange(country)

sous_nutrition <- sous_nutrition_table 


# Production actuelle
prod_t2 <- prod_t %>% drop_na(production_1000t) # %>% group_by(year) %>% summarise(production=sum(production_1000t))
prod_t3 <- prod_t %>% drop_na(food_1000t, feed_1000t) %>% filter(item_code %in% (cereal$item_code)) %>% group_by(year) %>% summarise('food' = sum(food_1000t), 'feed' = sum(feed_1000t)) %>% gather(key = Type, value = Value, food:feed) %>% ggplot(aes(x=year, y=Value, fill=Type)) + 
  geom_bar(position="dodge", stat="identity")

# Importation
pays_ss_nutrition<-sous_nutrition_table%>%
  filter(is.na(nb_personnes)==FALSE)%>%
  select(country)

export_produit<- prod_t%>%
  filter(country %in% pays_ss_nutrition$country) %>%
  group_by(item)%>%
  summarise(export_qu_millierst=sum(export_qu_1000t,na.rm=TRUE))%>%
  arrange(desc(export_qu_millierst))%>%
  head(15)


grandes_importations<-prod_t%>%
  filter(item %in% export_produit$item)%>%
  # select(country_code,country,item_code,item,year,origin,import_qu_1000t)%>%
  arrange(desc(import_qu_1000t))

grandes_exportations<-prod_t%>%
  filter(item %in% export_produit$item)%>%
  # select(country_code,country,item_code,item,year,origin,import_qu_1000t)%>%
  arrange(desc(export_qu_1000t))

#Gâchis 
waste_country <- prod_t %>% group_by(country) %>% summarise(waste = sum(losses_1000t, na.rm = TRUE)) %>% arrange(waste) 
waste_item <- prod_t %>% group_by(item) %>% summarise(waste = sum(losses_1000t, na.rm = TRUE)) %>% arrange(waste) 
population_2014 <- population_table %>% filter(year == 2014)
n_waste_country <- waste_country %>% arrange(country)
ratio_habitant <- merge(population_2014, n_waste_country)
ratio_habitant <- ratio_habitant %>% mutate(ratio = waste*1000 / population) %>% arrange(ratio)



# R Shiny -----------------------------------------------------------------


#Shiny exploitation
ui <- fluidPage(navbarPage("Dashboard"
                           ,tabPanel("Présentation"
                                ,h1(img(class="img-polaroid",
                                         src=paste0("https://afr100.org/"
                                         ,"sites/default/files/"
                                         ,"FAO_logo_Blue_3lines_en.jpg")))
                                ,h2("FAO est l'Organisation des Nations unies pour l'alimentation et l'agriculture.")
                                ,h2("C'est une organisation spécialisée du système des Nations unies, créée en 1945 à Québec. Son siège est à Rome, au Palazzo FAO, depuis 1951."
                                    )
                                ,h3("Dans l'application Rshiny nous allons vous présenter:")
                                ,h4(" - population mondiale")
                                ,h4(" - production actuelle")
                                ,h4(" - sous-nutrition")
                                ,h4(" - importation")
                                ,h4(" - gâchis alimentaire")
                                ,h4(" - prévision")
                                )
                           
                           ,tabPanel("Population mondiale"
                                       , titlePanel("Nombre d'habitants dans le monde")
                                       , img(class="img-polaroid", height="50%", width="70%"
                                       ,  src=paste0("https://media-eng.dhakatribune.com/"
                                                    ,"uploads/2019/06/"
                                                    ,"bigstock-group-of-people-in-form-of-wor-262052332-1560760029753.jpg"
                                                    ))

                                       ,mainPanel("Population mondiale selon l'année", tableOutput("table"))
                                       ,mainPanel("Top 5  des pays ayant le plus grand nombre d'habitants en moyenne depuis 2014"
                                                  , tableOutput("liste1"))
                                       , sliderInput("year", "Top 5 des pays avec le plus grand nombre d'habitants selon l'année",
                                                     min = 2014, max = 2017, value = 1)
                                                  , tableOutput("year")  
                                       , sliderInput("year2", "Top 5 des pays avec le moins grand nombre d'habitants selon l'année",
                                                     min = 2014, max = 2017, value = 1)
                                                    , tableOutput("year2")
                                       , selectInput(inputId = "pays", label = strong("Liste des pays présents dans la base"),
                                                     choices = unique(sort(top_hab$country)),
                                                     selected = "France")
                                       , "Nombre d'habitant par pays" , tableOutput("data")
                                       , "Nombre d'habitant selon le pays", plotOutput("pop")
                                                        )
                           
                            #production actuelle
                           ,tabPanel("Production actuelle"
                                    
                                     
                                     , sidebarPanel(tags$h3("Input:"),
                                                    checkboxGroupInput("type", "Type d'alimentation:", c("vegetal", "animal"),selected = "vegetal"))
                                     
                                     , mainPanel(
                                       h1("Evolution de la production au fil des années"),
                                       plotOutput("graph1")
                                       , img(class="img-polaroid", height="50%", width="70%"
                                             ,  src=paste0("http://i.huffpost.com/"
                                                           ,"gen/1247156/thumbs/"
                                                           ,"o-BARLEY-AND-BUCKWHEAT-facebook.jpg"
                                             ))
                                       
                                       ,  h3("Quantité de céréales en milliers de tonnes utilisées pour l'alimentation humaine et animale"),
                                       plotOutput("graph2")) # mainPanel
                                                                ) 
                           
                           #Sous-nutrition
                           ,tabPanel("Pays en sous-nutrition"
                                     , img(class="img-polaroid", height="50%", width="70%"
                                           ,  src=paste0("https://www.visiondumonde.fr/sites/visiondumonde.fr/"
                                                         ,"files/styles/middle_height_3_1_sm/public/headerimage/2019-10/"
                                                         ,"cours-nutrition-enfants_0.jpg?itok=GyTm01iF"
                                           ))
                                     ,h1("Top 10 des pays en sous-nutrition")
                                     ,tableOutput("sous_nutri")
                                     ,h1("Top 10 des pays en sous-nutrition selon l'année")
                                     , selectInput(inputId = "date", label = strong("Sélectionner l'année"),
                                                    choices = c("2014","2015","2016","2017"),
                                                    selected = "2014"),tableOutput("sous_nutri2")
                                     , selectInput(inputId = "pays2", label = strong("Liste des pays en sous nutrition répertorié dans la FAO"),
                                                   choices = unique(sort(sous_nutrition_table$country)),
                                                   selected = "India"),tableOutput("data2")
                                     
                           )
                           
                           #Importation
                           ,tabPanel("Importation"
                                     , img(class="img-polaroid", height="50%", width="70%"
                                           ,  src=paste0(" http://www.sorial-logistics.com/"
                                                         ,"sorial-logistics/images/gallery/"
                                                         ,"14.jpg"
                                           ))
                                     
                                     ,titlePanel('Pays importateurs des produits des pays en sous-nutrition')
                                     ,tagList(
                                       selectInput(inputId = "produit", label = strong("Liste des produits les plus exportés par les pays en sous-nutritions"),
                                                   choices = unique(sort(export_produit$item))
                                                   ,selected = "Soyabeans")
                                       ,selectInput(inputId = "an", label = strong("Choisir l'année"),
                                                    choices = c("2014","2015","2016","2016","2017")
                                                    ,selected = "2014")), plotOutput("imp")
                                    ,selectInput(inputId = "an", label = strong("Choisir l'annéee"),
                                                 choices = c("2014","2015","2016","2016","2017")
                                                 ,selected = "2014"), plotOutput("exp")
                           )
                           
                           #gâchis alimentaire
                           ,tabPanel("Gâchis alimentaire" 
                                     , img(class="img-polaroid", height="50%", width="70%",align="center"
                                           ,  src=paste0("https://wikiagri.fr/"
                                                         ,"uploads/article/cover/1276/"
                                                         ,"home_big_gaspillage_alimentaire.png"
                                           ))

                                     , sliderInput("tail1", "Top des pays qui font le plus de gâchis",
                                                   min = 1, max = 10, value = 5), plotOutput("tail1")
                                     , sliderInput("tail2", "Top des produits les plus jetés",
                                                   min = 1, max = 10, value = 5), plotOutput("tail2")
                                     , sliderInput("tail3", "Top des pays qui font le plus de gâchis par habitant en 2014",
                                                   min = 1, max = 10, value = 5), plotOutput("tail3")
                                     , selectInput(inputId = "an4", label = strong("Distribution de la production par pays et par année"),
                                                   choices = c("2014","2015","2016","2017")), plotOutput("an4")
                                     ,tagList( selectInput(inputId = "pays3", label = strong("Liste des pays"),
                                                           choices = unique(sort(top_hab$country)),
                                                           selected = "France")
                                               
                                               , selectInput(inputId = "an3", label = strong("Distribution de la production par pays et par année"),
                                                             choices = c('2014',"2015","2016","2017"))), plotOutput("pie")
                           )
                           
                           
                           ,tabPanel("Prévision"
                                     , img(class="img-polaroid", height="50%", width="70%",align="center"
                                         ,  src=paste0("https://avatars.mds.yandex.net/"
                                                       ,"get-altay/2809325/2a000001735cd760cbe2828685ebaf3c6da0/"
                                                       ,"XXXL"
                                         ))
                                     
                            , h3("Prévision population mondial")
                            , plotOutput("pre")
                            , h3("Prévision population en sous-nutrition")
                            , plotOutput("pre2")
                            )
))


server <- function(input, output){
  # Chiffres Clés
  output$table <- renderTable({table_pop})
  output$pays <- renderTable({population_t %>% filter(country == input$pays)})
  output$liste1 <- renderTable({top_hab %>% head(5) })
  output$year <- renderTable({pop2%>% filter(year== input$year) %>% head(5)})
  output$year2 <- renderTable({pop2%>% filter(year== input$year2) %>% tail(5)})
  output$data <- renderTable({pop2%>% filter(country== input$pays)})
  output$pop <- renderPlot({ggplot(pop2%>% filter(country== input$pays), aes(x=year,y=population,fill=year))+
      geom_line()})
  
  #Sous-nutrition
  output$sous_nutri <- renderTable({sous_nutrition %>% group_by(country) %>% summarise(moy=mean(nb_personnes)) %>% arrange(desc(moy))%>% head(10)})
  output$sous_nutri2 <- renderTable({sous_nutrition %>%filter(year == input$date)%>% group_by(country) %>% summarise(moy=mean(nb_personnes)) %>% arrange(desc(moy))%>% head(10)})
  
  output$data2 <- renderTable({sous_nutrition%>% filter(country== input$pays2)})
  
  
  #Production actuelle
  output$graph1 <- renderPlot(ggplot(prod_t2 %>% filter(origin %in% c(input$type))%>% group_by(year) %>% summarise(production=sum(production_1000t)), aes(x=year, y=production))+geom_point() + geom_line()+ylab('milliers de tonnes'))
  output$graph2 <- renderPlot(prod_t3) 

  
  
  #Exportation des pays en sous nutrition
  output$imp <- renderPlot({ggplot(grandes_importations %>%
                                     filter(item== input$produit, year==input$an)  %>% 
                                     group_by(country) %>% 
                                     summarise(s=sum(import_qu_1000t), na.rm=TRUE)%>%
                                     arrange(desc(s)) %>%
                                     head(5), aes(x=reorder(country,-s),y=s , fill=s))+geom_bar(stat="identity") + labs(y="Milliers de tonnes",title="Pays importateurs",x="Pays") })
  
  output$exp <- renderPlot({ggplot(grandes_exportations %>%
                                     filter(item== input$produit, year==input$an)  %>% 
                                     group_by(country,year==input$an) %>% 
                                     summarise(s=sum(export_qu_1000t), na.rm=TRUE)%>%
                                     arrange(desc(s)) %>%
                                     head(5), aes(x=reorder(country,-s),y=s , fill=s))+geom_bar(stat="identity") + labs(y="Milliers de tonnes",title="Pays exportateurs",x="Pays") })
  
  
  #Gâchis alimentaire
  output$tail1 <- renderPlot({ ggplot(tail(waste_country,input$tail1), aes(x = reorder(country, waste), waste, fill = waste)) +
      geom_col() +
      coord_flip() +
      labs(titles = "Gachis par Pays", x = "Pays", y = "Gachis en milliers de tonnes", fill = "Gachis")})
  
  output$tail2 <- renderPlot({ggplot(tail(waste_item,input$tail2), aes(x = reorder(item, waste), waste, fill = waste)) +
      geom_col() +
      coord_flip() +
      labs(titles = "Produits les plus jetés", x = "Produits", y = "Gachis en milliers de tonnes", fill = "Gachis")})
  
  output$tail3 <- renderPlot({ggplot(tail(ratio_habitant,input$tail3), aes(x = reorder(country, ratio), ratio, fill = ratio)) +
      geom_col() +
      coord_flip() +
      labs(titles = "Gachis par Habitant en 2014", x = "Pays", y = "Perte moyenne en tonnes/habitant", fill = "Gachis")})
  
  output$an4 <- renderPlot({ggplot(prod_t %>% 
                                    filter(year ==  input$an4 ) %>% group_by(country, origin) %>%
                                    summarize(production = sum(production_1000t, na.rm = TRUE)) %>% 
                                    filter((origin == 'animal' & production >= 50000) | (origin == 'vegetal' & production >= 400000)), aes(x = reorder(country, production), production, fill = origin)) +
      geom_col() +
      coord_flip() +
      labs(titles = "Distribution de la production par Pays", x = "Pays", y = "Production en milliers de tonnes", fill = "Origine")
  })
  
  output$pie <- renderPlot({ggplot(prod_t %>% filter(year == input$an3, country == input$pays3) %>% 
                                     group_by(country, item) %>% summarize(production = sum(production_1000t, na.rm = TRUE)) %>%
                                     arrange(production) %>% tail(10)
                                   , aes(x = reorder(country, production), production, fill = item)) +
      geom_col() +coord_polar("y", start=0) +
      labs(titles = "Répartition des items produits", x = "Pays", y = "Production en milliers de tonnes", fill = "Origine")
  })
  
  #Prévision
  output$pre <- renderPlot({ ggplot(pop_totale, aes(x=as.numeric(year), y=total)) +
    geom_point() +
    geom_smooth(mapping = NULL, data = NULL,method=lm, fullrange=TRUE) +
    xlim(NA, 2050)})
  
  output$pre2 <- renderPlot({ ggplot(sous_nutrition_year, aes(x=as.numeric(year), y=nb_personnes_sous_nutrition)) +
    geom_point() +
   geom_smooth(method=lm, fullrange=TRUE) +
    xlim(NA, 2050)})
}

shinyApp(ui = ui,server = server)





