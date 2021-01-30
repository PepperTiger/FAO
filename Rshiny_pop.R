library(shiny)
install.packages("shinythemes")
library(shinythemes)
library(ggplot2)

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

#Gâchis 
waste_country <- prod_t %>% group_by(country) %>% summarise(waste = sum(losses_1000t, na.rm = TRUE)) %>% arrange(waste) 
waste_item <- prod_t %>% group_by(item) %>% summarise(waste = sum(losses_1000t, na.rm = TRUE)) %>% arrange(waste) 
population_2014 <- population_table %>% filter(year == 2014)
n_waste_country <- waste_country %>% arrange(country)
ratio_habitant <- merge(population_2014, n_waste_country)
ratio_habitant <- ratio_habitant %>% mutate(ratio = waste / population) %>% arrange(ratio)



#Shiny exploitation
ui <- fluidPage(navbarPage("Dashboard"
                ,tabPanel("Chiffres Clés"
                
                ,titlePanel("Quelques chiffres")
                ,icon("users")
                
                , column(1,img(class="img-polaroid",
                                     src=paste0("https://afr100.org/"
                                                ,"sites/default/files/"
                                                ,"FAO_logo_Blue_3lines_en.jpg")))
                ,"La poulation mondiale"
                ,textInput(inputId = "Population",
                          label = "Nombre de population",
                          value = "Population mondial selon l'année"), verbatimTextOutput("table")
                
               , textInput("", "Top 5  des pays ayant le plus grand nombre de population en moyenne","tableau1")
                        , verbatimTextOutput("liste1")
               , sliderInput("year", "Top 5 des pays avec le plus grand nombre de population selon l'année",
                              min = 2014, max = 2017, value = 1), tableOutput("year")  
               , sliderInput("year2", "Top 5 des pays avec le moins grand nombre de population selon l'année",
                             min = 2014, max = 2017, value = 1), tableOutput("year2")
               , selectInput(inputId = "pays", label = strong("Liste des pays présents dans la base"),
                             choices = unique(sort(top_hab$country)),
                             selected = "France")
                           , mainPanel( 
                                     h1("Nombre d'habitant par pays") , tableOutput("data")
                                   , h2("Nombre d'habitant selon le pays"), plotOutput("pop"))
                            
               , textInput(" ","Nombre de personnes en sous-nutrition","tableu2"), verbatimTextOutput("sous_nutri")
               
               , selectInput(inputId = "pays2", label = strong("Liste des pays présents dans la base"),
                             choices = unique(sort(top_hab$country)),
                             selected = "India"),tableOutput("data2")
                )
               
               #production actuelle
               ,tabPanel("Production actuelle"
                         , sidebarPanel(tags$h3("Input:"),
                              checkboxGroupInput("type", "Type d'alimentation:", c("vegetal", "animal"),selected = "vegetal")  )
                         
                         , # sidebarPanel
                                mainPanel(
                                h1("Evolution de la production au fil des années"),
                                plotOutput("graph1"),
                                                          
                                h3("Proportions de céréales utilisées pour l'alimentation humaine et animale"),
                                plotOutput("graph2")) # mainPanel
               ) 
               ,tabPanel("Importation"
                         ,titlePanel('Pays importateurs des pays en sous-nutrition')
                         ,selectInput(inputId = "produit", label = strong("Liste des produits les plus exportés par les pays en sous-nutritions"),
                                     choices = unique(sort(export_produit_2017$item))
                                     ,selected = "Soyabeans")
                         , plotOutput('graph')
                )
               
               
               ,tabPanel("Gâchis alimentaire" 
                         , sliderInput("tail1", "Top des pays qui font le plus de gâchis",
                                       min = 1, max = 10, value = 5), plotOutput("tail1")
                         , sliderInput("tail2", "Top des produits les plus jetés",
                                       min = 1, max = 10, value = 5), plotOutput("tail2")
                         , sliderInput("tail3", "Top des pays qui font le plus de gâchis par habitant en 2014",
                                       min = 1, max = 10, value = 5), plotOutput("tail3")
               )

                         
               ,tabPanel("Prévision")))


server <- function(input, output){
  # Chiffres Clés
  output$table <- renderPrint({data.frame(table_pop$total)})
  output$pays <- renderPrint({population_t %>% filter(country == input$pays)})
  output$liste1 <- renderPrint({top_hab %>% head()%>% data.frame() })
  output$year <- renderTable({pop2%>% filter(year== input$year) %>% head(5)})
  output$year2 <- renderTable({pop2%>% filter(year== input$year2) %>% tail(5)})
  output$data <- renderTable({pop2%>% filter(country== input$pays)})
  output$pop <- renderPlot({ggplot(pop2%>% filter(country== input$pays), aes(x=year,y=population,fill=year))+geom_bar(stat="identity")})
  output$sous_nutri <- renderPrint({sous_nutrition %>% head(10)%>% data.frame()})
  output$data2 <- renderTable({sous_nutrition%>% filter(country== input$pays2)})
  
  #Production actuelle
  output$graph1 <- renderPlot(ggplot(prod_t2 %>% filter(origin %in% c(input$type))%>% group_by(year) %>% summarise(production=sum(production_1000t)), aes(x=year, y=production))+geom_point() + geom_line())
  output$graph2 <- renderPlot(prod_t3) 

  #Exportation des pays en sous nutrition
  plot_data <- reactive({
    grandes_importations_2017 %>%
      filter(item==input$produit)  %>% 
      group_by(country) %>% 
      summarise(s=sum(import_qu_1000t), na.rm=TRUE)%>%
      head(5)
  })
  
  output$graph<-renderPlot({
    ggplot(plot_data(),aes(x=country,y=s , fill=s))+geom_bar(stat="identity")
  })
  
  #Gâchis alimentaire
  output$tail1 <- renderPlot({ ggplot(tail(waste_country,input$tail1), aes(x = reorder(country, waste), waste, fill = waste)) +
      geom_col() +
      coord_flip() +
      labs(titles = "Gachis par Pays", x = "Pays", y = "Gachis en tonne", fill = "Gachis")})
  
  output$tail2 <- renderPlot({ggplot(tail(waste_item,input$tail2), aes(x = reorder(item, waste), waste, fill = waste)) +
      geom_col() +
      coord_flip() +
      labs(titles = "Produits les plus jetés", x = "Produits", y = "Gachis en tonne", fill = "Gachis")})
  
  output$tail3 <- renderPlot({ggplot(tail(ratio_habitant,input$tail3), aes(x = reorder(country, ratio), ratio, fill = ratio)) +
      geom_col() +
      coord_flip() +
      labs(titles = "Gachis par Habitant en 2014", x = "Pays", y = "Gachis en tonne", fill = "Gachis")})

  #Prévision
  
}
  
shinyApp(ui = ui,server = server)


# porduits les + exporter par pays en sous-nutrition



 
