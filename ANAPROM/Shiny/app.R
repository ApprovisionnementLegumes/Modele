########################################
# Load the libraries, functions & datas
########################################

library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)
library(highcharter)
library(shinydashboard)
source("Data definition.R")
source("Get_Production.R")
source("Get_Province.R")
#source("Get_Production_Monthly.R")
source("Main.R")
library(shiny)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(formattable)
library(kableExtra)

options(warn=-1)



get_dispo = function(input_demand, input_legume, input_code, input_time, input_production_mode, input_transformation){
  # yealds :
  yealds_id <- (impacts %>% 
                  filter(name == "Rendements")
  )$id_impact
  
  # legumes
  input_legume_id <- (legumes%>% 
                        filter(name == input_legume)
  )$id_legume
  
  # production modes
  input_production_mode_id <- (production_modes %>% 
                                 filter(name == input_production_mode)
  )$id_mode
  
  # transformations
  input_transformation_id <- (transformation %>% 
                                filter(transformation == input_transformation)
  )$id_transformation
  
  # product
  input_product_id <- (repartition_modes %>% 
                         filter(id_mode == input_production_mode_id,
                                id_transformation == input_transformation_id,
                                id_legume == input_legume_id))$id_product
  
  production_list = list()
  provinces_id_list = as.numeric(rownames(GetProvince(input_code)[[1]]))
  
  for(i in provinces_id_list){
    
    production_list = append(production_list, get_production_province(input_product_id, input_time, i))
  }
  
  df = do.call(rbind.data.frame, production_list)
 
  colnames(df) = "value"
  
  ndf = cbind(GetProvince(input_code)[[1]], df)
 
  
  
  demand_list = vector()
  offer = ndf$value
  for(i in list(1,2,3,4,5)){
    
    
    if(offer[i] >= input_demand){
      
      
      demand_list = append(demand_list, input_demand)
      input_demand = 0
    }
    else{
      demand_list = append(demand_list, offer[i])
      input_demand = input_demand - offer[i]
    }
  }
  
  ndf$demand = demand_list
  
  return(ndf)
}
cmap = brewer.pal(n = 11, name = 'Dark2')
second_el_to_numeric <- function(ls){
  
  map(ls, function(x){
    x[[2]] <- as.numeric(x[[2]])
    x
  })
  
}

cmap = brewer.pal(n = 11, name = 'Dark2')

ui  = dashboardPage(
  dashboardHeader(title = "Anaprom"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Etat des lieux", tabName = "state", icon = icon("stats", lib = "glyphicon")),
      menuItem("About", tabName = "about", icon = icon("education", lib = "glyphicon"))
      
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(p("Choisissez le produit pour lequel vous voulez formuler une demande. \n 
                         Seule l'offre en produit frais sera impactée par la dimension temporelle."),
                    p("Une fois vos paramètres choisis, cliquez sur le bouton",strong(" \"Go!\""),
                      " tout en bas."),
                    title = ("Input parameters"), width = 5, solidHeader = TRUE,
                    status = "primary",
                    
                    sliderInput("input_demand", label = "Demande en Tonnes:", animate= FALSE,
                                min = 0, max = 8000, step = 0.5, value = 50),
                    selectInput("input_time", label = "Mois:", choices =months),
                    selectInput("input_legume", label = "Legume:", choices =NULL),
                    numericInput("input_code", "Code Postal:", 1348, min = 1, max = 10000),
                    selectInput("input_production_mode", label = "Mode:", choices=NULL),
                    selectInput("input_transformation", label = "Transformation:",choices = NULL),
                    actionButton("goButton", strong("Go!")),
                    p("Voici un calendrier de la saison et de la période de récolte du légume choisi.
                               Attention, tous les légumes n'ont pas de période de conservation. Le mode conventionnel
                                est considéré comme disponible toute l'année."),
                    htmlOutput("calendarlegend"),
                    plotOutput("calendar", height = "100px")
                ),
                
                tabBox(
                  tabPanel(htmlOutput("bilantext"),
                           htmlOutput("locality"),
                           title = "Resultats", solidHeader = TRUE, status = "primary",
                           tableOutput("dat"),
                           htmlOutput("pietext"),
                           htmlOutput("pielegend"),  
                           
                           highchartOutput("pie"),
                           downloadButton("generate_report", "Télécharger un rapport")
                  ),
                  tabPanel(title = "Impacts Environnementaux", solidHeader = TRUE, status = "primary",
                           htmlOutput("impacttext"),
                           htmlOutput("scoretext"),
                           tableOutput("impact")
                           
                  )
                )
              )
      ),
      
      tabItem(tabName = "about", h2("Bienvenue !"), box(
        p("Cette application a été conçue par 5 étudiant bioingénieur de l'Uclouvain."),
        p("Elle vous permet d'explorer les données que nous avons collectées au sujet de l'offre
                      en légumes de la wallonie et plus spécialement les",strong("Carottes, les Haricots, les Pois et les Oignons.")),
        p("Dans le panel",strong("\"Dashboard\""), "vous pourrez formuler une demande et voir comment se répartierais l'offre
                      en fontion des provinces et de votre localité. Vous pourrez également vérifier si vous êtes dans la bonne
                      saison si vous voulez des produits frais, ou si vous êtes encore dans la période de conservation."),
        p("Dans le panel",strong("\"Dashboard\""), "vous pourrez consulter les impacts environnementaux de votre
                      de votre demande et voir où elle se situe comparativement à la moyenne des différents modes de production disponibles.
                      Vous pourrez ainsi l'ajuster en fonction de vos priorités ou simplement comprendre à titre éducatif comment
                      les différents modes impact différement l'environnement."),
        p("Dans le panel",strong("\"Etat des lieux\""),"Vous Pourrez également avoir accès a u état des lieux de l'offre en", strong("Wallonie."),
          "Il vous suffira de cliquer sur une province pour avoir l'offre de chaque légume affichée. Vous pourrez ensuite cliquer sur un 
          légume pour avoir sa répartition en modes de production."),
        uiOutput("gitlink"),
        h3("Les données utilisées proviennent de :"),
        p("ADEME.Données FoodGES v1.1. 2016"),
        p("STATBEL.Chiffres agricoles de 2019. 2019"),
        p("ADEME.Données AGRIBALYSE v3.0. 2020."),
        p("Anton RIERA, Clémentine ANTIER et Philippe BARET. 
           État des lieux et scénarios à horizon 2050 de la filière légumière en Région wallonne.
           Earth & Life Institute, UCLouvain, Région Wallonne,2020")
        
        )),
      
      tabItem(tabName = "state",
              fluidRow(
                box("Cliquez sur une province pour faire apparaitre son offre anuelle.", 
                    p("La carte peut mettre quelques secondes à apparaitre. Actualisez la page si 
                      les provinces n'apparaissent pas."),
                    title = "Carte de la Wallonie", solidHeader = TRUE, width = 6, status = "primary",
                    leafletOutput("mymap")
                ),
                box("Cliquez sur un légume pour afficher sa production par mode.",
                    title = "Offre annuelle", solidHeader = TRUE, width = 6, status = "primary",
                    highchartOutput("mapplot")
                )
              )
      )
    )
  ) 
)


second_el_to_numeric <- function(ls){
  
  map(ls, function(x){
    x[[2]] <- as.numeric(x[[2]])
    x
  })
  
}

server <- function(input, output, session) {
  
  ########################################
  # Set Inputs
  ########################################
  
  observe({
    updateSelectInput(session, "input_legume", choices = legumes$name)
  })
  
  
  observe({
    req(input$input_legume)
    id_leg <- legumes$id_legume[legumes$name == input$input_legume]
    id_modes <- unique(repartition_modes[repartition_modes$id_legume == id_leg,]$id_mode)
    names_modes <- production_modes$name[production_modes$id_mode %in% id_modes]
    
    updateSelectInput(session, "input_production_mode", choices = names_modes)
  })
  
  
  observe({
    req(input$input_legume)
    req(input$input_production_mode)
    id_leg <- legumes$id_legume[legumes$name == input$input_legume]
    id_modes <- production_modes$id_mode[production_modes$name == input$input_production_mode]
    id_transformation <- unique(repartition_modes$id_transformation[repartition_modes$id_legume == id_leg &                                                                      repartition_modes$id_mode == id_modes])
    name_transformation <- transformation$transformation[transformation$id_transformation %in% id_transformation]
    updateSelectInput(session, "input_transformation", choices = name_transformation)
  })
  
  
  
  ########################################
  # Dashboard & Impacts
  ########################################
  observeEvent(input$goButton,{            
    df = Main(input$input_demand, input$input_legume, input$input_code, input$input_time,
              input$input_production_mode, input$input_transformation)
    locality = GetProvince(input$input_code)[[2]]
    
    output$dat = function(){
    color_tile_mean <- function (...) {
     
      formatter("span", style = function(x) {
        style(display = "block",
              padding = "0 4px", 
              `border-radius` = "4px", 
              `background-color` = ifelse(df[[1]]$status_id == 0 , "#ff7f7f", "#ADFF2F")) # Remember to change the colors!
      })}
   
    if("status_id" %in% colnames(df[[1]]))
    {
      tablebilan = df[[1]]%>%select(-c("status_id"))
      tablebilan%>%mutate(
        status = color_tile_mean()(status))%>%
        kable("html", escape = F, align = c("l","c","l")) %>%
        kable_styling("basic", full_width = T)%>%
        column_spec(1, bold = TRUE, color = "#565656")
    }
    else{
      tablebilan = df[[1]]
      names(tablebilan)[1] <- "Status"
      tablebilan%>%
        mutate(Status = color_tile(max.color = "#ff7f7f", min.color = "#ff7f7f")(Status))%>%
        kable("html", escape = F, align = c("l","c","l")) %>%
        kable_styling("hover", full_width = T)%>%
        column_spec(1, bold = FALSE, color = "#565656",)
      }
      
  }
    
    output$impact = output$kable <- function() {
      
      tdf = df[[2]] 
      tdf$normalized = sqrt(sqrt(tdf$impact_relatif/max(tdf$impact_relatif)))
      
      tdf%>%
        
        mutate(
          value = cell_spec(value, color = spec_color(normalized, end = 0.9, option = "D", direction =-1),
                            bold = T))%>%
        mutate(
          impact_relatif = cell_spec(impact_relatif, color = spec_color(normalized, end = 0.9, option = "D", direction =-1),
                                     bold = T))%>%
        select(-c("value_base", "means", "normalized")) %>%
        kable("html", escape = F) %>%
        kable_styling("hover", full_width = T) %>%
        row_spec(13, bold = TRUE, italic = FALSE, font_size = 18)%>%
        column_spec(1, bold = TRUE, color = "#565656")%>%
        column_spec(4, color = spec_color(tdf$incertitude, end = 0.9, option = "D", direction =-1))
      
      
      
    }
    output$locality = renderText({paste("Demande faite à ","<b>", locality[[1]], "</b>")})
    output$bilantext = renderText({paste("Voici votre bilan. Le programme commence par la province la plus <b>proche</b> de votre localisation puis, si l'offre n'est pas suffisante, explore lesautres provinces par ordre de proximité."
    )
      })
    output$impacttext = renderText({paste("Voici les impacts environnementaux de votre Demande. Vous y trouverez une mesure de <b>l\'incertitude</b> du calcul ainsi que <b> l'impact environemental relatif </b> qui est une comparaison aux impacts moyen des autres légumes et modes de production. Et tout en bas un <b>score global</b> qui doit être <b>faible</b> pour être bon."
    )
      })
    output$scoretext = renderText({paste("Votre ","<b> Score Environnemental Global </b> est de <b> ", df[[2]][13,]$value, " </b> Et son <b> Impact Relatif</b> est de <b>", df[[2]][13,]$impact_relatif, ". </b> ")})
  })
  
  
  
  
  ########################################
  # Pie
  ########################################
  
  
  observeEvent(
    input$goButton,{ndf = get_dispo(input$input_demand, input$input_legume, input$input_code,
                                    input$input_time, input$input_production_mode,
                                    input$input_transformation)
    demand = input$input_demand
    output$pietext = renderText({paste("Ce graphique représente la répartition de l\'offre par province.")})
    output$pielegend = renderText({paste(shiny::span(strong("En Rouge:"), style = "color:red", "Votre demande"),
                                         shiny::span(strong("En Violet:"), style = "color:purple", "Votre excès"))})
    output$pie = renderHighchart({
      
      hchart(ndf,"pie", hcaes(x = names, y = value), name = "Offer",
             center = list("300", "170"), size = 350)%>%
        
        hc_add_series(data = list(sum(ndf$demand), sum(ndf$value)-sum(ndf$demand)),
                      name=c("Demand"),type = "pie",
                      stack = "Values", colors = c('#FF0000', '#00ADFF2F'), 
                      dataLabels = list(enabled = FALSE), size = 180, center = c("300", "170"))%>%
        
        hc_add_series(data = list(demand - sum(ndf$value), 3*sum(ndf$value)),
                      name=list("Exces"),type = "pie",
                      stack = "Values", colors = c('#BA55D3', '#00000000'), 
                      dataLabels = list(enabled = FALSE), size = 80, borderColor = "white", borderWidth = 0,
                      stack= "value", center = c("300", "170"))
      
    })
    }
  )
  
  ########################################
  # Map
  ########################################    
  
  
  output$mymap <- renderLeaflet({
    file = '/srv/connect/apps/Modele2/Database projet.xlsx'
    #file = "/Users/charles/Documents/projet_disc/Modele/Modele2/Database projet.xlsx"
    provinces = read_excel(file, sheet = 'provinces')
    file = '/srv/connect/apps/Modele2/code-postaux-belge.csv'
    #file = "/Users/charles/Documents/projet_disc/Modele/Modele2/code-postaux-belge.csv"
    df = read_csv2(file)
    
    lon1 = (df%>%filter(Code == input$input_code))$Longitude[[1]]
    lat1 = (df%>%filter(Code == input$input_code))$Latitude[[1]]
    localite = (df%>%filter(Code == input$input_code))$Localite
    
    icons <- icons("https://generations.fr/media/news/thumb/870x489_jujujul-65178.png",
                   iconWidth = 38, iconHeight = 25,
                   iconAnchorX = 0, iconAnchorY = 0,
                   shadowWidth = 50, shadowHeight = 64,
                   shadowAnchorX = 4, shadowAnchorY = 62)
    
    file = "/srv/connect/apps/Modele2/BE_ADMIN_PROVINCE.rdata"
    #file = "/Users/charles/Documents/projet_disc/Modele/Modele2/BE_ADMIN_PROVINCE.rdata"
    load(file)
    
    poly_list = list()
    for(i in c(3,6,7,9,10)){poly_list = append(poly_list, BE_ADMIN_PROVINCE@polygons[[i]])}
    
    m <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8, maxZoom = 8))%>%
      addProviderTiles("OpenStreetMap.Mapnik")%>%
      
      addPolygons(data = poly_list[[1]], fillColor = cmap[1],
                  color = 'black', opacity = 0.6, fillOpacity = 0.5,
                  highlight = highlightOptions(fillOpacity = 1, opacity = 1),label = "Brabant-Wallon",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), layerId = 1)%>% 
      
      addPolygons(data = poly_list[[2]], fillColor = cmap[3],
                  color = 'black', opacity = 0.6, fillOpacity = 0.5,
                  highlight = highlightOptions(fillOpacity = 1, opacity = 1),label = "Hainaut",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), layerId = 2)%>% 
      
      addPolygons(data = poly_list[[3]], fillColor = cmap[6],
                  color = 'black', opacity = 0.6, fillOpacity = 0.5,
                  highlight = highlightOptions(fillOpacity = 1, opacity = 1), label = "Liège",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), layerId = 3)%>% 
      
      addPolygons(data = poly_list[[4]], fillColor = cmap[8],
                  color = 'black', opacity = 0.6, fillOpacity = 0.5,
                  highlight = highlightOptions(fillOpacity = 1, opacity = 1), label = "Luxembourg",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), layerId = 4)%>%
      
      addPolygons(data = poly_list[[5]], fillColor = cmap[5],
                  color = 'black', opacity = 0.6, fillOpacity = 0.5,
                  highlight = highlightOptions(fillOpacity = 1, opacity = 1), label = "Namur",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), layerId = 5)%>%
      
      addMarkers(lon1, lat1, label=localite)
    
    
  })
  
  
  ########################################
  # Drilldown
  ########################################
  
  
  observe({ 
    event <- input$mymap_shape_click
    total_list = vector()
    if(is.null(event))
      return()
    else
      
      output$mapplot = renderHighchart({
        total_list = vector()
        pea_list = vector()
        bean_list = vector()
        carrot_list = vector()
        oinion_list = vector()
        for(input_legume in legumes$name){
          
          input_legume_id <- (legumes%>%filter(name == input_legume))$id_legume
          id_product_list = (repartition_modes%>%filter(id_legume == input_legume_id))$id_product
          yealds_id <- (impacts %>% filter(name == "Rendements"))$id_impact
          prod_list = vector()
          
          for(input_product_id in id_product_list){
            
            prod = get_production(input_product_id, event$id, yealds_id)
            prod_list = append(prod_list, prod)
          }
          
          total = sum(prod_list)                
          total_list = append(total_list, total)
          
          if(input_legume == legumes$name[1]){pea_list = prod_list}
          if(input_legume == legumes$name[2]){bean_list = prod_list}
          if(input_legume == legumes$name[3]){carrot_list = prod_list}
          if(input_legume == legumes$name[4]){oinion_list = prod_list}
        }
        
        mod_list  = c("Conventionnel Transformé", "Conventionnel Raisonné Transfomé", 'Bio Transfomé', 'Conventionnel Raisonné Frais', 'Bio Frais', 'Zéro Traitement')
        pea_df    = data.frame('name' = mod_list, 'value' = pea_list)
        pea_ds    = second_el_to_numeric(list_parse2(pea_df))
        bean_df   = data.frame('name' = mod_list, 'value' = bean_list)
        bean_ds   = second_el_to_numeric(list_parse2(bean_df))
        carrot_df = data.frame('name' = mod_list, 'value' = carrot_list)
        carrot_ds = second_el_to_numeric(list_parse2(carrot_df))
        onion_df  = data.frame('name' = mod_list, 'value' = oinion_list)
        onion_ds  = second_el_to_numeric(list_parse2(onion_df))
        
        
        map_df = data.frame(legumes$name, total_list)
        df <- data_frame(
          name = c('Pea','Green beans','Carrot','Oinon'),
          y = map_df$total_list,
          drilldown = tolower(name))
        
        ds <- list_parse(df)
        names(ds) <- NULL
        
        hc <- highchart() %>%
          hc_chart(type = "column")%>%
          
          hc_title(text = paste("Offre Totale Annuelle dans la",
                                (provinces%>%filter(id_province == event$id))$name, " en Tonnes"))%>%
          
          hc_xAxis(type = "category")%>%
          
          hc_legend(enabled = FALSE)%>%
          
          hc_plotOptions(
            series = list(
              boderWidth = 3, borderColor = "#000000",
              dataLabels = list(enabled = FALSE)))%>%
          
          hc_add_series(name="Legumes",
                        data = ds,
                        colorByPoint = TRUE,
                        borderColor = "#000000", borderWidth = 3)%>%
          
          hc_drilldown(
            allowPointDrilldown = TRUE,animation = list(duration = 1000),
            series = list(
              list(
                id = 'pea',
                data = pea_ds, borderColor = "#000000", borderWidth = 3),
              list(
                id = "green beans",
                data = bean_ds,  borderColor = "#000000", borderWidth = 3),
              list(
                id = "carrot",
                data = carrot_ds,  borderColor = "#000000", borderWidth = 3),
              list(
                id = "oinon",
                data = onion_ds,  borderColor = "#000000", borderWidth = 3)
            )
          )
      })
    
  })
  output$generate_report <- downloadHandler(
    
    
    filename = function() {
      paste('my-report', sep = '.', "HTML")
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        "HTML",
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    } 
    
    
    
  )
  
  observe({
    req(input$input_legume)
    output$calendar = renderPlot({
      
      Calendar = function(input_legume){
        Months = month.abb[append((1:12),1)]
        harvest_start <- (legumes %>% filter(name == input_legume))$"harvest start"
        harvest_stop  <- (legumes %>% filter(name == input_legume))$"harvest stop"
        conservation_start <- (legumes %>% filter(name == input_legume))$"conservation start"
        conservation_stop <- (legumes %>% filter(name == input_legume))$"conservation stop"
        
        if(conservation_start>conservation_stop){
          ggplot() +
            geom_segment(mapping=aes(x=conservation_start, y=1,
                                     xend=13, yend=1), arrow=NULL,
                         size=2, color="red")+
            geom_segment(mapping=aes(x=1, y=1,
                                     xend=conservation_stop+1, yend=1), arrow=NULL,
                         size=2, color="red")+
            geom_segment(mapping=aes(x=harvest_start, y=1,
                                     xend=harvest_stop+1, yend=1), arrow=NULL,
                         size=2, color="green")+
            xlim(labels = Months)
        }else
        {ggplot() +
            geom_segment(mapping=aes(x=conservation_stop, y=1,
                                     xend=conservation_start, yend=1), arrow=NULL,
                         size=2, color="red")+
            geom_segment(mapping=aes(x=harvest_start, y=1,
                                     xend=harvest_stop, yend=1), arrow=NULL,
                         size=2, color="green")+
            xlim(labels = Months)
        }
      }
      Calendar(input$input_legume)+theme_classic()+theme(axis.title.x=element_blank(),
                                                         axis.line.y=element_blank(),
                                                         axis.ticks.y = element_blank(),
                                                         axis.text.y = element_blank(),
                                                         axis.title.y = element_blank())
      
    })
    output$calendarlegend = renderText({paste(shiny::span(strong("En Vert:"), style = "color:green", "Période de Récolte"),
                                              shiny::span(strong(" En Rouge:"), style = "color:red", "période de Conservation"))})})
    
    url=a("Notre Github", href="https://github.com/ApprovisionnementLegumes/Modele")
    output$gitlink = renderUI({
      tagList(url)
    })
    
}



 
shinyApp(ui = ui, server = server)
  






