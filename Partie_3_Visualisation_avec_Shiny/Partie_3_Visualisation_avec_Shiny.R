# nouvelle application 

library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyWidgets)
library(knitr)
library(plotrix)

ui <- fluidPage (theme = shinytheme("flatly"),
  # Nom de l'application
  titlePanel("Visualisation des données EHCVM"),
  h1("Welcome"),
  navbarPage( "Résultats du projet R",
              tabPanel("Visualisation de la base",
  
  # Affichage des données : l'utilisateur pourra séléctionner le nombre de
  # de lignes à afficher
  sidebarPanel(
  numericInput(
    inputId = "n",
    label="Veillez s'il vous plait entrez le nombre de lignes à affichez",
    value=5,
    min=1,
    max=nrow(df))
  ),
  
  mainPanel(
  h3("Base non apurée"),
  tableOutput(outputId = "tab"),
  h3("Base apurée"),
  tableOutput(outputId = "tab1"))
              ),
  
  tabPanel("Analyse des données",
  
  
  h2("Graphiques"),
  
  h3("Graphique en barre"),
  
  ## couleur du graphique 
  
    selectInput(
    inputId = "color",
    label = "Veillez selectionner la couleur du graphique",
    choices = colors(),
    selected = "cyan"
  ),
  
  # barplot
  
  selectInput(
    inputId = "bar",
    label = " Veillez selectionner la variable dont vous voulez sortir le graphique en barre",
    choices = c("Nationalite", "Situation_Matrimoniale","Localite_d_origine_des_migrants"
                ,"Raisons_non_frequentation_EF","Gerant_de_l_ecole",
                "Resultat_2019_2020","Raisons_d_abandon_de_l_ecole",
                "Diplome_le_plus_eleve"),
    selected = "Situation_Matrimoniale"
  ),
  
  plotOutput(outputId = "bar_plot"),
  
  
  h3("Histogramme"),
  
  ## choix de la variable de l'histogramme (numérique)
  selectInput(
    inputId = "variable_hist",
    label = " Veillez selectionner la variable dont vous voulez sortir l'e graphique en barre'histogramme",
    choices = c("Age","Age_au_premier_mariage"),
    selected = "Age"
  ),
  
  
  h4("Histogramme classique"),
  
  
  ## choix de la couleur du graphique
  selectInput(
    inputId = "Couleur",
    label = "Veillez selectionner la couleur du graphique",
    choices = colors(),
    selected = "pink"
  ),

  ## Histogramme classique
  
  ## Barre latérale avec un curseur d'entrée pour le nombre de barre
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "number of bins : ",
                  min = 1,
                  max = 50,
                  value = 11)
    ),
    
    # Affichage du graphique
    mainPanel(
      plotOutput("distPlot")
    )
  ),
 
  
  h4("Histogramme suivant le sexe"),
  
  
  ## Histogramme par Sexe
  
  plotOutput(outputId = "h_by_sex"),
  
  
  
    h3("Diagramme en secteur"),
  
  selectInput(
    inputId = "variable_pie",
    label = " Veillez selectionner la variable dont vous voulez sortir le diagramme circulaire",
    choices = c("Sexe","Insuffisance_livres_fornitures", "Insuffisance_tablesb_equipements","Absenteisme_enseignants_greve"   
                ,"Enseignement_pas_satisfaisant", "Effectifs_plethoriques","Insuffisance_d_enseignants"      
                ,"Manque_de_toilettes", "Frequence_des_cotisations", "Salle_de_classe_en_mauvais_etat"),
    selected = "Sexe"
  ),
  
  plotOutput(outputId = "pie_plot"),
  
  
  h2("Dépendance entre les variables"),
  
  h3("Tableau et graphique croisé"),
  
  ## choix de la 1ère variable 
  selectInput(
    inputId = "v1",
    label = " Veillez selectionner la  première variable à croiser",
    choices = colnames(df),
    selected = "Situation_Matrimoniale"
  ),
  
  ## choix de la 2ème variable 
  selectInput(
    inputId = "v2",
    label = " Veillez selectionner la  deuxième variable à croiser avec la prémière",
    choices = colnames(df),
    selected = "Sexe"
  ),
  
  tableOutput(outputId = "cross_tab"),
  plotOutput(outputId = "cross_plot")
  ),
  
  
  tabPanel("Indice de Maîtrise de langue(IML)",
  sidebarPanel(
    h2("IML par langue"),
  selectInput(
    inputId = "langue",
    label = "Veuillez choisir la langue dont vous voulez connaître l'IML",
    choices = c("Français", "Langue locale", "Autre langues"),
    selected = "Français")),
  
  mainPanel(
    h2("IML pour :"),
    textOutput(outputId = "langue1"),
    textOutput(outputId = "iml"),
    
    h2("Indice Global de maîtrise de la langue IGML"),
    textOutput(outputId = "igml")
  )),
  
  
h3("Thanks for visiting our app ! See you soon.")
))

server <- function(input, output) {
  

  ## visualisation de la base
  output$tab <- renderTable({
   
     y <- my_data3
    head(y,input$n)
  })
  
  output$tab1 <- renderTable({
    y <- df
    head(y,input$n)
  })
  
  ## Diagramme en barre (barplot)
  output$bar_plot <- renderPlot({
    df <- df
    x <- df[,input$bar]
    barplot(prop.table(table(x)),main = input$bar,xlab = input$bar,
            ylab="Effectif",col = input$color, las = 3)
  })
  
  
  
  ## Histogramme
  
  ### Histogramme simple
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- df[,input$variable_hist]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$Couleur , border = 'white')
  })
  
  ### Histogramme par sexe
  output$h_by_sex <- renderPlot({
    df <- df
    datah <- df[,c(input$variable_hist,"Sexe")]
    colnames(datah) <- c("var","Sexe")
    ggplot(df, aes(x=datah$var, color=Sexe, fill=Sexe)) +
      geom_histogram(position="dodge") +
      theme(legend.position="top")+
      scale_color_brewer(palette="Paired") + 
      theme_classic()+
      scale_x_continuous(breaks=seq(0,110,10), limits=c(0,110))
  })
  
  ## Diagramme en secteur
  output$pie_plot <- renderPlot({
    pie3D(table(df[input$variable_pie]),labels = c("Oui","Non") ,
          col = c("aquamarine","pink"), main = input$variable_pie)
  })
  
  # liaisons entre les vatiables
  ## tableaux croisés
 output$cross_tab <- renderTable({
  df <- df
  table(df[,input$v1],df[,input$v2])
  })
  
 output$cross_plot <- renderPlot({
   df <- df
   mosaicplot(table(df[,input$v1],df[,input$v2]), color = TRUE )
 })
  
  ## IML
  output$iml <- renderText({
    IML <- IML
    print(IML[input$langue,])
  })
  
  output$langue1 <- renderText({
    print(input$langue)
  })
  ## IGML
  output$igml <- renderText({
    IGML <- IGML
    print(IGML)
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
