#PROYECTO FINAL

library(DT)
library(shiny)
library(tidyverse)

# Base de datos
movies <- read.csv("C:/Users/andre/Downloads/movie.csv")

#Nombre de las variables de la base de datos
Release.Date <- movies$Release.Date
Title <- movies$Title
Overview <- movies$Overview
Genre <- movies$Genre
Vote.Average <- movies$Vote.Average
Count <- movies$Count

# Define la interfaz de usuario (UI)
ui<-fluidPage(
  titlePanel("Peliculas de Netflix"),
  navbarPage(
    tabsetPanel(
      tabPanel("Base", DT::dataTableOutput("Tabla")),
      tabPanel("Preguntas",
               sidebarPanel(
                 selectInput("variable", "Seleccione una variable:", choices = names(movies)),
                 selectInput("pregunta", "Seleccione una pregunta:", 
                             choices = c(
                               "¿Cuál es el género más común entre las películas?",
                               "¿Cuál es el género menos común entre las películas?",
                               "¿Cuál es el promedio de las calificaciones de las películas?",
                               "¿Cuál es el promedio de las votaciones de las películas?",
                               "¿Cuál es la mediana del total de votaciones de las películas?",
                               "¿Cuál es la mediana del total de calificaciones de las películas?",
                               "¿Qué películas tienen la calificación más alta?",
                               "¿Qué películas tienen más votos?",
                               "¿Qué películas tienen la calificación más baja?",
                               "¿Qué películas tienen menos votos?"
                             )
                 ),
                 conditionalPanel(
                   condition = "input.variable == 'Vote.Average'",
                   textOutput("pregunta_histograma"),
                   actionButton("histograma_button", "Mostrar Histograma")
                 )
               ),
               mainPanel(
                 h4("Respuestas a las Preguntas:"),
                 tableOutput("descripcion_muestra"),
                 conditionalPanel(
                   condition = "input.histograma_button > 0",
                   h4("Histograma:"),
                   plotOutput("histograma")
                 ),
                 conditionalPanel(
                   condition = "input.variable != ''",
                   h4("Información Filtrada:"),
                   tableOutput("datos_filtrados")
                 )
               )
      )
    ),
  )
)

# Define el servidor
server<-function(input,output){
  
  #Genera una vista de la base de datos en la cual se pueden filtrar los datos por columna
  output$Tabla<-DT::renderDataTable(DT::datatable({movies},
                                                  options=list(lengtMenu=list(c(10,50,100),c("10","50","100"))),
                                                  filter="top",selection="multiple",style="bootstrap"
  )
)
  # Filtra la información según la variable seleccionada
  datos_filtrados <- reactive({ movies[, input$variable, drop = FALSE]
  })
  
  # Renderiza la tabla con los datos filtrados
  output$datos_filtrados <- renderTable({datos_filtrados()
  })
  
  # Función para generar la respuesta según la pregunta y la variable seleccionada
  generar_respuesta <- function(pregunta, variable) {
    switch(
      pregunta,
      "¿Cuál es el género más común entre las películas?" = {
        respuesta <- table(movies$Genre)
        respuesta <- data.frame(Género = names(respuesta), Frecuencia = as.numeric(respuesta))
        respuesta <- respuesta[which.max(respuesta$Frecuencia), ]
        return(respuesta)
      },
      "¿Cuál es el género menos común entre las películas?" = {
        respuesta <- table(movies$Genre)
        respuesta <- data.frame(Género = names(respuesta), Frecuencia = as.numeric(respuesta))
        respuesta <- respuesta[which.min(respuesta$Frecuencia), ]
        return(respuesta)
      },
      "¿Cuál es el promedio de las calificaciones de las películas?" = {
        respuesta <- mean(movies$Vote.Average)
        return(data.frame(Promedio = respuesta))
      },
      "¿Cuál es el promedio de las votaciones de las películas?" = {
        respuesta <- mean(movies$Count)
        return(data.frame(Promedio = respuesta))
      },         
      "¿Cuál es la mediana del total de votaciones de las películas?" = {
        respuesta <- median(movies$Count)
        return(data.frame(Mediana = respuesta))
      },
      "¿Cuál es la mediana del total de calificaciones de las películas?" = {
        respuesta <- median(movies$Vote.Average)
        return(data.frame(Mediana = respuesta))
      },
      "¿Qué películas tienen la calificación más alta?" = {
        max_calificacion <- max(movies$Vote.Average)
        peliculas_max_calificacion <- movies[movies$Vote.Average == max_calificacion, c("Title", "Vote.Average")]
        colnames(peliculas_max_calificacion) <- c("Título", "Calificación")
        return(peliculas_max_calificacion)
      },
      "¿Qué películas tienen más votos?" = {
        max_votos <- max(movies$Count)
        peliculas_max_votos <- movies[movies$Count == max_votos, c("Title", "Count")]
        colnames(peliculas_max_votos) <- c("Título", "Número de Votos")
        return(peliculas_max_votos)
      },
      "¿Qué películas tienen la calificación más baja?" = {
        min_calificacion <- min(movies$Vote.Average)
        peliculas_min_calificacion <- movies[movies$Vote.Average == min_calificacion, c("Title", "Vote.Average")]
        colnames(peliculas_min_calificacion) <- c("Título", "Calificación")
        return(peliculas_min_calificacion)
      },
      "¿Qué películas tienen menos votos?" = {
        min_votos <- min(movies$Count)
        peliculas_min_votos <- movies[movies$Count == min_votos, c("Title", "Count")]
        colnames(peliculas_min_votos) <- c("Título", "Número de Votos")
        return(peliculas_min_votos)
      }
    )
  }
  
  # Renderiza la descripción de la muestra
  output$descripcion_muestra <- renderTable({
    generar_respuesta(input$pregunta, input$variable)
  })
  
  # Renderiza el título del histograma
  output$pregunta_histograma <- renderText({
    if (input$variable == "Vote.Average") {
      return("Histograma:")
    } else {
      return(NULL)
    }
  })
  
  # Renderiza el histograma cuando se hace clic en el botón
  observeEvent(input$histograma_button, {
    if (input$variable == "Vote.Average") {
      output$histograma <- renderPlot({
        ggplot(movies, aes_string(x = input$variable)) +
          geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
          labs(title = paste("Histograma de", input$variable),
               x = input$variable, y = "Frecuencia") +
          theme_minimal()
      })
    }
  })
  
}
# Crea la aplicación Shiny
shinyApp(ui, server)
