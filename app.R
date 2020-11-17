setwd("C:/Documentos/Topicos")

#install.packages("openxlsx")


library(shiny)
library(leaflet)
library(openxlsx)
library(tidyverse)


# Base 1: Matriz de Leontief
base1 <- read.xlsx("BASES-EMPREGO-E-RENDA.xlsx",sheet=6)
base1$setnum<- c(1:67)
#base1_mat <- data.matrix(base1[,-1])

# Base 2: RAIS compilada IBGE
base2 <- read.xlsx("BASES-EMPREGO-E-RENDA.xlsx",sheet=7)

base2$codmun<-substring(base2$`Municipio/Setores`, 1, 6)
base2$uf<-substring(base2$`Municipio/Setores`, 8, 9)
base2$NomeMun<-substring(base2$`Municipio/Setores`, 11, )
base2<- base2 %>%  mutate(estado=case_when(uf=="AC"~"Acre", uf=="AL"~"Alagoas",
                                           uf=="AP"~"Amapa",                                            
                                           uf=="BA"~"Bahia", uf=="AM"~"Amazonas",
                                           uf=="DF"~"Distrito Federal ", uf=="CE"~"Ceara",
                                           uf=="ES"~"Espirito Santo ", uf=="GO"~"Goias ",
                                           uf=="MA"~"Maranhao", uf=="MT"~"Mato Grosso",
                                           uf=="MS"~"Mato Grosso do Sul", uf=="MG"~"Minas Gerais",
                                           uf=="PA"~"Para", uf=="PB"~"Paraiba",
                                           uf=="PR"~"Parana",
                                           uf=="PE"~"Pernambuco", uf=="PI"~"Piaui",
                                           uf=="RJ"~"Rio de Janeiro", uf=="RN"~"Rio Grande do Norte",
                                           uf=="RS"~"Rio Grande do Sul", uf=="RO"~"Rondonia",
                                           uf=="RR"~"Roraima", uf=="SC"~"Santa Catarina",
                                           uf=="SP"~"São Paulo", uf=="SE"~"Sergipe",
                                           uf=="TO"~"Tocantins ",
))




# Base 3: Distancias entre municipios
base3 <- read.xlsx("BASES-EMPREGO-E-RENDA.xlsx",sheet=8)


ui <- fluidPage(
    
    #    sidebarLayout(
    
    #    sidebarPanel(
    fluidRow(column(12, wellPanel(
        numericInput(inputId = 'investimento',label ="Investimento em reais (R$)",value=NULL,min=1),
        
        selectInput(inputId = "setor_produtivo",label ="Setor Produtivo", 
                    choices = base1$Setores,selected = NULL),
        
        selectInput(inputId = "UF",label = "Estado", 
                    choices = unique(base2$uf),selected = NULL),
        
        
        selectInput(inputId ="mun",label = "Municipio", 
                    choices = NULL)))),
    #        ),
    
    #    mainPanel(
    
    titlePanel("Trabalho Topicos "),
    navbarPage("Emprego e Renda", position = 'static-top'),
    theme=shinythemes::shinytheme('cosmo'),
    fixedRow(
        column(6, 
               fluidRow(column(12, wellPanel(
                   
                   leafletOutput(outputId = "map",width=854, height = 480)))),
               
               fluidRow(column(12,wellPanel(
                   textOutput('fonte')
               )))),
        #    column(4,
        # fluidRow(column(6,wellPanel(
        #                 tableOutput('tabela'))),
        #            column(6,wellPanel(
        #                   tableOutput('tabela2')))),
        #     fluidRow(column(12, wellPanel(
        #textOutput('indicador')))
        #))
    )
)
##))

server <- function(input, output,session) {
    
    
    
    UF = reactive({
        filter(base2, uf == input$UF)
    }) 
    
    observeEvent(UF(), {
        choices <- unique(UF()$NomeMun)
        updateSelectInput(session, "mun", choices = choices) 
    })
    
    Investimento = reactive({
        input$investimento
    }) 
    
    SetorProdutivo = reactive({
        filter(base1,  Setores == input$setor_produtivo)
        #        setornum <-as.numeric(base1$setnum)
    }) 
    
    
    
    vals <- reactiveValues ()
    
    observe({vals<-SetorProdutivo()
    invest<-Investimento()
    Setornum<-as.numeric(vals$setnum)
    vals<- select(base1,-c(setnum,Setores))
    valsmat<- data.matrix(vals)
    Y <- matrix(c(rep.int(0,(Setornum-1)),invest,rep.int(0,(67-Setornum))),nrow=67)
    X <- valsmat %*% Y 
    delta <- X-valsmat[,Setornum]
    print(delta)})
    
    #    observe(vals<-SetorProdutivo())
    
    #    valsmat<- as.matrix(vals)
    
    #################################3parte a resolver ##############################
    
    
    #    observe(valsmat$a <- Investimento() )
    #     Y = matrix(c(rep.int(0,(valsmat-1)),Investimento(),rep.int(0,(length(Setores)-valsmat))),nrow=length(Setores))
    
    
    output$map = renderLeaflet({
        leaflet() %>% setView(lng = -53, lat = -11, zoom = 5) %>%
            addProviderTiles(providers$OpenStreetMap) } )
    
    
    #output$map <- renderLeaflet({
    # leaflet() %>%
    #   addTiles(
    #       urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    #        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    #    ) %>%
    #        setView(lng = -52.4704, lat = -12.3829, zoom = 4)
    # })
    
    
    
    
    name <- c("Brasil","Argentina","Venezuela","Alemanha","Inglaterra","China","JapÃ£o","Australia","Russia","Canada")
    posi <- c(1,2,3,4,5,6,7,8,9,10)
    tab <- data.frame(name,posi)
    output$tabela <- renderTable({
        tab
    })
    output$tabela2<- renderTable({
        tab
    })
    output$indicador <- renderText({
        "Indicador sobre o impacto do setor que recebeu o investimento em cadeias produtivas anteriores ou posteriores."
    })
    output$fonte <- renderText({
        "IndicaÃ§Ã£o das bases de dados utilizadas com as respectivas fontes."
    })
}
### Teste edição
# Run the application 
shinyApp(ui = ui, server = server)




