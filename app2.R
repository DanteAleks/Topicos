setwd("C:/Documentos/Topicos")

#install.packages("openxlsx")

library(shiny)
library(leaflet)
library(openxlsx)
library(tidyverse)
require(rgdal)
require(tmap)
require(sf)
library(shinyWidgets)
library(DT)
library(shinythemes)



#Shape Municipios 2019
shp<- st_read("BR_Municipios_2019.shp", stringsAsFactors = FALSE)



#####Variavel para o Bind
head(shp$CD_MUN,5)
######Filter UF
head(shp$SIGLA_UF)
shp$CD_MUN<-substring(shp$CD_MUN, 1, 6)
###testes
head(shp$CD_MUN,5)


# Base 1: Matriz de Leontief
base1 <- read.xlsx("2020-11-23-BASES-EMPREGO-E-RENDA.xlsx",sheet=6)
base1_mat <- data.matrix(base1[,-1])
base1$setnum<- c(1:67)
#base1_mat <- data.matrix(base1[,-1])

# Base 2: RAIS compilada IBGE
base2 <- read.xlsx("2020-11-23-BASES-EMPREGO-E-RENDA.xlsx",sheet=7)

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
base2_uf<- base2 %>% filter(uf == "RO")

# Base 3: Distancias entre municipios
base3 <- read.xlsx("2020-11-23-BASES-EMPREGO-E-RENDA.xlsx",sheet=8)
base3$uf_origem <- str_sub(base3$origem_uf_mun,1,2) 
base3$mun_origem <- str_sub(base3$origem_uf_mun,4) 
base3$uf_destino <- str_sub(base3$destino_uf_mun,1,2) 
base3$mun_destino <- str_sub(base3$destino_uf_mun,4)
base3$distancia <- base3$distancia/1000




ui <- fluidPage(setBackgroundColor("#155675"),
                tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/d/d0/S%C3%ADmbolo_da_UnB.png",
                         width = "200px", height = "100px",
                         style = "position:absolute;left:3em ;top:43em"),
                navbarPage("Emprego e Renda - Rondônia", position = 'static-top'),
                
                theme=shinythemes::shinytheme('cerulean'),
                fixedRow(
                  column(2, 
                         fluidRow(column(12, wellPanel(style = "background-color: #fff; border-color: #2c3e50",
                                                       numericInput(inputId = 'investimento',label ="Investimento em reais (R$)",value=100,min=100),
                                                       selectInput(inputId = "setor_produtivo",label ="Setor Produtivo", 
                                                                   choices = base1$Setores,selected = NULL),
                                                       
                                                       selectInput(inputId ="mun",label = "Municipio", 
                                                                   choices = sort(unique(base2_uf$NomeMun))),
                                                       selectInput(inputId = "UF",label = "Estado do Efeito a Calcular", 
                                                                   choices = sort(unique(base2$estado)),selected = NULL),
                         )))),
                  column(4, 
                         fluidRow(column(12, wellPanel(style = "background-color: #fff; border-color: #2c3e50;;height:45.5em",
                                                       leafletOutput("map",height = 600)))),
                         fluidRow(column(12,wellPanel(
                           textOutput('fonte')
                         )))),
                  column(6,
                         fluidRow(column(6,wellPanel(style = "background-color: #fff; border-color: #2c3e50;height:45.5em",
                                                     tableOutput('tabela'))),
                                  column(6,wellPanel(style = "background-color: #fff; border-color: #2c3e50;height:45.5em",
                                                     tableOutput('tabela2')))),
                         fluidRow(column(12, wellPanel(style = "background-color: #fff; border-color: #2c3e50",
                                                       textOutput('indicador')))
                         ),
                         p("", a(href = "https://github.com/DanteAleks/Topicos",target ="_blank","Link Código"))
                         )
                )
)


server <- function(input, output,session) {
  
  UF = reactive({
    filter( base2, estado == input$UF) %>% select(uf)
  })
  
  Investimento = reactive({
    input$investimento
  }) 
  
  SetorProdutivo = reactive({
    filter(base1,  Setores == input$setor_produtivo)
  }) 
  
  CODMUN = reactive({
    filter( base2_uf, NomeMun == input$mun) %>% select(codmun)
  })
  
  
  
  output$map<-renderLeaflet({
    
    
    vals1<-SetorProdutivo()
    invest<-Investimento() 
    
    Setornum<-as.numeric(vals1$setnum)
    
    vals<- select(base1,-c(setnum,Setores))
    valsmat<- data.matrix(vals)
    
    
    
    Y <- matrix(c(rep.int(0,(Setornum-1)),invest,rep.int(0,(67-Setornum))),nrow=67)
    X <- valsmat %*% Y 
    delta <- X-valsmat[,Setornum]
    
    
    codigom<- CODMUN() 
    alpha <- 1
    beta <- 1
    
    fator_1 <- sum(base2[,Setornum+1])
    
    
    
    codigo_mun_b <- base3[base3$origem==codigom$codmun,2]
    distancia_a_b <- base3[base3$origem==codigom$codmun,3]
    
    indice <- alpha*log(fator_1)+beta*log(1/distancia_a_b)
    
    base3_origem <- subset(base3,base3$origem==codigom$codmun)
    base3_origem$indice <- indice 
    
    
    base_final <- merge(base2,base3_origem,by.x="codmun", by.y = "origem", all=FALSE)
    
    indice_a <- alpha*log(fator_1)
    efeito <- (indice/(indice_a+indice))*delta[Setornum,]
    base_final$efeito <- efeito 
    
    basemap <- base_final %>% select(destino, mun_destino, uf_destino, distancia, efeito) %>% 
      rename("Codigo Municipio de Destino" = "destino", "Municipio de Destino" = "mun_destino", "UF de Destino" = "uf_destino", "Distancia entre Municipio" = "distancia", "Efeito do Investimento" = "efeito")
    
    mapa <- merge(shp, basemap, by.x = "CD_MUN", by.y = "Codigo Municipio de Destino", duplicateGeoms = TRUE)
    
    lol<-head(UF(),1)
    mapa1<- filter(mapa, SIGLA_UF == lol$uf)
    
    print(head(base_final,2))
    print(head(basemap,2))
    
    tmap_mode("view")
    
    tm <- tm_shape(mapa1) + 
      tm_polygons("Efeito do Investimento",n = 7, palette = 'Blues', title = "Efeito do Investimento:", popup.vars= c("Municipio de Destino", "UF de Destino", "Distancia entre Municipio", "Efeito do Investimento") )
    tmap_leaflet(tm) 
    
  })
  
  
  
  output$tabela <- renderTable({
    
    vals1<-SetorProdutivo()
    invest<-Investimento() 
    
    Setornum<-as.numeric(vals1$setnum)
    
    vals<- select(base1,-c(setnum,Setores))
    valsmat<- data.matrix(vals)
    
    Y <- matrix(c(rep.int(0,(Setornum-1)),invest,rep.int(0,(67-Setornum))),nrow=67)
    X <- valsmat %*% Y 
    delta <- X-valsmat[,Setornum]
    
    
    codigom<- CODMUN() 
    alpha <- 1
    beta <- 1
    
    fator_1 <- sum(base2[,Setornum+1])
    
    codigo_mun_b <- base3[base3$origem==codigom$codmun,2]
    distancia_a_b <- base3[base3$origem==codigom$codmun,3]
    
    indice <- alpha*log(fator_1)+beta*log(1/distancia_a_b)
    
    base3_origem <- subset(base3,base3$origem==codigom$codmun)
    base3_origem$indice <- indice 
    
    
    base_final <- merge(base2,base3_origem,by.x="codmun", by.y = "origem", all=FALSE)
    
    indice_a <- alpha*log(fator_1)
    efeito <- (indice/(indice_a+indice))*delta[Setornum,]
    base_final$efeito <- efeito 
    
    basemap <- base_final %>% select(destino, mun_destino, uf_destino, distancia, efeito) %>% 
      rename("Codigo Municipio de Destino" = "destino", "Municipio de Destino" = "mun_destino", "UF de Destino" = "uf_destino", "Distancia entre Municipio" = "distancia", "Efeito do Investimento" = "efeito")
    
    tab<- basemap %>% arrange(desc(efeito)) 
    tab<-head(tab, 10) %>% select("Municipio de Destino", "UF de Destino", "Efeito do Investimento")
    tab
    
    
  })
  output$tabela2<- renderTable({
    
    vals1<-SetorProdutivo()
    invest<-Investimento() 
    
    Setornum<-as.numeric(vals1$setnum)
    
    vals<- select(base1,-c(setnum,Setores))
    valsmat<- data.matrix(vals)
    
    Y <- matrix(c(rep.int(0,(Setornum-1)),invest,rep.int(0,(67-Setornum))),nrow=67)
    X <- valsmat %*% Y 
    delta <- X-valsmat[,Setornum]
    
    
    codigom<- CODMUN() 
    alpha <- 1
    beta <- 1
    
    fator_1 <- sum(base2[,Setornum+1])
    
    codigo_mun_b <- base3[base3$origem==codigom$codmun,2]
    distancia_a_b <- base3[base3$origem==codigom$codmun,3]
    
    indice <- alpha*log(fator_1)+beta*log(1/distancia_a_b)
    
    base3_origem <- subset(base3,base3$origem==codigom$codmun)
    base3_origem$indice <- indice 
    
    
    base_final <- merge(base2,base3_origem,by.x="codmun", by.y = "origem", all=FALSE)
    
    indice_a <- alpha*log(fator_1)
    efeito <- (indice/(indice_a+indice))*delta[Setornum,]
    base_final$efeito <- efeito 
    
    basemap <- base_final %>% select(destino, mun_destino, uf_destino, distancia, efeito) %>% 
      rename("Codigo Municipio de Destino" = "destino", "Municipio de Destino" = "mun_destino", "UF de Destino" = "uf_destino", "Distancia entre Municipio" = "distancia", "Efeito do Investimento" = "efeito")
    
    
    tab2<- basemap %>% arrange(efeito)
    tab2<-head(tab2, 10) %>% select("Municipio de Destino", "UF de Destino", "Efeito do Investimento")
    tab2
    
  })
  output$indicador <- renderText({
    "Este é um projeto da equipe de emprego e renda da disciplina Tópicos em Estatística 1, do Departamento de Estatística da Universidade de Brasília (UnB) 
    com o Instituto de Pesquisa Econômica Aplicada (IPEA). O grupo foi demandado e orientado pelo profissional do IPEA,Bruno Cruz, autor da ideia do Dashboard. 
    Apresenta como objetivo calcular uma simulação do impacto que um dado valor de investimento, feito em um setor produtivo e município do estado de Rondônia 
    escolhido pelo usuário, terá em outros setores e municípios do Brasil. A tabela à esquerda apresenta os dez municipios mais impactados por esse investimento, 
    enquanto a tabela à direita apresenta os dez municípios menos impactados. O gráfico apresenta por cores, o valor desses impactos em cada município por estado escolhido.
    Equipe de trabalho: Bruno Brandão,  Carlo Aleksandr, Fabiana Mariquito, Gilson Daniel e Rafael Morum; 
    Mentor: Bruno Cruz;
    Professores: Jhames Sampaio e Ana Maria Nogales"
  })
  output$fonte <- renderText({
    "Fontes: IBGE; Ministério da Economia - Secretaria do Trabalho; Nomination; Google Maps; Open Street Map; Geofabrik."
  })}


shinyApp(ui = ui, server = server)
