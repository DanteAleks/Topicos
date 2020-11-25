setwd("C:/Users/carlo/Documents/Faculdade/Topicos/Shinny-Topicos/Topicos")

#install.packages("openxlsx")

library(shiny)
library(leaflet)
library(openxlsx)
library(tidyverse)
require(rgdal)
require(tmap)
require(sf)

#Shape Municipios 2019
shp<- st_read("BR_Municipios_2019.shp", stringsAsFactors = FALSE)

#####Variavel para o Bind
head(shp$CD_MUN,5)
######Filter UF
head(shp$SIGLA_UF)
shp$CD_MUN<-substring(shp$CD_MUN, 1, 6)
###testes
head(shp$CD_MUN,5)

mycols <- c("#FFEEF3", "#DEB6AD", "#F06A8F", "#BB737A", "#CC6566","#5C243B","#532a3D")

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
base3$uf_origem <- str_sub(base3$origem_uf_mun,1,2) # Insere coluna com a UF da origem
base3$mun_origem <- str_sub(base3$origem_uf_mun,4) # Insere coluna com o nome do municipio da origem
base3$uf_destino <- str_sub(base3$destino_uf_mun,1,2) # Insere coluna com a UF do destino
base3$mun_destino <- str_sub(base3$destino_uf_mun,4)
base3$distancia <- base3$distancia/1000



ui <- fluidPage(
    navbarPage("Emprego e Renda", position = 'static-top'),
    theme=shinythemes::shinytheme('cosmo'),
    fixedRow(
        column(2, 
               fluidRow(column(12, wellPanel(
                   numericInput(inputId = 'investimento',label ="Investimento em reais (R$)",value=1,min=1),
                   selectInput(inputId = "setor_produtivo",label ="Setor Produtivo", 
                               choices = base1$Setores,selected = NULL),
                   #selectInput(inputId = "UF",label = "Estado", 
                               #choices = sort(unique(base2$estado)),selected = NULL),
                   selectInput(inputId ="mun",label = "Municipio", 
                               choices = sort(unique(base2_uf$NomeMun))))))),
        column(6, 
               fluidRow(column(12, wellPanel(
                   leafletOutput("map",height = 600)))),
               fluidRow(column(12,wellPanel(
                   textOutput('fonte')
               )))),
        column(4,
               fluidRow(column(6,wellPanel(
                   tableOutput('tabela'))),
                   column(6,wellPanel(
                       tableOutput('tabela2')))),
               fluidRow(column(12, wellPanel(
                   textOutput('indicador')))
               )
        )
    )
)

server <- function(input, output,session) {
    
    
    #UF = reactive({
       # filter(base2, estado == input$UF)
   # }) 
    
    #observeEvent(UF(), {
       # choices <- unique(UF()$NomeMun)
        #updateSelectInput(session, "mun", choices = choices) 
   # })
    
    Investimento = reactive({
        input$investimento
    }) 
    
    SetorProdutivo = reactive({
        filter(base1,  Setores == input$setor_produtivo)
    }) 
    
    CODMUN = reactive({
        filter( base2_uf, NomeMun == input$mun) %>% select(codmun)
    })
    
    
    
    vals <- reactiveValues()
    
    observe({ 
    
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
    
    fator_1 <- sum(base2[,Setornum+1])### ok
    
    
    
    codigo_mun_b <- base3[base3$origem==codigom$codmun,2]
    distancia_a_b <- base3[base3$origem==codigom$codmun,3]
    
    indice <- alpha*log(fator_1)+beta*log(1/distancia_a_b)
    
    base3_origem <- subset(base3,base3$origem==codigom$codmun)
    base3_origem$indice <- indice ### ok
    
    
    base_final <- merge(base2,base3_origem,by.x="codmun", by.y = "origem", all=FALSE)
    
    indice_a <- alpha*log(fator_1)
    efeito <- (indice/(indice_a+indice))*delta[Setornum,]
    base_final$efeito <- efeito ###ok
    
    basemap <- base_final %>% select(destino, mun_destino, uf_destino, distancia, efeito) %>% 
      rename("Codigo Municipio de Destino" = "destino", "Municipio de destino" = "mun_destino", "UF de destino" = "uf_destino", "Distancia entre Municipio" = "distancia", "Efeito do Investimento" = "efeito")
    mapa <- merge(shp, basemap, by.x = "CD_MUN", by.y = "Codigo Municipio de Destino", duplicateGeoms = TRUE)
    
    
   # print(head(base3_origem$indice,4))
    
    ################seria origem?
    #fator_2 <- 1/base3[base3$destino==codigom$codmun,3]
    #i_mun_b <- alpha*log(fator_1)+beta*log(fator_2) 
    
    #ifelse(length(i_mun_b)>47, (base2_uf<- base2 %>% filter(base2$estado==input$UF)%>%mutate(indice=i_mun_b)), (base2_uf<- base2 %>% filter(base2$estado==input$UF)%>%mutate(indice=0)))
    #i_mun_a <- base2_uf[base2_uf$codmun==codigom$codmun,73]
    #efeito1 <- (i_mun_b/(i_mun_a+sum(i_mun_b>0)))*delta[Setornum,]
    ####colocar dist
    #ifelse(length(efeito1)>48,(base2_uf<- base2_uf %>% mutate(efeito = efeito1) %>% select(NomeMun, uf, codmun, efeito)), base2_uf<- base2_uf %>% mutate(efeito = 0))
    
    
    ###################tentando mapa
    #ifelse(length(efeito1)>48, mapa <- merge(shp, base2_uf, by.x = "CD_MUN", by.y = "codmun", duplicateGeoms = TRUE), mapa<-base2_uf)
    
    #if(length(i_mun_b)>48){maps<-2 
    #print("map1 SENDO ATUALIZADO")}else{maps<-1}
    #if(maps>1){mapp=mapp1}else{mapp=mapp2}
    
    #print(fator_1)
    
    
    #
    })
    #mapa1<- reactive(mapa,print("deu"))
    
    
    
    
    
    #############33COMO ATUALIZAR A FUNCAO MAPp?????????
    
    output$map<-renderLeaflet({
     
      tmap_mode("view")
        tm <- tm_shape(mapa) + 
        tm_polygons("Efeito do Investimento",n = 7, palette = mycols)
        tmap_leaflet(tm) 
        })
    
    #geodata <- reactive({
     # if(maps>1){mapp=mapp1}else{mapp=mapp2}
      #})
    
    
    
    #output$map<-renderLeaflet({
    
    #validate(
    #  need(geodata(),"")
    #)
    
    
    #tmap_mode("view")
    #tm <- tm_shape(mapa, name = "Maps") + 
    #  tm_polygons("efeito", n = 4, palette = mycols)
    #tmap_leaflet(tm,mode="view",show=T)
    
   # })
    
    
    
    
    #tmap_mode("view")
    #tm <- tm_shape(mapa, name = "map") + 
    #tm_polygons("efeito",n = 6, palette = mycols)
    #tmap_leaflet(tm)
    #
    
    #observe({
    # req(maps == "1")
    # leafletProxy("map", data = mapa)
    #})
    #output$map =renderLeaflet(
    #leafet() %>% setView(lng = -53, lat = -11, zoom = 5) %>%  addProviderTiles(providers$OpenStreetMap) 
    #)
    
    
    #output$map <- renderLeaflet({
    # leaflet() %>%
    #   addTiles(
    #       urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    # href="http://www.ma        attribution = 'Maps by <    #    ) %>%pbox.com/">Mapbox</a>'
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

shinyApp(ui = ui, server = server)
