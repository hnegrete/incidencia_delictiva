# Inicio de sesión
Logged = FALSE

# Omitir uso de notacio'n cienti'fica para nu'meros muy grandes
options(scipen=999)

##### Libreri'as requeridas #####

# Identificar paquetes faltantes

paquetes_req <- c("shiny", "shinydashboard", "foreign", "reshape2", "ggplot2", "shinyWidgets", "DT")
if(.Platform$GUI=="RStudio") paquetes_req <- c(paquetes_req, "rstudioapi")

paquetes_fal <- paquetes_req[!(paquetes_req %in% installed.packages()[, "Package"])]

# Instalar y cargar paquetes faltantes
if(length(paquetes_fal)) install.packages(paquetes_fal, dependencies = TRUE)
sapply(paquetes_req, require, character.only = TRUE)
paquetes_forzar<-!sapply(paquetes_req, require, character.only = TRUE)
library("RColorBrewer" )

##### 01 - Ruta de los archivos #####
if(.Platform$GUI=="RStudio" & "rstudioapi"%in%installed.packages()[, "Package"]){
  Mi_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")
  } else{
  Mi_Path <- "/home/hnegrete/Documents/Maestría/Semestre3/PFM/Proyecto Final/"
}

rm(paquetes_req,paquetes_fal)

##### 02 - Bases de Datos y Cata'logos #####
# Cata'logo de entidades federativas
CAT_ENT <- data.frame(ENT=0:32,
                      ENT_DESC=c("NACIONAL","AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CAMPECHE",
                                 "COAHUILA", "COLIMA", "CHIAPAS", "CHIHUAHUA", "CIUDAD DE MEXICO", "DURANGO",
                                 "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO", "MÉXICO", "MICHOACÁN",
                                 "MORELOS", "NAYARIT", "NUEVO LEÓN", "OAXACA", "PUEBLA", "QUERÉTARO",
                                 "QUINTANA ROO", "SAN LUIS POTOSÍ", "SINALOA", "SONORA", "TABASCO",
                                 "TAMAULIPAS", "TLAXCALA", "VERACRUZ", "YUCATÁN", "ZACATECAS"),
                      NOM_ABR=c("NA","AG","BC","BS","CA","CO","CL","CS", "CH", "CM", "DG", "GT", "GR", "HG", "JA", "MX", "MI",
                                "MO", "NY", "NL", "OA", "PU", "QT", "OR", "SL", "SI", "SO", "TB", "TM", "TL", "VZ", "YU", "ZA"),
                      ABR_OFICIAL=c("NAC", "AGS", "BC", "BCS", "CAMP", "COAH", "COL", "CHIS", "CHIH", "CDMX", "DGO", "GTO", "GRO",
                                    "HGO", "JAL", "MEX", "MICH", "MOR", "NAY", "NL", "OAX", "PUE", "QRO", "QROO","SLP", "SIN",
                                    "SON", "TAB", "TAMPS", "TLAX", "VER", "YUC","ZAC"),
                      ENT_MINUSC=c("Nacional", "Aguascalientes", "Baja California","Baja California Sur","Campeche",
                                   "Coahuila", "Colima","Chiapas","“Chihuahua","Ciudad de México", "Durango",
                                   "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacan", "Morelos",
                                   "Nayarit", "Nuevo Ledn", "Oaxaca", "Puebla", "Querétaro","Quintana Roo",
                                   "San Luis Potosi","Sinaloa","Sonora","Tabasco", "Tamaulipas", "Tlaxcala",
                                   "Veracruz" ,"Yucatan", "Zacatecas"),
                      stringsAsFactors = F)

CAT_ENT$ETIQUETA <- paste0(formatC(CAT_ENT$ENT,width = 2,flag = "0")," - ",CAT_ENT$ENT_MINUSC)

# Cata'logo de meses
CAT_MES <- data.frame(CVE_MES = 1:12,
                      MES = c("Enero", "Febrero", "Marzo", "Abril", "Mayo","Junio",
                              "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                      stringsAsFactors = F)

# Cata'logo de sexo
CAT_SEXO <- data.frame(SEXO=1:3,
                       SEXO_DESC=c("Hombre", "Mujer", "No identificado"),
                       stringsAsFactors = F)

# Cata'logo de edad
CAT_EDAD <- data.frame(EDAD=1:4,
                       EDAD_DESC=c("Menores de edad (0-17)", "Adultos (18 y más)", "No especificado", "No identificado"),
                       stringsAsFactors = F)

# Cata'logo de delitos del Fuero Comu'n
CAT_DELITOS_FC <- read.csv(paste0(Mi_Path,"Catálogos/CAT_DELITOS_FC.csv"), header = T, stringsAsFactors = F, encoding = "latin1")

# Cata'logo de delitos del Fuero Federal
CAT_DELITOS_FF <- read.csv(paste0(Mi_Path,"Catálogos/CAT_DELITOS_FF.csv"), header = T, stringsAsFactors = F, encoding = "latin1")

# Metadatos
METADATOS <- read.csv(paste0(Mi_Path,"Catálogos/Metadatos.csv"), header = T, stringsAsFactors = F, encoding = "latin1")
METADATOS <- METADATOS[!duplicated(METADATOS$ID_INDICADOR),]

Mis_Bases <- c("Seleccionar", "Delitos del Fuero Común (IDEFC)", "Víctimas de delitos del Fuero Común (IDVFC)")
NOM_BASES <- c("BD_DFC", "BD_VFC")
names(NOM_BASES) <- Mis_Bases[-1]
TIPO_BASES <- c("IDEFC", "IDVFC")
names(TIPO_BASES) <- Mis_Bases[-1]

# Base de datos: Poblaci'on CONAPO 1990-2030 (Abril, 2014)
POB_CONAPO <- read.csv(paste0(Mi_Path,"Bases de Datos/POB_CONAPO.csv"), header = T, stringsAsFactors = F, encoding = "latin1")

# Base de datos: Delitos del Fuero Comu'n
BD_DFC <- read.csv(paste0(Mi_Path,"Bases de Datos/BD_DFC.csv"), header = T, stringsAsFactors = F,encoding = "latin1")

# Base de datos: Vi'ctimas de delitos del Fuero Común
BD_VFC <- read.csv(paste0(Mi_Path,"Bases de Datos/BD_VFC.csv"), header = T, stringsAsFactors = F,encoding = "latin1")

# Base de datos: Delitos del Fuero Federal
#BD_DFF <- read.csv(paste0(Mi_Path,"Bases de Datos/BD_DFF.csv"), header = T, stringsAsFactors = F,encoding = "latin1")

# Base de datos: Delitos del Fuero Comu'n con la Anterior Metodologi'a
#BD_DFC_AM <- read.csv(paste0(Mi_Path,"Bases de Datos/BD_DFC_AM.csv"), header = T, stringsAsFactors = F,encoding = "latin1")
#BD_DFC_AM <- BD_DFC_AM[order(BD_DFC_AM$ANIO),]

# Funcio'n para cruces de indicadores
source(paste0(Mi_Path,"Indicadores.R"))

# Niveles de desglose a presentar
ESTATAL <- T
MUNICIPAL <- F

# Me'todo de estratificacio'n (Me'todos disponibles: 'Intervalos iguales', 'Valores unicos')
METODO_ESTRAT <- "Intervalos iguales"

# Nu'mero de estratos
NUM_ESTRATOS <- 5

# Paleta de colores para los estratos
PALETA <- c("tan", "sienna1", "saddlebrown")

# Base de datos: Coordenadas para el mapa HTML
#BD_COORD <- read.csv(paste0(Mi_Path, "Cartografia/Coordenadas HTML.csv"),header = T,stringsAsFactors = F)
#BD_COORD$X <- BD_COORD$COORD_X - min(BD_COORD$COORD_X)
#BD_COORD$Y <- BD_COORD$COORD_Y - min(BD_COORD$COORD_Y)

# Funcio'n para estratificar
source(paste0(Mi_Path,"Estratificar.R"))

# Funcio'n para calcular indicadores
source(paste0(Mi_Path, "Indicadores.R"))

mapa_coords <- function(ENTS, LEYENDA, FACT_MULT=1){
    return(
    paste0(
    '
    <svg id="div_mapa" onmouseover="FuncMapa(0)">
      <!-- Baja California -->
      <polygon id="MapEnt02"
    points="', paste(FACT_MULT*c(16,3,49,4,43,11,45,17,45,17,46, 21,45, 31,47,45,51,51,59,60,60,64,68,75,68,80,48,80,51,72,51,69,46,61,42,58, 32,50, 30,39,28,35,29,33,25,29,24,24,21,20,21,15,16,3),collapse = ","),'"
      style="fill:rgb(',ENTS[2],');stroke: black; stroke-width:2;" onclick="FuncMapa(2)"/>
      <!-- Baja California Sur -->
      <polygon id="MapEnt03"
    points="', paste(FACT_MULT*c(46, 80, 67,80, 70,85,73,86, 74,92, 76,95, 80,103,81,99,85,105,84, 109,87, 116,90,122,90,125,95,129,92,132,92,139,97,141, 99,139, 104, 144, 107, 150,110,152, 118,156,103, 160,100,158,99,152,92,147,87,140, 79,133, 77,136, 73,132,73,127,74,119, 74,112, 69,107,61,99,56,98,51,94,47,91,42,88,43,86,36,81,46,80),collapse = ","),'"
      style="fi11:rgb(',ENTS[3],');stroke:black;stroke-width:2;" onclick="FuncMapa(3)"/>
      <!-- Sonora -->
      <polygon id="MapEnt26"
    points="',paste(FACT_MULT*c(47,8,93,29,125,30,124,34,126,37,122,40,127,47,126,54,125,58,123,67,126,73,126,77,123,79,119,78,118,79,121,86,124,93,124,97,125,98,124,103,117,109,114,103,111,102,107,101,105,95,100,94,97,83,91,82,85,75,79,67,77,61,74,55,70,49,69,42,66,36,67,30,61,27,56,21,53,24,46,19,44,16,43,11,47,8),collapse = ","),'"
      style="fi11:rgb(',ENTS[26],');stroke:black;stroke-width:2;" onclick="FuncMapa(26)"/>
      <!-- Yucatán -->
      <polygon id="MapEnt31”
    points="',paste(FACT_MULT*c(384,192,385,186,388,182,485,179,417,173,425,175,424,183,424,189,419,194,413,195,402,207,393,196,392,195,384,192), collapse = ","),'”
      style="fill:rgb(',ENTS[31],');stroke:black;stroke-width:2;" onclick="FuncMapa(31)"/>
        ',
    paste0('
           <polygon fill="',LEYENDA$COLOR_VAR_MAP,'" stroke="',LEYENDA$COLOR_VAR_MAP,'" stroke-width="1"
    points="',298*FACT_MULT,',',15*((1:dim(LEYENDA)[1])-1)*FACT_MULT,',',300*FACT_MULT,',',15*((1:dim(LEYENDA)[1])-1)*FACT_MULT,',',300*FACT_MULT,',',(10+15*((1:dim(LEYENDA)[1])-1))*FACT_MULT,',',290*FACT_MULT,',',(10+15*((1:dim(LEYENDA)[1])-1))*FACT_MULT,'"/>
           <text x="',300*FACT_MULT+10,'" y="',(10+15*((1:dim(LEYENDA)[1])-1))*FACT_MULT,'" text-anchor="start"
    fill="black" font-size="',10*FACT_MULT,'">',LEYENDA$GPO_VAR_MAP,'</text>
           ',collapse="\n         ")
      ,'
    </svg>
    '
    )
  )
}

buscar_punto <- function(punto, BD_COORD){
  resultado <- 0
  for(i in 1:32)
    if(point.in.polygon(point.x = punto[1],point.y = punto[2],pol.x = BD_COORD$COORD_X[BD_COORD$ENT==i]+punto[3]+24.5,pol.y = BD_COORD$COORD_Y[BD_COORD$ENT==i]+punto[4]+12))
      resultado <- i
  return(resultado)
}

##### 03 - Interfaz de usuario (UI) #####
ui <- dashboardPage(
dashboardHeader(title = "Incidencia Delictiva", dropdownMenuOutput("notif")),
dashboardSidebar(
sidebarMenu(
menuItem("Inicio",tabName = "tabInicio",icon = icon("home")),
menuItem("Indicadores",tabName = "tabIndic",icon = icon("th")),
menuItem("Actualizar datos",tabName = "tabCarga",icon = icon("download")),
menuItem("Bases de datos",tabName = "tabBases",icon = icon("database"))
)
),
dashboardBody(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
      height: 100px;
      width: 880px;
      position:fixed;
      top: calc(50% - 50px);
      left: calc(50% - 400px);
      font-size: 250%;
      text-align: center;
      }
      .content-wrapper, .right-side {
      background-color: #ffffff;
      }
      "
      )
    )
  ),
tabItems(
tabItem(tabName = "tabInicio",
h2("Sistema de visualización de cifras sobre Incidencia Delictiva en México"),
div(id='divInicio', class='simpleDiv',
tags$br(),
tags$p("Este sistema permite al usuario realizar consultas de indicadores y estadisticas provenientes, principalmente, de las cifras de incidencia delictiva del Secretariado Ejecutivo del Sistema Nacional de Seguridad Publica (SESNSP). Mediante esta herramienta es posible visualizar las cifras-por medio de tablas, gráficas y mapas."),
tags$br()
),
box(htmlOutput("CuadroInf1"),title=span(icon("info")," Información general"), collapsible = T, collapsed = F,background = "red", width = 6),
box(htmlOutput("CuadroInf2"), title = span(icon("calendar"),"Actualizaciones"), collapsible = T, collapsed = F,background = "red", width = 6),
box(dataTableOutput("CuadroInf3"),title = span(icon("table"),"Cifras mensuales de Incidencia Delictiva"),collapsible = T,collapsed = F,background = "red",width = 12)
),
tabItem(tabName = "tabIndic",
h2("Consulta de indicadores"),
h4("Seleccionar el indicador"),
fluidRow(
column(5,selectInput("lstIndicador",NULL,c("Seleccionar...",paste0(METADATOS$ID_INDICADOR," - ",METADATOS$INDICADOR)),width = "15cm")),
column(7,uiOutput("UI_BOTONES"))
),
uiOutput("UI_INDICADORES")
),
tabItem(tabName = "tabBases",
h2("Descarga de bases de datos"),
selectInput("lstBaseCruces","Seleccionar base de datos",Mis_Bases,selected="Seleccionar..."),
actionButton("btnDescarga", "Descargar base de datos",icon = icon("download"))
),
tabItem(tabName = "tabCarga",
h2("Actualización de cifras"),
selectInput("lstCargaBD","Base de datos a actualizar",Mis_Bases,selected = "Seleccionar..."),
fileInput("pathBase", "Seleccionar archivo",accept = c(".csv"), buttonLabel = "Explorar",placeholder = "Ningún archivo seleccionado"),
uiOutput("UI_CARGA")
),
tabItem(tabName = "tabManual",
h2("Manual de usuario")
#includeHTML(paste0(Mi_Path, "www/Manual.htm1"))
)
)
),
skin = "red")

##### 04 - Funcionalidad del tablero (SERVER) #####
server <- function(input, output, session) {
options(shiny.maxRequestSize=200*1024^2) 
# #values <- reactiveValues(authenticated = FALSE,val_map=0)
# #txt_usuario <- gsub(".SEOPN","",unlist(strsplit(path.expand('~'),split = "[\\]"))[3])
# #load(file = paste0(Mi_Path, "Paquetes/sosecca.RData"))
# txt_usuario <- "u12893"
# dataModal <- function(failed = FALSE) {
# modalDialog(
# title = paste0("Usuario: ",txt_usuario),
# passwordInput ("txtPassword", "Contraseña:",
# # ifelse(Sys.Date()==unlist(strsplit(rev(sosecca$ahcef[sosecca$oirausu==txt_usuario])[1]," "))[1],
# # paste0("\u0045", "\u0073","\u0074", substr(txt_usuario,2,6)),"")),
# paste0("\u0045", "\u0073","\u0074", substr(txt_usuario,2,6))),
# footer = tagList(actionButton("InicioSesion", "Iniciar sesión"))
# )
# }
# 
# obs1 <- observe({showModal(dataModal())})
# obs2 <- observe({
# req(input$InicioSesion)
# isolate({
# #txt_usuario <- input$txtUsuario
# txt_usuario <- "u12893"
# txt_password <- input$txtPassword
# })
# val_usuario <- (substr(txt_usuario,1,1)=="u" & nchar(txt_usuario) == 6)
# val_password <- (txt_password == paste0("\u0045","\u0073", "\u0074", substr(txt_usuario,2,6)))
# if (val_usuario & val_password) {
# Logged <<- TRUE
# values$authenticated <- TRUE
# obsi$suspend()
# removeModal ()
# #sosecca <- rbind(sosecca, data.frame(oirausu=txt_usuario, ahcef=as.character(Sys.time()),stringsAsFactors = F))
# #save(sosecca, file=pasted(Mi_Path, "Paquetes/sosecca.RData"))
# } else {
# values$authenticated <- FALSE
# }
# })

output$notif <- renderMenu({
dropdownMenu(type = "notifications", icon = icon("user"), badgeStatus = "success",headerText = "Logueado como:",
notificationItem(text = "txt_usuario", icon = icon("user"), status = "success")
)
})

output$Cuadrolnf1 <- renderText({paste0('<b>Total de indicadores:</b> ',length(METADATOS$ID_INDICADOR),'<br>
  <b>Total de bases de datos:</b> ',2,'<br>')})

HOY_DIA <- as.numeric(substr(as.character(Sys.Date()),9,10))
HOY_MES <- as.numeric(substr(as.character(Sys.Date()),6,7))
HOY_ANO <- as.numeric(substr(as.character(Sys.Date()),1,4))

FECHA_ACT <- ifelse(HOY_DIA<20, paste0("20/", formatC(ifelse(HOY_MES==1,12,HOY_MES-1),width = 2,flag =
"0"),"/",ifelse(HOY_MES==1,HOY_ANO-1,HOY_ANO)), paste0("20/",formatC(HOY_MES,width = 2,flag = "0"),"/",HOY_ANO))
FECHA_PROX <- ifelse(HOY_DIA>=20, paste0("20/", formatC(ifelse(HOY_MES==12,1,HOY_MES+1),width = 2,flag =
"0"),"/",ifelse(HOY_MES==12, HOY_ANO+1,HOY_ANO)), paste0("20/", formatC(HOY_MES,width = 2,flag = "0"),"/",HOY_ANO))
MES_ACT <- as.numeric(substr(as.character(as.Date(as.numeric(as.Date(FECHA_ACT, format = "%d/%m/%Y"))-20,origin =
"1978-01-01")),6,7))
ANIO_ACT <- as.numeric(substr(as.character(as.Date(as.numeric(as.Date(FECHA_ACT, format = "%d/%m/%Y"))-20,origin =
"1970-01-01")),3,4))

output$Cuadrolnf2 <- renderText({paste0('<b>Última actualización:</b> ',FECHA_ACT, '<br>
                                        <b>Próxima actualizacion:</b> ‘,FECHA_PROX, ‘<br>')})

INCIDENCIA_MES<-suppressMessages(acast (BD_DFC[,c("ANIO","CVE_MES", "DELITOS")],ANIO~CVE_MES, sum))
INCIDENCIA_MES <- as.data.frame(INCIDENCIA_MES,stringsAsFactors = F)
colnames(INCIDENCIA_MES) <- substr(CAT_MES$MES[as.numeric(colnames(INCIDENCIA_MES))],1,3)
INCIDENCIA_MES$Total <- apply(INCIDENCIA_MES, 1, sum)
INCIDENCIA_MES$Año <- row.names(INCIDENCIA_MES)
INCIDENCIA_MES <- INCIDENCIA_MES[,c(rev(rev(colnames(INCIDENCIA_MES))[1]),rev(rev(colnames(INCIDENCIA_MES))[-1]))]
INCIDENCIA_MES[ INCIDENCIA_MES==0] <- ""

output$CuadroInf3 <- renderDataTable({formatStyle(formatRound(datatable(INCIDENCIA_MES, options = list(paging =
FALSE, searching = FALSE, info = FALSE) ,rownames=FALSE),columns=names(INCIDENCIA_MES)[-1],
digits=0), columns=1:length (names (INCIDENCIA_MES)), backgroundColor="#7e7e7e")})

observe({
  output$UI_INDICADORES <- NULL
  output$UT_BOTONES <- NULL
  #output$btnMeta <- NULL
  output$txtNotasTabla <- NULL
  
  IND_SELEC <- input$lstIndicador
  
  if(IND_SELEC!="Seleccionar..."){
  BD_IND <- read.csv(paste0(Mi_Path,"Indicadores/Ind_",as.numeric(substr(IND_SELEC,1,2)),".csv"),header = T,stringsAsFactors = F,encoding = "latin1")
  MAPA_IND <- suppressMessages(dcast(BD_IND[BD_IND$CVE_MES==0,c("ANIO", "CVE_ENT", "INDICADOR")],CVE_ENT~ANIO, sum))
  TABLA_IND <- MAPA_IND
  TABLA_IND$Entidad <- ""
  for(i in TABLA_IND$CVE_ENT) TABLA_IND$Entidad[TABLA_IND$CVE_ENT==i] <- CAT_ENT$ENT_MINUSC[CAT_ENT$ENT==i]
  TABLA_IND$CVE_ENT <- TABLA_IND$Entidad
  TABLA_IND$Entidad <- NULL
  names(TABLA_IND)[1] <- "Entidad"
  
  GRAF_IND <- aggregate(INDICADOR~CVE_MES+ANIO, data=BD_IND[BD_IND$CVE_ENT==9 & BD_IND$CVE_MES!=0,],FUN=sum)
  GRAF_IND$MES <- ""
  
  for(i in GRAF_IND$CVE_MES) GRAF_IND$MES[GRAF_IND$CVE_MES==i] <- CAT_MES$MES[CAT_MES$CVE_MES==i]
  GRAF_IND$ANIO_MES <- paste0(substr(GRAF_IND$MES,1,3),",",GRAF_IND$ANIO-2000)
  GRAF_IND$FECHA <- as.Date(paste0(GRAF_IND$ANIO,"-",formatC(GRAF_IND$CVE_MES,width = 2,flag = "0"),"-01"))
  GRAF_IND$ORDEN <- formatC(1:dim(GRAF_IND)[1],width = 3,flag = "0")
  
  GRAF_IND$ETIQUETA <- "Indicador"
  
  GRAF_IND <- GRAF_IND[,c("ORDEN", "FECHA", "ANIO_MES", "INDICADOR", "ETIQUETA")]
  
  output$UI_BOTONES <- renderUI({
  tagList(
  
  div(style="display: inline-block; vertical-align: bottom; width: 120px;",actionBttn(inputId = "btnTablero",label = "Tablero",style = "unite",color = "danger",icon = icon("th"))),
  div(style="display: inline-block; vertical-align: bottom; width: 100px;",actionBttn(inputId = "btnTabla",label = "Tabla",style = "unite",color = "danger",icon = icon("table"))),
  div(style="display: inline-block; vertical-align: bottom; width: 100px;",actionBttn(inputId = "btnMapa",label = "Mapa",style = "unite",color = "danger",icon = icon("globe"))),
  div(style="display: inline-block; vertical-align: bottom; width: 120px;",actionBttn(inputId = "btnGraf", label = "Grafica",style = "unite",color = "danger",icon = icon("line-chart"))),
  div(style="display: inline-block; vertical-align: bottom; width: 60px;",actionBttn(inputId = "btnMeta", label = "",style = "unite",color = "danger",icon = icon("thumb-tack")))
  
  # splitLayout(
  # #cellWidths = 136,
  # #cellArgs = list(style = “padding: 6px"),
  # actionBttn(inputId = "“btnTablero",label = “Tablero",style = "unite",color = "danger",icon = icon("th")),
  # actionBttn(inputId = “btnTabla",label = "“Tabla",style = "unite",color = "danger",icon = icon("table")),
  # actionBttn(inputId = "btnMapa",label = "Mapa",style = "unite",color = "danger",icon = icon("globe")),
  # actionBttn(inputld = "btnGraf",label = "Gráfica”,style = "unite",color = "danger”,icon = icon("line-chart")),
  # actionBttn(inputId = "btnMeta",label = "",style = "unite",color = "danger",icon = icon("thumb-tack"))
  # )
  )
  })
  output$UI_INDICADORES <- renderUI({
  tagList(
  tags$style('
  #div_mapa {
  height: 320px;
  width: 480px;
  padding: 18px;
  text-align: center;
  }
  '),
  tags$script(HTML('
  $(document) .ready(function() {
  // create a click handler which listens for a click on the element with id equal to RStudio
  $("#mi_mapa").on("click", function(){
  var x = event.clientx;
  var y = event.clienty;
  var ml = $("#mi_mapa").offset().left;
  var mt = $("#mi_mapa").offset().top;
  var coords =x + "," +y +4" - "4+ ml 4+ "," + mt;
  // send message to Shiny
  Shiny.onInputChange("coords", coords);
  });
  });
  ')),
  fluidRow(
  column(6,dataTableOutput("TABLA_TEND")),
  column(6, fluidRow(
  #plotOutput("MAP_TEND",
  # click = clickOpts(id = “plot_click_m"),
  # hover = hoverOpts(id = "plot_hover_m")),
  #div(id='divMapaGraf', class='simpleDiv',
  # tags$br()
  #),
  
  htmlOutput ("titMapa"),
  htmlOutput ("mi_mapa"),
  plotOutput ("GRAF_TEND",
              click = clickOpts(id = "plot_click_g"),
              hover = hoverOpts(id = "plot_hover_g")),
  uiOutput("hover_info_m"),
  uiOutput("hover_info_g")
  ))
  ),
  htmlOutput("txtNotasTabla")
  )
  })
  
  #---------- Generar tabla ----------
  output$TABLA_TEND <- renderDataTable({
    formatRound(datatable(TABLA_IND, options = list(paging = FALSE,searching = FALSE, info = FALSE),rownames=FALSE), columns=names(TABLA_IND)[-1], digits=ifelse(METADATOS$TIPO_VALOR[as.numeric(substr(IND_SELEC,1,2))]=="Absolutos",0,1))
  })
  output$txtNotasTabla <- renderText({
    paste0("<b>Fuente: </b>" ,METADATOS$FUENTE[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))], "<br>
           <b>Nota(s): </b>",METADATOS$NOTA[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))])
  })
  
  #---------- Generar Mapa ----------
  #point.in.polygon(point.x = 412,point.y = 195,pol.x = c(384, 390,405, 417,425,424,424,419,413, 402, 392, 390,384), pol.y = c(191, 182,179, 173,175,183,189,194,195,207,195,193,192))
  output$mi_mapa <- renderUI({
    names(MAPA_IND)[which(names(MAPA_IND)=="CVE_ENT")] <- "ENT"
    MAPA_IND$VAR_MAP <- get(rev(names(MAPA_IND))[1],pos = MAPA_IND)
    lista_mapa <- estratificar(MAPA_IND[MAPA_IND$ENT!=0, ],NUM_ESTRATOS,METODO_ESTRAT,PALETA, CAT_ENT)
    lista_mapa$BD$COLOR_RGB <- apply(col2rgb(lista_mapa$BD$COLOR_VAR_MAP),2,paste, collapse=",")
    HTML(mapa_coords(lista_mapa$BD$COLOR_RGB, lista_mapa$leyenda))
  })
  output$titMapa <- renderText({
    paste0("<b>" ,METADATOS$INDICADOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))],",",rev(names(MAPA_IND))[1],"</b>")
  })
  output$text <- renderText({
    req(input$coords)
    print(input$coords)
    #coordenadas <- as.numeric(unlist(strsplit(input$coords,split = ",")))
    paste("you pass over: ", input$coords)
  })
  
  #---------- Generar gra'fica ----------
  output$GRAF_TEND <- renderPlot({
  TITULO_PREG <- METADATOS$INDICADOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))]
  TIPO_VALOR <- METADATOS$TIPO_VALOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))]
  x <- unique(GRAF_IND$ETIQUETA)
  x <- x[order(x)]
  colors <- rgb(red = col2rgb("saddlebrown")["red",],green = col2rgb("saddlebrown")["green",], blue = col2rgb("saddlebrown")["blue",], maxColorValue = 255)#c(brewer.pal(8,"Dark2"),brewer.pal(12,"Paired"),brewer.pal(8,"Accent"))[1:length(x)]
  names(colors) <- x
  val_max <- max(get("INDICADOR",pos = GRAF_IND) )
  
  # Graficar con ggplot2
  GRAF_IND$VALOR_GRAF <- GRAF_IND$INDICADOR
  GRAF_GGPLOT <- ggplot(data=GRAF_IND, aes(x=ORDEN, y=VALOR_GRAF, group=ETIQUETA, colour=ETIQUETA) )
  GRAF_GGPLOT <- GRAF_GGPLOT +
    geom_line(size=1, linetype="longdash") +
    scale_fill_manual(values=colors) +
  geom_point(size=2) +
    labs(title=TITULO_PREG,
         subtitle="Tendencia",
         caption="SESNSP. Incidencia Delictiva del Fuero Común, 2015-2019.",
         y=METADATOS$UNIDAD_MEDIDA[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))], x="Mes") +
    scale_color_manual(name="",
                       values = colors) +
    scale_y_continuous(limits = c(0,ifelse(TIPO_VALOR!="Absolutos",ifelse(val_max<1,1,ifelse(val_max<5,5,10*ceiling(val_max/10))),ceiling(val_max/(10^(nchar(val_max)-1)))*10^(nchar(val_max)-1)))) +
    #scale_x_date(breaks = GRAF_IND$FECHA, labels = GRAF_IND$ANIO MES) +
    scale_x_discrete(breaks = GRAF_IND$ORDEN, labels = GRAF_IND$ANIO_MES) +
    theme(panel.background = element_rect(fill = "white",colour = "white",size = 0.5)) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 99, hjust = 1))
  GRAF_GGPLOT + geom_vline(xintercept = (1+12*(1:(floor(dim(GRAF_IND)[1]/12)))), linetype="dotted")
  })#, height = 500)#,width = 600)
  }
})

observeEvent(input$btnTablero, {
  
  output$UI_INDICADORES <- NULL
  IND_SELEC <- as.numeric(substr(input$lstIndicador,1,2))
  
  BD_IND <- read.csv(paste0(Mi_Path, "Indicadores/Ind_",as.numeric(substr(IND_SELEC,1,2)),".csv"),header = T, stringsAsFactors = F,encoding = "latin1")
  MAPA_IND <- suppressMessages(dcast(BD_IND[BD_IND$CVE_MES==0,c("ANIO", "CVE_ENT", "INDICADOR")],CVE_ENT~ANIO,sum))
  TABLA_IND <- MAPA_IND
  TABLA_IND$Entidad <- ""
  for(i in TABLA_IND$CVE_ENT) TABLA_IND$Entidad[ TABLA_IND$CVE_ENT==i] <- CAT_ENT$ENT_MINUSC[CAT_ENT$ENT==i]
  TABLA_IND$CVE_ENT <- TABLA_IND$Entidad
  TABLA_IND$Entidad <- NULL
  names(TABLA_IND)[1] <- "Entidad"
  
  GRAF_IND <- aggregate(INDICADOR~CVE_MES+ANIO,data=BD_IND[BD_IND$CVE_ENT==0 & BD_IND$CVE_MES!=0, ], FUN=sum)
  GRAF_IND$MES <- ""
  
  for(i in GRAF_IND$CVE_MES) GRAF_IND$MES[GRAF_IND$CVE_MES==i] <- CAT_MES$MES[CAT_MES$CVE_MES==i]
  GRAF_IND$ANIO_MES <- paste0(substr(GRAF_IND$MES,1,3),",",GRAF_IND$ANIO-2000)
  
  GRAF_IND$FECHA <- as.Date(paste0(GRAF_IND$ANIO,"-", formatC(GRAF_IND$CVE_MES,width = 2,flag = "0"),"-01"))
  GRAF_IND$ORDEN <- formatC(1:dim(GRAF_IND)[1],width = 3,flag = "0")
  
  GRAF_IND$ETIQUETA <- "Indicador"
  
  GRAF_IND <- GRAF_IND[,c("ORDEN","FECHA","ANIO_MES", "INDICADOR", "ETIQUETA")]
  
  output$UI_INDICADORES <- renderUI({
  tagList(
  tags$style('
  #div_mapa {
  height: 320px;
  width: 480px;
  padding: 19px;
  text-align: center;
  }
  '),
  tags$script(HTML('
  $(document).ready(function() {
  
  // create a click handler which listens for a click on the element with id equal to RStudio
                           $("#mi_mapa").on("click", function(){
                           
                           var x = event.clientx;
                           var y = event.clienty;
                           
                           var ml = $("#mi_mapa").offset().left;
                           var mt = $("#mi_mapa").offset().top;
                           var coords =x +","+y+" - "+ ml + "," 4+ mt;
                           
                           // send message to Shiny
                           Shiny.onInputChange("coords", coords);
                           
                           });
                           });
                           ')),
                           fluidRow(
                             column(6,dataTableOutput("TABLA_TEND")),
                             column(6, fluidRow(
                               #plotOutput ("MAP_TEND",
                               # click = clickOpts(id = "plot_click_m"),
                               # hover = hoverOpts(id = “plot_hover_m")),
                               
                               #div(id='divMapaGraf', class='simpleDiv' ,
                               # tags$br()
                               #),
                               htmlOutput("titMapa"),
                               htmlOutput("mi_mapa"),
                               plotOutput("GRAF_TEND",
  click = clickOpts(id = "plot_click_g"),
  hover = hoverOpts(id = "plot_hover_g")),
  uiOutput("hover_info_m"),
  uiOutput("hover_info_g")
  ))
  ),
  htmlOutput("txtNotasTabla")
  )
  })
  
  #---------- Generar tabla ----------
  output$TABLA_TEND <- renderDataTable({
    formatRound(datatable(TABLA_IND, options = list(paging = FALSE,searching = FALSE, info = FALSE) , rownames=FALSE), columns=names(TABLA_IND)[-1], digits=ifelse(METADATOS$TIPO_VALOR[as.numeric(substr(IND_SELEC,1,2))]=="Absolutos",0,2))
  })
  output$txtNotasTabla <- renderText({
  paste0("<b>Fuente: </b>" ,METADATOS$FUENTE[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))],"<br>
         <b>Nota(s): </b>" ,METADATOS$NOTA[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))])
  })
  
  #----------- Generar mapa ----------
  #point.in.polygon(point.x = 412,point.y = 195,pol.x = c (384,390, 405,417,425,424,424, 419,413,402, 392, 390,384),pol.y = c(191,182,179,173,175,183,189,194,195,207,195,193,192))
  output$mi_mapa <- renderUI({
    names(MAPA_IND)[which(names(MAPA_IND)=="CVE_ENT")] <- "ENT"
    MAPA_IND$VAR_MAP <- get(rev(names(MAPA_IND))[1],pos = MAPA_IND)
    lista_mapa <- estratificar(MAPA_IND[MAPA_IND$ENT!=0, ],NUM_ESTRATOS ,METODO_ESTRAT, PALETA, CAT_ENT)
    lista_mapa$BD$COLOR_RGB <- apply(col2rgb(lista_mapa$BD$COLOR_VAR_MAP),2,paste, collapse=",")
    HTML(mapa_coords(lista_mapa$BD$COLOR_RGB, lista_mapa$leyenda) )
  })
  output$titMapa <- renderText({
    paste0("<b>" ,METADATOS$INDICADOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))],",",rev(names(MAPA_IND))[1],"</b>")
  })
  output$text <- renderText({
    req(input$coords)
    print(input$coords)
    #coordenadas <- as.numeric(unlist(strsplit(input$coords,split = ",")))
    paste("you pass over: ", input$coords)
  })
  
  #----------- Generar gra'fica ----------
  output$GRAF_TEND <- renderPlot({
    TITULO_PREG <- METADATOS$INDICADOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))]
    TIPO_VALOR <- METADATOS$TIPO_VALOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))]
    x <- unique(GRAF_IND$ETIQUETA)
    x <- x[order(x)]
    colors <- rgb(red = col2rgb("saddlebrown")["red",],green = col2rgb("saddlebrown")["green",],blue = col2rgb("saddlebrown")["blue",],maxColorValue = 255)
    names(colors) <- x
    val_max <- max(get("INDICADOR",pos = GRAF_IND))
    
    # Graficar con ggplot2
    GRAF_IND$VALOR_GRAF <- GRAF_IND$INDICADOR
    GRAF_GGPLOT <- ggplot(data=GRAF_IND, aes(x=ORDEN, y=VALOR_GRAF, group=ETIQUETA, colour=ETIQUETA))
    GRAF_GGPLOT <- GRAF_GGPLOT +
     geom_line(size=1, linetype="longdash") +
     scale_fill_manual(values=colors) +
     geom_point(size=2) +
     labs(title=TITULO_PREG,
          subtitle="Tendencia",
          caption="SESNSP. Incidencia Delictiva del Fuero Común, 2015-2019.",
          y=METADATOS$UNIDAD_MEDIDA[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))], x="Mes") +
     scale_color_manual(name="",
                          values = colors) +
      scale_y_continuous(limits =
                           c(0,ifelse(TIPO_VALOR!="Absolutos",ifelse(val_max<1,1,ifelse(val_max<5,5,10*ceiling(val_max/10))),ceiling(val_max/(10^(nchar(val_max)-1)))*10^(nchar(val_max)-1)))) +
      #scale_x_date(breaks = GRAF_IND$FECHA, labels = GRAF_IND$ANIO_MES) +
      scale_x_discrete(breaks = GRAF_IND$ORDEN, labels = GRAF_IND$ANIO_MES) +
      theme(panel.background = element_rect(fill = "white",colour = "white",size = 0.5)) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    GRAF_GGPLOT + geom_vline(xintercept = (1+12*(1:(floor(dim(GRAF_IND)[1]/12)))),linetype="dotted")
  })
})

observeEvent(input$btnTabla, {

output$UI_INDICADORES <- NULL

IND_SELEC <- as.numeric(substr(input$lstIndicador,1,2))

BD_IND <- read.csv(paste0(Mi_Path,"Indicadores/Ind_",as.numeric(substr(IND_SELEC,1,2)),".csv"), header= T,stringsAsFactors = F,encoding = "latin1")

ANIO_SELEC <- max(BD_IND$ANIO)

if(!is.null(input$lstAnioTabla)) ANIO_SELEC <- input$lstAnioTabla

output$UI_INDICADORES <- renderUI({
  tagList(
    selectInput("lstAnioTabla","Año",choices=unique(BD_IND$ANIO)[order(unique(BD_IND$ANIO),decreasing = T)], selected = ANIO_SELEC, width = "5cm"),
    dataTableOutput("TABLA_TEND"),
    htmlOutput("txtNotasTabla")
  )
})

observe({
req(input$lstAnioTabla)
ANIO_SELEC <- input$lstAnioTabla
BD_IND$CVE_MES <- formatC(BD_IND$CVE_MES,width = 2,flag = "0")
BD_IND$CVE_MES[BD_IND$CVE_MES=="00"] <- "13"
BD_IND$ANIO_MES <- paste(BD_IND$ANIO, BD_IND$CVE_MES,sep="_")
TABLA_IND <- suppressMessages(dcast(BD_IND[BD_IND$ANIO==ANIO_SELEC, c("ANIO_MES", "CVE_ENT", "INDICADOR") ],CVE_ENT~ANIO_MES, sum))
TABLA_IND$Entidad <- ""
for(i in TABLA_IND$CVE_ENT) TABLA_IND$Entidad[TABLA_IND$CVE_ENT==i] <- CAT_ENT$ENT_MINUSC[CAT_ENT$ENT==i]
TABLA_IND$CVE_ENT <- TABLA_IND$Entidad
TABLA_IND$Entidad <- NULL
names(TABLA_IND)[1] <- "Entidad"
names(TABLA_IND) <- gsub(paste0("_",13),"",names(TABLA_IND))
for(y in 1:12) names(TABLA_IND) <- gsub(paste0("_",formatC(y,width = 2,flag = "0")), paste0("/",substr(CAT_MES$MES[CAT_MES$CVE_MES==y],1,3)),names(TABLA_IND))

#---------- Generar tabla ----------
output$TABLA_TEND <- renderDataTable({
  formatRound(datatable(TABLA_IND, options = list(paging = FALSE,searching = FALSE, info = FALSE),rownames=FALSE),columns=names(TABLA_IND)[-1],
  digits=ifelse(METADATOS$TIPO_VALOR[as.numeric(substr(IND_SELEC,1,2))]=="Absolutos",0,2))
})
output$txtNotasTabla <- renderText({
  paste0("<b>Fuente: </b>",METADATOS$FUENTE[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))],"<br>
         <b>Nota(s): </b>",METADATOS$NOTA[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))])
})
})
})
observeEvent(input$btnMapa, {
  output$UI_INDICADORES <- NULL
  output$UI_CONTRMAPA <- NULL
  IND_SELEC <- as.numeric(substr(input$lstIndicador,1,2))
  BD_IND <- read.csv(paste0(Mi_Path,"Indicadores/Ind_",as.numeric(substr(IND_SELEC,1,2)),".csv"),header = T,stringsAsFactors = F,encoding = "latin1")
  MAPA_MES <- "opcAnioMap"
  if(!is.null(input$opcTempMap)) MAPA_MES <- input$opcTempMap
  
  output$UI_INDICADORES <- renderUI({
    tagList(
      tags$style('
        #div_mapa {
        height: 550px;
        width: 850px;
        padding: 1px;
        text-align: center;
        }
      '),
      tags$script(HTML('
        function FuncMapa(arg1) {
        Shiny.onInputChange("coords", arg1);
        }
      ')),
      fluidRow(
        column(1,htmlOutput("auxMapa1")),
        column(8,fluidRow(htmlOutput("titMapa"),htmlOutput("mi_mapa"),uiOutput("hover_info_m"))),
        column(3,fluidRow(numericInput("numEstratMap", label="Estratos", value = 5, min = 3, max = 10, step = 1, width = "2cm"),
        radioButtons("opcTempMap","Temporalidad a graficar",choices = c("Mensual" = "opcMesMap","Anual" = "opcAnioMap"), selected = MAPA_MES),
        htmlOutput("UI_CONTRMAPA")))
      )
    )
  })
  
  observe({
    req(input$opcTempMap, input$numEstratMap)
    MAPA_MES <- input$opcTempMap
    
    NUM_ESTRATOS <- ifelse(length(input$numEstratMap) , input$numEstratMap, 5)
    x <- unique(BD_IND$ANIO)
    x <- x[order(x,decreasing=T)]
    MAPA_ANIO <- x[1]
    if(!is.null(input$lstAnioMap)) MAPA_ANIO <- input$lstAnioMap
    output$UI_CONTRMAPA <- NULL
    if(MAPA_MES=="opcMesMap"){
    output$UI_CONTRMAPA <- renderUI({
      tagList(
        selectInput("lstAnioMap","Año",choices=x,selected=MAPA_ANIO, width = "3cm"),
        div(style="display: inline-block; vertical-align: bottom; width: 45px;",actionButton(inputId="btnAtras", label="", icon = icon("arrow-left"))),
        div(style="display: inline-block; vertical-align: middle; width: 60px; text-align:center;",htmlOutput("txtMapaSel")),
        div(style="display: inline-block; vertical-align: bottom; width: 45px;",actionButton(inputId="btnAdelante", label="", icon = icon("arrow-right")))
      )
    })
    
    MAPA_IND <- suppressMessages(dcast(BD_IND[BD_IND$CVE_MES!=0 & BD_IND$ANIO==MAPA_ANIO, c("CVE_MES","CVE_ENT", "INDICADOR")],CVE_ENT=CVE_MES,sum))
    output$txtMapaSel <- renderText({paste0('<b>',CAT_MES$MES[as.numeric(rev(names(MAPA_IND))[2])],'</b>')})
    } else{
      output$UI_CONTRMAPA <- renderUI({
        tagList(
          div(style="display: inline-block; vertical-align: bottom; width: 45px;",actionButton(inputId = "btnAtras", label="", icon = icon("arrow-left"))),
          div(style="display: inline-block; vertical-align: middle; width: 60px; text-align: center;",htmlOutput("txtMapaSel")),
          div(style="display: inline-block; vertical-align: bottom; width: 45px;",actionButton(inputId = "btnAdelante", label="", icon = icon("arrow-right")))
        )
      })
      MAPA_IND <- suppressMessages(dcast(BD_IND[BD_IND$CVE_MES==0,c("ANIO","CVE_ENT", "INDICADOR")],CVE_ENT~ANIO,sum))
      output$txtMapaSel <- renderText({paste0('<b>',rev(names(MAPA_IND))[2],'</b>')})
    }
    
    names(MAPA_IND)[which(names(MAPA_IND)=="CVE_ENT")] <- "ENT"
    MAPA_IND$VAR_MAP <- get(rev(names(MAPA_IND))[1],pos = MAPA_IND)
    
    lista_mapa <- estratificar(MAPA_IND[MAPA_IND$ENT!=0,],NUM_ESTRATOS,METODO_ESTRAT,PALETA,CAT_ENT)
    lista_mapa$BD$COLOR_RGB <- apply(col2rgb(lista_mapa$BD$COLOR_VAR_MAP),2,paste,collapse=",")
    
    #---------- Generar mapa ----------
    output$mi_mapa <- renderUI({
      HTML(mapa_coords(lista_mapa$BD$COLOR_RGB,lista_mapa$leyenda,1.8))
    })
    output$titMapa <- renderText({
      paste0("<b>" ,METADATOS$INDICADOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))],",",ifelse(MAPA_MES=="opcMesMap",paste0(CAT_MES$MES[as.numeric(rev(names(MAPA_IND))[2])],"/",MAPA_ANIO),rev(names(MAPA_IND))[2]),"</b>")
    })
    output$text <- renderText({
      req(input$coords)
      print(input$coords)
      #coordenadas <- as.numeric(unlist(strsplit(input$coords,split = ",")))
      paste("you pass over: ", input$coords)
    })
  })
})
observeEvent(input$btnAdelante, {
  values$val_map <- values$val_map + 1
})

observeEvent(input$btnGraf, {
  output$UI_INDICADORES <- NULL
  IND_SELEC <- as.numeric(substr(input$lstIndicador,1,2))
  
  BD_IND <- read.csv(paste0(Mi_Path,"Indicadores/Ind_",as.numeric(substr(IND_SELEC,1,2)),".csv"),header = T,stringsAsFactors = F,encoding = "latin1")
  ENT_SELEC <- CAT_ENT$ETIQUETA[1]
  GRAF_MES <- "opcMesGraf"
  
  if(!is.null(input$lstEntGraf)) ANIO_SELEC <- input$lstEntGraf
  if(!is.null(input$opcTempGraf)) GRAF_MES <- input$opcTempGraf
  
  output$UI_INDICADORES <- renderUI({
    tagList(
      fluidRow(
        column(2,selectInput("lstEntGraf","Entidad Federativa", choices=CAT_ENT$ETIQUETA, selected = ENT_SELEC, width = "5cm")),
        column(2,radioButtons("opcTempGraf", "Temporalidad a graficar", choices = c("Mensual" = "opcMesGraf", "Anual" = "opcAnioGraf"),selected = GRAF_MES,inline = T))
      ),
      plotOutput("GRAF_TEND",
                 click = clickOpts(id = "plot_click_g2"),
                 hover = hoverOpts(id = "plot_hover_g2")),
      uiOutput("hover_info_g2")
    )
  })
  
  observe({
    req(input$lstEntGraf,input$opcTempGraf)
    ENT_SELEC <- input$lstEntGraf
    GRAF_MES <- input$opcTempGraf=="opcMesGraf"
    if(GRAF_MES){
      GRAF_IND <- aggregate(INDICADOR~CVE_MES+ANIO, data=BD_IND[BD_IND$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)) & BD_IND$CVE_MES!=0, ], FUN=sum)
      GRAF_IND$MES <- ""
      for(i in GRAF_IND$CVE_MES) GRAF_IND$MES[GRAF_IND$CVE_MES==i] <- CAT_MES$MES[CAT_MES$CVE_MES==i]
      GRAF_IND$ANIO_MES <- paste0(substr(GRAF_IND$MES,1,3),",",GRAF_IND$ANIO-2000)
      GRAF_IND$FECHA <- as.Date(paste0(GRAF_IND$ANIO,"-", formatC(GRAF_IND$CVE_MES,width = 2,flag = "0"),"-01"))
    } else{
      GRAF_IND <- aggregate(INDICADOR~CVE_MES+ANIO, data=BD_IND[BD_IND$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)) & BD_IND$CVE_MES==0, ],FUN=sum)
      if(F)#as.numeric(substr(IND_SELEC,1,2))%in%BD_DFC_AM$ID_INDICADOR)
        GRAF_IND <- rbind(BD_DFC_AM[BD_DFC_AM$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2)) & BD_DFC_AM$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)),-c(1,2)],GRAF_IND)
      
      GRAF_IND$MES <- "Total"
      GRAF_IND$ANIO_MES <- paste0(GRAF_IND$ANIO)
      GRAF_IND$FECHA <- as.Date(paste0(GRAF_IND$ANIO,"-","12-31"))
      mes_max <- max(BD_IND$CVE_MES[BD_IND$ANIO==max(BD_IND$ANIO)])
      if(mes_max<12){
        mod_mes <- lm(log(INDICADOR) ~ as.numeric(CVE_MES),
        data=aggregate(INDICADOR~CVE_MES+ANIO,data=BD_IND[BD_IND$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)) & BD_IND$CVE_MES!=0 & BD_IND$ANIO==max(BD_IND$ANIO),],FUN=sum))
        GRAF_IND$INDICADOR[GRAF_IND$ANIO==max(GRAF_IND$ANIO)] <- sum(aggregate(INDICADOR~CVE_MES+ANIO, data=BD_IND[BD_IND$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)) &
        BD_IND$CVE_MES!=0 & BD_IND$ANIO==max(BD_IND$ANIO),],FUN=sum)$INDICADOR)+
        sum(as.data.frame(exp(predict(mod_mes,list(CVE_MES=(mes_max+1):12),interval = "confidence")),stringsAsFactors = F)$fit)
      }
    }
    GRAF_IND$ORDEN <- formatC(1:dim(GRAF_IND)[1],width = 3,flag = "0")
    GRAF_IND$ETIQUETA <- "Observado"
    #GRAF_IND <- GRAF_IND[,c("ORDEN", “FECHA”, “ANIO_MES", "INDICADOR", "ETIQUETA")]
    
    #---------- Generar gra'fica ----------
    output$GRAF_TEND <- renderPlot({
      
      # Modelo de regresión exponencial
      if(GRAF_MES){
        modelo <- lm(log(INDICADOR+10) ~ as.numeric(ORDEN), data=tail(GRAF_IND,12))
        x_predic <- max(as.numeric(GRAF_IND$ORDEN))+(1:5)
        x <- predict(modelo,list(ORDEN=x_predic),interval = "confidence")
        x <- exp(x)
        x[,3] <- x[,1]+(x[,1]-x[,2])
        
        prediccion <- as.data.frame(x-10,stringsAsFactors = F)
        
        GRAF_IND_PRED <- data.frame(CVE_MES=x_predic %% 12,ANIO=0,
                                    INDICADOR=prediccion$fit,
                                    MES="",ANIO_MES="",FECHA="",
                                    ORDEN=formatC(x_predic,width = 3,flag = "0"),
                                    ETIQUETA="Pronosticado",
                                    stringsAsFactors = F)
        GRAF_IND_PRED$CVE_MES[GRAF_IND_PRED$CVE_MES==0] <- 12
        GRAF_IND_PRED$ANIO[1] <- round(GRAF_IND$ANIO[dim(GRAF_IND)[1]]+GRAF_IND$CVE_MES[dim(GRAF_IND)[1]]/24)
        for(i in 2:dim(GRAF_IND_PRED)[1]) GRAF_IND_PRED$ANIO[i] <- round(GRAF_IND_PRED$ANIO[i-1]+GRAF_IND_PRED$CVE_MES[i-1]/24)
        for(i in GRAF_IND_PRED$CVE_MES) GRAF_IND_PRED$MES[GRAF_IND_PRED$CVE_MES==1] <- CAT_MES$MES[CAT_MES$CVE_MES==i]
        
        GRAF_IND_PRED$ANIO_MES <- paste0(substr(GRAF_IND_PRED$MES,1,3),",",GRAF_IND_PRED$ANIO-2000)
        GRAF_IND_PRED$FECHA <- as.Date(paste0(GRAF_IND_PRED$ANIO,"-",formatC(GRAF_IND_PRED$CVE_MES,width = 2,flag = "0"),"-01"))
      } else{
        modelo <- lm(log(INDICADOR+10) ~ ANIO, data=GRAF_IND)
        x_predic <- max(GRAF_IND$ANIO)+(1:5)
        x <- predict(modelo,list(ANIO=x_predic),interval = "confidence")
        x <- exp(x)
        x[,3] <- x[,1]+(x[,1]-x[,2])
        
        prediccion <- as.data.frame(x-10,stringsAsFactors = F)
        
        GRAF_IND_PRED <- data.frame(CVE_MES=0,ANIO=x_predic,
                                    INDICADOR=prediccion$fit,
                                    MES="Total" ,ANIO_MES=paste0(x_predic),FECHA=as.Date(paste0(x_predic,"-","12-31")),
                                    ORDEN=formatC(max(as.numeric(GRAF_IND$ORDEN))+(1:5),width = 3,flag = "0"),
                                    ETIQUETA="Pronosticado",
                                    stringsAsFactors = F)
        
      }
      
      GRAF_IND <- rbind(GRAF_IND,GRAF_IND_PRED)
      GRAF_IND_PRED$ETIQUETA <- "Limite inferior"
      GRAF_IND_PRED$INDICADOR <- prediccion$lwr
      GRAF_IND <- rbind(GRAF_IND,GRAF_IND_PRED)
      GRAF_IND_PRED$ETIQUETA <- "Límite superior"
      GRAF_IND_PRED$INDICADOR <- prediccion$upr
      GRAF_IND <- rbind(GRAF_IND,GRAF_IND_PRED)
      GRAF_IND$INDICADOR[GRAF_IND$INDICADOR<0 & GRAF_IND$ETIQUETA%in%c("Limite inferior", "Limite superior")] <- 0
      
      TITULO_PREG <- METADATOS$INDICADOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))]
      TIPO_VALOR <- METADATOS$TIPO_VALOR[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))]
      x <- unique(GRAF_IND$ETIQUETA)
      colors <- c(rgb(red = col2rgb("saddlebrown")["red",],green = col2rgb("saddlebrown")["green",],blue = col2rgb("saddlebrown")["blue",],maxColorValue = 255), "#222D32" ,"#222D32" ,"#222D32")
      names(colors) <- x
      val_max <- round(max(get("INDICADOR",pos = GRAF_IND)))
      
      # Graficar con ggplot2
      GRAF_IND <- GRAF_IND[,c("ORDEN","FECHA","ANIO_MES", "INDICADOR", "ETIQUETA")]
      GRAF_IND$VALOR_GRAF <- GRAF_IND$INDICADOR
      GRAF_GGPLOT <- ggplot(data=GRAF_IND, aes(x=ORDEN, y=VALOR_GRAF, group=ETIQUETA, colour=ETIQUETA))
      GRAF_GGPLOT <- GRAF_GGPLOT +
      geom_line(aes(linetype=ETIQUETA))+
      geom_point()+
      scale_linetype_manual(values=c("dotted", "dotted", "solid", "longdash"))+
      geom_point(size=2) +
      labs(title=TITULO_PREG,
      subtitle=paste0(CAT_ENT$ENT_MINUSC[CAT_ENT$ENT==as.numeric(substr(ENT_SELEC,1,2))],". Tendencia ",ifelse(GRAF_MES,"mensual","anual"),"."),
      caption="SESNSP. Incidencia Delictiva del Fuero Común, 2015-2019.",
      y=METADATOS$UNIDAD_MEDIDA[METADATOS$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2))], x=ifelse(GRAF_MES,"Mes", "Año")) +
      scale_color_manual(name= "",
                         values = colors) +
      scale_y_continuous(limits = 
      c(0,ifelse(TIPO_VALOR!="Absolutos", ifelse(val_max<1,1,ifelse(val_max<5,5,10*ceiling(val_max/10))),ceiling(val_max/(10^(nchar(val_max)-1)))*10^(nchar(val_max)-1)))) +
      scale_x_discrete(breaks = GRAF_IND$ORDEN, labels = GRAF_IND$ANIO_MES) +
      theme(panel.background = element_rect(fill = "white",colour = "white",size = 0.5)) +
      theme(panel.grid.minor = element_blank()) +
      theme(legend.title = element_blank()) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
      if (GRAF_MES){
        GRAF_GGPLOT + geom_vline(xintercept = (1+12*(1:(floor(dim(GRAF_IND)[1]/12)))), linetype="dotted")
      } else{
        GRAF_GGPLOT
      }
    }, height = 500)
  })
})
observeEvent(input$btnMeta, {
  IND_SELEC <- as.numeric(substr(input$lstIndicador,1,2))
  showModal(modalDialog(
    withMathJax(HTML(paste0(
      '<div id="Cap1">
        <center><h4 style="color:#527F6D;"><b>',METADATOS$INDICADOR[METADATOS$ID_INDICADOR==IND_SELEC],'</b></h4></center><br>
        <b>Fuente de información: </b>',METADATOS$FUENTE[METADATOS$ID_INDICADOR==IND_SELEC],'<br><br>
        <b>Periodicidad: </b>' ,METADATOS$PERIODICIDAD[METADATOS$ID_INDICADOR==IND_SELEC],'<br><br>
        <b>Frecuencia: </b>',METADATOS$FRECUENCIA[METADATOS$ID_INDICADOR==IND_SELEC],'<br><br>
        <b>Fórmula de calculo:</b><br><br>' ,METADATOS$FORMULA[METADATOS$ID_INDICADOR==IND_SELEC],'<br><br>
        <b>Desglose geográfico: </b>',METADATOS$DESGLOSE_GEO[METADATOS$ID_INDICADOR==IND_SELEC],'<br><br>
        <b>Desglose conceptual: </b>',METADATOS$VARS_DESGLOSE[METADATOS$ID_INDICADOR==IND_SELEC],'<br><br>
        <b>Nota(s): </b>',METADATOS$NOTA[METADATOS$ID_INDICADOR==IND_SELEC],'<br><br>
      </div>'
  ))),
  easyClose = TRUE,
  footer = NULL
  ))
})

observe({
  req(input$coords)
  print(input$coords)
})
subTend <- reactive({
  req(input$lstIndicador)
  if(input$lstIndicador=="Seleccionar...") return(NULL)
  IND_SELEC <- input$lstIndicador
  BD_IND <- read.csv(paste0(Mi_Path,"Indicadores/Ind_",as.numeric(substr(IND_SELEC,1,2)),".csv"),header = T, stringsAsFactors = F,encoding = "latin1")
  
  GRAF_IND <- aggregate(INDICADOR~CVE_MES+ANIO,data=BD_IND[BD_IND$CVE_ENT==0 & BD_IND$CVE_MES!=0,], FUN=sum)
  
  GRAF_IND$MES <- ""
  for(i in GRAF_IND$CVE_MES) GRAF_IND$MES[GRAF_IND$CVE_MES==i] <- CAT_MES$MES[CAT_MES$CVE_MES==i]
  
  GRAF_IND$ANIO_MES <- paste0(GRAF_IND$MES," de ",GRAF_IND$ANIO)
  GRAF_IND$FECHA <- as.Date(paste0(GRAF_IND$ANIO,"-", formatC(GRAF_IND$CVE_MES,width = 2,flag = "0"),"-01"))
  GRAF_IND$ORDEN <- formatC(1:dim(GRAF_IND)[1],width = 3,flag = "0")
  GRAF_IND$ETIQUETA <- "Indicador"
  GRAF_IND <- GRAF_IND[,c("ORDEN", "FECHA", "ANIO_MES", "INDICADOR", "ETIQUETA") ]
  GRAF_IND$VALOR_GRAF <- GRAF_IND$INDICADOR
  
  return(GRAF_IND)
})
output$hover_info_g <- renderUI({
  req(input$plot_hover_g)
  hover <- input$plot_hover_g
  point <- nearPoints(subTend(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  if(nrow(point) == 0) return(NULL)
  
  left_pct <- (hover$x - hover$domain$left) / (hover$domaingright - hover$domain$left)
  top_pct <- (hover$domain$top - hoverfy) / (hover$domain$top - hover$domain$bottom)
  
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$rangeg$bottom - hover$range$top)
  
  style <- paste0("position: absolute; z-index:100; background-color: rgba(221, 75, 57, 8.5); ", "left:", left_px-90, "px; top:", top_px+260, "px;")
  
  wellPanel(
    style = style,
    p(HTML(paste0("<b>", point$ANIO_MES,":</b>" ,round(point$VALOR_GRAF,1), ifelse(METADATOS$TIPO_VALOR[METADATOS$ID_INDICADOR==as.numeric(substr(input$lstIndicador,1,2))]=="Porcentaje","%",""))))
  )
})
subTend2 <- reactive({
  req(input$llstIndicador, input$lstEntGraf, input$opcTempGraf)
  if(input$lstIndicador=="Seleccionar...") return(NULL)
  IND_SELEC <- input$lstIndicador
  ENT_SELEC <- input$lstEntGraf
  BD_IND <- read.csv(paste0(Mi_Path, "Indicadores/Ind_",as.numeric(substr(IND_SELEC,1,2)),".csv"),header = T,stringsAsFactors = F,encoding = "latin1")
  GRAF_MES <- input$opcTempGraf=="opcMesGraf"
  if (GRAF_MES){
    GRAF_IND <- aggregate(INDICADOR~CVE_MES+ANIO, data=BD_IND[BD_IND$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)) & BD_IND$CVE_MES!=0, ], FUN=sum)
    GRAF_IND$MES <- ""
    for(i in GRAF_IND$CVE_MES) GRAF_IND$MES[GRAF_IND$CVE_MES==i] <- CAT_MES$MES[CAT_MES$CVE_MES==i]
    GRAF_IND$ANIO_MES <- paste0(substr(GRAF_IND$MES,1,3)," ",GRAF_IND$ANIO-2000)
    GRAF_IND$FECHA <- as.Date(paste0(GRAF_IND$ANIO,"-",formatC(GRAF_IND$CVE_MES,width = 2,flag = "0"),"-01"))
    mes_max <- 0
  } else{
    GRAF_IND <- aggregate(INDICADOR~CVE_MES+ANIO, data=BD_IND[BD_IND$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)) & BD_IND$CVE_MES==0,], FUN=sum)
    if(F)#as.numeric(substr(IND_SELEC,1,2))%in%BD_DFC_AM$ID_INDICADOR)
      GRAF_IND <- rbind(BD_DFC_AM[BD_DFC_AM$ID_INDICADOR==as.numeric(substr(IND_SELEC,1,2)) & BD_DFC_AM$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)),-c(1,2)],GRAF_IND)
    GRAF_IND$MES <- "Total"
    GRAF_IND$ANIO_MES <- paste0(GRAF_IND$ANIO)
    GRAF_IND$FECHA <- as.Date(paste0(GRAF_IND$ANIO, "-","12-31"))
    mes_max <- max(BD_IND$CVE_MES[BD_IND$ANIO==max(BD_IND$ANIO) ] )
    if(mes_max<12){
      mod_mes <- lm(log(INDICADOR) ~ as.numeric(CVE_MES), data=aggregate(INDICADOR~CVE_MES+ANIO,data=BD_IND[BD_IND$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)) & BD_IND$CVE_MES!=0 & BD_IND$ANIO==max(BD_IND$ANIO),],FUN=sum))
      GRAF_IND$INDICADOR[GRAF_IND$ANIO==max(GRAF_IND$ANIO)] <-
      sum(aggregate(INDICADOR~CVE_MES+ANIO,data=BD_IND[BD_IND$CVE_ENT==as.numeric(substr(ENT_SELEC,1,2)) & BD_IND$CVE_MES!=0 & BD_IND$ANIO==max(BD_IND$ANIO),],FUN=sum)$INDICADOR)+
      sum(as.data.frame(exp(predict(mod_mes,list(CVE_MES=(mes_max+1):12),interval = "confidence")),stringsAsFactors = F)$fit)
    }
  }
  GRAF_IND$ORDEN <- formatC(1:dim(GRAF_IND)[1],width = 3,flag = "0")
  GRAF_IND$ETIQUETA <- "Observado"
  if(GRAF_MES){
    modelo <- lm(log(INDICADOR+10) ~ as.numeric(ORDEN), data=tail(GRAF_IND,12))
    x_predic <- max(as.numeric(GRAF_IND$ORDEN))+(1:5)
    x <- predict(modelo, list(ORDEN=x_predic),interval = "confidence")
    x <- exp(x)
    x[,3] <- x[,1]+(x[,1]-x[,2])
    prediccion <- as.data.frame(x-10,stringsAsFactors = F)
    GRAF_IND_PRED <- data.frame(CVE_MES=x_predic %% 12,ANIO=0,
                                INDICADOR=prediccion$fit,
                                MES="",ANIO_MES="",FECHA="",
                                ORDEN=formatC(x_predic,width = 3,flag = "0"),
                                ETIQUETA="Pronosticado",
                                stringsAsFactors = F)
    
    GRAF_IND_PRED$CVE_MES[GRAF_IND_PRED$CVE_MES==0] <- 12
    GRAF_IND_PRED$ANIO[1] <- round(GRAF_IND$ANIO[dim(GRAF_IND)[1]]+GRAF_IND$CVE_MES[dim(GRAF_IND)[1]]/24)
    for(i in 2:dim(GRAF_IND_PRED)[1]) GRAF_IND_PRED$ANIO[i] <- round(GRAF_IND_PRED$ANIO[i-1]+GRAF_IND_PRED$CVE_MES[i-1]/24)
    for(i in GRAF_IND_PRED$CVE_MES) GRAF_IND_PRED$MES[GRAF_IND_PRED$CVE_MES==i] <- CAT_MES$MES[CAT_MES$CVE_MES==i]
    GRAF_IND_PRED$ANIO_MES <- paste0(substr(GRAF_IND_PRED$MES,1,3),",",GRAF_IND_PRED$ANIO- 2000)
    
    GRAF_IND_PRED$FECHA <- as.Date(paste0(GRAF_IND_PRED$ANIO, "-", formatC(GRAF_IND_PRED$CVE_MES,width = 2,flag = "0"),"-01"))
  } else{
    modelo <- lm(log(INDICADOR+10) ~ ANIO, data=GRAF_IND)
    x_predic <- max(GRAF_IND$ANIO)+(1:5)
    x <- predict(modelo, list(ANIO=x_predic), interval = "confidence")
    x <- exp(x)
    x[,3] <- x[,1]+(x[,1]-x[,2])
    
    prediccion <- as.data.frame(x-10,stringsAsFactors = F)
    GRAF_IND_PRED <- data.frame(CVE_MES=0,ANIO=x_predic,
                                INDICADOR=prediccion$fit,
                                MES="Total",ANIO_MES=paste0(x_predic),FECHA=as.Date(paste0(x_predic,"-","12-31")),
                                ORDEN=formatC(max(as.numeric(GRAF_IND$ORDEN))+(1:5),width = 3,flag = "0"),
                                ETIQUETA="Pronosticado",
                                stringsAsFactors = F)
  }
  GRAF_IND <- rbind(GRAF_IND,GRAF_IND_PRED)
  GRAF_IND_PRED$ETIQUETA <- "Límite inferior"
  GRAF_IND_PRED$INDICADOR <- prediccion$lwr
  GRAF_IND <- rbind(GRAF_IND,GRAF_IND_PRED)
  GRAF_IND_PRED$ETIQUETA <- "Limite superior"
  GRAF_IND_PRED$INDICADOR <- prediccion$upr
  GRAF_IND <- rbind(GRAF_IND,GRAF_IND_PRED)
  GRAF_IND$INDICADOR[GRAF_IND$INDICADOR<0 & GRAF_IND$ETIQUETA%in%c("Límite inferior","Límite superior")] <- 0
  GRAF_IND <- GRAF_IND[,c("ORDEN", "FECHA", "ANIO_MES", "INDICADOR", "ETIQUETA")]
  GRAF_IND$VALOR_GRAF <- GRAF_IND$INDICADOR
  return(list(GRAF_IND=GRAF_IND,GRAF_MES=GRAF_MES,mes_max=mes_max))
})

output$hover_info_g2 <- renderUI({
  req(input$plot_hover_g2)
  hover <- input$plot_hover_g2
  consulta_x <- subTend2()
  point <- nearPoints(consulta_x$GRAF_IND, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  
  if(nrow(point) == 0) return(NULL)
  
  left_pct <- (hover$x - hover$domaingleft) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  style <- paste0("position:absolute; z-index:100; background-color: rgba(221, 75, 57, 0.5); ", "left:", left_px+110, "px; top:", top_px+180, "px;")
  
  wellPanel(
    style = style,
    p(HTML(paste0(ifelse(point$ETIQUETA!="0bservado",paste0("<u>",point$ETIQUETA,"</u><br><b>"),ifelse(!consulta_x$GRAF_MES & consulta_x$mes_max<12 &
    as.numeric(point$ORDEN)==max(as.numeric(consulta_x$GRAF_IND$ORDEN[consulta_x$GRAF_IND$ETIQUETA=="Observado"])),
    paste0("<u>0bservado</u>: ",ifelse(consulta_x$mes_max==1,substr(CAT_MES$MES[1],1,3),paste0(substr(CAT_MES$MES[1],1,3),"-",
    substr(CAT_MES$MES[consulta_x$mes_max],1,3))),"<br><u>Pronosticado</u>: ",
    ifelse(consulta_x$mes_max==11,substr(CAT_MES$MES[12],1,3),paste0(substr(CAT_MES$MES[consulta_x$mes_max+1],1,3),"-", substr(CAT_MES$MES[12],1,3))),"<br><b>"),"<b>")),
    point$ANIO_MES,": </b>",round(point$VALOR_GRAF,1),ifelse(METADATOS$TIPO_VALOR[METADATOS$ID_INDICADOR==as.numeric(substr(input$lstIndicador,1,2))]=="Porcentaje","%",""))))
  )
})
observe({
  ##### Funciones para Actualizaciones #####
  output$UI_CARGA <- NULL
  NOM_BD_CARGA <- input$lstCargaBD
  
  if(!is.null(input$pathBase) & NOM_BD_CARGA!="Seleccionar..."){
    output$UI_CARGA <- renderuI({
      actionButton("btnCarga", paste0("Cargar '",NOM_BD_CARGA,"'"),icon = icon("download"))
    })
  }
})
observeEvent(input$btnCarga, {
  METADATOS <- read.csv(paste0(Mi_Path, "Catálogos/Metadatos.csv"),header = T,stringsAsFactors = F, encoding = "latin1")
  NOM_BD_CARGA <- input$lstCargaBD
  BD_CARGA <- read.csv(input$pathBase$datapath, header=T,stringsAsFactors = F,encoding = "latin1")
  if("Total"%in%names(BD_CARGA)) BD_CARGA <- BD_CARGA[,names(BD_CARGA)[-which(names(BD_CARGA)=="Total")]]
  vars_mes <- names(BD_CARGA)%in%c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  BD_CARGA[vars_mes] <- lapply(BD_CARGA[vars_mes], gsub, pattern = ",", replacement = "")
  BD_CARGA[vars_mes] <- lapply(BD_CARGA[vars_mes], as.numeric)
  BD_CARGA_ORIG <- BD_CARGA
  
  if(NOM_BD_CARGA=="Delitos del Fuero Común (IDEFC)"){
    withProgress(message = 'Carga en progreso', value = 0,detail="0%", {
      BD_CARGA$CONCATENA <- paste(BD_CARGA$Bien.jurídico.afectado, BD_CARGA$Tipo.de.delito, BD_CARGA$Subtipo.de.delito, BD_CARGA$Modalidad,sep = "_")
      CAT_DELITOS_FC$CONCATENA <- paste(CAT_DELITOS_FC$BIEN_JURIDICO, CAT_DELITOS_FC$TIPO,CAT_DELITOS_FC$SUBTIPO,CAT_DELITOS_FC$MODALIDAD, sep = "_")
      BD_CARGA <- merge(BD_CARGA, CAT_DELITOS_FC[,c("CONCATENA", "CVE_BIEN_JURIDICO", "CVE_TIPO", "CVE_SUBTIPO", "CVE_MODALIDAD", "PRIORITARIOS")],by=c("CONCATENA"),all.x = T)
      incProgress(1/10,detail = paste0(1*10,"%"))
      BD_CARGA$CONCATENA <- NULL
      BD_CARGA <- BD_CARGA[,-which(names(BD_CARGA)%in%c("Entidad","Bien.jurídico.afectado","Tipo.de.delito","Subtipo.de.delito","Modalidad"))]
      BD_CARGA <- melt(BD_CARGA, id=c("Año","Clave_Ent","CVE_BIEN_JURIDICO","CVE_TIPO","CVE_SUBTIPO","CVE_MODALIDAD","PRIORITARIOS"),variable.name = "MES",value.name = "Delitos")
      incProgress(1/10,detail = paste0(2*10,"%"))
      BD_CARGA <- merge(BD_CARGA, CAT_MES, by=c("MES"),all.x=T)
      incProgress(1/10,detail = paste0(3*10,"%"))
      names(BD_CARGA) <- c("MES", "ANIO", "CVE_ENT", "BIEN_JURIDICO", "TIPO_DELITO", "SUBTIPO_DELITO", "MODALIDAD" , "PRIORITARIOS", "DELITOS", "CVE_MES")
      BD_CARGA <- BD_CARGA[,c("ANIO", "CVE_MES", "CVE_ENT", "BIEN_JURIDICO", "TIPO_DELITO", "SUBTIPO_DELITO", "MODALIDAD" , "PRIORITARIOS", "DELITOS") ]
      BD_CARGA$DELITOS <- as.numeric(BD_CARGA$DELITOS)
      BD_CARGA <- BD_CARGA[!is.na(BD_CARGA$DELITOS),]
      incProgress(1/10,detail = paste0(4*10,"%"))
      write.csv(BD_CARGA, paste0(Mi_Path,"Bases de Datos/BD_DFC.csv"),row.names = F,fileEncoding = "latin1")
      incProgress(1/10,detail = paste0(5*10,"%"))
      CalcularInd(BD_CARGA,POB_CONAPO,Mi_Path,METADATOS, "IDEFC")
      incProgress(3/10,detail = paste0(8*10,"%"))
      BD_DFC <- read.csv(paste0(Mi_Path,"Bases de Datos/BD_DFC.csv"),header = T,stringsAsFactors = F,encoding = "latin1")
      incProgress(1/10,detail = paste0(9*10,"%"))
      write.csv(BD_CARGA_ORIG, paste0(Mi_Path, "Bases de Datos/Cargadas/IDEFC_NM_",tolower(substr(CAT_MES$MES[CAT_MES$CVE_MES==max(BD_CARGA$CVE_MES[BD_CARGA$ANIO==max(BD_CARGA$ANIO)])],1,3)),substr(max(BD_CARGA$ANIO),3,4),".csv"),row.names = F,fileEncoding = "latin1")
      incProgress(1/10,detail = paste0(10*10,"%"))
    })
    
    showModal(modalDialog(
      title = "Carga finalizada",
      paste0("Se cargó correctamente la base de datos '",NOM_BD_CARGA,"', a partir del archivo seleccionado."),footer = modalButton("Cerrar", icon = icon("close"))
    ))
  }
  if(NOM_BD_CARGA=="Victimas de delitos del Fuero Común (IDVFC)"){
    withProgress(message = 'Carga en progreso', value = 0,detail="0%", {
      BD_CARGA$CONCATENA <- paste(BD_CARGA$Bien.jurídico.afectado, BD_CARGA$Tipo.de.delito, BD_CARGA$Subtipo.de.delito,BD_CARGA$Modalidad, sep = "")
      CAT_DELITOS_FC$CONCATENA <- paste(CAT_DELITOS_FC$BIEN_JURIDICO,CAT_DELITOS_FC$TIPO,CAT_DELITOS_FC$SUBTIPO,CAT_DELITOS_FC$MODALIDAD, sep = "")
      BD_CARGA <- merge(BD_CARGA,CAT_DELITOS_FC[,c("CONCATENA", "CVE_BIEN_JURIDICO", "CVE_TIPO", "CVE_SUBTIPO", "CVE_MODALIDAD", "PRIORITARIOS") ],by=c("CONCATENA"),all.x = T)
      incProgress(1/10,detail = paste0(1*10,"%"))
      BD_CARGA$CONCATENA <- NULL
      BD_CARGA <- merge(BD_CARGA, CAT_SEXO, by.x=c("Sexo") ,by.y=c("SEXO_DESC"),all.x = T)
      incProgress(1/10,detail = paste0(2*10,"%"))
      BD_CARGA <- merge(BD_CARGA, CAT_EDAD, by.x=c("Rango.de.edad") ,by.y=c("EDAD_DESC"),all.x = T)
      incProgress(1/10,detail = paste0(3*10,"%"))
      BD_CARGA <- BD_CARGA[, -which(names(BD_CARGA)%in%c("Entidad","Bien.jurídico.afectado","Tipo.de.delito","Subtipo.de.delito","Modalidad","Sexo","Rango.de.edad"))]
      BD_CARGA <- melt(BD_CARGA, id=c("Año","Clave_Ent","CVE_BIEN_JURIDICO","CVE_TIPO","CVE_SUBTIPO","CVE_MODALIDAD","PRIORITARIOS","SEXO","EDAD"),variable.name = "MES",value.name = "Delitos")
      i <- sapply(BD_CARGA, is.factor)
      BD_CARGA[i] <- lapply(BD_CARGA[i], as.character)
      incProgress(1/10,detail = paste0(4*10,"%"))
      BD_CARGA <- merge(BD_CARGA,CAT_MES,by=c("MES"),all.x=T)
      incProgress(1/10,detail = paste0(5*10,"%"))
      names(BD_CARGA) <- c("MES","ANIO" ,"CVE_ENT","BIEN_JURIDICO", "TIPO_DELITO", "SUBTIPO_DELITO", "MODALIDAD", "PRIORITARIOS", "SEXO", "EDAD", "DELITOS","CVE_MES")
      
      BD_CARGA <- BD_CARGA[,c("ANIO","CVE_MES","CVE_ENT","BIEN_JURIDICO","TIPO_DELITO","SUBTIPO_DELITO","MODALIDAD","PRIORITARIOS","SEXO","EDAD","DELITOS")]
      BD_CARGA$DELITOS <- as.numeric(BD_CARGA$DELITOS)
      BD_CARGA <- BD_CARGA[!is.na(BD_CARGA$DELITOS), ]
      incProgress(1/10,detail = paste0(6*10,"%"))
      write.csv(BD_CARGA, paste0(Mi_Path, "Bases de Datos/BD_VFC.csv"),row.names = F,fileEncoding = "latin1")
      incProgress(1/10,detail = paste0(7*10,"%"))
      CalcularInd(BD_CARGA, POB_CONAPO,Mi_Path,METADATOS, "IDVFC")
      incProgress(1/10,detail = paste0(8*10,"%"))
      BD_VFC <- read.csv(paste0(Mi_Path, "Bases de Datos/BD_VFC.csv"),header = T,stringsAsFactors = F,encoding = "latin1")
      incProgress(1/10,detail = paste0(9*10,"%"))
      write.csv(BD_CARGA_ORIG, paste0(Mi_Path,"Bases de Datos/Cargadas/IDVFC_NM_",tolower(substr(CAT_MES$MES[CAT_MES$CVE_MES==max(BD_CARGA$CVE_MES[BD_CARGA$ANIO==max(BD_CARGA$ANIO)])],1,3)), substr(max(BD_CARGA$ANIO) ,3,4),".csv"),row.names = F,fileEncoding = "latin1")
      incProgress(1/10,detail = paste0(10*10,"%"))
    })
    
    showModal(modalDialog(
      title = "Carga finalizada",
      paste0("Se cargó correctamente la base de datos '",NOM_BD_CARGA,"', a partir del archivo seleccionado."),
      footer = modalButton("Cerrar", icon = icon("close"))
    ))
  }
  if(NOM_BD_CARGA=="Delitos del Fuero Federal (IDEFF)"){
    withProgress(message = 'Carga en progreso', value = 0,detail="0%", {
      BD_CARGA$CONCATENA <- paste(BD_CARGA$LEY,BD_CARGA$CONCEPTO,BD_CARGA$TIPO,sep = "_")
      CAT_DELITOS_FF$CONCATENA <- paste(CAT_DELITOS_FF$LEY,CAT_DELITOS_FF$CONCEPTO,CAT_DELITOS_FF$TIPO,sep = "_")
      BD_CARGA <- merge(BD_CARGA,CAT_DELITOS_FF[,c("CONCATENA", "CVE_LEY", "CVE_CONCEPTO", "CVE_TIPO")], by=c("CONCATENA"),all.x = T)
      incProgress(1/10,detail = paste0(1*10,"%"))
      BD_CARGA$CONCATENA <- NULL
      BD_CARGA <- BD_CARGA[,-which(names(BD_CARGA)%in%c("ENTIDAD", "LEY", "CONCEPTO", "TIPO"))]
      BD_CARGA <- melt(BD_CARGA, id=c("ANO", "INEGI", "CVE_LEY", "CVE_CONCEPTO", "CVE_TIPO"), variable.name = "MES", value.name = "Delitos")
      incProgress(1/10,detail = paste0(2*10,"%"))
      i <- sapply(BD_CARGA, is.factor)
      BD_CARGA[i] <- lapply(BD_CARGA[i], as.character)
      BD_CARGA$MES <- paste0(substr(BD_CARGA$MES,1,1),tolower(substr(BD_CARGA$MES, 2, nchar(BD_CARGA$MES) ) ))
      BD_CARGA <- merge(BD_CARGA, CAT_MES, by=c("MES"),all.x=T)
      incProgress(1/10,detail = paste0(3*10,"%"))
      names(BD_CARGA) <- c("MES","ANIO","CVE_ENT", "LEY", "CONCEPTO", "TIPO", "DELITOS", "CVE_MES")
      BD_CARGA <- BD_CARGA[,c("ANIO", "CVE_MES", "CVE_ENT", "LEY", "CONCEPTO", "TIPO", "DELITOS")]
      BD_CARGA$CVE_ENT[is.na(BD_CARGA$CVE_ENT)] <- 33
      BD_CARGA$DELITOS <- as.numeric(BD_CARGA$DELITOS)
      incProgress(1/10,detail = paste0(4*10,"%"))
      BD_CARGA <- BD_CARGA[!is.na(BD_CARGA$DELITOS),]
      incProgress(1/10,detail = paste0(5*10,"%"))
      write.csv(BD_CARGA, paste0(Mi_Path, "Bases de Datos/BD_DFF.csv"),row.names = F,fileEncoding = "latin1")
      incProgress(1/10,detail = paste0(7*10,"%"))
      BD_DFF <- read.csv(paste0(Mi_Path, "Bases de Datos/BD_DFF.csv"),header = T,stringsAsFactors = F,encoding = "latin1")
      incProgress(1/10,detail = paste0(9*10,"%"))
      write.csv(BD_CARGA_ORIG, paste0(Mi_Path, "Bases de Datos/Cargadas/IDEFF_NM_", tolower(substr(CAT_MES$MES[CAT_MES$CVE_MES==max(BD_CARGA$CVE_MES[BD_CARGA$ANIO==max(BD_CARGA$ANIO) ])],1,3)), substr(max(BD_CARGA$ANIO) ,3,4),".csv"),row.names = F,fileEncoding = "latin1")
      incProgress(1/10,detail = paste0(10*10,"%"))
    })
    
    showModal(modalDialog(
      title = "Carga finalizada",
      paste0("Se cargó correctamente la base de datos '",NOM_BD_CARGA,"', a partir del archivo seleccionado."),
      footer = modalButton("Cerrar", icon = icon("close"))
    ))
  }
})

METADATOS <- read.csv(paste0(Mi_Path,"Catálogos/Metadatos.csv"),header = T,stringsAsFactors = F,encoding = "latin1")
observeEvent(input$btnDescarga, {
  NOM_BD_DESCARGA <- input$lstBaseCruces
  
  if(NOM_BD_DESCARGA!="Seleccionar..."){
    ARCHIVO_FREC <- paste0(gsub("Documents", "Downloads/",path.expand('~')),NOM_BD_DESCARGA,".csv")
    
    BD_BDS <- data.frame(ARCHIVO=sub('\\.RData$', '', list.files(path = paste0(Mi_Path,"/Bases de Datos/Cargadas"),pattern = "\\.csv$")),stringsAsFactors = F)
    
    BD_BDS$NOMBRE <- gsub(".csv","",BD_BDS$ARCHIVO)
    BD_BDS$TIPO <- substr(BD_BDS$NOMBRE, 1,5)
    BD_BDS$ANIO <- as.numeric(substr(BD_BDS$NOMBRE, nchar(BD_BDS$NOMBRE)-1, nchar(BD_BDS$NOMBRE)))
    BD_BDS$MES <- substr(BD_BDS$NOMBRE,nchar(BD_BDS$NOMBRE)-4,nchar(BD_BDS$NOMBRE)-2)
    x <- cbind(CAT_MES, data.frame(ABR_MES=tolower(substr(CAT_MES$MES,1,3)),stringsAsFactors = F))
    
    BD_BDS$ID_MES <- 0
    for(i in unique(BD_BDS$MES)) BD_BDS$ID_MES[BD_BDS$MES==i] <- x$CVE_MES[x$ABR_MES==i]
    BD_BDS <- BD_BDS[BD_BDS$TIPO==TIPO_BASES[NOM_BD_DESCARGA],]
    
    BD_DESCARGA <- read.csv(paste0(Mi_Path, "Bases de Datos/Cargadas/" ,BD_BDS$NOMBRE[order(BD_BDS$ANIO,BD_BDS$ID_MES, decreasing = T)][1],".csv"),header = T,stringsAsFactors = F,encoding = "latin1")
    write.table(BD_DESCARGA, ARCHIVO_FREC, sep = ",", col.names = T, row.names = F)
    
    if(file.exists(ARCHIVO_FREC)){
      showModal (modalDialog(
        title = "Descarga finalizada",
        paste0("Se descargó correctamente la base de datos '",NOM_BD_DESCARGA,"', la cual se encuentra en la carpeta de descargas de su equipo."),
        footer = modalButton("Cerrar", icon = icon("close"))
      ))
    }
  } else{
    showModal(modalDialog(
      title = "Error al descargar",
      paste0("Debe seleccionar una base de datos."),
      footer = modalButton("Cerrar", icon = icon("close"))
    ))
  }
})
}

##### 05 - Ejecutar aplicacio'n #####
shinyApp(ui, server)
