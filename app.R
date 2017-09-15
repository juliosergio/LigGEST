# ========================
# LigGEST0.R
# Interfaz para manejar Links
# 
# ========================
library(shiny)
if (!exists("LEIDO.MiBiblioteca")) source("../RR/MiBiblioteca.R", chdir = T)
if (!exists("LEIDO.Intercala")) source("../RR/Intercala.R", chdir = T)
source("Ligas.R")
# debugSource("Ligas.R")


#<-- DEFINICIONES
indir <- "./" # (***) Directorio inicial para búsqueda de archivos de datos

tabIni <- data.frame(
    name=c(
        "href",
        "tit",
        "tags",
        "nota"
    ),
    descr=c(
        "Liga (href):", 
        "Título:",
        "Tags separados por ',':",
        "Notas:"),
    value=rep("",4), 
    stringsAsFactors = F
)

tabEdt <- tabIni
tabEdt$value <- c("uno", "dos", "tres", "cuatro")
maxR = 800
cn <- c("descr","href","add_date","note","private","tags")
vtnams <- 1:length(cn)
names(vtnams) <- cn


groupTexts <- function(t0=tabIni) {
    # Debe entregar el resultado como una lista
    lapply(
        1:nrow(t0), 
        function(i)
            with(t0[i,],
                textInput(name, descr, value)
                # print(paste(name, descr, value))
            )
    )
}

# =====================================

ui <- fluidPage(
    h2(img(height=80, width=80*591/203, src="logoImtaM.png"),em(strong("LigGEST:"), "Administrador de links de la Web")),
    hr(),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "op", "Operación",
                c("", 
                  "Crea Archivo",
                  "Agrega Registro",
                  "Busca en estructura",
                  "Prueba tags",
                  "Modifica Registro",
                  "Elimina Registros",
                  "Ve tabla completa"
                  )
            ),
            tags$hr(),
            conditionalPanel(
                condition = "input.op == 'Crea Archivo'",
                fileInput("fdat","Archivo HTML",accept = c("text/html", "text/xml"))
            ),
            conditionalPanel(
                condition = "input.op == 'Agrega Registro'",
                groupTexts()
            ),
            conditionalPanel(
                condition = "input.op == 'Busca en estructura'",
                # textInput("txt", "Columnas a buscar\n FMT:c(i,...) o *=todas :"),
                checkboxGroupInput("selcols", "Columnas a buscar", vtnams, vtnams),
                radioButtons("tbusc", "Tipo de búsqueda", c("Texto-simple"="txt", "Expr-regular"="rgexp")),
                textInput("rgtxt", "RegExpr o Texto a buscar:")
            ),
            conditionalPanel(
                condition = "input.op == 'Modifica Registro'",
                numericInput("regNum","Número del registro:", value = 0, min = 0, max = maxR, step = 1),
                conditionalPanel(
                    condition = "+input.regNum > 0",
                    groupTexts(tabEdt)
                )
            ),
            conditionalPanel(
                condition = "input.op == 'Prueba tags'",
                fluidRow(
                    column(8, textInput("tags0","Tags, separados por \",\"")),
                    column(2, br(), actionButton("go0", "Actualiza"))
                ),
                uiOutput(
                    "oMask"
                )
            ),
            conditionalPanel(
                condition = "input.op == 'Elimina Registros'",
                textInput("regNum0","Números de registros separados por ','")
            ),
            # hr(),
            wellPanel(actionButton("go3", "Ejecuta")),
            width = 5
        ),
        mainPanel(
            # verbatimTextOutput("value")
        )
    )
)

server <- function(input, output) {
    dTags <- eventReactive(input$go0, {
        strsplit(input$tags0, E_SepComma)[[1]]
    })
    
    #output$value <- renderText({">>" %,% dTags() %,% "<<"})
    output$oMask <- renderUI({
        tg <- dTags()
        vtn <- 1:length(tg)
        names(vtn) <- "__" %,% tg
        checkboxGroupInput("iMask", "Mascara", vtn, vtn)
    })
}

shinyApp(ui = ui, server = server)







