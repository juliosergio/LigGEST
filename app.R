# ========================
# LigGEST0.R
# Interfaz para manejar Links
# 
# ========================
library(shiny)
library(shinyjs)
library(DT)
if (!exists("LEIDO.MiBiblioteca")) source("../RR/MiBiblioteca.R", chdir = T)
if (!exists("LEIDO.Intercala")) source("../RR/Intercala.R", chdir = T)
source("Ligas.R")
# debugSource("Ligas.R")

# === Estructuras Globales ===
MisLinks <- NULL # Tabla Global de Links
subLinks <- NULL # Un fragmento de MisLinks
displTable <- NULL # Tabla a desplegar


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


groupTexts <- function(t0=tabIni, apnd="") {
    # Debe entregar el resultado como una lista
    lapply(
        1:nrow(t0), 
        function(i)
            with(t0[i,],
                textInput(name %,% apnd, descr, value)
                # print(paste(name, descr, value))
            )
    )
}

ArmaJS_InSet <- function (elt, Set, eltIsJSVar = T) {
    # Implementa una expresión en JavaScript
    # para determinar si el elemento elt está
    # en el conjunto Set. 
    # ambos argumentos son strings y el resultado
    # también.
    encl <- if (eltIsJSVar) "" else "'"
    "'" %,% Set %,% "'.match(RegExp(" %,% encl %,%
        elt %,% encl %,%")) != null"
}

stylizedDT <- function(ddf, ...) {
    datatable(ddf, 
              options = list(scrollX=T, style='bootstrap'), ...)
}

# =====================================

ui <- fluidPage(
    useShinyjs(),  # Set up shinyjs
    tags$head(tags$style("#dspTbl {white-space: nowrap;}")),
    fluidRow(
        column(4,img(height=80, width=80*591/203, src="logoImtaM.png")),
        column(7,
               h2(em(strong("LigGEST:"), "Administrador de links de la Web")),
               offset = 0)
    ),
    hr(),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "op", "Operación",
                c("", 
                  "Crea Archivo",
                  "Lee Archivo",
                  "Guarda Archivo",
                  "Agrega Registro",
                  "Busca en estructura",
                  "Prueba tags",
                  "Modifica Registro",
                  "Elimina Registros",
                  "Ver tabla completa"
                  )
            ),
            tags$hr(),
            conditionalPanel(
                condition = "input.op == 'Crea Archivo'",
                fileInput("fdat","Archivo HTML",accept = c("text/html", "text/xml"))
            ),
            conditionalPanel(
                condition = "input.op == 'Lee Archivo'",
                fileInput("fdat0","Archivo rds",accept = c("application/rds", "application/RDS"))
            ),
            conditionalPanel(
                condition = "input.op == 'Guarda Archivo'",
                textInput("fnam", "Nombre Archivo(.rds)", "")
            ),
            conditionalPanel(
                condition = "input.op == 'Agrega Registro'",
                groupTexts()
            ),
            conditionalPanel(
                condition = "input.op == 'Busca en estructura'",
                checkboxGroupInput("selcols", "Columnas a buscar", vtnams, vtnams),
                radioButtons("tbusc", "Tipo de búsqueda", c("Texto-simple"="txt", "Expr-regular"="rgexp")),
                textInput("rgtxt", "RegExpr o Texto a buscar:")
            ),
            conditionalPanel(
                condition = "input.op == 'Modifica Registro'",
                numericInput("regNum","Número del registro:", value = 0, min = 0, max = maxR, step = 1),
                conditionalPanel(
                    condition = "+input.regNum > 0",
                    groupTexts(tabEdt, "E")
                )
            ),
            conditionalPanel(
                condition = "input.op == 'Prueba tags'",
                fluidRow(
                    column(8, textInput("tags0","Tags, separados por \",\"")),
                    column(2, br(), actionButton("go0", "", icon = icon("check"))) #, lib="glyphicon")))
                ),
                uiOutput(
                    "oMask"
                )
            ),
            conditionalPanel(
                condition = "input.op == 'Elimina Registros'",
                textInput("regNum0","Números de registros separados por ','")
            ),
            conditionalPanel(
                condition = "input.op != 'Guarda Archivo'",
                wellPanel(actionButton("go1", "Ejecuta")) 
            ),
            conditionalPanel(
                condition = "input.op == 'Guarda Archivo'",
                wellPanel(downloadButton("downloadData", "Descarga Tabla"))
            )
        ),
        mainPanel(
            dataTableOutput("dspTbl")
        )
    )
)

server <- function(input, output) {
    
    dTags <- eventReactive(input$go0, {
        strsplit(input$tags0, E_SepComma)[[1]]
    })
    vv <- reactiveValues(Vop=NULL)
    observeEvent(input$go1, {
        # Vop(      # input$op)
        # renderText(
            print("Aquí toy")
            op <- switch(
                input$op, 
                "Crea Archivo"="C",
                "Lee Archivo"="L",
                  #>>  "Guarda Archivo"="G",
                "Agrega Registro"="A",
                "Busca en estructura"="B",
                "Prueba tags"="P",
                "Modifica Registro"="M",
                "Elimina Registros"="E",
                "Ver tabla completa"="V"
            )
            switch (
                op,
                C = { # Crea la Tabla a partir de un HTML (tipo Delicious)
                    MisLinks <<- creaDe_HTML(input$fdat$datapath)
                    displTable <<- stylizedDT(MisLinks)
                },
                L = { # Crea la Tabla a partir de un archivo RDS
                    MisLinks <<- readRDS(input$fdat0$datapath)
                    displTable <<- stylizedDT(MisLinks)
                }
            )
            vv$Vop <- displTable
        #)
        # output$tipo <- Vop()
        # outputOptions(output, "tipo", suspendWhenHidden=F)
    }) #, priority = 1)
    observeEvent(input$fnam, {
        print("Entré")
        vv$Vop <- (displTable <<- stylizedDT(MisLinks))
    })
    # outputOptions(output, "tipo", suspendWhenHidden=F)
    # vals <- eventReactive(input$go1, {
        #">>>" %,% Vop() %,% "<<<"
        # ">>>" %,% output$tipo %,% "<<<"
        # tabEdt
        # c(
        #     input$op, "\n",
        #     "fnam:" %,% input$fdat$name, "\n",
        #     "fsize:" %,% input$fdat$size, "\n",
        #     "ftype:" %,% input$fdat$type, "\n",
        #     "fpath:" %,% input$fdat$datapath, "\n",
        #     "-----------------", "\n",
        #     "href:" %,% input$href, "\n",
        #     "tit:" %,% input$tit, "\n",
        #     "tags:" %,% input$tags, "\n",
        #     "nota:" %,% input$nota, "\n",
        #     "-----------------", "\n",
        #     "hrefE:" %,% input$hrefE, "\n",
        #     "titE:" %,% input$titE, "\n",
        #     "tagsE:" %,% input$tagsE, "\n",
        #     "notaE:" %,% input$notaE, "\n",
        #     "-----------------", "\n",
        #     "selcols:" %,% paste(input$selcols, collapse = ", "), "\n",
        #     "tbusc:" %,% input$tbusc, "\n",
        #     "rgtxt:" %,% input$rgtxt, "\n",
        #     "-----------------", "\n",
        #     "tags0:" %,% input$tags0, "\n",
        #     "iMask:" %,% paste(input$iMask, collapse = ", "), "\n",
        #     "-----------------", "\n",
        #     "regNum0" %,% input$regNum0, "\n"
        #  )
    #})
    
    output$dspTbl <- renderDataTable(vv$Vop)

    output$oMask <- renderUI({
        tg <- dTags()
        vtn <- 1:length(tg)
        names(vtn) <- "__" %,% tg
        checkboxGroupInput("iMask", "Mascara", vtn, vtn)
    })
    
    output$downloadData <- downloadHandler(
        filename = function () {
                print("fnam>>" %,% input$fnam)
                if (grepl("\\.rds$", input$fnam)) input$fnam else input$fnam %,% ".rds"
            },
        content = function(file) saveRDS(MisLinks, file)
    )
}

shinyApp(ui = ui, server = server)

