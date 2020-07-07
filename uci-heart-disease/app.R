library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "Doenças cardíacas"),
                    dashboardSidebar(
                        # customizando o tema
                        tags$head(tags$style(HTML(' 
                                /* logo */
                                .skin-black .main-header .logo {
                                background-color: #F21835;
                                color: #FFFFFF;
                                border: #F21835;
                                }
                                /* logo when hovered */
                                .skin-black .main-header .logo:hover {
                                background-color:  #F21835;
                                }
                                /* navbar (rest of the header) */
                                .skin-black .main-header .navbar {
                                background-color: #F21835;
                                color: #FFFFFF;
                                }
                                /* main sidebar */
                                .skin-black .main-sidebar {
                                background-color: #4C5457;
                                }
                                /* active selected tab in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #C51B29;
                                color: #FFFFFF; 
                                }
                                /* other links in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #4C5457;
                                color: #FFFFFF; 
                                }
                                /* other links in the sidebarmenu when hovered */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #F21835;
                                color: #FFFFFF;
                                }
                                /* toggle button when hovered  */
                                .skin-black .main-header .navbar .sidebar-toggle:hover{
                                background-color: #C51B29;
                                color: #FFFFFF;
                                }
                                /* toggle button  */
                                .skin-black .main-header .navbar .sidebar-toggle{
                                background-color: #F21835;
                                color: #FFFFFF;
                                border: #F21835;
                                }
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #F5F4F6;
                                }
                                .shiny-notification {
                                position:fixed;
                                top: calc(50%);
                                left: calc(30%);
                                }
                                '))
                        ),
                        sidebarMenu(
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
                            menuItem("Dados", tabName = "dados", icon=icon("database")),
                            menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/maria-lgr/Teste-Oper/tree/master/uci-heart-disease")
                        )
                    ),
                    dashboardBody(
                        tabItems(
                            tabItem("dashboard", # criando a janela com as visualizações
                                    fluidRow(
                                        column(6,
                                               fluidRow(
                                                   column(12, 
                                                          selectInput('continua', "Variável dos boxplots", 
                                                                      choices = c("Idade"="age",
                                                                                  "Pressão sanguínea em repouso"="trestbps",
                                                                                  "Nível de colesterol"="chol",
                                                                                  "Máxima frequência cardíaca atingida"="thalach",
                                                                                  "Depressão do segmento ST"="oldpeak")
                                                          )
                                                   )
                                               ),
                                               fluidRow(
                                                   column(12,
                                                          tabsetPanel(type="tabs",
                                                                      tabPanel('Violin plot',
                                                                               plotlyOutput('violin',height = "300px")),
                                                                      tabPanel('Boxplot', 
                                                                               plotlyOutput('boxplot',height="300px"))
                                                          )
                                                   )
                                               ),
                                               fluidRow(
                                                   column(12,
                                                          fluidRow(
                                                              # breve resumo das conclusões da análise
                                                              p(strong("Quais variáveis influenciam na presença de doença cardíaca?")),
                                                              p("As variáveis idade, máxima frequência cardíaca atingida e 
                                                              depressão do segmento ST apresentam distribuições diferentes se a doença for presente ou não.
                                                              Já as variáveis sexo, tipo de dor no peito, angina induzido por exercício, 
                                                              inclinação do pico do segmento ST, número de vasos coloridos
                                                              pela fluroscopia e talassamia apresentam proporções discrepantes entre os dois grupos para alguns
                                                              de seus níveis. Por isso, podemos dizer que essas variáveis tem influência sobre 
                                                              a incidência de doença cardíaca."),
                                                              style="text-align:justify;color:#404040;background-color:#FFFFFF;
                                                               padding:15px;margin:10px;border-radius:7px"
                                                          )
                                                   )
                                               )
                                        ),
                                        column(6,
                                               fluidRow(
                                                   column(12,
                                                          selectInput('categorica', "Variável dos gráficos de barras", 
                                                                      choices = c("Faixa etária"="age_groups","Sexo"="sex",
                                                                                  "Dor no peito"="cp",
                                                                                  "Glicose acima de 120mg/dl"="fbs",
                                                                                  "Resultado do eletrocardiograma"="restecg",
                                                                                  "Angina induzida por exercício"="exang",
                                                                                  "Inclinação do pico do segmento ST"="slope",
                                                                                  "Número de vasos coloridos pela fluroscopia"="ca",
                                                                                  "Talassamia"="thal"), 
                                                                      selected="Idade")
                                                   )
                                               ),
                                               fluidRow(
                                                   box(width = 12,
                                                       fluidRow(
                                                           column(12,
                                                                  plotlyOutput('barplot_stacked',height="250px")
                                                           )
                                                       ),
                                                       fluidRow(
                                                           column(12, 
                                                                  plotlyOutput('barplot_percent',height="250px"))
                                                       )
                                                   )
                                               )
                                        )
                                    )
                            ),
                            tabItem("dados", # criando janela com os dados
                                    fluidRow(
                                        column(12,
                                               tabsetPanel(type="tabs",
                                                           tabPanel('Descrição', box(width=NULL,
                                                                     tags$div(HTML('
                                    <body style="background-color:#FFFFFF;text-align:justify;color:#404040;">
                                    <div style="margin-bottom:20px;">
                                    </div>
                                    <p>O banco de dados <b>Heart Disease</b> originalmente 
                                    continha 76 variáveis, mas apenas 14 delas eram realmente usadas em experimentos 
                                    publicados.</p> <p>Estas 14 variáveis são:</p> 
                                    <ol>
                                	    <li><b>age:</b> Idade em anos inteiros</li>
                                	    <li><b>sex:</b> Sexo feminino ou masculino</li>
	                                    <li><b>cp:</b> Dor no peito (angina típica, angina atípica,dor não-anginal,assintomático)</li>
                                	    <li><b>tresbps:</b> Pressão sanguínea em repouso</li>
                                	    <li><b>chol:</b> Colesterol no sangue (em mg/dl)</li>
                                	    <li><b>fbs:</b> Glicose em jejum acima de 120mg/dl</li>
                                	    <li><b>restecg:</b> Resultado do eletrocardiograma em repouso(normal,ondas ST-T anormais,
                                	    mostra hipertropia ventricular esquerda provável ou definitiva pelo critério de Estes)</li>
                                	    <li><b>thalach:</b> Frequência cardíaca máxima atingida</li>
                                	    <li><b>exang:</b> Angina induzida por exercício</li>
                                	    <li><b>oldpeak:</b> Depressão do segmento ST induzida</li>
                                	    <li><b>slope:</b> Inclinação do pico do segmento ST (ascendente,plana,descendente</li>
                                	    <li><b>ca:</b> Número de vasos coloridos pela fluoroscopia</li>
	                                    <li><b>thal:</b> 3 = normal, 6 = defeito corrigido, 7= defeito reversível</li>
	                                    <li><b>target:</b> Diagnóstico de doença (resultado do angiograma)</li>
                                    </ol> 
                                    <div style="margin-bottom:30px;">
                                    </div>
                                    <small>Fonte: <a href="url">ttps://archive.ics.uci.edu/ml/datasets/Heart+Disease</a></small>
                                    </body> ')))),
                                                           tabPanel('Banco', 
                                                                    column(12,
                                                                           box(width=12,DT::dataTableOutput('banco')
                                                                               )
                                                                           )
                                                           )
                                               )
                                        )
                                    )
                            )
                        )
                    )
)

server <- function(input, output) {
    
    # abrindo o banco
    dados <- read.csv("datasets_33180_43520_heart.csv")
    names(dados)[names(dados)=="ï..age"] <- "age" 
    # criando uma variável com as faixas etárias
    dados$age_groups <- findInterval(dados$age, c(25,30,35,40,45,50,55,60,65,70,75)) 
    dados$target <- as.factor(dados$target) 
    # transformando os níveis da variável target de 0,1 para Não,Sim
    levels(dados$target) <- plyr::mapvalues(levels(dados$target), from = levels(dados$target), to = c("Não","Sim"))
    
    
    
    
    #
    continuas_label <- c("age"="idade","trestbps"="pressão sanguínea em repouso","chol"="nível de colesterol",
                   "thalach"="frequência cardíaca",
                   "oldpeak"="depressão do segmento ST")
    
    # customizando estilo da tooltip
    label_style <- list(bordercolor = "white",font = list(size = 15,color = "white"))
    
    paletafill <- c("#6FDAE6","#FFA04E")
    paletacolor <- c("#098C9A","#E55400")
    
    
    # criando um dicionário para a tooltip dos gráficos de barra
    categoricas_label <- c("age_groups"="Faixa etária:","sex"="Sexo:",
                           "cp"="Tipo de dor no peito:","fbs"="Glicose acima de 120mg/dl:",
                           "restecg"="Resultado do eletrocardiograma:",
                           "exang"="Angina induzida por exercício:","slope"="Queda no pico ST:",
                           "ca"="Número de vasos coloridos pela fluroscopia:",
                           "thal"="Talassamia")
    
    # dicionário para o título dos gráficos de barras
    categoricas <- c("age_groups"="faixa etária","sex"="sexo","cp"="intensidade da dor no peito",
                     "fbs"="glicose acima de 120mg/dl","restecg"="eletrocardiograma",
                     "exang"="angina induzida por exercício","slope"="queda no pico",
                     "ca"="número de vasos coloridos pela fluroscopia",
                     "thal"="talassamia")
    
    # dicionário para renomear os grupos nos gráficos de barras
    label_niveis <- list("age_groups"=c("25-30","31-35","36-40","41-45","46-50","51-55","56-60",
                                        "61-65","66-70","71-75","76-80"),
                         "sex"=c("Feminino","Masculino"),
                         "cp"=c("Angina típica","Angina atípica","Dor não-anginal","Assintomática"),
                         "fbs"=c("Sim","Não"),"restecg"=c("Normal","Ondas ST-T anormais",
                                                          "Hipertropia ventricular"),
                         "exang"=c("Sim","Não"),"slope"=c("Ascendente","Plana","Descendente"),
                         "ca"=c("0","1","2","3","4"),
                         "thal"=c("0","3 = normal",
                                  "6 = defeito corrigido",
                                  "7 = defeito revertível"))
    
    # gerando gráfico de barras empilhado
    output$barplot_stacked <- renderPlotly({
        
        title <- HTML(paste("Incidência de doença cardíaca por",categoricas[[input$categorica]]))
        gsub('(.{4,10})(\\s|$)', '\\1\n', title) # possibilitando quebras de linha no título
        
        # criando um df com a contagem de cada categoria da variável, por valor da variável target
        dados_contagem <- count(dados,get(input$categorica),target,name="count")
        colnames(dados_contagem) <- c("variavel","target","contagem")
        dados_contagem$variavel <- as.factor(dados_contagem$variavel)
        
        # modificando os nomes dos níveis da variável para que sejam os estabelecidos em label_niveis
        levels(dados_contagem$variavel) <- plyr::mapvalues(levels(dados_contagem$variavel), 
                                                           from = levels(dados_contagem$variavel), 
                                                           to = label_niveis[[input$categorica]])
        
        p2 <- dados_contagem %>%
            ggplot() +
            geom_col(aes(x=variavel,y=contagem,fill=target,
                         # customizando a tooltip
                         text = paste(categoricas_label[[input$categorica]],variavel,
                                      "\nContagem:", contagem,
                                      "\nIncidência de doença:", target))) +
            theme_minimal() +
            scale_fill_manual(values=paletafill) +
            xlab("") + ylab("Contagem") +
            # strwrap possibilita que o título quebre a linha se ultrapassar a largura estabelecida 
            ggtitle(paste(strwrap(title, width = 50), collapse = "\n")) + 
            theme(plot.title = element_text(hjust = 0.5)) +
            guides(fill=guide_legend(title=NULL))
        
        # deixa as labels do eixo x inclinadas dependendo da variável,
        # para que os valores não se sobreponham
        if(input$categorica=="age_groups"){ 
            p2 <- p2 + theme(axis.text.x = element_text(angle = 35))
        } 
        else if(input$categorica=="thal"||input$categorica=="restecg"){
            p2 <- p2 + theme(axis.text.x = element_text(angle = 20))
        }
        
        p2 %>% ggplotly(tooltip = "text") %>%
            style(hoverlabel=label_style)%>%
            layout(legend = list(x = 1, y=0.5, orientation = "v"))
        
    })
    
    
    # gerando gráfico de barras empilhado em 100%
    output$barplot_percent <- renderPlotly({
        
        title <- HTML(paste("Incidência de doença cardíaca por",categoricas[[input$categorica]]))
        gsub('(.{4,10})(\\s|$)', '\\1\n', title) # possibilitando quebras de linha no título
        
        # criando um df com a porcentagem de cada categoria da variável, por valor da variável target
        dados_percent <- dados %>% group_by(get(input$categorica),target) %>%
            summarise(count=n()) %>% mutate(perc=count/sum(count)*100)
        colnames(dados_percent) <- c("variavel","target","contagem","porcentagem")
        dados_percent$variavel <- as.factor(dados_percent$variavel)
        
        #modificando os nomes dos níveis da variável para que sejam os estabelecidos em label_niveis
        levels(dados_percent$variavel) <- plyr::mapvalues(levels(dados_percent$variavel), 
                                                           from = levels(dados_percent$variavel), 
                                                           to = label_niveis[[input$categorica]])
        
        p2 <- dados_percent %>%
            ggplot() +
            geom_col(aes(x=variavel,y=round(porcentagem,2),fill=target,
                         #customizando a tooltip
                         text = paste(categoricas_label[[input$categorica]],variavel,
                                      "\nPorcentagem:", round(porcentagem,2),"%",
                                      "\nIncidência de doença:", target)),
                     position="fill") +
            theme_minimal() +
            scale_fill_manual(values=paletafill) +
            xlab("") + ylab("Porcentagem") + ylim(0,100) + 
            scale_y_continuous(labels = scales::percent_format()) + 
            guides(fill=guide_legend(title=NULL))
        
        if(input$categorica=="age_groups"){ # inclina o nome dos grupos se a variável for age_groups
            p2 <- p2+theme(axis.text.x = element_text(angle = 35))
        } 
        else if(input$categorica=="thal"||input$categorica=="restecg"){
            p2 <- p2 + theme(axis.text.x = element_text(angle = 20))
        }
        
        p2 %>% ggplotly(tooltip = "text") %>%
            style(hoverlabel=label_style) %>%
            layout(legend = list(x = 1, y=0.5, orientation = "v"))
        
    })
    
    
    # gerando boxplot
    output$boxplot <- renderPlotly({
        
        title <- HTML(paste("Incidência de doença cardíaca por",continuas_label[[input$continua]]))
        gsub('(.{4,10})(\\s|$)', '\\1\n', title) # possibilitando quebras de linha no título
        
        p <- ggplot(dados, aes(x=target, y=get(input$continua),fill=target)) + 
            geom_boxplot(color=paletacolor,fill=paletafill) +
            ggtitle(paste(strwrap(title, width = 50), collapse = "\n")) + 
            coord_flip() + theme_minimal()+
            theme(plot.title = element_text(hjust = 0.5)) +
            ylab(HTML(paste(continuas_label[[input$continua]]))) + 
            xlab("Incidência de doença cardíaca") +
            theme(legend.position = "none")
        
        if(input$categorica=="age_groups"){ # definindo número de casas para arredondar na tooltip 
            round <- '.0f'}                 # dependendo da variável escolhida
        else{
            round <- '.1f'}
        
        p %>% ggplotly %>%
            layout(xaxis = list(hoverformat = round))
    })
    
    
    # gerando violin plot
    output$violin <- renderPlotly({
        
        title <- HTML(paste("Incidência de doença cardíaca por",continuas_label[[input$continua]]))
        gsub('(.{4,10})(\\s|$)', '\\1\n', title) # possibilitando quebras de linha no título
        
        p <- ggplot(dados, aes(x=target, y=get(input$continua),color=target,fill=target)) + 
            geom_violin(color="transparent") +
            scale_fill_manual(values = paletafill,) +
            ggtitle(paste(strwrap(title, width = 50), collapse = "\n")) +
            coord_flip() + theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            ylab(HTML(paste(continuas_label[[input$continua]]))) + 
            xlab("Incidência de doença cardíaca") +
            theme(legend.position="none")
        
        p %>% ggplotly(tooltip = "text") %>%
            style(hoverlabel=label_style)%>%
            layout(xaxis = list(hoverformat = ".3f"))
    })
    

    # tabela apresentada na janela Dados
    output$banco <- renderDataTable({
        
        dados <- select(dados,-age_groups) # retirando esta variável pois não fazia parte do banco original
        datatable(dados,selection="multiple", escape=FALSE,options = list(sDom='<"top">lrt<"bottom">ip'))
    
    })
    
    
    # criando uma janela de aviso
    observe({
        
        id <- showNotification(
            tags$div(HTML('
                           <head><meta charset="UTF-8"></head>
                           <p><strong>&#9888; Mensagem importante!</strong></p>
                           <p>O aplicativo apresenta alguns erros de formatação
                           se aberto por um smartphone.</p> <p>Opte por abrir por um 
                           computador para uma melhor visualização!</p>
                          ')),
            duration = 10, 
            closeButton = TRUE,
            type = "warning")
        
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
