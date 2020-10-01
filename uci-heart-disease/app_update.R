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
                            menuItem("Análise e Resultados",tabName = "resultados", icon = icon("clipboard-check")),
                            menuItem("Dados", tabName = "dados", icon=icon("database")),
                            menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/maria-lgr/Teste-Oper/tree/master/uci-heart-disease")
                        )
                    ),
                    dashboardBody(
                        tabItems(
                            tabItem("dashboard", # criando a janela com as visualizações
                            h2("Dashboard"),
                                    fluidRow(
                                        column(6,
                                               fluidRow(
                                                   box(title="Variáveis quantitativas contra presença de doença",width=12,
                                                       fluidRow(
                                                           column(width=8,
                                                           selectInput('continua', "Categorias", 
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
                                                                  tabBox(width = 12,
                                                                      tabPanel('Violin plot',
                                                                               plotlyOutput('violin',height = "300px")),
                                                                      tabPanel('Boxplot', 
                                                                               plotlyOutput('boxplot',height="300px"))
                                                                      )
                                                                  )
                                                           )
                                                       )
                                                   )
                                               ),
                                        column(6,
                                               fluidRow(
                                                   box(title="Variáveis qualitativas contra presença de doença",width=12,height = NULL,
                                                       fluidRow(
                                                   column(8,
                                                          selectInput('categorica', "Categorias", 
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
                                                   column(width=12,
                                                       fluidRow(
                                                           column(12,
                                                                  plotlyOutput('barplot_stacked',height="200px")
                                                           )
                                                       ),
                                                       fluidRow(
                                                           column(12, 
                                                                  plotlyOutput('barplot_percent',height="200px"))
                                                       )
                                                   )
                                                   
                                               )
                                                   )
                                               )
                                        )
                                    )
                                    ),
                            tabItem("resultados",
                                    h2("Análise e resultados"),
                                    fluidRow(
                                        tabBox(width = 12,
                                               tabPanel("Metodologia e ajuste do modelo",
                                                        fluidRow(
                                                        column(6,
                                                               br(),
                                                               
                                                               p("A variável de interesse deste estudo (se o indivíduo apresenta doença cardíaca ou não) é uma
                                                         variável binária, por assumir valores 0 ou 1, que representam não e sim. Para descobrir como e quais outras
                                                         características registradas para cada indivíduo influenciam nesta resposta, foi utilizado um ",
                                                                      strong("Modelo Linear Generalizado Binomial")," com função de ligação logit; ou seja, foi realizada uma
                                                         Regressão Logística."),
                                                                    
                                                         br(),
                                                                    
                                                         p("Um modelo inicial foi definido se utilizando da técnica Stepwise. A partir dele, foram observadas as significâncias de cada regressor e as deviances para vários modelos, até se atingir um modelo mais adequado. Na análise de resíduos, foram
                                                         identificados 27 pontos influentes pela Distância de Cook. Todos os passos foram, então, refeitos para o banco
                                                         de dados sem tais pontos influentes. O ajuste do novo modelo obtido se apresentou melhor que o do modelo anterior,
                                                         com uma deviance menor e todos os coeficientes dos regressores significativos. Nas tabelas ao lado são apresentados
                                                         os as estimativas e os valores-p obtidos para cada regressor, a deviance do modelo, sua estatística generalizada de Pearson e seu respectivo
                                                         valor-p e o AIC do modelo."),
                                                                    
                                                                    style="text-align:justify;color:#404040;font-size:15px"),
                                                        column(6,
                                                               tableOutput('valoresp'),
                                                               tableOutput('deviance'))
                                                        )
                                                   ),
                                                       
                                                       
                                                   tabPanel("Resultados",
                                                            fluidRow(
                                                            column(6,
                                                                   br(),
                                                                   
                                                                   p("O modelo ajustado levou em consideração as variáveis explicativas apresentadas
                                                                     na tabela ao lado, juntamente com suas respectivas estimativas (exponenciais), o intervalo
                                                                     de confiança de 95% para tais estimativas e o impacto da variável na odds de um indivíduo."),
                                                                   
                                                                   br(),
                                                                   
                                                                   tags$li(strong("cp:"), "apesar dos coeficientes de cada nível da variável cp terem
                                                                           sido considerados significativos, os intervalos de confiança para suas estimativas,
                                                                           além de serem muito extensos, contém o valor 1, o que implica que o impacto dos níveis
                                                                           dessa variávels obre a odds pode não ser significativo;"),
                                                                   tags$li(strong("oldpeak:"), "o acréscimo de uma unidade no valor assumido pela variável oldpeak para um
                                                                           dado indivíduo acarreta em uma diminuição de 79.2% da sua odds: o indivíduo é 
                                                                           menos propenso a apresentar doença cardíaca;"),
                                                                   tags$li(strong("sex:"), "quando um indivíduo passa do sexo Feminino para o Masculino, há uma diminuição de
                                                                           92.93% na sua chance de apresentar doença cardíaca;"),
                                                                   tags$li(strong("trestbps:"), "o acréscimo de uma unidade no valor assumido por essa variável para um dado
                                                                           indivíduo acarreta em uma diminuição de 2.72% da sua chance de desenvolver doença cardíaca;"),
                                                                   tags$li(strong("exang:"), "o acréscimo de uma unidade no valor assumido por essa variável para um dado
                                                                           indivíduo acarreta em uma diminuição de 78.32% da sua chance de desenvolver doença cardíaca;"),
                                                                   
                                                                   style="text-align:justify;color:#404040;font-size:15px"),
                                                            column(6,
                                                                   tableOutput('odds'))
                                                            )
                                                   )
                                               )
                                    )
                                    
                                    ),
                            tabItem("dados", # criando janela com os dados
                                    h2("Dados"),
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
	                                    <li><b>cp:</b> Dor no peito (angina típica, angina atípica, dor não-anginal, assintomático)</li>
                                	    <li><b>tresbps:</b> Pressão sanguínea em repouso</li>
                                	    <li><b>chol:</b> Colesterol no sangue (em mg/dl)</li>
                                	    <li><b>fbs:</b> Glicose em jejum acima de 120mg/dl</li>
                                	    <li><b>restecg:</b> Resultado do eletrocardiograma em repouso (normal, ondas ST-T anormais,
                                	    mostra hipertropia ventricular esquerda provável ou definitiva pelo critério de Estes)</li>
                                	    <li><b>thalach:</b> Frequência cardíaca máxima atingida</li>
                                	    <li><b>exang:</b> Angina induzida por exercício</li>
                                	    <li><b>oldpeak:</b> Depressão do segmento ST induzida</li>
                                	    <li><b>slope:</b> Inclinação do pico do segmento ST (ascendente, plana, descendente)</li>
                                	    <li><b>ca:</b> Número de vasos coloridos pela fluoroscopia</li>
	                                    <li><b>thal:</b> 3 = normal, 6 = defeito corrigido, 7 = defeito reversível</li>
	                                    <li><b>target:</b> Diagnóstico de doença (resultado do angiograma)</li>
                                    </ol> 
                                    <div style="margin-bottom:30px;">
                                    </div>
                                    <small>Fonte: <a href="url">ttps://archive.ics.uci.edu/ml/datasets/Heart+Disease</a></small>
                                    </body> ')))),
                                                           tabPanel('Banco de dados', 
                                                                    column(12,
                                                                           box(width=12,height = NULL,
                                                                               DT::dataTableOutput('banco')
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
    
    # tabela de valores-p e deviance
    output$valoresp <- renderTable(cbind(
                                         "int"=c(" ","Dor no peito (angina atípica)","Dor no peito (não-anginal)",
                                                 "Dor no peito (assintomático)","Depressão do segmento ST induzida",
                                                 "Sexo (masculino)","Pressão sanguínea em repouso","Angina induzida por exercício (sim)"),
                                         "cov"=c("Intercepto","cp nível 2","cp nível 3","cp nível 4","oldpeak","sex (masculino)","trestbps","exang"),
                                         read_csv("coef.csv")) %>% 
                                       rename("Covariáveis"=int,"Coeficiente"=cov,"Estimado"=estimate_coef,"Valor-p"=valorp_coef))
    
    output$deviance <- renderTable(read_csv("deviance.csv") %>%
                                       rename("Deviance"=deviance,
                                              "Estatística \ngeneralizada \nde Pearson"=est_pearson,
                                              "Valor-p da estatística"=est_pearson_valorp,
                                              "AIC"=aic))
    
    output$odds <- renderTable(cbind(
        "int"=c("Dor no peito (angina atípica)","Dor no peito (não-anginal)",
                "Dor no peito (assintomático)","Depressão do segmento ST induzida",
                "Sexo (masculino)","Pressão sanguínea em repouso","Angina induzida por exercício (sim)"),
        "cov"=c("cp nível 2","cp nível 3","cp nível 4","oldpeak","sex (masculino)","trestbps","exang"),
        read_csv("odds.csv")) %>% 
            rename("Covariáveis"=int,"Coeficiente"=cov,"Estimado (exponencial)"=Estimate,
                   "Limite inferior do IC"=X2.5..,
                   "Limite superior do IC"=X97.5..,
                   "Impacto"=impacto))
    
    
    
    
    # tabela apresentada na janela Dados
    output$banco <- renderDataTable({
        
        dados <- read.csv("datasets_33180_43520_heart.csv")
        names(dados)[names(dados)=="ï..age"] <- "age" 
        datatable(dados,selection="multiple", escape=FALSE,options = list(sDom='<"top">lrt<"bottom">ip'))
        
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
