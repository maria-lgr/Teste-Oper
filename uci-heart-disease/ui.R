library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "Doen�as card�acas"),
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
                        tabItem("dashboard", # criando a janela com as visualiza��es
                                fluidRow(
                                  column(6,
                                         fluidRow(
                                           column(12, 
                                                  selectInput('continua', "Vari�vel dos boxplots", 
                                                              choices = c("Idade"="age",
                                                                          "Press�o sangu�nea em repouso"="trestbps",
                                                                          "N�vel de colesterol"="chol",
                                                                          "M�xima frequ�ncia card�aca atingida"="thalach",
                                                                          "Depress�o do segmento ST"="oldpeak")
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
                                                    # breve resumo das conclus�es da an�lise
                                                    p(strong("Quais vari�veis influenciam na presen�a de doen�a card�aca?")),
                                                    p("As vari�veis idade, m�xima frequ�ncia card�aca atingida e 
                                                              depress�o do segmento ST apresentam distribui��es diferentes se a doen�a for presente ou n�o.
                                                              J� as vari�veis sexo, tipo de dor no peito, angina induzido por exerc�cio, 
                                                              inclina��o do pico do segmento ST, n�mero de vasos coloridos
                                                              pela fluroscopia e talassamia apresentam propor��es discrepantes entre os dois grupos para alguns
                                                              de seus n�veis. Por isso, podemos dizer que essas vari�veis tem influ�ncia sobre 
                                                              a incid�ncia de doen�a card�aca."),
                                                    style="text-align:justify;color:#404040;background-color:#FFFFFF;
                                                               padding:15px;margin:10px;border-radius:7px"
                                                  )
                                           )
                                         )
                                  ),
                                  column(6,
                                         fluidRow(
                                           column(12,
                                                  selectInput('categorica', "Vari�vel dos gr�ficos de barras", 
                                                              choices = c("Faixa et�ria"="age_groups","Sexo"="sex",
                                                                          "Dor no peito"="cp",
                                                                          "Glicose acima de 120mg/dl"="fbs",
                                                                          "Resultado do eletrocardiograma"="restecg",
                                                                          "Angina induzida por exerc�cio"="exang",
                                                                          "Inclina��o do pico do segmento ST"="slope",
                                                                          "N�mero de vasos coloridos pela fluroscopia"="ca",
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
                                                     tabPanel('Descri��o', box(width=NULL,
                                                                               tags$div(HTML('
                                    <body style="background-color:#FFFFFF;text-align:justify;color:#404040;">
                                    <div style="margin-bottom:20px;">
                                    </div>
                                    <p>O banco de dados <b>Heart Disease</b> originalmente 
                                    continha 76 vari�veis, mas apenas 14 delas eram realmente usadas em experimentos 
                                    publicados.</p> <p>Estas 14 vari�veis s�o:</p> 
                                    <ol>
                                	    <li><b>age:</b> Idade em anos inteiros</li>
                                	    <li><b>sex:</b> Sexo feminino ou masculino</li>
	                                    <li><b>cp:</b> Dor no peito (angina t�pica, angina at�pica,dor n�o-anginal,assintom�tico)</li>
                                	    <li><b>tresbps:</b> Press�o sangu�nea em repouso</li>
                                	    <li><b>chol:</b> Colesterol no sangue (em mg/dl)</li>
                                	    <li><b>fbs:</b> Glicose em jejum acima de 120mg/dl</li>
                                	    <li><b>restecg:</b> Resultado do eletrocardiograma em repouso(normal,ondas ST-T anormais,
                                	    mostra hipertropia ventricular esquerda prov�vel ou definitiva pelo crit�rio de Estes)</li>
                                	    <li><b>thalach:</b> Frequ�ncia card�aca m�xima atingida</li>
                                	    <li><b>exang:</b> Angina induzida por exerc�cio</li>
                                	    <li><b>oldpeak:</b> Depress�o do segmento ST induzida</li>
                                	    <li><b>slope:</b> Inclina��o do pico do segmento ST (ascendente,plana,descendente</li>
                                	    <li><b>ca:</b> N�mero de vasos coloridos pela fluoroscopia</li>
	                                    <li><b>thal:</b> 3 = normal, 6 = defeito corrigido, 7= defeito revers�vel</li>
	                                    <li><b>target:</b> Diagn�stico de doen�a (resultado do angiograma)</li>
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
