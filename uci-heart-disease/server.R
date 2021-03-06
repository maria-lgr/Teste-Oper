server <- function(input, output) {
    
    # abrindo o banco
    dados <- read.csv("datasets_33180_43520_heart.csv")
    names(dados)[names(dados)=="�..age"] <- "age" 
    # criando uma vari�vel com as faixas et�rias
    dados$age_groups <- findInterval(dados$age, c(25,30,35,40,45,50,55,60,65,70,75)) 
    dados$target <- as.factor(dados$target) 
    # transformando os n�veis da vari�vel target de 0,1 para N�o,Sim
    levels(dados$target) <- plyr::mapvalues(levels(dados$target), from = levels(dados$target), to = c("N�o","Sim"))
    
    
    
    
    #
    continuas_label <- c("age"="idade","trestbps"="press�o sangu�nea em repouso","chol"="n�vel de colesterol",
                         "thalach"="frequ�ncia card�aca",
                         "oldpeak"="depress�o do segmento ST")
    
    # customizando estilo da tooltip
    label_style <- list(bordercolor = "white",font = list(size = 15,color = "white"))
    
    paletafill <- c("#6FDAE6","#FFA04E")
    paletacolor <- c("#098C9A","#E55400")
    
    
    # criando um dicion�rio para a tooltip dos gr�ficos de barra
    categoricas_label <- c("age_groups"="Faixa et�ria:","sex"="Sexo:",
                           "cp"="Tipo de dor no peito:","fbs"="Glicose acima de 120mg/dl:",
                           "restecg"="Resultado do eletrocardiograma:",
                           "exang"="Angina induzida por exerc�cio:","slope"="Queda no pico ST:",
                           "ca"="N�mero de vasos coloridos pela fluroscopia:",
                           "thal"="Talassamia")
    
    # dicion�rio para o t�tulo dos gr�ficos de barras
    categoricas <- c("age_groups"="faixa et�ria","sex"="sexo","cp"="intensidade da dor no peito",
                     "fbs"="glicose acima de 120mg/dl","restecg"="eletrocardiograma",
                     "exang"="angina induzida por exerc�cio","slope"="queda no pico",
                     "ca"="n�mero de vasos coloridos pela fluroscopia",
                     "thal"="talassamia")
    
    # dicion�rio para renomear os grupos nos gr�ficos de barras
    label_niveis <- list("age_groups"=c("25-30","31-35","36-40","41-45","46-50","51-55","56-60",
                                        "61-65","66-70","71-75","76-80"),
                         "sex"=c("Feminino","Masculino"),
                         "cp"=c("Angina t�pica","Angina at�pica","Dor n�o-anginal","Assintom�tica"),
                         "fbs"=c("Sim","N�o"),"restecg"=c("Normal","Ondas ST-T anormais",
                                                          "Hipertropia ventricular"),
                         "exang"=c("Sim","N�o"),"slope"=c("Ascendente","Plana","Descendente"),
                         "ca"=c("0","1","2","3","4"),
                         "thal"=c("0","3 = normal",
                                  "6 = defeito corrigido",
                                  "7 = defeito revert�vel"))
    
    # gerando gr�fico de barras empilhado
    output$barplot_stacked <- renderPlotly({
        
        title <- HTML(paste("Incid�ncia de doen�a card�aca por",categoricas[[input$categorica]]))
        gsub('(.{4,10})(\\s|$)', '\\1\n', title) # possibilitando quebras de linha no t�tulo
        
        # criando um df com a contagem de cada categoria da vari�vel, por valor da vari�vel target
        dados_contagem <- count(dados,get(input$categorica),target,name="count")
        colnames(dados_contagem) <- c("variavel","target","contagem")
        dados_contagem$variavel <- as.factor(dados_contagem$variavel)
        
        # modificando os nomes dos n�veis da vari�vel para que sejam os estabelecidos em label_niveis
        levels(dados_contagem$variavel) <- plyr::mapvalues(levels(dados_contagem$variavel), 
                                                           from = levels(dados_contagem$variavel), 
                                                           to = label_niveis[[input$categorica]])
        
        p2 <- dados_contagem %>%
            ggplot() +
            geom_col(aes(x=variavel,y=contagem,fill=target,
                         # customizando a tooltip
                         text = paste(categoricas_label[[input$categorica]],variavel,
                                      "\nContagem:", contagem,
                                      "\nIncid�ncia de doen�a:", target))) +
            theme_minimal() +
            scale_fill_manual(values=paletafill) +
            xlab("") + ylab("Contagem") +
            # strwrap possibilita que o t�tulo quebre a linha se ultrapassar a largura estabelecida 
            ggtitle(paste(strwrap(title, width = 50), collapse = "\n")) + 
            theme(plot.title = element_text(hjust = 0.5)) +
            guides(fill=guide_legend(title=NULL))
        
        # deixa as labels do eixo x inclinadas dependendo da vari�vel,
        # para que os valores n�o se sobreponham
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
    
    
    # gerando gr�fico de barras empilhado em 100%
    output$barplot_percent <- renderPlotly({
        
        title <- HTML(paste("Incid�ncia de doen�a card�aca por",categoricas[[input$categorica]]))
        gsub('(.{4,10})(\\s|$)', '\\1\n', title) # possibilitando quebras de linha no t�tulo
        
        # criando um df com a porcentagem de cada categoria da vari�vel, por valor da vari�vel target
        dados_percent <- dados %>% group_by(get(input$categorica),target) %>%
            summarise(count=n()) %>% mutate(perc=count/sum(count)*100)
        colnames(dados_percent) <- c("variavel","target","contagem","porcentagem")
        dados_percent$variavel <- as.factor(dados_percent$variavel)
        
        #modificando os nomes dos n�veis da vari�vel para que sejam os estabelecidos em label_niveis
        levels(dados_percent$variavel) <- plyr::mapvalues(levels(dados_percent$variavel), 
                                                          from = levels(dados_percent$variavel), 
                                                          to = label_niveis[[input$categorica]])
        
        p2 <- dados_percent %>%
            ggplot() +
            geom_col(aes(x=variavel,y=round(porcentagem,2),fill=target,
                         #customizando a tooltip
                         text = paste(categoricas_label[[input$categorica]],variavel,
                                      "\nPorcentagem:", round(porcentagem,2),"%",
                                      "\nIncid�ncia de doen�a:", target)),
                     position="fill") +
            theme_minimal() +
            scale_fill_manual(values=paletafill) +
            xlab("") + ylab("Porcentagem") + ylim(0,100) + 
            scale_y_continuous(labels = scales::percent_format()) + 
            guides(fill=guide_legend(title=NULL))
        
        if(input$categorica=="age_groups"){ # inclina o nome dos grupos se a vari�vel for age_groups
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
        
        title <- HTML(paste("Incid�ncia de doen�a card�aca por",continuas_label[[input$continua]]))
        gsub('(.{4,10})(\\s|$)', '\\1\n', title) # possibilitando quebras de linha no t�tulo
        
        p <- ggplot(dados, aes(x=target, y=get(input$continua),fill=target)) + 
            geom_boxplot(color=paletacolor,fill=paletafill) +
            ggtitle(paste(strwrap(title, width = 50), collapse = "\n")) + 
            coord_flip() + theme_minimal()+
            theme(plot.title = element_text(hjust = 0.5)) +
            ylab(HTML(paste(continuas_label[[input$continua]]))) + 
            xlab("Incid�ncia de doen�a card�aca") +
            theme(legend.position = "none")
        
        if(input$categorica=="age_groups"){ # definindo n�mero de casas para arredondar na tooltip 
            round <- '.0f'}                 # dependendo da vari�vel escolhida
        else{
            round <- '.1f'}
        
        p %>% ggplotly %>%
            layout(xaxis = list(hoverformat = round))
    })
    
    
    # gerando violin plot
    output$violin <- renderPlotly({
        
        title <- HTML(paste("Incid�ncia de doen�a card�aca por",continuas_label[[input$continua]]))
        gsub('(.{4,10})(\\s|$)', '\\1\n', title) # possibilitando quebras de linha no t�tulo
        
        p <- ggplot(dados, aes(x=target, y=get(input$continua),color=target,fill=target)) + 
            geom_violin(color="transparent") +
            scale_fill_manual(values = paletafill,) +
            ggtitle(paste(strwrap(title, width = 50), collapse = "\n")) +
            coord_flip() + theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            ylab(HTML(paste(continuas_label[[input$continua]]))) + 
            xlab("Incid�ncia de doen�a card�aca") +
            theme(legend.position="none")
        
        p %>% ggplotly(tooltip = "text") %>%
            style(hoverlabel=label_style)%>%
            layout(xaxis = list(hoverformat = ".3f"))
    })
    
    
    # tabela apresentada na janela Dados
    output$banco <- renderDataTable({
        
        dados <- select(dados,-age_groups) # retirando esta vari�vel pois n�o fazia parte do banco original
        datatable(dados,selection="multiple", escape=FALSE,options = list(sDom='<"top">lrt<"bottom">ip'))
        
    })
    
    
    # criando uma janela de aviso
    observe({
        
        id <- showNotification(
            tags$div(HTML('
                           <head><meta charset="UTF-8"></head>
                           <p><strong>&#9888; Mensagem importante!</strong></p>
                           <p>O aplicativo apresenta alguns erros de formata��o
                           se aberto por um smartphone.</p> <p>Opte por abrir por um 
                           computador para uma melhor visualiza��o!</p>
                          ')),
            duration = 10, 
            closeButton = TRUE,
            type = "warning")
        
    })
    
}