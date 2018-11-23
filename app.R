list.of.packages <- c("shinytheme", "shiny", "igraph", "tm", "tokenizers","stringr","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinythemes)

ui_prova<-fluidPage(
  theme = shinytheme("superhero"),
  title = "SemanticApp",
  titlePanel(h1("Semantic Network of Concepts' Co-occurences per sentence with R",h6("by",em("Iacopo Ghinassi")))),
  
  sidebarLayout(
    sidebarPanel(h2("Input"),width=4,
                 fluidRow(
                   fileInput("file1",h3("File input"))),
                 fluidRow(
                   textInput("dictionary1",label = "Insert 1st dictionary's words",
                             value = "write something"),
                   textInput("dictionary2",label = "Insert 2nd dictionary's words",
                             value = "write something"),
                   textInput("dictionary3",label = "Insert 3rd dictionary's words",
                             value = "write something"),
                   textInput("dictionary4",label = "Insert 4th dictionary's words",
                             value = "write something"),
                   textInput("dictionary5",label = "Insert 5th dictionary's words",
                             value = "write something"),
                   textInput("dictionary6",label = "Insert 6th dictionary's words",
                             value = "write something"),
                   textInput("dictionary7",label = "Insert 7th dictionary's words",
                             value = "write something"),
                   textInput("dictionary8",label = "Insert 8th dictionary's words",
                             value = "write something"),
                   textInput("dictionary9",label = "Insert 9th dictionary's words",
                             value = "write something"),
                   textInput("dictionary10",label = "Insert 10th dictionary's words",
                             value = "write something"),
    submitButton("Submit"))),
    mainPanel(column(12,h2("Semantic Network"),align="center"),
      column(12,plotOutput(outputId = "graphnet"),align="center",style="background-color:#ccccff"),
      column(1,br()),
    column(12,img(src="FOCA-1-300x192.jpg"),align="center"),
    column(12, selectInput("layout","Layouts:",c(layout.mds="layout.mds",layout.graphopt="layout.graphopt",layout.gem="layout.gem",
                                                layout.circle="layout.circle"))),
    column(12,selectInput("color","Network color:",c(Red="red",Blue="blue",Green="green",Yellow="yellow", Black="black"))),
             column(12, selectInput("language","Language:",c(english="english", 
                                                    italian="italian",
                                                    spanish="spanish",
                                                    german="german"))))))
    
    server_prova<-function(input,output,session){
      output$graphnet <- renderPlot({
        library(tokenizers)
        library(tm)
        library(stringr)
        library(readr)
        library(igraph)
        if(is.null(input$file1)){
          return(title("No text"))
        }
        else{
        text<-readLines(input$file1$datapath)
        text<-paste(text,collapse = " ")
        preproc_text<-gsub("((?:\b| )?([.,:;!?]+)(?: |\b)?)", " \\1 ", text, perl=T)
        preproc_text<-stemDocument(preproc_text)
        preproc_text<-paste(preproc_text, collapse = " ")
        dict_list<-list()
        dict_list[[1]]<-vector()
        dict_list[[1]]<-input$dictionary1
        dict_list[[1]]<-strsplit(dict_list[[1]], ",")
        dict_list[[1]]<-unlist(dict_list[[1]])
        dict_list[[1]]<-str_trim(dict_list[[1]])
        dict_list[[2]]<-vector()
        dict_list[[2]]<-input$dictionary2
        dict_list[[2]]<-strsplit(dict_list[[2]], ",")
        dict_list[[2]]<-unlist(dict_list[[2]])
        dict_list[[2]]<-str_trim(dict_list[[2]])
        dict_list[[3]]<-vector()
        dict_list[[3]]<-input$dictionary3
        dict_list[[3]]<-strsplit(dict_list[[3]], ",")
        dict_list[[3]]<-unlist(dict_list[[3]])
        dict_list[[3]]<-str_trim(dict_list[[3]])
        dict_list[[4]]<-vector()
        dict_list[[4]]<-input$dictionary4
        dict_list[[4]]<-strsplit(dict_list[[4]], ",")
        dict_list[[4]]<-unlist(dict_list[[4]])
        dict_list[[4]]<-str_trim(dict_list[[4]])
        dict_list[[5]]<-vector()
        dict_list[[5]]<-input$dictionary5
        dict_list[[5]]<-strsplit(dict_list[[5]], ",")
        dict_list[[5]]<-unlist(dict_list[[5]])
        dict_list[[5]]<-str_trim(dict_list[[5]])
        dict_list[[6]]<-vector()
        dict_list[[6]]<-input$dictionary6
        dict_list[[6]]<-strsplit(dict_list[[6]], ",")
        dict_list[[6]]<-unlist(dict_list[[6]])
        dict_list[[6]]<-str_trim(dict_list[[6]])
        dict_list[[7]]<-vector()
        dict_list[[7]]<-input$dictionary7
        dict_list[[7]]<-strsplit(dict_list[[7]], ",")
        dict_list[[7]]<-unlist(dict_list[[7]])
        dict_list[[7]]<-str_trim(dict_list[[7]])
        dict_list[[8]]<-vector()
        dict_list[[8]]<-input$dictionary8
        dict_list[[8]]<-strsplit(dict_list[[8]], ",")
        dict_list[[8]]<-unlist(dict_list[[8]])
        dict_list[[8]]<-str_trim(dict_list[[8]])
        dict_list[[9]]<-vector()
        dict_list[[9]]<-input$dictionary9
        dict_list[[9]]<-strsplit(dict_list[[9]], ",")
        dict_list[[9]]<-unlist(dict_list[[9]])
        dict_list[[9]]<-str_trim(dict_list[[9]])
        dict_list[[10]]<-vector()
        dict_list[[10]]<-input$dictionary10
        dict_list[[10]]<-strsplit(dict_list[[10]], ",")
        dict_list[[10]]<-unlist(dict_list[[10]])
        dict_list[[10]]<-str_trim(dict_list[[10]])
        for (i in 1:10) {
          if(length(dict_list[[i]])==0){
            dict_list[[i]][1]<-"write something"
          }
        }
        if (dict_list[[1]][1]=="write something") {
          title(warning("WRITE AT LEAST THREE DICTIONARIES!"))
        }
        else{
        for (i in length(dict_list):1) {
          if (dict_list[[i]][1]=="write something") {
            dict_list[[i]]<-NULL
          }
        }
        for (i in 1:length(dict_list)) {
          dict_list[[i]]<-append(dict_list[[i]],stemDocument(dict_list[[i]],language = "english"))# Select the language that apply
        }
        
        All_dict_words_to1term <- function(starting_doc,dict,sub_term){
          for (i in 1:length(dict)) {
            if (i==1){
              new_doc<-str_replace_all(starting_doc,dict[i],sub_term)
            }
            else{
              new_doc<-str_replace_all(new_doc,dict[i],sub_term)
            }
          }
          return(new_doc)
        }
        
        # Function to iterate the previous function over several dictionaries and create a list of the texts thus processed (the final element of the list is the one processed after all of the dictionaries)
        All_dict_words_to1term_fin <-  function(starting_doc,dictionaries){
          result<-list()
          for (i in 1:length(dictionaries)) {
            if (i==1) {
              result[[i]]<-All_dict_words_to1term(starting_doc,dictionaries[[i]],dictionaries[[i]][1])
            }
            else{
              result[[i]]<-All_dict_words_to1term(result[[i-1]],dictionaries[[i]],dictionaries[[i]][1])
            }
          }
          return(result)
        }
        processed_text<-All_dict_words_to1term_fin(preproc_text,dict_list)
        processed_text<-processed_text[[length(dict_list)]]
        
        clean_token<-function(x){
          library(tokenizers)
          x<-iconv(x, "latin1",'UTF-8', sub = "") #change the parameters of conversion as needed, esepcially if errors are thrown at this stage
          x<-tokenize_sentences(x)
          return(x)
        }
        text_sentence<-clean_token(processed_text)
        
        Corpusv<-VCorpus(VectorSource(unlist(text_sentence)))
        Corpusv <- tm_map(Corpusv, content_transformer(tolower))
        Corpusv <- tm_map(Corpusv, removePunctuation)
        Corpusv <- tm_map(Corpusv, removeNumbers)
        if (input$language=="english") {
        Corpusv <- tm_map(Corpusv, removeWords, stopwords("english"))#select the language of the texts
        }
        if (input$language=="italian") {
          Corpusv <- tm_map(Corpusv, removeWords, stopwords("it"))#select the language of the texts
        }
        if (input$language=="spanish") {
          Corpusv <- tm_map(Corpusv, removeWords, stopwords("sp"))#select the language of the texts
        }
        if (input$language=="german") {
          Corpusv <- tm_map(Corpusv, removeWords, stopwords("ger"))#select the language of the texts
        }
        Corpusv <- tm_map(Corpusv, stripWhitespace)
        TDM<-TermDocumentMatrix(Corpusv)
        dict_new<-vector()
        for (i in 1:length(dict_list)) {
          dict_new[i] <- dict_list[[i]][1]
        }
        dict_new<-tolower(dict_new)
        print(dict_new)
        prova<-list()
        for (i in 1:length(dict_new)) {
          prova[[i]]<-findAssocs(TDM, dict_new[i],corlimit = .001)
        }
        prova<-unlist(prova)
        provone<-list()
        for (i in 1:length(dict_new)) {
          provone[[i]]<-prova[paste(dict_new[i],".",dict_new,sep = "")]
          provone[[i]]<-`names<-`(provone[[i]],dict_new)
        }
        delete_na<-function(lists){
          for (i in 1:length(lists)) {
            lists[[i]]<-na.omit(lists[[i]])
          }
          return(lists)
        }
        Delete_dupl_in_corrlist<-function(corr_list,dict){
          corr_list1<-list()
          for (i in 1:(length(dict)-1)) {
            corr_list1[[i]]<-corr_list[[i]][dict[i+1:length(dict)]]
          }
          for (i in 1:(length(dict)-1)) {
            corr_list1[[i]]<-corr_list1[[i]][1:(length(dict)-i)]
          }
          return(corr_list1)
        }
        provone<-try(Delete_dupl_in_corrlist(provone,dict_new))
        if(class(provone)=="try-error"){
         title(warning("Include at least three dictionaries"))
        }
        else{
        provone<-delete_na(provone)
        
        node_reference<- data.frame(dict_new, c(1:length(dict_new)))
        node_reference<-node_reference[,2]
        node_reference<-`names<-`(node_reference,dict_new)
        
        create_edge_table<-function(weight_list,node_vector){
          finalist<-list()
          for (i in 1:length(weight_list)) {
            finalist[[i]]<-data.frame(rep(names(node_vector[i]),times=length(weight_list[[i]])),rep(node_vector[names(node_vector[i])],times=length(weight_list[[i]])),node_vector[names(weight_list[[i]])],weight_list[[i]])
          }
          for (i in 1:length(finalist)) {
            finalist[[i]]<-`names<-`(finalist[[i]],c('Label','Source','Target','Weight'))
          }
          for (i in 1:(length(finalist))) {
            if (i>1&i<3) {
              df<-merge(finalist[[i]],finalist[[i-1]],all.x = TRUE,all.y = TRUE)
            }
            if (i>2) {
              df<-merge(finalist[[i]],df,all.x = TRUE,all.y = TRUE)
            }
          }
          return(df)
        }
        
        edge_df<-create_edge_table(provone, node_reference)
        if(class(edge_df)=="function"){
          title(warning("Include at least three dictionaries or,","\n",
                        "if three dictionaries were included,","\n", 
                        "no correlation was found between the dictionaries"))
        }
        else{
        termFreq2<-function(x){
          termFreq(x, 
                   control = list(content_transformer(tolower),
                                  removePunctuation,removeNumbers,
                                  removeWords, stopwords("en"),stripWhitespace))# Change the                                                           language of stopwords if needed
        }
        
        processed_text<-iconv(processed_text, "UTF-8",'latin1', sub = "") #change the parameters of conversion as needed, esepcially if errors are thrown at this stage
        Text_freq<-termFreq2(processed_text)
        Text_freq<-Text_freq[dict_new]
        
        nodes_df<-data.frame(dict_new,node_reference,Text_freq)
        
        edges_vector<-vector()
        for (j in 1:nrow(edge_df)) {
          edges_vector<-append(edges_vector,as.integer(edge_df[j,2:3]))
        }
          if(length(na.omit(edges_vector))==0){
          title('No Co-Occurrences Found!')
        }
          else{
        graphnet<-graph(edges_vector,directed = FALSE) 
        for (j in 1:nrow(edge_df)) {
          graphnet<-set.edge.attribute(graphnet,'Weight',
                                       index = j,value = edge_df[j,"Weight"])
        }
        
        for (j in 1:length(dict_new)) {
          graphnet<-set.vertex.attribute(graphnet,'name',
                                         index = j,value = dict_new[j])
        }
        
        V(graphnet)$color<-input$color
        
        for (j in 1:length(dict_new)) {
          graphnet<-set.vertex.attribute(graphnet,'Occurrence',
                                         index = j,value = nodes_df[j,3])
        }
        print(V(graphnet)$Occurrence)
        
        if(length(na.omit(V(graphnet)$Occurrence))==0){
          plot.igraph(graphnet, 
                      edge.width=E(graphnet)$Weight*40)
          title('Words Frequency is Missing!')
        }
        else{
        print(E(graphnet)$Weight)
          for (i in 1:length(V(graphnet)$Occurrence)) {
            if(is.na(V(graphnet)$Occurrence[i])==TRUE){
              V(graphnet)$Occurrence[i]<-1
            }
          }
        if(input$layout=="layout.mds"){
        plot.igraph(graphnet, vertex.size=V(graphnet)$Occurrence/40, 
                    edge.width=E(graphnet)$Weight*40,layout=layout.mds)}
        if(input$layout=="layout.graphopt"){
          plot.igraph(graphnet, vertex.size=V(graphnet)$Occurrence/40, 
                      edge.width=E(graphnet)$Weight*40,layout=layout.graphopt)}
        if(input$layout=="layout.gem"){
          plot.igraph(graphnet, vertex.size=V(graphnet)$Occurrence/40, 
                      edge.width=E(graphnet)$Weight*40,layout=layout.gem)}
        if(input$layout=="layout.circle"){
          plot.igraph(graphnet, vertex.size=V(graphnet)$Occurrence/40, 
                      edge.width=E(graphnet)$Weight*40,layout=layout.circle)}}}}}}}
      })
    }

    shinyApp(ui=ui_prova,server = server_prova)
