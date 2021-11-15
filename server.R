server <- function(input, output, session) {

  observeEvent(input$datafile, {
    if (stringr::str_detect(input$datafile$datapath,".xlsx")){
      output$colname3 = renderUI({ 
        selectInput("sheet", "Sheet:",getSheetNames(input$datafile$datapath))
      })
      data_file = reactive(read.xlsx(input$datafile$datapath,sheet = input$sheet))

      
      
    }else {
      data_file =reactive(read.csv(input$datafile$datapath,fileEncoding="utf-8"))

    }
    
    output$table = renderDataTable(data_file(), editable = 'cell',filter = "top",
                                   options = list(pagelength = 100))
    
    output$colname1 = renderUI({ 
      selectInput("x", "x axis:", colnames(data_file()))
    })
    output$colname2 = renderUI({ 
      selectInput("y", "y axis:", colnames(data_file()))
    })
    output$soubetuka = renderUI({ 
      selectInput("group", "Group:",colnames(data_file()))
    })

  })
  
  
    #### keypressed .js ####
  txt <- reactiveVal()
  observeEvent(input[["keyPressed"]], {
    txt(input[["col_text"]])
  })
  txt2 <- reactiveVal()
  observeEvent(input[["keyPressed"]], {
    txt2(input[["col_enter"]])
  })

    #### ggplot ####
  observeEvent(input$submit, {
    name = names(input$datafile)
    if (str_detect(input$datafile$datapath,".xlsx"))
      data_file = reactive(read.xlsx(input$datafile$datapath, sheet = input$sheet))
    else{
      data_file = reactive(read.csv(input$datafile$datapath))
    }


    x = data_file()[input$x]
    y = data_file()[input$y]
    grp = data_file()[input$group]
    
    
    #### keeping data order ####
    
    if ((is.numeric(x[,1]) == T) && (is.character(grp[,1]) == T)){
        grp[,1] <- factor(grp[,1], as.character(unique(grp[,1])))
        x2 = x[,1]
        num <- length(table(grp[,1]))

    }else if (is.character(x[,1]) == T){
      if (is.character(grp[,1]) == T)
        x2 = factor(x[,1],as.character(unique(x[,1])))
        grp[,1] <- factor(grp[,1],as.character(unique(grp[,1])))
        num <- length(table(grp[,1]))
        
      }else if (is.numeric(grp[,1]) == T){
        x2 = factor(x[,1], as.character(unique(x[,1])))
      }



    
    
    output$plot = renderPlot({
      data_file <- data_file()[input$table_rows_all,]
      if (input$factify == T)
         g<-ggplot(data_file()) + aes(x=factor(x2), y=y[,1]) + theme_classic()
      else 
         g<-ggplot(data_file()) + aes(x=x2, y=y[,1]) + theme_classic()
      
      if (input$col_enter_tf == T){
        enter_col <- as.character(unlist(strsplit(txt2(),",")))
      }else{
        enter_col <- input$fill_one}

      #dodge  <- position_dodge(width=input$basic_inter)
      #dodge2 <- position_dodge2(width=input$basic_inter)
      #dodge3 <- position_dodge2(width=input$basic_inter_e)
      dodge4 <- position_dodge(width=input$basic_lap)
      dodge5 <- position_dodge2(width=input$basic_lap)
      
      
      if (input$divide == 1)
        div <- aes(color = grp[,1] ,fill = grp[,1])
        div2 <- aes(group = grp[,1])
        div3 <- aes(fill = grp[,1])
        div4 <- aes(color = grp[,1])
        
        
        
      if (input$col_dot=="default"){
        dot_line <- NA
      }else if (input$col_dot=="default_o"){
         dot_line <- input$col_dot_line
      }
        #else if (input$col_dot=="select"){
       #  dot_fill <- input$col_dot_fill
      #   dot_line <- NA
      #}else if (input$col_dot=="select_o"){
      #   dot_fill <- input$col_dot_fill
      ##   dot_line <- input$col_dot_line
       #}
      
        
      if (input$fomul == "1"){
        type_formu <- y ~ x
          
      }else if (input$fomul == "2"){
        type_formu <- y ~ x + I(x^2)
      
      }else if (input$fomul == "3"){
        type_formu <- y ~ x + I(x^2) + I(x^3)
      }
      
        
        
        
        
    ###### kind of plot ######
    #ggplot deal dotplot with color, and box with fill
    
      if ((input$graph == "dotplot") && (input$jit == "cent") && (input$divide == T)){
       
        g<-g + geom_dotplot(div3,position = dodge4,dotsize = input$size, color=dot_line,
                            binaxis = "y",stackdir="center", binwidth = .5,alpha=input$line_trans)
      
      }
    
      else if ((input$graph == "dotplot") && (input$jit == "cent"))
        g<-g + geom_dotplot(binaxis = "y",stackdir="center", binwidth = .5 ,alpha=input$line_trans,dotsize = input$size,color=dot_line)
        
        
      
      else if ((input$graph == "dotplot") && (input$jit == "quasirandom") && (input$divide == 1))
        g<-g + geom_quasirandom(aes(color = grp[,1]),dodge.width = input$basic_lap,size = input$size, alpha=input$line_trans,stroke = NA)
      
      else if ((input$graph == "dotplot") && (input$jit == "quasirandom"))
        g<-g + geom_quasirandom(alpha=input$line_trans,size = input$size,stroke = NA)  
        
      
      
      else if ((input$graph == "dotplot") && (input$jit == "no")&& (input$divide == 1))
        g<-g + geom_point(div3,position = dodge4,alpha=input$line_trans,size = input$size,color=dot_line)
      
      else if ((input$graph == "dotplot") && (input$jit == "no"))
        g<-g + geom_point(alpha=input$line_trans,size = input$size,color=dot_line)
      
                                                ########## boxplot ##########
      
      else if ((input$graph == "boxplot")&&(input$jit == "cent")&&(input$divide == 1))
        g<-g + geom_boxplot(div,position = dodge4,alpha=input$fill_trans,outlier.colour = NA,notch = input$notch_tf,width = input$basic_inter) +  
               geom_dotplot(div3,position = dodge4, color =dot_line,
                            binaxis = "y",stackdir="center",binwidth = .5  ,alpha=input$line_trans,dotsize = input$size)
      
      else if ((input$graph == "boxplot")&&(input$jit == "cent"))
        g<-g + geom_boxplot(position = dodge4,alpha=input$fill_trans,outlier.colour = NA,notch = input$notch_tf,width = input$basic_inter,
                            fill = enter_col) +  
               geom_dotplot(position = dodge4, color =dot_line,
                            binaxis = "y",stackdir="center",binwidth = .5 ,alpha=input$line_trans,dotsize = input$size)
        
      else if ((input$graph == "boxplot")&&(input$jit == "quasirandom")&&(input$divide == 1))
        g<-g + geom_boxplot(div,position = dodge4,alpha=input$fill_trans,outlier.colour = NA,notch = input$notch_tf,width = input$basic_inter) +  
               geom_quasirandom(aes(color = grp[,1]),dodge.width = input$basic_lap,size = input$size, alpha=input$line_trans,stroke = NA)
        
      else if ((input$graph == "boxplot")&&(input$jit == "quasirandom"))
        g<-g + geom_boxplot(position = dodge4,alpha=input$fill_trans,outlier.colour = NA,notch = input$notch_tf,width = input$basic_inter,
                            fill = enter_col) +  
               geom_quasirandom(alpha=input$line_trans,size = input$size,stroke = NA) 
      
    
      
      else if ((input$graph == "boxplot") && (input$jit == "no") && (input$divide == 1))
        g<-g + geom_boxplot(div,position = dodge4,alpha=input$fill_trans,notch = input$notch_tf)
      
      else if ((input$graph == "boxplot"))
        g<-g + geom_boxplot(position = dodge4,alpha=input$fill_trans,notch = input$notch_tf)
        
                                                ########### lineplot #########

         
      else if ((input$graph == "lineplot")&&(input$jit == "cent")&&(input$divide == 1)){
        g<-g  + geom_point(div4,alpha=input$line_trans,size = input$size, position=dodge4) +
                geom_point(div4, position=dodge4,
                           stat = 'summary', fun = 'mean') + 
                geom_errorbar(div4,position=dodge4, fun.args = list(mult = as.numeric(input$SE)),
                              stat = 'summary', fun.data = 'mean_se',width=0) +
                stat_summary(aes(group = grp[,1], color = grp[,1]),fun = "mean", geom = "line",position=dodge4)
        shokiti_summ <- "no"
        
      }else if ((input$graph == "lineplot")&&(input$jit == "cent")){
        g<-g  + geom_dotplot(color=dot_line, alpha=input$line_trans, dotsize=input$size,
                             binaxis = "y",binwidth = .5) +
          geom_point(stat = 'summary', fun = 'mean') + 
          geom_errorbar(fun.args = list(mult = as.numeric(input$SE)),
                        stat = 'summary', fun.data = 'mean_se',width=0) +
          stat_summary(fun = "mean", geom = "line")
        shokiti_summ <- "no"
      }
             
                                               ########### scatter #########

        #fill sinnrakukan col sen
        
        
      else if ((input$graph == "scatter")&&(input$divide == 1)){
          shokiti_summ = "no"
          g<-g  + geom_point(div4,alpha=input$line_trans,size = input$size)+
                  geom_smooth(aes(color = group, fill=group),se = input$CI, method = input$type_sm,formula = type_formu)
      
      }else if ((input$graph == "scatter")&&(input$divide == 1) &&(input$fomul == "2")){
        shokiti_summ <- "no"
        g<-g  + geom_point(div4,alpha=input$line_trans,size = input$size)+
          geom_smooth(aes(color = group, fill=group),se = input$CI, method = input$type_sm,formula = y ~ x + I(x^2))  
          
      }else if ((input$graph == "scatter")&&(input$divide == 1) &&(input$fomul == "3")){
        shokiti_summ <- "no"
        g<-g  + geom_point(div4,alpha=input$line_trans,size = input$size)+
          geom_smooth(aes(color = group, fill=group),se = input$CI, method = input$type_sm,formula = y ~ x + I(x^2) + I(x^3))    
      }
          
        
        
    
    ##### stat #####
      if (input$col_stat == F)
        div2 = div4
      
      if(shokiti_summ != "no"){
      
        if ((input$summ == "mean") && (input$graph == "boxplot") && (input$divide == 1))
          g<-g+stat_summary(div2, position = dodge5,fun = "mean", geom = "point",shape=4,size=4)
        
        else if ((input$summ == "mean")&&(input$graph == "boxplot")&& (input$divide == 0))
          g<-g+stat_summary(fun = "mean", geom = "point",size=4,shape=4)
        
        else if ((input$summ == "mean") && (input$graph == "dotplot") && (input$divide == 1))
          g<-g+stat_summary(div2,position = dodge4,
                            fun = "mean", geom = "crossbar",width= input$basic_s * 1.4)
        else if (input$summ == "mean")
          g<-g+stat_summary(fun = "mean", geom = "crossbar",width= input$basic_s *1.4)
        
        else if ((input$summ == "SE") && (input$divide == 1))
          g<-g+stat_summary(div2,position = dodge4,fun = "mean", geom = "crossbar",width= input$basic_s *1.4) + 
          stat_summary(div2,position = dodge4,fun.data = "mean_se", geom = "errorbar",width= input$basic_s )
        else if (input$summ == "SE")
          g<-g+stat_summary(fun = "mean", geom = "crossbar",width= input$basic_s *1.4) + 
          stat_summary(fun.data = "mean_se", geom = "errorbar",width= input$basic_s )
      
      }
        
    #### color ####
      if ((input$col_select == T) && (input$fill_fill %in% palet$wesanderson)){
        txt_chara <- as.numeric(unlist(strsplit(txt(),",")))
        pale_w_s <- wes_palette(input$fill_fill)[txt_chara]
      
      }else if (input$col_select == T){
        txt_chara <- as.numeric(unlist(strsplit(txt(),",")))
        max_num <- brewer.pal.info[input$fill_fill,]$maxcolors
        pale_r_s <- brewer.pal(max_num, input$fill_fill)[txt_chara]
      }
        
      if ((input$one_or_two == T || input$col_enter_tf == T)&& (input$fill_l_b == 1)){
        pale_s <- colorRampPalette(enter_col)(num)
        g<-g + scale_fill_manual(values =pale_s) + scale_color_grey(start = 0,end = 0)  

      }else if ((input$col_enter_tf == T) && (str_detect(input$col_enter,","))){
        g<-g + scale_fill_manual(values =c(enter_col))+ scale_color_manual(values =c(enter_col))
        
      }else if (input$one_or_two == T || input$col_enter_tf == T){
         pale_s <- colorRampPalette(enter_col)(num)
         tryCatch({
           g<-g + scale_fill_manual(values =pale_s) + scale_color_manual(values = pale_s)
         }, 
         finally = {            
           print("ERROR avoiding!!")
           g<-g + scale_fill_gradientn(colours =pale_s) + scale_color_gradientn(colours = pale_s)
         },
         silent = TRUE
         )
       
       
            
        
      
      
      
                               ### weanderson & make outline black & select any color ###
        
      }else if ((input$col_select == T) && (input$fill_fill %in% palet$wesanderson) && (input$fill_l_b == 1)){
        g<-g + scale_fill_manual(values = pale_w_s) + scale_color_grey(start = 0,end = 0)
        
      }else if ((input$col_select == T) && (input$fill_fill %in% palet$wesanderson)){
        g<-g + scale_fill_manual(values =pale_w_s) + scale_color_manual(values = pale_w_s)
        
      }else if ((input$fill_fill %in% palet$wesanderson) && (input$fill_l_b == 1)){
        pale_w <- wes_palette(input$fill_fill, n = num)
        g<-g + scale_fill_manual(values =pale_w) + scale_color_grey(start = 0,end = 0)
        
      }else if (input$fill_fill %in% palet$wesanderson){
        pale_w <- wes_palette(input$fill_fill, n = num)
        g<-g + scale_fill_manual(values =pale_w) + scale_color_manual(values = pale_w)
      
      
      
                             ### Rcolorbrewer & makeoutline black & darken color(1st,2nd condition) & select any color(3rd,4th condition) ###
        
      
        
        
      }else if ((input$col_check == T) && (input$fill_l_b == T)){
        
        num2 <- num + input$col_num-1
        col_pal <- brewer.pal(9, input$fill_fill)[(input$col_num:num2)]
        g <- g + scale_fill_manual(values=col_pal) + scale_color_grey(start = 0,end = 0)
      }
    
      else if (input$col_check == T){
        num2 <- num + input$col_num-1
        col_pal <- brewer.pal(9, input$fill_fill)[(input$col_num:num2)]
        g <- g + scale_fill_manual(values=col_pal) +scale_color_manual(values = col_pal)
      }
        
      else if ((input$col_select == T) && (input$divide == 1) && (input$fill_l_b == 1)){
        g<-g + scale_fill_manual(values = pale_r_s) + scale_color_grey(start = 0,end = 0)
      
      }else if ((input$col_select == T) && (input$divide == 1)){
        g <- g + scale_fill_manual(values = pale_r_s)  + scale_color_manual(values = pale_r_s)
        
        
      }else if ((input$divide == 1) && (input$fill_l_b == 1))
        g<-g + scale_fill_brewer(palette = input$fill_fill) +
               scale_color_grey(start = 0,end = 0)
        
      else if (input$divide == T)
        g<-g + scale_fill_brewer(palette = input$fill_fill) + 
               scale_color_brewer(palette = input$fill_fill)
      

      
    #### axis option ####      
      if (input$lab_x_a == 0)
        lab_x_axis <- element_text(family = input$font_a, size = input$font_x_s,colour = "black")

      else
        lab_x_axis <- element_text(family = input$font_a, size = input$font_x_s,angle=input$lab_x_a,hjust=1,colour = "black",axis.ticks=)
        
      g <- g + theme(plot.title = element_text(family = input$font_t, size = input$font_t_s,hjust = .5),
                     axis.title.x = element_text(family = input$font_a, size = input$lab_x_s),
                     axis.title.y = element_text(family = input$font_a, size = input$lab_y_s),
                     axis.text.x =  lab_x_axis,
                     axis.text.y = element_text(family = input$font_a, size = input$font_y_s,colour = "black"),
                     legend.text = element_text(family = input$font_a, size = input$font_l_s),
                     legend.title = element_text(colour = NA)
      )
    
      g <- g + labs(title = input$lab_t)+  xlab(input$lab_x)+ylab(input$lab_y)

      if (input$not_x == 1)
        g <- g + theme(axis.text.x = element_text(colour =NA))
      
      if (input$change_x == 1)
        g <- g + scale_x_continuous(expand= c(0,0),breaks = seq(from = 0, to = input$max_x, by = input$by_x), limits = c(0,input$max_x))
      
      if (input$change_y == 1)
        g <- g + scale_y_continuous(expand= c(0,0),breaks = seq(from = 0, to = input$max_y, by = input$by_y), limits = c(0,input$max_y)) 
      else{
        g <- g + scale_y_continuous(expand= c(0,0))
      }
      
      if (input$flip == T )
        g <- g + coord_flip()
      
    
      #if ((input$reverse == T) && (reverse_flag == F))
       # g <- g + scale_x_reverse()
      

      return(g)
    
         })
    
  })
  
  #### book marking ####
  
  setBookmarkExclude(c("submit","x","y","group","data_file","datafile","x2","name","grp","x2","g","table","colname1","colname2","plot","Plot","Table"))
  reactive(onRestore(input$rds))
  latestBookmarkURL <- reactiveVal()
  
  onBookmarked(
    fun = function(url) {
      latestBookmarkURL(parseQueryString(url))
    }
  )
  
  onRestored(function(state) {
    showNotification(paste("Restored session:", basename(state$dir)), duration = 10, type = "message")
  })
  
  observeEvent(input$save_inputs, {
    showModal(modalDialog(
      title = "Session Name",
      textInput("session_name", "Please enter a session name (optional):"),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_inputs", "OK")
      )
    ))
  }, ignoreInit = TRUE)
  
  # SAVE SESSION
  output$download_inputs <- downloadHandler(
    filename = function() {
      removeModal()
      session$doBookmark()
      if (input$session_name != "") {
        
        tmp_session_name <- sub("\\.rds$", "", input$session_name)
        
        # "Error: Invalid state id" when using special characters - removing them:
        tmp_session_name <- stri_replace_all(tmp_session_name, "", regex = "[^[:alnum:]]")
        # TODO: check if a valid filename is provided (e.g. via library(shinyvalidate)) for better user feedback
        
        tmp_session_name <- paste0(tmp_session_name, ".rds")
        
      } else {
        paste(req(latestBookmarkURL()), "rds", sep = ".")
      }
    },
    content = function(file) {
      file.copy(from = file.path(
        ".",
        "shiny_bookmarks",
        req(latestBookmarkURL()),
        "input.rds"
      ),
      to = file)
    }
  )
  
  # LOAD SESSION
  observeEvent(input$restore_bookmark, {
    
    sessionName <- file_path_sans_ext(input$restore_bookmark$name)
    targetPath <- file.path(".", "shiny_bookmarks", sessionName, "input.rds")
    
    if (!dir.exists(dirname(targetPath))) {
      dir.create(dirname(targetPath), recursive = TRUE)
    }
    
    file.copy(
      from = input$restore_bookmark$datapath,
      to = targetPath,
      overwrite = TRUE
    )
    
    restoreURL <- paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, ":", session$clientData$url_port, "/?_state_id_=", sessionName)
    
    # redirect user to restoreURL
    runjs(sprintf("window.location = '%s';", restoreURL))
    
    # showModal instead of redirecting the user
    # showModal(modalDialog(
    #     title = "Restore Session",
    #     "The session data was uploaded to the server. Please visit:",
    #     tags$a(restoreURL),
    #     "to restore the session"
    # ))
  })
  
  
  
  
  
}