ui <- function(request){
  fluidPage(theme = shinytheme("united"),
    tags$script(src = "returnClick.js"),
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(type="tabs",
                  tabPanel("basic",
                           fileInput("datafile", "Choose data File",
                                     accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv",
                                       ".xlsx")),
                           htmlOutput("colname3"),
                           tags$hr(),
                          
                           htmlOutput("colname1"),
                           conditionalPanel(condition = "input.graph == 'scatter' || input.graph == 'lineplot'",
                                                     tags$h6("Please check out")),
                           checkboxInput("factify","make x factor",T),
                           htmlOutput("colname2"),
                           checkboxInput("divide","by Group(or Colored)",T),
                           conditionalPanel(condition = "input.divide == true",
                                            htmlOutput("soubetuka")),
                           
                           selectInput("graph","graph:",choices=c("dotplot(n<10)"="dotplot",
                                                                  "boxplot",
                                                                  "line graph"="lineplot",
                                                                  "scatter plot"="scatter")),
                           actionButton("submit", "plot"),
                           selectInput("jit","jitter:",choices=c("center"="cent",
                                                                 "quasirandom",
                                                                 "none(Not Recomended)"="no")),
                           conditionalPanel(condition = "input.graph == 'boxplot'",
                            sliderInput("basic_inter","width:",min = 0,max = 1,value = 0.6)),
                           conditionalPanel(condition = "input.graph == 'dotplot'",
                           sliderInput("basic_s","width:",min = 0,max = 1,value = 0.5)),
                           sliderInput("basic_lap","Overlaping:",min = 0,max = 2,step = .01,value = 0.7)),
                           
                  tabPanel("color",
                           conditionalPanel(condition = "input.col_enter_tf == true",
                              textInput("col_enter","Writing:",placeholder = "#FFF000,#DE3163,black")),
                           
                           conditionalPanel(condition = "input.one_or_two == false",
                             conditionalPanel(condition = "input.col_enter_tf == false",
                                              selectInput("fill_fill","Fill:",choices=palet),
                             
                                             conditionalPanel(condition = "input.col_check == false",
                                               checkboxInput("col_select","select any color",F),
                                               conditionalPanel(condition = "input.col_select == true",
                                                                textInput("col_text", "Writing:",placeholder = "1,2,5")
                                                                )),
                                             
                                             conditionalPanel(condition = "input.col_select == false ",
                                               checkboxInput("col_check","darken the color(not used wes)",F),
                                               conditionalPanel(condition = "input.col_check == true",
                                                                numericInput("col_num", "Gradient:",min = 1,max = 9,step = 1,value = 4)
                                                                )))
                             ),
                           
                           conditionalPanel(condition = "input.one_or_two == true",
                             conditionalPanel(condition = "input.col_enter_tf == false",
                                              colourpicker::colourInput("fill_one", "color:", value = "red",palette = "limited"))
                             ),
                           
                           
                           checkboxInput("fill_l_b","Make the outline black",F),
                           conditionalPanel(condition = "input.col_enter_tf == false",
                                            
                                            checkboxInput("one_or_two","use a single color",F)),
                           
                           conditionalPanel(condition = "input.one_or_two == false",
                             checkboxInput("col_enter_tf","Enter any color",F)),
                           conditionalPanel(condition = "input.graph == 'boxplot' || input.graph == 'violinplot'",
                             checkboxInput("notch_tf","notch ON",F),
                             sliderInput("fill_trans","Transparency(plot)",min = 0,max = 1,value = .5)),
                           sliderInput("line_trans","Transparency(dot)",min = 0,max = 1,value = .8),
                           selectInput("col_dot","dot color + outline:",choices = c("default + none"="default",
                                                                                    "default + outline"="default_o"
                                                                                   # "select + none"="select",
                                                                                   # "select + outline"="select_o"
                                                                                    )),
                           conditionalPanel(condition = "input.col_dot == 'select_o' || input.col_dot == 'select'",
                                            colourpicker::colourInput("col_dot_fill", "Fill(dot):", value = "black",palette = "limited")),
                           conditionalPanel(condition = "input.col_dot == 'default_o' || input.col_dot == 'select_o'",
                                            colourpicker::colourInput("col_dot_line", "Outline:", value = "black",palette = "limited"))

                         ),
                  tabPanel("axis",
                           numericInput("size","dot size:",value=1.5),
                           textInput("lab_t", "Title:",v=""),
                           splitLayout(cellWidths = c("75%","25%"),
                            selectInput("font_t","font",choices = font),
                            numericInput("font_t_s","size",value = 11),
                            tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))),
                           tags$hr(),
                           splitLayout(cellWidths = c("75%","25%"),
                                       textInput("lab_x", "x lab:",v=""),
                                       numericInput("lab_x_s","size",value = 11)),
                           splitLayout(cellWidths = c("75%","25%"),
                                       textInput("lab_y", "y lab:",v=""),
                                       numericInput("lab_y_s","size",value = 11)),
                           selectInput("font_a","font",choices = font),
                           splitLayout(cellWidths = c("33%","33%","33%"),
                                       numericInput("font_x_s"," x value size",value = 11),
                                       numericInput("font_y_s"," y",value = 11),
                                       numericInput("font_l_s"," legend",value = 11)),
                           
                           
                           tags$hr(),
                           splitLayout(cellWidths = c("50%","50%"),
                              numericInput("lab_x_a","x lab angle", value = 0),
                              checkboxInput("not_x","or not showing",F)),
                           checkboxInput("flip","flip x axis",F),
                           checkboxInput("reverse","reverse axis", F),
                           checkboxInput("lab_zero","genten 0", F),
                           checkboxInput("change_x","change max value(x)",F),
                           checkboxInput("change_y","change max value(y)",F),
                           conditionalPanel(condition = "input.change_x == true",
                             splitLayout(cellWidths = c("50%","50%"),
                              numericInput("max_x","max value(x):", value = 50),
                              numericInput("by_x","by value(x):", value = 10))),
                           conditionalPanel(condition = "input.change_y == true",
                             splitLayout(cellWidths = c("50%","50%"),
                              numericInput("max_y","max value(y):", value = 50),
                              numericInput("by_y","by value(y):", value = 10)))),
                  tabPanel("option",
                           fileInput("restore_bookmark", "Restore Session", multiple = FALSE, accept = ".rds"),
                           actionButton("save_inputs", 'Save Session', icon = icon("download")))
          )),
      mainPanel(
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Table",
                             dataTableOutput('table')),
                    
                    tabPanel("Plot", 
                             column(1,
                              img(src = "tate.png", height = 786, width = 28)),
                             column(11,
                              jqui_resizable(plotOutput("plot", height = "800px")),
                              img(src = "yoko.png", height = 28, width = 786),
                              conditionalPanel(condition = "input.graph == 'dotplot' || input.graph == 'boxplot'",
                               
                               selectInput("summ","summary:",choices=c("mean","SE","no"),selected=shokiti_summ),
                               checkboxInput("col_stat","stat is black",F)
                                               ),
                              conditionalPanel(condition = "input.graph == 'lineplot'",
                               selectInput("SE", "SE:",choices = c("1SE(68%)"=1,
                                                                  "1.645SE(90%)"=1.645,
                                                                  "2SE(95%)"=2,
                                                                  "2.575SE(99%)"=2.575),selected = "1SE(68%)"),
                               
                                               ),
                              conditionalPanel(condition = "input.graph == 'scatter'",
                                selectInput("fomul","degree:",choices = c("y ~ x"="1",
                                                                          "y ~ x + I(x^2)"="2",
                                                                          "y ~ x + I(x^2) + I(x^3)"="3"),selected = "y ~ x"),
                                checkboxInput("CI","SE:",T),
                                selectInput("type_sm","Type of smooth:",choices = c("lm","glm","gam","loess"))
                                              ),
                                selectInput("type_er","type of error bar",choices = c("1","2"))
                                    )),
                    tabPanel("palette",  img(src = "wesanderson.png", height = 264, width = 466),
                                         img(src = "rcolorbrewer.png", height = 496, width = 396))
                    )
                    
        )
      )
    )
}