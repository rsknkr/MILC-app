

############################################################
#
#		MILC APP: lung cancer natural history prediction
#
############################################################


# make sure user has all needed packages
names<-c("MILC", "shinythemes","shiny")

for(name in names){
  #if package not installed, install the package
  if (!(name %in% installed.packages())){
    print("here")
    install.packages(name, repos="http://cran.us.r-project.org",quiet=TRUE,dependencies=FALSE)} 
  library(name, character.only=TRUE,warn.conflicts=FALSE,quietly=TRUE)
}


library(shiny)
library(shinythemes)
library(MILC)


ui <- shinyUI(navbarPage(
  
  # Choose a theme
  theme = shinytheme("united"),
  
  # And custom it with additionnal CSS
  includeCSS("www/style.css") ,
 
  
  # ----------------------
  # SHEET 1 : HOME PAGE
  # ----------------------
  
  
  tabPanel(h4("Home"),
         # fluidRow(  
           
            column(12, offset=0, align="center",
                  
                  # Set the style of this page
                  style="
                  opacity: 0.9;
                  margin-top: -20 px;
                  background-color: #CECAB9;
                  width: 100%;", 
                  br(""),
                  column (6, offset=3, align="justify",
                          style="background-image: url(text-2.png); background-size: cover;",
                          br(""),br(""),br(""),
                          br(""),
                          br(""),br(""),br(""), br(""), br("")),
                  
                  column(8, offset=2,align="justify",
                  # And write the welcome message
                  
                  helpText("The MIcrosimulation Lung Cancer model (MILC) describes the natural history
                                  of lung cancer in the absence of any screening or treatment component.
                            The model simulates the course of lung cancer from the disease free 
                                  state to the more and more advanced disease stages and eventually to death 
                                  from either lung cancer or some other cause.", 
                                  style="color:black ; 
                                  font-family: 'tahoma'; 
                                  font-size:14pt ; font-type:bold"), 
                  
                  helpText("This application is designed to predict and visualize individual 
                                  trajectories of lung cancer progression in a person with given characteristics
                                  using the MILC continuous time microsimulation model.",
                                  style="color:black ; 
                                  font-family: 'tahoma'; 
                                  font-size:14pt ; font-type:bold"),
                  br(""), br(""),br(""), br(""), br(""))
                  
                  
         #         ) 
        )
  ),
  
  
  # -----------------------------------
  # SHEET 2 : DATA INPUT AND PREDICT
  # -----------------------------------
  
  
  tabPanel(h4("Predict"),
           fluidRow(
             column(2, #style="background-color: #CECAB9; height = 100%",
                  
                           # Make some space
                          wellPanel( style="background-color: #CECAB9; height = 100%; opacity: 0.9;",
                         
                           # Age input
                          numericInput("age", label=h3("Age*"), NA, min=0),
                           
                           # Add error message if age is missing
                          uiOutput("warn_age"),
                          
                           # Gender input
                          radioButtons("gender", label = h3("Gender*"),
                                                  choices = c("Male" = "male",
                                                              "Female" = "female")),

                           # Smoking status input
                          radioButtons("smok_status", label = h3("Smoking status*"),
                                                  choices = c("Never" = "never",
                                                              "Former" = "former",
                                                              "Current" = "current")),

                          # Number of cigarettes per day and age when a person started smoking
                          uiOutput("conditionalInput.cig.day"),
                          
                          # warning if not entered
                          uiOutput("warn_cig.day"),
                          
                          # Age of start smoking
                          uiOutput("conditionalInput.age.start"),
                          
                          # warning if not entered
                          uiOutput("warn_age.start"),
                          
                          # quit smoking date for former smokers
                          uiOutput("conditionalInput.age.quit"),
                        
                          # warning if not entered
                          uiOutput("warn_age.quit"),
                         
                          br(), 
                          
                            # Submit button
                          actionButton("submit", label = "Submit", width = "50%",
                                       style="background-color: #163E49;"),
                          br(),br()

           )),
           
           column(10,
                  # plot survival probability
                  plotOutput(outputId = "survival_graph")) 
           )),
  
  
  # ----------------------
  # SHEET 3 : REFERENCES
  # ----------------------
  
  
  tabPanel(h4("References"))
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # ON "SUBMIT" CLICK
  observeEvent(input$submit, {
    #----------
    # WARNINGS
    #----------
    
    # validate input arguments
    output$warn_age <- reactive({
      validate(need(input$age, "Please indicate person's age"))
    })
    
    output$warn_cig.day <- reactive({
      if (input$smok_status %in% c("former","current")) {
        validate(need(input$conditionalInput.cig.day > 0, "Please indicate the number of cigarettes"))
      }
        })
    
    output$warn_age.start <- reactive({
      if (input$smok_status %in% c("former","current")) {
        validate(need(input$conditionalInput.age.start<= input$age, 
                      "Please indicate the age which is less or equal to current age"))
      }
    })
    
    output$warn_age.quit <- reactive({
      if (input$smok_status == "former") {
        validate(need(input$conditionalInput.age.quit >= input$conditionalInput.age.start, 
                      "Please indicate the age which is bigger than the start age"))
      }
    })
    
    #-------
    # PLOTS
    #-------
    # output$survival_graph <- renderPlot({
    # })
   
     
  })
  # END OF ACTION ON "SUBMIT" CLICK
  
   #---------------------
   # CONDITIONAL OUTPUTS
   #--------------------- 
  
   # if a person is a current or former smoker: enter cigarettes per day and age of starting to smoke
  
   output$conditionalInput.cig.day <- renderUI({
     if(input$smok_status %in% c("former", "current")){
       numericInput("d_input", label=h4("Cigarettes per day*"), NA, min=0)
     }
   })
   
   output$conditionalInput.age.start <- renderUI({
     if(input$smok_status %in% c("former", "current")){
       numericInput("age_start_input", label=h4("Age when started smoking*"), NA, min=0)
     }
   })
   
   #if a person is a former smoker: enter the age when last quit smoking
   
   output$conditionalInput.age.quit <- renderUI({
     if(input$smok_status == "former"){
       numericInput("age_quit_input", label=h4("Age when quit smoking*"), NA, min=0)
     }
   })
   

   
}

# Run the application 
shinyApp(ui = ui, server = server)

