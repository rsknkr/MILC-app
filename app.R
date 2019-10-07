




############################################################
#
#		MILC APP: lung cancer natural history prediction
#
############################################################


# make sure user has all needed packages
names <- c("MILC", "shinythemes", "shiny", "survival", "doParallel", "shinyjs", "shinycssloaders", "foreach")

for (name in names) {
  #if package not installed, install the package
  if (!(name %in% installed.packages())) {
    print("here")
    install.packages(name,
                     repos = "http://cran.us.r-project.org",
                     quiet = TRUE,
                     dependencies = FALSE)
  }
  library(
    name,
    character.only = TRUE,
    warn.conflicts = FALSE,
    quietly = TRUE
  )
}


ui <- shinyUI(navbarPage(
  # Choose a theme
  theme = shinytheme("united"),
  
  # And customize it with additionnal CSS
  includeCSS("www/style.css") ,
  
  
  # ----------------------
  # SHEET 1 : HOME PAGE
  # ----------------------
  
  
  tabPanel(
    h4("Home"),
    # fluidRow(
    
    column(
      12,
      offset = 0,
      align = "center",
      
      # Set the style of this page
      style = "
      opacity: 0.9;
      margin-top: -20 px;
      background-color: #CECAB9;
      width: 100%;",
      br(""),
      column (
        6,
        offset = 3,
        align = "justify",
        img(
          src = "MILC.png",
          height = "80%",
          width = "100%"
        )
        
      ),
      
      column(
        10,
        offset = 1,
        align = "justify",
        # And write the welcome message
        
        helpText(
          "The MIcrosimulation Lung Cancer model (MILC) describes the natural history
          of lung cancer in the absence of any screening or treatment component.
          The model simulates the course of lung cancer from the disease free
          state to the more and more advanced disease stages and eventually to death
          from either lung cancer or some other cause.",
          style = "color:black ;
          font-family: 'arial';
          font-size:16pt ; font-type:bold"
        ),
        
        helpText(
          "This application is designed to predict and visualize individual risks of
          being diagnosed with lung cancer in a person with given characteristics
          using the MILC continuous time microsimulation model.",
          style = "color:black ;
          font-family: 'arial';
          font-size:16pt ; font-type:bold"
        ),
        br(""),
        br(""),
        helpText(
          "- shiny app by kira raskina -",
          style = "color:white ;
          text-align: center;
          font-style: bold; font-family: 'Courier New';
          font-size:16pt ; "
        ),
        br(""),
        br(""),
        br("")
        )
      
      
      #         )
        )
    ),
  
  
  # -----------------------------------
  # SHEET 2 : DATA INPUT AND PREDICT
  # -----------------------------------
  
  
  tabPanel(h4("Predict"),
           fluidRow(useShinyjs(),
                    
                    column(
                      2,
                      #style="background-color: #CECAB9; height = 100%",
                      
                      # Make some space
                      wellPanel(
                        style = "background-color: #CECAB9; height = 100%; opacity: 0.9;",
                        
                        div(
                          id = "form",
                          
                          # Age input
                          numericInput("age", label = h3("Age*"), NA, min =
                                         0),
                          
                          # Add error message if age is missing
                          uiOutput("warn_age"),
                          
                          # Gender input
                          radioButtons(
                            "gender",
                            label = h3("Gender*"),
                            choices = c("Male" = "male",
                                        "Female" = "female")
                          ),
                          
                          # Smoking status input
                          radioButtons(
                            "smok_status",
                            label = h3("Smoking status*"),
                            choices = c(
                              "Never" = "never",
                              "Former" = "former",
                              "Current" = "current"
                            )
                          )
                          
                        ),
                        # close div
                          
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
                        
                        actionButton(
                          "submit",
                          label = "Submit",
                          width = "85%",
                          style = "background-color: #163E49;",
                          icon = icon("check")
                        ),
                        
                        br(),
                        br(),
                        
                        # Reset button
                        actionButton(
                          "reset",
                          label = "Reset",
                          width = "85%",
                          style = "background-color: #163E49;",
                          icon = icon("refresh")
                        ),
                        
                        br(),
                        br()
                        
                      )
                    ),
                    
                    column(
                      10,
                      hidden(div(id = "loading",
                                       # plot survival probability
                                       withSpinner(plotOutput(outputId = "survival_plot",
                                                              width = "70%"), color = "#0dc5c1")
                                 )
                             ) #close hidden div
              
                    ))), 
  
  
  # ----------------------
  # SHEET 3 : REFERENCES
  # ----------------------
  
  
  tabPanel(h4("References"),
           column(
             10,
             offset = 1,
             align = "justify",
             # And write the welcome message
             
             helpText(
               tags$ol(
                 tags$li("Package 'MILC' by Stavroula Chrysanthopoulou"
                 ),
                 tags$li("Stavroula A Chrysanthopoulou, 2017. 
                         'MILC: A Microsimulation Model of the Natural History of Lung Cancer,' 
                         International Journal of Microsimulation, International 
                         Microsimulation Association, vol. 10(3), pages 5-26")
               ), 
               style = "color:black ;
               font-family: 'arial';
               font-size:16pt ; font-type:bold"
             )
             ))
             ))




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # ON "SUBMIT" CLICK
  observeEvent(input$submit, {
    
    #----------
    # WARNINGS
    #----------
    
    # validate input arguments
    
    #AGE
    output$warn_age <- reactive({
        shiny::validate(need(input$age >= 0, "Please indicate person's age"))
      })
    #}
    req(input$age)

    # IF EVER SMOKED - NUM OF CIGS
    if (input$smok_status %in% c("former", "current")) {
      output$warn_cig.day <- reactive({
        shiny::validate(need(input$d_input > 0,
                      "Please indicate the number of cigarettes"))
        
      })
    }
    
    # IF EVER SMOKED - WHEN STARTED; ALSO, THIS AGE OF START TO BE LESS OR EQUAL TO CURRENT
    if (input$smok_status %in% c("former", "current")) {
      agediff <- reactive({
        input$age >= input$age_start_input
      })
      
      output$warn_age.start <- reactive({
        shiny::validate(
          need(input$age_start_input >= 0,
               "Please indicate the correct age"),
          need(
            agediff() == TRUE,
            "Please indicate the age which is less or equal to current age"
          )
        )
        
      })
    }
    
    
    # IF FORMER SMOKER - WHEN QUIT
    output$warn_age.quit <- reactive({
      if (input$smok_status == "former") {
        agediffsmoke <- reactive({
          input$age_quit_input >= input$age_start_input
        })
        
        shiny::validate(
          need(
            input$age_quit_input >= 0,
            "Please indicate the correct age"
          ),
          need(
           agediffsmoke() == TRUE,
            "Please indicate the age which is bigger than the start age"
          )
        )
      }
    })
    
    
    if (input$smok_status %in% c("former", "current")) {
      req(input$d_input>0)
      req(input$age_start_input>=0)
      req(agediff() == TRUE)
    }
    
    if (input$smok_status == "former") {
      req(input$age_quit_input>=0)
      req(agediffsmoke() == TRUE)
    }
    
    
    shinyjs::showElement(id = "loading")
    disable("submit")
    
    #------------------------
    # SURVIVAL DATA CREATION
    #------------------------
    
    isolate({
      age <- input$age
      pred_yrs <- max(120 - age, 20)
      gender <- input$gender
      status <- input$smok_status
      cig <-  ifelse(status=="never",
                     0,
                     input$d_input)
      
      ts <- ifelse(status=="never",
                   NA,
                   input$age_start_input)
      
      tq <- ifelse(!status=="former",
                   NA,
                   input$age_quit_input)
    })

    # SIMULATE DATA WITH parallel processing

    registerDoParallel(detectCores())
      # Return a data frame
      df <-
        foreach (
          i = 1:10000,
          .combine = rbind,
          .packages = c("MILC")
          # .export = c(
          #   "age",
          #   "cig",
          #   "pred_yrs",
          #   "gender",
          #   "status",
          #   "ts",
          #   "tq"
          # )
        ) %dopar% {
          unlist(nat_hist(
            c(runif(5), age, cig),
            pred_yrs,
            gender,
            status,
            ts,
            tq,
            0.00042,
            c(3.91, 3.91),
            c(1.1, 1.1),
            c(2.8, 2.8)
          )[c("T_diagn", "T_death")])
        }


      # CREATE TABLE OF SURVIVAL DATA
      ## create columns with time and status

      # old colnames - c("T_diagn", "T_death")
      colnames(df) <- c("status", "time")

      # finish parallel processing
      stopImplicitCluster()


      cl <- makeCluster(detectCores())
      clusterExport(cl, "df") #  in order for the function to see the variable

      #T_final <- 120
      # IF diagnosed (T_diagn) before death (T_death) THEN status=1, ELSE 0
      # IF dead (T_death) before the end of follow-up (T_final) THEN time=T_death, ELSE T_final

      df[, "status"] <- parApply(cl, data.frame(df), 1, 
                                 function(x)
                                   1 * (x[1] < x[2]))
      
      df[, "time"] <- parSapply(cl, df[, "time"], 
                                function(x)
                                  x = min(x, max(age+20, 120)))

      stopCluster(cl)

      # FIT SURVIVAL FUNCTION
      ## Kaplan-Meier estimator without grouping

      #T_entry <- age #age at the beginning of the prediction period
      #T_final <- 120 #age at the end of the prediction period

      km <- survfit(Surv(time, status) ~ 1, data = data.frame(df))


      #---------------
      # SURVIVAL PLOT
      #---------------
      output$survival_plot <- renderPlot({
        isolate(
          # PLOT KAPLAN-MEIER CURVE
          plot(
            km,
            xlab = "Follow-up time (years)",
            ylab = "Survival probability",
            xlim = c(age, max(km$time)),
            ylim = c(0.9*min(summary(km)$lower), 1) # from minimal lower edge of survival CI
          )
        )
        #Close the render-plot
      }, height = 800)

    enable("submit")
  })
  # END OF ACTION ON "SUBMIT" CLICK
  
  # ON RESET CLICK
  observeEvent(input$reset, {
    reset("form")
    output$warn_age <- reactive({return(NULL)})
    output$warn_age.quit <- reactive({return(NULL)})
    output$warn_age.start <- reactive({return(NULL)})
    output$warn_cig.day <- reactive({return(NULL)})
    shinyjs::hideElement(id = "loading")
  })
  

  #---------------------
  # CONDITIONAL OUTPUTS
  #---------------------
  
  # if a person is a current or former smoker: enter cigarettes per day and age of starting to smoke
  
  output$conditionalInput.cig.day <- renderUI({
    if (input$smok_status %in% c("former", "current")) {
      numericInput("d_input",
                   label = h4("Cigarettes per day*"),
                   value = NA)
    }
  })
  
  output$conditionalInput.age.start <- renderUI({
    if (input$smok_status %in% c("former", "current")) {
      numericInput(
        "age_start_input",
        label = h4("Age when started smoking*"),
        value = NA
      )
    }
  })
  
  #if a person is a former smoker: enter the age when last quit smoking
  
  output$conditionalInput.age.quit <- renderUI({
    if (input$smok_status == "former") {
      numericInput(
        "age_quit_input",
        label = h4("Age when quit smoking*"),
        value = NA
      )
    }
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
