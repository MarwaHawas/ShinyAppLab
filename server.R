

library(shiny)
library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
library(DT)
library(glue)

db <- dbConnect(SQLite(), "./database/database.sqlite3")
 credentials <- tbl(db, "credentials") %>% collect()
#   mutate(locked_out = as.logical(locked_out))
#orders <- tbl(db, "orders") %>% collect()
#results <- tbl(db, "results") %>% collect()
#new_orders <- tbl(db, "new_orders") %>% collect()

lab_managers = data.frame(user = c("David", "john"),
                          pw = c(111111, 111111))


standard_file <- read_csv("./standard.csv") %>%
  mutate(Test_Name = factor(rep(c("HDL", "LDL", "Triglycerides"), each = 11)))
#predict_results <- function()

num_fails_to_lockout <- 3

shinyServer(function(input, output, session) {
  
  
#### UI code --------------------------------------------------------------
 
  observeEvent(input$log_out, {
    user_input$authenticated = FALSE
    output$see_result = NULL
    output$new_order = NULL
    output$new_order_manager = NULL
    output$see_result_manager = NULL
  })
  
  
   output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        sidebarLayout(
          sidebarPanel(width = 3,
            radioButtons("account", "Account Log in:",
                         choices = c("Patient", "Lab Manager")),
            textInput("user_name", "User ID:"),
            passwordInput("password", "Password:"),
            actionButton("login_button", "Log in",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            uiOutput("pass"),
            br(),
            h4("New here? Sign up!"),
            textInput("user_name_signup", "User ID:"),
            textInput("name_signup", "Name:"),
            passwordInput("password_signup", "Password:"),
            actionButton("signup_action", "Sign Up",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            br(),
            uiOutput("signup_error"),
            uiOutput("signup_success")
          ),
          mainPanel(
            h1("Welcome to M Lab!"),
            actionButton("location", "Our Location", icon("paper-plane"), 
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            actionButton("about_us", "About us",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            hr(),
            uiOutput("location"),
            uiOutput("about_us"),
            img(src = "intro1.png"),
            h5("Lab Website Developed using R Shiny"), 
            hr()
            
          )
        )
      )
    } 
    else if(input$account == "Patient"){
      fluidPage(
        column(11,
               sidebarLayout(
                 sidebarPanel(
                   h3("Patient's Interface", style = "color:gray"),
                   tableOutput("patient_details"),
                   br(),
                   actionButton("new_order", "New Order"),
                   actionButton("see_result", "See Result"),
                   uiOutput("new_order")
                 ),
                 mainPanel(
                   img(src = "patient.png"),
                   hr(),
                   dataTableOutput("see_result")
                 )
               )
             ),
        
        column(1,
               actionButton("log_out", "Log Out"))
      )
      
    }
    else if(input$account == "Lab Manager"){
      fluidPage(
        column(11,
               sidebarLayout(
                 sidebarPanel(
                   h3("Lab Manager's Interface", style = "color:gray"),
                   br(),
                   actionButton("new_order_manager", "See New Orders"),
                   actionButton("see_result_manager", "See Results"),
                   br(),
                   br(),
                   actionButton("upload_result", "Upload Result"),
                   actionButton("report", "Report"),
                   br(),
                   br(),
                   actionButton("standardf", "Standard File"),
                   actionButton("data_analysis", "Data Analysis"),
                   uiOutput("warning"),
                   uiOutput("file1"),
                   uiOutput("action_result")
                 ),
                 mainPanel(
                   dataTableOutput("new_order_manager"),
                   uiOutput("download1_ui"),
                   uiOutput("seen"),
                   br(),
                   dataTableOutput("see_result_manager"),
                   uiOutput("download2_ui"),
                   br(),
                   dataTableOutput("standard_file"),
                   br(),
                   uiOutput("report_ui"),
                   textOutput("report1"),
                   plotOutput("report2"),
                   plotOutput("report3"),
                   plotOutput("report4"),
                   downloadButton("report_download", "Download report"),
                   br(),
                   br(),
                   plotOutput("data_analysis")
                 )
               )
        ),
        
        column(1,
               actionButton("log_out", "Log Out"))
      )
    } 
    
  })
  
#### APP'S SERVER CODE GOES HERE ----------------------------------------

   ## Patient page
  
  output$patient_details <- renderTable({
    credentials <- tbl(db, "credentials") %>% collect()
    data.frame("User_ID" = input$user_name,
               "Patient_Name" = credentials$user_name[credentials$user == input$user_name])
  })
  
  observeEvent(input$new_order,{
    output$new_order = renderUI({
      p(br(),
        "No appointment is necessary, but you will need the following:",
        tags$li("Bring your ID"),
        tags$li("Request Test Order online"),
        br(),
        p("All laboratory services in our locations are walk-in clinics."),
        selectInput("new_test", "Choose test from list",
                    choices = c("HDL", "LDL", "Triglycerides")),
        actionButton("submit_order", "Submit order"),
        br(),
        br(),
        uiOutput("order_submitted")
      )
     })
  })
  
  observeEvent(input$submit_order, {
    new_orders <- tbl(db, "new_orders") %>% collect()
    credentials <- tbl(db, "credentials") %>% collect()
    new <- data.frame(user = input$user_name,
                      test_name = input$new_test,
                      user_name = credentials$user_name[credentials$user == input$user_name])
    new_orders <- rbind(new_orders, new)
    dbWriteTable(db, "new_orders", new_orders, overwrite = TRUE)
    #db <- dbConnect(SQLite(), "./database/database.sqlite3")
    output$order_submitted <- renderUI({
      h4("Order Submitted!")
    })
  })
  
  model = reactive({
    lm(result ~ expected_raw + Test_Name, data = standard_file)
  })

  observeEvent(input$see_result, {
    df = tbl(db, "results") %>% collect()
    df$Result = round(predict(model(), newdata = df), 2)
    output$see_result <- renderDataTable({
      datatable(df %>%
                  select(User_ID , Test_Name, Date_Submitted, Date_analyzed, Result) %>%
                  filter(User_ID == input$user_name),
                options = list(autoWidth = TRUE,
                               dom = "tp")) %>%
        formatStyle("Result", color = "blue", fontWeight = "bold")
      
    })
  })
  
  observeEvent(input$new_order_manager, {
    output$new_order_manager <- renderDataTable({
      datatable(tbl(db, "new_orders") %>% collect(),
                class = 'cell-border stripe',
                rownames = FALSE,
                caption = "New Orders",
                options = list(dom = "tip",
                               autoWidth = TRUE,
                               pageLength = 5,
                               columnDefs = list(list(width = "10px", targets = "_all"))))
    })
    
    
    output$download1_ui <- renderUI({
      downloadButton("download1", "Download New Orders.csv")
    })
    
    output$download1 <- downloadHandler("New Orders.csv",
                                        content = function(file){
                                          write_csv(tbl(db, "new_orders") %>% collect(), file)
                                        })

    
    output$seen <- renderUI({
      checkboxInput("seen", "New orders delivered")
    })
    
  })
  
  observeEvent(input$standardf, {
    output$standard_file <- renderDataTable({
      datatable(standard_file,
                class = 'cell-border stripe',
                caption = "Standard File",
                options = list(autoWidth = TRUE,
                               dom = "tp"))
    })
  })

  
  observeEvent(input$see_result_manager, {
    output$see_result_manager <- renderDataTable({
      df = tbl(db, "results") %>% collect()
      df$Result = round(predict(model(), newdata = df), 2)
      datatable(df,
                class = 'cell-border stripe',
                rownames = FALSE,
                caption = "Results",
                options = list(dom = "tip",
                               pageLength = 5,
                               autoWidth = TRUE,
                               columnDefs = list(list(width = "10px", targets = "_all"))))
    })
    
    output$download2_ui <- renderUI({
      downloadButton("download2", "Download Results.csv")
    })
    
    output$download2 <- downloadHandler("Results.csv",
                                        content = function(file){
                                          df = tbl(db, "results") %>% collect()
                                          df$Result = round(predict(model(), newdata = df), 2)
                                          write_csv(df, file)
                                        }
    )
  })
  
  observeEvent(input$seen, {
    orders <- tbl(db, "orders") %>% collect()
    new_orders <- tbl(db, "new_orders") %>% collect()
    orders <- rbind(orders, new_orders)
    new_orders <- new_orders[-c(1:nrow(new_orders)), ]
    dbWriteTable(db, "new_orders", new_orders, overwrite = TRUE)
    dbWriteTable(db, "orders", orders, overwrite = TRUE)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$upload_result, {
    output$warning <- renderUI({
      p("Please make sure that the file you are going to upload
        has the exact same columns as the original 'results' data file", style = "color:gray;")
    })
    
    output$file1 <- renderUI({
      fileInput("file1", "Please select the results file")
    })
    
    
    output$action_result <- renderUI({
      actionButton("action_result", "Upload Result")
    })
    
  })
  
  observeEvent(input$action_result, {
    new_results <- read_csv(input$file1$datapath)
    print(head(new_results))
    dbAppendTable(db, "results", new_results)
  })
  
  observeEvent(input$report, {
    output$report <- renderUI({
      h5("Reports not yet implemented", style = "color:gray;")
    })
  })
  
  observeEvent(input$data_analysis, {
    output$data_analysis = renderPlot({
      standard_file %>%
        mutate(test = rep(c("HDL", "LDL", "Triglycerides"), each = 11)) %>%
        ggplot(aes(x = result, y = expected_raw)) +
        geom_point() +
        facet_wrap(~test) +
        ggtitle("Data analysis plots grouped by Tests")
    })
  })
  
  output$report_download = downloadHandler(
    filename = "report.pdf",
    content = function(file){
      tempReport <- file.path(tempdir(), "Re.Rmd")
      file.copy("Re.Rmd", tempReport, overwrite = TRUE)
      params <- list(results = tbl(db, "results") %>% collect() %>%
                       mutate(Result = case_when(
                         Test_Name == "HDL" ~ expected_raw * 300,
                         Test_Name == "LDL" ~ expected_raw * 200,
                         Test_Name == "Triglycerides" ~ expected_raw * 300
                       )))
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  observeEvent(input$report, {
    df = tbl(db, "results") %>% collect() %>%
      mutate(Result = case_when(
        Test_Name == "HDL" ~ expected_raw * 300,
        Test_Name == "LDL" ~ expected_raw * 200,
        Test_Name == "Triglycerides" ~ expected_raw * 300
      ))
    #print(head(df))
    output$report_ui = renderUI({
      h3("Report")
    })
    
    output$report1 = renderText({
      glue("Total HDL tests: {sum( df$Test_Name == 'HDL')}
      Total Triglycerides tests: {sum( df$Test_Name == 'Triglycerides')}
           Total LDL tests: {sum( df$Test_Name == 'LDL')}") 
    })
    
    output$report2 = renderPlot({
      hist(df$Result[df$Test_Name == "Triglycerides"], main="Painet Result for Triglycerides",
           xlab="Triglycerides Level ",
           col="gray",
           border="black"
      )
    })
    
    output$report3 = renderPlot({
      hist(df$Result[df$Test_Name == "LDL"], main="Painet Result for LDL",
           xlab="LDL Level ",
           col="gray",
           border="black"
      )
    })
    
    output$report4 = renderPlot({
      hist(df$Result[df$Test_Name == "HDL"], main="Painet Result for HDL",
           xlab="HDL Level ",
           col="gray",
           border="black"
      )
    })
  })
  
  observeEvent(input$signup_action, {
    credentials <- tbl(db, "credentials") %>% collect()
    user <- input$user_name_signup
    status <- "ok"
    pw <- input$password_signup
    if (!is.character(user) || is.na(user) || user == "" || str_detect(user, " ")){
      status <- "Invalid User_ID!"
    }
    if(user %in% credentials$user){
      status <- "User_ID exists!"
    }
    if(pw == "" || is.na(pw)){
      status = "Invalid Password!"
    }
    
    if(status != "ok"){
      output$signup_error <- renderUI({
        h5(glue({status}), style = "color:red;")
      })
    }
    else{
      output$signup_success <- renderUI({
        h5("Congrats! You have successfully signed up!", style = "color:gray")
      })
      dbAppendTable(db, "credentials",
                    data.frame(user = user,
                               pw = pw,
                               locked_out = 0,
                               user_name = input$name_signup))
    }
      
  })
  
  ## location
  observeEvent(input$location, {
    output$location <- renderUI({
      p("We Have Two Location in STLouis County 1-1145 Olive St 63141 creve coure  2-1546 Baxter Rd 61676 chesterfield ", style = "color:gray;")
    })
  })
  
  observeEvent(input$about_us, {
    output$about_us <- renderUI({
      p("M Lab typically processes tests on more than 2.5 million patient specimens per week and supports clinical trial activity in approximately 100 countries through its industry-leading central laboratory business
", style = "color:gray")
    })
  })
  
  
  
  
    
#### PASSWORD server code ---------------------------------------------------- 

  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "")

  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out, 
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    credentials <- tbl(db, "credentials") %>% collect()
    
    row_username <- which(credentials$user == input$user_name)
    row_password <- which(credentials$pw == input$password) # digest() makes md5 hash of password
    

    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$user_locked_out <- credentials$locked_out[row_username]
    }

    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in 
    #     credentials DF to TRUE and save DF
    if (input$login_button == num_fails_to_lockout & 
        user_input$user_locked_out == FALSE) {

      user_input$user_locked_out <- TRUE
            
      if (length(row_username) == 1) {
        credentials$locked_out[row_username] <- TRUE
        
        #saveRDS(credentials, "credentials/credentials.rds")
      }
    }
      
    # if a user has valid credentials and is not locked out, he is authenticated      
    if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
      user_input$authenticated <- TRUE
    } else {
      user_input$authenticated <- FALSE
    }
    
    # check for lab manager
    if(input$account == "Lab Manager"){
      user_input$authenticated <- FALSE
      if(input$user_name %in% lab_managers$user){
        if(input$password == lab_managers$pw[lab_managers$user == input$user_name]){
          user_input$authenticated <- TRUE
        }
      }
    }
    
    if(input$user_name %in% lab_managers$user & input$account == "Patient"){
      user_input$authenticated <- FALSE
    }

    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (user_input$user_locked_out == TRUE) {
        user_input$status <- "locked_out"  
      } else if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"  
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      } else if(!(input$user_name %in% lab_managers$user) & input$account == "Lab Manager"){
        user_input$status <- "bad_lab_manager"
      } else if(input$user_name %in% lab_managers$user & input$account == "Patient"){
        user_input$status <- "bad_patient"
      }
    }
  })   

  # password entry UI componenets:
  #   username and password text fields, login button

  

  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {    
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else if(user_input$status == "bad_lab_manager"){
      h5(strong("Not a Lab Manager!", style = "color:red"), align = "center")
    } else if(user_input$status == "bad_patient"){
      h5(strong("Not a Patient!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })  
})
