

library(shiny)     #libraries to be imported
library(markdown)

u <-shinyUI( #ui function
  navbarPage("CSMSC 150 APP",             # creates a Navbarpage panel
         tabPanel("QSI",
              sidebarPanel( #this panel contains a sidebarPanel and a mainPanel for performing QSI 
                textInput('vec1', 'Enter your x values (SEPARATE VALUES USING COMMA)', "0,1,2"),       #these are where the inputs will be placed
                textInput('vec2', 'Enter your y / f(x) values (SEPARATE VALUES USING COMMA)', "0,1,2"),
                numericInput('x', "Enter value to be approximated using quadratic spline interpolation",0,) 
              ),
              
              mainPanel(
                h2('Entered values:'),       #the main panel will contain the text outputs
                verbatimTextOutput("oid1"),
                verbatimTextOutput("oid2"),  
                verbatimTextOutput("oid4"),
                verbatimTextOutput("oid3"),
                h2('Results:',style="color:red"), #headers for the results
                tags$head(tags$style("#oid3{
                color: red
                 }")),
                
                verbatimTextOutput("oid5"),
                tags$head(tags$style("#oid5{
                color: blue;
                font-size: 15px;
                font-style: italic;
                 }")),
              )   
                  
                  
                  
                  
          ),
         
         tabPanel("SIMPLEX",                                                             #this tabPanel contains the functions for performing Simplex
                  sidebarPanel(
                    textAreaInput('vec3', 'Enter the values of your initial tableau: (SEPARATE VALUES USING COMMA)', "0,1,2"), #These are the inputs
                    radioButtons('choice', "Choose an option", c("maximization"=TRUE,"minimization"=FALSE)),
                    radioButtons('choice2', "Will you be solving the given problem?", c("Yes"=TRUE,"No"=FALSE)),
                  ),
                  
                  mainPanel(                                                                 #these are the outputs
                    h2('Initial Tableau:'),
                    uiOutput("initialTab"),
                    
                    h2('Results:',style="color:red"),   #The outputs can be text or rendered tables
                    h3("Final Tableau"),
                    uiOutput("finalTableau"),
                    h3("Basic solution"),
                    uiOutput("basicSol"),
                    h3("Problem answer"),
                    uiOutput("shippingProb"),
                    h3("Optimum value"),
                    verbatimTextOutput("OPT"),
                    tags$head(tags$style("#OPT{
                      color: blue;
                      font-size: 20px;
                      font-style: italic;
                       }")),
                    
                    
                
                    
                   
                  )   
                  
                  
                  
                  
         ),
             
  )
)

s <- shinyServer(function(input, output) {                   #this is the server function
  source("QSI.r")            #imports the functions in the different r scripts
  source("SIMPLEX.r")
  #FOR QSI
  output$oid1<-renderPrint({                                  #renders the output for the x values in QSI
    x <- as.numeric(unlist(strsplit(input$vec1,",")))   #extracts the integers from the string
    cat("x values:\n")
    cat(x)
  }
  )
  
  output$oid2<-renderPrint({
    x <- as.numeric(unlist(strsplit(input$vec2,","))) #renders the output for the y values in QSI
    cat("y values:\n")
    cat(x)
  }
  )
  output$oid3<-renderPrint({                               #outputs a warning if length of x is not equal to length of y
    x <- as.numeric(unlist(strsplit(input$vec1,",")))
    y <- as.numeric(unlist(strsplit(input$vec2,",")))
    if(length(x) != length(y)){
      cat("WARNING: The number of x values is not equal to the number of y values!")
    }
  }
  )
  
  output$oid4<-renderPrint({                       #outputs the value to be approximated
    cat("value to be approximated:")
    cat(input$x)
  }
  )
  
  output$oid5<-renderPrint({                              #outputs the approximated value               
    x <- as.numeric(unlist(strsplit(input$vec1,",")))
    y <- as.numeric(unlist(strsplit(input$vec2,",")))
    z = input$x
    data = list (x,y)
    LL = poly.qsi(data,z)
    fxns = LL$qsi.fxns
    if(length(fxns!=0)){                              #prints each expression for the intervals  
      for (i in 1:length(fxns)){
        cat("expression in interval",i,": ")
        cat(fxns[[i]],"\n\n")
      }
      cat("approximated value:",LL$y)             #prints the approximated value
    }
    
  }
  )
  
  #FOR SIMPLEX
  output$initialTab<-renderTable({                 #outputs the initial tableau
    x <- as.numeric(unlist(strsplit(input$vec3,",")))
    mat = matrix(x,nrow=16,ncol=25,byrow=TRUE)
    colnames(mat)=c("S1","S2","S3","s4","s5","s6","s7","s8","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","z","SOLUTION")
    mat
  }
  )
  
  output$finalTableau<-renderTable({ #outputs the final tableau
    
   tryCatch({                                       #tryCatch implemented to display prompt if there is error
     x <- as.numeric(unlist(strsplit(input$vec3,",")))    #extracts the text inputs for the matrix values
     mat = matrix(x,nrow=16,ncol=25,byrow=TRUE)
     colnames(mat)=c("S1","S2","S3","s4","s5","s6","s7","s8","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","z","SOLUTION")
     final_list = simplex(mat,input$choice,input$choice2)
     mat = final_list$final.tableau
     if(length(mat)==0){
       mat = ("NOT AVAILABLE") 
     }
     mat
     
   },
   error = function(e){
     mat = ("NOT AVAILABLE")
   }
   
   )
  }
  )
  
  output$basicSol<-renderTable({  #outputs the basic solution
    tryCatch({x <- as.numeric(unlist(strsplit(input$vec3,",")))
      mat = matrix(x,nrow=16,ncol=25,byrow=TRUE)
      colnames(mat)=c("S1","S2","S3","s4","s5","s6","s7","s8","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","z","SOLUTION")
      final_list = simplex(mat,input$choice,input$choice2)
      basicSol = final_list$basic.solution
      mat2 = matrix(nrow=1,ncol=24)
      colnames(mat2)=c("S1","S2","S3","s4","s5","s6","s7","s8","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","z")
      mat2[1,]=basicSol
      print(mat2)},                                             #creates a matrix with coloumn names for the basic solution
      error = function(e){
        mat = ("NOT AVAILABLE")
      }
    )
    
    
    
  }
  )
  
  output$shippingProb<-renderTable({  #outputs the answer to the shipping problem
     
    if(input$choice2 == FALSE){
      return("NOT AVAILABLE")
    }
    x <- as.numeric(unlist(strsplit(input$vec3,",")))
    mat = matrix(x,nrow=16,ncol=25,byrow=TRUE)
    colnames(mat)=c("S1","S2","S3","s4","s5","s6","s7","s8","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","z","SOLUTION")
    final_list = simplex(mat,input$choice,input$choice2)
    problem = final_list$shipping.num
    rownames(problem)=c("DEN","PHO","DAL")                             #creates a new matrix with rownames for the shipping problem
    problem
    },
    rownames = TRUE
    
  )
  
  output$OPT<-renderPrint({            #outputs the optimum values
    
   tryCatch({
     x <- as.numeric(unlist(strsplit(input$vec3,",")))
     mat = matrix(x,nrow=16,ncol=25,byrow=TRUE)
     colnames(mat)=c("S1","S2","S3","s4","s5","s6","s7","s8","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","z","SOLUTION")
     final_list = simplex(mat,input$choice,input$choice2)   #The bool value from the radio button inputs will be passed to the function parameter of simplex()
     OPT = final_list$opt.val
     cat(OPT)
   },
   error = function(e){
     x = ("NOT AVAILABLE")

   }
   )
    
  }
  
  )

  
}
)
shinyApp(ui = u, server = s)

