colGetter<-function (system){                   #function for getting the length of the coloumns
  max=1
  for(i in 1:length(system)){
    dep=deparse(system[[i]])
    spl=strsplit(dep[2],"\\+")              #deparses system and gets the max of the length of each list
    if (length(spl[[1]])>=max){
      max=length(spl[[1]])
    }
  }
  return(length(spl[[1]]))                   #returns the length as the coloumn size
}

varNumberchcker<-function(system){              #function for checking if the number of unknown variables are equal
  variablescount=c()
  for(i in 1:length(system)){
    dep=deparse(system[[i]])                      #deparses each list in system using the comma
    spl=strsplit(dep[1],"\\,")                     #the length of the deparsed list will be stored in a vector  
    variablescount[i]=length(spl[[1]])
  }
  if (var(variablescount)==0){               #this checks if there are any difference in the values
    return (TRUE)
  }
  else{
    return(FALSE)                    #returns true if var==0, else return false
  }
}



assignName<-function(matrix, system){                  #assigns the col name for each coloumn of the matrix
  variables=c()
  for(i in 1:length(system)){
    dep=deparse(system[[i]])
    spl=strsplit(dep[2],"\\+")  
    for(j in 1:(length(spl[[1]])-1)){                     
      spl2=strsplit(spl[[1]][j],"\\*")              #splits the each list with + and *, and puts the variable name in the vector variable
      variables[j]=spl2[[1]][2]                    #stores the extracted coloumn name in the variables vector
    }  
  }
  
  return(variables)                                   #returns the vector variable
}

extractLastCol<-function(system){                       #this extracts the last coloumn of the matrix
  mat=matrix(nrow=length(system),ncol=1)
  for(i in 1:length(system)){
    dep=deparse(system[[i]])                            
    spl=strsplit(dep[2],"\\+")                               
    spl2=strsplit(spl[[1]][colGetter(system)],"\\*")     #only extracts the last object from the double deparsed list
    mat[i,1]=as.numeric(spl2)*-1                        #puts that object converted to numeric and multiplied to -1 in an nx1 matrix since this is only one coloumn(end coloumn)
  }  
  colnames(mat)="RHS"                                   #names the coloumn "RHS"
  return(mat)                                           #returns the matrix
}


AugCoeffMatrix<-function(system){                         
  if(varNumberchcker(system)==FALSE){                               #returns NA if varNumber checker returns FALSE
    return (NA) 
  }
  mat=matrix(nrow=length(system),ncol=colGetter(system)-1)            #sets the ncol and nrow of the matrix
  colnames(mat)=assignName(mat,system)                                #starts naming the coloumns of the matrix
  for(i in 1:length(system)){
    dep=deparse(system[[i]])                                               #deparses each list in system and separates the objects by the separator + and *
    spl=strsplit(dep[2],"\\+")                                              
    for(j in 1:(length(spl[[1]])-1)){                                    # nested for loops on the list to get the desired single value
      spl2=strsplit(spl[[1]][j],"\\*")
      mat[i, spl2[[1]][2] ] = ((as.numeric(spl2[[1]][1])))            #puts the numeric version of the extracted value to its corresponding coloumn based on the deparsed and split variable
    }  
  }
  rownames(mat)=1:length(system)                              #sets the name of the rows
  mat=cbind(mat,extractLastCol(system))                      #appends the last coloumn named RHS to mat
  finalList=list(augcoeffmatrix=(mat),variables=(c(colnames(mat)[1:length(colnames(mat))-1]))) #puts everything in a labelled list
  
  return(finalList)                         #returns labelled list
}


pivot<-function(mat,x){
  
  mat=mat[order(abs(mat[,x]), decreasing = TRUE),]
  return(mat)
}



GaussianElimination<-function(mat){      #function for gaussion elimination
  y=matrix()                                  #gets the matrix from the augcoeff function               
  n=nrow(mat)
  asMat=mat                        
  for(i in 1:(n-1)){                                #iterates through n-1
    tempMat=matrix()
    asMat[i:n,]=pivot(asMat[i:n,],i)                    #pivots the current pivot row according to i
    if(asMat[i,i]==0){                                #if pivot element is 0 returns null
      return(NA)
    }
    
    for(j in (i+1):n){                                 #nested for loop for getting the multiplier
      pivotElement=asMat[i,i]
      if(pivotElement == 0){return (NULL)}
      del=asMat[j,i]                                     #gets the index to be deleted
      multiplier=del/pivotElement
      tempMat=multiplier*asMat[i,]                            #stores the multiplierXpivotRow in a matrix
      asMat[j,]=(round(asMat[j,],4))-(round(tempMat,4))       #subtracts the matrix to the pivoted matrix based on index j
    }
    
  } 
  
  y[ncol(asMat)-1]=asMat[nrow(asMat),ncol(asMat)]/asMat[nrow(asMat),ncol(asMat)-1]      #for backward substitution. Gets the value of X3 and stores i in an index
  for (i in (n - 1): 1){
    y[i] = (asMat[i,n+1] - sum(asMat[i, (i+1):n] * y[(i+1):n])) / asMat[i,i]         #gets the other unknowns
  }
  return(y)                                        #stores the matrix, solution set, and variables in a labelled list
} #returns the labelled list




getVals<-function(data,index){         #function that gets the coeffecient values per x values in the quadratic equation
  val=c()
  val[1]=data[[1]][index]*data[[1]][index]
  val[2]=data[[1]][index]
  val[3]=1
  val[4]=data[[2]][index]
  return(round(val,digits = 2))            #returns the values as a vector
}

getVals2<-function(data,index){ #returns the coeffecient value in the derivative of the interior knots formula
  val=c()
  val[1]=data[[1]][index]*2
  val[2]=1
  val[3]=0
  val[4]=(data[[1]][index]*2)*-1
  val[5]=1*-1
  val[6]=0
  
  return(round(val,digits = 2)) #returns the values as a vector
}



internalMatrix<-function(data){            #matrix for the internal knots
  
  knots=c()                               
  startCol=0
  index=2
  
  points=length(data[[1]])
  a=matrix(nrow=((points-2)*2),ncol=(3*(points-1))+1)
  
  for(i in 1:nrow(a)){
    knots[1:4]=getVals(data, index) #puts the necessary value of the coeffs in the knots vector
    for(j in 1:3){       #iteration to put the values in the knots inside the matrix
      a[i,startCol+j]=knots[j]
      a[i,ncol(a)]=knots[4]       #puts the last value in the last coloumn 
    }
    startCol=startCol+3        #skips 3 coloumns to start putting the next coeffs there
    if(i%%2==0){                 #if there has already been two iterations of i, then the coloumn will reset and a new value from the internal knots will be used
      startCol=startCol-3
      index=index+1             
    } 
  }
  
  
  return(a)     #returns the internal matrix
  
  
}


externalMatrix<-function(data){                              #this creates the external knots matrix
  val1=c()
  val2=c()
  points=length(data[[1]])                 
  val1=getVals(data,1)                     #gets the necessary coeffs to place in the matrix based on the value of the external knots
  val2=getVals(data,length(data[[2]]))
  b=matrix(nrow=2,ncol=(3*(points-1))+1)
  b[1,1:3]=val1[1:3]                      #puts the computed coeff value of the external knots in the designated index of the matrix
  b[1,ncol(b)]=val1[4]                    #puts the last value in the last coloumn
  b[2,(ncol(b)-3):(ncol(b)-1)]=val2[1:3]  #similar but for the last last x value
  b[2,ncol(b)]=val2[4]
  return(b)                        #returns the external matrix
}





derMatrix<-function(data){      #creates the matrix for the derivative equation of the interior knots                   
  knots=c()
  startCol=0
  index=2
  
  points=length(data[[1]])        
  a=matrix(nrow=(points-2),ncol=(3*(points-1))+1)
  
  for(i in 1:nrow(a)){  #iterates through the rows of the created matrix  
    knots[1:6]=getVals2(data, index) #gets the necessary coeffs and puts them on a vertex
    
    for(j in 1:6){            #puts the computed coeffs in the matrix
      a[i,startCol+j]=knots[j]
      
    }
    startCol=startCol+3 #skips 3 coloumns after putting the vertex coeff values in the matrix
    index=index+1  #moves on to the next interior knot value
  } 
  return(a) #returns the matrix
}


poly.qsi<-function(data,x){                #required function
  tryCatch({
    mat=rbind(internalMatrix(data),externalMatrix(data),derMatrix(data)) #binds all the matrices created
    functions=list()                                                    #creates a list of functions
    final_mat=mat[,-1]                                         #since a1=0, the first coloumn is removed
    final_mat[is.na(final_mat)]=0                          #turns all NA to 0
    list=GaussianElimination(final_mat)                     #performs gaussian elimination to the final matrix and puts the solved values in a list
    final_list=append(0,list)                       #appends 0 to the start of the list for a1
    index=0
    index2=1                                   
    intervals=length(data[[1]])-1
    for(j in 1:intervals){ #iterates through the number of intervals
      functions[index2]=paste(round(final_list[j+index],digits = 2),"*x1^2+",round(final_list[j+1+index],digits = 2),"*x2+",round(final_list[j+2+index],digits = 2),sep="")#this creates a string of functions per interval
      index=index+2           
      index2=index2+1
    }
    
    for(i in 1:(length(data[[1]])-1)){              #checks which interval the x value in the parameter belongs to, and places that value in a variable
      if(x>data[[1]][i]&&x<data[[1]][i+1]){
        index=i
      }
      else{
        
      }
    }
    d=paste("function(x1,x2)",functions[[index]])    #setup for the evaluation function. The function is chosen based on the value of index
    funct=eval(parse(text=d))          
    labelled_list=list(qsi.fxns=functions,y=round(funct(x,x),digits = 2)) #the labelled list for the value of funct's evaluation and the polynomials for each interval
    
    return(labelled_list) #returns the labelled list
  },
  error = function(e){         
    cat("UNABLE TO APPROXIMATE USING GIVEN VALUES.\n\nREASON:\n\n")
    print(e)
    
  }
  
  
  
  )
}

x = c(3.0,4.5,7.0,9.0)
y = c(2.5,1.0,2.5,0.5)
data1 = list(x,y)