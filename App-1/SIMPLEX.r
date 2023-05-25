



simplex<-function(tableau,isMax,problem){  #functin for the simplex method
  
  
  
  while(any(tableau[nrow(tableau),]<0)){                           #checks if there are still negtaive numbers in the last row
    basicSol = c()
    lastCol=tableau[,ncol(tableau)]                        #gets the last coloumn and the last row
    lastRow=tableau[nrow(tableau),1:ncol(tableau)-1]
    colIn = which(lastRow == min(lastRow),arr.ind = TRUE)     #gets the smallest  value in the last row and designates the pivot col
    pivotCol=tableau[,colIn]
    TR = lastCol[-length(lastCol)]/pivotCol[-length(pivotCol)] #gets the test ratio
    least = min(TR[TR>=0])
    pivotInd = which(TR == least)                             #gets the row index of the tableau with the least TR
    pivotElement=tableau[pivotInd[[1]],colIn]                  #gets the pivot element     
    pivotRow = tableau[pivotInd[[1]],]                          #gets the pivot row    
    npr = pivotRow/pivotElement                                 #gets the normalized pivot row
    tableau[pivotInd[1],]=npr                                 #assigne the npr to the pivot row
    for(i in 1:nrow(tableau)){
      if(i==pivotInd[1]){                    #checks if the current row is the pivot row, if it is theen the row is skipped
        next
      }
      else{
        C = tableau[i,colIn]                    #if it is not then apply the operation with the npr
        tableau[i,]=tableau[i,]-(npr*C)
      }
      
    }
    
  }
  if (isMax == TRUE){               #gets the value of z and the variables based on isMAX
    for(i in 1:ncol(tableau)){
      
      
      if(any(1 == tableau[,i]) && (sum(tableau[,i]==0)==nrow(tableau)-1)){   #gets the solution value based on which coulumn only has 1 value
        
        
        row = which(tableau[,i] == 1)
        basicSol[i]= tableau[row[[1]],ncol(tableau)]
        
      }
      
    }
  }
  else{           
    
    for(i in 1:(ncol(tableau)-1)){
      basicSol[i]=tableau[nrow(tableau),i]          #directly gets the value in the last row for the variable
      
    }
    basicSol[length(basicSol)]=tableau[nrow(tableau),"SOLUTION"]
    
  }
  
  
  names(basicSol)=colnames(tableau[,1:ncol(tableau)-1]) 
  basicSol[is.na(basicSol)]=0
  
  
  if(problem == TRUE){                          
    matrix1=matrix(nrow=3,ncol=5)
    colnames(matrix1)=c("SAC","SL","ALB","CHI","NYC")                  #creates the matrix for the shipping problem by putting the rownames, colnames, and matrix size
    rownames(matrix1)=c("DEN","PHO","DAL")
    index=9
    for(i in 1:nrow(matrix1)){
      for(j in 1:ncol(matrix1)){
        matrix1[i,j]=basicSol[index]
        index=index+1
      }
    }
    final.list = list(final.tableau = tableau,basic.solution=basicSol,opt.val=basicSol[length(basicSol)],shipping.num=matrix1)
    return(final.list)
  }
  else{
    final.list = list(final.tableau = tableau,basic.solution=basicSol,opt.val=basicSol[length(basicSol)])
    return(final.list)
  }
  #returns the final labelled list
  
  
}

a =c(-1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,-1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8, -1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,6,-1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,5,-1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,4,
     0,-1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,6,
     0,-1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,5,
     0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,4,
     0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,3,
     0,-1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,6,
     0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,3,
     0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,4,
     0,0,-1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,5,
     0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,5,
     0,0,-1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,9,
     310,260,280,-180,-80,-200,-160,-220,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
print(length(a))
mat = matrix(a,nrow=16,ncol=25,byrow=TRUE)
colnames(mat)=c("S1","S2","S3","s4","s5","s6","s7","s8","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","z","SOLUTION")
