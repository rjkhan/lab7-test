#' @title Linear Regression
#' @description  RC class mainpluate linear algeric caluclautions giving formula and data.
#' @field formula Formula
#' @field data A data frame
#' @name ridgereg
#' @export ridgereg
#' @exportClass ridgereg
 
ridgereg<-setRefClass("ridgereg",
                      fields=list(formula="formula", data="data.frame",
                                  lambda="numeric",  beta_ridge="numeric",
                                  datasetName="character",y_hat="numeric"
                                  ),
                      methods= list(
                        initialize = function(formula, data, lambda = 0)
                        {
                          "This function acts as constructor"
                          
                          X <- model.matrix(formula,data)
                          formula <<- formula #formula assign to class formula variable
                          data <<- data #data assign to class data variable
                          lambda <<- lambda #lambda
                        
                          y<-data[,colnames(data)==all.vars(formula)[1], drop=FALSE]
                          
                          #normalize the data if normiliz param is TRUE
                          for(i in 2:ncol(X))
                            X[,i] <- ( X[,i] - mean(X[,i] )) / sd(X[,i] )
                      
                          # QR decompsotion
                          QR_X <- qr(X)
                          Q <- qr.Q(QR_X)
                          R <- qr.R(QR_X)
                          ######
                          mat_Y <- as.matrix(y)
                          
                          #beta_calcultion
                          i_mat <- diag(lambda, nrow = ncol(X))
                          beta_hat <- solve( t(R) %*% R + i_mat) %*% ( t(X) %*% mat_Y)
                          beta_ridge <<- beta_hat[,1]
                          y_hat <<- as.numeric(X %*% beta_ridge)

                          #extract dataset name
                          datasetName <<-  deparse(substitute(data))
                        },
                        predict = function(values_v = NULL)
                        {
                          "This function returns the vector of calculated fitted values"
                          if(!(is.null(values_v)))
                          {
                            values_v <- data.frame( Intercept = 1 , values_v )
                            result <- ( as.matrix(values_v) %*% matrix(beta_ridge, nrow = length(beta_ridge)))
                            return(result[,1])
                          } 
                          return(y_hat)
                        },
                        coef = function()
                        {
                          "Prints out the estimated coefficients"
                          return(beta_ridge)
                        },
                        print = function()
                        {
                          "This function prints the formula and dataset name as well as the calculated coefficients"
                          cat("Call:")
                          cat("\n")
                          formula_print<- paste0("ridgereg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",datasetName,", lambda = ", lambda, ")", "\n", "\n", sep="")
                          cat(formula_print)
                          cat("Coefficients:")
                          cat("\n")
                          beta_ridge
                        }
                      ))

# a<- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length,data=iris, lambda= 2.6)
# a$coef()
# lm.r<- MASS::lm.ridge(Petal.Length ~ Sepal.Width + Sepal.Length,data=iris, lambda= 2.6)
# lm.r$coef
