#' @title Linear Regression
#' @description  RC class mainpluate linear algeric caluclautions giving formula and data.
#' @field formula Formula
#' @field data A data frame
#' @name ridgereg
#' @export ridgereg
#' @exportClass ridgereg

ridgereg <- setRefClass("ridgereg",
                      fields=list(formula="formula",lambda="numeric",
                                  data="data.frame",
                                  datasetName="character",
                                  beta_coef="numeric",
                                  y_hat="numeric"),
                      methods= list(

                        initialize = function(formula, data, lambda = 0){

                          formula <<- formula #formula assign to class formula variable
                          data <<- data #data assign to class data variable
                          lambda <<- lambda #lambda

                          X <- model.matrix(formula,data)
                          Y <- data[[(all.vars(formula)[1])]]

                          #normalize the data if normiliz param is TRUE
                          for(i in 2:ncol(X))
                            X[,i] <- ( X[,i] - mean(X[,i] )) / sd(X[,i] )

                          #Qr decompistion
                          QR_X <- qr(X)
                          QR_R <- qr.R(QR_X)

                          I_mat <- matrix(c(0),nrow = ncol(X),ncol = ncol(X)) #create a identity Matrix
                          diag(I_mat) <- lambda #update diagnal of lambda matrix of I_mat
                          mat_Y <- as.matrix(Y) #convert into martix

                          beta_ridge <- solve(( (t(QR_R) %*% QR_R) + I_mat)) %*% (t(X) %*% mat_Y )

                          beta_coef <<- beta_ridge[,1]
                          y_hat <<- as.numeric(X %*% Coef)

                        },

                        print = function(){
                          "This function prints the formula and dataset name as well as the calculated coefficients"
                          r_name <- rownames(as.data.frame(Coef))
                          cat("Call:")
                          cat("\n")
                          formula_print<- paste0("ridgereg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",datasetName,")",sep="")
                          cat(formula_print)
                          cat("\n")
                          cat("\n")
                          cat("Coefficients:")
                          cat("\n")
                          cat(" ")
                          cat(r_name)
                          cat(" ")
                          cat("\n")
                          cat(beta_coef)
                          cat("\n")
                          cat("\n")
                        },

                        predict = function(newdata=NULL){
                          "This function returns the vector of calculated fitted values"
                          if(!is.null(newdata))
                          {
                            beta_mat <- matrix(beta_coef, nrow=length(beta_coef))
                            newdata <- as.matrix(data.frame(Intercept=1,newdata))
                            new_y_hat <- as.numeric((newdata %*% beta_mat))
                            return(new_y_hat)
                          }
                          return(y_hat)
                        },

                        coef = function(){
                          "This function returns the vector of beta coefficients  "
                          return(beta_coef)
                        }
                      ))

