#' @title Linear Regression
#' @description  RC class mainpluate linear algeric caluclautions giving formula and data.
#' @field formula Formula
#' @field data A data frame
#' @name ridgereg
#' @export ridgereg
#' @exportClass ridgereg

ridgereg <- setRefClass("ridgereg",
                        fields = list(formula="formula",data="data.frame",
                                      lambda="numeric",datasetName="character",
                                      beta_ridge="matrix", y_hat = "numeric",
                                      ridge_coef="numeric"),
                        methods = list(
                          initialize= function(formula,data,lambda = 0, normalize = FALSE)
                          {
                            "This function acts as constructor"

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
                            beta_ridge <<- solve(( (t(QR_R) %*% QR_R) + I_mat)) %*% (t(X) %*% mat_Y )



                            y_hat <<- as.numeric(X %*% beta_ridge)    #y_hat calculate

                            ridge_t <- as.numeric(beta_ridge)
                            names(ridge_t) <- rownames(beta_ridge)

                            ridge_coef <<- ridge_t[-1]
                            #extract dataset name
                            datasetName <<-  deparse(substitute(data))

                          },
                          predict =  function()
                          {
                            "This function returns the vector of calculated fitted values"
                            return(y_hat)
                          },
                          coef = function()
                          {
                            "This function returns the vector of beta coefficients  "
                            return(ridge_coef)
                          },

                          print = function()
                          {
                            "This function prints the formula and dataset name as well as the calculated coefficients"
                            r_name <- rownames(as.data.frame(ridge_coef))
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
                            cat(ridge_coef)
                            cat("\n")
                            cat("\n")

                          }


                        )
)

# a<- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length,data=iris, lambda= 2.6)
# a$coef()
# lm.r<- MASS::lm.ridge(Petal.Length ~ Sepal.Width + Sepal.Length,data=iris, lambda= 2.6)
# lm.r$coef
