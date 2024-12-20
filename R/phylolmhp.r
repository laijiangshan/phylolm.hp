#' Hierarchical Partitioning of R2 for Phylogenetic Linear Regression

#' @param  mod  Fitted phylolm or phyloglm model objects.
#' @param  iv  optional the relative importance of predicotr groups will be evaluated. The input for iv should be a list containing the names of each group of variables. The variable names must be the names of the predictor variables in mod.

#' @param  commonality Logical; If TRUE, the result of commonality analysis is shown, the default is FALSE. 

#' @details This function conducts hierarchical partitioning to calculate the individual contributions of phylogenetic signal and each predictor towards total R2 from rr2 package for phylogenetic linear regression. 

#' @return \item{Total.R2}{The R2 for the full model.}
#' @return \item{commonality.analysis}{If commonality=TRUE, a matrix containing the value and percentage of all commonality (2^N-1 for N predictors or matrices).}
#' @return \item{Individual.R2}{A matrix containing individual effects and percentage of individual effects for phylogenetic tree and each predictor}

#' @author {Jiangshan Lai} \email{lai@njfu.edu.cn}



#' @references
#' \itemize{
#' \item Lai J.,Zhu W., Cui D.,Mao L.(2023)Extension of the glmm.hp package to Zero-Inflated generalized linear mixed models and multiple regression.Journal of Plant Ecology,16(6):rtad038<DOI:10.1093/jpe/rtad038>
#' \item Lai J.,Zou Y., Zhang S.,Zhang X.,Mao L.(2022)glmm.hp: an R package for computing individual effect of predictors in generalized linear mixed models.Journal of Plant Ecology,15(6):1302-1307<DOI:10.1093/jpe/rtac096>
#' \item Lai J.,Zou Y., Zhang J.,Peres-Neto P.(2022) Generalizing hierarchical and variation partitioning in multiple regression and canonical analyses using the rdacca.hp R package.Methods in Ecology and Evolution,13(4):782-788<DOI:10.1111/2041-210X.13800>
#' \item Chevan, A. & Sutherland, M. (1991). Hierarchical partitioning. American Statistician, 45, 90-96. doi:10.1080/00031305.1991.10475776
#' \item Nimon, K., Oswald, F.L. & Roberts, J.K. (2013). Yhat: Interpreting regression effects. R package version 2.0.0.
#' \item Nimon, Ho, L. S. T. and Ane, C. 2014. "A linear-time algorithm for Gaussian and non-Gaussian trait evolution models". Systematic Biology 63(3):397-408.
#' }

#'@export
#'@examples
#'library(phylolm)
#'library(rr2)
#'set.seed(231)
#'tre <- rcoal(60)
#'taxa <- sort(tre$tip.label) 
#'b0 <- 0      
#'b1 <- 0.3    
#'b2 <- 0.5 
#'b3 <- 0.4
#'x <- rTrait(n=1, phy=tre, model="lambda", parameters=list(ancestral.state=0, sigma2=15, lambda=0.9))          
#'x2 <- rTrait(n=1, phy=tre, model="lambda",  
#'parameters=list(ancestral.state=0, sigma2=10, lambda=0.9))  
#'x3 <- rTrait(n=1, phy=tre, model="lambda",  
#'parameters=list(ancestral.state=0, sigma2=13, lambda=0.9)) 
        
#'y <- b0 + b1 * x + b2 * x2 + b3*x3+ rTrait(n=1, phy=tre, model="lambda",
#'parameters=list(ancestral.state=0, sigma2=5, lambda=0.9))            
#'dat <- data.frame(trait=y[taxa], pred=x[taxa], pred2=x2[taxa],pred3=x3[taxa])

#'fit <- phylolm(trait ~ pred + pred2 + pred3, data=dat, phy=tre, model="lambda")
#'phylolm.hp(fit,commonality=TRUE)
#'iv=list(env1="pred",env2=c("pred2","pred3"))
#'phylolm.hp(fit,iv)


#'set.seed(123456)
#'tre <- rtree(50)
#'x1 <- rTrait(n=1, phy=tre)  
#'x2 <- rTrait(n=1, phy=tre)
#'x3 <- rTrait(n=1, phy=tre)
#'X <- cbind(rep(1, 50), x1, x2, x3)
#'y <- rbinTrait(n=1, phy=tre, beta=c(-1, 0.9, 0.9, 0.5), alpha=1, X=X)
#'dat <- data.frame(trait01=y, predictor1=x1, predictor2=x2, predictor3=x3)
#'fit <- phyloglm(trait01 ~ predictor1 + predictor2 + predictor3, phy=tre, data=dat)
#'phylolm.hp(fit)
#'iv=list(env1="predictor1",env2=c("predictor2","predictor3"))
#'phylolm.hp(fit,iv)

phylolm.hp <- function(mod,iv=NULL,commonality=FALSE) 
{
phyloglm.hp(mod=mod,iv=iv,commonality=commonality)  
}





