#' Hierarchical Partitioning of R2 for Phylogenetic Generalized Linear Regression for 

#' @param  mod  Fitted phyloglm model objects.

#' @details This function conducts hierarchical partitioning to calculate the individual contributions of phylogenetic signal and each predictor towards total R2 from rr2 package for phylogenetic generalized linear regression. 

#' @return \item{Total.R2}{The R2 for the full model.}
#' @return \item{Individual.R2}{A vector containing individual R2 for each predictor and phylogenetic tree.}

#' @author {Jiangshan Lai} \email{lai@njfu.edu.cn}



#' @references
#' \itemize{
#' \item Lai J.,Zhu W., Cui D.,Mao L.(2023)Extension of the glmm.hp package to Zero-Inflated generalized linear mixed models and multiple regression.Journal of Plant Ecology,16(6):rtad038<DOI:10.1093/jpe/rtad038>
#' \item Lai J.,Zou Y., Zhang S.,Zhang X.,Mao L.(2022)glmm.hp: an R package for computing individual effect of predictors in generalized linear mixed models.Journal of Plant Ecology,15(6):1302-1307<DOI:10.1093/jpe/rtac096>
#' \item Lai J.,Zou Y., Zhang J.,Peres-Neto P.(2022) Generalizing hierarchical and variation partitioning in multiple regression and canonical analyses using the rdacca.hp R package.Methods in Ecology and Evolution,13(4):782-788<DOI:10.1111/2041-210X.13800>
#' \item Chevan, A. & Sutherland, M. (1991). Hierarchical partitioning. American Statistician, 45, 90-96. doi:10.1080/00031305.1991.10475776
#' \item Nimon, K., Oswald, F.L. & Roberts, J.K. (2013). Yhat: Interpreting regression effects. R package version 2.0.0.
#' \item Ives, A. R. and T. Garland, Jr.(2010). "Phylogenetic logistic regression for binary dependent variables". Systematic Biology 59:9-26.
#' }

#'@export
#'@examples

#'library(phylolm)
#'set.seed(123456)
#'tre <- rtree(50)
#'x1 <- rTrait(n=1, phy=tre)  
#'x2 <- rTrait(n=1, phy=tre)
#'X <- cbind(rep(1, 50), x1, x2)
#'y <- rbinTrait(n=1, phy=tre, beta=c(-1, 0.5, 0.8), alpha=1, X=X)
#'dat <- data.frame(trait01=y, predictor1=x1, predictor2=x2)
#'fit <- phyloglm(trait01 ~ predictor1 + predictor2, phy=tre, data=dat)
#'phyloglm.hp(fit)


phyloglm.hp <- function(mod) 
{
  # initial checks
  if (!inherits(mod, "phyloglm")) stop("phyloglm.hp only supports phyloglm objects at the moment")

  Formu <- strsplit(as.character(mod$call$formula)[3],"")[[1]]
  if("*"%in%Formu)stop("Please put the interaction term as a new variable (i.e. link variables by colon(:)) and  avoid the asterisk (*) and colon(:) in the original model")
  #if phylolm，not use orgianl ivname
  #if('phylolm' %in% class(mod)) ivname <- str_split(str_split(as.character(mod$call$formula)[2] ,'~')[[1]][2] ,'[/+]')[[1]] else ivname <- strsplit(as.character(mod$call$formula)[3],"[+]")[[1]] 
  #try delete call in mod$call$formula, if some question
  #if('phylolm' %in% class(mod)) ivname <- str_split(as.character(mod$formula)[3] ,'[/+]')[[1]] else ivname <- strsplit(as.character(mod$call$formula)[3],"[/+]")[[1]] 
  
  if(!is.null(attr(mod$terms, "offset")))
  {ivname <- strsplit(as.character(mod$formula)[3],"[/+]")[[1]][-(attr(mod$terms, "offset")+1)]} 
  if(is.null(attr(mod$terms, "offset")))
  {ivname <- strsplit(as.character(mod$formula)[3],"[/+]")[[1]]} 
  
  
  iv.name <- ivname
  nvar <- length(iv.name)
if(nvar>1)
{
 #  stop("Analysis not conducted. Insufficient number of predictors.")

  totalN <- 2^nvar - 1
  binarymx <- matrix(0, nvar, totalN)
  for (i in 1:totalN) {
    binarymx <- creatbin(i, binarymx)
  }

#if(type=="adjR2")outr2  <- summary(mod)$r.sq
#if(type=="dev")outr2  <- summary(mod)$dev.expl
outr2  <- rr2::R2_lik(mod)


r2type  <-  row.names(outr2)
nr2type   <-  length(r2type)
if(nr2type==0)
{nr2type <- 1
#if(commonality)
#{r2type <- 'commonality.analysis'}
#else
#{r2type <- 'hierarchical.partitioning'}
}


if(inherits(mod,"phyloglm"))
{dat <- eval(mod$call$data)
 #phy0 <- eval(mod$call$phy)

if(!inherits(dat, "data.frame")){stop("Please change the name of data object in the original phylolm analysis then try again.")}
#if('phylolm' %in% class(mod)) to_del <- paste(str_split(as.character(mod$call$formula)[2],'~')[[1]][1],"~","1") else to_del <- paste(as.character(mod$call$formula)[2],"~","1")
to_del <- paste(paste("-", iv.name, sep= ""), collapse = " ")
  # reduced formula
  modnull<- stats::update(stats::formula(mod), paste(". ~ . ", to_del, sep=""))
  mod_null <-  glm(modnull,data = dat,family="binomial")

#if('phylolm' %in% class(mod)) to_del <- paste(as.character(mod$formula)[2],"~","1") else to_del <- paste(as.character(mod$call$formula)[2],"~","1")
 }


outputList  <- list()
outputList[[1]] <- outr2
for (k in 1:nr2type)
{
  commonM <- matrix(nrow = totalN, ncol = 3)
  for (i in 1:totalN) {
    tmp.name <- iv.name[as.logical(binarymx[, i])]
	
	to_add <- paste("~", paste(c(tmp.name, if (!is.null(mod$offset)) as.character(attr(mod$terms, "variables")[attr(mod$terms, "offset") + 1])), collapse = " + "))
      modnew <- stats::update(object = mod_null, data = dat, formula = as.formula(to_add))

   #to_add <- paste("~",paste(tmp.name,collapse = " + "),sep=" ")
   # modnew  <- stats::update(object = mod_null, data = dat,to_add) 
    #if(type=="dev")commonM[i, 2]  <- summary(modnew)$dev.expl
	#if(type=="adjR2")commonM[i, 2]  <- summary(modnew)$r.sq
	commonM[i, 2]  <- rr2::R2_lik(modnew)
  }

  commonlist <- vector("list", totalN)

  seqID <- vector()
  for (i in 1:nvar) {
    seqID[i] = 2^(i-1)
  }


  for (i in 1:totalN) {
    bit <- binarymx[1, i]
    if (bit == 1)
      ivname <- c(0, -seqID[1])
    else ivname <- seqID[1]
    for (j in 2:nvar) {
      bit <- binarymx[j, i]
      if (bit == 1) {
        alist <- ivname
        blist <- genList(ivname, -seqID[j])
        ivname <- c(alist, blist)
      }
      else ivname <- genList(ivname, seqID[j])
    }
    ivname <- ivname * -1
    commonlist[[i]] <- ivname
  }

  for (i in 1:totalN) {
    r2list <- unlist(commonlist[i])
    numlist  <-  length(r2list)
    ccsum  <-  0
    for (j in 1:numlist) {
      indexs  <-  r2list[[j]]
      indexu  <-  abs(indexs)
      if (indexu != 0) {
        ccvalue  <-  commonM[indexu, 2]
        if (indexs < 0)
          ccvalue  <-  ccvalue * -1
        ccsum  <-  ccsum + ccvalue
      }
    }
    commonM[i, 3]  <-  ccsum
  }

  orderList <- vector("list", totalN)
  index  <-  0
  for (i in 1:nvar) {
    for (j in 1:totalN) {
      nbits  <-  sum(binarymx[, j])
      if (nbits == i) {
        index  <-  index + 1
        commonM[index, 1] <- j
      }
    }
  }

  outputcommonM <- matrix(nrow = totalN + 1, ncol = 2)
  totalRSquare <- sum(commonM[, 3])
  for (i in 1:totalN) {
    outputcommonM[i, 1] <- round(commonM[commonM[i,
                                                 1], 3], digits = 4)
    outputcommonM[i, 2] <- round((commonM[commonM[i,
                                                  1], 3]/totalRSquare) * 100, digits = 2)
  }
  outputcommonM[totalN + 1, 1] <- round(totalRSquare,
                                        digits = 4)
  outputcommonM[totalN + 1, 2] <- round(100, digits = 4)
  rowNames  <-  NULL
  for (i in 1:totalN) {
    ii  <-  commonM[i, 1]
    nbits  <-  sum(binarymx[, ii])
    cbits  <-  0
    if (nbits == 1)
      rowName  <-  "Unique to "
    else rowName  <-  "Common to "
    for (j in 1:nvar) {
      if (binarymx[j, ii] == 1) {
        if (nbits == 1)
          rowName  <-  paste(rowName, iv.name[j], sep = "")
        else {
          cbits  <-  cbits + 1
          if (cbits == nbits) {
            rowName  <-  paste(rowName, "and ", sep = "")
            rowName  <-  paste(rowName, iv.name[j], sep = "")
          }
          else {
            rowName  <-  paste(rowName, iv.name[j], sep = "")
            rowName  <-  paste(rowName, ", ", sep = "")
          }
        }
      }
    }
    rowNames  <-  c(rowNames, rowName)
  }
  rowNames  <-  c(rowNames, "Total")
  rowNames <- format.default(rowNames, justify = "left")
  colNames <- format.default(c("Fractions", " % Total"),
                             justify = "right")
  dimnames(outputcommonM) <- list(rowNames, colNames)

  VariableImportance <- matrix(nrow = nvar, ncol = 4)
# VariableImportance <- matrix(nrow = nvar, ncol = 2)
  for (i in 1:nvar) {
	VariableImportance[i, 3] <-  round(sum(binarymx[i, ] * (commonM[,3]/apply(binarymx,2,sum))), digits = 4)
	#VariableImportance[i, 1] <-  round(sum(binarymx[i, ] * (commonM[,3]/apply(binarymx,2,sum))), digits = 4)
  }
  
  VariableImportance[,1] <- outputcommonM[1:nvar,1]
  VariableImportance[,2] <- VariableImportance[,3]-VariableImportance[,1]
  
  total=round(sum(VariableImportance[,3]),digits = 4)
  #total=round(sum(VariableImportance[,1]),digits = 4)
  VariableImportance[, 4] <- round(100*VariableImportance[, 3]/total,2)
#VariableImportance[, 2] <- round(100*VariableImportance[, 1]/total,2)
 #dimnames(VariableImportance) <- list(iv.name, c("Individual","I.perc(%)"))
 dimnames(VariableImportance) <- list(iv.name, c("Unique","Average.share","Individual","I.perc(%)"))
  
#if(commonality)
#{outputList[[k+1]]<-outputcommonM}

#else
#{outputList[[k+1]]<-VariableImportance}

outputList[[k+1]]<- c(VariableImportance[,"Individual"],phytree=outr2-sum(VariableImportance[,"Individual"]))

}
}

if(nvar==1)
{
outr2  <- rr2::R2_lik(mod)


r2type  <-  row.names(outr2)
nr2type   <-  length(r2type)
if(nr2type==0)
{nr2type <- 1
#if(commonality)
#{r2type <- 'commonality.analysis'}
#else
#{r2type <- 'hierarchical.partitioning'}
}


if(inherits(mod,"phyloglm"))
{dat <- eval(mod$call$data)
 #phy0 <- eval(mod$call$phy)

if(!inherits(dat, "data.frame")){stop("Please change the name of data object in the original phylolm analysis then try again.")}
#if('phylolm' %in% class(mod)) to_del <- paste(as.character(mod$formula)[2],"~","1") else to_del <- paste(as.character(mod$call$formula)[2],"~","1")
 }


outputList  <- list()
outputList[[1]] <- outr2

for (k in 1:nr2type)
{
dat <- eval(mod$call$data)
Individual <- rr2::R2_lik(glm(formula(mod),data = dat,family="binomial"))

outputList[[k+1]]<- c(Individual,phytree=outr2-Individual)
names(outputList[[k+1]])<-c(iv.name,"phytree")
}

}

#if(type=="adjR2"){names(outputList) <- c("adjusted.R2",r2type)}
#if(type=="dev"){names(outputList) <- c("Explained.deviance",r2type)}
#if(inherits(mod, "lm")&!inherits(mod, "glm")){names(outputList) <- c("Total.R2",r2type)}
#outputList$variables <- iv.name
#if(commonality){outputList$type="commonality.analysis"}
#if(!commonality){outputList$type="hierarchical.partitioning"}
names(outputList) <- c("Total.R2","Individual.R2")
class(outputList) <- "phyloglmhp" # Class definition
outputList
}
