library(Matrix)
library(MASS)
library(ggplot2)
library(cowplot)
library(viridis)
library(lme4qtl) 
library(matlm) 
library(wlm) 
library(RNOmni)
library(parallel)
library(tictoc)
library(dplyr)
options(scipen = 999)
cores.available = 30 #Set this number to the amount of cores available. For Windows systems 1 is recommended!

matlm_pred3 <- function (x, ind = NULL, num_batches = 1, batch_size = NULL, 
                         path_pred = ".", ...) 
{
  stopifnot(!any(duplicated(ind)))
  out <- list(path = path_pred)
  # if (class(x) == "matrix") {
  # if (T) {
  if (any(class(x) == "matrix")) {
    out$data <- x
    oldClass(out) <- c("matlmPredMat", "matlmPred")
  }
  else if (class(x) == "big.matrix") {
    out$data <- describe(x)
    oldClass(out) <- c("matlmPredBigMat", "matlmPred")
  }
  else if (class(x) == "big.matrix.descriptor") {
    out$data <- x
    oldClass(out) <- c("matlmPredBigMat", "matlmPred")
  }
  else {
    stop("not supported class")
  }
  if (is.null(ind)) {
    ind <- seq(1, pred_nrow(out))
  }
  out$ind <- ind
  out$complete <- (length(ind) == pred_nrow(out))
  cnames <- pred_colnames(out)
  if (is.null(cnames)) {
    out$cnames <- as.character(paste0("pred", seq(1, pred_ncol(out))))
    out$null_cnames <- TRUE
  }
  else {
    out$cnames <- cnames
    out$null_cnames <- FALSE
  }
  if (num_batches > pred_ncol(out)) {
    warning("num_batches > pred_ncol: force num_batches <- pred_ncol")
    num_batches <- pred_ncol(out)
  }
  if (is.null(batch_size)) {
    beg <- seq(1, pred_ncol(out), length = num_batches) %>% 
      floor
    end <- c(beg[-1] - 1, pred_ncol(out))
  }
  else {
    beg <- seq(1, pred_ncol(out), by = batch_size)
    end <- c(beg[-1] - 1, pred_ncol(out))
  }
  stopifnot(all(beg <= pred_ncol(out)))
  stopifnot(all(end <= pred_ncol(out)))
  out$num_batches <- length(beg)
  out$batch_size <- batch_size
  out$beg <- beg
  out$end <- end
  return(out)
}

matlm2 <- function (formula, data, ..., varcov = NULL, transform = NULL, 
                    ids = NULL, pred, int, num_batches = 1, batch_size = NULL, 
                    path_pred = ".", num_perm = 0, seed_perm, returnRespOrth = FALSE, 
                    returnPredOrth = FALSE, returnPredOrthSc = FALSE, stats_full = FALSE, 
                    cores = 1, verbose = 0) 
{
  tic.clearlog()
  tic("matlm")
  tic("args")
  mc <- match.call()
  env <- parent.frame(1)
  missing_transform <- missing(transform)
  missing_varcov <- missing(varcov)
  missing_ids <- missing(ids)
  missing_seed_perm <- missing(seed_perm)
  stopifnot(class(formula) == "formula")
  data <- as.data.frame(data)
  model <- ifelse(missing(int), "marginal", "interaction")
  permutation <- ifelse(num_perm > 0, "perm_x", "none")
  weighted <- (!missing_transform | !missing_varcov)
  toc(log = TRUE, quiet = TRUE)
  tic("indices")
  if (verbose > 0) {
    cat(" - computing indices...\n")
  }
  ind_model <- matlm_ind(formula, data, ...)
  nobs_data <- nrow(data)
  nobs_model <- length(ind_model)
  nobs_omit <- nobs_data - nobs_model
  if (verbose > 1) {
    cat("  -- nobs_data", nobs_data, "/ nobs_model", nobs_model, 
        "/ nobs_omit", nobs_omit, "\n")
  }
  toc(log = TRUE, quiet = TRUE)
  tic("transform")
  if (verbose > 0) {
    cat(" - computing transform...\n")
  }
  if (weighted) {
    if (missing_transform) {
      stopifnot(!missing_varcov)
      nobs_varcov <- nrow(varcov)
      if (nobs_varcov == nobs_data) {
        if (nobs_omit) {
          varcov <- varcov[ind_model, ind_model]
        }
      }
      else {
        stopifnot(!missing_ids)
        ids_model <- ids[ind_model]
        stopifnot(!is.null(rownames(varcov)))
        ids_varcov <- rownames(varcov)
        stopifnot(all(ids_model %in% ids_varcov))
        ind <- which(ids_varcov %in% ids_model)
        varcov <- varcov[ind, ind]
      }
      evd <- eigen(varcov, symmetric = TRUE)
      vectors <- evd$vectors
      values <- evd$values
      transform <- vectors %*% diag(1/sqrt(values)) %*% 
        t(vectors)
    }
    else {
      nobs_transform <- nrow(transform)
      if (nobs_transform == nobs_data) {
        if (nobs_omit) {
          transform <- transform[ind_model, ind_model]
        }
      }
      else {
        stopifnot(!missing_ids)
        stopifnot(!is.null(rownames(transform)))
        ids_transform <- rownames(transform)
        ids_model <- ids[ind_model]
        stopifnot(all(ids_model %in% ids_transform))
        ind <- which(ids_transform %in% ids_model)
        transform <- transform[ind, ind]
      }
    }
  }
  toc(log = TRUE, quiet = TRUE)
  tic("pred")
  if (verbose > 0) {
    cat(" - creating `matlmPred`...\n")
  }
  pred <- matlm_pred3(pred, ind = ind_model, num_batches = num_batches, 
                      batch_size = batch_size, path_pred = path_pred)
  num_batches <- pred$num_batches
  stopifnot(pred_nrow(pred) == nobs_data)
  toc(log = TRUE, quiet = TRUE)
  tic("model matrices")
  if (verbose > 0) {
    cat(" - model matrices & response...\n")
  }
  y <- model.extract(model.frame(formula, data), "response")
  C <- model.matrix(formula, data)
  stopifnot(nrow(C) == nobs_model)
  stopifnot(length(y) == nobs_model)
  if (model == "interaction") {
    d_fct <- data[ind_model, int]
    stopifnot(class(d_fct) == "factor")
    stopifnot(nlevels(d_fct) == 2)
    d <- as.numeric(d_fct) - 1
    d_cname <- paste0(int, levels(d_fct)[2])
    stopifnot(d_cname %in% colnames(C))
  }
  N <- nobs_model
  toc(log = TRUE, quiet = TRUE)
  tic("apply transform")
  if (weighted) {
    y <- crossprod(transform, y)
    C <- crossprod(transform, C)
  }
  toc(log = TRUE, quiet = TRUE)
  tic("tests")
  y_orth <- matlm_orth(C, y)
  if (returnRespOrth) {
    return(as.numeric(y_orth))
  }
  y_sc <- matlm_scale(y_orth)
  matlm_batch_marginal <- function(batch, ind) {
    if (verbose > 1) {
      cat("  --  batch", batch, "/", num_batches, "\n")
    }
    X <- pred_batch(pred, batch, ind)
    if (weighted) {
      X <- crossprod(transform, X)
    }
    X_orth <- matlm_orth(C, X)
    if (returnPredOrth) {
      return(list(X_orth))
    }
    sd_X <- matlm_sd(X_orth)
    X_sc <- matlm_scale(X_orth, sd_X = sd_X)
    if (returnPredOrthSc) {
      return(list(X_sc))
    }
    k <- ncol(C) + 1
    r <- as.numeric(crossprod(X_sc, y_sc)/(N - 1))
    r2 <- r * r
    s <- sqrt((1 - r2)/(N - k))
    z <- r/s
    z2 <- z * z
    pvals <- pchisq(z2, df = 1, lower = FALSE)
    if (stats_full) {
      se <- s/sd_X
      b <- z * se
    }
    gc()
    if (!stats_full) {
      list(data_frame(predictor = colnames(X), zscore = z, 
                      pval = pvals))
    }
    else {
      list(data_frame(predictor = colnames(X), b = b, se = se, 
                      zscore = z, pval = pvals))
    }
  }
  matlm_batch_interaction <- function(batch, ind) {
    if (verbose > 1) {
      cat("  --  batch", batch, "/", num_batches, "\n")
    }
    X <- pred_batch(pred, batch, ind)
    M <- ncol(X)
    Xi <- diag(d) %*% X
    if (weighted) {
      X <- crossprod(transform, X)
      Xi <- crossprod(transform, Xi)
    }
    X_orth <- matlm_orth(C, X, Xi)
    if (returnPredOrth) {
      return(list(X_orth))
    }
    sd_X <- matlm_sd(X_orth)
    X_sc <- matlm_scale(X_orth, sd_X = sd_X)
    if (returnPredOrth) {
      return(list(X_sc))
    }
    Y <- tcrossprod(y, rep(1, M))
    Y_orth <- matlm_orth(C, X, Y)
    Y_sc <- matlm_scale(Y_orth)
    r <- apply(X_sc * Y_sc, 2, sum)/(N - 1)
    s <- r * sqrt((N - 2)/(1 - r * r))
    s2 <- s^2
    pvals <- pchisq(s2, df = 1, lower = FALSE)
    gc()
    list(data_frame(predictor = colnames(X), zscore = s, 
                    pval = pvals))
  }
  if (verbose > 0) {
    cat(" - computing association `tab`...\n")
  }
  if (cores > 1) {
    cl <- makeCluster(cores, type = "FORK")
    out <- switch(model, marginal = parSapply(cl, seq(1, 
                                                      num_batches), function(b) matlm_batch_marginal(b, 
                                                                                                     ind_model)), interaction = parSapply(cl, seq(1, num_batches), 
                                                                                                                                          function(b) matlm_batch_interaction(b, ind_model)), 
                  stop("switch by model (tab)"))
    stopCluster(cl)
  }
  else {
    out <- switch(model, marginal = sapply(seq(1, num_batches), 
                                           function(b) matlm_batch_marginal(b, ind_model)), 
                  interaction = sapply(seq(1, num_batches), function(b) matlm_batch_interaction(b, 
                                                                                                ind_model)), stop("switch by model (tab)"))
  }
  if (returnPredOrth | returnPredOrthSc) {
    mat <- do.call(cbind, out)
    return(mat)
  }
  tab <- bind_rows(out)
  if (verbose > 0) {
    cat(" - computing permutation `ptab`...\n")
  }
  pout <- switch(permutation, none = NULL, perm_x = {
    ind <- ind_model
    L <- num_perm
    N <- length(ind)
    P <- sample(ind, size = N * L, replace = TRUE) %>% matrix(nrow = N, 
                                                              ncol = L)
    pout <- sapply(seq(1:L), function(l) {
      if (verbose > 0) {
        cat("  --  permutation", l, "/", L, "\n")
      }
      if (cores > 1) {
        cl <- makeCluster(cores, type = "FORK")
        out <- switch(model, marginal = parSapply(cl, 
                                                  seq(1, num_batches), matlm_batch_marginal, 
                                                  ind = P[, l]), interaction = parSapply(cl, 
                                                                                         seq(1, num_batches), matlm_batch_interaction, 
                                                                                         ind = P[, l]), stop("switch by model (tab)"))
        stopCluster(cl)
      } else {
        out <- switch(model, marginal = sapply(seq(1, 
                                                   num_batches), matlm_batch_marginal, ind = P[, 
                                                                                               l]), interaction = sapply(seq(1, num_batches), 
                                                                                                                         matlm_batch_interaction, ind = P[, l]), stop("switch by model (tab)"))
      }
      tab <- bind_rows(out)
      tab <- mutate(tab, perm = l) %>% select(perm, everything())
      return(tab)
    }, simplify = FALSE)
  }, stop("switch by permutation"))
  ptab <- bind_rows(pout)
  toc(log = TRUE, quiet = TRUE)
  tic("return")
  out <- list(formula = formula, model = model, weighted = weighted, 
              nobs_data = nobs_data, nobs_model = nobs_model, nobs_omit = nobs_omit, 
              npred = pred_ncol(pred), permutation = permutation, tab = tab, 
              ptab = ptab, num_batches = num_batches, cores = cores)
  oldClass(out) <- c("matlmResults", "matlmList", oldClass(out))
  toc(log = TRUE, quiet = TRUE)
  toc(log = TRUE, quiet = TRUE)
  out$tictoc <- tic.log(format = FALSE)
  out$tictoc_formated <- tic.log(format = TRUE)
  tic.clearlog()
  tictoc_matlm <- tail(out$tictoc, 1)
  tictoc_elapsed <- tictoc_matlm[[1]]$toc - tictoc_matlm[[1]]$tic
  out$tictoc_elapsed <- unname(tictoc_elapsed)
  return(out)
}


GWAS <- function(genotypes, trait, phenotype.name,snp.info, kinship, out.dir) {
  dir.create(paste0(out.dir,"/plots"),recursive = T)
  dir.create(paste0(out.dir,"/frames"),recursive = T)
  dir.create(paste0(out.dir,"/herit"),recursive = T)
  phenotype <- toString(phenotype.name)
  letkin <- kinship
  usemat <- genotypes
  
  chr <- snp.info[,1]   
  pos <- snp.info[,2]
  
  use.trait <- trait
  names(use.trait) <- rownames(letkin)
  
  phe.snp.cor <- t(cor(use.trait,usemat,use = "pairwise"))
  phe.snp.cor[is.na(phe.snp.cor)] <- 0
  snp.selc <- abs(phe.snp.cor)>0.3 & !is.na(phe.snp.cor) 
  
  usemat.pruned <- usemat[,snp.selc]
  ID <- rownames(letkin) 
  mod <- lme4qtl::relmatLmer(use.trait ~ (1|ID), relmat = list(ID = letkin))
  
  herit.mod <- lme4qtl::VarProp(mod) 
  V <- lme4qtl::varcov(mod, idvar = "ID")
  V_thr <- V
  V_thr[abs(V) < 1e-10] <- 0
  decomp <- wlm::decompose_varcov(V, method = "evd", output = "all")
  W <- decomp$transform
  
  nnn <- rep(1,ncol(letkin))
  
  gassoc_gls <- matlm2(use.trait ~ nnn, nnn, pred =  usemat.pruned, ids = rownames(W), transform = W, batch_size = 2000, verbose = 2,cores = 1,stats_full = F)
  gassoc_gls$trait.name <- phenotype
  gassoc_gls$trait.noobs <- length(use.trait)
  
  ###Save as integers
  ##We save values as integers to reduce the amount of disk space used when saving
  gassoc_gls$tab$pval <- gassoc_gls$tab$pval*100000
  gassoc_gls$tab$zscore <- gassoc_gls$tab$zscore*100000
  gassoc_gls$chr <- chr[snp.selc]
  gassoc_gls$pos <- pos[snp.selc]
  snp_index <- c(1:length(snp.selc))[snp.selc]
  save(herit.mod,file=paste(out.dir,"/herit/",phenotype,".out",sep=""))
  
  plot_frame <- data.frame(pval = -log10(gassoc_gls$tab$pval/100000),chr = gassoc_gls$chr,pos = gassoc_gls$pos,zscore = gassoc_gls$tab$zscore,snp_index = snp_index)
  save(plot_frame,file=paste(out.dir,"/frames/",phenotype,".out",sep=""))
  
  manh <- ggplot(plot_frame,aes(x = pos, y = -log10(pval),col = as.factor(chr)))+
    geom_point()+
    facet_grid(~as.factor(chr),space = "free_x",scale = "free_x")+
    xlab("Position Mb / 10")+
    ylab("-log10 p-value")+
    ggtitle(gsub("_"," ",phenotype))+
    theme_minimal()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text.y = element_blank(),
          legend.position = "none",
          text = element_text(size = 35),
          axis.line = element_line(size = 0.2),
          axis.ticks= element_line(size = 0.2),
          panel.spacing.x = unit(6, "mm"),
          plot.title = element_text(hjust = 0.5)) +
    panel_border(color = "black",size = 0.2)+
    scale_color_viridis(discrete = T)
  
  png(filename = paste0(out.dir,"/plots/",phenotype,".png"),width = 2500,height = 750)
  print(manh)
  dev.off()
}

load("GWAS_objects/obj_phe.sat.mean.1106.2506.diff.include.rat.R4.3.2.out")
#Instead of loading this file here we manually code what accession numbers we use.
#load("GWAS_objects/sat.accessions.out")
accessions <- c("LK001", "LK002", "LK003", "LK004", "LK005", "LK006", "LK007", "LK008",
                "LK009", "LK010", "LK011", "LK012", "LK013", "LK014", "LK015", "LK016",
                "LK017", "LK018", "LK019", "LK020", "LK021", "LK022", "LK023", "LK024",
                "LK025", "LK026", "LK027", "LK028", "LK029", "LK030", "LK031", "LK032",
                "LK033", "LK034", "LK035", "LK036", "LK037", "LK038", "LK039", "LK040",
                "LK041", "LK042", "LK043", "LK044", "LK045", "LK046", "LK047", "LK048",
                "LK049", "LK050", "LK051", "LK052", "LK053", "LK054", "LK055", "LK056",
                "LK057", "LK058", "LK059", "LK060", "LK061", "LK062", "LK063", "LK064",
                "LK065", "LK066", "LK067", "LK068", "LK069", "LK070", "LK071", "LK072",
                "LK073", "LK074", "LK075", "LK076", "LK077", "LK078", "LK079", "LK080",
                "LK081", "LK082", "LK083", "LK084", "LK086", "LK087", "LK088", "LK089",
                "LK091", "LK092", "LK093", "LK094", "LK095", "LK097", "LK098", "LK099",
                "LK100", "LK101", "LK102", "LK103", "LK104", "LK105", "LK106", "LK107",
                "LK108", "LK109", "LK110", "LK111", "LK112", "LK113", "LK114", "LK115",
                "LK116", "LK117", "LK118", "LK119", "LK120", "LK121", "LK122", "LK123",
                "LK124", "LK125", "LK126", "LK127", "LK128", "LK129", "LK130", "LK131",
                "LK132", "LK133", "LK134", "LK135", "LK136", "LK137", "LK138", "LK139",
                "LK140", "LK141", "LK142", "LK143", "LK144", "LK145", "LK146", "LK147",
                "LK148", "LK149", "LK150", "LK151", "LK152", "LK153", "LK154", "LK155",
                "LK156", "LK157", "LK158", "LK159", "LK160", "LK161", "LK162", "LK163",
                "LK164", "LK165", "LK166", "LK168", "LK169", "LK170", "LK171", "LK172",
                "LK173", "LK174", "LK175", "LK176", "LK177", "LK178", "LK179", "LK180",
                "LK181", "LK182", "LK183", "LK184", "LK185", "LK186", "LK187", "LK189",
                "LK191", "LK192", "LK193", "LK194", "LK195", "LK196", "LK197", "LK198",
                "LK199", "LK200")

# Generate the kinship matrix by taking the covariance of the SNPs
usemat <- load("GWAS_objects/obj_all.ALTREF_SNP_matrix_sat_2024_R4.3.2.out")
usemat <- eval(parse(text=usemat))
usemat <- usemat[,-c(1:10)]
usemat <- as.matrix(t(usemat))

usemat <- usemat[rownames(usemat)%in%accessions,]
usemat[usemat == 9] <- 1
usemat <- t(usemat)
kinship <- cov(usemat)

rownames(kinship) <- accessions
colnames(kinship) <- accessions

save(kinship,file = "GWAS_objects/obj_KinshipMatrix.R4.3.2.out ")

#If you ran this script previously you can use this line to spare time
#load("GWAS_objects/obj_KinshipMatrix.R4.3.2.out ") 
letkin <- kinship

usemat <- load("GWAS_objects/obj_all.ALTREF_SNP_matrix_sat_2024_R4.3.2.out")
usemat <- eval(parse(text=usemat))
snp.info <- usemat[,c(1,7)]
usemat <- usemat[,-c(1:10)]
usemat <- as.matrix(t(usemat))

usemat <- usemat[rownames(usemat)%in%accessions,]

usemat[usemat == 9] <- 1

mclapply(1:ncol(sat.full), function(x) GWAS(genotypes = usemat, trait = as.vector(sat.full[,x]), phenotype.name = gsub(".","_",colnames(sat.full)[x],fixed = T), kinship=letkin, snp.info = snp.info,out.dir = "/GWAS_sat"),mc.cores = cores.available)
