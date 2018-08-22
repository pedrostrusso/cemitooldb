read.gmt <- function(fname){
    res <- list(genes=list(), desc=list())
    gmt <- file(fname)
    gmt.lines <- readLines(gmt)
    close(gmt)
    gmt.list <- lapply(gmt.lines, function(x) unlist(strsplit(x, split="\t")))
    gmt.list <- lapply(gmt.list, function(x) x[x != ""])
    gmt.names <- sapply(gmt.list, '[', 1)
    gmt.desc <- lapply(gmt.list, '[', 2)
    gmt.genes <- lapply(gmt.list, function(x){x[3:length(x)]})
    names(gmt.desc) <- names(gmt.genes) <- gmt.names
    res[['genes']] <- gmt.genes
    res[['desc']] <- gmt.desc
    return(res)
}

main <- function(){
    ExpGSets <- read.gmt(args[1])$genes
    RefGSets <- read.gmt(args[2])$genes
    
    UnivSize <- length(unique(unlist(RefGSets)))
    
    L <- length(RefGSets)
    M <- length(ExpGSets)
    
    Pmatrix <- matrix(0, M, L)
    
    for(i in 1:M){
        for(j in 1:L){
            SampleGs <- ExpGSets[[i]]
            PathGs <- RefGSets[[j]]
            HitSample <- length(intersect(SampleGs, PathGs))
            FailSample <- length(SampleGs) - HitSample
            HitLeft <- length(PathGs) - HitSample
            FailLeft <- UnivSize - length(SampleGs) - HitLeft
            Pmatrix[i, j] <- fisher.test(matrix(c(HitSample, HitLeft, FailSample, FailLeft), nrow=2, ncol=2), alternative="greater")$p.value
        }
    }
    
    
    preCES <- -sum(log(1 - apply(Pmatrix, 2, min)))
    CES <- (L/sqrt(M))*(preCES - M/L)
    print(CES)
}