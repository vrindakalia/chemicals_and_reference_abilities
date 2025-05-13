# functions

replaceLODzero <- function(x) "[<-"(x, x=="<LOD", "0")

replaceLODNA <- function(x) "[<-"(x, x=="<LOD", "NA")

replaceNFNA <- function(x) "[<-"(x, x=="N/F", "NA")

replacezero <- function(x) "[<-"(x, !x|x==0, min(x[x>0], na.rm = T)/2)

replaceNA <- function(x) "[<-"(x, !x|is.na(x), min(x[x>0], na.rm = T)/2)

codeLODNF <- function(x) {
    y = case_when(x == "<LOD" ~ 2,
                  x == "N/F" ~ 1,
                  x != "<LOD|N/F" ~3,
                  is.na(x) ~ 1)
    return(y)
}

