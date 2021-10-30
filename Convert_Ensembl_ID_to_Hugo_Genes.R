# Gene Annotation - Oct 28, 2021
# Copyright: Usama Bakry

# R version 4.1.1 (2021-08-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.0.1

# Instal packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("org.Hs.eg.db")

# Import packages
library(org.Hs.eg.db)
library(data.table)

# Function that splits a string into a vector by a certain pattern
split_col <- function(pattern, col, idx){ 
  col.chr <- as.character(col)
  lst <- sapply(strsplit(col.chr, split = pattern),`[`, idx)
  vctr <- unlist(lst)
  return(vctr)
}

# Function for gene annotation
convert <- function(lst){
  lst <- as.vector(lst)
  lst <- split_col("\\.", lst,1)
  
  if (lst[1] %like% "ENST"){
    col.val <- "SYMBOL"
    keytype.val <- "ENSEMBLTRANS"
  } else if (lst[1] %like% "ENSG"){
    col.val <- "SYMBOL"
    keytype.val <- "ENSEMBL"
  } else {
    col.val <- "ENSEMBL"
    keytype.val <- "SYMBOL"
  }
  
  result = tryCatch({
    genes.df <- as.data.frame(select(org.Hs.eg.db, keys=lst, column=col.val, keytype=keytype.val))
    return(genes.df)
  }, warning = function(w) {
    cat("Warning: ", toString(w))
  }, error = function(e) {
    cat("Error: ", toString(e))
    print("Sorry, we couldn't annotate your input.")
  })
  
}

# Load data for test
data.test <- read.csv("data/test_ids_input.tsv", sep = '\t', header = TRUE)

# From ENSEMBL ID to GENE SYMBOL
res.df <- convert(data.test$gene_id)

# From ENSEMBL TRANSCRIPT ID to GENE SYMBOL
res.df <- convert(data.test$transcript_id)

# From GENE SYMBOL to ENSEMBL ID
res.df <- convert("KRAS")

# From ENSEMBL ID to GENE SYMBOL (with only one ID)
res.df <- convert("ENSG00000133703")

# Test tryCatch
res.df <- convert("ENST00000256078")


