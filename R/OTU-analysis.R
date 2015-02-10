

tidy_phyloseq <- function(my_phyloseq){
  # Fix ranks. 
  colnames(tax_table(my_phyloseq)) = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  # fix taxa names
  tax_table(my_phyloseq)[,colnames(tax_table(my_phyloseq))] <- gsub(tax_table(my_phyloseq)[,colnames(tax_table(my_phyloseq))],pattern="[a-z]__",replacement="")
  # If NA in phylum position of taxonomy table then change it to "Unidentified"
  tax_table(my_phyloseq)[tax_table(my_phyloseq)[,"Phylum"]==NA,"Phylum"] <- "Unidentified"
  return(my_phyloseq)
}

################### compare_pairs function ################
# This test uses chisq. An alternative might be Fishers exact test
compare_pairs <- function(phy,factor="zone",group1="D.BP",group2="U.OV",topN=30,include_otus=NA,name="") {
  
  outfile <- NULL
  
  # Identify the top N OTUs before doing any subsetting
  TopNOTUs <- names(sort(taxa_sums(phy), TRUE)[1:topN])
  
  # Merge by each group
  phy.merged <- merge_samples(phy,group="zone")  
  
  # record the number of observations to enable calculation of expected values
  ss <- sample_sums(phy.merged)
  
  # Keep only the groups of interest
  phy.merged <- prune_samples(samples=c(group1,group2),phy.merged)
  ss <- ss[c(group1,group2)]
  
  # Keep only the chosen OTUs if specified
  if(!is.na(include_otus[1])) {
    phy.merged <- prune_taxa(include_otus,phy.merged) 
  }
  
  cat(paste("<h4>Compare",name,"by",factor,":",group1,"(",ss[group1],"seqs)",group2,"(",ss[group2],"seqs). Top", topN,"taxa.</h4>"))
  
  sink(outfile)
  print("Sequences in raw merged data:")
  print(ss)
  
  # remove OTUs which are not observed more than 20 times in the subsetted object
  # A low expected value leads to ## Warning: Chi-squared approximation may be incorrect
  phy.merged <- filter_taxa(phy.merged, function(x) sum(x) > 20 , TRUE)
  #data <- extract_from_phyloseq(phy.merged,factor,factor)
  
  # keep only top N taxa
  phy.merged <- prune_taxa(TopNOTUs, phy.merged)
  
  # store as data frame
  merged.table <- data.frame(t(otu_table(phy.merged)))
  
  # Do Chi squared test on each OTU.
  # The expected value is adjusted according to the number of sequence reads stored in ss
  print(results <- apply(X=merged.table,MARGIN=1,FUN=chisq.test,p=ss,rescale.p=TRUE))
  
  #create an empty list for the results
  mylist <- list()  
  # Arrange the results in the list
  for(otu in row.names(merged.table)) {
    mylist[[otu]] <- c(tax_table(phy)[otu,c("Phylum")],results[[otu]]$observed,results[[otu]]$expected,results[[otu]]$p.value)
  }
  
  #combine all vectors into a matrix
  df <- do.call("rbind",mylist) 
  colnames(df) <- c("Phylum",group1,group2,paste(names(results[[otu]]$expected),"expected",sep="."),"Xsq_p.value")
  df <- as.data.frame(df,stringsAsFactors=FALSE,)
  
  # repair the data types in the dataframe
  df[,group1] <- as.numeric(df[,group1])
  df[,group2] <- as.numeric(df[,group2]) 
  df[,paste(group1,"expected",sep=".")] <- as.numeric(df[,paste(group1,"expected",sep=".")])
  df[,paste(group2,"expected",sep=".")] <- as.numeric(df[,paste(group2,"expected",sep=".")])
  # df$Xsq_p.value <- as.numeric(df$Xsq_p.value)
  # rounding is done here to avoid confusing output which occurs for p<2.2e-16 due to number storage limitations
  # Instead "0" will be output which should be interpreted as <2.2e-16
  df$Xsq_p.value <- round(as.numeric(df$Xsq_p.value),15)
  df$Xsq_p.value.corrected <- p.adjust(p=df$Xsq_p.value,method="fdr")
  #df
  df[,paste(group1,"pc",sep="_")] <- 100 * df[,group1] / ss[group1]
  df[,paste(group2,"pc",sep="_")] <- 100 * df[,group2] / ss[group2]
  
  sink()
  cat(paste("<a href='",outfile,"'>Link to results</a>",sep=""))
  return(df)
}


