
#################################
### Weighting Input Variables ###
#################################


flatten_square_matrix <- function(m) {
  ## Format the cor.prob output to a 4 column matrix
  ## with row/column indices, correlation, and p-value.
  ## See StackOverflow question: http://goo.gl/fCUcQ
  #https://www.r-bloggers.com/more-on-exploring-correlations-in-r/
  
  ## INPUTS: 
  ## m (matrix): matrix of correlations
  
  ## RETURN:
  ## (data frame): data frame with column names, correlation, and p-value
  
  if ((class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if (!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  return(data.frame(rown = rownames(m)[row(m)[ut]],
             coln = rownames(m)[col(m)[ut]],
             cor = t(m)[ut],
             p = m[ut]))
}

var_corr_matrix <- function(df, trial_name) {
  ## Creates correlation matrix with absolute value of pairwise variable correlations
  
  ## INPUTS:
  ## df (data frame): data frame containing variables to use in clustering analysis
  ## trial_name (string): name of current trial
  
  ## OUTPUTS:
  ## cor_final (data frame): correlation data frame
  
  corr <- cor(as.matrix(df))
  cor_flat <- flatten_square_matrix(corr)
  cor_final <- cor_flat %>%
    mutate(abs_cor = abs(cor))
  
  path <- paste( trial_name, "var_corr.csv", sep = "_")
  
  # write to a csv
  write.csv(cor_final, file = here("results", path), row.names = FALSE)
  
  return(cor_final)
  
}

weight_vars <- function(full_df, var_corr, upwt_cols, upwt_wts, trial_name) {
  ## Calculate weights for variables used for clustering based on the following rules:
  ## - drop one variable from pairs with an absolute correlation > .9 from weighting
  ## - start at original weight of 4 for each variable
  ## - for variable pairs correlated at 0.75 or above, subtract 2 from weight for one variable in the pair and 1 for the other.
  ## - for variable pairs correlated at 0.5 <= x < 0.75, we subtract 1 from weight for each variable in the pair.
  ## - for variable pairs correlated at 0.25 <= x < 0.5, we subtract 1 from weight for one variable in the pair.
  ## - if weight is now negative, reset weight to 1
  ## - if any variables in the upweight columns were dropped for high correlation, add back in with original weight of 1
  ## - assign multiplication factor according to upwt_wts for upwt_cols, all other vars have mult factor of 1
  ## - calculate final weights by multiplying updated weight by multiplication factor 
  ## - for any variables dropped for high correlation that haven't been added back in, add in now with final weight of 1
  ## Code adapted from Mendonca and MacDonald (2017)
  
  
  ## INPUTS:
  ## full_df (data frame): data frame with variables used for analysis
  ## var_corr (data frame): data frame with absolute variable correlations
  ## upwt_cols (vector of strings): names of columns to manually upweight
  ## upwt_wts (vector of integers): weights corresponding to columns to manually upweight
  ## trial_name (string): name of current trial
  
  ## OUTPUT:
  ## wt.all.5 (data frame): data frame containing weights for variables
  
  
  ### Eliminate Highly Correlated Variables ###
  
  #high correlation pairs
  high.cor <- var_corr %>%
    select(rown, coln, cor, abs_cor) %>%
    filter(abs_cor  > 0.9)
  
  #non-high correlation pairs
  non.high.cor <- var_corr %>%
    select(rown, coln, cor, abs_cor) %>%
    filter(abs_cor < 0.9)
  
  #creating a list of highly correlated columns
  #The rown values are to be eliminated
  high.cor.var.r <- high.cor %>%
    mutate(rown = as.character(rown)) %>%
    pull(rown)
  
  high.cor.to.drop <- high.cor.var.r
  
  print(high.cor.to.drop)
  
  #loop through the non high correlation data frame and delete any pair that contains a value in high.cor.to.drop
  pairs.with.high.var.rown <- non.high.cor[non.high.cor$rown %in% high.cor.to.drop,]
  pairs.with.high.var.coln <- non.high.cor[non.high.cor$coln %in% high.cor.to.drop,]
  
  pairs.with.high.var <- rbind(pairs.with.high.var.rown, pairs.with.high.var.coln)
  
  pairs.under.consideration <-  anti_join(non.high.cor, 
                                          pairs.with.high.var, 
                                          by = c("rown", "coln", "cor", "abs_cor"))
  
  ### Duplicate Variables and Down-Weight ###
  
  #Create 3 additional copies of each variable (4 copies total). We "down-weight" using the following rules, in order:
  
  #1. For variable pairs correlated at 0.75 or above, we eliminate 2 copies from one variable in the pair and one from the other.
  #Note: we subtract 2 from column rown and 1 from column coln
  
  cor.75 <- pairs.under.consideration %>%
    mutate(rown = as.character(rown), coln = as.character(coln)) %>%
    filter(abs_cor  >=  0.75)
  
  #--------
  #we subtract 2 from column rown and 1 from column coln
  #--------
  #rown processing
  #t1 is a data frame that contains value -2 for each row. 
  t1 <- data.frame("reps" = rep.int(-2, nrow(cor.75)))
  analy.var <- data.frame( "analysis.var" = cor.75$rown) 
  d1 <-  bind_cols(analy.var, t1)
  
  #coln
  #t2 is a data frame that contains value -1 for each row. 
  t2 <- data.frame("reps" = rep.int(-1, nrow(cor.75)))
  analy.var.1 <- data.frame( "analysis.var" = cor.75$coln) 
  d2 <-  bind_cols(analy.var.1, t2)
  
  #appending the row and col
  wt.1 <- suppressWarnings(suppressMessages(bind_rows(d2, d1)))
  
  #2. For variable pairs correlated at 0.5 <= x < 0.75, we eliminate 1 copy from each variable in the pair.
  cor.50 <- pairs.under.consideration %>%
    filter(abs_cor >= 0.5 & abs_cor < 0.75)
  
  nrep.2 <- nrow(cor.50)
  t3 <- data.frame("reps" = rep.int(-1, nrep.2*2))
  
  analy.var.2 <- data.frame( "analysis.var" = cor.50$coln)
  analy.var.3 <- data.frame( "analysis.var" = cor.50$rown)
  analy.var.23 <- suppressWarnings(suppressMessages(bind_rows(analy.var.2,
                                                              analy.var.3)))
  wt.2 <- bind_cols(analy.var.23, t3)
  
  #3. For variable pairs correlated at 0.25 <= x < 0.5, we eliminate 1 copy from one variable in the pair.
  #Note: we eliminate 1 from coln and 0 from rown
  
  cor.25 <- pairs.under.consideration %>%
    filter(abs_cor < 0.50 & abs_cor >= 0.25)
  
  #eliminate 1 from the coln
  t4 <- data.frame("reps" = rep.int(-1, nrow(cor.25)))
  analy.var.4 <- data.frame( "analysis.var" = cor.25$coln)
  analy.44 <- cbind(analy.var.4, t4)
  
  #eliminate 0 from rown
  t5 <- data.frame("reps" = rep.int(0, nrow(cor.25)))
  analy.var.5 <- data.frame( "analysis.var" = cor.25$rown)
  analy.55 <- cbind(analy.var.5, t5)
  
  wt.3 <- suppressWarnings(suppressMessages(bind_rows(analy.44, analy.55)))
  
  #putting the 3 weight data frames together
  #aggregating the number of reps we need to subtract out
  
  wt.all.0 <- suppressWarnings(suppressMessages(bind_rows(wt.1, wt.2, wt.3)))
  
  wt.all.agg <- aggregate(reps~analysis.var, wt.all.0, sum)

  # add upweight columns
  imp <- data.frame("analysis.var" = upwt_cols, "mult.fact" = upwt_wts)
  
  wt.all.1 <- merge(x = imp, y = wt.all.agg, by = "analysis.var", all = TRUE)
  
  #adding column orig.wt that has value 4. All columns have an initial weight of 4, unless in high correlation variables,
  #then has original weight of 1. This accounts for edge case in which one of upweight variables in the high.cor.to.drop
  #in this case, we want final weight to be equal to the upweight factor
  t6 <- data.frame("orig.wt" = rep.int(4, nrow(wt.all.1))) 
  wt.all.1.2 <- cbind(wt.all.1, t6) %>% 
    mutate(orig.wt = ifelse(analysis.var %in% high.cor.to.drop, 1, orig.wt))
  
  #determining the weight for each col
  #replacing NA's with 0. 
  wt.all.1.2[c("reps")][is.na(wt.all.1.2[c("reps")])] <- 0
  wt.all.2 <- wt.all.1.2 %>%
    mutate(Diff.origwt.reps = reps + orig.wt)
  
  
  #for all the weights that are negative we change final weight to 1
  wt.all.3 <- wt.all.2 %>%
    mutate(final.wt0 = ifelse(Diff.origwt.reps < 1, 1, Diff.origwt.reps)) 
  
  #multiplying the mult.fact
  #replacing the NA's in final.wt and mult.fact by 1
  wt.all.3[c("mult.fact")][is.na(wt.all.3[c("mult.fact")])] <- 1
  
  wt.all.4 <- wt.all.3 %>%
    mutate(final.wt = final.wt0 * mult.fact)
  
  #adding in the variables that are not in the wt.all.4
  col.pre.corr <- colnames(full_df)
  wt.all.col.names <- unique(wt.all.4$analysis.var)
  
  vals.to.add <-data.frame("analysis.var"= col.pre.corr[!(col.pre.corr %in% wt.all.col.names)])
  
  if (length(vals.to.add$analysis.var) != 0) {
  t7 <- data.frame("final.wt0" = rep.int(NA, nrow(vals.to.add)),
                   "mult.fact" = NA,
                   "reps" = NA,
                   "orig.wt"= NA,
                   "final.wt" = 4,
                   "Diff.origwt.reps" = NA
  )
  
  
  vals.to.add2 <- bind_cols(vals.to.add, t7)
  
  #for the variables with high correlation that were eliminated at the start and were not already added back in 
  #because they are upweight columns, we assign a weight of 1
  vals.to.add4 <- vals.to.add2 %>%
    mutate(final.wt = ifelse(analysis.var %in% high.cor.to.drop, 1, final.wt))
  
  #---
  wt.all.5 <- rbind(vals.to.add4, wt.all.4)
  } else{
    wt.all.5 <- wt.all.4
  }
  
  
  path = paste( trial_name, "weights.csv", sep = "_")
  
  #write out the weights file
  write.csv(wt.all.5 %>% arrange(desc(final.wt)), file = here("results", path), row.names = FALSE)
  
  return(wt.all.5)
}

replicate_cols <- function(df, weights, repdist, trial_name, to_scale = TRUE) {
  ## replicate columns for cluster analysis according to weights calculated in weight_vars function
  ## code adapted from Mendonca and MacDonald (2017)
  
  ## INPUTS:
  ## df (data frame): input data frame with columns for analysis
  ## weights (data frame): data frame of weights calculated by weight_vars function
  ## repdist (vector): vector of reporting districts corresponding to rows of df
  ## trial_name (string): name of modeling trial
  ## to_scale (boolean): whether to scale (subtract mean and divide by std. dev) resulting data frame for analysis, 
  ##  defaults to true
  
  ## RETURNS:
  ## col.replicates (data frame): data frame with repliated cols for analysis (also writes to csv)
  
  col.replicates <- data.frame(row.names = seq_len(nrow(df)))
  for (i in seq_len(nrow(weights))) {
    rowlop <- weights %>%
      slice(i)
    
    var.yui <- rowlop %>%
      mutate(analysis.var = as.character(analysis.var)) %>%
      pull(analysis.var)
    
    n <- rowlop %>%
      pull(final.wt)
    
    col.replicates <- cbind(col.replicates, 
                            as.data.frame(replicate(n, df[var.yui])))
  }
  
  #writing it out as a CSV

  path <- paste(trial_name, "weighted_data.csv", sep = "_")
  
  if (to_scale == TRUE) {
    # Scale the data
    col.replicates <- data.frame(scale(col.replicates))
  }
  
  col.replicates_out <- cbind(col.replicates, as.data.frame(repdist))
  write.csv(col.replicates_out, file = here("results", path), row.names = FALSE)
  return(col.replicates)
}

##########################
### Stability Analysis ###
##########################


compare_results <- function(data_a, data_b) {
  ## Crosswalk cluster assignments in 'comparison model'(data_b) to 'master model' (data_a). For each cluster label in the comparison model, 
  ## identify the cluster number in the master model that contains the most members of the given cluster. So, for example, if the label 2 in 
  ##comparison is *only* matched to group 1 in master, then group 2 for comparison would be assigned a label mapping of group 1. Similarly, 
  ## if the label 3 in comparison had 8 observations linking it to label 2 in master but 22 observations linking it to group 4 in master, 
  ## then group 3 for comparison would be assigned a label mapping of group 3.
  ## Code adapted from Mendonca and MacDonald (2017)
  
  ## INPUTS:
  ## data_a (vector): vector of cluster labels for master model
  ## data_b (vector): vector of cluster labels for comparison model
  
  ## RETURN:
  ## temp_out$target (vector): vector of mapped cluster labels from comparison model to master
  
  temp <- tibble(data_a, data_b)
  summary <- temp %>% 
    group_by(data_a, data_b) %>% 
    summarise(count = n())
  maximums <- summary %>% 
    group_by(data_b) %>% 
    summarise(max = max(count)) %>% 
    mutate(max = as.integer(max))
  summary_filter <- summary %>% 
    left_join(maximums, by = "data_b") %>% 
    filter(count == max) %>% 
    ungroup() %>% 
    distinct(data_b, max, .keep_all=TRUE) %>% 
    select(-max, -count) %>% 
    rename(target = data_a)
  temp_out <- temp %>% 
    left_join(summary_filter, by = "data_b")
  return(temp_out$target)
}

one_col_stability <- function(col, analysis_df, final_n, final_clusters) {
  ## Conducts stability analysis for one column. Drops column (col) from analytical data frame (analysis_df) and
  ## runs GMM algorithm with the assigned number of clusters (final_n) corresponding to the master model (final_clusters).
  ## Identifies the number of reporting districts that are assigned to the same cluster across the master and comparison model.
  
  ## INPUTS:
  ## col (string): name of column to drop
  ## analysis_df (data frame): analytical df used for master model cluster analysis
  ## final_n (int): number of clusters in final_clusters
  ## final_clusters (vector): cluster assignments for master model
  
  ## RETURNS:
  ## sum(compare_clusters$match) (int): number of reporting districts with matching cluster assignment
  
  analysis_df_mod <- analysis_df %>% 
    select(-grep(col, colnames(analysis_df))) # drop all copies of column containing base name
  g_mod <- Mclust(analysis_df_mod, final_n, verbose=FALSE)
  g_mod_cluster_target <- compare_results(final_clusters, g_mod$classification)
  compare_clusters <- data.frame(cbind(final_clusters, g_mod_cluster_target)) %>% 
    mutate(match = ifelse(final_clusters == g_mod_cluster_target, 1, 0))
  
  # return the number of reporting districts that match across clusters
  return(sum(compare_clusters$match)) 
  
}

stability_analysis <- function(colnames, analysis_df, final_n, final_clusters, trial_name) {
  ## Conducts stability analysis across all columns in analytical data frame. 
  
  ## INPUTS:
  ## colnames (vector): vector of unique column names included in analysis_df
  ## analysis_df (data frame): analytical df used for master model cluster analysis
  ## final_n (int): number of clusters in final_clusters
  ## final_clusters (vector): cluster assignments for master model
  ## trial_name (string): name of modeling trial 
  
  ## RETURNS:
  ## data (data frame): column names and corresponding number of reporting districts assigned to same cluster 
  ##  when the column is dropped from cluster analysis 
  
  col_similarity <- c()
  for (col in colnames) {
      col_similarity <- c(col_similarity, one_col_stability(col, analysis_df, final_n, final_clusters)) 
    }
    data <- tibble(colnames, col_similarity) %>% 
      arrange(col_similarity) %>% 
      rename("Column Name" = colnames, "Reporting Districts in Same Cluster" = col_similarity)
    write.csv(data, here("results", paste(trial_name, "stability_scores.csv", sep = "_")), row.names=FALSE)
    
    return(data)
}



##########################
### K-Means Clustering ###
##########################


k_means_cluster <- function(analysis_df, start_k, end_k, trial_name) {
  ## Performs k-means cluster analysis over range of cluster numbers from start_k to end_k. For each number of clusters,
  ## calculates the within-cluster sum of squares and average silhouette metrics
  
  ## INPUTS:
  ## analysis_df (data frame): analytical df used for cluster analysis
  ## start_k (int): smallest number of clusters to test
  ## end_k (int): greatest number of clusters to test, all numbers from start_k to end_k are tested
  ## trial_name (string): name of modeling trial 
  
  ## RETURNS:
  ## (list of numeric): containing within-cluster sum of squares (within_ss) and average silhouette (avg_silhouette)

  # loop over possible values of k
  within_ss <- vector(mode = "numeric", length = end_k - start_k)
  avg_silhouette <- vector(mode = "numeric", length = end_k - start_k)

 k_vals <- c(start_k:end_k)
 for (k in k_vals) {
   km <- kmeans(analysis_df, k, nstart = 25)
   within_ss[k - (start_k - 1)] <- km$tot.withinss # store within-cluster sum of squares
   ss <- cluster::silhouette(km$cluster, dist(analysis_df)) # calculate silhouette for each observation
   avg_silhouette[k - (start_k - 1)] <- mean(ss[, 3]) # store average silhouette (reference: https://uc-r.github.io/kmeans_clustering)
  } 
  results <- data.frame(within_ss, avg_silhouette)
  path <- paste(trial_name, "kmeans_results.csv", sep ="_")
  write.csv(results, file = here("results", path), row.names = FALSE)

  return(list("within_ss" = within_ss, "avg_silhouette" = avg_silhouette))
}

elbow_plot <- function(n_vals, within_ss) {
  ## Plots within-cluster sum of squares by number of clusters
  
  ## INPUTS:
  ## n_vals (vector of ints): vector of the numbers of clusters tested
  ## within_ss (vector of numeric): vector of within-cluster sum of squares corresponding to numbers of clusters
  
  ## RETURNS:
  ## within_ss_plot (ggplot): elbow plot
  
  # Plot results to pick optimal number clusters using elbow method

  within_ss_data <- data.frame(n_vals, within_ss)
  
  within_ss_plot <- ggplot(data = within_ss_data, aes(x = n_vals, y = within_ss)) +
    geom_line() + 
    geom_point() +
    labs(y = "Total Within-Cluster Sum of Squares", x = "Number of Clusters")
  
  return(within_ss_plot)
}


silhouette_plot <- function(n_vals, avg_silhouette) {
  ## Plots average silhouette by number of clusters
  
  ## INPUTS:
  ## n_vals (vector of ints): vector of the numbers of clusters tested
  ## within_ss (vector of numeric): vector of average silhouette corresponding to numbers of clusters
  
  ## RETURNS:
  ## avg_silhouette_plot (ggplot): average silhouette plot
  
  #Silhouette Method
  avg_silhouette_data <- data.frame(n_vals, avg_silhouette)
  
  avg_silhouette_plot <- ggplot(data = avg_silhouette_data, aes(x = n_vals, y = avg_silhouette)) +
    geom_line() + 
    geom_point() +
    labs(y = "Average Silhouette", x = "Number of Clusters")
  
  return(avg_silhouette_plot)
  
}

#############################
## Hierarchical Clustering ##
#############################


hierarchical_cluster <- function(analysis_df, start_k, end_k, trial_name) {
  ## Performs hierarchical cluster analysis over range of cluster numbers from start_k to end_k. For each number of clusters,
  ## calculates the within-cluster sum of squares and average silhouette metrics
  
  ## INPUTS:
  ## analysis_df (data frame): analytical df used for cluster analysis
  ## start_k (int): smallest number of clusters to test
  ## end_k (int): greatest number of clusters to test, all numbers from start_k to end_k are tested
  ## trial_name (string): name of modeling trial 
  
  ## RETURNS:
  ## (list of numeric): containing within-cluster sum of squares (within_ss) and average silhouette (avg_silhouette)
  
  # loop over possible values of k
  within_ss <- vector(mode = "numeric", length = end_k - start_k)
  avg_silhouette <- vector(mode = "numeric", length = end_k - start_k)
  
  wss_plot <- fviz_nbclust(analysis_df,
                           FUN = hcut,
                           k.max = end_k,
                           method = "wss",
                           hc_func = "agnes",
                           hc_method = "ward.D2",
                           hc_metric = "euclidean")
  
  wss <- ggplot_build(wss_plot)
  wss_data <- wss$data[[1]]$y[-1] #remove value for k = 1
    
  avg_silhouette_plot <- fviz_nbclust(analysis_df,
                                      FUN = hcut,
                                      k.max = end_k,
                                      method = "silhouette",
                                      hc_func = "agnes",
                                      hc_method = "ward.D2",
                                      hc_metric = "euclidean")
  
  avg_silhouette <- ggplot_build(avg_silhouette_plot)
  as_data <- avg_silhouette$data[[1]]$y[-1] #remove value for k = 1
  
  results <- data.frame(wss_data, as_data)
  path <- paste(trial_name, "hclust_results.csv", sep ="_")
  write.csv(results, file = here("results",  path), row.names = FALSE)
  
  return(list("within_ss" = wss_data, "avg_silhouette" = as_data))
}


################################
#### Gaussian Mixed Methods ####
################################


gmm_cluster <- function(analysis_df, start_k, end_k, trial_name){
  
  ## Performs k-means cluster analysis over range of cluster numbers from start_k to end_k. For each number of clusters,
  ## calculates the within-cluster sum of squares and average silhouette metrics
  
  ## INPUTS:
  ## analysis_df (data frame): analytical df used for cluster analysis
  ## start_k (int): smallest number of clusters to test
  ## end_k (int): greatest number of clusters to test, all numbers from start_k to end_k are tested
  ## trial_name (string): name of modeling trial 
  
  ## RETURNS:
  ## (list of numeric): containing within-cluster sum of squares (within_ss) and average silhouette (avg_silhouette)
  
  
  # loop over possible values of k
  bic <- vector(mode = "numeric", length = end_k - start_k)
  avg_silhouette <- vector(mode = "numeric", length = end_k - start_k)
  
  k_vals <- c(start_k:end_k)
  for (k in k_vals) {
    gmm <- Mclust(analysis_df, k, verbose = FALSE)
    bic[k - (start_k - 1)] <- gmm$bic # bayesian information criterion
    ss <- cluster::silhouette(gmm$classification, dist(analysis_df)) # calculate silhouette for each observation
    avg_silhouette[k - (start_k - 1)] <- mean(ss[, 3]) # store average silhouette (reference: https://uc-r.github.io/kmeans_clustering)
  }
  results <- data.frame(bic, avg_silhouette)
  path <- paste(trial_name, "gmm_results.csv", sep = "_")
  write.csv(results, file = here("results",  path), row.names = FALSE)
  
  return(list("bic" = bic, "avg_silhouette" = avg_silhouette))
}

bic_plot <- function(n_vals, bic){
  ## Creates Bayesian Information Criterion plot for GMM cluster results
  
  ## INPUTS:
  ## n_vals (vector of ints): vector of the numbers of clusters tested
  ## bic (vector of numeric): vector of Bayesian Information Criterion corresponding to numbers of clusters
  
  ## RETURNS:
  ## bic_plot (ggplot): Bayesian Information Criterion plot
  
  # Plot results to pick optimal number clusters
  bic_data <- data.frame(n_vals, bic)
  
  bic_plot <- ggplot(data = bic_data, aes(x = n_vals, y = bic)) +
    geom_line() + 
    geom_point() +
    labs(y = "Bayesian Information Criterion", x = "Number of Clusters")
  
  return(bic_plot)
}

################
#### T-Test ####
################

t_test_cluster <-function(df, cluster_var, results_cols) {
  ## Performs t-tests of cluster means against population mean for each cluster and variable in df
  
  ## INPUTS:
  ## df (data frame): data frame of cluster means where columns corresponds to variables and rows are cluster means
  ## cluster_var (string): name of variable corresponding to to clusters
  ## results_cols (vector of strings): vector of column names to perform t-tests on
  
  ## RETURNS:
  ## data.frame(p_list) (data frame): data frame where one column is the variable names, and each other column represents p-values
  ##  for two-sided t-test of cluster mean for the corresponding variable against population mean for that variable
  
  p_list <- list()
  p_list[["variables"]] <- results_cols
  for (cluster in sort(unique(df[[cluster_var]]))) {
    clust_df <- df %>% filter(!!sym(cluster_var) == cluster) 
    p_values <- c()
    for (col in results_cols) {
      p_values <- c(p_values, 
                    t.test(clust_df[[col]], 
                           mu = mean(df[[col]], na.rm = TRUE), 
                           alternative = "two.sided")$p.value)
    }
    p_list[[paste("cluster", cluster, sep = "_")]] <- p_values
    
  }
  return(data.frame(p_list))
}

#######################
### T-Test Outliers ###
#######################

t_test_outlier <- function(df, cols_to_test, outlier_var){
  
  out_t <- lapply(df[, cols_to_test], function(x) t.test(x ~ df[[outlier_var]], var.equal = TRUE))
  
  out_df_t <- data.frame(matrix(unlist(out_t), nrow=length(out_t), byrow=T)) 
  cols <- insert(names(out_t[[1]]), ats = c(5,6), c("conf.int.upper", "estimate_1"))
  colnames(out_df_t) <- cols
  
  out_df_t <- out_df_t %>% 
    mutate(variable = names(out_t),
           p.value = as.numeric(as.character(p.value))) %>%
    rename(estimate_0 = estimate, conf.int.lower = conf.int) 
  
  out_df_t %>% write.csv(here("results", paste(trial_name, outlier_var, "t", sep = "_")))
  
  df_to_return <- out_df_t %>%
    filter(p.value < 0.05) %>%
    select(variable, estimate_0, estimate_1, p.value)
  
  return(df_to_return)
  
}
