make_cluster_scatterplot <- function(results_df, cluster_var, trial_name) {
  # Creates a scatterplot of reporting districts with total police and total 
  # resident activity as the x and y axis and color as the cluster variable. 
  # Returns ggplot and writes png file to images directory.
  
  #INPUTS:
  #results_df (dataframe): dataframe 
  #cluster_var (string): name of variable with cluster assignments
  #trial_name (string): name of current trial  
  
  #OUTPUTS:
  #cluster_plot: ggplot
  
  set_urbn_defaults(style = "print")
  
  cluster_plot <- ggplot(data = results_df, 
                         aes(x = total_police_scale, y = total_citizen_scale)) +
    geom_point(aes(color = !!sym(cluster_var)), alpha  = .75) +
    scale_x_continuous(name = "Police-Initiated Activity: Stop and Arrest Rate (Standard Deviations from Mean)",
                       limits = c(-2, 13)) +
    scale_y_continuous(name = "Resident-Initiated Activity: Calls for Service Rate (Standard Deviations from Mean)",
                       limits = c(-2, 13)) +
    labs(title = "Group Assignment Follows Calls for Service & Stops/Arrests", 
         subtitle = "Total calls for service by total stops and arrests in each reporting district,\n colored by group assignment",
         color = "Cluster") +
    ggsave(here("images", 
                paste(trial_name, cluster_var, "cluster_plot.png", sep = "_")), 
           width = 6, height = 4.7, units = "in")
  
  return(cluster_plot)
}

make_count_by_type_barplot <- function(vars, lbls, subj, trial_name) {
  # Creates a barplot where each bar is the sum across reporting districts 
  # in a given cluster. Returns ggplot and writes png file to images directory.
  
  #INPUTS:
  #vars (vector of strings): of variables to sum and plot
  #lbls (vector of strings): factor labels corresponding to the variables in vars
  #subj (string): plot subject, used in plot labels and file name
  #trial_name (string): name of current trial  
  
  #OUTPUTS:
  #count_type_plot: ggplot
  
  set_urbn_defaults(style = "print")
  
  count_type_plot <- raw_results_df %>% 
    select(vars, !!sym(final_cluster)) %>% 
    group_by(!!sym(final_cluster)) %>% 
    summarise_all(sum) %>%
    gather(key = variable, value = value, -!!sym(final_cluster)) %>% 
    mutate(variable = factor(variable, 
                             levels = vars, 
                             labels = lbls)) %>%
    ggplot(aes(fill = variable, y = value, x = !!sym(final_cluster))) +
    geom_bar(position = "dodge", stat = "identity") +
    coord_flip() +
    labs(x =  sprintf("Cluster and %s Type", subj), 
         y= sprintf("%s Count", subj), 
         title = sprintf("%s Count by Type and Cluster", subj), 
         fill = "Cluster") +
    ggsave(here("images", 
                paste(trial_name, 
                      sprintf("%s-type.png", tolower(subj)), sep = "_")))
  
  return(count_type_plot)
}

make_mean_by_cluster_barplot <- function(vars, lbls, file_name, trial_name,
                                         x_lab, y_lab, title) {
  # Creates a barplot where each bar is the mean across reporting districts 
  # in a given cluster. Returns ggplot and writes png file to images directory.
  
  #INPUTS:
  #vars (vector of strings): of variables to sum and plot
  #lbls (vector of strings): factor labels corresponding to variables in vars
  #file_name (string): file_name - appended to trial_name in file written
  #trial_name (string): name of current trial  
  #x_lab (string): x-axis label
  #y_lab (string): y-axis label
  #title (string): plot title
  
  #OUTPUTS:
  #mean_by_cluster_plot: ggplot
  
  set_urbn_defaults(style = "print")
  
  mean_by_cluster_plot <- raw_results_df %>% 
    select(vars, !!sym(final_cluster)) %>% 
    group_by(!!sym(final_cluster)) %>% 
    summarise_all(mean) %>%
    gather(key = variable, value = value, -!!sym(final_cluster)) %>% 
    mutate(variable = factor(variable, 
                             levels = vars , 
                             labels = lbls)) %>%
    ggplot(aes(fill = variable, y = value, x = !!sym(final_cluster))) +
    geom_bar(position = "dodge", stat = "identity") +
    coord_flip() +
    labs(x =  x_lab, 
         y = y_lab, 
         title = title, 
         fill = "Cluster") +
    ggsave(here("images", paste(trial_name, file_name, sep = "_")))
  
  return(mean_by_cluster_plot)
}

make_prop_by_type_barplot <- function(vars, lbls, subj, trial_name) {
  # Creates a barplot of subtypes where each bar is the proportion of the whole
  # represented by the given subtype in a cluster (eg. pedestrian stops of 
  # total stops). Returns ggplot and writes png file to images directory.
  
  #INPUTS:
  #vars (vector of strings): of variables to sum and plot
  #lbls (vector of strings): factor labels corresponding to variables in vars
  #subj (string): plot subject, used in plot labels and file name
  #trial_name (string): name of current trial 
  
  #OUTPUTS:
  #pct_type_plot: ggplot
  
  set_urbn_defaults(style = "print")
  
  pct_type_plot <- raw_results_df %>% 
    select(!!sym(final_cluster), vars) %>% 
    gather(type, count, vars) %>% 
    group_by(!!sym(final_cluster), type) %>% 
    summarise(Percentage = sum(count)) %>% 
    group_by(!!sym(final_cluster)) %>% 
    mutate(Percentage = Percentage / sum(Percentage) * 100) %>% 
    mutate(type = factor(type, levels = vars , labels = lbls)) %>%
    ggplot(aes(fill = type, y = Percentage, x = !!sym(final_cluster))) +
    geom_bar(position = "dodge", stat = "identity") +
    coord_flip() +
    labs(x =  sprintf("Cluster and %s Type", subj), 
         y= sprintf("Percentage of Cluster %ss by Type", subj), 
         title = sprintf("%ss by Type and Cluster", subj), 
         fill = "Cluster") +
    ggsave(here("images", 
                paste(trial_name, 
                      sprintf("%s-type-pct.png", subj), sep = "_")))
  
  return(pct_type_plot)
}
  
make_per_capita_barplot <- function(vars, denom_var, lbls, file_name,
                                    trial_name, x_lab, y_lab, title, 
                                    mult_factor) {
  # Creates a barplot where each bar is the variable per capita (where the
  # relevant denominator is defined denom_var) for a given cluster 
  # (eg. count of college graduates per adults over 25). Returns ggplot
  # and writes png file to images directory.
  
  #INPUTS:
  #vars (vector of strings): vector of variables to use as numerators for plot
  #denom_var (string): name of variable to use as denominator
  #lbls (vector of strings): factor labels corresponding to variables in vars
  #file_name (string): file_name - appended to trial_name in file written
  #trial_name (string): name of current trial  
  #x_lab (string): x-axis label
  #y_lab (string): y-axis label
  #title (string): plot title
  #multi_factor (int): factor to multiply rate by 
  # (eg. 1000 corresponds to '<numerator> per 1000 <denom_var>)
  
  
  #OUTPUTS:
  #per_capita_plot: ggplot
  
  set_urbn_defaults(style = "print")
  
  per_capita_plot <- raw_results_df %>% 
    select(vars, !!sym(denom_var), !!sym(final_cluster)) %>% 
    group_by(!!sym(final_cluster)) %>%
    summarise_all(sum) %>% 
    mutate_at(vars, ~(. / !!sym(denom_var)) * mult_factor) %>%
    select(vars, !!sym(final_cluster)) %>%
    gather(key = variable, value = value, -!!sym(final_cluster)) %>% 
    mutate(variable = factor(variable, 
                             levels = vars, 
                             labels = lbls)) %>%
    ggplot(aes(fill = !!sym(final_cluster), y = value, x = variable)) +
    geom_bar(position = "dodge", stat = "identity") +
    coord_flip() +
    labs(x = x_lab, 
         y = y_lab,
         title = title, 
         fill = "Cluster") +
    ggsave(here("images", 
                paste(trial_name, file_name, sep = "_")), 
           width = 6, height = 4.5, units = "in")
  
  return(per_capita_plot)
}

make_facet_per_capita_barplot <- function(vars, denom_var, lbls, file_name,
                                           trial_name, x_lab, y_lab, title, 
                                           mult_factor) {
  # Creates a facet barplot where each bar is the variable per capita 
  # (where the relevant denominator is defined denom_var) for a given
  # cluster (eg. count of college graduates per adults over 25), 
  # faceted by variable. Returns ggplot and writes png file to images directory.
  
  #INPUTS:
  #vars (vector of strings): vector of variables to use as numerators for plot
  #denom_var (string): name of variable to use as denominator
  #lbls (vector of strings): factor labels corresponding to the variables in vars
  #file_name (string): file_name - will be appended to trial_name in file written
  #trial_name (string): name of current trial  
  #x_lab (string): x-axis label
  #y_lab (string): y-axis label
  #title (string): plot title
  #multi_factor (int): factor to multiply rate by 
  # (eg. 1000 corresponds to '<numerator> per 1000 <denom_var>)
  
  
  #OUTPUTS:
  #facet_per_capita_plot: ggplot
  
  set_urbn_defaults(style = "print")
  
  facet_per_capita_plot <- raw_results_df %>% 
    select(!!sym(final_cluster), !!sym(denom_var), vars) %>% 
    group_by(!!sym(final_cluster)) %>%
    summarise_all(sum) %>% 
    mutate_at(vars, ~(. / !!sym(denom_var)) * mult_factor) %>%
    select(vars, !!sym(final_cluster)) %>%
    gather(key = variable, value = value, -!!sym(final_cluster)) %>% 
    mutate(variable = factor(variable, 
                             levels = vars, 
                             labels = lbls)) %>%
    ggplot(aes(fill = !!sym(final_cluster), 
               y = value, 
               x = !!sym(final_cluster))) +
    geom_bar(stat = "identity") +
    facet_wrap(~variable) +
    labs(x =  x_lab, 
         y = y_lab, 
         title = title, 
         fill = "Cluster") +
    ggsave(here("images", paste(trial_name, file_name, sep = "_")))
  
  return(facet_per_capita_plot)
}