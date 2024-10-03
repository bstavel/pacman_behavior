
prep_conn_plot_df <- function(df, sub){
  
  # filter to subject #
  conn_sub_df <- df %>%
    rename(pval = percent_sig) %>%
    filter(subject == sub) %>%
    arrange(pairs)
  
  # correct for time points #
  conn_sub_sig_df <- conn_sub_df %>%
    mutate(roi_pair = paste0(first_region, "_", second_region)) %>%
    group_by(roi_pair, metric, pairs) %>%
    mutate(p_frd = p.adjust(pval, method = "fdr")) %>%
    mutate(sig = p_frd < 0.05) %>% 
    group_by(roi_pair, metric, time) %>%
    mutate(count_sig = sum(sig)) %>%
    mutate(number_of_region_pairs = length(unique(pairs))) %>%
    mutate(percent_sig = count_sig / number_of_region_pairs * 100)
  
  # create symmetric data #
  amyg_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_amyg") %>%
    mutate(roi_pair = "amyg_hc") %>%
    mutate(first_region = "amyg") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  ofc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_ofc") %>%
    mutate(roi_pair = "ofc_amyg") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "amyg")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  ofc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_ofc") %>%
    mutate(roi_pair = "ofc_hc") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  cing_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_cing") %>%
    mutate(roi_pair = "cing_hc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  cing_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_cing") %>%
    mutate(roi_pair = "cing_amyg") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "amyg")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  cing_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_cing") %>%
    mutate(roi_pair = "cing_ofc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "ofc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  dlpfc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_dlpfc") %>%
    mutate(roi_pair = "dlpfc_hc") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  dlpfc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_dlpfc") %>%
    mutate(roi_pair = "dlpfc_amyg") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "amyg")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  dlpfc_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_dlpfc") %>%
    mutate(roi_pair = "dlpfc_ofc") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "ofc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  dlpfc_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_dlpfc") %>%
    mutate(roi_pair = "dlpfc_cing") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "cing")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  
  insula_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_insula") %>%
    mutate(roi_pair = "insula_hc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  insula_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_insula") %>%
    mutate(roi_pair = "insula_amyg") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "amyg")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  insula_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_insula") %>%
    mutate(roi_pair = "insula_ofc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "ofc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  insula_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_insula") %>%
    mutate(roi_pair = "insula_cing") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "cing")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  insula_dlpfc <- conn_sub_sig_df %>%
    filter(roi_pair == "dlpfc_insula") %>%
    mutate(roi_pair = "insula_dlpfc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "dlpfc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  conn_sub_sig_df <- bind_rows(conn_sub_sig_df, amyg_hc, ofc_amyg, ofc_hc, cing_hc, cing_amyg, cing_ofc, dlpfc_hc, dlpfc_amyg, dlpfc_ofc, dlpfc_cing, insula_hc, insula_amyg, insula_ofc, insula_cing, insula_dlpfc)
  
  conn_sub_sig_df <- conn_sub_sig_df %>%
    mutate(second_region = factor(second_region, levels = c("hc", "amyg", "ofc", "cing", "dlpfc", "insula"), labels = c("Hippocampus", "Amygdala", "OFC", "Ant. Cingulate", "dlPFC", "Insula"))) %>%
    mutate(first_region = factor(first_region, levels = c("hc", "amyg", "ofc", "cing", "dlpfc", "insula"), labels = c("Hippocampus", "Amygdala", "OFC", "Ant. Cingulate", "dlPFC", "Insula")))
  
  return(conn_sub_sig_df)
}


prep_conn_allsubs_plot_df <- function(df){
  
  # filter to subject #
  conn_sub_df <- df %>%
    rename(pval = percent_sig) %>%
    arrange(pairs)
  
  # correct for time points #
  conn_sub_sig_df <- conn_sub_df %>%
    mutate(roi_pair = paste0(first_region, "_", second_region)) %>%
    group_by(subject, roi_pair, metric, pairs) %>%
    mutate(p_frd = p.adjust(pval, method = "fdr")) %>%
    mutate(sig = p_frd < 0.05) %>% 
    group_by(subject, roi_pair, metric, time) %>%
    mutate(count_sig = sum(sig)) %>%
    mutate(number_of_region_pairs = length(unique(pairs))) %>%
    mutate(percent_sig = count_sig / number_of_region_pairs * 100)
  
  # create symmetric data #
  amyg_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_amyg") %>%
    mutate(roi_pair = "amyg_hc") %>%
    mutate(first_region = "amyg") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  ofc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_ofc") %>%
    mutate(roi_pair = "ofc_amyg") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "amyg")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  ofc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_ofc") %>%
    mutate(roi_pair = "ofc_hc") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  cing_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_cing") %>%
    mutate(roi_pair = "cing_hc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  cing_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_cing") %>%
    mutate(roi_pair = "cing_amyg") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "amyg")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  cing_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_cing") %>%
    mutate(roi_pair = "cing_ofc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "ofc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  dlpfc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_dlpfc") %>%
    mutate(roi_pair = "dlpfc_hc") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  dlpfc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_dlpfc") %>%
    mutate(roi_pair = "dlpfc_amyg") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "amyg")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  dlpfc_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_dlpfc") %>%
    mutate(roi_pair = "dlpfc_ofc") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "ofc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  dlpfc_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_dlpfc") %>%
    mutate(roi_pair = "dlpfc_cing") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "cing")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  
  insula_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_insula") %>%
    mutate(roi_pair = "insula_hc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  insula_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_insula") %>%
    mutate(roi_pair = "insula_amyg") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "amyg")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  insula_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_insula") %>%
    mutate(roi_pair = "insula_ofc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "ofc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  insula_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_insula") %>%
    mutate(roi_pair = "insula_cing") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "cing")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  insula_dlpfc <- conn_sub_sig_df %>%
    filter(roi_pair == "dlpfc_insula") %>%
    mutate(roi_pair = "insula_dlpfc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "dlpfc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) %>%
    select(-first_pair, -second_pair)
  
  conn_sub_sig_df <- bind_rows(conn_sub_sig_df, amyg_hc, ofc_amyg, ofc_hc, cing_hc, cing_amyg, cing_ofc, dlpfc_hc, dlpfc_amyg, dlpfc_ofc, dlpfc_cing, insula_hc, insula_amyg, insula_ofc, insula_cing, insula_dlpfc)
  
  conn_sub_sig_df <- conn_sub_sig_df %>%
    mutate(second_region = factor(second_region, levels = c("hc", "amyg", "ofc", "cing", "dlpfc", "insula"), labels = c("Hippocampus", "Amygdala", "OFC", "Ant. Cingulate", "dlPFC", "Insula"))) %>%
    mutate(first_region = factor(first_region, levels = c("hc", "amyg", "ofc", "cing", "dlpfc", "insula"), labels = c("Hippocampus", "Amygdala", "OFC", "Ant. Cingulate", "dlPFC", "Insula")))
  
  return(conn_sub_sig_df)
}


prep_detailed_conn_allsubs_plot_df <- function(df){
  
  # filter to subject #
  conn_sub_df <- df %>%
    rename(pval = percent_sig) %>%
    arrange(pairs)
  
  # correct for time points #
  conn_sub_sig_df <- conn_sub_df %>%
    mutate(roi_pair = paste0(first_region, "_", second_region)) %>%
    group_by(subject, roi_pair, metric, pairs) %>%
    mutate(p_frd = p.adjust(pval, method = "fdr")) %>%
    mutate(sig = p_frd < 0.05) %>% 
    group_by(subject, roi_pair, metric, time) %>%
    mutate(count_sig = sum(sig)) %>%
    mutate(number_of_region_pairs = length(unique(pairs))) %>%
    mutate(percent_sig = count_sig / number_of_region_pairs * 100)
  
  # create symmetric data #
  amyg_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_amyg") %>%
    mutate(roi_pair = "amyg_hc") %>%
    mutate(first_region = "amyg") %>%
    mutate(second_region = "hc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  amyg_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_amyg") %>%
    mutate(roi_pair = "amyg_ofc") %>%
    mutate(first_region = "amyg") %>%
    mutate(second_region = "ofc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  amyg_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_amyg") %>%
    mutate(roi_pair = "amyg_cing") %>%
    mutate(first_region = "amyg") %>%
    mutate(second_region = "cing")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  amyg_insula <- conn_sub_sig_df %>%
    filter(roi_pair == "insula_amyg") %>%
    mutate(roi_pair = "amyg_insula") %>%
    mutate(first_region = "amyg") %>%
    mutate(second_region = "insula")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  amyg_mfg <- conn_sub_sig_df %>%
    filter(roi_pair == "mfg_amyg") %>%
    mutate(roi_pair = "amyg_mfg") %>%
    mutate(first_region = "amyg") %>%
    mutate(second_region = "dlpfc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  amyg_sfg <- conn_sub_sig_df %>%
    filter(roi_pair == "sfg_amyg") %>%
    mutate(roi_pair = "amyg_sfg") %>%
    mutate(first_region = "amyg") %>%
    mutate(second_region = "dlpfc")  %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  
  
  ofc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_ofc") %>%
    mutate(roi_pair = "ofc_amyg") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "amyg")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  ofc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_ofc") %>%
    mutate(roi_pair = "ofc_hc") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "hc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  ofc_insula <- conn_sub_sig_df %>%
    filter(roi_pair == "insula_ofc") %>%
    mutate(roi_pair = "ofc_insula") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "insula")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  ofc_mfg <- conn_sub_sig_df %>% 
    filter(roi_pair == "mfg_ofc") %>%
    mutate(roi_pair = "ofc_mfg") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "ofc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  ofc_sfg <- conn_sub_sig_df %>%
    filter(roi_pair == "sfg_ofc") %>%
    mutate(roi_pair = "ofc_sfg") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "ofc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  ofc_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_ofc") %>%
    mutate(roi_pair = "ofc_cing") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "ofc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  cing_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_cing") %>%
    mutate(roi_pair = "cing_hc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "hc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  cing_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_cing") %>%
    mutate(roi_pair = "cing_amyg") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "amyg")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  cing_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_cing") %>%
    mutate(roi_pair = "cing_ofc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "ofc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  cing_mfg <- conn_sub_sig_df %>%
    filter(roi_pair == "mfg_cing") %>%
    mutate(roi_pair = "cing_mfg") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "mfg")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  cing_insula <- conn_sub_sig_df %>%
    filter(roi_pair == "insula_cing") %>%
    mutate(roi_pair = "cing_insula") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "insula")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  cing_sfg <- conn_sub_sig_df %>%
    filter(roi_pair == "sfg_cing") %>%
    mutate(roi_pair = "cing_sfg") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "dlpfc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  mfg_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_mfg") %>%
    mutate(roi_pair = "mfg_hc") %>%
    mutate(first_region = "mfg") %>%
    mutate(second_region = "hc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  mfg_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_mfg") %>%
    mutate(roi_pair = "mfg_amyg") %>%
    mutate(first_region = "mfg") %>%
    mutate(second_region = "amyg")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  mfg_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_mfg") %>%
    mutate(roi_pair = "mfg_ofc") %>%
    mutate(first_region = "mfg") %>%
    mutate(second_region = "ofc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  mfg_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_mfg") %>%
    mutate(roi_pair = "mfg_cing") %>%
    mutate(first_region = "mfg") %>%
    mutate(second_region = "cing")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  mfg_insula <- conn_sub_sig_df %>%
    filter(roi_pair == "insula_mfg") %>%
    mutate(roi_pair = "mfg_insula") %>%
    mutate(first_region = "mfg") %>%
    mutate(second_region = "insula")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  sfg_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_sfg") %>%
    mutate(roi_pair = "sfg_hc") %>%
    mutate(first_region = "sfg") %>%
    mutate(second_region = "hc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  sfg_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_sfg") %>%
    mutate(roi_pair = "sfg_amyg") %>%
    mutate(first_region = "sfg") %>%
    mutate(second_region = "amyg")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  sfg_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_sfg") %>%
    mutate(roi_pair = "sfg_ofc") %>%
    mutate(first_region = "sfg") %>%
    mutate(second_region = "ofc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  sfg_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_sfg") %>%
    mutate(roi_pair = "sfg_cing") %>%
    mutate(first_region = "sfg") %>%
    mutate(second_region = "cing")    %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  sfg_insula <- conn_sub_sig_df %>%
    filter(roi_pair == "insula_sfg") %>%
    mutate(roi_pair = "sfg_insula") %>%
    mutate(first_region = "sfg") %>%
    mutate(second_region = "insula")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair))
  
  
  insula_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_insula") %>%
    mutate(roi_pair = "insula_hc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "hc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  insula_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_insula") %>%
    mutate(roi_pair = "insula_amyg") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "amyg")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  insula_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_insula") %>%
    mutate(roi_pair = "insula_ofc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "ofc")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  insula_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_insula") %>%
    mutate(roi_pair = "insula_cing") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "cing")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  insula_mfg <- conn_sub_sig_df %>%
    filter(roi_pair == "mfg_insula") %>%
    mutate(roi_pair = "insula_mfg") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "mfg")   %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  insula_sfg <- conn_sub_sig_df %>%
    filter(roi_pair == "sfg_insula") %>%
    mutate(roi_pair = "insula_sfg") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "sfg")    %>%
    mutate(first_pair = gsub("_to_.*", "", pairs)) %>%
    mutate(second_pair = gsub(".*_to_", "", pairs)) %>%
    mutate(pairs = paste0(second_pair, "_to_", first_pair)) 
  
  conn_sub_sig_df <- bind_rows(conn_sub_sig_df, 
                               amyg_hc, amyg_ofc, amyg_cing, amyg_mfg, amyg_sfg, amyg_insula, 
                               ofc_amyg, ofc_hc, ofc_cing, ofc_mfg, ofc_sfg, ofc_insula,
                               cing_hc, cing_amyg, cing_ofc, cing_mfg, cing_sfg, cing_insula,
                               mfg_hc, mfg_amyg, mfg_ofc, mfg_cing, mfg_insula,
                               sfg_hc, sfg_amyg, sfg_ofc, sfg_cing, sfg_insula,
                               insula_hc, insula_amyg, insula_ofc, insula_cing, insula_mfg, insula_sfg)
  
  conn_sub_sig_df <- conn_sub_sig_df %>%
    distinct() %>%
    mutate(second_region = factor(second_region, levels = c("hc", "amyg", "ofc", "cing", "mfg", "sfg", "insula"), labels = c("Hippocampus", "Amygdala", "OFC", "Ant. Cingulate", "MFG", "SFG", "Insula"))) %>%
    mutate(first_region = factor(first_region, levels = c("hc", "amyg", "ofc", "cing", "mfg", "sfg", "insula"), labels = c("Hippocampus", "Amygdala", "OFC", "Ant. Cingulate", "MFG", "SFG", "Insula")))
  
  return(conn_sub_sig_df)
}

