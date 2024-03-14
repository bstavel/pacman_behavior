
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
    mutate(second_region = "hc")
  
  ofc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_ofc") %>%
    mutate(roi_pair = "ofc_amyg") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "amyg")
  
  ofc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_ofc") %>%
    mutate(roi_pair = "ofc_hc") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "hc")
  
  cing_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_cing") %>%
    mutate(roi_pair = "cing_hc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "hc")
  
  cing_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_cing") %>%
    mutate(roi_pair = "cing_amyg") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "amyg")
  
  cing_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_cing") %>%
    mutate(roi_pair = "cing_ofc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "ofc")
  
  dlpfc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_dlpfc") %>%
    mutate(roi_pair = "dlpfc_hc") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "hc")
  
  dlpfc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_dlpfc") %>%
    mutate(roi_pair = "dlpfc_amyg") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "amyg")
  
  dlpfc_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_dlpfc") %>%
    mutate(roi_pair = "dlpfc_ofc") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "ofc")
  
  dlpfc_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_dlpfc") %>%
    mutate(roi_pair = "dlpfc_cing") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "cing")
  
  
  insula_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_insula") %>%
    mutate(roi_pair = "insula_hc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "hc")
  
  insula_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_insula") %>%
    mutate(roi_pair = "insula_amyg") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "amyg")
  
  insula_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_insula") %>%
    mutate(roi_pair = "insula_ofc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "ofc")
  
  insula_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_insula") %>%
    mutate(roi_pair = "insula_cing") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "cing")
  
  insula_dlpfc <- conn_sub_sig_df %>%
    filter(roi_pair == "dlpfc_insula") %>%
    mutate(roi_pair = "insula_dlpfc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "dlpfc")
  
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
    mutate(second_region = "hc")
  
  ofc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_ofc") %>%
    mutate(roi_pair = "ofc_amyg") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "amyg")
  
  ofc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_ofc") %>%
    mutate(roi_pair = "ofc_hc") %>%
    mutate(first_region = "ofc") %>%
    mutate(second_region = "hc")
  
  cing_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_cing") %>%
    mutate(roi_pair = "cing_hc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "hc")
  
  cing_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_cing") %>%
    mutate(roi_pair = "cing_amyg") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "amyg")
  
  cing_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_cing") %>%
    mutate(roi_pair = "cing_ofc") %>%
    mutate(first_region = "cing") %>%
    mutate(second_region = "ofc")
  
  dlpfc_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_dlpfc") %>%
    mutate(roi_pair = "dlpfc_hc") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "hc")
  
  dlpfc_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_dlpfc") %>%
    mutate(roi_pair = "dlpfc_amyg") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "amyg")
  
  dlpfc_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_dlpfc") %>%
    mutate(roi_pair = "dlpfc_ofc") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "ofc")
  
  dlpfc_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_dlpfc") %>%
    mutate(roi_pair = "dlpfc_cing") %>%
    mutate(first_region = "dlpfc") %>%
    mutate(second_region = "cing")
  
  
  insula_hc <- conn_sub_sig_df %>%
    filter(roi_pair == "hc_insula") %>%
    mutate(roi_pair = "insula_hc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "hc")
  
  insula_amyg <- conn_sub_sig_df %>%
    filter(roi_pair == "amyg_insula") %>%
    mutate(roi_pair = "insula_amyg") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "amyg")
  
  insula_ofc <- conn_sub_sig_df %>%
    filter(roi_pair == "ofc_insula") %>%
    mutate(roi_pair = "insula_ofc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "ofc")
  
  insula_cing <- conn_sub_sig_df %>%
    filter(roi_pair == "cing_insula") %>%
    mutate(roi_pair = "insula_cing") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "cing")
  
  insula_dlpfc <- conn_sub_sig_df %>%
    filter(roi_pair == "dlpfc_insula") %>%
    mutate(roi_pair = "insula_dlpfc") %>%
    mutate(first_region = "insula") %>%
    mutate(second_region = "dlpfc")
  
  conn_sub_sig_df <- bind_rows(conn_sub_sig_df, amyg_hc, ofc_amyg, ofc_hc, cing_hc, cing_amyg, cing_ofc, dlpfc_hc, dlpfc_amyg, dlpfc_ofc, dlpfc_cing, insula_hc, insula_amyg, insula_ofc, insula_cing, insula_dlpfc)
  
  conn_sub_sig_df <- conn_sub_sig_df %>%
    mutate(second_region = factor(second_region, levels = c("hc", "amyg", "ofc", "cing", "dlpfc", "insula"), labels = c("Hippocampus", "Amygdala", "OFC", "Ant. Cingulate", "dlPFC", "Insula"))) %>%
    mutate(first_region = factor(first_region, levels = c("hc", "amyg", "ofc", "cing", "dlpfc", "insula"), labels = c("Hippocampus", "Amygdala", "OFC", "Ant. Cingulate", "dlPFC", "Insula")))
  
  return(conn_sub_sig_df)
}
