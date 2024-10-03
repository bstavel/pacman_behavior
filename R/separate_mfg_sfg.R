separate_mfg_sfg <- function(df) {

  # load csv with mfg, sfg electrodes
  regions_df <- read_csv(path(here(), "munge", "mni_coordinates_all_subs_with_detailed_regions.csv"))
  
  # create mfg df
  mfg_df <- regions_df %>%
    filter(region == "mfg") %>%
    mutate(elec_id = paste0(subject, "_", Electrode))
  
  # create sfg df
  sfg_df <- regions_df %>%
    filter(region == "sfg") %>%
    mutate(elec_id = paste0(subject, "_", Electrode))
  
  # split electrode column into two and create columns for mfg, sfg
  detailed_df <- df %>%
    mutate(
      elec_1 = gsub("-.*", "", electrode),
      elec_2 = gsub(".*-", "", electrode),
    ) %>%
    mutate(
      mfg = if_else(
          paste0(subject, "_", elec_1) %in% mfg_df$elec_id |
          paste0(subject, "_", elec_2) %in%  mfg_df$elec_id,
        1, 0),
      sfg = if_else(
          paste0(subject, "_", elec_1) %in% sfg_df$elec_id |
          paste0(subject, "_", elec_2) %in%  sfg_df$elec_id,
        1, 0)
    ) %>%
    select(-elec_1, -elec_2)
  
  return(detailed_df)

}
