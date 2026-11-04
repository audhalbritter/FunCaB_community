# make fancy data

make_fancy_data <- function(data, gridded_climate, fix_treatment = TRUE){

  data2 <- data |>

    # add temperature and precipitation levels
    mutate(temperature_level = case_when(siteID %in% c("Ulvehaugen", "Skjelingahaugen", "Lavisdalen", "Gudmedalen") ~ "alpine",
                                          siteID %in% c("Alrust", "Veskre", "Rambera", "Hogsete") ~ "sub-alpine",
                                          TRUE ~ "boreal"),
          temperature_level = factor(temperature_level,
            levels = c("alpine", "sub-alpine", "boreal")),
          precipitation_level = case_when(siteID %in% c("Fauske", "Alrust", "Ulvehaugen") ~ 1,
                                             siteID %in% c("Vikesland", "Hogsete", "Lavisdalen") ~ 2,
                                             siteID %in% c("Arhelleren", "Rambera", "Gudmedalen") ~ 3,
                                             TRUE ~ 4)) |>

    # # add gridded climate data
    left_join(gridded_climate, by = "siteID")


  if(fix_treatment == TRUE){

     # convert treatment to FG present
    data2 <- data2 |>
      mutate(fg_remaining = case_when(
      treatment == "C" ~ "FGB",
      treatment == "FB" ~ "G",
      treatment == "FGB" ~ "none",
      treatment == "GB" ~ "F",
      treatment == "F" ~ "GB",
      treatment == "GF" ~ "B",
      treatment == "B" ~ "GF",
      treatment == "G" ~ "FB",
      treatment == "XC" ~ "FGB",
      TRUE ~ NA_character_)) |>
      rename(fg_removed = treatment) |>
      mutate(fg_removed = if_else(fg_removed == "C", "none", fg_removed)) |>
      # add number of functional groups
      mutate(fg_richness = case_when(fg_removed == "none" ~ 3,
                                             fg_removed %in% c("F", "G", "B") ~ 2,
                                             fg_removed %in% c("FB", "GF", "GB") ~ 1,
                                             fg_removed == "FGB" ~ 0))

  } else {
    data2 <- data2
  }


}



#' Clean compensation-model term names for tables/figures
#' @param tidy_df A tidy model output with a `term` column
#' @return The same data frame with shortened term labels
#'   (G_treat/F_treat/B_treat → G/F/B; temperature_scaled → T; precipitation_scaled → P)
clean_comp_model_terms <- function(tidy_df) {
  tidy_df |>
    dplyr::mutate(
      term = term |>
        stringr::str_replace_all("G_treat", "G") |>
        stringr::str_replace_all("F_treat", "F") |>
        stringr::str_replace_all("B_treat", "B") |>
        stringr::str_replace_all("temperature_scaled", "T") |>
        stringr::str_replace_all("precipitation_scaled", "P")
    )
}

#' Turn emmeans trt.vs.ctrl contrast labels into readable removal names
#' @param contrast Character/factor vector of contrasts like
#'   "G_treat1 F_treat0 B_treat0 / G_treat0 F_treat0 B_treat0"
#' @return Character vector, e.g. "Graminoids", "Graminoids + forbs"
#'   (always relative to none removed)
clean_comp_contrast_labels <- function(contrast) {
  left <- stringr::str_split_fixed(as.character(contrast), " / ", n = 2)[, 1]

  purrr::map_chr(left, function(x) {
    removed <- c(
      G = stringr::str_detect(x, "G_treat1"),
      F = stringr::str_detect(x, "F_treat1"),
      B = stringr::str_detect(x, "B_treat1")
    )
    names_removed <- c(G = "Graminoids", F = "Forbs", B = "Bryophytes")[removed]

    if (length(names_removed) == 0) {
      "None"
    } else {
      paste(names_removed, collapse = " + ")
    }
  })
}


#rename traits to fancy names for figures

fancy_trait_name_dictionary <- function(dat){

  dat <- dat %>%
    # make fancy names with units
    mutate(trait_fancy = recode(trait_trans,
      height_log = "Height cm",
      fresh_mass_log = "Wet mass g",
      dry_mass_log = "Dry mass g",
      leaf_area_log = "Area cm2",
      leaf_thickness_log = "Thickness mm",
      LDMC = "LDMC g/g",
      SLA = "SLA cm2/g",
      N = "N %",
      C = "C %",
      CN_ratio = "CN",
      d13C = "δC13 ‰",
      d15N = "δN15 ‰")) |>
    # make names that can be used in figures with unit
        mutate(figure_names = case_when(
          trait_trans == "height_log" ~ "Plant~height~(cm)",
          trait_trans == "fresh_mass_log" ~ "Leaf~wet~mass~(g)",
          trait_trans == "dry_mass_log" ~ "Leaf~dry~mass~(g)",
          trait_trans == "leaf_area_log" ~ "Leaf~area~(cm^2)",
          trait_trans == "leaf_thickness_log" ~ "Leaf~thickness~(mm)",
          trait_trans == "LDMC" ~ "LDMC~(gg^{-1})",
          trait_trans == "SLA" ~ "SLA~(cm^2*g^{-1})",
          trait_trans == "C" ~ "C~('%')",
          trait_trans == "N" ~ "N~('%')",
          trait_trans == "CN_ratio" ~ "CN",
          trait_trans == "d13C" ~ "δ^{13}~C~'(‰)'",
          trait_trans == "d15N" ~ "δ^{15}~N~'(‰)'"
        )) |>

    # order for trait_trans
    mutate(trait_trans = factor(trait_trans,
          levels = c("height_log", "fresh_mass_log", "dry_mass_log", "leaf_area_log", "leaf_thickness_log", "LDMC", "SLA", "C", "N", "CN_ratio", "d13C", "d15N"))) |>

    # order for figure names
    mutate(figure_names = factor(figure_names,
                                 levels = c("Plant~height~(cm)",
                                 "Leaf~dry~mass~(g)",
                                 "Leaf~area~(cm^2)",
                                 "Leaf~thickness~(mm)",
                                 "SLA~(cm^2*g^{-1})",
                                 "LDMC~(gg^{-1})",
                                 "C~('%')",
                                 "N~('%')",
                                 "CN",
                                "δ^{13}~C~'(‰)'",
                                "δ^{15}~N~'(‰)'"))) |>
    # add class
    mutate(class = case_when(trait_trans %in% c("height_log", "fresh_mass_log", "dry_mass_log", "leaf_area_log", "leaf_thickness_log") ~ "Size",
                             trait_trans %in% c("d13C", "d15N") ~ "Isotopes",
                             TRUE ~ "Leaf economics"),
           class = factor(class, levels = c("Size", "Leaf economics", "Isotopes")))

}
