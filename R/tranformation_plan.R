# data curation plan
transformation_plan <- list(
  # mean climate
  tar_target(
    name = gridded_climate,
    command = {
      dat <- gridded_climate_raw |>
        filter(
          variable %in% c("temperature", "precipitation"),
          year(date) %in% c(2008:2019)
        ) |>
        pivot_wider(names_from = variable, values_from = value) |>
        mutate(year = year(date))

      left_join(
        dat |>
          # annual precipitation
          group_by(year, siteID) |>
          summarise(precipitation = sum(precipitation, na.rm = TRUE)) |>
          ungroup() |>
          group_by(siteID) |>
          summarise(precipitation = mean(precipitation, na.rm = TRUE)),
        # summer temperature
        dat |>
          mutate(month = month(date)) |>
          filter(month %in% c(6, 7, 8)) |>
          group_by(year, month, siteID) |>
          summarise(temperature = mean(temperature)) |>
          ungroup() |>
          group_by(siteID) |>
          summarise(temperature = mean(temperature)),
        by = "siteID"
      )
    }
  ),

  # prep aboveground biomass
  # Removed biomass (2015-2022)
  tar_target(
    name = biomass,
    command = removed_biomass_raw |>
      # Clean 2018 Ovstedalen duplicates
      clean_ovstedalen_2018_duplicates() |>
      mutate(
        # standardise FG → GF in treatment and plotID
        treatment = if_else(treatment == "FG", "GF", treatment),
        plotID = if_else(str_detect(plotID, "FG"), str_replace(plotID, "FG", "GF"), plotID)
      ) |>
      # remove extra plots in 2016
      filter(treatment != "XC") %>%
      # fix blockID
      funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE) |>
      # remove invalid removed_fg rows
      remove_invalid_removed_fg_rows() |>
      mutate(
        fg_status = "removed",
        # scale precipitation and temperature
        precipitation_scaled = precipitation / 1000,
        temperature_scaled = temperature / 10
      )
  ),

  # prep standingbiomass (2022 only)
  tar_target(
    name = biomass_22,
    command = removed_biomass_final_raw %>%
      # convert to Funder format
      funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE) |>
      # remove litter
      filter(removed_fg != "L") |>
      # annotate 2022 fg status
      annotate_2022_fg_status() |>
      select(-no_treatment) |>
      # scale precipitation and temperature
      mutate(
        precipitation_scaled = precipitation / 1000,
        temperature_scaled = temperature / 10
      )
  ),

  # prep standing biomass 2022 (contains remaining biomass)
  tar_target(
    name = standing_biomass_22,
    command = biomass_22 |>
      # only keep remaining biomass and FGB (bare ground)
      filter(fg_status == "remaining" | fg_removed == "FGB") |>
      mutate(biomass = if_else(fg_removed == "FGB", 0, biomass)) |>
      mutate(fg_removed = factor(fg_removed,
        levels = c("none", "G", "F", "B", "GF", "GB", "FB", "FGB")
      ))
  ),

  # filter for biomass in 2015
  tar_target(
    name = removed_biomass,
    command = biomass |>
      filter(year == 2015) |>
      # sum biomass across the growing season
      group_by(siteID, blockID, plotID, fg_removed, removed_fg, fg_remaining, fg_richness, fg_status, temperature_level, precipitation_level, temperature_scaled, precipitation_scaled) |>
      summarise(removed_biomass = sum(biomass), .groups = "drop")
  ),

  # standardised removed biomass by site
  tar_target(
    name = standardised_removed_biomass_site,
    command = {
      # Get control biomass per site and FG
      control_biomass_site <- removed_biomass |>
        filter(fg_removed == "FGB") |>
        group_by(siteID, removed_fg, fg_status) |>
        summarise(mean_removed_biomass_ctrl_site = mean(removed_biomass), .groups = "drop") |>
        group_by(siteID, fg_status) |>
        mutate(total_removed_biomass_ctrl_site = sum(mean_removed_biomass_ctrl_site)) |>
        ungroup()

      # Join and calculate proportion (include FGB so figures can show full-removal plots)
      removed_biomass |>
        tidylog::left_join(control_biomass_site, by = c("siteID", "removed_fg", "fg_status")) |>
        mutate(prop_removed_biomass_trt = removed_biomass / total_removed_biomass_ctrl_site)
    }
  ),


  # make community data, impute missing cover values, construct FG cover coefficients
  tar_target(
    name = community,
    command = fg_cleaning(community_raw, gridded_climate, species_corrections_raw) |>
      make_fg_cover_coefficients()
  ),


  # prep cover
  tar_target(
    name = cover_data,
    command = community |>
      # remove extra plots in 2016
      filter(fg_removed != "XC") |>
      select(year:fg_removed, species, cover, functional_group, temperature_level:fg_remaining)
  ),
  tar_target(
    name = fg_cover,
    command = community |>
      # remove extra plots in 2016
      filter(fg_removed != "XC") |>
      select(year:fg_removed, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes) |>
      tidylog::distinct() |>
      # remove duplicates
      dplyr::mutate(n = dplyr::n(), .by = c(year, siteID, blockID, plotID, removal, fg_removed)) |>
      tidylog::filter(!c(n == 2 & is.na(total_graminoids))) |>
      # remove last duplicate
      filter(!c(year == 2019 & plotID == "Alr3C" & total_bryophytes == 2))
  ),


  # calculate diversity metrics
  tar_target(
    name = diversity,
    command = calc_diversity(cover_data)
  ),
  tar_target(
    name = traits,
    command = traits_raw |>
      select(-date, -flag) |>
      # log transform size traits
      mutate(
        value_trans = if_else(
          trait %in% c(
            "height",
            "fresh_mass",
            "dry_mass",
            "leaf_area",
            "leaf_thickness"
          ),
          true = suppressWarnings(log(value)), # suppress warnings from log(-value) in isotopes (these are calculated but not kept)
          false = value
        ),
        trait_trans = recode(
          trait,
          "height" = "height_log",
          "fresh_mass" = "fresh_mass_log",
          "dry_mass" = "dry_mass_log",
          "leaf_area" = "leaf_area_log",
          "leaf_thickness" = "leaf_thickness_log"
        ),
        trait_trans = factor(trait_trans,
          levels = c("height_log", "fresh_mass_log", "dry_mass_log", "leaf_area_log", "leaf_thickness_log", "SLA", "LDMC", "C", "N", "CN_ratio", "d13C", "d15N")
        )
      ) %>%
      make_fancy_data(., gridded_climate, fix_treatment = FALSE)
  ),

  # bootstrapping
  # trait imputation
  tar_target(
    name = imputed_traits,
    command = make_trait_impute(cover_data, traits)
  ),

  # bootstrapping for CWM
  tar_target(
    name = trait_means,
    command = make_bootstrapping(imputed_traits)
  ),

  # analysis data

  # 2022 biomass reshaped and wide
  tar_target(
    name = standing_biomass_22_wide,
    command = standing_biomass_22 |>
      pivot_wider(names_from = removed_fg, values_from = biomass, values_fill = 0) |>
      mutate(
        B_treat = as.integer(str_detect(fg_removed, stringr::fixed("B"))),
        G_treat = as.integer(str_detect(fg_removed, stringr::fixed("G"))),
        F_treat = as.integer(str_detect(fg_removed, stringr::fixed("F")))
      ) |>
      group_by(plotID) |>
      mutate(total_biomass = sum(F, G, B)) |>
      ungroup() |>
      tidylog::filter(fg_removed != "FGB")
  ),
  tar_target(
    name = standing_biomass_22_cover_15,
    command = standing_biomass_22_wide |>
      tidylog::left_join(fg_cover |> filter(year == 2015) |> select(siteID, blockID, total_graminoids, total_forbs, total_bryophytes)) |>
      rename(
        G_cover = total_graminoids,
        F_cover = total_forbs,
        B_cover = total_bryophytes
      )
  )
)
