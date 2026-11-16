# download_data
download_plan <- list(
  # download data

  # biomass data
  # biomass 2015-2021
  #   tar_target(
  #   name = removed_biomass_download,
  #   command =  get_file(node = "4c5v2",
  #                       file = "FunCaB_clean_biomass_2015-2021.csv",
  #                       path = "data",
  #                       remote_path = "1_Biomass_removal"),
  # format = "file"
  # ),

  # # biomass 2022
  # tar_target(
  #   name = removed_biomass_final_download,
  #   command =  get_file(node = "tx9r2",
  #                       file = "FUNDER_clean_standing_biomass_2022.csv",
  #                       path = "data",
  #                       remote_path = "1_Vegetation/Clean_data"),
  # format = "file"
  # ),

  # community composition
  tar_target(
    name = community_download,
    command = get_file(
      node = "4c5v2",
      file = "FunCaB_clean_composition_2015-2019.csv",
      path = "data",
      remote_path = "3_Plant_composition"
    ),
    format = "file"
  ),

  # species corrections
  tar_target(
    name = species_corrections_download,
    command = get_file(
      node = "tx9r2",
      file = "FUNCAB_species_corrections.csv",
      path = "data",
      remote_path = "z_misc"
    ),
    format = "file"
  ),

  # plant functional traits
  tar_target(
    name = traits_download,
    command = get_file(
      node = "npfa9",
      file = "VCG_clean_trait_data_2012-2016.csv",
      path = "data",
      remote_path = "5_Trait_data"
    ),
    format = "file"
  ),

  # climate data
  tar_target(
    name = gridded_climate_download,
    command = get_file(
      node = "npfa9",
      file = "VCG_clean_gridded_daily_climate_2008-2022.csv",
      path = "data",
      remote_path = "8_Environmental_data"
    ),
    format = "file"
  ),

  # import data
  # biomass removal 2015 -2022
  tar_target(
    name = removed_biomass_raw,
    command = read_csv(here::here("data/i_FUNDER_clean_biomass_removal_2015-2022.csv"))
  ),
  # standing biomass2022
  tar_target(
    name = removed_biomass_final_raw,
    command = read_csv(here::here("data/i_FUNDER_clean_standing_biomass_2022.csv"))
  ),

  # belowground biomass
  tar_target(
    name = belowground_biomass_raw,
    command = read_csv(here::here("data/ii_FUNDER_clean_root_biomass_productivity_traits_2021-2022.csv"))
  ),

  # community composition
  tar_target(
    name = community_raw,
    command = read_csv(community_download)
  ),

  # community composition 2022
  tar_target(
    name = community22_raw,
    command = read_csv(here::here("data/iv_FUNDER_clean_community_2015-2022.csv"))
  ),

  # species corrections
  tar_target(
    name = species_corrections_raw,
    command = read_csv(species_corrections_download)
  ),

  # plant functional traits
  tar_target(
    name = traits_raw,
    command = read_csv(traits_download)
  ),

  # climate data
  tar_target(
    name = gridded_climate_raw,
    command = read_csv(gridded_climate_download)
  )
)
