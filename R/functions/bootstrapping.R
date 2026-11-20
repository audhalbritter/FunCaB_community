## Bootstrapping

# trait imputation
make_trait_impute <- function(cover_data, traits){

  #prepare community data
  comm <- cover_data |>
    filter(year == 2019,
    !is.na(cover))

  #prepare trait data
  trait <- traits

  #set seed for bootstrapping repeatability
  set.seed(1525)
  trait_fill(
    comm = comm,
    traits = trait,
    scale_hierarchy = c("siteID"),
    taxon_col = "species",
    value_col = "value_trans",
    trait_col = "trait_trans",
    abundance_col = "cover",
    other_col = c("blockID", "plotID", "fg_removed", "fg_remaining"),
    min_n_in_sample = 2
  )


}


# modified bootstrap summary to accommodation grouping over functional groups within plots
traits_summarise_moments_fg <- function(
    bootstrap_moments,
    groups = NULL,
    parametric = TRUE,
    sd_mult = 1,
    ci = 0.95
) {
  attrib <- attr(bootstrap_moments, "attrib")

  if (is.null(groups)) {
    groups <- c(
      as.character(attrib$scale_hierarchy),
      attrib$trait_col,
      attrib$other_col
    )

    if (!is.null(attrib$treatment_col)) {
      groups <- c(groups, paste0(attrib$treatment_col, "_comm"))
    }
  }

  groups <- unique(groups[!is.na(groups) & nzchar(groups)])

  missing_groups <- setdiff(groups, names(bootstrap_moments))
  if (length(missing_groups) > 0) {
    stop(
      "Grouping columns not found: ",
      paste(missing_groups, collapse = ", ")
    )
  }

  get_ci <- getFromNamespace("get_ci", "traitstrap")

  bootstrap_moments |>
    dplyr::ungroup() |>
    dplyr::rename(MEAN = "mean") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(
      n = dplyr::n(),

      mean = mean(.data$MEAN),
      ci_low_mean = get_ci(.data$MEAN, sd_mult, ci, "low", parametric),
      ci_high_mean = get_ci(.data$MEAN, sd_mult, ci, "high", parametric),

      var = mean(.data$variance),
      ci_low_var = get_ci(.data$variance, sd_mult, ci, "low", parametric),
      ci_high_var = get_ci(.data$variance, sd_mult, ci, "high", parametric),

      skew = mean(.data$skewness),
      ci_low_skew = get_ci(.data$skewness, sd_mult, ci, "low", parametric),
      ci_high_skew = get_ci(.data$skewness, sd_mult, ci, "high", parametric),

      kurt = mean(.data$kurtosis),
      ci_low_kurt = get_ci(.data$kurtosis, sd_mult, ci, "low", parametric),
      ci_high_kurt = get_ci(.data$kurtosis, sd_mult, ci, "high", parametric),

      .groups = "drop"
    )
}



# bootstrapping
make_bootstrapping <- function(imputed_traits){

  # trait_imp <- wg_trait_impute
  CWM <- imputed_traits |>
    group_by(global, siteID, trait_trans, blockID, plotID, fg_removed, functional_group) |>
    trait_np_bootstrap(nrep = 100, sample_size = 200)

  CWM_mean_global <- CWM |>
    trait_summarise_boot_moments() %>%
    dplyr::ungroup() |>
    select(siteID:mean, var, skew, kurt, -n) |>
    rename(mean_global = mean, var_global = var, skew_global = skew, kurt_global = kurt)

    CWM_mean_functional_group <- CWM |>
    traits_summarise_moments_fg(groups = c("global", "siteID", "trait_trans", "blockID", "plotID", "fg_removed", "functional_group")) %>%
    dplyr::ungroup() |>
    select(siteID:mean, var, skew, kurt, -n)

    CWM_mean <- tidylog::full_join(CWM_mean_global, CWM_mean_functional_group, by = join_by(siteID, trait_trans, blockID, plotID, fg_removed))

  # #prepare bootstrapped trait data for analyses
  traitMean <- CWM_mean %>%
    fancy_trait_name_dictionary(.) |>
    dplyr::ungroup()

}
