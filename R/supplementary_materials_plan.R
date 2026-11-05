# testing for outliers in imputed values
# community |> select(year:fg_removed, fg_remaining, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes, moss_height_imputed, site_moss_height) |> distinct() |> mutate(diff_height = moss_height_imputed - site_moss_height, is_imputed = if_else(moss_height_imputed == site_moss_height, TRUE, FALSE))|> filter(year == 2016) |> ggplot(aes(moss_height_imputed, fill = is_imputed)) + geom_histogram()

si_plan <- list(
      # make pca
  tar_target(
    name = community_pca,
    command = make_sp_pca(cover_data)
  ),

  tar_target(
    name = fig_pca,
    command = make_sp_pca_figure(community_pca)
  )
)