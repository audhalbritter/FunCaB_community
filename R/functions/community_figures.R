# community figures

make_sp_pca_figure <- function(community_pca){

  e_B <- eigenvals(community_pca[[3]])/sum(eigenvals(community_pca[[3]]))

  species <- community_pca[[2]] |>
    mutate(length = sqrt(pc1^2 + pc2^2),
           label = capitalize(label)) |>
    filter(length > 0.7) |>
    select(label, length)

  community_pca[[1]] |>
    ggplot(aes(x = pc1, y = pc2, colour = fg_removed)) +
    # ## arrows
    # geom_segment(data = pca_sp[[2]], aes(x = 0, y = 0, xend = pc1, yend = pc2),
    #              arrow=arrow(length=unit(0.2,"cm")),
    #              alpha = 0.75, color = 'grey70') +
    # points and path
    geom_point(aes(shape = ifelse(year == min(as.numeric(year)), "First", "Other")))+ 
    scale_shape_manual(name = "Year", values = c(First = 16, Other = 1)) +
    geom_path(aes(linetype = fg_removed, group = plotID)) +
    coord_equal() +
    # scale_linetype_manual(values = c("dashed", "solid")) +
    # scale_shape_manual(values = c(1, 17)) +
    labs(
      x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
      y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)"),
      colour = "Removal",
      linetype = "Removal",
      shape = "Year"
    ) +
    facet_grid(temperature_level ~ precipitation_level) +
    theme_bw() +
    theme(text = element_text(size = 13),
          legend.box = "vertical",
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

}

make_diversity_figure <- function(cover){

  diversity |> 
    ggplot(aes(x = treatment, y = richness, fill = treatment)) +
    geom_boxplot() +
    facet_grid(rows = vars(precipitation_level),
                cols = vars(temperature_level))
  
}

