# Figure plan for FunCaB Community Analysis
figure_plan <- list(

# functional group richness
  tar_target(
    name = fig_fg_richness_effects,
    command = plot_model_effects(fg_richness_analysis$model_2way)
  ),

  ## Part 2

  # fg richness focals
  tar_target(
    name = fig_fg_richness_focal_effects,
    command = plot_tidy_effects_facet(data = fg_richness_focal_tidy,
                                      facet_var = "focal_fg",
                                      term_order = rev(c("fg_richness", "T", "P", "fg_richness:T", "fg_richness:P", "P:T")))
  ),

  tar_target(
    name = fig_removed_biomass_stacked,
    command = {
      standardised_removed_biomass_site %>%
        mutate(fg_removed = factor(fg_removed, levels = c("G", "F", "B", "GF", "GB", "FB"))) %>%
        ggplot(aes(x = fg_removed, y = removed_biomass, fill = removed_fg)) +
        geom_col() +
        scale_fill_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Removed FG"
        ) +
        facet_grid(temperature_level ~ precipitation_level) +
        theme_bw() +
        labs(
          x = "Functional group removed",
          y = "Removed biomass"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  ),

  tar_target(
    name = fig_prop_removed_biomass_stacked,
    command = {
      standardised_removed_biomass_site %>%
        mutate(fg_removed = factor(fg_removed, levels = c("G", "F", "B", "GF", "GB", "FB"))) %>%
        ggplot(aes(x = fg_removed, y = prop_removed_biomass_trt, fill = removed_fg)) +
        geom_col() +
        scale_fill_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Removed FG"
        ) +
        facet_grid(temperature_level ~ precipitation_level) +
        theme_bw() +
        labs(
          x = "Functional group removed",
          y = "Proportion removed biomass"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  ),

  tar_target(
    name = fig_removed_biomass_comparison,
    command = {
      standardised_removed_biomass_site %>%
        ggplot(aes(x = removed_biomass, y = prop_removed_biomass_trt, color = removed_fg)) +
        geom_point(alpha = 0.6, size = 2) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
        scale_color_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Functional group"
        ) +
        facet_grid(temperature_level ~ precipitation_level) +
        theme_bw() +
        labs(
          x = "Removed biomass (raw)",
          y = "Proportional removed biomass (standardized)"
        )
    }
  ),

  tar_target(
    name = fig_control_biomass_by_site,
    command = {
      removed_biomass %>%
        filter(fg_removed == "FGB") %>%
        group_by(siteID, removed_fg) %>%
        summarise(total_control_biomass = sum(removed_biomass), .groups = "drop") %>%
        ggplot(aes(x = siteID, y = total_control_biomass, fill = removed_fg)) +
        geom_col() +
        scale_fill_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Functional group"
        ) +
        theme_bw() +
        labs(y = "Total control biomass", x = "Site") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  ),

  tar_target(
    name = fig_total_biomass_treatment_temperature,
    command = {
      ggplot(standing_biomass_22_wide, aes(x = fg_removed, y = total_biomass)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.12, alpha = 0.6) +
        labs(
          x = "Functional groups removed",
          y = "Total biomass"
        ) +
        theme_classic() + facet_grid(~temperature_level)
    }
  ),

  tar_target(
    name = fig_B_temperature_contrast,
      command = ggplot(
        B_comp_contrasts_temperature,
        aes(x = temperature_scaled, y = estimate)
      ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_pointrange(
        aes(ymin = asymp.LCL, ymax = asymp.UCL)
      ) +
      labs(
        x = "Scaled temperature",
        y = "G × F interaction in bryophyte biomass (g m⁻²)" # is this the correct unit?
      ) +
      theme_classic()+
      facet_wrap(~contrast)
  ),

  tar_target(
    name = fig_B_precipitation_contrast,
      command = ggplot(
        B_comp_contrasts_precipitation,
        aes(x = precipitation_scaled, y = estimate)
      ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_pointrange(
        aes(ymin = asymp.LCL, ymax = asymp.UCL)
      ) +
      labs(
        x = "Scaled precipitation",
        y = "G × F interaction in bryophyte biomass (g m⁻²)" # is this the correct unit?
      ) +
      theme_classic()+
      facet_wrap(~contrast)
  ),

  tar_target(
    name = fig_G_comp_precipitation_contrast,
    command = G_comp_contrasts_precipitation |>
      ggplot(
        aes(
          x = precipitation_scaled,
          y = estimate,
          colour = contrast,
          fill = contrast,
          group = contrast
        )
      ) +
      geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
      geom_ribbon(
        aes(ymin = asymp.LCL, ymax = asymp.UCL),
        alpha = 0.15,
        colour = NA
      ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Precipitation",
        y = "G biomass relative to control",
        colour = "Contrast",
        fill = "Contrast"
      ) +
      theme_classic() +
      facet_grid(~category)
  ),

  tar_target(
    name = fig_G_comp_temperature_contrast,
    command = G_comp_contrasts_temperature |>
      ggplot(
        aes(
          x = temperature_scaled,
          y = estimate,
          colour = contrast,
          fill = contrast,
          group = contrast
        )
      ) +
      geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
      geom_ribbon(
        aes(ymin = asymp.LCL, ymax = asymp.UCL),
        alpha = 0.15,
        colour = NA
      ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Temperature",
        y = "G biomass relative to control",
        colour = "Contrast",
        fill = "Contrast"
      ) +
      theme_classic() +
      facet_grid(~category)
  ),

  tar_target(
    name = fig_B_comp_temperature_contrast,
    command = B_comp_contrasts_temperature |>
      ggplot(
        aes(
          x = temperature_scaled,
          y = estimate,
          colour = contrast,
          fill = contrast,
          group = contrast
        )
      ) +
      geom_hline(
        yintercept = 1,
        linetype = 2,
        colour = "grey40"
      ) +
      geom_ribbon(
        aes(
          ymin = asymp.LCL,
          ymax = asymp.UCL
        ),
        alpha = 0.15,
        colour = NA
      ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Temperature",
        y = "B biomass relative to control",
        colour = "Contrast",
        fill = "Contrast"
      ) +
      theme_classic() +
      facet_grid(~category)
  ),

  tar_target(
    name = fig_B_comp_precipitation_contrast,
    command = B_comp_contrasts_precipitation |>
      ggplot(
        aes(
          x = precipitation_scaled,
          y = estimate,
          colour = contrast,
          fill = contrast,
          group = contrast
        )
      ) +
      geom_hline(
        yintercept = 1,
        linetype = 2,
        colour = "grey40"
      ) +
      geom_ribbon(
        aes(
          ymin = asymp.LCL,
          ymax = asymp.UCL
        ),
        alpha = 0.15,
        colour = NA
      ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Precipitation",
        y = "B biomass relative to control",
        colour = "Contrast",
        fill = "Contrast"
      ) +
      theme_classic() +
      facet_grid(~category)
  ),

  tar_target(
    name = fig_F_comp_precipitation_contrast,
    command = F_comp_contrasts_precipitation |>
      ggplot(
        aes(
          x = precipitation_scaled,
          y = estimate,
          colour = contrast,
          fill = contrast,
          group = contrast
        )
      ) +
      geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
      geom_ribbon(
        aes(ymin = asymp.LCL, ymax = asymp.UCL),
        alpha = 0.15,
        colour = NA
      ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Precipitation",
        y = "F biomass relative to control",
        colour = "Contrast",
        fill = "Contrast"
      ) +
      theme_classic() +
      facet_grid(~category)
  ),

  tar_target(
    name = fig_F_comp_temperature_contrast,
    command = F_comp_contrasts_temperature |>
      ggplot(
        aes(
          x = temperature_scaled,
          y = estimate,
          colour = contrast,
          fill = contrast,
          group = contrast
        )
      ) +
      geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
      geom_ribbon(
        aes(ymin = asymp.LCL, ymax = asymp.UCL),
        alpha = 0.15,
        colour = NA
      ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Temperature",
        y = "F biomass relative to control",
        colour = "Contrast",
        fill = "Contrast"
      ) +
      theme_classic() +
      facet_grid(~category)
  )

)
