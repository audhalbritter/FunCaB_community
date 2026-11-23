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
      # 2015 removed biomass (all removal treatments including FGB)
      removed_dat <- standardised_removed_biomass_site %>%
        filter(fg_removed != "none") %>%
        select(siteID, blockID, plotID, fg_removed, removed_fg, removed_biomass,
               temperature_level, precipitation_level)

      # 2022 standing biomass in control plots (nothing removed)
      control_22 <- standing_biomass_22 %>%
        filter(fg_removed == "none") %>%
        transmute(
          siteID, blockID, plotID,
          fg_removed = "none",
          removed_fg,
          removed_biomass = biomass,
          temperature_level, precipitation_level
        )

      bind_rows(control_22, removed_dat) %>%
        mutate(fg_removed = factor(fg_removed, levels = c("none", "G", "F", "B", "GF", "GB", "FB", "FGB"))) %>%
        ggplot(aes(x = fg_removed, y = removed_biomass, fill = removed_fg)) +
        geom_col() +
        scale_fill_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Functional group"
        ) +
        facet_grid(temperature_level ~ precipitation_level) +
        theme_bw() +
        labs(
          x = "Functional group removed",
          y = "Biomass"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  ),

  tar_target(
    name = fig_prop_removed_biomass_stacked,
    command = {
      standardised_removed_biomass_site %>%
        filter(fg_removed != "none") %>%
        mutate(fg_removed = factor(fg_removed, levels = c("G", "F", "B", "GF", "GB", "FB", "FGB"))) %>%
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
    name = factorial_comparisons_overview,
    command = {
      primary_comparisons <- list(
        "G removal vs control (F remains)" = c(-1, 1, 0, 0),
        "F removal vs control (G remains)" = c(-1, 0, 1, 0),
        "GF removal vs control"            = c(-1, 0, 0, 1)
      )

      factorial_comparisons <- list(
        "GF removal vs F removal"                       = c(0, 0, -1, 1),
        "GF removal vs G removal"                       = c(0, -1, 0, 1),
        "(GF removal - F removal) - (G removal - ctrl)" = c(1, -1, -1, 1)
      )

      treatments <- c("Control", "G removal", "F removal", "GF removal")

      # 2. Define pie composition: always 3 equal thirds (B always Present)
      composition <- tribble(
        ~treatment,   ~group, ~status,   ~prop,
        "Control",    "B",    "Present", 1/3,
        "Control",    "G",    "Present", 1/3,
        "Control",    "F",    "Present", 1/3,

        "G removal",  "B",    "Present", 1/3,
        "G removal",  "G",    "Removed", 1/3,
        "G removal",  "F",    "Present", 1/3,

        "F removal",  "B",    "Present", 1/3,
        "F removal",  "G",    "Present", 1/3,
        "F removal",  "F",    "Removed", 1/3,

        "GF removal", "B",    "Present", 1/3,
        "GF removal", "G",    "Removed", 1/3,
        "GF removal", "F",    "Removed", 1/3
      ) %>%
        mutate(
          group  = factor(group, levels = c("B", "G", "F")),
          status = factor(status, levels = c("Present", "Removed"))
        )

      # 3. Combine contrasts and keep ONLY active treatments (weight != 0)
      build_pie_df <- function(comp_list, set_label) {
        bind_rows(lapply(names(comp_list), function(cname) {
          data.frame(
            set = set_label,
            comparison = cname,
            treatment = treatments,
            weight = comp_list[[cname]],
            stringsAsFactors = FALSE
          )
        })) %>%
          mutate(comparison = factor(comparison, levels = names(comp_list)))
      }

      df_pies <- bind_rows(
        build_pie_df(primary_comparisons, "Primary Comparisons"),
        build_pie_df(factorial_comparisons, "Factorial Comparisons")
      ) %>%
        filter(weight != 0) %>%
        mutate(treatment = factor(treatment, levels = treatments)) %>%
        left_join(composition, by = "treatment", relationship = "many-to-many") %>%
        arrange(comparison, treatment, group)

      # 4. Plot pie charts with washed-out wedges for removed functional groups
      ggplot(df_pies, aes(x = 1, y = prop, fill = group, alpha = status)) +
        geom_col(width = 1, color = "grey30", linewidth = 0.4) +
        coord_polar(theta = "y") +
        facet_grid(comparison ~ treatment, drop = FALSE) +
        scale_fill_manual(
          values = c(
            "B" = "#BC94D1",  # Purple
            "G" = "#79BA47",  # Green
            "F" = "#CC8E3D"   # Orange
          ),
          name = "Functional Group"
        ) +
        scale_alpha_manual(
          values = c("Present" = 1.0, "Removed" = 0.15),
          name = "Status"
        ) +
        guides(
          fill  = guide_legend(override.aes = list(alpha = 1)),
          alpha = guide_legend(override.aes = list(fill = "grey40"))
        ) +
        labs(
          title = "Experimental Contrasts by Functional Group Presence",
          subtitle = "Full color = present; washed-out = removed"
        ) +
        theme_void(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4)),
          plot.subtitle = element_text(hjust = 0.5, color = "grey30", margin = margin(b = 12)),
          strip.text.x = element_text(face = "bold", margin = margin(b = 6)),
          strip.text.y = element_text(angle = 0, hjust = 0, margin = margin(l = 8)),
          legend.position = "top",
          legend.box = "horizontal",
          panel.spacing = unit(0.6, "lines")
        )

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
    name = fig_total_biomass_treatment_precipitation,
    command = {
      ggplot(standing_biomass_22_wide, aes(x = fg_removed, y = total_biomass)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.12, alpha = 0.6) +
        labs(
          x = "Functional groups removed",
          y = "Total biomass"
        ) +
        theme_classic() + facet_grid(~precipitation_level)
    }
  ),

  tar_target(
    name = fig_fg_biomass_treatment_temperature,
    command = {
      standing_biomass_22 |>
        filter(fg_removed != "FGB") |>
        ggplot(aes(x = fg_removed, y = biomass, colour = removed_fg)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.12, alpha = 0.6) +
        labs(
          x = "Functional groups removed",
          y = "Total biomass"
        ) +
        scale_color_brewer(palette = "Set2") +
        theme_classic() +
        facet_grid(~temperature_level)
    }
  ),

    tar_target(
    name = fig_fg_biomass_treatment_precipitation,
    command = {
      standing_biomass_22 |>
        filter(fg_removed != "FGB") |>
        ggplot(aes(x = fg_removed, y = biomass, colour = removed_fg)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.12, alpha = 0.6) +
        labs(
          x = "Functional groups removed",
          y = "Total biomass"
        ) +
        scale_color_brewer(palette = "Set2") +
        theme_classic() +
        facet_grid(~precipitation_level)
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
