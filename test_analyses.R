### figure

test <- standing_biomass_22 |>
  tidylog::left_join(fg_cover |> filter(year == 2015) |> select(siteID, blockID, plotID, total_graminoids, total_forbs, total_bryophytes), by = c("siteID", "blockID", "plotID")) |>
  tidylog::left_join(root_biomass |> select(siteID, blockID, plotID, root_biomass, root_dry_matter_content), by = c("siteID", "blockID", "plotID")) |>
  rename(
    G_cover = total_graminoids,
    F_cover = total_forbs,
    B_cover = total_bryophytes
  ) |>
 # pivot_wider(names_from = removed_fg, values_from = biomass, values_fill = 0) |>
 # mutate(
 #   B_treat = as.integer(str_detect(fg_removed, stringr::fixed("B"))),
 #   G_treat = as.integer(str_detect(fg_removed, stringr::fixed("G"))),
 #   F_treat = as.integer(str_detect(fg_removed, stringr::fixed("F")))
 # ) |>
 # group_by(plotID) |>
 # mutate(total_biomass = sum(F, G, B)) |>
  ungroup() |>
  tidylog::filter(fg_removed != "FGB") |>
 # filter(!is.na(total_biomass), !is.na(root_biomass), total_biomass > 0, root_biomass > 0) |>
  mutate(log_above_ground = log(biomass),
         log_root_biomass = log(root_biomass),
         log_root_dry_matter_content = log(root_dry_matter_content))


test |>
  ggplot(aes(log_above_ground, log_root_biomass, colour = removed_fg)) +
  geom_point(alpha =  0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap( ~ fg_removed) +
  labs(colour = "Removed functional group",
       x = "Log above-ground biomass",
       y = "Log root biomass") +
  theme_bw()

test |>
  ggplot(aes(log_above_ground, log_root_dry_matter_content, colour = removed_fg)) +
  geom_point(alpha =  0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap( ~ fg_removed) +
  labs(colour = "Removed functional group",
       x = "Log above-ground biomass",
       y = "Log root dry matter content") +
  theme_bw()


lmer(
  log_root_biomass ~
    log_aboveground*removal_treatment +
    (1 | siteID) +
    (1 | siteID:blockID),
  data = test
)


rdmc_fit <- lmer(
  log_root_dry_matter_content ~
    fg_removed*temperature_scaled + precipitation_scaled +
    (1 | siteID:blockID),
  data = test
)

tidy(rdmc_fit)
plot_model(rdmc_fit)



#### model

test2 <- standing_biomass_22 |>
  tidylog::left_join(fg_cover |> filter(year == 2015) |> select(siteID, blockID, plotID, total_graminoids, total_forbs, total_bryophytes), by = c("siteID", "blockID", "plotID")) |>
  tidylog::left_join(root_biomass |> select(siteID, blockID, plotID, root_biomass), by = c("siteID", "blockID", "plotID")) |>
  rename(
    G_cover = total_graminoids,
    F_cover = total_forbs,
    B_cover = total_bryophytes
  ) |>
   pivot_wider(names_from = removed_fg, values_from = biomass, values_fill = 0) |>
   mutate(
     B_treat = as.integer(str_detect(fg_removed, stringr::fixed("B"))),
     G_treat = as.integer(str_detect(fg_removed, stringr::fixed("G"))),
     F_treat = as.integer(str_detect(fg_removed, stringr::fixed("F")))
   ) |>
   group_by(plotID) |>
   mutate(total_biomass = sum(F, G, B)) |>
  ungroup() |>
  tidylog::filter(fg_removed != "FGB") |>
  # filter(!is.na(total_biomass), !is.na(root_biomass), total_biomass > 0, root_biomass > 0) |>
  mutate(log_G = log(G),
         log_F = log(F),
         log_B = log(B),
         log_root_biomass = log(root_biomass))


fit_1 <- lmer(
  log1p(root_biomass) ~
    log1p(G) + log1p(F) + log1p(B) + fg_removed + temperature_scaled +
    (1|siteID/blockID),
  data = test2
)

tidy(fit_1)
plot_model(fit_1) +
  geom_hline(yintercept = 0, colour = "grey60") +
  theme_bw()

plot_model(fit_1, type = "pred")


fit_2 <- lmer(
  log1p(root_biomass) ~
    fg_removed + temperature_scaled + precipitation_scaled +
    (1|siteID/blockID),
  data = test2
)

tidy(fit_2)
plot_model(fit_2) +
  geom_hline(yintercept = 0, colour = "grey60") +
  theme_bw()

plot_model(fit_2, type = "pred")
