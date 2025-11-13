library(htmlwidgets)
library(wdiexplorer)

# a folder to save interactive plots
dir.create("interactive-plots", showWarnings = FALSE)


## The data wrangling decisions and FP data are saved in the `FP_Survey_Model_Data_wrangling.R` script.

source("FP_Survey_Model_Data_wrangling.R")

## The Trend strength measure

# survey data
survey_contraceptive_use_trendstrength <- wdiexplorer::compute_trend_shape_features(
  wdi_data = survey_fc_contraceptive_use_data, 
  index = "contraceptive_use_modern"
) |> 
  select(country, trend_strength)


survey_contraceptive_use_trendstrength_group <- wdiexplorer::add_group_info(
  metric_summary = survey_contraceptive_use_trendstrength, 
  wdi_data = survey_fc_contraceptive_use_data
)

#model data
model_contraceptive_use_trendstrength <- wdiexplorer::compute_trend_shape_features(
  wdi_data = model_fc_contraceptive_use_data, 
  index = "contraceptive_use_modern"
)  |> 
  select(country, trend_strength)

model_contraceptive_use_trendstrength_group <- wdiexplorer::add_group_info(
  metric_summary = model_contraceptive_use_trendstrength, 
  wdi_data = model_fc_contraceptive_use_data
)


## (a) A scatterplot of model-based estimates trend strength against survey data trend strength.

combined_trend_strength <- model_contraceptive_use_trendstrength_group |>
  select(country, sub_region, model_trend_strength = trend_strength) |>
  left_join(survey_contraceptive_use_trendstrength_group |>
              select(country, survey_trend_strength = trend_strength),
            join_by("country")) |>
  mutate(ratio = model_trend_strength / survey_trend_strength)


# identifying the countries with the extreme differences
trend_strength_top_10 <- combined_trend_strength |>
  arrange(desc(ratio)) |>
  dplyr::slice_head(n = 10) |>
  pull(country)

# the interactive scatterplot
T <- ggplot2::ggplot(data = combined_trend_strength) + 
  ggiraph::geom_point_interactive(
    ggplot2::aes(x = survey_trend_strength, y = ratio, colour = sub_region, tooltip = paste0(
      country,  "\n",
      "Survey: ", sprintf("%.3f", survey_trend_strength), "\t",
      ", Model: ", sprintf("%.3f", model_trend_strength)), data_id = country), size = 2.2) +
  ggplot2::geom_hline(yintercept = 1) +
  ggplot2::theme_bw() +
  theme(legend.position = "bottom")


# Interactive plot
saveWidget(
ggiraph::girafe(
  ggobj = T,
  width_svg = 10,
  height_svg = 8.3,
  options = list(
    ggiraph::opts_hover_inv(css = "opacity: 0.1;"),  # makes non-hovered lines fade
    ggiraph::opts_hover(css = "opacity: 1; stroke-width: 2;")  # on hover: no color change
  )
),
file = "interactive-plots/trendstrength_plot.html",
selfcontained = TRUE
) 


## (b) A scatterplot of model-based estimates trend strength against survey data trend strength

survey_contraceptive_use_silwidth <- wdiexplorer::compute_variation(
  wdi_data = survey_contraceptive_use_data, 
  index = "contraceptive_use_modern",
  group_var = "sub_region"
) |> 
  select(country, sil_width)


survey_contraceptive_use_silwidth_group <- wdiexplorer::add_group_info(
  metric_summary = survey_contraceptive_use_silwidth, 
  wdi_data = survey_contraceptive_use_data
)

#model data
model_contraceptive_use_silwidth <- wdiexplorer::compute_variation(
  wdi_data = model_contraceptive_use_data, 
  index = "contraceptive_use_modern",
  group_var = "sub_region"
)  |> 
  select(country, sil_width)

model_contraceptive_use_silwidth_group <- wdiexplorer::add_group_info(
  metric_summary = model_contraceptive_use_silwidth, 
  wdi_data = model_contraceptive_use_data
)

# filter sub_region with more than one country as the valid sub_region for the partition plot
fc_subregions_with_multi_countries <- focus_countries |>
  distinct(country, sub_region) |>
  count(sub_region) |>
  filter(n > 1) |>
  pull(sub_region)

# survey
survey_fc_contraceptive_use_silwidth_group <- survey_contraceptive_use_silwidth_group |>
  filter(
    country %in% focus_countries$country,      # focus countries
    sub_region %in% fc_subregions_with_multi_countries) # in subregion with more than 1 country

# model
model_fc_contraceptive_use_silwidth_group <- model_contraceptive_use_silwidth_group |>
  filter(
    country %in% focus_countries$country,      # focus countries
    sub_region %in% fc_subregions_with_multi_countries) # in subregion with more than 1 country


combined_sil_widths <- model_fc_contraceptive_use_silwidth_group |>
  select(country, sub_region, model_sil_width = sil_width) |>
  left_join(survey_fc_contraceptive_use_silwidth_group |>
              select(country, survey_sil_width = sil_width),
            join_by("country")) |>
  mutate(diff = abs(model_sil_width - survey_sil_width))


# Identifying countries with high differences
sil_widths_top_10 <- combined_sil_widths |>
  arrange(desc(diff)) |>
  dplyr::slice_head(n = 10) |>
  pull(country)

S <- ggplot(data = combined_sil_widths) +
  ggiraph::geom_point_interactive(
    aes(x = survey_sil_width, y = model_sil_width, colour = sub_region, 
        tooltip = paste(
    country, "\n",
    "Survey: ", sprintf("%.3f", survey_sil_width), "\t",
    "Model: ", sprintf("%.3f", model_sil_width)), data_id = country),
    size = 2.2) +
  geom_line(aes(x = survey_sil_width, y = survey_sil_width), colour = "grey") +
  theme_bw()

# Interactive plot
saveWidget(
ggiraph::girafe(
  ggobj = S,
  width_svg = 10,
  height_svg = 8.3,
  options = list(
    ggiraph::opts_hover_inv(css = "opacity: 0.1;"),  # makes non-hovered lines fade
    ggiraph::opts_hover(css = "opacity: 1; stroke-width: 2;")  # on hover: no color change
  )
),
file = "interactive-plots/silwidths_plot.html",
selfcontained = TRUE
) 


## (c) Identifying top 10 countries with the highest differences and 
## compare their data trajectories with their neighbouring countries in the same sub-region.


# subregion of the top 10 countries
sil_widths_top_10_subregion <- combined_sil_widths |>
  arrange(desc(diff)) |>
  dplyr::slice_head(n = 10) |>
  pull(sub_region) |> unique()


# The data for focus countries with more than one country in its sub-region
sil_widths_top_10_subregion_data <- survey_fc_contraceptive_use_data |>
  filter(
    sub_region %in% sil_widths_top_10_subregion) # focus countries in subregion with more than 1 country


#Joining the model-based estimates with the combine sil_widths

model_fc_data_sil_width_metrics <- model_fc_contraceptive_use_data |>
  filter(
    sub_region %in% sil_widths_top_10_subregion) |> # focus countries in subregion with more than 1 country
  left_join(model_fc_contraceptive_use_silwidth_group |>
              select(country, model_sil_width = sil_width), by = "country") |>
  left_join(survey_fc_contraceptive_use_silwidth_group |>
              select(country, survey_sil_width = sil_width), by = "country")

# the plot

S2 <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = sil_widths_top_10_subregion_data,
      ggplot2::aes(x = year, y = contraceptive_use_modern), 
      size = 1.55, colour = "grey60") +
  ggplot2::geom_point(
    data = sil_widths_top_10_subregion_data |> 
        filter(country %in% sil_widths_top_10),
    ggplot2::aes(x = year, y = contraceptive_use_modern, colour = country,), 
        size = 1.55) +
  ggiraph::geom_line_interactive(
    data = model_fc_data_sil_width_metrics, 
      ggplot2::aes(x = year, y = contraceptive_use_modern, group = country, 
            tooltip = paste(
               country, "\n",
               "Survey: ", sprintf("%.3f", survey_sil_width), "\t",
               "Model: ", sprintf("%.3f", model_sil_width)), data_id = country), 
                colour = "grey55") +
  ggiraph::geom_line_interactive(
    data = model_fc_data_sil_width_metrics |> 
      filter(country %in% sil_widths_top_10), 
    ggplot2::aes(x = year, y = contraceptive_use_modern, group = country, colour = country, 
                 tooltip = paste(
      country, "\n",
      "Survey: ", sprintf("%.3f", survey_sil_width), "\t",
      ", Model: ", sprintf("%.3f", model_sil_width)), data_id = country)) +
  ggplot2::facet_wrap(~forcats::fct_relevel(sub_region, sil_widths_top_10_subregion), 
             nrow = 3, scales = "free") +
  ggplot2::ylab("Modern contraceptive use proportion") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")


# Interactive plot
saveWidget(
ggiraph::girafe(
  ggobj = S2,
  width_svg = 10,
  height_svg = 8.3,
  options = list(
    ggiraph::opts_hover_inv(css = "opacity: 0.1;"),  # makes non-hovered lines fade
    ggiraph::opts_hover(css = "opacity: 1; stroke-width: 2;")  # on hover: no color change
  )
),
file = "interactive-plots/silwidths_trajectories_plot.html",
selfcontained = TRUE
)


## (d) Filtering the corresponding predicted data for pairs where we have survey data.

contraceptive_data_residuals <- survey_fc_contraceptive_use_data |>
  select(country, year, survey_data = contraceptive_use_modern) |>
  left_join(model_fc_contraceptive_use_data |>
              select(country, year, region, sub_region,
                     model_data = contraceptive_use_modern),
            by = c("country", "year")) |>
  mutate(residuals = model_data - survey_data)


## Computing the linearity and curvature

residual_temporal_features <- wdiexplorer::compute_trend_shape_features(
  wdi_data = contraceptive_data_residuals,
  index = "residuals")

residual_temporal_features_group <- wdiexplorer::add_group_info(
  metric_summary = residual_temporal_features, 
  wdi_data = contraceptive_data_residuals
)


LC <- ggplot2::ggplot(
    data = residual_temporal_features_group) +
  ggiraph::geom_point_interactive(
    data = residual_temporal_features_group, 
    ggplot2::aes(x = linearity, y = curvature,
                 tooltip = paste(country), data_id = country, colour = sub_region)
    ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

# Interactive plot
saveWidget(
  ggiraph::girafe(
    ggobj = LC,
    width_svg = 10,
    height_svg = 8.3,
    options = list(
      ggiraph::opts_hover_inv(css = "opacity: 0.1;"),  # makes non-hovered lines fade
      ggiraph::opts_hover(css = "opacity: 1; stroke-width: 2;")  # on hover: no color change
    )
  ),
  file = "interactive-plots/curvature_linearity_plot.html",
  selfcontained = TRUE
)




#### creating a folder for all the plots

# a list of all HTML files inside the interactive-plots folder
files <- list.files("interactive-plots", pattern = "\\.html$", full.names = FALSE)

links <- paste0("<li><a href='interactive-plots/", files, "'>", files, "</a></li>")


family_planning_analysis_interactive_plot_html <- paste0(
  "<!DOCTYPE html><html><head><title>Family planning modern contraceptive use exploratory data analysis interactive plots gallery</title></head><body>",
  "<h1>Family planning modern contraceptive use exploratory data analysis interactive plots gallery</h1><ul>",
  paste(links, collapse = "\n"),
  "</ul></body></html>"
)

writeLines(family_planning_analysis_interactive_plot_html, "index.html")
