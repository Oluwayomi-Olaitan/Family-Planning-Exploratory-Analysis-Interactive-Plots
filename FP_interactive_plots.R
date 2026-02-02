# Install packages (if necessary)

#install.packages("htmlwidgets", "wdiexplorer")

# Loading the packages
library(htmlwidgets)
library(wdiexplorer)

# a folder to save interactive plots
dir.create("interactive-plots", showWarnings = FALSE)


## The data wrangling decisions and FP data are saved in the `FP_Survey_Model_Data_wrangling.R` script.

source("FP_Survey_Model_Data_wrangling.R")

## To ensure consistent grouping and use of colours throughout this analysis, 
# we declare the grouping variable and the colour palette for group levels.

# For this exploratory analysis, our grouping variable is sub_region

group_var <- "sub_region"

group_levels <- sort(unique(survey_fc_contraceptive_use_data[[group_var]]))

group_colours <- scales::hue_pal()(length(group_levels))


# Assigning names to each of the colours
names(group_colours) <- group_levels

## The Trend strength measure

# survey data
survey_contraceptive_use_trendstrength <- wdiexplorer::compute_trend_shape_features(
  wdi_data = survey_fc_contraceptive_use_data, 
  index = "contraceptive_use_modern"
) |> 
  dplyr::select(country, trend_strength)


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


## (a) A scatterplot of the ratio of model-based estimates trend strength to survey data trend strength against survey data trend strength.  
## Interactive verison of Figure 2

combined_trend_strength <- model_contraceptive_use_trendstrength_group |>
  dplyr::select(country, sub_region, model_trend_strength = trend_strength) |>
  dplyr::left_join(survey_contraceptive_use_trendstrength_group |>
                     dplyr::select(country, survey_trend_strength = trend_strength),
                   dplyr::join_by("country")) |>
  dplyr::mutate(ratio = model_trend_strength / survey_trend_strength)

# identifying the countries with the extreme differences (by ratio)

trend_strength_labelled_countries <- combined_trend_strength |>
  dplyr::arrange(desc(ratio)) |>
  dplyr::slice_head(n = 10) |> # top 10 countries with high ratio
  dplyr::pull(country)

# the interactive scatterplot
T <- ggplot2::ggplot(data = na.omit(combined_trend_strength)) + 
  ggiraph::geom_point_interactive(
    ggplot2::aes(x = survey_trend_strength, y = ratio, colour = sub_region, tooltip = paste0(
      country,  "\n",
      "Survey: ", sprintf("%.3f", survey_trend_strength), "\t",
      ", Model: ", sprintf("%.3f", model_trend_strength)), data_id = country), size = 2.2) +
  ggplot2::geom_hline(yintercept = 1, colour = "grey40") +
  ggplot2::theme_bw() + 
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 4, byrow = TRUE) 
  ) +
  ggplot2::scale_colour_manual(values = group_colours) + # consistent colour scheme
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_text(size = 11),
    legend.text = ggplot2::element_text(size = 11),
    legend.key.width = grid::unit(0.6, "cm"),
    legend.spacing.x = grid::unit(0.3, "cm")
  )


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


## (b) A scatterplot of model-based estimates silhouette width against survey data silhouette width.  
# Interactiver version of Figure 6
survey_contraceptive_use_silwidth <- wdiexplorer::compute_variation(
  wdi_data = survey_contraceptive_use_data, 
  index = "contraceptive_use_modern",
  group_var = "sub_region"
) |> 
  dplyr::select(country, sil_width)


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

## The partition function of `wdiexplorer` ignores group levels with just one country 
# and only present the metric values of groups with more than one country.

# survey
survey_fc_contraceptive_use_silwidth_group <- survey_contraceptive_use_silwidth_group |>
  dplyr::filter(country %in% focus_countries$country)   # focus countries

# model
model_fc_contraceptive_use_silwidth_group <- model_contraceptive_use_silwidth_group |>
  dplyr::filter(country %in% focus_countries$country)  # focus countries

## combining the survey and modelled sil-width
combined_sil_widths <- model_fc_contraceptive_use_silwidth_group |>
  dplyr::select(country, sub_region, model_sil_width = sil_width) |>
  dplyr::left_join(survey_fc_contraceptive_use_silwidth_group |>
                     dplyr::select(country, survey_sil_width = sil_width),
                   dplyr::join_by("country")) |>
  dplyr::mutate(diff = abs(model_sil_width - survey_sil_width))


# the top 3 with the highest absolute differences and the most positive survey_sil_widths
sil_widths_labelled_countries <- combined_sil_widths |>
  dplyr::arrange(desc(diff)) |>
  dplyr::filter(
    dplyr::row_number() <= 3 | survey_sil_width > 0.4
  ) |>
  dplyr::pull(country)

ggplot2::ggplot(data = combined_sil_widths,
                ggplot2::aes(x = survey_sil_width, y = model_sil_width, colour = sub_region)) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::geom_line(
    ggplot2::aes(x = survey_sil_width, y = survey_sil_width), colour = "grey40"
  ) +
  ggplot2::theme_bw() +
  ggrepel::geom_label_repel(
    data = combined_sil_widths |> dplyr::filter(country %in% sil_widths_labelled_countries),
    ggplot2::aes(
      label = country, x = survey_sil_width, y = model_sil_width, colour = sub_region), 
    size = 5.6, inherit.aes = FALSE, show.legend = FALSE, segment.color = "grey"
  ) +
  ggplot2::scale_colour_manual(values = group_colours) + # consistent colour scheme
  ggplot2::theme_bw() +
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 3, byrow = TRUE) 
  ) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 19, face = "bold"),
    axis.text = ggplot2::element_text(size = 17),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(size = 17),
    legend.text = ggplot2::element_text(size = 15)
  )


S <- ggplot(data = combined_sil_widths) +
  ggiraph::geom_point_interactive(
    aes(x = survey_sil_width, y = model_sil_width, colour = sub_region, 
        tooltip = paste(
    country, "\n",
    "Survey: ", sprintf("%.3f", survey_sil_width), "\t",
    "Model: ", sprintf("%.3f", model_sil_width)), data_id = country),
    size = 2.4) +
  geom_line(aes(x = survey_sil_width, y = survey_sil_width), colour = "grey40") +
  ggplot2::scale_colour_manual(values = group_colours) + # consistent colour scheme
  ggplot2::theme_bw() +
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 3, byrow = TRUE) 
  ) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 19, face = "bold"),
    axis.text = ggplot2::element_text(size = 17),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(size = 15),
    legend.text = ggplot2::element_text(size = 13)
  )

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


## (c) Data trajectories of the labelled countries, comparing their trajectories with neighbouring countries in their sub-region. 
# Interactive version of Figure 7.

# top 3 with the highest absolute differences and the most positive survey_sil_widths
sil_widths_labelled_countries_subregion <- combined_sil_widths |>
  dplyr::arrange(desc(diff)) |>
  dplyr::filter(
    dplyr::row_number() <= 3 | 
      survey_sil_width > 0.4
  ) |>
  dplyr::select(sub_region, labelled_country = country)

# labelled countries
sil_widths_labelled_countries <- sil_widths_labelled_countries_subregion |>
  dplyr::pull(labelled_country)

sil_widths_labelled_countries_data_metrics <- survey_fc_contraceptive_use_data |>
  dplyr::filter(country %in% sil_widths_labelled_countries) |> # survey data of labelled countries
  dplyr::select(division_numeric_code, year, country, region, sub_region, contraceptive_use_modern) |>
  dplyr::inner_join(
    sil_widths_labelled_countries_subregion, 
    by = "sub_region",
    relationship = "many-to-many"
  ) |>
  dplyr::left_join( # joining both survey and modelled sil-width metrics
    combined_sil_widths |> 
      dplyr::select(country, model_sil_width, survey_sil_width),
    by = c("labelled_country" = "country")
  )

# pulling the modelled data of labelled countries and other countries in their subregion
sil_widths_model_data <- model_fc_contraceptive_use_data |>
  dplyr::filter(
    sub_region %in% sil_widths_labelled_countries_subregion$sub_region
  ) |>
  dplyr::inner_join(
    sil_widths_labelled_countries_subregion, 
    by = "sub_region",
    relationship = "many-to-many" # for sub_region with more than 1 labelled country
  )

sub_region_order <- c("Western Africa", "Melanesia", "Western Asia", "Middle Africa")

sil_widths_labelled_countries_data_metrics$sub_region <- factor(
  sil_widths_labelled_countries_data_metrics$sub_region, levels = sub_region_order
)

sil_widths_model_data$sub_region <- factor(
  sil_widths_model_data$sub_region, levels = sub_region_order
)

# the plot


S2 <- ggplot2::ggplot() +
  ggiraph::geom_line_interactive(
  data = sil_widths_model_data |> 
    dplyr::filter(country != labelled_country), 
  ggplot2::aes(x = year, y = contraceptive_use_modern, group = country, 
                tooltip = paste(country), data_id = country
  ), colour = "grey80") +
  ggplot2::geom_point(
    data = sil_widths_labelled_countries_data_metrics |> 
      dplyr::filter(country == labelled_country),
    ggplot2::aes(x = year, y = contraceptive_use_modern, colour = sub_region), 
    size = 1.5) +
  ggiraph::geom_line_interactive(
    data = sil_widths_model_data |> 
      dplyr::filter(country == labelled_country), 
    ggplot2::aes(x = year, y = contraceptive_use_modern, group = country, 
                 colour = sub_region, tooltip = paste(
                   country, "\n",
                   "Survey: ", sprintf("%.3f", survey_sil_width), "\t",
                   "Model: ", sprintf("%.3f", model_sil_width)), data_id = country
    )) +
  ggh4x::facet_wrap2(~sub_region + labelled_country, 
                     nrow = 3, scales = "free") +
  ggplot2::scale_colour_manual(values = group_colours) + # consistent colour scheme
  ggplot2::ylab("Modern contraceptive use proportion") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none",
                 strip.text = ggplot2::element_text(size = 12, face = "bold"),
                 axis.title = ggplot2::element_text(size = 12, face = "bold"))

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


## (d) A scatterplot of residuals curvature versus linearity. 
# Interactive version of Figure 8

# filtering the predicted data for country-year pair with available survey data
fc_contraceptive_data_residuals <- survey_fc_contraceptive_use_data |>
  dplyr::select(country, year, survey_data = contraceptive_use_modern) |>
  dplyr::left_join(
    model_fc_contraceptive_use_data |> # modelled data for available survey data pairs
      dplyr::select(country, year, region, sub_region,
                    model_data = contraceptive_use_modern),
    by = c("country", "year")
  ) |>
  dplyr::mutate(residuals = survey_data - model_data)


## Computing the linearity and curvature

residual_temporal_features <- wdiexplorer::compute_trend_shape_features(
  wdi_data = fc_contraceptive_data_residuals,
  index = "residuals")

residual_temporal_features_group <- wdiexplorer::add_group_info(
  metric_summary = residual_temporal_features, 
  wdi_data = fc_contraceptive_data_residuals
)

# labelled countries
residuals_labelled_countries <- residual_temporal_features_group |>
  dplyr::filter(
    linearity > 0.05 | 
      curvature < -0.018 |
      (linearity < -0.05 & curvature < -0.01)|
      (linearity < -0.045 & curvature > 0.01)
  ) |>
  dplyr::pull(country)

# the plot
LC <- ggplot2::ggplot(
    data = na.omit(residual_temporal_features_group)) +
  ggiraph::geom_point_interactive( 
    ggplot2::aes(x = linearity, y = curvature,
                 tooltip = paste(
                   country, "\n",
                   "Linearity: ", sprintf("%.3f", linearity), "\t",
                   ", Curvature: ", sprintf("%.3f", curvature)), 
                 data_id = country, colour = sub_region)
    ) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey20") + 
  ggplot2::geom_vline(xintercept = 0, colour = "grey20") +
  ggplot2::theme_bw() +
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 3, byrow = TRUE) 
  ) +
  ggplot2::scale_colour_manual(values = group_colours) + # consistent colour scheme
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 15, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_text(size = 14),
    legend.text = ggplot2::element_text(size = 10),
    legend.key.width = grid::unit(0.6, "cm"),
    legend.spacing.x = grid::unit(0.5, "cm")
  )

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
  file = "interactive-plots/residuals_curvature_linearity_plot.html",
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
