# ---
# title: "TidyTuesday 2019-10-29 - A Glimpse on the NYC Squirrel Census by Lasse Scheele"
# output:
#   html_document:
#     df_print: paged
# ---


## Loading packages
library(tidyverse)
library(grid)
library(gridExtra)


## Reading data
data_source_name <- "NYC Squirrel Census"
nyc_squirrels_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")


## Settings
plot_signature <- textGrob(
  paste0("Data: ",data_source_name,", Author: Lasse Scheele (@LasSchee), Date: ",format(Sys.time(), "%x")),
  gp = gpar(fontface = 3, fontsize = 9),
  hjust = 1,
  x = 1
)

## Cleaning data
nyc_squirrels <- nyc_squirrels_raw %>%
  mutate(date = lubridate::mdy(date),
         hectare_ns = str_extract(hectare, "[0-9]+"),
         hectare_ew = str_extract(hectare, "[aA-zZ]+"),
         hectare = as.factor(hectare),
         hectare_ns = as.factor(hectare_ns),
         hectare_ew = as.factor(hectare_ew),
         human_interaction = ifelse(approaches==TRUE,"approaches",
                                    ifelse(indifferent==TRUE,"indifferent",
                                           ifelse(runs_from==TRUE,"runs_from",NA))))
c_hectare_ns <- sort(unique(nyc_squirrels$hectare_ns))
c_hectare_ew <- sort(unique(nyc_squirrels$hectare_ew))


## Analyzing spatial destribution of primary colors

### Settings colors
primary_fur_colors <- vector(mode="list", length=5)
names(primary_fur_colors) <- c(unique(nyc_squirrels$primary_fur_color %>% na.omit()),"all","NA")
primary_fur_colors[[1]] <- c("white","gray50")
primary_fur_colors[[2]] <- c("white","sienna3")
primary_fur_colors[[3]] <- c("white","black")
primary_fur_colors[[4]] <- c("white","green4")
primary_fur_colors[[5]] <- c("white","red")


### Functions for data processing
func_create_plot <- function(df, color_code_low, color_code_high, plot_title){
  plot <- ggplot(df, aes(x=hectare_ew, y=hectare_ns, fill=sum_sightings)) +
  geom_tile() +
  scale_fill_continuous(low = color_code_low, high = color_code_high) +
  coord_equal() +
  theme(legend.position="bottom", legend.title = element_blank(), plot.title = element_text(size = 12)) +
  ggtitle(plot_title) +
  xlab("west-east") + ylab("south-north")
  return(plot)
}

func_plot_color <- function(df,color_filter_str){
  if (color_filter_str == "all") {
    plot_title <- "All squirrels"
    color_filter <- names(primary_fur_colors)[!names(primary_fur_colors)=="all"]
    color_code_low <- primary_fur_colors[color_filter_str][[1]][[1]]
    color_code_high <- primary_fur_colors[color_filter_str][[1]][[2]]
  } else if (color_filter_str == "NA") {
    plot_title <- paste("Squirrel color:",color_filter_str)
    color_filter <- c(NA)
    color_code_low <- primary_fur_colors[color_filter_str][[1]][[1]]
    color_code_high <- primary_fur_colors[color_filter_str][[1]][[2]]
  } else if (color_filter_str %in% names(primary_fur_colors)[!names(primary_fur_colors)=="all"]) {
    plot_title <- paste("Squirrel color:",color_filter_str)
    color_filter <- c(color_filter_str)
    color_code_low <- primary_fur_colors[color_filter_str][[1]][[1]]
    color_code_high <- primary_fur_colors[color_filter_str][[1]][[2]]
  } else {
      stop(paste("No matching 'color_filter_str' provided!"))
  }
  
  df_transform <- df %>%
    filter(primary_fur_color %in% color_filter) %>%
    expand(hectare_ns, hectare_ew) %>%
    left_join(nyc_squirrels %>%
                filter(primary_fur_color %in% color_filter) %>%
                group_by(hectare_ns,hectare_ew) %>%
                summarise(sum_sightings = n()), by = c("hectare_ns", "hectare_ew")) %>%
    mutate(sum_sightings = replace_na(sum_sightings, 0))
  plot <- func_create_plot(df_transform, color_code_low, color_code_high, plot_title)
  return(plot)
}


### Plotting

# Create single plots
plot_all <- func_plot_color(df=nyc_squirrels, color_filter_str="all")
plot_gray <- func_plot_color(df=nyc_squirrels, color_filter_str="Gray")
plot_cinnamon <- func_plot_color(df=nyc_squirrels, color_filter_str="Cinnamon")
plot_black <- func_plot_color(df=nyc_squirrels, color_filter_str="Black")
plot_na <- func_plot_color(df=nyc_squirrels, color_filter_str="NA")
#Create combined plot
# grid.arrange(plot_all, plot_gray, plot_cinnamon, plot_black, plot_na, nrow = 1,
#              top = "Squirrel Sightings by Color",
#              bottom = textGrob(
#                "Data: Squirrel Census, Author: Lasse Scheele (2019)",
#                gp = gpar(fontface = 3, fontsize = 9),
#                hjust = 1,
#                x = 1
#              ))
g <- arrangeGrob(plot_all, plot_gray, plot_cinnamon, plot_black, plot_na, nrow = 1,
                 top = "Squirrel Sightings by Color",
                 bottom = plot_signature)
ggsave(file="Plots/primary_fur_color.png", g,
       width=30, height=24, units="cm",
       dpi=600)

# Findings:
# - the gray squirrels are the main population and seen all over Central Park
# - the cinnamon colored squirrels are seen more often in the south
# - the black squirrels are seen more often in the north


### Verification via density plots

nyc_squirrels %>%
  filter(!is.na(primary_fur_color)) %>%
  ggplot(aes(x=long, y=lat, color=primary_fur_color)) +
  geom_point(size=1.5) + 
  scale_color_manual(values=c("black", "sienna3", "gray50")) +
  geom_density_2d() +
  coord_equal() +
  facet_grid(. ~ primary_fur_color)


## Analyzing spatial destribution of behaviour

### Settings colors
behaviours <- vector(mode="list", length=3)
names(behaviours) <- c("approaches","indifferent","runs_from")
behaviours[[1]] <- c("white","green4")
behaviours[[2]] <- c("white","gold")
behaviours[[3]] <- c("white","firebrick")


### Functions for data processing

func_plot_behaviur <- function(df,column_name_behaviour){
  if (column_name_behaviour == "approaches") {
    plot_title <- paste("Squirrel",column_name_behaviour)
    color_code_low <- behaviours[column_name_behaviour][[1]][[1]]
    color_code_high <- behaviours[column_name_behaviour][[1]][[2]]
  } else if (column_name_behaviour == "indifferent") {
    plot_title <- paste("Squirrel",column_name_behaviour)
    color_code_low <- behaviours[column_name_behaviour][[1]][[1]]
    color_code_high <- behaviours[column_name_behaviour][[1]][[2]]
  } else if (column_name_behaviour == "runs_from") {
    plot_title <- paste("Squirrel runs away")
    color_code_low <- behaviours[column_name_behaviour][[1]][[1]]
    color_code_high <- behaviours[column_name_behaviour][[1]][[2]]
  } else {
      stop(paste("No matching 'column_name_behaviour' provided!"))
  }
  
  df_transform <- df %>%
    filter(!!sym(column_name_behaviour)==TRUE) %>%
    # filter(column_name_behaviour==TRUE) %>%
    expand(hectare_ns, hectare_ew) %>%
    left_join(nyc_squirrels %>%
                filter(!!sym(column_name_behaviour)==TRUE) %>%
                group_by(hectare_ns,hectare_ew,!!sym(column_name_behaviour)) %>%
                summarise(sum_sightings = n()) %>%
                select(hectare_ns,hectare_ew,sum_sightings),
              by = c("hectare_ns", "hectare_ew")) %>%
    mutate(sum_sightings = replace_na(sum_sightings, 0)) %>%
  func_create_plot(color_code_low,color_code_high,plot_title) -> plot
  return(plot)
}


plot_approaches <- func_plot_behaviur(nyc_squirrels, "approaches")
plot_indifferent <- func_plot_behaviur(nyc_squirrels, "indifferent")
plot_runs_from <- func_plot_behaviur(nyc_squirrels, "runs_from")

# grid.arrange(plot_approaches, plot_indifferent, plot_runs_from, nrow = 1,
#              top = "Squirrel Sightings by Interaction with Humans",
#              bottom = textGrob(
#                "Data: Squirrel Census, Author: Lasse Scheele (2019)",
#                gp = gpar(fontface = 3, fontsize = 9),
#                hjust = 1,
#                x = 1
#              ))
g <- arrangeGrob(plot_approaches, plot_indifferent, plot_runs_from, nrow = 1,
                 top = "Squirrel Sightings by Interaction with Humans",
                 bottom = plot_signature)
ggsave(file="Plots/behaviour.png", g,
       width=20, height=24, units="cm",
       dpi=600)

# Findings:
# - Most squirrels seem to be indifferent when they meet humans
# - Squirrels to the south seem to be more likely to approach humans
# - Squirels to the north seem to be more likely to run frm humans
