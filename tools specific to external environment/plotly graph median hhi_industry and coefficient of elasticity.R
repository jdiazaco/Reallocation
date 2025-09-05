library(dplyr)
library(plotly)
data <- read_csv("C:/Users/nb/Dropbox/Reallocation - shared folder/Exports/Zip files/04.09/all_firms/all/data_for_image_patent_window_emp_interaction_firmfe.csv")
NACE_list <- read_excel("C:/Users/nb/Dropbox/Reallocation - shared folder/Inputs/NACE_list.xlsx")

data <- data %>%
  left_join(NACE_list, by = c("filter" = "NACE")) %>%
  mutate(log_empl_bar_HHI_industry=asinh(log_empl_bar_HHI_industry),
         sector=ifelse(log_empl_bar_HHI_industry>=0, "Positive", "Negative"))

HHI_employment_graph<-ggplot(data, aes(x=median_HHI, 
                                       y=log_empl_bar_HHI_industry, 
                                       color=sector,
                                       text=paste0(industry, "\n x:", median_HHI, "\n y: ", log_empl_bar_HHI_industry,
                                                   "\n nobs:", nobs))) + 
       geom_point(alpha=0.5) +
  # scale_y_continuous(limits=c(-40, 10))
  labs(x = "Median HHI in Industry", y = "Coefficient for Log Employment * Industry HHI", color="Sign of the Coefficient")

HHI_employment_plotly<-ggplotly(HHI_employment_graph, tooltip = "text") %>%
  plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick") %>%
  # plotly::highlight_key(~category, ~industry)  %>% 
  plotly::layout(legend="Hello") 

ggsave(HHI_employment_graph, filename = "C:/Users/nb/Dropbox/Reallocation - shared folder/Exports/Zip files/04.09/all_firms/all/patent_window_emp_interaction_firmfe.png", width = 8, height = 6)


print(HHI_employment_plotly)
 
htmlwidgets::saveWidget(HHI_employment_plotly, "G:/My Drive/IWH/PhD/Reallocation/Exports/2025/HHI_employment2.html")




library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(crosstalk)
library(htmltools)   # for saving the composite (controls + plot)

data <- read_csv("C:/Users/nb/Dropbox/Reallocation - shared folder/Exports/Zip files/04.09/all_firms/all/data_for_image_patent_window_emp_interaction_firmfe.csv")
NACE_list <- read_excel("C:/Users/nb/Dropbox/Reallocation - shared folder/Inputs/NACE_list.xlsx")

data <- data %>%
  left_join(NACE_list, by = c("filter" = "NACE")) %>%
  mutate(
    log_empl_bar_HHI_industry = (log_empl_bar_HHI_industry),
    sector = ifelse(log_empl_bar_HHI_industry >= 0, "Positive", "Negative")
  )

# Share the data so controls can talk to the plot
sd <- SharedData$new(data, key = ~industry, group = "ind_search")

HHI_employment_graph <- ggplot(sd, aes(
  x = median_HHI,
  y = log_empl_bar_HHI_industry,
  color = sector,
  text = paste0(
    industry,
    "\n x: ", median_HHI,
    "\n y: ", log_empl_bar_HHI_industry,
    "\n nobs: ", nobs
  )
)) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Median HHI in Industry",
    y = "Coefficient for Log Employment * Industry HHI",
    color = "Sign of the Coefficient"
  )

HHI_employment_plotly <- ggplotly(HHI_employment_graph, tooltip = "text") %>%
  plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick")

# Add a searchable picker for industry
ctrl <- filter_select(
  id = "industry_pick",
  label = "Find industry",
  sharedData = sd,
  ~industry,
  multiple = FALSE
)

# Arrange controls + plot side by side
page <- bscols(widths = c(4, 8), ctrl, HHI_employment_plotly)

# Save both together (controls + plot)
save_html(page, "G:/My Drive/IWH/PhD/Reallocation/Exports/2025/HHI_employment.html")

# If you still want the static PNG too:
ggsave(
  HHI_employment_graph,
  filename = "C:/Users/nb/Dropbox/Reallocation - shared folder/Exports/Zip files/04.09/all_firms/all/patent_window_emp_interaction_firmfe.png",
  width = 8, height = 6
)

