# The Evolution of the Olympic Games 1896-2016

This  repository contains a Shiny web application which displays multiple interactive data visualizations showing the evolution of the modern Olympic Games from 1896 to 2016. I created the web application as a part of the coursework for the data science course Interactive Data Visualization offered by the University of Helsinki. The interactive visualizations are powered by Plotly's R library.

The application is available at [](). I am using a free plan offered by [shinyapps.io](https://www.shinyapps.io/) which includes 25 active hours per month for applications. Therefore, if the application cannot be accessed, then the likely reason is that the available active hours have been consumed for this month. In this case I recommend running the application locally. The application can be run locally by cloning the repository (or just downloadind the directory [olympics_evolution/](olympics_evolution/)) and opening [olympics_evolution/app.R](olympics_evolution/app.R) in the RStudio IDE and then pressing the *Run App* button.

## Data Set and Acknowledgements

My interactive data visualization is based on historical Olympic Games data available from Kaggle: [https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results](https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results). The GitHub repository that corresponds to the Kaggle data is [https://github.com/rgriff23/Olympic_history](https://github.com/rgriff23/Olympic_history).

Many thanks **rgriffin** (Kaggle username) / **rgriff23** (GitHub username) for making this data available in a convenient format!

I have personally augmented the data by collecting information on continents the countries belong to and by adding more descriptive National Olympic Committee (NOC) names. This augmented information is available at [data/noc_regions_continents.csv](data/noc_regions_continents.csv).

All of the data that the application needs is contained in *olympics_evolution/data_for_app/Olympics.RData*. This file was created with the R script [R/modify_create_dataset.R](R/modify_create_dataset.R). The script loads the file *athlete_events.csv* (available from Kaggle) and the augmented file *noc_regions_continents.csv* (available here in the data directory) and then creates the file *olympics_evolution/data_for_app/Olympics.RData* which the application loads before it launches. The R script is provided for reference only and, there is no need to run it since the directory *olympics_evolution/* already contains everything the application needs.

## R Libraries Used in This Application

- **[shiny](https://shiny.rstudio.com/)**: Provides the framework for building web applications using R

- **[plotly](https://plotly.com/r/)**: All of the interactive data visualizations are powered by Plotly's R library

- **[tidyverse](https://www.tidyverse.org/)**: Provides the framework for data processing and manipulation

- **[bslib](https://rstudio.github.io/bslib/)**: Provides the Bootstrap theme used by the application

- **[shinycssloaders](https://github.com/daattali/shinycssloaders)**: Provides the loading screen for the visualizations

- **[catmaply](https://cran.r-project.org/web/packages/catmaply/vignettes/catmaply.html)**: Provides an interactive heatmap for data with categorical outcomes (the visualization in the third tab)

- **[scales](https://scales.r-lib.org/)**: Provides the hue/color palette (hue_pal() function) for the visualization in the third tab

## My Inspiration and Practical Guidance for Building the Application

The technical implementation of the web application and the interactive data visualizations within it draw inspiration and practical guidance from the online books [Interactive web-based data visualization with R, plotly, and shiny](https://plotly-r.com/) by Carson Sievert and [Mastering Shiny](https://mastering-shiny.org/) by Hadley Wickham. Additionally, the design process behind the visualizations themselves follows the theoretical foundation taught during the course.
