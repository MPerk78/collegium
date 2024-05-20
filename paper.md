---
output: 
  html_document:
    keep_md: true
bibliography: references.bib
biblio-style: numeric
    
---




# Collegium: A dashboard to help community colleges find peer institutions
Mark A. Perkins and Sean Field

## Summary
Many community colleges and two-year institutions require program and institutional evaluation. However, it is often difficult to find indicators of comparison when considering an institution’s metrics as no two institutions are the same. Collegium is a tool to help institutional researchers identify peer institutions when benchmarking. This is an open-source software tool that uses k-means cluster analysis to help institutional researchers or other community college stakeholders narrow down lists of potential peer institutions. Unlike other methods in the past, this dashboard integrates both institution level and county level variables to help identify peer institutions.

## Statement of Need
The identification and selection of peer institutions have evolved significantly over the decades, beginning with early descriptive studies and progressing to sophisticated statistical and hybrid methodologies. Curry’s [-@curry_seven_1972] pioneering work laid the groundwork by comparing the "character" of universities, introducing a framework that evolved through the 1980s. Terenzini et al. [-@terenzini_conceptual_1980] employed cluster analysis using the BMDP2M program, highlighting faculty productivity and salaries. Brinkman & Teeter [-@brinkman_methods_1987] later emphasized the subjective judgment inherent in cluster analysis. The 1990s saw refinements with hierarchical cluster analysis by Szelest [-@szelest_search_1996] and Boronico & Choksi [-@boronico_identifying_2012], utilizing IPEDS data to narrow down potential peers. The 2000s introduced hybrid approaches, as documented by Weeks et al. [-@weeks_developing_2000], blending statistical methods with subjective judgments. Zhao & Dean [-@zhao_selecting_1997] and Yan [-@yan_program-level_2017] further advanced the field with latent class modeling and Naïve Bayesian Classification. Recent years have seen continued innovations, with D’Allegro & Zhou [-@dallegro_case_2013], D’Allegro [-@dallegro_case_2017], and Chatman [-@chatman_constructing_2017] employing percentile selection indices and proximity indices to enhance precision. This historic evolution underscores the increasing complexity and precision in peer institution identification, reflecting broader trends in data availability and methodological advancements in higher education research.

Effective benchmarking and strategic planning in higher education demand identifying peer institutions, often overlooked for community factors, especially in two-year institutions. Existing tools lack user-friendliness, hindering strategic planning and resource allocation. Developing an intuitive dashboard leveraging advanced data analytics can enhance institutional benchmarking. Additionally, there's a need for user-friendly software to manage institutional data and identify peer institutions, democratizing access to sophisticated analytical capabilities and enabling more stakeholders to participate in data-driven strategic planning and benchmarking.

### Description
Collegium integrates IPEDS and US Census data to empower educational benchmarking. Through K-means clustering, it facilitates the identification of peer institutions based on diverse metrics. Users can explore and analyze institutional data, ranging from enrollment and graduation rates to demographic characteristics, fostering informed decision-making in the education sector. With features enabling data export and visualization, the dashboard serves as a comprehensive platform for educators, policymakers, and researchers alike, promoting collaboration and insights-driven strategies. Its user-friendly interface and analytical capabilities make it a valuable resource for navigating the complexities of educational data and deriving actionable insights for improving institutional performance and student outcomes.

### Data Collection
The required libraries are loaded, including factoextra [@kassambara_factoextra_2020], `FactoMineR` [@le_factominer_2008], `tidyverse` [@wicham_welcome_2019], `dplyr` [@wickham_dplyr_2022], `data.table` [@dowle_datatable_2023], `DT` [@xie_dt_2022], `shiny` [@noauthor_shiny_2023], `shinyWidgets` [@perrier_shinywidgets_2023], and `shinydashboard` [@chang_shinydashboard_2021].

Data collection involves retrieving and processing from IPEDS and the US Census Bureau. For IPEDS, connections were established to gather institution information, enrollment, graduation rates, and cost data. This data was then joined, transformed, and exported for comprehensive analysis. Census data retrieval included population and demographic information, which was calculated for distribution insights, then cleaned and exported for integration.

The integrated dataset combines IPEDS and census data by county codes, undergoing further cleaning and preparation. The final CSV export offers insights into educational institutions' relationship with demographic characteristics at the county level, achieved through comprehensive data collection and transformation. This approach facilitates in-depth analysis and research.

#### Dashboard Overview
The Shiny dashboard, using IPEDS and US Census data, facilitates educational benchmarking through interactive visualizations and K-means clustering. Data is loaded from ipedsgradmassive2.csv, filtered, and analyzed. The UI, defined with tabs for "Introduction", "K Means", and "Data Dictionary", offers specific functionalities. Server logic, employing reactive expressions, updates plots and tables based on user inputs. Plot outputs visualize PCA and clustering results. Data filtering enables users to explore specific clusters. Download handlers allow exporting filtered datasets and data dictionaries in CSV format, enhancing decision-making processes in education.

#### Dashboard Workflow
Instead of figures, we present a link to the working dashboard (https://marksresearch.shinyapps.io/collegium/). The dashboard loads with a captivating image and a title, drawing users into the exploration process. Using the hamburger menu in the upper left corner, users can navigate to the tab dedicated to refining their selection of peer institutions. With an intuitive interface accommodating three distinct clustering stages, users can determine the optimal number of clusters via the scree plot and subsequently locate their college within the search box, identifying its associated cluster. Leveraging the "Select Cluster" slicer, users can further refine their options, progressing through Levels 2 and 3 to ultimately compile a refined list of potential peer institutions. Upon completion, users can employ either random selection hybrid methodologies from the existing literature to finalize their peer institution selection.

## Conclusion and Availability
Collegium, hosted on GitHub (https://github.com/MPerk78/collegium), advances educational benchmarking by integrating IPEDS and US Census data with K-means clustering. Its user-friendly interface and interactive features empower stakeholders to strategize based on diverse metrics. Valuable for educators, policymakers, and researchers, Collegium fosters collaboration and data-driven strategies. It's open-source, adaptable for future enhancements and expansion beyond community colleges and with more datasets. By simplifying data integration and analysis, Collegium streamlines peer institution identification, enhancing institutional performance and student outcomes while facilitating informed decision-making in education.

# References
