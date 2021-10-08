FROM rocker/verse:4.1.0

RUN R -e "remotes::install_version('yardstick', repos='http://cran.us.r-project.org', version = '0.0.8' , upgrade = 'never')"
RUN R -e "remotes::install_version('gt', repos='http://cran.us.r-project.org', version = '0.3.0' , upgrade = 'never')"
RUN R -e "remotes::install_version('igraph', repos='http://cran.us.r-project.org', version = '1.2.6' , upgrade = 'never')"

RUN R -e "remotes::install_version('targets', repos='http://cran.us.r-project.org', version = '0.6.0' , upgrade = 'never')"

RUN R -e "remotes::install_version('DT', repos='http://cran.us.r-project.org', version = '0.18' , upgrade = 'never')"
RUN R -e "remotes::install_version('cowplot', repos='http://cran.us.r-project.org', version = '1.1.1' , upgrade = 'never')"
RUN R -e "remotes::install_version('visNetwork', repos='http://cran.us.r-project.org', version = '2.0.9' , upgrade = 'never')"
RUN R -e "remotes::install_version('styler', repos='http://cran.us.r-project.org', version = '1.5.1' , upgrade = 'never')"
RUN R -e "remotes::install_version('tarchetypes', repos='http://cran.us.r-project.org', version = '0.2.1' , upgrade = 'never')"
RUN R -e "remotes::install_version('car', repos='http://cran.us.r-project.org', version = '3.0-11' , upgrade = 'never')"

ENV XDG_CONFIG_HOME=/home/rstudio/app/.config/