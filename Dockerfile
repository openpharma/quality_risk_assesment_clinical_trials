FROM rocker/verse:4.1.0

RUN R -e "remotes::install_version('tidymodels', repos='http://cran.us.r-project.org', version = '0.1.3' , upgrade = 'never')"
RUN R -e "remotes::install_version('gt', repos='http://cran.us.r-project.org', version = '0.3.0' , upgrade = 'never')"
RUN R -e "remotes::install_version('igraph', repos='http://cran.us.r-project.org', version = '1.2.6' , upgrade = 'never')"

RUN git clone -b 0.5.0 --single-branch https://github.com/ropensci/targets.git targets
RUN R -e "devtools::install('targets/', upgrade='never')"
RUN R -e "devtools::test('targets/')"
RUN rm -r targets/ -f

RUN R -e "remotes::install_version('DT', repos='http://cran.us.r-project.org', version = '0.18' , upgrade = 'never')"
RUN R -e "remotes::install_version('cowplot', repos='http://cran.us.r-project.org', version = '1.1.1' , upgrade = 'never')"
RUN R -e "remotes::install_version('visNetwork', repos='http://cran.us.r-project.org', version = '2.0.9' , upgrade = 'never')"

ENV XDG_CONFIG_HOME=/home/rstudio/app/.config/