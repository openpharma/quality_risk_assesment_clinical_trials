
# Quality Risk Assesment Clinical Trials

- [Quality Risk Assesment Clinical Trials](#quality-risk-assesment-clinical-trials)
  - [Publication](#publication)
  - [Anonymization](#anonymization)
  - [Documentation and Code Output](#documentation-and-code-output)
  - [Run Code](#run-code)
    - [Docker](#docker)
    - [Targets](#targets)
    - [Run all code and render website](#run-all-code-and-render-website)
      - [Shell](#shell)
      - [Rstudio](#rstudio)
  
## Publication

This repository contains part of the analysis code used for:


Koneswarakantha, B., Ménard, T. *Statistical modeling for quality risk assessment of clinical trials: follow-up at the era of remote auditing* (under-review) [Therapeutic Innovation & Regulatory Science](https://www.springer.com/journal/43441)


**Background** - As investigator site audits have largely been conducted remotely during the COVID-19 pandemic, remote quality monitoring has gained some momentum. To further facilitate the conduct of remote Quality Assurance (QA) activities, we developed new quality indicators, building on a previously published statistical modelling methodology.   
**Methods** - We modeled the risk of having an audit or inspection finding using historical audits and inspections data from 2011 - 2019. We used logistic regression to model finding risk for 4 clinical impact factor (CIF) categories: Safety Reporting, Data Integrity, Consent and Protecting Endpoints.   
**Results** - Resulting Area Under the Receiver Operating Characteristic Curves were between 0.59 - 0.63 with calibrated predictive ranges of 25 - 43%. The combined and adjusted risk factors could be used to easily interpret risk estimates.   
**Conclusion** - Continuous surveillance of the identified risk factors and resulting risk estimates could be used to complement remote QA strategies and help to manage audit targets and audit focus also in post-pandemic times.   

## Anonymization
We do not share any non-public data on clinical trials or company IT systems to this end the following steps have been performed.

- repository does not contain any data only code output that describes and analyses the data   
- data preparation and features selection steps were removed   
- all internal IDs were removed  
- geographic locations were removed 
- all dates where removed except for `start_date` which was set to the first day of the corresponding year  

## Documentation and Code Output

[website](https://openpharma.github.io/simaerep/quality_risk_assesment_clinical_trials)

## Run Code

The repository does not contain any data, therefore code will only run if all
data files are supplied.

### Docker

This repository uses [`docker`](https://www.docker.com/)

### Targets

[`targets`](https://github.com/ropensci/targets) is used as the execution manager.
The workflow is specified in `_targets.R` and the `_targets_r/` directory.

In order to implement changes to the workflow edit `src/Rmd/00_workflow.Rmd` and knit document from RStudio to refresh `targets` files.

Make changes to existing functions by adding `browser()` statements and executing `tar_make(callr_function = NULL)`

`targets` manages the rendering of the reports. The `.Rmd` templates can be found as `src/Rmd/_{[0-9]}2_[a-z]+.Rmd`. They begin with an underscore because like this they will be ignored by `rmarkdown::render_site`. `targets` creates `.html` files for each report into `src/Rmd` that will get picked up by `rmarkdown::render_site` and copied into `docs`. `rmarkdown::render_site` triggers only the rendering of `00_workflow.Rmd` which triggers `targets` which then renders the reports.

### Run all code and render website

#### Shell
```
docker compose build
docker compose up -d shell
docker container exec -it quality_risk_assesment_clinical_trials_shell_1 /bin/bash
R -e 'rmarkdown::render_site("src/Rmd")'
docker compose down
```

#### Rstudio
```
docker compose build
docker compose up -d rstudio
```
open [http://localhost:8787/](http://localhost:8787/)

enter user name and password as specified in `docker-compose.yml`

enter in R-Console
```
rmarkdown::render_site("src/Rmd")
```

enter in terminal
```
docker compose down
```

