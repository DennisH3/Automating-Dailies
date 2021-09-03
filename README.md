[(Français)](#le-nom-du-projet)

# Automating Dailies

This program assists data analysts in writing short articles, Dailies, where they can load a data source, manipulate it, and insert values into paragraphs.

## R Version
R 4.0.5

## Files

There are 2 main files that can be referred to:
- Daily_Replication.Rmd
- Daily_Replication_Fr.Rmd (French version of the daily)

The functions used in these notebooks can be found in:
- daily_functions.R

## Code dependencies

This file uses four packages: tidyverse, data.table, janitor, and plotly.

- tidyverse is the standard collection of R packages. Some important packages it includes are: dplyr, ggplot2, stringr, tidyr, tibble, readr, etc.
- data.table is a package used for data manipulation. It also enables the use of the `%>%` operator.
- janitor is a package that cleans data. It is used to clean the column names of the data frame
- plotly is a package that generates plots. It is an alternative to the ggplot2, and creates interactive plots.

Ensure that you are using R version 3.6.2 or above. You can see and change what version of R you are using in RStudio by going to the Tools tab -> Global options -> R Version.

## Data

Two full tables were downloaded as csv files.
- 13100111 — used in Daily_Replication.Rmd (https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310011101)
- 13100111-fr — used in Daily_Replication_Fr.Rmd (French version of the data)

Metadata for all the data is also provided.

## Daily

The daily replicated was the Canadian cancer statistics, 2020 which can be found here: https://www150.statcan.gc.ca/n1/daily-quotidien/200922/dq200922b-eng.htm

For convenience, the original daily is uploaded as Lung_Cancer_Daily_2020.pdf and Lung_Cancer_Daily_2020_fr.pdf

The data for the graphs were downloaded from the daily and are called:
- lg_data.csv — line graph data for Chart 1
- bar_data.csv — bar graph data for Chart 2
- lg_data_fr.csv — line graph data for Chart 1 (French)
- bar_data_fr.csv — bar graph data for Chart 2 (French)

### How to Contribute

See [CONTRIBUTING.md](CONTRIBUTING.md)

### License

Unless otherwise noted, the source code of this project is covered under Crown Copyright, Government of Canada, and is distributed under the [MIT License](LICENSE).

The Canada wordmark and related graphics associated with this distribution are protected under trademark law and copyright law. No permission is granted to use them outside the parameters of the Government of Canada's corporate identity program. For more information, see [Federal identity requirements](https://www.canada.ca/en/treasury-board-secretariat/topics/government-communications/federal-identity-requirements.html).

______________________

## Le nom du projet

- Quel est ce projet?
- Comment ça marche?
- Qui utilisera ce projet?
- Quel est le but de ce projet?

### Comment contribuer

Voir [CONTRIBUTING.md](CONTRIBUTING.md)

### Licence

Sauf indication contraire, le code source de ce projet est protégé par le droit d'auteur de la Couronne du gouvernement du Canada et distribué sous la [licence MIT](LICENSE).

Le mot-symbole « Canada » et les éléments graphiques connexes liés à cette distribution sont protégés en vertu des lois portant sur les marques de commerce et le droit d'auteur. Aucune autorisation n'est accordée pour leur utilisation à l'extérieur des paramètres du programme de coordination de l'image de marque du gouvernement du Canada. Pour obtenir davantage de renseignements à ce sujet, veuillez consulter les [Exigences pour l'image de marque](https://www.canada.ca/fr/secretariat-conseil-tresor/sujets/communications-gouvernementales/exigences-image-marque.html).
