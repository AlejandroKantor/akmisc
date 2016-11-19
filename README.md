
## akmisc


Collection of miscellaneous functions for data manipulation written by Alejandro Kantor. Most functions use data.table. *Currently in development*.

### Instalation

Package can be installed with the following R code:


``` r
is_not_installed <- ! "devtools" %in% rownames(installed.packages())
if (packageVersion("devtools") < 1.6 | is_not_installed ) {
   install.packages("devtools")
}
devtools::install_github("AlejandroKantor/akmisc")
```



