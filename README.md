# electricity-consumption-analysis-web-app
simple R shiny web app for analyzing residential consumption of electricity.

Data Source: [Residential Energy Consumption Survey](https://www.eia.gov/consumption/residential/data/2015/)

It was tested only in Ubuntu 16.04.

## Requirements
* R
```bash
# in bash
$ sudo apt-get install r-base
```
* shiny 
install shiny library
```bash
# in bash
sudo su - \
-c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
```

Then, install shiny-server
```bash
$ sudo apt-get install gdebi-core
$ wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.7.907-amd64.deb
$ sudo gdebi shiny-server-1.5.7.907-amd64.deb
```

## Run
just run bash script `deploy.sh`

## License
MIT License
