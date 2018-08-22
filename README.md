# electricity-consumption-analysis-web-app
simple R shiny web app for analyzing residential consumption of electricity.

Data Source: [Residential Energy Consumption Survey](https://www.eia.gov/consumption/residential/data/2015/)

It was tested only in Ubuntu 16.04.

## Requirements
### Install in local machine
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
### Install in docker
```bash
# before run setup.sh, you should change 'config.conf.template' file 
# for docker configuration and rename it to 'config.conf'

# in bash
$ cd utils
$ bash setup.sh
```

## Run
### Run in local
edit `utils/config.conf`(please rename `utils/config.conf.template` to ``utils/config.conf), set environment variable properly and just run bash script `utils/deploy.sh`

### Run in docker
`utils/config.conf` is already set while docker setup so you can run just `deploy.sh`, `stage-deply.sh` or 'test-deploy.sh`

## License
MIT License
