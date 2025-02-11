#!/bin/bash
python3 -m venv tempEnv 
source tempEnv/bin/activate
tempEnv/bin/pip install --use-pep517 odfpy
tempEnv/bin/pip install pandas
python3 ./ProcessTrainingLog.py
deactivate
rm -r tempEnv
R -e "shiny::runApp('app.R',launch.browser=TRUE)"
