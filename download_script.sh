#!/usr/bin/env bash

for i in {1985..2017};
do
    wget -O "$i.txt" "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h$i.txt.gz&dir=data/historical/stdmet/"
done
