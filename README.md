# Replication: _Are plastic bag regulations effective in reducing plastic litter? Evidence from shoreline cleanups_
Anna Papp ([ap3907@columbia.edu](mailto:ap3907@columbia.edu)) and Kimberly L. Oremus ([oremus@udel.edu](mailto:oremus@udel.edu))
_ReadMe last modified_: May 7th, 2024
_Code last modified_: 

_Abstract_: 
____

### Data 

- __TIDES__: Downloaded cleanup data for the United States from Ocean Conservancy [website](https://www.coastalcleanupdata.org/reports). 
    - Download files for 2000-2009, 2010-2014, and then each separate year from 2015 until 2023. Save files in the __data/tides__ directory.
- __Other__: Helper files with county and state fips codes in __data/other__ diretory.

____
### Code

After downloading the above data, run the following scripts in order. 

- __00_cleanup__ (compile and format cleanup data)
    - __00a_cleanup_data_country.R__: compile cleanups and match to county
    - <details>
      <summary>00a_cleanup_data_country.R: compile cleanups and match to county</summary>
        
- requires: 
    - data/shapefiles/county/cb_2018_us_county_500k.shp (county shapefile)
    - data/other/us-state-ansi-fips.csv 
    - data/other/statefips.csv (state fips codes)
    - data/other/us-county-ansi-fips.csv (county fips codes)
- produces: 
    - data/processed/00_data_cleanup_county.rda
        
      </details>
        
    - __00b_cleanup_data_zip.R__: match cleanups to zip code 
        - requires: 
            - data/processed/00_data_cleanup_country.rda (from previous step)
            - data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp (zip code tabulation area shapefile)
        - produces: 
            - data/processed/00_data_cleanup_county_zip.rda 
    - __00c_cleanup_data_cell.R__: create 0.1/0.01/0.001 degree cells
        - requires: 
            - data/processed/00_data_cleanup_county_zip.rda (from previous step)
        - produces: 
            - 
- __01_policy__ (compile and format bag policy data)


- __FILE__: 
        - requires: 
            - 
        - produces: 
            - 
            
