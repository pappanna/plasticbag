# Replication: _Are plastic bag regulations effective in reducing plastic litter? Evidence from shoreline cleanups_

Anna Papp ([ap3907@columbia.edu](mailto:ap3907@columbia.edu)) and Kimberly L. Oremus ([oremus@udel.edu](mailto:oremus@udel.edu))

_ReadMe last modified_: May 7th, 2024

_Code last modified_: 

_Abstract_: Plastic pollution poses threats to marine ecosystems and ecosystem services. While plastic bag bans and taxes are increasingly implemented worldwide, their effectiveness in reducing plastic litter remains unknown. Leveraging the patchwork of bag policies across different geographic scales in the United States and citizen science data on 97,774 U.S. shoreline cleanups, we assess the impact of these policies on plastic bag litter. We find that plastic bag policies lead to a 22-25% decrease in plastic bags as a share of total items collected at cleanups, with larger-scale bag bans and taxes further reducing shoreline litter. At a time when many jurisdictions are considering bag policies while others are preemptively prohibiting them, our study provides evidence that these policies can reduce shoreline plastic pollution.
____

### Data 

Download the following data: 

- __Policies__: Compiled from [Bag the Ban](https://www.bagtheban.com/in-your-state/), the [Retail Compliance Center](https://www.rila.org/retail-compliance-center/consumer-bag-legislation), [BagLaws.com]{https://www.baglaws.com/}, the [Duke Nicholas Institute's Plastics Policy Inventory](https://nicholasinstitute.duke.edu/plastics-policy-inventory), and [Wikipedia](https://en.wikipedia.org/wiki/Plastic_bag_bans_in_the_United_States); and using [MassGreen](http://www.massgreen.org/plastic-bag-legislation.html) and [Californians Against Waste](https://www.cawrecycles.org/list-of-local-bag-bans) to confirm legislation in Massachusetts and California.
    - Saved in __data/policies__ directory, provided in directory.
- __TIDES__: Downloaded cleanup data for the United States from Ocean Conservancy [website](https://www.coastalcleanupdata.org/reports). 
    - Download files for 2000-2009, 2010-2014, and then each separate year from 2015 until 2023.
    - Save files in the __data/tides__ directory.
- __Shapefiles__: Download shapefiles for processing cleanups and policies.
    - Download county shapefiles from [here](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html) from the US Census Bureau.
    - Download TIGER Zip Code tabulation areas from [here](https://catalog.data.gov/dataset/tiger-line-shapefile-2019-2010-nation-u-s-2010-census-5-digit-zip-code-tabulation-area-zcta5-na) from the US Census Bureau (through [data.gov](data.gov).
    - Save files in the __data/shapefiles__ directory.
- __Other__: Helper files with US county and state fips codes, lists of US counties and zip codes in __data/other__ directory, provided in the directory except as noted below.
    - Download zip code list and 2020 IRS population data from United States zip codes [here](https://www.unitedstateszipcodes.org/zip-code-database/) and save in __data/other__ directory.

____
### Code

After downloading the above data, run the following scripts in order:

#### Step 0: __00_cleanup__: Compile cleanup data, match to geographic boundaries, and then aggregate to various spatio-temporal levels.
<details><summary> 
    
##### a. `00a_cleanup_data_country.R`: compile cleanups and match to county
</summary>

##### Details: 
- requires: 
    - data/shapefiles/county/cb_2018_us_county_500k.shp (county shapefile)
    - data/other/us-state-ansi-fips.csv 
    - data/other/statefips.csv (state fips codes)
    - data/other/us-county-ansi-fips.csv (county fips codes)
- produces: 
    - data/processed/00_data_cleanup_county.rda
</details>
<details><summary>
    
##### b. `00b_cleanup_data_zip.R`: match cleanups to zip code 
</summary>

##### Details: 
- requires: 
    - data/processed/00_data_cleanup_country.rda (from previous step)
    - data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp (zip code tabulation area shapefile)
- produces: 
    - data/processed/00_data_cleanup_county_zip.rda 

</details>
<details><summary>
    
##### c. `00c_cleanup_data_cell.R`: create 0.1/0.01/0.001 degree cells 
</summary>

##### Details: 
- requires: 
    - data/processed/00_data_cleanup_county_zip.rda (from previous step)
- produces: 
    - data/processed/00_data_cleanup_county_zip_cell.rda 
</details>
<details><summary>
    
##### d. `00d_cleanup_data_clean.R`: clean (normalized variables, outliers, etc.) 
</summary>

##### Details: 
- requires: 
    - data/processed/00_dat_cleanup_county_zip_cell.rda (from previous step)
- produces: 
    - data/processed/00_data_cleanup.rda
    - data/processed/00_data_cleanup_locations.csv (for Google Earth Engine distance calculations)     
</details>
<details><summary>
    
##### e. `00e_cleanup_distance.R`: calculate the distance to water bodies 
</summary>
        
- requires:
    - data/processed/00_data_cleanup.rda (from previous step)
    - data/other/distanceCoast.csv (to replicate, use [GEE script](https://code.earthengine.google.com/1c7d8600e39ea4426ed228ec37b7d880))
    - data/other/distanceRivers.csv (to replicate, use [GEE script](https://code.earthengine.google.com/f76e17729aa4d81bf88f6f27902b8f14))
    - data/other/distanceLakes.csv (to replicate, use [GEE script](https://code.earthengine.google.com/04129098eec313af5444f2a417dd8209))
- produces: 
    - data/processed/00_data_cleanup.rda
</details>
<details><summary>

##### f. `00f_cleanup_final.R`: aggregate to various levels (e.g., cell, month)
</summary>
        
- requires:
    - data/processed/00_data_cleanup.rda (from previous step)
- produces: 
    - data/processed/00_data_intermediate/... (separate files for each aggregation)
</details>


#### Step 1: __01_policy__: Compile bag policies and match to zip codes. 
<details><summary>

##### a. `01a_policy_county_zip.R`: aggregate policies
</summary>
        
- requires:
    - data/other/uscounties.csv (list of US counties)
    - data/other/uszipcodes.csv (list of US zip codes)
    - data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp (zip code tabulation area shapefile)
    - data/policies/policies.xlsx (list of bag policies) 
- produces: 
    - data/processed/01_zip_policy.rda
    - data/processed/01_county_policy.rda 
</details>
<details><summary>

##### b. `01b_neighbor_zip.R`: find all the neighbors of each zip code 
</summary>
        
- requires:
    - data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp (zip code tabulation area shapefile)
- produces: 
    - data/processed/01_zip_neighbors_list.rda 
</details>
<details><summary>

##### c. `01c_neighbor_zip_policy.R`: find all the neighbors of each zip code 
</summary>
        
- requires:
    - data/processed/01_zip_neighbors_list.rda (from previous step)
    - data/processed/01_zip_policy.rda (policy data, from step 1a.)
- produces: 
    - data/processed/01_zip_neighbors_policy.rda 
</details>


#### Step 2: __02_merge__: Merge cleanup and policy data 
<details><summary>

##### a. `01a_merge.R`: merge all policies (helper script)
</summary>
        
- requires:
    - data/other/uscounties.csv (list of US counties)
    - data/other/uszipcodes.csv (list of US zip codes)
    - data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp (zip code tabulation area shapefile)
    - data/policies/policies.xlsx (list of bag policies) 
- produces: 
    - data/processed/01_zip_policy.rda
    - data/processed/01_county_policy.rda 
</details>
<details><summary>

- __FILE__: 
        - requires: 
            - 
        - produces: 
            - 
            
