# Replication: _Are plastic bag regulations effective in reducing plastic litter? Evidence from shoreline cleanups_

Anna Papp ([ap3907@columbia.edu](mailto:ap3907@columbia.edu)) and Kimberly L. Oremus ([oremus@udel.edu](mailto:oremus@udel.edu))

_ReadMe last modified_: May 7th, 2024

_Code last modified_: April 15th, 2024

_Abstract_: Plastic pollution poses threats to marine ecosystems and ecosystem services. While plastic bag bans and taxes are increasingly implemented worldwide, their effectiveness in reducing plastic litter remains unknown. Leveraging the patchwork of bag policies across different geographic scales in the United States and citizen science data on 97,774 U.S. shoreline cleanups, we assess the impact of these policies on plastic bag litter. We find that plastic bag policies lead to a 22-25% decrease in plastic bags as a share of total items collected at cleanups, with larger-scale bag bans and taxes further reducing shoreline litter. At a time when many jurisdictions are considering bag policies while others are preemptively prohibiting them, our study provides evidence that these policies can reduce shoreline plastic pollution.
____

### Data 

Download the following data: 

- __Policies__: Compiled from [Bag the Ban](https://www.bagtheban.com/in-your-state/), the [Retail Compliance Center](https://www.rila.org/retail-compliance-center/consumer-bag-legislation), [BagLaws.com](https://www.baglaws.com/), the [Duke Nicholas Institute's Plastics Policy Inventory](https://nicholasinstitute.duke.edu/plastics-policy-inventory), and [Wikipedia](https://en.wikipedia.org/wiki/Plastic_bag_bans_in_the_United_States); and using [MassGreen](http://www.massgreen.org/plastic-bag-legislation.html) and [Californians Against Waste](https://www.cawrecycles.org/list-of-local-bag-bans) to confirm legislation in Massachusetts and California.
    - Saved in __data/policies__ directory, provided in directory.
- __TIDES__: Downloaded cleanup data for the United States from Ocean Conservancy [website](https://www.coastalcleanupdata.org/reports). 
    - Download files for 2000-2009, 2010-2014, and then each separate year from 2015 until 2023.
    - Save files in the __data/tides__ directory, as _year.csv_ (and _2000-2009.csv_, _2010-2014.csv_)
- __Shapefiles__: Download shapefiles for processing cleanups and policies.
    - Download county shapefiles from [here](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html) from the US Census Bureau.
    - Download TIGER Zip Code tabulation areas from [here](https://catalog.data.gov/dataset/tiger-line-shapefile-2019-2010-nation-u-s-2010-census-5-digit-zip-code-tabulation-area-zcta5-na) from the US Census Bureau (through [data.gov](data.gov).
    - Save files in the __data/shapefiles__ directory, county shapefile should be in folder called _county_, files called _cb_2018_us_county_500k.shp_, while zip codes shapefile folder and files should be called _tl_2019_us_zcta510_.
    - Download lakes from [Natural Earth](https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-lakes/) (North America supplement) and save in _lakes_ folder in __data/shapefiles__ directory.
    - Download rivers from [USGS](https://www.sciencebase.gov/catalog/item/4fb55df0e4b04cb937751e02) and save in _rivers_ folder in __data/shapefiles__ directory.
- __Other__: Helper files with US county and state fips codes, lists of US counties and zip codes in __data/other__ directory, provided in the directory except as noted below.
    - Download zip code list and 2020 IRS population data from United States zip codes [here](https://www.unitedstateszipcodes.org/zip-code-database/) and save as _uszipcodes.csv_ in __data/other__ directory.
    - Download demographic characteristics of zip codes from [Social Explorer](https://www.socialexplorer.com/) and save as _raw_zip_characteristics.csv_ in __data/other__ directory.

Refer to the .txt files in each data folder to ensure all necessary files are downloaded. 

____
### Code

Scripts to run entire pipeline: 
- __`_00-02_create_data.R`__: This script runs all of Steps 0 - 2 and creates the final data used in the analysis (it requires all of the data outlined above).
   - Please see Section Data Code Details below for more details on file requirements and steps.
   - All final data (various spatio-temporal aggregations) should be saved in the __data/processed/02_data_merged/__ folder.   
- __`_03_04a_main_figures.R`__: This script creates all the main figures.
   - See below for each separate file.
   - All outputs are saved in the __figures/__ and __tables/__ folders.
- __`_03_04b_appendix_figures_tables.R`__: This script creates all the appendix figures and tables.
   - See below for each separate file.
   - All outputs are saved in the __figures/appendix__ and __tables/appendix__ folders. 

#### Main analysis files: 
3. __Step 03: 03_summary__: This folder creates summary statistic figures and tables for both main text and appendix.
   - Main Figures: 
       - Figure 1: `03a_figure1_data_summary.R` (data summary)
       - Figure 3 (part a): `03b_figure3_spillover_demo.R`
             - Note that this requires lake and river shapefiles (see above).
   - Appendix Figures and Tables: 
       - Appendix Table S1: `03z_appendix_tabs01_cleanup_stats.R` (cleanup summary statistics) 
       - Appendix Figure S2 and Table S2: `03z_appendix_figs02_tabs02_repeat_cleanup.R` (repeat cleanups)
       - Appendix Figure S3: `03z_appendix_figs03_policy_map.R` (map of policies in the US)
       - Appendix Figure S4: `03z_appendix_figs04_cleanup_objects.R` (chart with most common objects found in cleanups)
       - Appendix Figure S5: `03z_appendix_figs05_cleanup_year.R` (chart with the number of cleanups per year) 
4. __Step 04: 04_analysis__: This folder conducts the main regression analyses and robustness checks.
   - Main Figures and Tables:
       - Figure 2: `04a_figure2_main_results.R` (main results and placebo outcomes)
       - Figure 3 (part b): `04b_figure3_spillover.R` (spillover results)
       - Figure 4: `04c_figure4_policy_type.R` (analysis by policy type) 
   - Appendix Figures:
       - Appendix Figure S6 and Appendix Table S4: `04z_appendix_figs06_tabs04_other.R` (other outcomes)
       - Appendix Figure S7: `04z_appendix_figs07_time_aggregation.R` (robustness to various temporal aggregations of the data)
       - Appendix Figure S8: `04z_appendix_figs08_spatial_aggregation.R` (robustness to various spatial aggregations of the data)
       - Appendix Figure S9 and S10: `04z_appendix_figs09_figs10_balances.R` (robustness to using balanced panel)
       - Appendix Figure S11: `04z_appendix_figs11_pandemic.R` (robustness to dropping the pandemic)
       - Appendix Figure S12: `04z_appendix_figs12_cleanuplocation.R` (coastal vs. inland results)
       - Appendix Figure S13: `04z_appendix_figs13_state_details.R` (state results) 
____
### Data Code Details 

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

##### b. `01b_zip_characteristics.R`: add demographic characteristics to zip codes 
</summary>
        
- requires:
    - data/processed/01_zip_policy.rda (from previous step) 
    - data/other/raw_zip_characteristics.csv (downloaded from [Social Explorer](https://www.socialexplorer.com/))
- produces: 
    - data/processed/01_zip_policy_characteristics.rda 
</details>
<details><summary>

##### c. `01c_neighbor_zip.R`: find all the neighbors of each zip code 
</summary>
        
- requires:
    - data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp (zip code tabulation area shapefile)
- produces: 
    - data/processed/01_zip_neighbors_list.rda 
</details>
<details><summary>

##### d. `01d_neighbor_zip_policy.R`: find neighbors, neighbors-of-neighbors, and neighbors-of-neighbors-of-neighbors of treated zip codes 
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
    - data/processed/01_zip_policy_characteristics.rda (from step 1b)
    - data/processed/01_zip_neighbors_policy.rda (from step 1d)
    - data/processed/01_county_policy.rda (from step 1a)
    - data/processed/00_data_intermediate/... (separate file for each aggregation, from step 0f)
- produces: 
    - data/processed/02_data_merged/... (separate file for each aggregation)
</details>
