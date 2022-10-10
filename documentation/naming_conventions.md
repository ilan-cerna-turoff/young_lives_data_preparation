**<u>File naming conventions</u>**

-   See the main README.md file for a cheatsheet of the below naming conventions.
-   **All files** should be named using `snake_case_syntax` (all lowercase as well).
-   **All `.R` scripts** should be named using the following format: 

    ```[survey type]_r[round number]_[country abbreviation].R```

    where the three tags have possible values of:
    -    `survey_type = {com_main, cvd_och, cvd_ych}` for community survey, covid survey with older children, and covid suvey with younger children
    -    `round_number = {1, 2, 3, ...}`
    -    `country_abbreviation = {et, in, pe, vt}` for Ethiopia, India, Peru, and Vietnam 

    Some examples include:
    -    `com_main_r1_et.R` is the filename for the script that cleans the first round of Ethiopia's main community survey data
    -    `cvd_och_r2_pe.R` is the filename for the script that cleans the second round of Peru's older children covid survey data

-   **All raw data files** should be placed into the appropriate survey folder inside the `data/raw/` directory (i.e. the raw main community survey data should be in `data/raw/com_main` while the raw covid survey data for older children should be in `data/raw/cvd_och`). Prefixes should then be added to the raw data files so that they are recognised by the automated functions. These prefixes follow the same convention as the R scripts:

    ```r[round number]_[country abbreviation]_[file descriptor].[extension]```

    For example: `data/raw/com_main/r5_vt_communitylevel.dta` is the proper location and name for round 5 of Vietnam's main community survey file. The original file was named `communitylevel.dta`. The prefixes have been added for consistency and reduced errors when handling data

-   **All intermediate data files** are saved into the appropriate survey folder inside the `data/intermediate/` directory. They should be similarly named in the following format:

    ```[country abbreviation]_r[round number].[extension]```

    For example, `data/intermediate/com_main/r1_et.rds` gives the correct filepath and name for the cleaned version of the first round of Ethiopia's main community survey data
