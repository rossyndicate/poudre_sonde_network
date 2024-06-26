# Poudre Sonde Network Quality Assurance/Quality Control Verification

## 0 Pre-processing

Before beginning verification, the most recently flagged data needs to be 
pre-processed before being put through the verification pipeline. Pre-processing
adds verification tracking information to the flagged data frames and plugs that 
data into the directory structure required for verification. 

`0_verification_preprocessing.Rmd` must be run entirely and only once at the 
beginning of a *verification cycle*. A *verification cycle* is a recurring period 
during which the data across all of your sites is thoroughly reviewed and validated. 
This process ensures the accuracy and integrity of the information presented to users. 
The verification cycle involves a comprehensive audit of the data not yet verified 
thus far, spanning a significant duration to encompass all relevant sites 
and data sources.

This is to say that when `0_verification_preprocessing.Rmd` is executed at the 
beginning of a verification cycle, it should not be executed until the next 
verification cycle begins (this will likely occur annually). 

## 1 Set up

Once a verification cycle has been established, each site-parameter combination 
will be verified. A *verification round* is the granular cycle of verification. 
A *verification round* is the verification of a year's plus worth of data on a 
weekly (or daily or sub-daily) basis.

For each *verification round* `1_verification_setup.Rmd` must be executed. This 
file will:

1. load in the necessary libraries
2. source the necessary functions
3. establish the working directory for verification (*NOTE: this is not the same as setting a working directory in R*)
4. set the site-parameter combination for the verification round
5. generate the weekly plot data to be verified.

#### Notes on establishing the working directory for verification and setting the site-parameter combination for the verification round.

Within `1_verification_setup.Rmd` the `working data selection`, 
`set site and parameter variables`, and `set the site-param df to update` chunks
work in tandem to ensure that previous verification work is not overwritten. 

- `working data selection` will ask the user to which directory they are working from.
From here the user must decide between `pre` and `int`. `pre` refers to the 
`pre_verification_directory`, while `int` refers to the `intermediary_directory`.
These are two tracking directories that get updated based on the progress of your 
work. When starting a new site-parameter combination users should pick `pre`, when
continuing a site-parameter combination that has already been started users should
pick `int`. 

- `set site and parameter variables` is where the user updates the site and parameter
variables that will be used in the verification round. This is the site-parameter
combination for the verification round. 

- `set the site-param df to update` checks that the site-parameter combination exists,
and then checks if the site-parameter combination has already been started. If the
site-parameter combination has already been started but the user selected the `pre` 
directory, the user will be informed that they are about to overwrite verified data, 
and be asked to confirm the decision. If they decide to overwrite the verified data
it will be overwritten by data that has not been verified. However, the user for the
most part *should* not elect to overwrite the data. Once the user refuses to overwrite
the data they will automatically start using data from the intermediary directory.

## 2 Verification

Once the verification round set up is completed the user must move on to 
`2_verification.R`. This script contains the for loop that must be run in order
to verify the flagged data. In this script is also a `QUIT` toggle in order to 
reset the QUIT status, and the `clean_directories()` which should be run at the 
end of a verification round, or when a user `quits`.

To start verifying data the user will simply run the for loop. The week being
verified will populate the plot field and the user will answer prompts contained
entirely in the console. When a user is done verifying a week of data the next 
week of data will be presented until all of the data in the `week_plot_objects` object
have been verified. This workflow will track the users progress and 
update the necessary information accordingly. If a user `skips` data or `quits`,
the progress will be saved and can be resumed in the future. 

With some exceptions, here are the potential user decisions for data:

| | PASS | FAIL | SKIP | INSPECT | QUIT |
|----------|------|------|------|---------|------|
| ALL | pa | fa | sa | ia | q |
| VALID | pv | fv | | iv | |
| FLAGGED | pf | ff | | if | |
| NONE | pn | fn | | | |
| SOME | | | | is | |

- PASS: The data is considered accurate and will be included in the final dataset.
- FAIL: The data is considered inaccurate and will be excluded from the final dataset.
- SKIP: The accuracy of the data cannot be determined at this time and will be skipped for now.
- INSPECT: The user wants to inspect the data at a more granular level (e.g., daily or sub-daily) before making a decision.
- QUIT: The user wants to quit the verification process for the current week, day, or sub-daily period.

- ALL: Applies the decision to all data points within the current layer (weekly, daily, or sub-daily).
- VALID: Applies the decision only to data points that are not flagged within the current layer.
- FLAGGED: Applies the decision only to data points that are flagged within the current layer.
- NONE: Applies the decision to none of the data points within the current layer (used in combination with PASS or FAIL).
- SOME: Allows the user to select specific data points within the current layer to inspect.

- Permutations:

  - pa: Pass all data points within the current layer.
  - fa: Fail all data points within the current layer.
  - sa: Skip all data points within the current layer.
  - ia: Inspect all data points within the current layer at a more granular level.
  - q: Quit the verification process for the current week.
  - pv: Pass all valid (non-flagged) data points within the current layer.
  - fv: Fail all valid (non-flagged) data points within the current layer.
  - iv: Inspect all valid (non-flagged) data points within the current layer at a more granular level.
  - pf: Pass all flagged data points within the current layer.
  - ff: Fail all flagged data points within the current layer.
  - if: Inspect all flagged data points within the current layer at a more granular level.
  - pn: Pass none of the data points within the current layer.
  - fn: Fail none of the data points within the current layer.
  - is: Inspect some specific data points within the current layer at a more granular level.

With some exceptions, these verification decisions and permutations apply to all layers (weekly, daily, and sub-daily) of the data being verified. The user can make decisions at each layer, and the workflow will guide them through the verification process accordingly.

- Exceptions:
  - Can't inspect non-inspect data:
    - If you choose to inspect only valid or flagged data points within a layer (e.g., iv or if), you cannot inspect the remaining non-inspect data points within that layer. For example, if you choose to inspect only flagged data points for a specific day (if), you cannot inspect the valid (non-flagged) data points for that day.


  - Can only quit on the weekly level:
    - The QUIT decision (q) is only available at the weekly level. You cannot quit the verification process at the daily or sub-daily levels. If you choose to quit, the verification process will stop for the current week, and any remaining data points for that verification round will not be verified.
    
  - Can't inspect sub-daily data:
    - The INSPECT decision (i) is not available at the sub-daily level. You can only inspect data points at the weekly and daily levels. If you need to verify data at the sub-daily level, you must make a decision to pass, fail, or skip those data points.



