# Future viability readme
This is just for the first round of experiments, info and data will change once we complete the second round.

Acclimation

General info
Mean duration of 2.14 days (range = 2.02 to 2.23 days). Mean absolute value of difference between mean holding temp and temp at intro to respirometer of 0.29°C (range = 0.03 to 0.91°C).

acc_flag.csv
Supporting table of acclimation metadata. For use in paper supplement, if needed. Includes identifying information (experiment/group, starting temperature, starting salinity, acclimation and experimental salinity, target temperature), aspects of temperature ramp (rate, magnitude, duration), aspects of holding temperature (min, mean, max, duration), and aspects of destination temperature (respirometer temperature at fish introduction).

acc_flag.png
Supporting figure of acclimation temperature timeseries. For use in paper supplement, if needed.

Postmortem
meta_postmort.csv
Metadata associated with each fish that will be important in data analysis. For use in data analysis. Includes identifying information (fish id, experiment/group, holding tank, acclimation tank, respirometer chamber), information about fish fate (resting or pcrit mort and equipment failure), and postmortem metrics (wet weight, condition factor, hepatosomatic index, % dry weight of liver, % dry weight of tissue). Note that all fish are included in these data (n=144).

SMR
smr_exp.csv
Metabolic rates identified as SMR and associated experimental conditions. For use in data analysis. Includes identifying information (fish id), experimental conditions (date and time of SMR, salinity, oxygen concentration, temperature, atmospheric pressure), and the SMR metabolic rate. Note that these data only include fish that completed the overnight intermittent trial (n=110).

Alphaint
alpha_int.csv
Maximum oxygen supply capacity recorded during intermittent respirometry (alphaint) and associated experimental conditions. For use in data analysis. Includes identifying information (fish id), experimental conditions (date and time of alphaint, salinity, oxygen concentration, temperature, atmospheric pressure), and alphaint. Note that there are 3 values per fish – these values can be averaged to produce a final alphaint. Also note that these data include both fish that completed or died during the overnight trial (including that resulting from equipment failure) (n=137). The fish that are not a part of these data are those where acclimation failed (group2) (n=7).

Alphaclose
alpha_close.csv
Maximum oxygen supply capacity recorded during closed respirometry (alphaclose) and associated experimental conditions. For use in data analysis. Includes identifying information (fish id), experimental conditions (date and time of alphaclose, salinity, oxygen concentration, temperature, atmospheric pressure), and alphaclose. Note that there are 3 values per fish – these values can be averaged to produce a final alphaclose. Also note that these data include both fish that completed the overnight intermittent trial (n=110) and the two fish (FV043 and FV048) that were a part of the experiment where windows updated and closed AutoResp during the overnight period. These two happened to be in the flush phase, so closed respirometry was run for them the following morning. They can be removed prior to analysis, but are here for now.

int_rates.csv
Full timeseries of metabolic rates for all intermittent respirometry phases. For use in data analysis. Includes identifying information (fish id), experimental conditions (phase, date and time, salinity, oxygen concentration, temperature, atmospheric pressure), and metabolic rate and its R2. Obviously, these data include both fish that completed or died during the overnight trial (including those resulting from equipment failure) (n=137). The fish that are not a part of these data are those where acclimation failed (group2) (n=7). For the mortality risk analysis, the idea would be to use SMR (or predicted SMR for fish that died) to define when mortality occurred, and then use a time to event model to understand how mortality risk varied with experimental conditions and fish condition. A simpler (but perhaps less powerful) approach would be to have fish mortality as a binary variable and to use a logistic regression to model the probability of mortality.

close_rates.csv
I am including the closed rates data too – no specific reason for their use yet, but we may need a clean version of them later. These are the full timeseries of metabolic rates for all closed system respirometry phases. Includes identifying information (fish id), experimental conditions (date and time, salinity, oxygen concentration, temperature, atmospheric pressure), and metabolic rate and its R2 (1-min rates using rolling regression and 1-min averaged parameters). Also note that these data include both fish that completed the overnight intermittent trial (n=110) and the two fish (FV043 and FV048) that were a part of the experiment where windows updated and closed AutoResp during the overnight period. These two happened to be in the flush phase, so closed respirometry was run for them the following morning.
