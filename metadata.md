# Metadata

## Lifestages:
* freshwater smolt - smolt lifestage, pre-ocean entry
* saltwater smolt - smolt lifestage introduced to saltwater for 3 days
* subadult - fish that were raised to the subadult lifestage, ~6months in natural ocean conditions

## Salinity:
* Salinity column determines whether an individual was tested in freshwater (i.e., freshwater smolt) or in saltwater (i.e., saltwater smolt or subadults)
* Freshwater smolts have remained in freshwater thus far in there life cycle.
* Saltwater smolts have been acclimated in saltwater for 3 days.
* Saltwater subadults have been in saltwater for ~6 months.

## Alpha:
* Alpha is the maximum metabolic rate of an individual at a given temperature and oxygen level.
  * Alpha intermittent ("alphaint") is the maximum oxygen supply capacity recorded during intermittent respirometry.
  * Alpha closed ("alphaclose") is the maximum oxygen supply capacity recorded during closed respirometry.

# Alpha Intermittent data:
* fish_id: an identifying number specific to each individual fish.
* datetime: the date/time at which the measurement of a trial was recorded.
* sal_ppt: same as the salinity column above. This identifies the salinity of the water during the trial, 0 being freshwater and 35 being saltwater.
* o2_mgl : the percent o2 in the chamber in units of mg/l at each measurement.
* temp_c : the temperature in the chamber as measured in degrees Celsius.
* pres_hpa: the ambient atmospheric pressure during the trial.
* alpha_int_mgo2_kg_h_kPa: the alpha value at a given time/temp during the intermittent trial.

# Alpha Closed data:
* fish_id: an identifying number specific to each individual fish.
* datetime: the date/time at which the measurement of a trial was recorded.
* sal_ppt: same as the salinity column above. This identifies the salinity of the water during the trial, 0 being freshwater and 35 being saltwater.
* o2_mgl : the percent o2 in the chamber in units of mg/l at each measurement.
* temp_c : the temperature in the chamber as measured in degrees Celsius.
* pres_hpa: the ambient atmospheric pressure during the trial.
* alpha_close_mgo2_kg_h_kPa: the alpha value at a given time/temp during the closed trial.

## Post-mortem data:
* exp: this indicates the group of fish that were acclimated and tested together.
* holding_tank: this is the holding tank where fish were raised in.
* acc_tank: this is the acclimation tank where fish were held for the 3 day acclimation period.
* chamber: identifies the chamber used for respirometry trial.
* fish_mort_resting: "Y" indicates the fish died during the intermittent trial.
* fish_mort_pcrit: "Y" indicates the fish died during the closed trial.
* equipment_failiure_resting: "Y" indicates an equipment failure during the intermittent trial.
* posttrial_wet_weight_g: the wet weight in grams of an individual after the trial has concluded.
* cf: this is the condition factor, a ratio of the ratio of body weight to fork length cubed. A measure of how healthy a fish is.
* hsi: this is the hepatosomatic index, a ratio of the liver mass to total body mass.
* l_perc_dw: liver % dry weight, this measure is gained by measuring the liver weight before and after desiccating the sample.
* t_perc_dw: tissue % dry weight, this measure is gained by measuring a muscle tissue sample before and after desiccating the sample.
