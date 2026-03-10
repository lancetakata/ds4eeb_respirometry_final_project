# Future viability readme

**Motivating Question:** The goal of this project is to understand the how temperature-dependent metabolic performance varies across transitional life stages of an anadromous species.

**Rationale**: Changing ocean conditions will result in altered marine ecosystems. **As marine conditions change, it is important to understand how populations will be redistributed throughout the oceans as current habitat becomes unsuitable and new viable habitat becomes available**. Ecological theory of habitat selection by species originally focused on biotic and abiotic interactions, but now also includes the physiology of the species themselves^1^. One method of predicting future distribution of marine species is by understanding their physiological sensitivities to hydrographic conditions to determine potentially viable habitat^2^. Ectotherms, such as fish, are particularly physiologically sensitive to temperature^3^ and temperature-dependent metabolism is a commonly used metric for physiological performance^4^ .

**Background**: We used laboratory respirometry experiments and computational modeling to understand the temperature-dependent metabolic performance across California central valley Chinook salmon *(Onchorhychus tshawystscha*) lifestages.

**Study System:** California central valley Chinook salmon (*Oncorhynchus tshawystscha*) are an ideal study system for this project due to their economic, cultural and ecological importance. Fall-run Chinook salmon supports a large commercial and recreational fishing, while winter-run Chinook salmon are state and federally listed endangered species due to their dramatic population decline and critically low population^5^. There is already a large amount of research focused on the conservation of California Chinook salmon, but the majority is focused on freshwater survival^6^. The future condition of their marine habitat, the California Current, is uncertain due to climate change^7^ and how the species will respond is a question to be answered. For these reasons, California Chinook salmon are a model study system for this investigation.

**Data Collection:** Using NOAA SWFSC resources in Santa Cruz, CA, we raised hatchery-born California central valley Chinook salmon to a sub-adult marine life stage. These fish were raised in an artificial habitat under current ocean conditions in the California Current. Across three different transitional life stages we determined their metabolic performance capacity using closed respirometry at 13 different temperatures(8-20°C). The three life stages were a freshwater smolt lifestage, a saltwater smolt lifestage, and a saltwater sub-adult. Closed respirometry is the process of placing an organism in an airtight chamber and recording the dissolved oxygen consumption. In many species, such as salmon, the rate at which oxygen is consumed can be converted into the organism’s metabolic rate^4^. The process of closed respirometry entaileded acclimating groups of fish at a target temperature for 48 hours, then placing individuals into respirometry chambers for 24 hours to measure their dissolved oxygen consumption. Our data collection methods included an intermittent respirometry experiment where dissolved oxygen consumption was recorded at an elevated metabolic rate. This was acheived by conducting an exhaustive chase protocol then immediately placing the individual into the respirometry chamber. Additionally, closed respirometry experiment was conducted to record oxygen limited metabolic activity. This was done by sealing the respirometry chamber and allowing the individual to reduce the environmental oxygen. Finally, post-mortem data was collected on each individual including a wet weight, condition factor, hepatosomatic index, % dry weight of liver, and % dry weight of tissue.

**Target Audience:** These data are being analyzed and presented as a part of the UCSC Data Science for EEB course final project. Thus, findings are tailored to those familiar with ecology and evolutionary biology, but without the assumption that they are familiar with the study system.

**References**:

^1^Rosenzweig, 1991. ^2^Fausch, 1984. ^3^Huey and Kingsolver, 1993, Hochachka and Somero, 2002. ^4^Fry, 1971. ^5^Moyle et al 2017.  ^6^Eliason and Farrell, 2016. ^7^Crozier et al, 2019. ^8^Deutsch et al. 2015, Burford et al. 2022.

**Data Files**

**Postmortem**: *meta_postmort.csv*

Metadata associated with each fish that will be important in data analysis. For use in data analysis. Includes identifying information (fish id, experiment/group, holding tank, acclimation tank, respirometer chamber), information about fish fate (resting or pcrit mort and equipment failure), and postmortem metrics (wet weight, condition factor, hepatosomatic index, % dry weight of liver, % dry weight of tissue). Note that all fish are included in these data (n=144).

**Alphaint:** *alpha_int.csv*

Maximum oxygen supply capacity recorded during intermittent respirometry (alphaint) and associated experimental conditions. For use in data analysis. Includes identifying information (fish id), experimental conditions (date and time of alphaint, salinity, oxygen concentration, temperature, atmospheric pressure), and alphaint. Note that there are 3 values per fish – these values can be averaged to produce a final alphaint. Also note that these data include both fish that completed or died during the overnight trial (including that resulting from equipment failure) (n=137). The fish that are not a part of these data are those where acclimation failed (group2) (n=7).

**Alphaclose**: *alpha_close.csv*

Maximum oxygen supply capacity recorded during closed respirometry (alphaclose) and associated experimental conditions. For use in data analysis. Includes identifying information (fish id), experimental conditions (date and time of alphaclose, salinity, oxygen concentration, temperature, atmospheric pressure), and alphaclose. Note that there are 3 values per fish – these values can be averaged to produce a final alphaclose. Also note that these data include both fish that completed the overnight intermittent trial (n=110) and the two fish (FV043 and FV048) that were a part of the experiment where windows updated and closed AutoResp during the overnight period. These two happened to be in the flush phase, so closed respirometry was run for them the following morning. They can be removed prior to analysis, but are here for now.

**R Scripts**

**Respirometry data analysis**: *respirometry_data_analysis.qmd*

File containing the data analysis of respirometry data. It contains a workflow to determine which alpha values to use from the intermittent and closed data sets. It also contains a workflow to better understand the variation in alpha values seen using both bayesian and frequentist models.
