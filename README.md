# sifgedi: "Leaf area does not drive Amazon's seasonal SIF change"

The code available in this repository supports the findings of Southwick et al. (20**), "Leaf area index does not drive Amazon's seasonal SIF change".

Most of the raw data supporting the findings of this paper are publicly available (with a NASA Earthdata login). TROPOSIF data were used with permission. Many of the early (lower-numbered) scripts in this repository involve extracting, cleaning, and reorganizing this raw data. Cleaned and compiled data are available in a separate Zenodo. Downloading that repository will allow for a subset the scripts in this repository to be run, starting at ****.


Note #1: Some of the early scripts in this repo are written to be run on a computer that supports parallel processing. We use the 'mclapply' function regularly; this is a Mac-specific package and PC users will have to modify scripts accordingly. Some of these scripts take many hours and use >100 GB of memory. Please run with caution.

Note #2: Most of the code is written in R. A few early scripts are written in MATLAB to better support the extraction of raw satellite data from hdf4 type files. The name of MATLAB scripts cannot begin with numbers, so these follow a slightly different naming format.
