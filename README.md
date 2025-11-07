# sifgedi: "Leaf area index does not drive seasonal SIF change in the Amazon"

The code available in this repository supports the findings of Southwick et al. (2025), "Leaf area index does not drive seasonal SIF change in the Amazon".

Most of the raw data supporting the findings of this paper are publicly available (with a NASA Earthdata login). TROPOSIF data were used with permission. Many of the early (lower-numbered) scripts in this repository involve extracting, cleaning, and reorganizing this raw data. Cleaned and compiled data will be made available in a separate Zenodo. The scripts in this repository starting at 5.1 can be used to reproduce the majority of the derived data available in the Zenodo. Earlier scripts work on the publicly available data.


Note #1: Some of the early scripts in this repo are written to be run on a computer that supports parallel processing. We use the 'mclapply' function regularly; this comes from a Mac-specific package called 'parallel'. PC users will have to modify scripts accordingly. Some of these scripts take many hours and use >100 GB of memory. Proceed with caution.

Note #2: Most of the code is written in R. A few early scripts are written in MATLAB to better support the extraction of raw satellite data from hdf4 type files. The name of MATLAB scripts cannot begin with numbers, so these follow a slightly different naming format, and are available in a subfolder within the 'scripts' folder.
