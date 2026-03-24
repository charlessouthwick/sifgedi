# sifgedi: "Multi-sensor analysis of canopy density and physiology as drivers of seasonal solar-induced fluorescence dynamics in the Amazon"

The code available in this repository supports the findings of Southwick et al. (2026), "Multi-sensor analysis of canopy density and physiology as drivers of seasonal solar-induced fluorescence dynamics in the Amazon".

Zenodo v3 release: https://doi.org/10.5281/zenodo.19207004
Zenodo release contains all code, and some of the smaller output data products. These were copied over from a different repository for the peer review process and therefore do not match the repository structure in the code. 

Most of the raw data supporting the findings of this paper are publicly available (with a NASA Earthdata login). TROPOSIF data were used with permission. Many of the early (lower-numbered) scripts in this repository involve extracting, cleaning, and reorganizing this raw data. Cleaned and compiled data will be made available in a separate Zenodo. The scripts in this repository starting at 5.1 can be used to reproduce the majority of the derived data available in the Zenodo. Earlier scripts work on the publicly available data.


Note #1: Some of the early scripts in this repo are written to be run on a computer that supports parallel processing. We use the 'mclapply' function regularly; this comes from a Mac-specific package called 'parallel'. PC users will have to modify scripts accordingly. Some of these scripts take many hours and use >100 GB of memory. Proceed with caution.

Note #2: Most of the code is written in R. A few early scripts are written in MATLAB to facilitate easier extraction of raw satellite data from hdf4 type files. These are available in a subfolder within the 'scripts' folder.
