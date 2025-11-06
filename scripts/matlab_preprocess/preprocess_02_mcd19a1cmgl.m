% Define folder paths
inputFolder = '/Users/charlessouthwick/Documents/PhD/sifgedi/mcd19a1cmgl_data_2021/raw/';
outputFolder = '/Users/charlessouthwick/Documents/PhD/sifgedi/mcd19a1cmgl_data_2021/initial_process/';

% Get a list of all HDF files in the input folder
inputFiles = dir(fullfile(inputFolder, '*.hdf'));

% Loop through each HDF file
for i = 1:length(inputFiles)
    inputFilePath = fullfile(inputFolder, inputFiles(i).name);
    
    % Read HDF file and extract required layers
    band1 = hdfread(inputFilePath, '/NBAR_B01');
    band2 = hdfread(inputFilePath, '/NBAR_B02');
    qa_qc = hdfread(inputFilePath, '/QA');
    
    % Combine the two bands and the BRDF_Quality layer into a single raster
    combinedRaster = cat(3, band1, band2, qa_qc);
    
    % Define spatial referencing information
    latlim = [-90 90];  % Latitude limits
    lonlim = [-180 180];  % Longitude limits
    RasterSize = size(combinedRaster(:,:,1));  % Size of the raster
    RasterInterpretation = 'cells';  % Interpretation of raster cells
    ColumnsStartFrom = 'north';  % Start of column indexing
    RowsStartFrom = 'west';  % Start of row indexing
    RasterExtentInWorldX = diff(lonlim) / RasterSize(2);  % Longitude resolution
    RasterExtentInWorldY = diff(latlim) / RasterSize(1);  % Latitude resolution
    RasterReference = georasterref('RasterSize', RasterSize, 'Latlim', latlim, 'Lonlim', lonlim, ...
        'ColumnsStartFrom', ColumnsStartFrom, 'RowsStartFrom', RowsStartFrom, ...
        'RasterInterpretation', RasterInterpretation);
    
    % Create output file name
    [~, filename, ~] = fileparts(inputFilePath);
    outputFileName = ['firstproc_', filename, '.tif']; % Change output file name to indicate initial processing
   
    % Save the combined raster as a GeoTIFF file
    outputFilePath = fullfile(outputFolder, outputFileName);
    % Write GeoTIFF file
    geotiffwrite(outputFilePath, combinedRaster, RasterReference); % Save the raster with all three layers
    
    disp(['GeoTIFF saved to: ', outputFilePath]);
end
