% Define the input and output folder paths
inputFolder = '/Users/charlessouthwick/Documents/PhD/sifgedi/mcd12c1_landcover';
outputFolder = '/Users/charlessouthwick/Documents/PhD/sifgedi/mcd12c1_landcover';

% Get a list of all HDF files in the input folder
inputFiles = dir(fullfile(inputFolder, '*.hdf'));

% Loop through each HDF file
for i = 1:length(inputFiles)
    inputFilePath = fullfile(inputFolder, inputFiles(i).name);
    
    try
        % Read HDF file and extract required layers (adjust dataset names if needed)
        lc_type = hdfread(inputFilePath, '/Majority_Land_Cover_Type_1');
        lc_assessment = hdfread(inputFilePath, '/Majority_Land_Cover_Type_1_Assessment');
        
        % Combine the layers into a single raster stack
        combinedRaster = cat(3, lc_type, lc_assessment);  % [3600 x 7200 x 2]

        % Define manual georeferencing based on known grid layout
        nrows = 3600;
        ncols = 7200;
        latlim = [-90 90];
        lonlim = [-180 180];

        RasterReference = georasterref( ...
            'RasterSize', [nrows, ncols], ...
            'Latlim', latlim, ...
            'Lonlim', lonlim, ...
            'RasterInterpretation', 'cells', ...
            'ColumnsStartFrom', 'north');

        % Create output file name
        [~, filename, ~] = fileparts(inputFilePath);
        outputFileName = ['processed_', filename, '_lctype.tif'];

        % Define output file path
        outputFilePath = fullfile(outputFolder, outputFileName);

        % Save the raster as GeoTIFF
        geotiffwrite(outputFilePath, combinedRaster, RasterReference);

        % Display confirmation
        disp(['GeoTIFF saved to: ', outputFilePath]);

    catch ME
        % Handle errors
        warning('Failed to process %s. Error: %s', inputFilePath, ME.message);
    end
end
