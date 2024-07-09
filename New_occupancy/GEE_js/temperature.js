var AOI = ee.FeatureCollection("projects/ee-bioamaserwah/assets/AOI_nc");
Map.centerObject(AOI, 8);
Map.addLayer(AOI, {}, "AOI");

/*
=====================================================================================
                TEMPERATURE
======================================================================================
*/

//time period 
var startDate = '2023-10-01';
var endDate = '2024-03-31';

/*
//monthly
var monthlyTemp = ee.ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR") // 1km spatial resolution
  .filterDate(startDate, endDate)
  .select(['temperature_2m'])
  .filterBounds(AOI);

var clippedTemp = monthlyTemp.map(function(image) {
  return image.clip(AOI);
});

print(clippedTemp);

function exportVariable(variable, variableName) {
  // Define export parameters
  var exportParams = {
    image: variable,
    description: variableName,
    folder: 'TEMP_DAILY',
    scale: 100, // 1km for temp and precip 
    region: AOI.geometry(),
    maxPixels: 1e13,
    crs: 'EPSG:4326',
    fileFormat: 'GeoTIFF'
  };
  // Export the variable
  Export.image.toDrive(exportParams);
}

// Define variables to export
var variablesToExport = {
 'clippedTemp' : clippedTemp
};

// Loop through each variable and export it
for (var variableName in variablesToExport) {
  exportVariable(variablesToExport[variableName], variableName);
}

// Export parameters only
var doExportDrive = function() {
  print('Working');
  var ids = clippedTemp.aggregate_array('system:index');
  ids.evaluate(function(imageIds) {
    print('Total number of images', imageIds.length);
    print('Exporting now... (see Tasks tab)');
    for(var i = 1; i <= imageIds.length; i++) {  // Start index from 1
      var image = ee.Image(clippedTemp.toList(1, i - 1).get(0));  
      Export.image.toDrive({
        image: image.clip(AOI),
        region: AOI.geometry(),
        scale: 100, // 1km for temp and precip
        fileNamePrefix: 'Monthly_Temp_' + imageIds[i - 1], 
        folder: 'Monthly_Temp_',
        description: 'Monthly_Temp_' + i + '_' + imageIds[i - 1],
      });
    }
  });
}

print('Click button below to start export to Drive');
var button = ui.Button({label: 'Export to Drive', onClick: doExportDrive});
print(button);


/// Weekly composite

// Load daily temperature data from ERA5-Land
var dailyTemp = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
  .filterDate(startDate, endDate)
  .select(['temperature_2m'])
  .filterBounds(AOI);

// Function to sum daily temperature over a week and set month as a property
var weeklyComposite = function(imageCollection) {
  var weeks = ee.List.sequence(0, ee.Number(imageCollection.size()).subtract(1), 7);
  return ee.ImageCollection.fromImages(weeks.map(function(week) {
    var start = ee.Date(startDate).advance(week, 'day');
    var end = start.advance(6, 'day');
    var weeklySum = imageCollection.filterDate(start, end).sum();
    var month = start.format('YYYY-MM');
    return weeklySum.set('system:time_start', start.millis()).set('month', month);
  }));
};

// Calculate weekly temperature composite
var weeklyTemp = weeklyComposite(dailyTemp);

// Define a function to export an image or image collection
function exportVariable(variable, variableName, index) {
  // Get the month property from the image
  var month = variable.get('month').getInfo();

  // Define export parameters
  var exportParams = {
    image: variable,
    description: variableName + '_' + index + '_' + month,
    folder: 'Weekly_temp',
    scale: 100, // 1km for temp and precip
    region: AOI,
    maxPixels: 1e13,
    crs: 'EPSG:4326',
    fileFormat: 'GeoTIFF'
  };
  // Export the variable
  Export.image.toDrive(exportParams);
}

// Print button to start export to Drive
print('Click button below to start export to Drive');
var button = ui.Button({label: 'Export to Drive', onClick: doExportDrive});
print(button);

// Function to loop through each image and export it
var doExportDrive = function() {
  var ids = weeklyTemp.aggregate_array('system:index');
  ids.evaluate(function(imageIds) {
    for (var i = 1; i <= imageIds.length; i++) {
      // Filter using the image id
      var image = ee.Image(weeklyTemp.toList(1, i - 1).get(0));

      // Clip image to the geometry
      exportVariable(image.clip(AOI), 'Weekly_temp', i);
    }
    print('Exporting now... (see Tasks tab)');
  });
};

// Start exporting to Drive directly
doExportDrive();

*/


// biweekly 

var dailyTemp = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
  .filterDate(startDate, endDate)
  .select(['temperature_2m'])
  .filterBounds(AOI);

// Function to sum daily temperature over two weeks and set month as a property
var biweeklyComposite = function(imageCollection) {
  var biweeks = ee.List.sequence(0, ee.Number(imageCollection.size()).subtract(1), 14);
  return ee.ImageCollection.fromImages(biweeks.map(function(biweek) {
    var start = ee.Date(startDate).advance(biweek, 'day');
    var end = start.advance(13, 'day');
    var biweeklySum = imageCollection.filterDate(start, end).sum();
    var month = start.format('YYYY-MM');
    return biweeklySum.set('system:time_start', start.millis()).set('month', month);
  }));
};

// Calculate biweekly temperature composite
var biweeklyTemp = biweeklyComposite(dailyTemp);

// Define a function to export an image or image collection
function exportVariable(variable, variableName, index) {
  // Get the month property from the image
  var month = variable.get('month').getInfo();

  // Define export parameters
  var exportParams = {
    image: variable,
    description: variableName + '_' + index + '_' + month,
    folder: 'Biweekly_temp',
    scale: 100, // 1km for temp and precip
    region: AOI,
    maxPixels: 1e13,
    crs: 'EPSG:4326',
    fileFormat: 'GeoTIFF'
  };
  // Export the variable
  Export.image.toDrive(exportParams);
}

// Print button to start export to Drive
print('Click button below to start export to Drive');
var button = ui.Button({label: 'Export to Drive', onClick: doExportDrive});
print(button);

// Function to loop through each image and export it
var doExportDrive = function() {
  var ids = biweeklyTemp.aggregate_array('system:index');
  ids.evaluate(function(imageIds) {
    for (var i = 1; i <= imageIds.length; i++) {
      // Filter using the image id
      var image = ee.Image(biweeklyTemp.toList(1, i - 1).get(0));

      // Clip image to the geometry
      exportVariable(image.clip(AOI), 'Biweekly_temp', i);
    }
    print('Exporting now... (see Tasks tab)');
  });
};

// Start exporting to Drive directly
doExportDrive();