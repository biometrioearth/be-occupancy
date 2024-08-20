// // DEM.
var AOI = ee.FeatureCollection("projects/ee-bioamaserwah/assets/AOI_geojson");
Map.centerObject(AOI, 8);
Map.addLayer(AOI)

/*
=====================================================================================
                PRECIPITATION
======================================================================================
*/

//time period 
var startDate = '2023-10-01';
var endDate = '2024-03-31';

// PRECIPITATION IN THE DRYEST MONTH
var precipitation = ee.ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR") //1km spatial resolution
  .filterDate(startDate, endDate)
  .select(['total_precipitation_sum'])
  .filterBounds(AOI);
var dryestMonth = precipitation.sort('total_precipitation_sum').first().clip(AOI);//dryestmonth
Map.addLayer(dryestMonth, {}, "Driest Month Precipitation");

print(dryestMonth,"dry")
//export
exportVariable(dryestMonth, 'driest_month_precipitation');

/// daily precipitation

var daily_pre = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
  .filterDate(startDate, endDate)
  .select(['total_precipitation_sum'])
  .filterBounds(AOI);
/*
// Print the daily precipitation data to the console
print(daily_pre, "Daily Precipitation");

var dates = [
  '2023-10-11',
  '2023-11-02',
  '2023-12-06',
  '2024-02-26',
  '2024-03-06'
];

// Load and clip the images for the specified dates
var images = dates.map(function(date) {
  var img = ee.Image('ECMWF/ERA5_LAND/DAILY_AGGR/' + date.replace(/-/g, '')).select('total_precipitation_sum').clip(AOI);
  Map.addLayer(img, {}, 'Precipitation on ' + date);
  return img;
});


// Print the images to the console for verification
print(images, "Selected Images");

// export each image
images.forEach(function(img, index) {
  exportVariable(img, 'precipitation_' + dates[index]);
});



  //monthly
var monthlyPrecipitation = ee.ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR") // 1km spatial resolution
  .filterDate(startDate, endDate)
  .select(['total_precipitation_sum'])
  .filterBounds(AOI);

var clippedPrecipitation = monthlyPrecipitation.map(function(image) {
  return image.clip(AOI);
});

print (clippedPrecipitation)

  
// Define a function to export an image or image collection
function exportVariable(variable, variableName) {
  // Define export parameters
  var exportParams = {
    image: variable,
    description: variableName,
    folder: 'PRECIP_DAILY',
    scale: 500,//1km for temp and prep 
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
 //'clippedMonthlyFaparComposites': clippedMonthlyFaparComposites,
  // 'temp': temp,
 // 'fapar':fapar,
 'clippedPrecipitation' : clippedPrecipitation
};



// Loop through each variable and export it
for (var variableName in variablesToExport) {
  exportVariable(variablesToExport[variableName], variableName);
}


//Export parameters only
var doExportDrive = function() {
  print('Working')
  var ids = clippedPrecipitation.aggregate_array('system:index');
  // 
  // Export.image.toDrive()
  ids.evaluate(function(imageIds) {
    print('Total number of images', imageIds.length);
    print('Exporting now... (see Tasks tab)');
    print('Done done');
    for(var i = 0; i < imageIds.length; i++) {
      
      // Filter using the image id
      var image = ee.Image(clippedPrecipitation.toList(1, i).get(0));

      // Clip image to the geometry
      Export.image.toDrive({
        image: image.clip(AOI),
        region: AOI.geometry(),
        scale: 500,// 1km for precip and temp
        fileNamePrefix: 'Monthly_precip_' + imageIds[i+1],
        folder: 'Monthly_precip',
        description: 'Monthly_precip' + i + '_' + imageIds[i],
      })
      }
  })
  
}

print('Click button below to start export to Drive')
var button = ui.Button({label: 'Export to Drive', onClick: doExportDrive})
print(button)
*/


/// weekly composite

// Load daily precipitation data from ERA5-Land
var dailyPre = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
  .filterDate(startDate, endDate)
  .select(['total_precipitation_sum'])
  .filterBounds(AOI);

// Function to sum daily precipitation over a week and set month as a property
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

// Calculate weekly precipitation composite
var weeklyPre = weeklyComposite(dailyPre);

// Define a function to export an image or image collection
function exportVariable(variable, variableName, index) {
  // Get the month property from the image
  var month = variable.get('month').getInfo();

  // Define export parameters
  var exportParams = {
    image: variable,
    description: variableName + '_' + index + '_' + month,
    folder: 'Weekly_precip',
    scale: 500, // 1km for temp and precip
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
  var ids = weeklyPre.aggregate_array('system:index');
  ids.evaluate(function(imageIds) {
    for (var i = 0; i < imageIds.length; i++) {
      // Filter using the image id
      var image = ee.Image(weeklyPre.toList(1, i).get(0));

      // Clip image to the geometry
      exportVariable(image.clip(AOI), 'Weekly_precip', i);
    }
    print('Exporting now... (see Tasks tab)');
  });
};

// Start exporting to Drive directly
doExportDrive();

/*
/// biweekly

// Load daily precipitation data from ERA5-Land
var dailyPre = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
  .filterDate(startDate, endDate)
  .select(['total_precipitation_sum'])
  .filterBounds(AOI);

// Function to sum daily precipitation over two weeks and set month as a property
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

// Calculate biweekly precipitation composite
var biweeklyPre = biweeklyComposite(dailyPre);

// Define a function to export an image or image collection
function exportVariable(variable, variableName, index) {
  // Get the month property from the image
  var month = variable.get('month').getInfo();

  // Define export parameters
  var exportParams = {
    image: variable,
    description: variableName + '_' + index + '_' + month,
    folder: 'Biweekly_precip',
    scale: 500, // 1km for temp and precip
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
  var ids = biweeklyPre.aggregate_array('system:index');
  ids.evaluate(function(imageIds) {
    for (var i = 0; i < imageIds.length; i++) {
      // Filter using the image id
      var image = ee.Image(biweeklyPre.toList(1, i).get(0));

      // Clip image to the geometry
      exportVariable(image.clip(AOI), 'Biweekly_precip', i);
    }
    print('Exporting now... (see Tasks tab)');
  });
};

// Start exporting to Drive directly
doExportDrive();

/*
/// PRECIPITATION FOR  SEASON
var precipitation = ee.ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR") //1km spatial resolution
  .filterDate(startDate, endDate)
  .select(['total_precipitation_sum'])
  .filterBounds(AOI);
  
  //find sum within thia season
var totalPrecipitation = precipitation.sum();
var totalPrecipitationAOI = totalPrecipitation.clip(AOI);
print('Total Precipitation:', totalPrecipitationAOI);
// Export the result to Google Drive
Export.image.toDrive({
  image: totalPrecipitationAOI,
  description: 'Total_Precipitation for dry',
  folder: 'EarthEngineExports',
  fileNamePrefix: 'Total_Precipitation_dryseason',
  region: AOI,
  scale: 1000, // 1 km spatial resolution
  crs: 'EPSG:4326',
  maxPixels: 1e13
});



Export.table.toDrive({
  collection: AOI,
  description: 'AOI_GeoJSON',
  fileFormat: 'GeoJSON'
});
*/