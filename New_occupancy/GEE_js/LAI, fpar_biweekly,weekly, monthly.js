var AOI = ee.FeatureCollection("projects/ee-bioamaserwah/assets/AOI_geojson");
Map.addLayer(AOI);
Map.centerObject(AOI, 10);

var startDate = '2023-10-01';
var endDate = '2024-03-31';

var lai = ee.ImageCollection('MODIS/061/MCD15A3H')
    .filterDate(startDate, endDate)
    .select(['Lai'])
    .filterBounds(AOI);

var fapar = ee.ImageCollection('MODIS/061/MCD15A3H')
    .filterDate(startDate, endDate)
    .select(['Fpar'])
    .filterBounds(AOI);

var npp = ee.ImageCollection('MODIS/061/MOD17A3HGF')
    .filter(ee.Filter.date('2023-01-01', '2023-03-31'))
    .select('Npp')
    .filterBounds(AOI);

print(npp, 'Open');


var clippedNpp = npp.map(function(image) {
    return image.clip(AOI);
});
Map.addLayer(clippedNpp, {}, 'NoPP');
function exportImageCollectionToDrive(collection, folderName) {
    var imageList = collection.toList(collection.size());

    imageList.evaluate(function(list) {
        for (var i = 0; i < list.length; i++) {
            var image = ee.Image(imageList.get(i));
            var id = image.id().getInfo();

            Export.image.toDrive({
                image: image,
                description: 'Clipped_NPP_' + id,
                scale: 500,
                region: AOI,
                fileFormat: 'GeoTIFF',
                folder: folderName,
                crs: 'EPSG:4326',
                maxPixels: 1e13
            });
        }
    });
}

exportImageCollectionToDrive(clippedNpp, 'NPP');


var fstartDate = '2023-10-01';
var fendDate = '2024-03-31';

/*
var calculateMonthlyComposites = function(collection, startDate, endDate) {
    var start = ee.Date(fstartDate);
    var end = ee.Date(fendDate);
    var months = ee.List.sequence(0, end.difference(start, 'months'));

    var monthlyComposites = ee.ImageCollection.fromImages(months.map(function(i) {
        var monthStartDate = start.advance(i, 'months');
        var monthEndDate = monthStartDate.advance(1, 'months');
        var monthlyComposite = collection.filterDate(monthStartDate, monthEndDate).median();
        return monthlyComposite.set('month', monthStartDate.format('MM-yyyy'));
    }));

    return monthlyComposites;
};

var monthlyLAIComposites = calculateMonthlyComposites(lai, startDate, endDate);
var monthlyFaparComposites = calculateMonthlyComposites(fapar, startDate, endDate);

var clippedMonthlyLAIComposites = monthlyLAIComposites.map(function(image) {
    return image.clip(AOI);
});

var clippedMonthlyFaparComposites = monthlyFaparComposites.map(function(image) {
    return image.clip(AOI);
});

print('Clipped monthly LAI composites', clippedMonthlyLAIComposites);
print('Clipped monthly fAPAR composites', clippedMonthlyFaparComposites);

// Define a function to export an image or image collection with specific file name and folder
function exportVariable(variable, variableName, folderName) {
  var ids = variable.aggregate_array('system:index');

  ids.evaluate(function(imageIds) {
    print('Total number of images', imageIds.length);
    print('Exporting now... (see Tasks tab)');
    for (var i = 0; i < imageIds.length; i++) {
      // Filter using the image id
      var image = ee.Image(variable.toList(1, i).get(0));
      var month = image.get('month').getInfo();

      // Clip image to the geometry and export with appropriate file name and folder
      Export.image.toDrive({
        image: image.clip(AOI),
        region: AOI.geometry(),
        scale: 500,
        fileNamePrefix: variableName + '_' + month,
        folder: folderName,
        description: variableName + '_' + month,
        fileFormat: 'GeoTIFF',
        crs: 'EPSG:4326',
        maxPixels: 1e13
      });
    }
  });
}

print('Click button below to start export to Drive');
var button = ui.Button({label: 'Export LAI and FAPAR to Drive', onClick: function() {
  exportVariable(clippedMonthlyLAIComposites, 'lai', 'Monthly_LAI_Folder');
  exportVariable(clippedMonthlyFaparComposites, 'fapar', 'Monthly_FAPAR_Folder');
}});
print(button);


/*
=========================================================================================
                    WEEKLY COMPOSITES 
=========================================================================================
*/

/*
var calculateWeeklyComposites = function(collection, startDate, endDate) {
    var start = ee.Date(fstartDate);
    var end = ee.Date(fendDate);
    var weeks = ee.List.sequence(0, end.difference(start, 'week'));

    var weeklyComposites = ee.ImageCollection.fromImages(weeks.map(function(i) {
        var weekStartDate = start.advance(i, 'week');
        var weekEndDate = weekStartDate.advance(1, 'week');
        var weeklyComposite = collection.filterDate(weekStartDate, weekEndDate).median();
        return weeklyComposite.set('week', weekStartDate.format('YYYY-ww'));
    }));

    return weeklyComposites;
};

var weeklyLAIComposites = calculateWeeklyComposites(lai, startDate, endDate);
var weeklyFaparComposites = calculateWeeklyComposites(fapar, startDate, endDate);

var clippedWeeklyLAIComposites = weeklyLAIComposites.map(function(image) {
    return image.clip(AOI);
});

var clippedWeeklyFaparComposites = weeklyFaparComposites.map(function(image) {
    return image.clip(AOI);
});

print('Clipped weekly LAI composites', clippedWeeklyLAIComposites);
print('Clipped weekly fAPAR composites', clippedWeeklyFaparComposites);

// Define a function to export an image or image collection with specific file name and folder
function exportVariable(variable, variableName, folderName) {
  var ids = variable.aggregate_array('system:index');

  ids.evaluate(function(imageIds) {
    print('Total number of images', imageIds.length);
    print('Exporting now... (see Tasks tab)');
    for (var i = 0; i < imageIds.length; i++) {
      // Filter using the image id
      var image = ee.Image(variable.toList(1, i).get(0));
      var week = image.get('week').getInfo();

      // Clip image to the geometry and export with appropriate file name and folder
      Export.image.toDrive({
        image: image.clip(AOI),
        region: AOI.geometry(),
        scale: 500,
        fileNamePrefix: 'Weekly_'+ variableName + '_' + (i + 1) ,
        folder: folderName,
        description:  variableName + '_' + week + '_' + (i + 1),
        fileFormat: 'GeoTIFF',
        crs: 'EPSG:4326',
        maxPixels: 1e13
      });
    }
  });
}

print('Click button below to start export to Drive');
var button = ui.Button({label: 'Export LAI and FAPAR to Drive', onClick: function() {
  exportVariable(clippedWeeklyLAIComposites, 'lai', 'LAI_Folder');
  exportVariable(clippedWeeklyFaparComposites, 'fapar', 'FAPAR_Folder');
}});
print(button);
*/

/*
===============================================================================================
             biweekly 
===============================================================================================
*/



var calculateBiweeklyComposites = function(collection, startDate, endDate) {
    var start = ee.Date(startDate);
    var end = ee.Date(endDate);
    var weeks = ee.List.sequence(0, end.difference(start, 'weeks').subtract(1), 2);

    var biweeklyComposites = ee.ImageCollection.fromImages(weeks.map(function(i) {
        var weekStartDate = start.advance(i, 'weeks');
        var weekEndDate = weekStartDate.advance(2, 'weeks');
        var biweeklyComposite = collection.filterDate(weekStartDate, weekEndDate).median();
        return biweeklyComposite.set('period', weekStartDate.format('MM-dd-yyyy') + '_' + weekEndDate.format('MM-dd-yyyy'));
    }));

    return biweeklyComposites;
};

var biweeklyLAIComposites = calculateBiweeklyComposites(lai, startDate, endDate);
var biweeklyFaparComposites = calculateBiweeklyComposites(fapar, startDate, endDate);

var clippedBiweeklyLAIComposites = biweeklyLAIComposites.map(function(image) {
    return image.clip(AOI);
});

var clippedBiweeklyFaparComposites = biweeklyFaparComposites.map(function(image) {
    return image.clip(AOI);
});

print('Clipped biweekly LAI composites', clippedBiweeklyLAIComposites);
print('Clipped biweekly fAPAR composites', clippedBiweeklyFaparComposites);

function exportVariable(variable, variableName, folderName) {
    var ids = variable.aggregate_array('system:index');

    ids.evaluate(function(imageIds) {
        print('Total number of images', imageIds.length);
        print('Exporting now... (see Tasks tab)');
        for (var i = 0; i < imageIds.length; i++) {
            // Filter using the image id
            var image = ee.Image(variable.toList(1, i).get(0));
            var period = image.get('period').getInfo();

            // Clip image to the geometry and export with appropriate file name and folder
            Export.image.toDrive({
                image: image.clip(AOI),
                region: AOI.geometry(),
                scale: 500,
                fileNamePrefix: 'Biweekly_'+ variableName + '_' + (i + 1),
                folder: folderName,
                description: 'Biweekly_' + variableName + '_' + (i + 1),
                fileFormat: 'GeoTIFF',
                crs: 'EPSG:4326',
                maxPixels: 1e13
            });
        }
    });
}

print('Click button below to start export to Drive');
var button = ui.Button({label: 'Export LAI and FAPAR to Drive', onClick: function() {
    exportVariable(clippedBiweeklyLAIComposites, 'lai', 'Biweekly_LAI_Folder');
    exportVariable(clippedBiweeklyFaparComposites, 'fapar', 'Biweekly_FAPAR_Folder');
    
}});
print(button);



/*
=============================================================================================================
              daily
=====================================================================================================
*/
/*
print(lai, 'LAI Collection');
print(fapar, 'fAPAR Collection');

// Specify the dates for which you want to export images
var dates = [
  '2023-10-12',
  '2023-11-01',
  '2023-12-07',
  '2024-02-26',
  '2024-03-05'
];

// Function to export images
function exportImage(image, variableName, date) {
  Export.image.toDrive({
    image: image,
    description: variableName + '_' + date,
    scale: 500,
    region: AOI,
    fileFormat: 'GeoTIFF',
    folder: variableName.toUpperCase(),
    crs: 'EPSG:4326',
    maxPixels: 1e13
  });
}

// Load and clip the images for the specified dates
dates.forEach(function(date) {
  var laiImage = lai.filter(ee.Filter.date(date)).first().clip(AOI);
  var faparImage = fapar.filter(ee.Filter.date(date)).first().clip(AOI);

  // Add layers to the map for visualization (optional)
  Map.addLayer(laiImage, {}, 'LAI on ' + date);
  Map.addLayer(faparImage, {}, 'fAPAR on ' + date);

  // Export the images
  exportImage(laiImage, 'lai', date.replace(/-/g, ''));
  exportImage(faparImage, 'fapar', date.replace(/-/g, ''));
});


*/