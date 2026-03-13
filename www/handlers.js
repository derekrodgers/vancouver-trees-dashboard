// Street View handlers
Shiny.addCustomMessageHandler("updateStreetView", function(message) {
  var location = new google.maps.LatLng(message.lat, message.lon);
  var sv = new google.maps.StreetViewService();
  sv.getPanorama({location: location, radius: 50}, function(data, status) {
    if (status === google.maps.StreetViewStatus.OK) {
      var panoLatLng = data.location.latLng;
      var computedHeading = google.maps.geometry.spherical.computeHeading(panoLatLng, location);
      if (!window.streetPanorama) {
        window.streetPanorama = new google.maps.StreetViewPanorama(
          document.getElementById("street_view_container"),
          {
            position: location,
            pov: {heading: computedHeading, pitch: 0},
            visible: true
          }
        );
      } else {
        window.streetPanorama.setPosition(location);
        window.streetPanorama.setPov({heading: computedHeading, pitch: 0});
        window.streetPanorama.setVisible(true);
      }
    } else {
      console.log("No panorama found for this location.");
    }
  });
});

Shiny.addCustomMessageHandler("clearStreetView", function(message) {
  // If a Street View panorama exists, hide it and remove references.
  if (window.streetPanorama) {
    window.streetPanorama.setVisible(false);
    window.streetPanorama = null;
  }

  // Get the container for Street View.
  var container = document.getElementById("street_view_container");

  // Remove all existing child elements.
  while (container.firstChild) {
    container.removeChild(container.firstChild);
  }

  // Reset the container's background to white.
  container.style.backgroundColor = "white";

  // Insert the placeholder message.
  container.innerHTML = "<div style='font-size:16px; padding:10px;'>Select a single tree.</div>";
});

// Map popup / zoom handlers
Shiny.addCustomMessageHandler("openPopupAfterZoom", function(message) {
  var map = window.treeMap;
  if (!map) return;
  // Capture current view if not already saved
  if (!window.prevView) {
    window.prevView = {
      center: map.getCenter(),
      zoom: map.getZoom()
    };
  }
  map.once("zoomend", function() {
    var markerFound = null;
    map.eachLayer(function(layer) {
      if (layer.options && layer.options.layerId == message.id) {
        markerFound = layer;
      }
    });
    if (markerFound) {
      markerFound.bindPopup(message.content).openPopup();
      // Attach listener on this marker for popup close
      markerFound.on("popupclose", function(e) {
        Shiny.setInputValue("popup_closed", new Date().getTime(), {priority: "event"});
      });
    }
  });
});

Shiny.addCustomMessageHandler("saveCurrentMapView", function(message) {
  var map = window.treeMap;
  if (map) {
    if (!window.prevView) {
      window.prevView = {
        center: map.getCenter(),
        zoom: map.getZoom()
      };
    }
  }
});

Shiny.addCustomMessageHandler("restorePrevMapView", function(message) {
  var map = window.treeMap;
  if (map && window.prevView) {
    map.setView(window.prevView.center, window.prevView.zoom);
    window.prevView = null;
  }
});
