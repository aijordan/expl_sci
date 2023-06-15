mapHeight = function() {
  if ($("#gameMap").width() < 450) {
    $("#gameMap").css("height", "2000px");
  } else {
    $("#gameMap").height("2000px");
  }
};

window.onload = function() {
  mapHeight();
};
  
window.onresize = function() {
  mapHeight();
};