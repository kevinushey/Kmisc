$(document).ready( function() {
  
  // if we click on an image in the document, we should fancyboxify its larger
  // friend
  $("img.fancybox").click( function() {
    
    var $this = $(this);
    
    // blur everything else
    $("body > *").not($this).addClass("blur");
    
    // get id of brother image
    var id = $this.attr("id") + "-large";
    
    // get the actual width, height attributes of the image
    var img_width = $("#" + id)[0].width;
    var img_height = $("#" + id)[0].height;
    var padding = 20;
    
    // get the browser dimensions
    var width = $(window).width();
    var height = $(window).height();
    
    var top = Math.abs( (height - img_height) / 2 );
    var left = Math.abs( (width - img_width) / 2 );
    
    // make it large and in charge
    $("#" + id).parent()
      .css("display", "block")
      .css("margin", "0 auto")
      .css("top", (top - 2*padding) + "px")
      .css("left", (left - 2*padding) + "px")
      .css("width", (img_width + 50) + "px")
      .css("position", "fixed")
      .css("padding", padding + "px")
      .css("background-color", "#444")
      .css("border-radius", padding + "px")
      .css("box-shadow", "0 0 10px #333")
    ;
    
  });
  
  // if we click on a fancyboxed image, we should hide it again
  $("img.fancybox-large").click( function() {
    $("*").removeClass("blur");
    $(this).parent().css("display", "none");
  });
  
});
