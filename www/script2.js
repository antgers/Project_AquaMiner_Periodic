window.addEventListener('load', function(){
  
  var showHideButtonForMenu = document.getElementById("showHideButtonForMenu");
showHideButtonForMenu.addEventListener('click' , function(){
  
   var btnFilter = document.getElementById("showHideButtonForMenu");
  var parent = btnFilter.nextElementSibling;
  var sidebar = parent.getElementsByClassName("col-sm-4");

  var content = null;
  if(sidebar[0].style.display == "none"){
     sidebar[0].style.display = "";
      content = parent.getElementsByClassName("col-sm-12");
      content[0].className ="col-sm-8";
      btnFilter.className =" action-button shiny-bound-input close-filters";
  }
   
    else{
       content = parent.getElementsByClassName("col-sm-8");
        sidebar[0].style.display= "none";
         content[0].className ="col-sm-12";
          btnFilter.className =" action-button shiny-bound-input open-filters";
    }

  
});

var showHideButtonForMachine = document.getElementById("showHideButtonForMachine");
showHideButtonForMachine.addEventListener('click' , function(){
  var btnFilter = document.getElementById("showHideButtonForMachine");
  var parent = btnFilter.nextElementSibling;
  var sidebar = parent.getElementsByClassName("col-sm-4");
 
  var content = null;
  if(sidebar[0].style.display == "none"){
     sidebar[0].style.display = "";
      content = parent.getElementsByClassName("col-sm-12");
      content[0].className ="col-sm-8";
      btnFilter.className =" action-button shiny-bound-input close-filters";
  }
   
    else{
       content = parent.getElementsByClassName("col-sm-8")
        sidebar[0].style.display= "none";
         content[0].className ="col-sm-12";
         btnFilter.className =" action-button shiny-bound-input open-filters";
    }

  
});

}, false )