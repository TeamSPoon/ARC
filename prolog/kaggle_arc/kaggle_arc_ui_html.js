function clickGrid(thiz) {
    console.log("clickGrid(" + thiz + ")");
}

function clickAccordian(thizn) {
    var thiz = document.getElementById(thizn);
    console.log("clickAccordian(" + thiz + ")");
    thiz.classList.toggle("active");
    var panel = thiz.nextElementSibling;
    panel.classList.toggle("panel_hidden");
    panel.classList.toggle("panel_shown");
    if (panel.style.display === "block") {
        panel.style.display = "none";
    } else {
        panel.style.display = "block";
    }
}

var TwoFiftyPx = "250px";

function toggleNavL(Name) {
    if (document.getElementById(Name).style.width != "0px") {
        document.getElementById(Name).style.width = "0px";
        document.getElementById("main").style.marginLeft = "0px";
    } else {
        document.getElementById(Name).style.width = TwoFiftyPx;
        document.getElementById("main").style.marginLeft = TwoFiftyPx;
    }

    var iframeE = document.getElementById('lm_xref');
    iframeE.style.width = document.getElementById("main").style.width;
    /*
    iframeE.style.width= screen.width - document.getElementById("mySideNavL").style.width
               - document.getElementById("mySideNavR").style.width;
     */

}

function toggleNavR(Name) {
    if (document.getElementById(Name).style.width != "0px") {
        document.getElementById(Name).style.width = "0px";
        document.getElementById("main").style.marginRight = "0px";
    } else {
        document.getElementById(Name).style.width = TwoFiftyPx;
        document.getElementById("main").style.marginRight = TwoFiftyPx;
    }

    var iframeE = document.getElementById('lm_xref');
    iframeE.style.width = document.getElementById("main").style.width;
    /*
    iframeE.style.width= screen.width - document.getElementById("mySideNavL").style.width
               - document.getElementById("mySideNavR").style.width;
     */
}



/* window.addEventListener("click", (e) => {
   e = e || window.event;
   console.log(e.target);  // to get the element
   console.log(e.target.tagName);  // to get the element tag name alone
   console.log(e.target.id);  // to get the element tag name alone
   var target = e.target|| e.srcElement;
   while(target.id=="") {
     if (target.onclick != undefined) {
       target.click();
     }
     if(target.classList.contains("accordian")) {
       clickAccordian(target);
     }
     target = target.parentElement;
   }
   console.log(target);  // to get the element
   console.log(target.tagName);  // to get the element tag name alone
   if (target.tagName!="TABLE") {
    e.stopPropagation();
   }
   console.log(target.id);  // to get the element tag name alone
 });   */