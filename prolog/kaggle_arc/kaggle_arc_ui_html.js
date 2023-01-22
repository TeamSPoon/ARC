function clickGrid(thiz) {
	console.log("clickGrid(" + thiz + ")");
	navTo("?grid="+thiz);
}

function navTo(thiz) {
	var thisUrl = window.location.href;
	if ( thisUrl.indexOf("html.html")>-1 ){
		window.location.href = window.location.pathname + thiz;
	} else{
		var iframe = document.getElementById("lm_xref");
		iframe.contentWindow.location.href = iframe.contentWindow.location.pathname + thiz;
	}
}

function clickAccordian(thizn) {
	var thiz = document.getElementById(thizn);
	console.log("clickAccordian(" + thiz + ")");
	thiz.classList.toggle("active");
	var panel = thiz.nextElementSibling;
	panel.classList.toggle("panel_hidden");
	panel.classList.toggle("panel_shown");
	if ( panel.style.display === "block" ) {
		panel.style.display = "none";
	} else {
		panel.style.display = "block";
	}
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