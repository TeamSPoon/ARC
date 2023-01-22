function clickGrid(thiz) {
	console.log("clickGrid(" + thiz + ")");
	navToParams({grid: thiz});
}
function navCmd(thiz) {
	console.log("navCmd(" + thiz + ")");
	navToParams({cmd: thiz});
}

function getNavWindow() {
	var iframe = document.getElementById("lm_xref");
	if (iframe!=null) {
		var cw = iframe.contentWindow;
		if (cw!=null) {
			return cw;
		}
	}
	return window;
}

function navToParams(thiz) { 
	var windoe = getNavWindow();
	let url = new URL(windoe.location.href);
	url.search = new URLSearchParams(thiz);
	console.log("windoe="+windoe.location);
	console.log("url="+url);
	console.log("url.ref="+url.href);
	debugger;
	windoe.location.assign(url);
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