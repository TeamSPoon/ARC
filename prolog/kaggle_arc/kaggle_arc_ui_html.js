function clickGrid(thiz) {
	console.log("clickGrid(" + thiz + ")");
	var val = getComponent("grid", "");
	if ( val != thiz ) {
		navToParams({ grid: thiz, cmd: "click_grid"});
	}
}

function getSomeElementById(Name) {
	var nameE = getSomeElementById(Name, window);
	if ( nameE!=null ) return nameE;
	var P2 = window.parent;
	if ( P2!=null && P2!=this ) {
		nameE = getSomeElementById(Name, P2);
		if ( nameE!=null ) return nameE;
	}
	var P3 = window.top;
	if ( P3!=null && P3!=this && P3!=P2 ) {
		nameE = getSomeElementById(Name, P3);
		if ( nameE!=null ) return nameE;
	}
	if ( Name!="lm_xref" ) {
		var iframeP = getSomeElementById("lm_xref");
		if ( iframeP != null ) {
			var P4 = iframe.contentWindow;
			if ( P4!=null && P4!=this && P4!=P2 ) {
				nameE = getSomeElementById(Name, P4);
				if ( nameE!=null ) return nameE;
			}
		}
	} else {
		var iframeP = document.getElementById("lm_xref");
		if ( iframeP != null ) {
			var P4 = iframe.contentWindow;
			if ( P4!=null && P4!=this && P4!=P2 ) {
				nameE = getSomeElementById(Name, P4);
				if ( nameE!=null ) return nameE;
			}
		}
	}
	return null;
}


function getSomeElementById(Name, P) {
	if ( P==null ) {
		return null;
	}
	nameE = P.document.getElementById(Name);
	if ( nameE!=null ) return nameE;
	var iframeP = P.document.getElementById("lm_xref");
	if ( iframeP != null ) {
		var P2 = iframe.contentWindow;
		if ( P2!=null && P2!=P ) {
			nameE = P2.document.getElementById(Name);
			if ( nameE!=null ) return nameE;
		}
	}
}


var TwoFiftyPx = "250px";

function toggleNavL(Name) {
	var mainE = top.document.getElementById("main");
	var nameE = top.document.getElementById(Name);
	if ( nameE.style.width != "0px" ) {
		nameE.style.width = "0px";
		mainE.style.marginLeft = "0px";
		mainE.style.width = "200vh";
	} else {
		nameE.style.width = TwoFiftyPx;
		mainE.style.marginLeft = TwoFiftyPx;
	}
	var iframeE = top.document.getElementById('lm_xref');
	if ( iframeE!=null ) {
		iframeE.style.width = mainE.style.width;
		iframeE.style.marginRight = mainE.style.marginRight; 
	}
}


function toggleNavR(Name) {
	var mainE = top.document.getElementById("main");
	var nameE = top.document.getElementById(Name);
	if ( nameE.style.width != "0px" ) {
		nameE.style.width = "0px";
		mainE.style.marginRight = "0px";
		mainE.style.width = "200vh";
	} else {
		nameE.style.width = TwoFiftyPx;
		mainE.style.marginRight = TwoFiftyPx;
	}
	var iframeE = top.document.getElementById('lm_xref');
	if ( iframeE!=null ) {
		iframeE.style.width = mainE.style.width;
		iframeE.style.marginRight = mainE.style.marginRight; 
	}
}

function navCmd(thiz) {
	console.log("navCmd(" + thiz + ")");
	navToParams({ cmd: thiz});

	return false;
}

function getNavWindow() {
	var iframe = document.getElementById("lm_xref");
	if ( iframe == null ) {
		iframe = window.parent.document.getElementById("lm_xref");
	}
	if ( iframe == null ) {
		iframe = top.document.getElementById("lm_xref");
	}
	if ( iframe != null ) {
		var cw = iframe.contentWindow;
		if ( cw != null ) {
			return cw;
		}
	}
	return window;
}

function getTopWindow() {
   //	var nav = getNavWindow();
   // if ( nav != null ) return nav.parent;
	return window.top; //parent;
}

function setFormFields(srch) {
	var newSrch = new URLSearchParams(srch);
	for ( const [key, value] of newSrch.entries() ) {
		console.log(`newFF '${key}:' '${value}'`);
		setComponent(key, value);
		//  debugger;
	}
}

if ( window.var_edits === undefined ) {
	window.var_edits = 0;
}

function setMainQueryParams(srch) {
	//return;
	window.var_edits = 0;
	var topw = getTopWindow();
	var oldUrl = topw.location.href;
	if ( !oldUrl.endsWith(srch) ) {
		setFormFields(srch);
		if ( window.var_edits == 0 ) {
			let url = new URL(oldUrl);
			url.search = srch;
			topw.history.pushState({}, null, url.href);
		}
	}
	var iframe = topw.document.getElementById("lm_xref");
	if ( iframe != null ) {
		var oldUrl2 = iframe.contentWindow.location.href;
		if ( !oldUrl2.endsWith(srch) ) {
			let url2 = new URL(oldUrl2);
			url2.search = srch;
			iframe.setAttribute("href", url2.href);
			// debugger;
		}
	}
}

function isElement(element) {
	if ( typeof element !== 'object' ) {
		return false
	}
	return element instanceof Element || element instanceof HTMLDocument || element instanceof Node;
}

function setUrlParam(name, value) {
	console.log("setUrlParam(" + name + "," + value + ")");
	var topw = getTopWindow();
	let srch = new URLSearchParams(topw.location.search);
	if ( !srch.has(name) || srch.get(name) != value ) {
		srch.set(name, value);
		url = new URL(topw.location.href);
		url.search = srch;
		window.var_edits++;
		topw.history.pushState({}, null, url.href);
	}
}

function setComponent(name, value) {
	if ( typeof name === "string" ) {
		setUrlParam(name, value);
		var nav = getNavWindow();
		var ele = nav.document.getElementById("param_" + name);
		if ( ele == null ) ele = nav.document.getElementById(name);
		var retval = false;
		if ( ele != null ) retval = setComponent(ele, value);
		var topw = getTopWindow();
		var tele = topw.document.getElementById("param_" + name);
		if ( tele == null )	tele = topw.document.getElementById(name);
		if ( tele != null )
			if ( setComponent(tele, value) ) return true;
		return retval;
	}
	var bool_value = true;
	var svalue = ("" + value).trim().toLowerCase;
	if ( svalue == "false" || svalue == "no" || svalue == "off" || svalue == "unchecked" || value == "0" || value == "" || value == false || value == null || value == undefined || value == 0 ) {
		bool_value = false;
	}
	if ( isElement(name) ) {
		console.log("setComponent(" + name.id + "," + value + ")");
		if ( name.getAttribute('type') == 'checkbox' ) {
			name.checked = bool_value;
			return true;
		} else
			if ( name.tagName.toLowerCase == 'input' ) {
			name.value = value;
			return true;
		} else
			if ( name.tagName.toLowerCase == 'textarea' ) {
			name.value = value;
			return true;
		} else {
			if ( value.indexOf("<") == -1 ) {
				name.innerText = value;
				name.textContent = value;
			} else {
				name.innerHtml = value;
			}
		}
	}
	return false;
}

function getComponent(name, elsev) {
	if ( typeof name === "string" ) {
		var topw = getTopWindow();
		var srch = new URLSearchParams(topw.location.search);
		debugger;
		if ( srch.has(name) ) {
			return srch.get(name);
		}
		var nav = getNavWindow();
		var ele = nav.document.getElementById("param_" + name);
		if ( ele == null ) ele = nav.document.getElementById(name);
		if ( ele != null ) return getComponent(ele, value);
		var tele = topw.document.getElementById("param_" + name);
		if ( tele == null )	tele = topw.document.getElementById(name);
		if ( tele != null )	return getComponent(tele, value);
		return elsev;
	}
	if ( isElement(name) ) {
		if ( name.getAttribute('type') == 'checkbox' ) {
			return name.checked;
		} else
			if ( name.tagName.toLowerCase == 'input' ) {
			return name.value;
		} else
			if ( name.tagName.toLowerCase == 'textarea' ) {
			return name.value;
		}
		var it = name.innerText;
		if ( it != null ) it = it.trim();
		if ( it == null || it == "" ) it = name.textContent;
		if ( it != null ) it = it.trim();
		if ( it == null || it == "" ) it = name.innerHtml;
		if ( it != null ) it = it.trim();
		if ( it != null ) return it;
	}
	return elsev;
}

function getFormDicts() {
	var nav = getNavWindow();
	var topw = getTopWindow();

	var newSrch = new URLSearchParams({});

	for ( const ele of nav.document.querySelectorAll('input[type="checkbox"]') ) {
		var key = ele.id;
		if ( key == "" || key.startsWith("accord") ) continue;
		if ( key.indexOf("param_") == 0 ) {
			if ( ele.name )	key = ele.name;
			else {
				key = key.substring(6);
			}
		}
		var value = ele.checked;
		if ( !newSrch.has(key) ) {
			newSrch.set(key, value);
		}
	}
	for ( const ele of topw.document.querySelectorAll('input[type="checkbox"]') ) {
		var key = ele.id;
		if ( key == "" || key.startsWith("accord") ) continue;
		if ( key.indexOf("param_") == 0 ) {
			if ( ele.name )	key = ele.name;
			else {
				key = key.substring(6);
			}
		}
		var value = ele.checked;
		if ( !newSrch.has(key) ) {
			newSrch.set(key, value);
		}
	}

	for ( const ele of nav.document.querySelectorAll('input[type]:not([type="checkbox"])') ) {
		var key = ele.id;
		if ( key == "" || key.startsWith("accord") ) continue;
		if ( key.indexOf("param_") == 0 ) {
			if ( ele.name )	key = ele.name;
			else {
				key = key.substring(6);
			}
		}
		var value = ele.checked;
		if ( !newSrch.has(key) ) {
			newSrch.set(key, value);
		}
	}
	for ( const ele of topw.document.querySelectorAll('input[type]:not([type="checkbox"])') ) {
		var key = ele.id;
		if ( key == "" || key.startsWith("accord") ) continue;
		if ( key.indexOf("param_") == 0 ) {
			if ( ele.name )	key = ele.name;
			else {
				key = key.substring(6);
			}
		}
		var value = ele.value;
		if ( !newSrch.has(key) ) {
			newSrch.set(key, value);
		}
	}
	return newSrch;
}

function navToParams(jsonDict) {
	var newSrch = new URLSearchParams(jsonDict);
	var nav = getNavWindow();
	var topw = getTopWindow();
	var ooldSrch = new URLSearchParams(nav.location.search);
	ooldSrch.delete("");
	var topSrch = new URLSearchParams(topw.location.search);
	topSrch.delete("");
	console.log("nav=" + ooldSrch);
	console.log("topw=" + topSrch);
	console.log("newSrch=" + newSrch);

	var oldSrch = new URLSearchParams(nav.location.search);
	oldSrch.delete("");
	// Display the key/value pairs
	for ( const [key, value] of oldSrch.entries() ) {
		if ( key == "" || key.startsWith("accord") ) continue;
		console.log(`old ${key}: ${value}`);
	}
	oldSrch.delete("cmd");
	oldSrch.delete("grid");
	for ( const [key, value] of oldSrch.entries() ) {
		if ( key == "" || key.startsWith("accord") ) continue;
		if ( !newSrch.has(key) ) {
			newSrch.set(key, value);
		}
	}
	var formdicts = getFormDicts();
	for ( const [key, value] of formdicts.entries() ) {
		if ( key == "" || key.startsWith("accord") ) continue;
		if ( !newSrch.has(key) ) {
			newSrch.set(key, value);
		} else {
			setComponent(key, value);
		}
	}
	newSrch.delete("");
	for ( const [key, value] of newSrch.entries() ) {
		console.log(`new '${key}:' '${value}'`);
	}
	var newSrchStr = "?" + newSrch;
	var nsrc = topw.location.origin + "/arcproc_main" + newSrchStr;
	var tsrc = topw.location.origin + topw.location.pathname + newSrchStr;
	topw.history.pushState({}, null, tsrc);
	var iframe = document.getElementById("lm_xref");
	if ( iframe == null ) {
		iframe = window.parent.document.getElementById("lm_xref");
	}
	if ( iframe.src == "about:blank" || !sameSrch(ooldSrch, newSrch) ) {

		// setMainQueryParams(newSrchStr);
		//	debugger;
		//iframe.style.width = document.getElementById("main").style.width;
		if ( iframe.src == "about:blank" ) {
			iframe.setAttribute("src", nsrc);
		} else {
			nav.location.assign(nsrc);
		}
	}
}

function sameSrch(oldSrch, newSrch) {
	for ( const [key, value] of newSrch.entries() ) {
		if ( oldSrch.get(key) != newSrch.get(key) )	return false;
	}
	for ( const [key, value] of oldSrch.entries() ) {
		if ( oldSrch.get(key) != newSrch.get(key) )	return false;
	}
	return true;
}

function clickAccordian(thizn) {
	var thiz = document.getElementById(thizn);
	console.log("clickAccordian(" + thiz + ")");
	thiz.classList.toggle("active");
	var panel = document.getElementById(thizn+"_panel");
	if ( panel==null ) panel = thiz.nextElementSibling;
	togglePanel(panel,false);
}

function togglePanel(name,scrollTo) {
	var panel = intoPanel(name);
	if(panel==null) return;
	if ( panel.style.display === "none" ) {
		showPanel(panel,scrollTo);
	} else {
		hidePanel(panel);
	}
}


{
	window.lastPanelShown = null;
}

function intoPanel(name) {
	if(name==null) return null;
	var panel = name;
	if ( typeof name === "string" ) {
		var nav = getNavWindow();
		panel = nav.document.getElementById(name);
		console.warn(panel + "not a panel: " + panel.classList);
		if(panel==null) {
			console.warn(name + " not found in " + nav);
            return null;
		}
	}	
    if(!panel.classList.contains("panel")) {
		console.warn(panel + "not a panel: " + panel.classList);
		return null;
	}
	return panel;
}

function showPanel(name,scrollTo) {
	var panel = intoPanel(name);
	if(panel==null) return;

	if(scrollTo) {
		// gets us the show hide toggles
		if(window.lastPanelShown == panel) {
			if(panel.classList.contains("panel_shown")) {
			   hidePanel(panel);
			   window.lastPanelShown = null;
			   return;
			}
		} 
	}
	//panel.scrollIntoView(false);

	var PE = panel.parentElement;
	if(PE!=null) {
		if(!(PE.classList.contains("panel"))) {
             var GPE = PE.parentElement;
			 if(GPE!=null) {
				 if(GPE.classList.contains("panel")) {
					 PE = GPE;
				 }
			 }
		}
		if(PE.classList.contains("panel_hidden")) {
			var sibling = PE.firstElementChild;
			do {
				if (sibling != panel) {
					if(sibling.classList.contains("panel_shown")) {
						hidePanel(sibling);
					}
				}
			} while (sibling = sibling.nextElementSibling);
			showPanel(PE,false);
		}
	}
	panel.classList.add("panel_shown");
	panel.classList.remove("panel_hidden");
	panel.style.display = "block";
	panel.style.maxHeight = "100%";
	panel.style.minHeight = "10px";
	if(scrollTo) {
	  var scrl= panel.previousElementSibling;
	  if(scrl==null)scrl=panel;
	  scrl.scrollIntoView(true);
	  window.lastPanelShown = panel;
	}
	
}

function hidePanel(name) {
	var panel = intoPanel(name);
	if(panel==null) return;
	panel.classList.add("panel_hidden");
	panel.classList.remove("panel_shown");
	panel.style.display = "none";
	panel.style.maxHeight = "0px";
	panel.style.minHeight = "0px";  
}

function addAccordian(thizn,depth) {

	console.log("addAccordian(" + thizn.id  +  "," + thizn.innerText + "," + depth + ")");
	var nav = top.document.getElementById("navbar_items");
	var br=top.document.createElement('p');
	nav.appendChild(br);
	var e=top.document.createElement('li');
	e.classList.add("nav-item");
	var  spaces = "";
	for ( i=0;i<depth;i++ ) {
		spaces=spaces+"&nbsp;";
	}
	e.innerHTML ='<a class="nav-link" href="javascript:void(0)" id="'+ thizn.id+'_link" target="lm_xref" onclick="togglePanel(`'+ thizn.id+'_panel`,true);" >'+spaces+thizn.textContent+'</a>';
	nav.appendChild(e);
	e.scrollIntoView(false);
	// thizn.scrollIntoView(false);
	
}

function clearMenu() {
	var e = top.document.getElementById("navbar_items");
	var child = e.lastElementChild; 
	while (child) {
		e.removeChild(child);
		child = e.lastElementChild;
	}
}

function activateMenu(thizn) {
	var elem = top.document.getElementById(thizn);
	if(elem==null) return;
	if(elem.classList.contains("active")) {
	   // return;
	}
    var sibling = elem.parentNode.firstElementChild;
    do {
        if (sibling != elem) {
			sibling.classList.remove("active");
        }
    } while (sibling = sibling.nextElementSibling);
    elem.classList.add("active");
	elem.scrollIntoView(false);	
}

function htmlToPNG(name) {
	var node = document.getElementById(name);
	
	htmlToImage.toPng(node)
	  .then(function (dataUrl) {
		var img = new Image();
		img.src = dataUrl;
		node.appendChild(img);
	  })
	  .catch(function (error) {
		console.error('oops, something went wrong!', error);
	  });
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

