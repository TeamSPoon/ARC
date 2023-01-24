function clickGrid(thiz) {
    console.log("clickGrid(" + thiz + ")");
    var val = getComponent("grid", "");
    if (val != thiz) {
        navToParams({ grid: thiz, cmd: "click_grid" });
    }
}

function navCmd(thiz) {
    console.log("navCmd(" + thiz + ")");
    navToParams({ cmd: thiz });

    return false;
}

function getNavWindow() {
    var iframe = document.getElementById("lm_xref");
    if (iframe == null) {
        iframe = window.parent.document.getElementById("lm_xref");
    }
    if (iframe != null) {
        var cw = iframe.contentWindow;
        if (cw != null) {
            return cw;
        }
    }
    return window;
}

function getTopWindow() {
    var nav = getNavWindow();
    if (nav != null) return nav.parent;
    return window.parent;
}

function setFormFields(srch) {
    var newSrch = new URLSearchParams(srch);
    for (const [key, value] of newSrch.entries()) {
        console.log(`newFF '${key}:' '${value}'`);
        setComponent(key, value);
        //  debugger;
    }
}

if (window.var_edits === undefined) {
    window.var_edits = 0;
}

function setMainQueryParams(srch) {
    //return;
    window.var_edits = 0;
    var topw = getTopWindow();
    var oldUrl = topw.location.href;
    if (!oldUrl.endsWith(srch)) {
        setFormFields(srch);
        if (window.var_edits == 0) {
            let url = new URL(oldUrl);
            url.search = srch;
            topw.history.pushState({}, null, url.href);
        }
    }
    var iframe = topw.document.getElementById("lm_xref");
    if (iframe != null) {
        var oldUrl2 = iframe.contentWindow.location.href;
        if (!oldUrl2.endsWith(srch)) {
            let url2 = new URL(oldUrl2);
            url2.search = srch;
            iframe.setAttribute("href", url2.href);
            // debugger;
        }
    }
}

function isElement(element) {
    if (typeof element !== 'object') {
        return false
    }
    return element instanceof Element || element instanceof HTMLDocument || element instanceof Node;
}

function setUrlParam(name, value) {
    console.log("setUrlParam(" + name + "," + value + ")");
    var topw = getTopWindow();
    let srch = new URLSearchParams(topw.location.search);
    if (!srch.has(name) || srch.get(name) != value) {
        srch.set(name, value);
        url = new URL(topw.location.href);
        url.search = srch;
        window.var_edits++;
        topw.history.pushState({}, null, url.href);
    }
}

function setComponent(name, value) {
    if (name instanceof String) {
        setUrlParam(name, value);
        var nav = getNavWindow();
        var ele = nav.window.getElementById("param_" + name);
        if (ele == null) ele = nav.window.getElementById(name);
        var retval = false;
        if (ele != null) retval = setComponent(ele, value);
        var topw = getTopWindow();
        var tele = topw.window.getElementById("param_" + name);
        if (tele == null) tele = topw.window.getElementById(name);
        if (tele != null)
            if (setComponent(tele, value)) return true;
        return retval;
    }
    var bool_value = true;
    var svalue = ("" + value).trim().toLowerCase;
    if (svalue == "false" || svalue == "no" || svalue == "off" || svalue == "unchecked" || value == "0" || value == "" || value == false || value == null || value == undefined || value == 0) {
        bool_value = false;
    }
    if (isElement(name)) {
        console.log("setComponent(" + name.id + "," + value + ")");
        if (name.getAttribute('type') == 'checkbox') {
            name.checked = bool_value;
            return true;
        } else
        if (name.tagName.toLowerCase == 'input') {
            name.value = value;
            return true;
        } else
        if (name.tagName.toLowerCase == 'textarea') {
            name.value = value;
            return true;
        } else {
            if (value.indexOf("<") == -1) {
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
    if (name instanceof String) {
        var topw = getTopWindow();
        var srch = new URLSearchParams(topw.location.search);
        debugger;
        if (srch.has(name)) {
            return srch.get(name);
        }
        var nav = getNavWindow();
        var ele = nav.window.getElementById("param_" + name);
        if (ele == null) ele = nav.window.getElementById(name);
        if (ele != null) return getComponent(ele, value);
        var tele = topw.window.getElementById("param_" + name);
        if (tele == null) tele = topw.window.getElementById(name);
        if (tele != null) return getComponent(tele, value);
        return elsev;
    }
    if (isElement(name)) {
        if (name.getAttribute('type') == 'checkbox') {
            return name.checked;
        } else
        if (name.tagName.toLowerCase == 'input') {
            return name.value;
        } else
        if (name.tagName.toLowerCase == 'textarea') {
            return name.value;
        }
        var it = name.innerText;
        if (it != null) it = it.trim();
        if (it == null || it == "") it = name.textContent;
        if (it != null) it = it.trim();
        if (it == null || it == "") it = name.innerHtml;
        if (it != null) it = it.trim();
        if (it != null) return it;
    }
    return elsev;
}

function getFormDicts() {
    var nav = getNavWindow();
    var topw = getTopWindow();

    var newSrch = new URLSearchParams({});

    for (const ele of nav.document.querySelectorAll('input[type="checkbox"]')) {
        var key = ele.id;
        if (key == "" || key.startsWith("accord")) continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name) key = ele.name;
            else {
                key = key.substring(6);
            }
        }
        var value = ele.checked;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }
    for (const ele of topw.document.querySelectorAll('input[type="checkbox"]')) {
        var key = ele.id;
        if (key == "" || key.startsWith("accord")) continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name) key = ele.name;
            else {
                key = key.substring(6);
            }
        }
        var value = ele.checked;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }

    for (const ele of nav.document.querySelectorAll('input[type]:not([type="checkbox"])')) {
        var key = ele.id;
        if (key == "" || key.startsWith("accord")) continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name) key = ele.name;
            else {
                key = key.substring(6);
            }
        }
        var value = ele.checked;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }
    for (const ele of topw.document.querySelectorAll('input[type]:not([type="checkbox"])')) {
        var key = ele.id;
        if (key == "" || key.startsWith("accord")) continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name) key = ele.name;
            else {
                key = key.substring(6);
            }
        }
        var value = ele.value;
        if (!newSrch.has(key)) {
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
    for (const [key, value] of oldSrch.entries()) {
        if (key == "" || key.startsWith("accord")) continue;
        console.log(`old ${key}: ${value}`);
    }
    oldSrch.delete("cmd");
    oldSrch.delete("grid");
    for (const [key, value] of oldSrch.entries()) {
        if (key == "" || key.startsWith("accord")) continue;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }
    var formdicts = getFormDicts();
    for (const [key, value] of formdicts.entries()) {
        if (key == "" || key.startsWith("accord")) continue;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        } else {
            setComponent(key, value);
        }
    }
    newSrch.delete("");
    for (const [key, value] of newSrch.entries()) {
        console.log(`new '${key}:' '${value}'`);
    }
    var newSrchStr = "?" + newSrch;
    var nsrc = topw.location.origin + "/arcproc_main" + newSrchStr;
    var tsrc = topw.location.origin + topw.location.pathname + newSrchStr;
    topw.history.pushState({}, null, tsrc);
    var iframe = document.getElementById("lm_xref");
    if (iframe == null) {
        iframe = window.parent.document.getElementById("lm_xref");
    }
    if (iframe.src == "about:blank" || !sameSrch(ooldSrch, newSrch)) {

        // setMainQueryParams(newSrchStr);
        //	debugger;
        //iframe.style.width = document.getElementById("main").style.width;
        if (iframe.src == "about:blank") {
            iframe.setAttribute("src", nsrc);
        } else {
            nav.location.assign(nsrc);
        }
    }
}

function sameSrch(oldSrch, newSrch) {
    for (const [key, value] of newSrch.entries()) {
        if (oldSrch.get(key) != newSrch.get(key)) return false;
    }
    for (const [key, value] of oldSrch.entries()) {
        if (oldSrch.get(key) != newSrch.get(key)) return false;
    }
    return true;
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