//$(function() {

function dragElement(elmnt) {
    var pos1 = 0,
        pos2 = 0,
        pos3 = 0,
        pos4 = 0;
    if (document.getElementById(elmnt.id + "header")) {
        /* if present, the header is where you move the DIV from:*/
        document.getElementById(elmnt.id + "header").onmousedown = dragMouseDown;
    } else {
        /* otherwise, move the DIV from anywhere inside the DIV:*/
        elmnt.onmousedown = dragMouseDown;
    }

    function dragMouseDown(e) {
        e = e || window.event;
        e.preventDefault();
        // get the mouse cursor position at startup:
        pos3 = e.clientX;
        pos4 = e.clientY;
        document.onmouseup = closeDragElement;
        // call a function whenever the cursor moves:
        document.onmousemove = elementDrag;
    }

    function elementDrag(e) {
        e = e || window.event;
        e.preventDefault();
        // calculate the new cursor position:
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        // set the element's new position:
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
    }

    function closeDragElement() {
        /* stop moving when mouse button is released:*/
        document.onmouseup = null;
        document.onmousemove = null;
    }
}
/*
	function showPos(event, text) {
		var el, x, y;
		
		el = document.getElementById('PopUp');
		if (window.event) {
			x = window.event.clientX + document.documentElement.scrollLeft + document.body.scrollLeft;
			y = window.event.clientY + document.documentElement.scrollTop + + document.body.scrollTop;
		} else {
			x = event.clientX + window.scrollX;
			y = event.clientY + window.scrollY;
		}
		x -= 2; y -= 2;
		y = y+15
		el.style.left = x + "px";
		el.style.top = y + "px";
		el.style.display = "block";
		document.getElementById('PopUpText').innerHTML = text;
	}
	*/

function addAccordian(target_name, depth) {
    return addAccordian4("navbar_items", target_name, target_name.firstChild.textContent, depth);
}

function clickGrid(name) {
    // in case we are passed the object and no the name
    var node = toEle(name);
    if (node != null)
        name = node.id;
    console.log("clickGrid(" + name + ")");
    var val = getComponent("grid", "");
    if (val != name) {
        if (true)
            return;
        navToParams({
            grid: name,
            cmd: "click_grid"
        });
    }
}

function toEle(name) {
    if (isElement(name))
        return name;
    if (name == null)
        return null;
    var node = null;
    if (typeof name === "string") {
        node = document.getElementById(name);
        if (node != null)
            return node;
        var nav = getNavWindow();
        node = nav.document.getElementById(name);
        if (node != null)
            return node;
        var topw = getTopWindow();
        node = topw.document.getElementById(name);
        if (node != null)
            return node;
        node = top.document.getElementById(name);
        if (node != null)
            return node;
        console.warn(name + " not found in " + topw);
        console.warn(name + " not found in " + nav);
        debugger;
        return null;
    }
    return node;
}

function getSomeElementById(Name) {
    var nameE = getSomeElementById(Name, window);
    if (nameE != null)
        return nameE;
    var P2 = window.parent;
    if (P2 != null && P2 != this) {
        nameE = getSomeElementById(Name, P2);
        if (nameE != null)
            return nameE;
    }
    var P3 = window.top;
    if (P3 != null && P3 != this && P3 != P2) {
        nameE = getSomeElementById(Name, P3);
        if (nameE != null)
            return nameE;
    }
    if (Name != "lm_xref") {
        var iframeP = getSomeElementById("lm_xref");
        if (iframeP != null) {
            var P4 = iframe.contentWindow;
            if (P4 != null && P4 != this && P4 != P2) {
                nameE = getSomeElementById(Name, P4);
                if (nameE != null)
                    return nameE;
            }
        }
    } else {
        var iframeP = document.getElementById("lm_xref");
        if (iframeP != null) {
            var P4 = iframe.contentWindow;
            if (P4 != null && P4 != this && P4 != P2) {
                nameE = getSomeElementById(Name, P4);
                if (nameE != null)
                    return nameE;
            }
        }
    }
    return null;
}

function getSomeElementById(Name, P) {
    if (P == null) {
        return null;
    }
    nameE = P.document.getElementById(Name);
    if (nameE != null)
        return nameE;
    var iframeP = P.document.getElementById("lm_xref");
    if (iframeP != null) {
        var P2 = iframe.contentWindow;
        if (P2 != null && P2 != P) {
            nameE = P2.document.getElementById(Name);
            if (nameE != null)
                return nameE;
        }
    }
}

var TwoFiftyPx = "250px";

function toggleNavL(Name) {
    var mainE = top.document.getElementById("main");
    var nameE = top.document.getElementById(Name);
    if (nameE.style.width != "0px") {
        nameE.style.width = "0px";
        mainE.style.marginLeft = "0px";
        //mainE.style.width = "200vh";
    } else {
        nameE.style.width = TwoFiftyPx;
        mainE.style.marginLeft = TwoFiftyPx;
    }
    var iframeE = top.document.getElementById('lm_xref');
    if (iframeE != null) { //iframeE.style.width = mainE.style.width;
        //iframeE.style.marginRight = mainE.style.marginRight; 
    }
}

function toggleNavR(Name) {
    var mainE = top.document.getElementById("main");
    var nameE = top.document.getElementById(Name);
    if (nameE.style.width != "0px") {
        nameE.style.width = "0px";
        mainE.style.marginRight = "0px";
        //mainE.style.width = "200vh";
    } else {
        nameE.style.width = TwoFiftyPx;
        mainE.style.marginRight = TwoFiftyPx;
    }
    var iframeE = top.document.getElementById('lm_xref');
    if (iframeE != null) { // iframeE.style.width = mainE.style.width;
        //	iframeE.style.marginRight = mainE.style.marginRight; 
    }
}

function navCmd(target) {
    console.log("navCmd(" + target + ")");
    navToParams({
        cmd: target
    });

    return false;
}


function getNavWindow() {
    if (top.navWindow != null)
        return top.navWindow;
    var iframe = document.getElementById("lm_xref");
    if (iframe == null) {
        iframe = window.parent.document.getElementById("lm_xref");
    }
    if (iframe == null) {
        iframe = top.document.getElementById("lm_xref");
    }
    if (iframe != null) {
        var cw = iframe.contentWindow;
        if (cw != null) {
            top.navWindow = cw;
            return cw;
        }
    }
    return window;
}

function getTopWindow() {
    var nav = getNavWindow();
    if (nav != null)
        return nav.parent;
    return window.top;
    //parent;
}

function setFormFields(srch) {
    var newSrch = new URLSearchParams(srch);
    for (const [key, value] of newSrch.entries()) {
        console.log(`newFF '${key}:' '${value}'`);
        if (ignoredData(key))
            continue;
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

function toComponent(name) {
    if (isElement(name))
        return name;
    if (ignoredData(name))
        return null;
    var nav = getNavWindow();
    var ele = nav.document.getElementById("param_" + name);
    if (ele != null)
        return ele;
    ele = nav.document.getElementById(name);
    if (ele != null)
        return ele;
    ele = document.getElementById("param_" + name);
    if (ele != null)
        return ele;
    ele = toEle(name);
    if (ele != null)
        return ele;
    return null;
}

function setComponent(name, value) {
    if (ignoredData(name))
        return false;

    if (typeof name === "string") {
        setUrlParam(name, value);
        ele = toComponent(name);
        var retval = false;
        if (ele != null)
            retval = setComponent(ele, value);
        var topw = getTopWindow();
        var tele = topw.document.getElementById("param_" + name);
        if (tele == null)
            tele = topw.document.getElementById(name);
        if (tele != null)
            if (setComponent(tele, value))
                return true;
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
        } else if (name.tagName.toLowerCase == 'input') {
            name.value = value;
            return true;
        } else if (name.tagName.toLowerCase == 'textarea') {
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

function add_tool_tips(name, text) {
    var node = toEle(name);
    if (node == null) {
        console.error("add_tool_tips missing: " + name);
        return null;
    }
    if (node != null) {
        var wasTitle = node.getAttribute('title');
        if (wasTitle == null || wasTitle === undefined || wasTitle == "") {
            node.setAttribute('title', text);
        } else if (!wasTitle.include(text)) {
            if (text.startsWith("#10;")) {
                node.setAttribute('title', wasTitle + text);
            } else if (text.endsWith("#10;")) {
                node.setAttribute('title', text + wasTitle);
            } else if (wasTitle.endsWith("#10;")) {
                node.setAttribute('title', wasTitle + text);
            } else if (wasTitle.startsWith("#10;")) {
                node.setAttribute('title', text + wasTitle);
            } else {
                wasTitle += "#10;";
                node.setAttribute('title', wasTitle + text);
            }
        }
        return node;
    }
}

function getComponent(name, elsev) {
    if (ignoredData(name))
        return elsev;
    if (typeof name === "string") {
        var topw = getTopWindow();
        var srch = new URLSearchParams(topw.location.search);
        debugger;
        if (srch.has(name)) {
            return srch.get(name);
        }
        var nav = getNavWindow();
        var ele = nav.document.getElementById("param_" + name);
        if (ele == null)
            ele = nav.document.getElementById(name);
        if (ele != null)
            return getComponent(ele, value);
        var tele = topw.document.getElementById("param_" + name);
        if (tele == null)
            tele = topw.document.getElementById(name);
        if (tele != null)
            return getComponent(tele, value);
        return elsev;
    }
    if (isElement(name)) {
        if (name.getAttribute('type') == 'checkbox') {
            return name.checked;
        } else if (name.tagName.toLowerCase == 'input') {
            return name.value;
        } else if (name.tagName.toLowerCase == 'textarea') {
            return name.value;
        }
        var it = name.innerText;
        if (it != null)
            it = it.trim();
        if (it == null || it == "")
            it = name.textContent;
        if (it != null)
            it = it.trim();
        if (it == null || it == "")
            it = name.innerHtml;
        if (it != null)
            it = it.trim();
        if (it != null)
            return it;
    }
    return elsev;
}

function getFormDicts() {
    var nav = getNavWindow();
    var topw = getTopWindow();

    var newSrch = new URLSearchParams({});

    for (const ele of nav.document.querySelectorAll('input[type="checkbox"]')) {
        var key = ele.id;
        if (ignoredData(key))
            continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name)
                key = ele.name;
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
        if (ignoredData(key))
            continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name)
                key = ele.name;
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
        if (ignoredData(key))
            continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name)
                key = ele.name;
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
        if (ignoredData(key))
            continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name)
                key = ele.name;
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

function ignoredData(key) {
    if (key == null || key == undefined || key == [] || key == {} || key == 'nothing') return true;
    if (key == 'undefined' || key === "" || key == " " || key === "icmd" || key === "cmd") return true;
    if ((typeof key === 'string') && key.includes("accord")) return true;
    return false;
}

{
    top.winObj = null;
    top.milledImages = {};
    top.navWindow = null;
}

function clearMenu() {
    top.navWindow = null;
    top.milledImages = {};
    var e = top.document.getElementById("navbar_items");
    if (e == null)
        return;
    var child = e.lastElementChild;
    while (child) {
        e.removeChild(child);
        child = e.lastElementChild;
    }
}

function opener() {
    if (top.winObj == null || top.winObj.closed) {
        top.winObj = top.open("", "tips", "width=500, height=300");
        top.winObj.realTop = window.top;
        popup = top.winObj;
        popup.window.addEventListener('load', () => {
            popup.window.addEventListener('unload', () => {
                console.log('> Popup Closed');
                top.winObj = null;
                // window.location.reload();
            });
        });

        popup.document.open();
        popup.document.write(`<!DOCTYPE html>
<html lang="en">
<head>
   <meta charset="UTF-8" />
   <meta http-equiv="X-UA-Compatible" content="IE=edge" />
   <meta name="viewport" content="width=device-width, initial-scale=1.0" />
   <title>Remove HTML</title>
</head>
<body>
   <input type="button" value="click here to clear" onclick="document.getElementById('toolTips').innerHTML = '' "/>
   <div id='toolTips' name='toolTips'/>
</body>
</html>`);
        popup.document.close();

        //top.winObj.onblur = () => top.winObj.focus();
        /*top.winObj.onload = function() {
			let html = `<div style="font-size:30px">Welcome!</div>`;
			top.winObj.document.body.insertAdjacentHTML('afterbegin', html);
        };*/
    }
}

function easyToMatch(text) {
    return text.replace(/[^0-9a-zA-Z_()]/gi, '');
}

function writeTips(info) {

    if (ignoredData(info)) return;

    var infotext = "" + info;
    var jqinfo = null;

    if (isElement(info)) {
        jqinfo = $(info);
    } else {
        if ( // skip simple one liners
            (!(infotext.includes("\n"))) &&
            (!(infotext.includes("\r"))) &&
            (!(infotext.includes("<")))) return false;
        jqinfo = $(`<pre style="font-size:30px">${infotext}</pre>`);
    }

    opener();

    let tipsList = top.winObj.document.getElementById('toolTips');
    var e2m = easyToMatch(jqinfo.innerText);

    for (const tip in tipsList.children) {
        if (easyToMatch(tip.innerText).includes(easyToMatch(e2m))) {
            jqinfo = tip;
            tip.detach()
            break;
        }
    }

    tipsList.prepend(jqinfo);
    jqinfo.scrollIntoView(true);
    top.winObj.focus();
}


function resizer() {
    opener();
    top.winObj.moveTo(0, 0);
    top.winObj.resizeTo(screen.availWidth, screen.availHeight);
}

function navToParams(jsonDict) {
    console.log("navToParams=" + jsonDict);
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
        if (ignoredData(key))
            continue;
        console.log(`old ${key}: ${value}`);
    }
    oldSrch.delete("cmd");
    oldSrch.delete("grid");
    for (const [key, value] of oldSrch.entries()) {
        if (ignoredData(key))
            continue;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }
    var formdicts = getFormDicts();
    for (const [key, value] of formdicts.entries()) {
        if (ignoredData(key))
            continue;
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
        if (oldSrch.get(key) != newSrch.get(key))
            return false;
    }
    for (const [key, value] of oldSrch.entries()) {
        if (oldSrch.get(key) != newSrch.get(key))
            return false;
    }
    return true;
}

function addAccordian4(NBI, target_name, target_textContent, depth) {
    var target_id = target_name.id;
    // console.log(`addAccordian(${target_id},${target_textContent},${depth})`);
    var nav = top.document.getElementById(NBI);
    var spaces = "";
    for (i = 0; i < depth; i++) {
        spaces = spaces + "&nbsp;";
    }
    var href = `#${target_id}`;
    href = "javascript:void(0)";
    let html = `<li class="nav-item" id="${target_id}_menu" >${spaces}<a class="nav-link" href="${href}" id="${target_id}_link" target="lm_xref" onclick="clickAccordian('${target_id}',true);">${target_textContent}</a></li>`;
    nav.insertAdjacentHTML('beforeend', html);
    e = nav.lastElementChild;
    return e;
    //e.scrollIntoView(false);
    // target_name.scrollIntoView(false);
}

function clickAccordian(target_name, scrollTo) {
    console.log(`clickAccordian('${target_name}',${scrollTo})`);
    if (typeof target_name === "string") {
        target_name = accordianStem(target_name);
    }
    var target = toEle(target_name);
    target.classList.toggle("active");
    var panel = toEle(target_name + "_panel");
    var keepGoing = (panel == null || !(panel.classList.contains("panel")));
    if (panel == null) {
        panel = target.nextElementSibling;
        togglePanel(panel, keepGoing, scrollTo);
    } else {
        togglePanel(panel, keepGoing, scrollTo);
    }

}

function getCurrentFrameAbsolutePosition() {
    let currentWindow = window;
    let currentParentWindow;
    let positions = [];
    let rect;

    while (currentWindow !== window.top) {
        currentParentWindow = currentWindow.parent;
        for (let idx = 0; idx < currentParentWindow.frames.length; idx++)
            if (currentParentWindow.frames[idx] === currentWindow) {
                for (let frameElement of currentParentWindow.document.getElementsByTagName('iframe')) {
                    if (frameElement.contentWindow === currentWindow) {
                        rect = frameElement.getBoundingClientRect();
                        positions.push({
                            x: rect.x,
                            y: rect.y
                        });
                    }
                }
                currentWindow = currentParentWindow;
                break;
            }
    }
    return positions.reduce((accumulator, currentValue) => {
        return {
            x: accumulator.x + currentValue.x,
            y: accumulator.y + currentValue.y
        };
    }, {
        x: 0,
        y: 0
    });
}

function isInViewport_when_IFrame(el) {

    var currentFramePosition = getCurrentFrameAbsolutePosition();

    var rect = el.getBoundingClientRect();
    var elemTop = rect.top + currentFramePosition.y;
    var elemBottom = rect.bottom + currentFramePosition.y;
    var elemLeft = rect.left + currentFramePosition.x;
    var elemRight = rect.right + currentFramePosition.x;

    var viewportWidth = window.innerWidth || document.documentElement.clientWidth;
    var viewportHeight = window.innerHeight || document.documentElement.clientHeight;
    var viewportTopHeight = window.top.innerHeight || window.top.document.documentElement.clientHeight;
    var viewportTopWidth = window.top.innerWidth || window.top.document.documentElement.clientWidth;
    var windowInnerHeight = (viewportHeight + viewportTopHeight * 0.5);
    var windowInnerWidth = (viewportWidth + viewportTopWidth * 0.5);

    console.log("eleTop " + elemTop);
    console.log("elemBottom " + elemBottom);

    console.log("windowInnerHeight " + (viewportHeight + (viewportTopHeight * 0.5)))

    var isVisibleH = (elemLeft >= 0) && (elemRight <= windowInnerWidth);
    var isVisibleV = (elemTop >= 0) && (elemBottom <= windowInnerHeight);
    return isVisibleH && isVisibleV;
}

function isInViewport(element) {
    if (window !== window.top) {
        return isInViewport_when_IFrame(element)
    }
    const rect = element.getBoundingClientRect();
    const viewportWidth = window.innerWidth || document.documentElement.clientWidth;
    const viewportHeight = window.innerHeight || document.documentElement.clientHeight;

    for (const key in rect) {
        if (typeof rect[key] !== 'function') {
            console.warn(`isInViewport ${key} : ${rect[key]}`);
        }
    }
    console.warn(`viewportWidth/Height=${viewportWidth}/${viewportWidth}`);

    var elemTop = rect.top;
    var elemBottom = rect.bottom;

    return (rect.top >= 0 && rect.left >= 0 && rect.bottom <= viewportHeight && rect.right <= viewportWidth);
}

function togglePanel(name, keepGoing, scrollTo) {
    var panel = intoPanel(name);
    if (panel == null)
        return;

    var notHidden = panel.classList.contains("panel_shown");
    var inViewport = isInViewport_when_IFrame(panel);
    console.warn("inViewport=" + inViewport);
    var wasLastPanel = (top.lastPanelShown == panel);
    if (wasLastPanel) {
        inViewport = true;
    }

    if (notHidden && !inViewport) {
        scrollToPanel(panel);
        lastPanelShown = panel;
        return;
    }

    if (notHidden) {
        hidePanel(panel, keepGoing);
    } else {
        showPanel(panel, keepGoing, scrollTo);
    }

    if (scrollTo) {
        scrollToPanel(panel);
    }
}

function setVisible(panel, tf) {
    //var block = (tf?"block":"none");
    var block = tf;
    panel.style.display = block;
    //panel.nextElementSibling.style.display=block;
    panel.scrollIntoView(false);
    panel.scrollIntoView(true)
}

function scrollToPanel(panel) {
    panel.scrollIntoView(false);
    var scrl = panel.previousElementSibling;
    if (scrl == null)
        scrl = panel;
    scrl.scrollIntoView(true);
}

{
    top.lastPanelShown = null;
}

function intoPanel(name) {
    if (name == null)
        return null;
    var panel = name;
    if (typeof name === "string") {
        panel = toEle(name);
        if (panel == null) {
            console.warn(name + " not found in " + nav);
            return null;
        }
    }
    if (!panel.classList.contains("panel")) {
        console.warn(panel + "not a panel: " + panel.classList);
        // return null;
    }
    return panel;
}

function showPanel(name, keepGoing, scrollTo) {
    var panel = intoPanel(name);
    if (panel == null)
        return;

    /*
		if(scrollTo) {
			// gets us the show hide toggles
			if(top.lastPanelShown == panel) {
				if(panel.classList.contains("panel_shown")) {
				   hidePanel(panel);
				   top.lastPanelShown = null;
				   return;
				}
			} 
		}*/
    //panel.scrollIntoView(false);

    var PE = panel.parentElement;
    if (PE != null) {
        if (!(PE.classList.contains("panel"))) {
            var GPE = PE.parentElement;
            if (GPE != null) {
                if (GPE.classList.contains("panel")) {
                    PE = GPE;
                } else {
                    PE = GPE.parentElement;
                }
            }
        }
        if (PE.classList.contains("panel_hidden")) {
            var sibling = PE.firstElementChild;
            do {
                if (sibling != panel) {
                    if (sibling.classList.contains("panel_shown")) {
                        hidePanel(sibling);
                    }
                }
            } while (sibling = sibling.nextElementSibling);
            showPanel(PE, false, false);
        }
    }
    panel.classList.add("panel_shown");
    panel.classList.remove("panel_hidden");
    //panel.style.display = "inline-block";
    //panel.style.minHeight = "10px";
    //panel.style.maxHeight = "none";
    //panel.style.overflow = "auto";
    if (scrollTo) {
        var scrl = panel.previousElementSibling;
        if (scrl == null)
            scrl = panel;
        scrl.scrollIntoView(true);
        top.lastPanelShown = panel;
    }

}

function hidePanel(name, keepGoing) {
    var panel = intoPanel(name);
    if (panel == null)
        return;
    panel.classList.add("panel_hidden");
    panel.classList.remove("panel_shown");
    /*
		panel.style.display = "none";
		panel.style.minHeight = "0px";  
		panel.style.maxHeight = "0px";
		panel.style.overflow = "none";*/
}

function correctCmd(sessCmd) {
    var newSrch;
    if (sessCmd.startsWith("?")) {
        newSrch = new URLSearchParams(sessCmd);
        sessCmd = top.window.location.origin + "/arcproc_iframe";
    } else if (sessCmd.includes("/") || sessCmd.includes("?")) {
        var url = new URL(sessCmd, top.window.location);
        newSrch = new URLSearchParams(url.search);
        sessCmd = top.window.location.origin + url.pathname;
    } else if (sessCmd.indexOf("=") > 0) {
        newSrch = new URLSearchParams("?" + sessCmd);
        sessCmd = top.window.location.origin + "/arcproc_iframe";
    } else {
        newSrch = new URLSearchParams("?icmd=" + sessCmd);
        sessCmd = top.window.location.origin + "/arcproc_iframe";
    }

    let oldSrch = new URLSearchParams(top.window.location.search);
    oldSrch.delete("icmd");
    oldSrch.delete("cmd");
    for (const [key, value] of oldSrch.entries()) {
        if (ignoredData(key))
            continue;
        if (!newSrch.has(key)) { // newSrch.set(key, value);
        }
    }
    var s = "" + newSrch;
    if (s !== "" && s !== "?") {
        if (!s.startsWith("?")) {
            s = "?" + s;
        }
        sessCmd = sessCmd + s;
    }
    return sessCmd;
}

function set_delayed_accordian_cmd(accordian_in, someCmd) {

    var accordian = ensure_delayed_accordian(accordian_in, "Cmd: " + someCmd);

    sessCmd = correctCmd(someCmd);
    var accordian_id = accordian.id;

    document.getElementById(accordian_id + '_iframe_div').innerHTML = `<p><a href="${sessCmd}" target="${accordian_id}_iframe">Refresh</a><br></p><p><iframe  
			src="${sessCmd}" width="1024" height="70%" frameborder="0" id="${accordian_id}_iframe" 
			name="${accordian_id}_iframe" style="width: 100%" scrolling="auto"></p></iframe></p>`;
    //  document.getElementById("sess_accordian").click();
}

function add_delayed_accordian(accordian_in, title, sessCmd) {
    var accordian = ensure_delayed_accordian(accordian_in, title);
    addAccordian4("global_command_items", accordian, title, 1);
    set_delayed_accordian_cmd(accordian, sessCmd);
}

function ensure_delayed_accordian(accordian_in, title) {
    if (isElement(accordian_in)) {
        return accordian_in;
    }
    debugger;
    var accordian_id = accordian_in;
    var accordian = top.document.getElementById(accordian_id);
    if (accordian == null) {
        var p = top.document.createElement('p');
        p.innerHTML = `<input type="checkbox" id="${accordian_id}" class="hidecontent" style="top: 0; left: 0; width: 100%; margin-top: 2px;"/>
				<label for="${accordian_id}" style="top: 0; left: 0; width: 100%; margin-top: 0px;">${title}</label>
				<div id="${accordian_id}_panel" class="content hidecontent" style="top: 0; left: 0; width: 100%; height=100vh;">
					<div id="${accordian_id}_iframe_div"></div>
					<label id="${accordian_id}_x" for="${accordian_id}" style="right: 0; position: relative; width: 8px">x</label>
				</div>`
        var sess_iframe_div = top.document.getElementById("sess_iframe_div");
        sess_iframe_div.appendChild(p);
        accordian = top.document.getElementById(accordian_id);
    }
    return accordian;
}


function removeLastInstance(badtext, str) {
    if (!str.endsWith(badtext)) return str;
    var charpos = str.lastIndexOf(badtext);
    if (charpos < 0) return str;
    ptone = str.substring(0, charpos);
    pttwo = str.substring(charpos + (badtext.length));
    return (ptone + pttwo);
}

function accordianStem(target_name) {
    target_name = removeLastInstance('_x', target_name);
    target_name = removeLastInstance('_div', target_name);
    target_name = removeLastInstance('_iframe', target_name);
    target_name = removeLastInstance('_panel', target_name);
    target_name = removeLastInstance('_link', target_name);
    target_name = removeLastInstance('_menu', target_name);
    return target_name;
}

function activateMenu(target_name) {
    var elem = target_name;
    if (typeof target_name === "string") {
        target_name = accordianStem(target_name);
        elem = top.document.getElementById(target_name + '_menu');
    }

    if (!isElement(elem)) return;

    if (elem.classList.contains("active")) { return; }

    var PWC = findParentWithClass(elem, "nav-pills");
    var sibling = PWC.firstElementChild;
    do {
        if (sibling != elem) {
            sibling.classList.remove("active");
        }
    } while (sibling = sibling.nextElementSibling);
    elem.classList.add("active");
    elem.scrollIntoView(false);
}

function htmlToIMG2(name) {
    var node = toEle(name);
    name = node.id;
    var img = null;
    var url = null;
    htmlToImage.toJpeg(node, {
        quality: 0.95
    }).then(function(dataUrl) {
        img = new Image();
        img.id = node.id;
        img.onclick = node.onclick;
        img.src = dataUrl;
        node.replaceWith(img);
    }).catch(function(error) {
        console.error('htmlToIMG2', error);
    });
}

function htmlToJPEG(name) {
    var node = toEle(name);
    if (node == null)
        node = document.getElementById(name);
    if (node == null)
        return null;
    var url = null;
    var nodeid = node.id;
    htmlToImage.toJpeg(node, {
        quality: 1.00
    }).then(function(dataUrl) {
        window.top.milledImages[nodeid] = dataUrl;
        url = dataUrl;
    }).catch(function(error) {
        console.error('htmlToJPEG', error);
    });
    return url;
}

function alreadyMilled(name) {
    var node = toEle(name);
    if (node == null)
        node = document.getElementById(name);
    if (node == null)
        return null;
    var url = null;
    var nodeid = node.id;
    htmlToImage.toJpeg(node, {
        quality: 1.00
    }).then(function(dataUrl) {
        window.top.milledImages[nodeid] = dataUrl;
        url = dataUrl;
    }).catch(function(error) {
        console.error('htmlToJPEG', error);
    });
    return url;
}


function intoNamedImg(nodespec, nodeid, h, w) {

    var node = toEle(nodespec);
    if (node == null)
        node = document.getElementById(nodespec);
    if (node == null)
        return null;
    //debugger;
    htmlToImage.toJpeg(node, {
        quality: 1.00
    }).then(function(dataUrl) {
        window.top.milledImages[nodeid] = dataUrl;
        if (false) {
            var link = document.createElement('a');
            link.download = name + '.png';
            link.href = dataUrl;
            link.click();
        }
        var img = new Image();
        img.id = nodeid;
        img.style.width = w + "px";
        img.style.height = h + "px";
        img.setAttribute("width", w);
        img.setAttribute("height", h);
        img.onclick = node.onclick;
        img.src = dataUrl;
        img.title = node.title;
        node.replaceWith(img);
    }).catch(function(error) { // console.error('intoNamedImg', error);
    });
}

function cvtToIMG(name) {
    var node = toEle(name);
    if (node == null)
        node = document.getElementById(name);
    if (node == null)
        return null;
    //debugger;
    var nodeid = node.id;
    if (false) {
        return intoNamedImg(node, nodeid, node.clientWidth, node.clientHeight);
    }
    htmlToImage.toJpeg(node, {
        quality: 1.00
    }).then(function(dataUrl) {
        window.top.milledImages[nodeid] = dataUrl;
        /*
				var link = document.createElement('a');
				link.download = name+'.png';
				link.href = dataUrl;
				link.click();
			*/
        var img = new Image();
        img.id = nodeid;
        img.style.width = node.clientWidth
        img.style.height = node.clientHeight;
        img.onclick = node.onclick;
        img.src = dataUrl;
        img.title = node.title;
        node.replaceWith(img);
    }).catch(function(error) {
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
function interceptClickEvent(e) {
    var href;
    var target = e.target || e.srcElement;
    if (target.tagName !== 'A') {
        var PE = target.parentElement;
        if (PE != null && PE.tagName === 'A') {
            target = PE;
        }
    }
    if (target.tagName === 'A') {
        href = target.getAttribute('href');
        if (href != null) {

            if (href.startsWith("?")) {
                e.preventDefault();
                if (href.includes("icmd=")) {
                    add_delayed_accordian("" + href, target.textContent, href);
                    return;
                }
                navToParams(href);
                return;
            } else if (href.includes("cmd=")) {
                var i = href.indexOf('?');
                if (i > 0) {
                    href = href.substring(i);
                    e.preventDefault();
                    navToParams(href);
                    return;
                }
            }
        }
    }

    maybeShowTips(target);
    console.log(`interceptClickEvent('${target}',${e})`);
    return;
}

function maybeShowTips(target) {
    var TE = target;
    while (TE != null && ignoredData(TE.getAttribute('title'))) {
        TE = TE.parentElement;
    }
    if (TE != null) {
        var title = TE.getAttribute('title');
        console.log(`writeTips('${TE}',${title})`);
        if (!ignoredData(title)) {
            writeTips(title);
        }
    }
}

function findParentWithClass(target, clasz) {
    var TE = target;
    while (TE != null) {
        if (TE.classList.contains(clasz)) return TE;
        TE = TE.parentElement;
    }
    //if (TE == null) return target;
    return null;
}


function mouseOverEvent(e) {

    var target = e.target || e.srcElement;
    if (true) {
        $(target).children(".description").show();
        $(target).mouseout(
            function() { $(this).children(".description").hide(); }
        );
    }

    var panel = findParentWithClass(target, "panel");
    if (panel != null) {
        activateMenu(panel.id);
        //console.log("mouseOverPanel(" + panel.id + "," + panel.tagName + ")");
    }

    var selectable = findParentWithClass(target, "selectable");
    if (selectable != null) {
        //activateMenu(selectable.id);
        console.log("mouseOverSelectable(" + selectable.id + "," + selectable.tagName + ")");
    }

}

function hideDesc(target) {
    $(target).children(".description").hide();
}

function showDesc(target) {
    maybeShowTips(target);
}

//var top.seInd;

{
    top.sortAscnd = false;
    top.seInd = null;
}

function sortDesc(a, b) {
    return a - b;
}

function sortAsc(a, b) {
    return b - a;
}

function sortMatrixDesc(a, b) {
    if (a[top.seInd] === b[top.seInd]) {
        return 0;
    } else {
        return (a[top.seInd] < b[top.seInd]) ? -1 : 1;
    }
}

function sortMatrixAsc(a, b) {
    if (a[top.seInd] === b[top.seInd]) {
        return 0;
    } else {
        return (a[top.seInd] > b[top.seInd]) ? -1 : 1;
    }
}

function getTblCont(tb) {
    var cols = [];
    $("tr:first td", tb).each(function(i, el) {
        cols[i] = [];
    });
    for (var c = 0; c < cols.length; c++) {
        $("tr", tb).each(function(i, el) {
            cols[c].push(parseInt($("td", el).eq(c).text().trim()));
        });
    }
    return cols;
}

function sortRow(rObj, desc) {
    var tblObj = getTblCont(rObj.parent());
    var rowInd = rObj.index();
    return sortTableRowIndex(tblObj, rowInd, desc);
}

function sortTableRowIndex(tableSpec, rowInd, desc) {
    var tblObj = upToTable(tableSpec);
    console.log("Pre-Sort", tblObj, top.seInd);
    if (desc == undefined || desc == true) {
        tblObj.sort(sortMatrixDesc);
    } else {
        tblObj.sort(sortMatrixAsc);
    }
    top.sortAscnd = (!(desc));
    console.log("Post Sort", tblObj, top.seInd);
    rObj.parent().find("tr").each(function(r, tr) {
        $("td", tr).each(function(i, el) {
            $(el).html(tblObj[i][r]);
        });
    });
}

function sortTableCols(tableSpec, rowIndex) {

    var tableE = upToTable(tableSpec);
    if (true) {
        return sortTableRowIndex(tableE, rowIndex, top.sortAscnd);
    }
    return sortTableColsCur(tableE, rowIndex);
}

function sortTableColsCur(tableSpec, rowIndex) {

    var tableE = upToTable(tableSpec);
    if (tableE == null) {
        return;
    }

    let table = $(tableE);
    let tr = table.find('tr');
    let selectedRow = $(tr[rowIndex]);
    let selectedRowTd = selectedRow.find('td');
    let selectedRowSorting = [];

    // find and get clicked tr and it formats it in index and value of the cells
    selectedRowTd.each(function(tdIndex) {
        let td = $(selectedRowTd[tdIndex]);
        selectedRowSorting.push({
            tdIndex: tdIndex,
            value: parseInt(Math.ceil(td.text().trim()))
        })
    })

    // it will compare values and sort
    selectedRowSorting = selectedRowSorting.sort(function(a, b) {

        if (a.value == 0) {
            return -1;
        }

        if (b.value == 0) {
            return -1;
        }

        return b.value - a.value
    });

    console.log(selectedRowSorting)

    // it will only return indexs of sorted list of cells
    var sortedIndexs = selectedRowSorting.map(function(rowSorting) {
        return rowSorting.tdIndex
    })

    console.log(sortedIndexs)
    table.find('tr').each(function() {
        let tr = $(this);
        let modifiedTr = [];

        tr.children().each(function(tdIndex, td) {

            if (tdIndex == 0) {
                console.log(td)
                modifiedTr[0] = td;

            } else {
                for (let i = 0; i < sortedIndexs.length; i++) {
                    console.log(sortedIndexs[i])
                        // it gives me index of sorted column.
                    if (tdIndex == i) {
                        let sortedIndex = sortedIndexs[i];

                        if (sortedIndex == undefined) {
                            console.log('i', i, sortedIndex)
                            sortedIndex = sortedIndexs.length + 1
                        }

                        modifiedTr[sortedIndex] = td;
                    }
                }
            }

        })

        tr.append(modifiedTr)
    })
}

function toNumStr(x) {
    var x1;
    if (isElement(x)) {
        x1 = x.innerText;
    } else {
        x1 = "" + x;
    }
    var x2 = x1.toLowerCase().trim();
    var number = parseFloat(x2);
    if (!isNaN(number)) {
        number = number * 100;
        number = Math.round(number);
        let padToFour = number => number <= 99999999 ? `0000000${number}`.slice(-8) : number;
        return padToFour;
    }
    return x2;
}

function isGreater(y1, x1) {
    if (x1 === y1)
        return 0;
    var x = toNumStr(x1);
    var y = toNumStr(y1);
    if (x === y)
        return 0;
    if (x < y)
        return 1;
    return -1;
}

function upToTable(table) {
    if (typeof table === 'object') {
        while (table != null && (table.tagName !== 'TABLE' || !table.classList.contains("sortable"))) {
            table = table.parentElement;
        }
    }
    return table;
}

function sortTableRows(tableSpec, n) {
    var rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;

    var table = upToTable(tableSpec);

    if (table == null) {
        return;
    }

    switching = true;
    //Set the sorting direction to ascending:
    dir = "asc";
    /*Make a loop that will continue until
	  no switching has been done:*/
    while (switching) {
        //start by saying: no switching is done:
        switching = false;
        rows = table.rows;
        /*Loop through all table rows (except the
		first, which contains table headers):*/
        for (i = 1; i < (rows.length - 1); i++) {
            //start by saying there should be no switching:
            shouldSwitch = false;
            /*Get the two elements you want to compare,
		  one from current row and one from the next:*/
            x = rows[i].getElementsByTagName("TD")[n];
            y = rows[i + 1].getElementsByTagName("TD")[n];
            /*check if the two rows should switch place,
		  based on the direction, asc or desc:*/
            if (dir == "asc") {
                if (isGreater(x, y) > 0) {
                    //if so, mark as a switch and break the loop:
                    shouldSwitch = true;
                    break;
                }
            } else if (dir == "desc") {
                if (isGreater(y, x) > 0) {
                    //if so, mark as a switch and break the loop:
                    shouldSwitch = true;
                    break;
                }
            }
        }
        if (shouldSwitch) {
            /*If a switch has been marked, make the switch
		  and mark that a switch has been done:*/
            rows[i].parentNode.insertBefore(rows[i + 1], rows[i]);
            switching = true;
            //Each time a switch is done, increase this count by 1:
            switchcount++;
        } else {
            /*If no switching has been done AND the direction is "asc",
		  set the direction to "desc" and run the while loop again.*/
            if (switchcount == 0 && dir == "asc") {
                dir = "desc";
                switching = true;
            }
        }
    }
}

function intoDataTable(target) {
    $(target).DataTable();
}


function windowCreation(id) {
    var isOut;
    document.getElementById("closeButton" + id).onclick = function() {
        fadeOut(document.getElementById("mydiv" + id), 50);
        isOut = true;
    };
    document.getElementById("button" + id).onclick = function() {
        if (document.getElementById("mydiv" + id).style.display === "initial") {
            isOut = false;
        }
        if (isOut) {
            document.getElementById("mydiv" + id).style = "position: absolute;";
            document.getElementById("mydiv" + id).style = "top: 80px;";
            fadeIn(document.getElementById("mydiv" + id), 50);
        }
        isOut = false;
    };
    dragElement(document.getElementById("mydiv" + id));
    isOut = true;
}

function dragElement(elmnt) {
    var pos1 = 0,
        pos2 = 0,
        pos3 = 0,
        pos4 = 0;
    if (document.getElementById(elmnt.id + "header")) {
        document.getElementById(elmnt.id + "header").onmousedown = dragMouseDown;
    } else {
        elmnt.onmousedown = dragMouseDown;
    }

    function dragMouseDown(e) {
        e = e || window.event;
        e.preventDefault();
        pos3 = e.clientX;
        pos4 = e.clientY;
        document.onmouseup = closeDragElement;
        document.onmousemove = elementDrag;
        var active = document.getElementsByClassName("mydiv");
        for (var i = active.length - 1; i > -1; i--) {
            active[i].classList.remove("mydivActive");
        }
        document.getElementById(elmnt.id).className += " mydivActive";
    }

    function elementDrag(e) {
        e = e || window.event;
        e.preventDefault();
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
    }

    function closeDragElement() {
        document.onmouseup = null;
        document.onmousemove = null;
    }
}

function fadeIn(elem, ms) {
    elem.style.opacity = 0;
    elem.style.filter = "alpha(opacity=0)";
    elem.style.display = "inline-block";
    elem.style.visibility = "visible";

    if (ms) {
        var opacity = 0;
        var timer = setInterval(function() {
            opacity += 50 / ms;
            if (opacity >= 1) {
                clearInterval(timer);
                opacity = 0.9;
            }
            elem.style.opacity = opacity;
            elem.style.filter = "alpha(opacity=" + opacity * 100 + ")";
            var active = document.getElementsByClassName("mydiv");
            for (var i = active.length - 1; i > -1; i--) {
                active[i].classList.remove("mydivActive");
            }
            elem.className += " mydivActive";
        }, 50);
    } else {
        elem.style.opacity = 1;
        elem.style.filter = "alpha(opacity=1)";
    }
}

function fadeOut(elem, ms) {
    if (ms) {
        var opacity = 1;
        var timer = setInterval(function() {
            opacity -= 50 / ms;
            if (opacity <= 0) {
                clearInterval(timer);
                opacity = 0;
                elem.style.display = "none";
                elem.style.visibility = "hidden";
            }
            elem.style.opacity = opacity;
            elem.style.filter = "alpha(opacity=" + opacity * 100 + ")";
        }, 50);
    } else {
        elem.style.opacity = 0;
        elem.style.filter = "alpha(opacity=0)";
        elem.style.display = "none";
        elem.style.visibility = "hidden";
    }
}

/*Make resizable div by Hung Nguyen*/
function makeResizableDiv(div) {
    const element = document.querySelector(div);
    const resizers = document.querySelectorAll(div + ' .resizer')
    const minimum_size = 20;
    let original_width = 0;
    let original_height = 0;
    let original_x = 0;
    let original_y = 0;
    let original_mouse_x = 0;
    let original_mouse_y = 0;
    for (let i = 0; i < resizers.length; i++) {
        const currentResizer = resizers[i];
        currentResizer.addEventListener('mousedown', function(e) {
            e.preventDefault()
            original_width = parseFloat(getComputedStyle(element, null).getPropertyValue('width').replace('px', ''));
            original_height = parseFloat(getComputedStyle(element, null).getPropertyValue('height').replace('px', ''));
            original_x = element.getBoundingClientRect().left;
            original_y = element.getBoundingClientRect().top;
            original_mouse_x = e.pageX;
            original_mouse_y = e.pageY;
            window.addEventListener('mousemove', resize)
            window.addEventListener('mouseup', stopResize)
        })

        function resize(e) {
            if (currentResizer.classList.contains('bottom-right')) {
                const width = original_width + (e.pageX - original_mouse_x);
                const height = original_height + (e.pageY - original_mouse_y)
                if (width > minimum_size) {
                    element.style.width = width + 'px'
                }
                if (height > minimum_size) {
                    element.style.height = height + 'px'
                }
            } else if (currentResizer.classList.contains('bottom-left')) {
                const height = original_height + (e.pageY - original_mouse_y)
                const width = original_width - (e.pageX - original_mouse_x)
                if (height > minimum_size) {
                    element.style.height = height + 'px'
                }
                if (width > minimum_size) {
                    element.style.width = width + 'px'
                    element.style.left = original_x + (e.pageX - original_mouse_x) + 'px'
                }
            } else if (currentResizer.classList.contains('top-right')) {
                const width = original_width + (e.pageX - original_mouse_x)
                const height = original_height - (e.pageY - original_mouse_y)
                if (width > minimum_size) {
                    element.style.width = width + 'px'
                }
                if (height > minimum_size) {
                    element.style.height = height + 'px'
                    element.style.top = original_y + (e.pageY - original_mouse_y) + 'px'
                }
            } else {
                const width = original_width - (e.pageX - original_mouse_x)
                const height = original_height - (e.pageY - original_mouse_y)
                if (width > minimum_size) {
                    element.style.width = width + 'px'
                    element.style.left = original_x + (e.pageX - original_mouse_x) + 'px'
                }
                if (height > minimum_size) {
                    element.style.height = height + 'px'
                    element.style.top = original_y + (e.pageY - original_mouse_y) + 'px'
                }
            }
        }

        function stopResize() {
            window.removeEventListener('mousemove', resize)
        }
    }
}

function topReady() {
    $('#tableMySideNavL').DataTable();
    makeResizableDiv('.resizable');
    for (i = 1; i < 3; i++) {
        try {
            windowCreation(i);
        } catch (err) {}
    }
}

function xframeReady() {
    $(".tiptext").mouseover(function() {
        $(this).children(".description").show();
    }).mouseout(function() {
        $(this).children(".description").hide();
    });
    $(".sortable tbody th").on("click", function(e) {
        var r = $(this).parent();
        top.seInd = r.index();
        if ($(this).data("sort") == undefined) {
            $(this).data("sort", true);
        }
        sortRow(r, $(this).data("sort"));
        $(this).data("sort", $(this).data("sort") ? false : true);
    });
    console.log("XFrame Ready End: " + window.location.href);
}

function xframeLoading() {
    console.log("XFrame Loading Start: " + window.location.href);
    $(".tiptext").mouseover(function() {
        $(this).children(".description").show();
    }).mouseout(function() {
        $(this).children(".description").hide();
    });
    $(".sortable tbody th").on("click", function(e) {
        var r = $(this).parent();
        top.seInd = r.index();
        if ($(this).data("sort") == undefined) {
            $(this).data("sort", true);
        }
        sortRow(r, $(this).data("sort"));
        $(this).data("sort", $(this).data("sort") ? false : true);
    });
}

function commonLoading() {
    console.log("Loading: " + window.location.href);
    //listen for link click events at the document level
    if (document.addEventListener) {
        document.removeEventListener('click', interceptClickEvent);
        document.addEventListener('click', interceptClickEvent);
        document.removeEventListener('mouseover', mouseOverEvent);
        document.addEventListener('mouseover', mouseOverEvent);
    } else if (document.attachEvent) {
        document.detachEvent('onclick', interceptClickEvent);
        document.attachEvent('onclick', interceptClickEvent);
    }
}


$(document).ready(function() {
    console.log("Document Ready Start: " + window.location.href);
    if (window == top) topReady();
    if (window !== top) xframeReady();

    commonLoading();
    console.log("Document Ready End: " + window.location.href);
});

{
    if (window !== top) xframeLoading();
    commonLoading();
}

//});