!function e(t,n,o){function r(s,a){if(!n[s]){if(!t[s]){var l="function"==typeof require&&require;if(!a&&l)return l(s,!0);if(i)return i(s,!0);var c=new Error("Cannot find module '"+s+"'");throw c.code="MODULE_NOT_FOUND",c}var u=n[s]={exports:{}};t[s][0].call(u.exports,function(e){var n=t[s][1][e];return r(n||e)},u,u.exports,e,t,n,o)}return n[s].exports}for(var i="function"==typeof require&&require,s=0;s<o.length;s++)r(o[s]);return r}({1:[function(e,t,n){"use strict";Object.defineProperty(n,"__esModule",{value:!0}),n.setCookie=function(e,t){document.cookie=e+"="+encodeURIComponent(t)+";path=/;"},n.clearCookie=function(e){document.cookie=e+"=;path=/;expires=Thu, 01-Jan-1970 00:00:01 GMT;"},n.getCookie=function(e){for(var t=e+"=",n=document.cookie.split(";"),o=0;o<n.length;o++){for(var r=n[o];" "==r.charAt(0);)r=r.substring(1,r.length);if(0==r.indexOf(t))return decodeURIComponent(r.substring(t.length,r.length))}return null}},{}],2:[function(e,t,n){"use strict";function o(e){var t=d[e];if(void 0==t)throw new Error("could not find <details> element with id '"+e+"'");return t}function r(e){for(var t=e.target,n=t.id,r=o(n),i=r.element.open,s=0,l=r.toggles;s<l.length;s++){var c=l[s];c.classList.contains("details-toggle-control")&&(c.classList.add(i?"collapser":"expander"),c.classList.remove(i?"expander":"collapser"))}t.open==r.openByDefault?delete p[n]:p[n]=!0,a()}function i(){for(var e=0,t=Array.prototype.slice.call(document.getElementsByTagName("details"));e<t.length;e++){var n=t[e];"string"==typeof n.id&&n.id.length>0&&(d[n.id]={element:n,openByDefault:!!n.open,toggles:[]},n.addEventListener("toggle",r))}}function s(e){var t=o(e).element;t.open=!t.open}function a(){var e=Object.keys(p);document.cookie="toggled="+encodeURIComponent(e.join("+"))}function l(){var e=h.getCookie("toggled");if(e)for(var t=0,n=e.split("+");t<n.length;t++){var o=n[t],r=d[o];p[o]=!0,r&&(r.element.open=!r.element.open)}}function c(e){e.preventDefault();var t=e.currentTarget.getAttribute("data-details-id");if(!t)throw new Error("element with class 'details-toggle' has no 'data-details-id' attribute!");s(t)}function u(){Array.prototype.slice.call(document.getElementsByClassName("details-toggle")).forEach(function(e){var t=e.getAttribute("data-details-id");if(!t)throw new Error("element with class 'details-toggle' has no 'data-details-id' attribute!");var n=o(t);n.toggles.push(e),e.addEventListener("click",c),e.classList.contains("details-toggle-control")&&e.classList.add(n.element.open?"collapser":"expander")})}Object.defineProperty(n,"__esModule",{value:!0});var h=e("./cookies"),d={},p={};n.init=function(){i(),l(),u()}},{"./cookies":1}],3:[function(e,t,n){"use strict";Object.defineProperty(n,"__esModule",{value:!0});var o=e("./style-menu"),r=e("./details-helper"),i=e("./quick-jump");!function(e){"interactive"===document.readyState?e():document.addEventListener("readystatechange",function(){"interactive"===document.readyState&&e()})}(function(){document.body.classList.add("js-enabled"),o.init(),r.init(),i.init()})},{"./details-helper":2,"./quick-jump":4,"./style-menu":5}],4:[function(e,t,n){"use strict";function o(e,t,n){var o=new XMLHttpRequest;o.onreadystatechange=function(){if(o.readyState===XMLHttpRequest.DONE)if(200===o.status){if(t)try{t(JSON.parse(o.responseText))}catch(e){n(o)}}else n&&n(o)},o.open("GET",e,!0),o.send()}function r(e){var t=document.querySelector("#page-menu"),n=document.createElement("li");t.insertBefore(n,t.firstChild),d.render(p(v,{onClick:e,title:"Quick Jump"}),t,n)}function i(e,t){return t.length<=e?t:t.slice(0,e)}function s(){return p("table",{class:"keyboard-shortcuts"},p("tr",null,p("th",null,"Key"),p("th",null,"Shortcut")),p("tr",null,p("td",null,p("span",{class:"key"},"s")),p("td",null,"Open this search box")),p("tr",null,p("td",null,p("span",{class:"key"},"esc")),p("td",null,"Close this search box")),p("tr",null,p("td",null,p("span",{class:"key"},"↓"),",",p("span",{class:"key"},"ctrl")," + ",p("span",{class:"key"},"j")),p("td",null,"Move down in search results")),p("tr",null,p("td",null,p("span",{class:"key"},"↑"),",",p("span",{class:"key"},"ctrl")," + ",p("span",{class:"key"},"k")),p("td",null,"Move up in search results")),p("tr",null,p("td",null,p("span",{class:"key"},"↵")),p("td",null,"Go to active search result")))}function a(){return p("p",null,"You can find any exported type, constructor, class, function or pattern defined in this package by (approximate) name.")}function l(e){var t=[p("p",null,"Your search for '",e.searchString,"' produced the following list of results: ",p("code",null,"[]"),"."),p("p",null,p("code",null,"Nothing")," matches your query for '",e.searchString,"'."),p("p",null,p("code",null,"Left \"no matches for '",e.searchString,"'\" :: Either String (NonEmpty SearchResult)"))];return t[(e.searchString||"a").charCodeAt(0)%t.length]}function c(e,t){d.render(p(g,{baseUrl:e||".",showHideTrigger:t||r}),document.body)}var u=this&&this.__extends||function(){var e=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(e,t){e.__proto__=t}||function(e,t){for(var n in t)t.hasOwnProperty(n)&&(e[n]=t[n])};return function(t,n){function o(){this.constructor=t}e(t,n),t.prototype=null===n?Object.create(n):(o.prototype=n.prototype,new o)}}();Object.defineProperty(n,"__esModule",{value:!0});var h=e("fuse.js"),d=e("preact"),p=d.h,f=d.Component,v=function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return u(t,e),t.prototype.render=function(e){return p("li",null,p("a",{href:"#",onClick:function(t){t.preventDefault(),e.onClick()}},e.title))},t}(f),g=function(e){function t(){var t=null!==e&&e.apply(this,arguments)||this;return t.linkIndex=0,t.focusPlease=!1,t.navigatedByKeyboard=!1,t}return u(t,e),t.prototype.componentWillMount=function(){var e=this;this.setState({searchString:"",isVisible:!1,expanded:{},activeLinkIndex:-1,moduleResults:[]}),o(this.props.baseUrl+"/doc-index.json",function(t){e.setState({fuse:new h(t,{threshold:.25,caseSensitive:!0,includeScore:!0,tokenize:!0,keys:[{name:"name",weight:.7},{name:"module",weight:.3}]}),moduleResults:[]})},function(t){console&&console.error("could not load 'doc-index.json' for searching",t),e.setState({failedLoading:!0})}),document.addEventListener("mousedown",this.hide.bind(this)),document.addEventListener("keydown",function(t){e.state.isVisible&&("Escape"===t.key?e.hide():"ArrowUp"===t.key||"k"===t.key&&t.ctrlKey?(t.preventDefault(),e.navigateLinks(-1)):"ArrowDown"===t.key||"j"===t.key&&t.ctrlKey?(t.preventDefault(),e.navigateLinks(1)):"Enter"===t.key&&e.state.activeLinkIndex>=0&&e.followActiveLink()),"s"===t.key&&"input"!==t.target.tagName.toLowerCase()&&(t.preventDefault(),e.show())})},t.prototype.hide=function(){this.setState({isVisible:!1})},t.prototype.show=function(){this.state.isVisible||(this.focusPlease=!0,this.setState({isVisible:!0,activeLinkIndex:-1}))},t.prototype.toggleVisibility=function(){this.state.isVisible?this.hide():this.show()},t.prototype.navigateLinks=function(e){var t=Math.max(-1,Math.min(this.linkIndex-1,this.state.activeLinkIndex+e));this.navigatedByKeyboard=!0,this.setState({activeLinkIndex:t})},t.prototype.followActiveLink=function(){this.activeLinkAction&&this.activeLinkAction()},t.prototype.updateResults=function(){var e=this.input&&this.input.value||"",t={};this.state.fuse.search(e).forEach(function(e){var n=e.item.module;(t[n]||(t[n]=[])).push(e)});var n=[];for(var o in t)!function(e){var o=t[e],r=0;o.forEach(function(e){r+=1/e.score}),n.push({module:e,totalScore:1/r,items:o})}(o);n.sort(function(e,t){return e.totalScore-t.totalScore}),this.setState({searchString:e,isVisible:!0,moduleResults:n})},t.prototype.componentDidUpdate=function(){if(this.searchResults&&this.activeLink&&this.navigatedByKeyboard){var e=this.activeLink.getClientRects()[0],t=this.searchResults.getClientRects()[0].top;e.bottom>window.innerHeight?this.searchResults.scrollTop+=e.bottom-window.innerHeight+80:e.top<t&&(this.searchResults.scrollTop-=t-e.top+80)}this.focusPlease&&this.input&&this.input.focus(),this.navigatedByKeyboard=!1,this.focusPlease=!1},t.prototype.componentDidMount=function(){this.props.showHideTrigger(this.toggleVisibility.bind(this))},t.prototype.render=function(e,t){var n=this;if(t.failedLoading){var o="file:"==window.location.protocol;return p("div",{id:"search",class:t.isVisible?"":"hidden"},p("div",{id:"search-results"},p("p",{class:"error"},"Failed to load file 'doc-index.json' containing definitions in this package."),o?p("p",{class:"error"},"To use quick jump, load this page with HTTP (from a local static file web server) instead of using the ",p("code",null,"file://")," protocol. (For security reasons, it is not possible to fetch auxiliary files using JS in a HTML page opened with ",p("code",null,"file://"),".)"):[]))}this.linkIndex=0;var r=function(e){e.stopPropagation()},c=i(10,t.moduleResults).map(function(e){return n.renderResultsInModule(e)});return p("div",{id:"search",class:t.isVisible?"":"hidden"},p("div",{id:"search-form",onMouseDown:r},p("input",{placeholder:"Search in package by name",ref:function(e){n.input=e},onFocus:this.show.bind(this),onClick:this.show.bind(this),onInput:this.updateResults.bind(this)})),p("div",{id:"search-results",ref:function(e){n.searchResults=e},onMouseDown:r,onMouseOver:function(e){for(var t=e.target;t&&"function"==typeof t.getAttribute;){var o=t.getAttribute("data-link-index");if("string"==typeof o){var r=parseInt(o,10);n.setState({activeLinkIndex:r});break}t=t.parentNode}}},""===t.searchString?[p(a,null),p(s,null)]:0==c.length?p(l,{searchString:t.searchString}):p("ul",null,c)))},t.prototype.renderResultsInModule=function(e){var t=this,n=e.items,o=e.module,r=this.state.expanded[o]||n.length<=10,s=r?n:i(8,n),a=function(e){return p("li",{class:"search-result"},t.navigationLink(t.props.baseUrl+"/"+e.link,{},p(m,{html:e.display_html})))};return p("li",{class:"search-module"},p("h4",null,o),p("ul",null,s.map(function(e){return a(e.item)}),r?[]:p("li",{class:"more-results"},this.actionLink(function(){var e=Object.assign({},t.state.expanded);e[o]=!0,t.setState({expanded:e})},{},"show "+(n.length-s.length)+" more results from this module"))))},t.prototype.navigationLink=function(e,t){for(var n=this,o=[],r=2;r<arguments.length;r++)o[r-2]=arguments[r];var i=Object.assign({href:e,onClick:this.hide.bind(this)},t),s=function(){window.location.href=e,n.hide()};return this.menuLink.apply(this,[i,s].concat(o))},t.prototype.actionLink=function(e,t){for(var n=[],o=2;o<arguments.length;o++)n[o-2]=arguments[o];var r=Object.assign({href:"#",onClick:function(t){t.preventDefault(),e()}},t);return this.menuLink.apply(this,[r,e].concat(n))},t.prototype.menuLink=function(e,t){for(var n=this,o=[],r=2;r<arguments.length;r++)o[r-2]=arguments[r];var i=this.linkIndex;i===this.state.activeLinkIndex&&(e.class=(e.class?e.class+" ":"")+"active-link",e.ref=function(e){e&&(n.activeLink=e)},this.activeLinkAction=t);var s=Object.assign({"data-link-index":i},e);return this.linkIndex+=1,p.apply(void 0,["a",s].concat(o))},t}(f),m=function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return u(t,e),t.prototype.shouldComponentUpdate=function(e){return this.props.html!==e.html},t.prototype.render=function(e){return p("div",{dangerouslySetInnerHTML:{__html:e.html}})},t}(f);n.init=c,window.quickNav={init:c}},{"fuse.js":6,preact:7}],5:[function(e,t,n){"use strict";function o(e){return(" "+e+" ").replace(g," ")}function r(e){return e.replace(m,"")}function i(e,t){return o(e.className||"").indexOf(" "+t+" ")>=0}function s(e,t){var n=o(e.className||"");n.indexOf(" "+t+" ")<0&&(e.className=r(n+" "+t))}function a(e,t){var n=o(e.className||"");n=n.replace(" "+t+" "," "),e.className=r(n)}function l(e,t,n,o){return null==o&&(o=!i(e,t)),o?(a(e,n),s(e,t)):(a(e,t),s(e,n)),o}function c(e){var t=document.getElementById("page-menu");if(t&&t.firstChild){var n=t.firstChild.cloneNode(!1);n.innerHTML=e,t.appendChild(n)}}function u(){return Array.prototype.slice.call(document.getElementsByTagName("link")).filter(function(e){return-1!=e.rel.indexOf("style")&&e.title})}function h(){var e=u(),t="";e.forEach(function(e){t+="<li><a href='#' onclick=\"setActiveStyleSheet('"+e.title+"'); return false;\">"+e.title+"</a></li>"}),e.length>1&&c("<div id='style-menu-holder'><a href='#' onclick='styleMenu(); return false;'>Style &#9662;</a><ul id='style-menu' class='hide'>"+t+"</ul></div>")}function d(e){for(var t=u(),n=null,o=0;o<t.length;o++){var r=t[o];r.disabled=!0,r.title==e&&(n=r)}n?(n.disabled=!1,v.setCookie("haddock-style",e)):(t[0].disabled=!1,v.clearCookie("haddock-style")),f(!1)}function p(){var e=v.getCookie("haddock-style");e&&d(e)}function f(e){var t=document.getElementById("style-menu");t&&y(t,e)}Object.defineProperty(n,"__esModule",{value:!0});var v=e("./cookies"),g=/\s\s+/g,m=/^\s+|\s+$/g,y=function(e,t){return function(n,o){return l(n,e,t,o)}}("show","hide");n.init=function(){h(),p()}},{"./cookies":1}],6:[function(e,t,n){!function(e,o){"object"==typeof n&&"object"==typeof t?t.exports=o():"function"==typeof define&&define.amd?define("Fuse",[],o):"object"==typeof n?n.Fuse=o():e.Fuse=o()}(this,function(){return function(e){function t(o){if(n[o])return n[o].exports;var r=n[o]={i:o,l:!1,exports:{}};return e[o].call(r.exports,r,r.exports,t),r.l=!0,r.exports}var n={};return t.m=e,t.c=n,t.i=function(e){return e},t.d=function(e,n,o){t.o(e,n)||Object.defineProperty(e,n,{configurable:!1,enumerable:!0,get:o})},t.n=function(e){var n=e&&e.__esModule?function(){return e.default}:function(){return e};return t.d(n,"a",n),n},t.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},t.p="",t(t.s=8)}([function(e,t,n){"use strict";e.exports=function(e){return"[object Array]"===Object.prototype.toString.call(e)}},function(e,t,n){"use strict";function o(e,t){if(!(e instanceof t))throw new TypeError("Cannot call a class as a function")}var r=function(){function e(e,t){for(var n=0;n<t.length;n++){var o=t[n];o.enumerable=o.enumerable||!1,o.configurable=!0,"value"in o&&(o.writable=!0),Object.defineProperty(e,o.key,o)}}return function(t,n,o){return n&&e(t.prototype,n),o&&e(t,o),t}}(),i=n(5),s=n(7),a=n(4),l=function(){function e(t,n){var r=n.location,i=void 0===r?0:r,s=n.distance,l=void 0===s?100:s,c=n.threshold,u=void 0===c?.6:c,h=n.maxPatternLength,d=void 0===h?32:h,p=n.isCaseSensitive,f=void 0!==p&&p,v=n.tokenSeparator,g=void 0===v?/ +/g:v,m=n.findAllMatches,y=void 0!==m&&m,_=n.minMatchCharLength,k=void 0===_?1:_;o(this,e),this.options={location:i,distance:l,threshold:u,maxPatternLength:d,isCaseSensitive:f,tokenSeparator:g,findAllMatches:y,minMatchCharLength:k},this.pattern=this.options.isCaseSensitive?t:t.toLowerCase(),this.pattern.length<=d&&(this.patternAlphabet=a(this.pattern))}return r(e,[{key:"search",value:function(e){if(this.options.isCaseSensitive||(e=e.toLowerCase()),this.pattern===e)return{isMatch:!0,score:0,matchedIndices:[[0,e.length-1]]};var t=this.options,n=t.maxPatternLength,o=t.tokenSeparator;if(this.pattern.length>n)return i(e,this.pattern,o);var r=this.options,a=r.location,l=r.distance,c=r.threshold,u=r.findAllMatches,h=r.minMatchCharLength;return s(e,this.pattern,this.patternAlphabet,{location:a,distance:l,threshold:c,findAllMatches:u,minMatchCharLength:h})}}]),e}();e.exports=l},function(e,t,n){"use strict";var o=n(0),r=function e(t,n,r){if(n){var i=n.indexOf("."),s=n,a=null;-1!==i&&(s=n.slice(0,i),a=n.slice(i+1));var l=t[s];if(null!==l&&void 0!==l)if(a||"string"!=typeof l&&"number"!=typeof l)if(o(l))for(var c=0,u=l.length;c<u;c+=1)e(l[c],a,r);else a&&e(l,a,r);else r.push(l.toString())}else r.push(t);return r};e.exports=function(e,t){return r(e,t,[])}},function(e,t,n){"use strict";e.exports=function(){for(var e=arguments.length>0&&void 0!==arguments[0]?arguments[0]:[],t=arguments.length>1&&void 0!==arguments[1]?arguments[1]:1,n=[],o=-1,r=-1,i=0,s=e.length;i<s;i+=1){var a=e[i];a&&-1===o?o=i:a||-1===o||((r=i-1)-o+1>=t&&n.push([o,r]),o=-1)}return e[i-1]&&i-o>=t&&n.push([o,i-1]),n}},function(e,t,n){"use strict";e.exports=function(e){for(var t={},n=e.length,o=0;o<n;o+=1)t[e.charAt(o)]=0;for(var r=0;r<n;r+=1)t[e.charAt(r)]|=1<<n-r-1;return t}},function(e,t,n){"use strict";var o=/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g;e.exports=function(e,t){var n=arguments.length>2&&void 0!==arguments[2]?arguments[2]:/ +/g,r=new RegExp(t.replace(o,"\\$&").replace(n,"|")),i=e.match(r),s=!!i,a=[];if(s)for(var l=0,c=i.length;l<c;l+=1){var u=i[l];a.push([e.indexOf(u),u.length-1])}return{score:s?.5:1,isMatch:s,matchedIndices:a}}},function(e,t,n){"use strict";e.exports=function(e,t){var n=t.errors,o=void 0===n?0:n,r=t.currentLocation,i=void 0===r?0:r,s=t.expectedLocation,a=void 0===s?0:s,l=t.distance,c=void 0===l?100:l,u=o/e.length,h=Math.abs(a-i);return c?u+h/c:h?1:u}},function(e,t,n){"use strict";var o=n(6),r=n(3);e.exports=function(e,t,n,i){for(var s=i.location,a=void 0===s?0:s,l=i.distance,c=void 0===l?100:l,u=i.threshold,h=void 0===u?.6:u,d=i.findAllMatches,p=void 0!==d&&d,f=i.minMatchCharLength,v=void 0===f?1:f,g=a,m=e.length,y=h,_=e.indexOf(t,g),k=t.length,b=[],x=0;x<m;x+=1)b[x]=0;if(-1!==_){var w=o(t,{errors:0,currentLocation:_,expectedLocation:g,distance:c});if(y=Math.min(w,y),-1!==(_=e.lastIndexOf(t,g+k))){var S=o(t,{errors:0,currentLocation:_,expectedLocation:g,distance:c});y=Math.min(S,y)}}_=-1;for(var L=[],C=1,M=k+m,N=1<<k-1,I=0;I<k;I+=1){for(var A=0,O=M;A<O;)o(t,{errors:I,currentLocation:g+O,expectedLocation:g,distance:c})<=y?A=O:M=O,O=Math.floor((M-A)/2+A);M=O;var E=Math.max(1,g-O+1),T=p?m:Math.min(g+O,m)+k,j=Array(T+2);j[T+1]=(1<<I)-1;for(var P=T;P>=E;P-=1){var R=P-1,U=n[e.charAt(R)];if(U&&(b[R]=1),j[P]=(j[P+1]<<1|1)&U,0!==I&&(j[P]|=(L[P+1]|L[P])<<1|1|L[P+1]),j[P]&N&&(C=o(t,{errors:I,currentLocation:R,expectedLocation:g,distance:c}))<=y){if(y=C,(_=R)<=g)break;E=Math.max(1,2*g-_)}}if(o(t,{errors:I+1,currentLocation:g,expectedLocation:g,distance:c})>y)break;L=j}return{isMatch:_>=0,score:0===C?.001:C,matchedIndices:r(b,v)}}},function(e,t,n){"use strict";function o(e,t){if(!(e instanceof t))throw new TypeError("Cannot call a class as a function")}var r=function(){function e(e,t){for(var n=0;n<t.length;n++){var o=t[n];o.enumerable=o.enumerable||!1,o.configurable=!0,"value"in o&&(o.writable=!0),Object.defineProperty(e,o.key,o)}}return function(t,n,o){return n&&e(t.prototype,n),o&&e(t,o),t}}(),i=n(1),s=n(2),a=n(0),l=function(){function e(t,n){var r=n.location,i=void 0===r?0:r,a=n.distance,l=void 0===a?100:a,c=n.threshold,u=void 0===c?.6:c,h=n.maxPatternLength,d=void 0===h?32:h,p=n.caseSensitive,f=void 0!==p&&p,v=n.tokenSeparator,g=void 0===v?/ +/g:v,m=n.findAllMatches,y=void 0!==m&&m,_=n.minMatchCharLength,k=void 0===_?1:_,b=n.id,x=void 0===b?null:b,w=n.keys,S=void 0===w?[]:w,L=n.shouldSort,C=void 0===L||L,M=n.getFn,N=void 0===M?s:M,I=n.sortFn,A=void 0===I?function(e,t){return e.score-t.score}:I,O=n.tokenize,E=void 0!==O&&O,T=n.matchAllTokens,j=void 0!==T&&T,P=n.includeMatches,R=void 0!==P&&P,U=n.includeScore,D=void 0!==U&&U,V=n.verbose,F=void 0!==V&&V;o(this,e),this.options={location:i,distance:l,threshold:u,maxPatternLength:d,isCaseSensitive:f,tokenSeparator:g,findAllMatches:y,minMatchCharLength:k,id:x,keys:S,includeMatches:R,includeScore:D,shouldSort:C,getFn:N,sortFn:A,verbose:F,tokenize:E,matchAllTokens:j},this.setCollection(t)}return r(e,[{key:"setCollection",value:function(e){return this.list=e,e}},{key:"search",value:function(e){this._log('---------\nSearch pattern: "'+e+'"');var t=this._prepareSearchers(e),n=t.tokenSearchers,o=t.fullSearcher,r=this._search(n,o),i=r.weights,s=r.results;return this._computeScore(i,s),this.options.shouldSort&&this._sort(s),this._format(s)}},{key:"_prepareSearchers",value:function(){var e=arguments.length>0&&void 0!==arguments[0]?arguments[0]:"",t=[];if(this.options.tokenize)for(var n=e.split(this.options.tokenSeparator),o=0,r=n.length;o<r;o+=1)t.push(new i(n[o],this.options));return{tokenSearchers:t,fullSearcher:new i(e,this.options)}}},{key:"_search",value:function(){var e=arguments.length>0&&void 0!==arguments[0]?arguments[0]:[],t=arguments[1],n=this.list,o={},r=[];if("string"==typeof n[0]){for(var i=0,s=n.length;i<s;i+=1)this._analyze({key:"",value:n[i],record:i,index:i},{resultMap:o,results:r,tokenSearchers:e,fullSearcher:t});return{weights:null,results:r}}for(var a={},l=0,c=n.length;l<c;l+=1)for(var u=n[l],h=0,d=this.options.keys.length;h<d;h+=1){var p=this.options.keys[h];if("string"!=typeof p){if(a[p.name]={weight:1-p.weight||1},p.weight<=0||p.weight>1)throw new Error("Key weight has to be > 0 and <= 1");p=p.name}else a[p]={weight:1};this._analyze({key:p,value:this.options.getFn(u,p),record:u,index:l},{resultMap:o,results:r,tokenSearchers:e,fullSearcher:t})}return{weights:a,results:r}}},{key:"_analyze",value:function(e,t){var n=e.key,o=e.arrayIndex,r=void 0===o?-1:o,i=e.value,s=e.record,l=e.index,c=t.tokenSearchers,u=void 0===c?[]:c,h=t.fullSearcher,d=void 0===h?[]:h,p=t.resultMap,f=void 0===p?{}:p,v=t.results,g=void 0===v?[]:v;if(void 0!==i&&null!==i){var m=!1,y=-1,_=0;if("string"==typeof i){this._log("\nKey: "+(""===n?"-":n));var k=d.search(i);if(this._log('Full text: "'+i+'", score: '+k.score),this.options.tokenize){for(var b=i.split(this.options.tokenSeparator),x=[],w=0;w<u.length;w+=1){var S=u[w];this._log('\nPattern: "'+S.pattern+'"');for(var L=!1,C=0;C<b.length;C+=1){var M=b[C],N=S.search(M),I={};N.isMatch?(I[M]=N.score,m=!0,L=!0,x.push(N.score)):(I[M]=1,this.options.matchAllTokens||x.push(1)),this._log('Token: "'+M+'", score: '+I[M])}L&&(_+=1)}y=x[0];for(var A=x.length,O=1;O<A;O+=1)y+=x[O];y/=A,this._log("Token score average:",y)}var E=k.score;y>-1&&(E=(E+y)/2),this._log("Score average:",E);var T=!this.options.tokenize||!this.options.matchAllTokens||_>=u.length;if(this._log("\nCheck Matches: "+T),(m||k.isMatch)&&T){var j=f[l];j?j.output.push({key:n,arrayIndex:r,value:i,score:E,matchedIndices:k.matchedIndices}):(f[l]={item:s,output:[{key:n,arrayIndex:r,value:i,score:E,matchedIndices:k.matchedIndices}]},g.push(f[l]))}}else if(a(i))for(var P=0,R=i.length;P<R;P+=1)this._analyze({key:n,arrayIndex:P,value:i[P],record:s,index:l},{resultMap:f,results:g,tokenSearchers:u,fullSearcher:d})}}},{key:"_computeScore",value:function(e,t){this._log("\n\nComputing score:\n");for(var n=0,o=t.length;n<o;n+=1){for(var r=t[n].output,i=r.length,s=0,a=1,l=0;l<i;l+=1){var c=e?e[r[l].key].weight:1,u=(1===c?r[l].score:r[l].score||.001)*c;1!==c?a=Math.min(a,u):(r[l].nScore=u,s+=u)}t[n].score=1===a?s/i:a,this._log(t[n])}}},{key:"_sort",value:function(e){this._log("\n\nSorting...."),e.sort(this.options.sortFn)}},{key:"_format",value:function(e){var t=[];this._log("\n\nOutput:\n\n",JSON.stringify(e));var n=[];this.options.includeMatches&&n.push(function(e,t){var n=e.output;t.matches=[];for(var o=0,r=n.length;o<r;o+=1){var i=n[o];if(0!==i.matchedIndices.length){var s={indices:i.matchedIndices,value:i.value};i.key&&(s.key=i.key),i.hasOwnProperty("arrayIndex")&&i.arrayIndex>-1&&(s.arrayIndex=i.arrayIndex),t.matches.push(s)}}}),this.options.includeScore&&n.push(function(e,t){t.score=e.score});for(var o=0,r=e.length;o<r;o+=1){var i=e[o];if(this.options.id&&(i.item=this.options.getFn(i.item,this.options.id)[0]),n.length){for(var s={item:i.item},a=0,l=n.length;a<l;a+=1)n[a](i,s);t.push(s)}else t.push(i.item)}return t}},{key:"_log",value:function(){if(this.options.verbose){var e;(e=console).log.apply(e,arguments)}}}]),e}();e.exports=l}])})},{}],7:[function(e,t,n){!function(){"use strict";function e(){}function n(t,n){var o,r,i,s,a=O;for(s=arguments.length;s-- >2;)A.push(arguments[s]);for(n&&null!=n.children&&(A.length||A.push(n.children),delete n.children);A.length;)if((r=A.pop())&&void 0!==r.pop)for(s=r.length;s--;)A.push(r[s]);else"boolean"==typeof r&&(r=null),(i="function"!=typeof t)&&(null==r?r="":"number"==typeof r?r=String(r):"string"!=typeof r&&(i=!1)),i&&o?a[a.length-1]+=r:a===O?a=[r]:a.push(r),o=i;var l=new e;return l.nodeName=t,l.children=a,l.attributes=null==n?void 0:n,l.key=null==n?void 0:n.key,void 0!==I.vnode&&I.vnode(l),l}function o(e,t){for(var n in t)e[n]=t[n];return e}function r(e){!e.__d&&(e.__d=!0)&&1==j.push(e)&&(I.debounceRendering||E)(i)}function i(){var e,t=j;for(j=[];e=t.pop();)e.__d&&L(e)}function s(e,t,n){return"string"==typeof t||"number"==typeof t?void 0!==e.splitText:"string"==typeof t.nodeName?!e._componentConstructor&&a(e,t.nodeName):n||e._componentConstructor===t.nodeName}function a(e,t){return e.__n===t||e.nodeName.toLowerCase()===t.toLowerCase()}function l(e){var t=o({},e.attributes);t.children=e.children;var n=e.nodeName.defaultProps;if(void 0!==n)for(var r in n)void 0===t[r]&&(t[r]=n[r]);return t}function c(e,t){var n=t?document.createElementNS("http://www.w3.org/2000/svg",e):document.createElement(e);return n.__n=e,n}function u(e){var t=e.parentNode;t&&t.removeChild(e)}function h(e,t,n,o,r){if("className"===t&&(t="class"),"key"===t);else if("ref"===t)n&&n(null),o&&o(e);else if("class"!==t||r)if("style"===t){if(o&&"string"!=typeof o&&"string"!=typeof n||(e.style.cssText=o||""),o&&"object"==typeof o){if("string"!=typeof n)for(var i in n)i in o||(e.style[i]="");for(var i in o)e.style[i]="number"==typeof o[i]&&!1===T.test(i)?o[i]+"px":o[i]}}else if("dangerouslySetInnerHTML"===t)o&&(e.innerHTML=o.__html||"");else if("o"==t[0]&&"n"==t[1]){var s=t!==(t=t.replace(/Capture$/,""));t=t.toLowerCase().substring(2),o?n||e.addEventListener(t,p,s):e.removeEventListener(t,p,s),(e.__l||(e.__l={}))[t]=o}else if("list"!==t&&"type"!==t&&!r&&t in e)d(e,t,null==o?"":o),null!=o&&!1!==o||e.removeAttribute(t);else{var a=r&&t!==(t=t.replace(/^xlink\:?/,""));null==o||!1===o?a?e.removeAttributeNS("http://www.w3.org/1999/xlink",t.toLowerCase()):e.removeAttribute(t):"function"!=typeof o&&(a?e.setAttributeNS("http://www.w3.org/1999/xlink",t.toLowerCase(),o):e.setAttribute(t,o))}else e.className=o||""}function d(e,t,n){try{e[t]=n}catch(e){}}function p(e){return this.__l[e.type](I.event&&I.event(e)||e)}function f(){for(var e;e=P.pop();)I.afterMount&&I.afterMount(e),e.componentDidMount&&e.componentDidMount()}function v(e,t,n,o,r,i){R++||(U=null!=r&&void 0!==r.ownerSVGElement,D=null!=e&&!("__preactattr_"in e));var s=g(e,t,n,o,i);return r&&s.parentNode!==r&&r.appendChild(s),--R||(D=!1,i||f()),s}function g(e,t,n,o,r){var i=e,s=U;if(null!=t&&"boolean"!=typeof t||(t=""),"string"==typeof t||"number"==typeof t)return e&&void 0!==e.splitText&&e.parentNode&&(!e._component||r)?e.nodeValue!=t&&(e.nodeValue=t):(i=document.createTextNode(t),e&&(e.parentNode&&e.parentNode.replaceChild(i,e),y(e,!0))),i.__preactattr_=!0,i;var l=t.nodeName;if("function"==typeof l)return C(e,t,n,o);if(U="svg"===l||"foreignObject"!==l&&U,l=String(l),(!e||!a(e,l))&&(i=c(l,U),e)){for(;e.firstChild;)i.appendChild(e.firstChild);e.parentNode&&e.parentNode.replaceChild(i,e),y(e,!0)}var u=i.firstChild,h=i.__preactattr_,d=t.children;if(null==h){h=i.__preactattr_={};for(var p=i.attributes,f=p.length;f--;)h[p[f].name]=p[f].value}return!D&&d&&1===d.length&&"string"==typeof d[0]&&null!=u&&void 0!==u.splitText&&null==u.nextSibling?u.nodeValue!=d[0]&&(u.nodeValue=d[0]):(d&&d.length||null!=u)&&m(i,d,n,o,D||null!=h.dangerouslySetInnerHTML),k(i,t.attributes,h),U=s,i}function m(e,t,n,o,r){var i,a,l,c,h,d=e.childNodes,p=[],f={},v=0,m=0,_=d.length,k=0,b=t?t.length:0;if(0!==_)for(L=0;L<_;L++){var x=d[L],w=x.__preactattr_;null!=(S=b&&w?x._component?x._component.__k:w.key:null)?(v++,f[S]=x):(w||(void 0!==x.splitText?!r||x.nodeValue.trim():r))&&(p[k++]=x)}if(0!==b)for(L=0;L<b;L++){h=null;var S=(c=t[L]).key;if(null!=S)v&&void 0!==f[S]&&(h=f[S],f[S]=void 0,v--);else if(!h&&m<k)for(i=m;i<k;i++)if(void 0!==p[i]&&s(a=p[i],c,r)){h=a,p[i]=void 0,i===k-1&&k--,i===m&&m++;break}h=g(h,c,n,o),l=d[L],h&&h!==e&&h!==l&&(null==l?e.appendChild(h):h===l.nextSibling?u(l):e.insertBefore(h,l))}if(v)for(var L in f)void 0!==f[L]&&y(f[L],!1);for(;m<=k;)void 0!==(h=p[k--])&&y(h,!1)}function y(e,t){var n=e._component;n?M(n):(null!=e.__preactattr_&&e.__preactattr_.ref&&e.__preactattr_.ref(null),!1!==t&&null!=e.__preactattr_||u(e),_(e))}function _(e){for(e=e.lastChild;e;){var t=e.previousSibling;y(e,!0),e=t}}function k(e,t,n){var o;for(o in n)t&&null!=t[o]||null==n[o]||h(e,o,n[o],n[o]=void 0,U);for(o in t)"children"===o||"innerHTML"===o||o in n&&t[o]===("value"===o||"checked"===o?e[o]:n[o])||h(e,o,n[o],n[o]=t[o],U)}function b(e){var t=e.constructor.name;(V[t]||(V[t]=[])).push(e)}function x(e,t,n){var o,r=V[e.name];if(e.prototype&&e.prototype.render?(o=new e(t,n),N.call(o,t,n)):((o=new N(t,n)).constructor=e,o.render=w),r)for(var i=r.length;i--;)if(r[i].constructor===e){o.__b=r[i].__b,r.splice(i,1);break}return o}function w(e,t,n){return this.constructor(e,n)}function S(e,t,n,o,i){e.__x||(e.__x=!0,(e.__r=t.ref)&&delete t.ref,(e.__k=t.key)&&delete t.key,!e.base||i?e.componentWillMount&&e.componentWillMount():e.componentWillReceiveProps&&e.componentWillReceiveProps(t,o),o&&o!==e.context&&(e.__c||(e.__c=e.context),e.context=o),e.__p||(e.__p=e.props),e.props=t,e.__x=!1,0!==n&&(1!==n&&!1===I.syncComponentUpdates&&e.base?r(e):L(e,1,i)),e.__r&&e.__r(e))}function L(e,t,n,r){if(!e.__x){var i,s,a,c=e.props,u=e.state,h=e.context,d=e.__p||c,p=e.__s||u,g=e.__c||h,m=e.base,_=e.__b,k=m||_,b=e._component,w=!1;if(m&&(e.props=d,e.state=p,e.context=g,2!==t&&e.shouldComponentUpdate&&!1===e.shouldComponentUpdate(c,u,h)?w=!0:e.componentWillUpdate&&e.componentWillUpdate(c,u,h),e.props=c,e.state=u,e.context=h),e.__p=e.__s=e.__c=e.__b=null,e.__d=!1,!w){i=e.render(c,u,h),e.getChildContext&&(h=o(o({},h),e.getChildContext()));var C,N,A=i&&i.nodeName;if("function"==typeof A){var O=l(i);(s=b)&&s.constructor===A&&O.key==s.__k?S(s,O,1,h,!1):(C=s,e._component=s=x(A,O,h),s.__b=s.__b||_,s.__u=e,S(s,O,0,h,!1),L(s,1,n,!0)),N=s.base}else a=k,(C=b)&&(a=e._component=null),(k||1===t)&&(a&&(a._component=null),N=v(a,i,h,n||!m,k&&k.parentNode,!0));if(k&&N!==k&&s!==b){var E=k.parentNode;E&&N!==E&&(E.replaceChild(N,k),C||(k._component=null,y(k,!1)))}if(C&&M(C),e.base=N,N&&!r){for(var T=e,j=e;j=j.__u;)(T=j).base=N;N._component=T,N._componentConstructor=T.constructor}}if(!m||n?P.unshift(e):w||(e.componentDidUpdate&&e.componentDidUpdate(d,p,g),I.afterUpdate&&I.afterUpdate(e)),null!=e.__h)for(;e.__h.length;)e.__h.pop().call(e);R||r||f()}}function C(e,t,n,o){for(var r=e&&e._component,i=r,s=e,a=r&&e._componentConstructor===t.nodeName,c=a,u=l(t);r&&!c&&(r=r.__u);)c=r.constructor===t.nodeName;return r&&c&&(!o||r._component)?(S(r,u,3,n,o),e=r.base):(i&&!a&&(M(i),e=s=null),r=x(t.nodeName,u,n),e&&!r.__b&&(r.__b=e,s=null),S(r,u,1,n,o),e=r.base,s&&e!==s&&(s._component=null,y(s,!1))),e}function M(e){I.beforeUnmount&&I.beforeUnmount(e);var t=e.base;e.__x=!0,e.componentWillUnmount&&e.componentWillUnmount(),e.base=null;var n=e._component;n?M(n):t&&(t.__preactattr_&&t.__preactattr_.ref&&t.__preactattr_.ref(null),e.__b=t,u(t),b(e),_(t)),e.__r&&e.__r(null)}function N(e,t){this.__d=!0,this.context=t,this.props=e,this.state=this.state||{}}var I={},A=[],O=[],E="function"==typeof Promise?Promise.resolve().then.bind(Promise.resolve()):setTimeout,T=/acit|ex(?:s|g|n|p|$)|rph|ows|mnc|ntw|ine[ch]|zoo|^ord/i,j=[],P=[],R=0,U=!1,D=!1,V={};o(N.prototype,{setState:function(e,t){var n=this.state;this.__s||(this.__s=o({},n)),o(n,"function"==typeof e?e(n,this.props):e),t&&(this.__h=this.__h||[]).push(t),r(this)},forceUpdate:function(e){e&&(this.__h=this.__h||[]).push(e),L(this,2)},render:function(){}});var F={h:n,createElement:n,cloneElement:function(e,t){return n(e.nodeName,o(o({},e.attributes),t),arguments.length>2?[].slice.call(arguments,2):e.children)},Component:N,render:function(e,t,n){return v(n,e,{},!1,t,!1)},rerender:i,options:I};void 0!==t?t.exports=F:self.preact=F}()},{}]},{},[3]);
//# sourceMappingURL=haddock-bundle.min.js.map
