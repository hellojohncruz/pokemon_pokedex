!function(n){var r={};function o(e){if(r[e])return r[e].exports;var t=r[e]={i:e,l:!1,exports:{}};return n[e].call(t.exports,t,t.exports,o),t.l=!0,t.exports}o.m=n,o.c=r,o.d=function(e,t,n){o.o(e,t)||Object.defineProperty(e,t,{enumerable:!0,get:n})},o.r=function(e){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},o.t=function(t,e){if(1&e&&(t=o(t)),8&e)return t;if(4&e&&"object"==typeof t&&t&&t.__esModule)return t;var n=Object.create(null);if(o.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:t}),2&e&&"string"!=typeof t)for(var r in t)o.d(n,r,function(e){return t[e]}.bind(null,r));return n},o.n=function(e){var t=e&&e.__esModule?function(){return e.default}:function(){return e};return o.d(t,"a",t),t},o.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},o.p="",o(o.s=66)}({0:function(e,t,n){"use strict";n.d(t,"d",function(){return r}),n.d(t,"c",function(){return o}),n.d(t,"a",function(){return i}),n.d(t,"b",function(){return c}),n.d(t,"q",function(){return u}),n.d(t,"k",function(){return a}),n.d(t,"m",function(){return l}),n.d(t,"j",function(){return d}),n.d(t,"i",function(){return s}),n.d(t,"l",function(){return f}),n.d(t,"f",function(){return g}),n.d(t,"g",function(){return p}),n.d(t,"h",function(){return b}),n.d(t,"n",function(){return h}),n.d(t,"o",function(){return v}),n.d(t,"p",function(){return m}),n.d(t,"e",function(){return y});var r=nnads.config,o=nnads.cmd,i=nnads.fn.CMPTool,c=nnads.fn.DOMReady,u=(nnads.fn.checkForMoat,nnads.fn.checkPermutive,nnads.fn.loadScript),a=(nnads.fn.loadJSON,nnads.fn.getBrowserWidth),l=nnads.fn.getHeight,d=nnads.fn.elementInViewport,s=nnads.fn.element50InViewport,f=(nnads.fn.percentageElementInView,nnads.fn.getCookie),g=(nnads.fn.setCookie,nnads.debug.debugAll),p=nnads.debug.debugging,b=nnads.debug.debuggingTest,h=nnads.debug.getflag,v=nnads.debug.info,m=nnads.debug.kdebug,y=nnads.debug.log},1:function(e,t,n){"use strict";n.d(t,"n",function(){return r}),n.d(t,"j",function(){return c.b}),n.d(t,"o",function(){return c.c}),n.d(t,"d",function(){return c.a}),n.d(t,"a",function(){return d}),n.d(t,"h",function(){return b}),n.d(t,"c",function(){return h}),n.d(t,"k",function(){return v}),n.d(t,"m",function(){return m}),n.d(t,"l",function(){return y}),n.d(t,"b",function(){return w}),n.d(t,"e",function(){return O}),n.d(t,"i",function(){return k}),n.d(t,"g",function(){return P}),n.d(t,"f",function(){return j});var i=n(2);function o(e,t){for(var n=0;n<t.length;n++){var r=t[n];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(e,r.key,r)}}var r=new(function(){function e(){if(!(this instanceof e))throw new TypeError("Cannot call a class as a function")}var t,n,r;return t=e,(n=[{key:"sideBisectsViewport",value:function(e,t,n,r,o,i,c,u,a,l){var d;return r=r,o=o,i=i,c=c,l=l,d=["top","left"].includes(e=e)&&i<0&&(l?u:a)<c,e=["right","bottom"].includes(e)&&c<0&&(l?u:a)<i,c={},d||e?(d={},0<=r&&r<=(l?a:u)||0<=o&&o<=(l?a:u)?(d.amt="some",d.result=!0):r<0&&(l?a:u)<o?(d.amt="all",d.result=!0):d.result=!1,d):(r<=0&&(l?a:u)<=o&&0<=i&&i<=(l?u:a)?(c.amt="some",c.result=!0):c.result=!1,c)}},{key:"calculateLengthInViewport",value:function(e,t,n,r,o,i,c,u,a,l){var d=l?a:u;if(t||n)return(n?o:0<o?d:0)-(t||0<r?r:0);var t=this.sideBisectsViewport(e,t,n,r,o,i,c,u,a,l);if(t.result){if("all"===t.amt)return d;if("some"===t.amt){n=r,i=o,c=u,l=a,d={adjusted:!1,length:0},["left","right"].includes(t=e)?(d.adjusted=!0,d.length=n<0?i:l-n):["top","bottom"].includes(t)&&(d.adjusted=!0,d.length=n<0?c<(l=n+i)?c:l:c-n);r=d;if(r.adjusted)return r.length}}return 0}},{key:"vertexInViewport",value:function(e,t,n,r,o){return 0<=e&&e<=(o?r:n)&&0<=t&&t<=(o?n:r)}},{key:"sideInViewport",value:function(e,t,n,r,o,i,c){var u=!(7<arguments.length&&void 0!==arguments[7])||arguments[7],a=this.vertexInViewport(t,r,i,c,u),l=this.vertexInViewport(n,r,i,c,u);return{lowerPointView:a,higherPointInView:l,lengthOfSideInView:this.calculateLengthInViewport(e,a,l,t,n,r,o,i,c,u)}}},{key:"calculateAreaInView",value:function(e,t,n,r){function o(e,t){return e||t||0}return o(r,t)*o(e,n)}},{key:"calculatePercentageInView",value:function(e,t,n){return e/(t*n)*100}},{key:"elementInViewport",value:function(e){var e=e.getBoundingClientRect(),t=window.innerWidth,n=window.innerHeight,r=e.top,o=e.top+e.height,i=e.left,c=e.left+e.width,u=this.sideInViewport("top",i,c,r,o,t,n,!1),a=this.sideInViewport("right",r,o,c,i,t,n,!0),l=this.sideInViewport("bottom",i,c,o,r,t,n,!1),r=this.sideInViewport("left",r,o,i,c,t,n,!0),o=this.calculateAreaInView(u.lengthOfSideInView,a.lengthOfSideInView,l.lengthOfSideInView,r.lengthOfSideInView);return this.calculatePercentageInView(o,e.width,e.height)}}])&&o(t.prototype,n),r&&o(t,r),Object.defineProperty(t,"prototype",{writable:!1}),e}()),c=n(3);function u(e){return(u="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(e){return typeof e}:function(e){return e&&"function"==typeof Symbol&&e.constructor===Symbol&&e!==Symbol.prototype?"symbol":typeof e})(e)}function a(e,t){for(var n=0;n<t.length;n++){var r=t[n];r.enumerable=r.enumerable||!1,r.configurable=!0,"value"in r&&(r.writable=!0),Object.defineProperty(e,r.key,r)}}var l=window,d=function(){function e(){if(!(this instanceof e))throw new TypeError("Cannot call a class as a function");this.debug=i.a,this.loaded=!1,this.closed=!1,this.mobileWidth=480,this.timeout=null,this.fallbackCalled=!1,this.fallbackTimeout=2e3,this.callback=function(){return!1}}var t,n,r;return t=e,(n=[{key:"getVersion",value:function(){var e="function"==typeof l.__cmp,t="function"==typeof l.__tcfapi,n="function"==typeof l.__uspapi;return e&&!t?"cmp":t?"tcfapi":n&&"uspapi"}},{key:"getConsentString",value:function(){var n=null;return l.__tcfapi&&l.__tcfapi("getTCData",2,function(e,t){n=e}),n&&n.tcString?n.tcString:null}},{key:"getConsent",value:function(e){if(!this.getVersion())throw"CMPTool: No valid CMP";if(!e)throw"CMPTool: No vendor ID set";var t,n=null;switch(this.getVersion()){case"cmp":(n=l.__cmp("getCMPData"))&&n.vendorConsents&&(t=n.vendorConsents);break;case"tcfapi":l.__tcfapi("getTCData",2,function(e,t){n=e}),n&&"vendor"in n&&"consents"in n.vendor&&(t=n.vendor.consents)}var r=!1;return(t=!n&&l.__cmp&&(n=l.__cmp("getCMPData"))&&n.vendorConsents?n.vendorConsents:t)&&e in t?r=t[e]:n&&"googleVendorConsents"in n&&e in n.googleVendorConsents?r=n.googleVendorConsents[e]:n&&"customVendorConsents"in n&&e in n.customVendorConsents&&(r=n.customVendorConsents[e]),"object"===u(n)&&null!==n&&n&&"gdprApplies"in n&&n.gdprApplies?r:null}},{key:"fallback",value:function(){var e=this;this.loaded||(0<arguments.length&&void 0!==arguments[0]&&arguments[0]?(this.debug("CMP // %cFallback timeout reached! Executing callback!","font-weight:bold"),this.callback(),this.fallbackCalled=!0,this.callback=function(){return!1}):(this.debug("CMP // %cSetting fallback timeout...","font-weight:bold"),this.timeout=setTimeout(function(){e.fallback(!0)},this.fallbackTimeout)))}},{key:"removeListener",value:function(t){var n=this;l.__tcfapi("removeEventListener",2,function(e){e&&n.debug("CMP listener removed. Id:",t)},t)}},{key:"ready",value:function(n){var r=this;if(this.callback=n,this.fallbackCalled=!1,!l.__tcfapi)return this.debug("CMP // %cNOT FOUND. Executing callback!","font-weight:bold"),n();this.fallback(),l.__tcfapi("addEventListener",2,function(e,t){if(r.debug("CMP call",t,e),r.fallbackCalled)return r.debug("CMP fallback has already occurred. Removing listener."),r.removeListener(e.listenerId);if(t){if("loaded"===e.cmpStatus&&!1===e.gdprApplies)return clearTimeout(r.timeout),r.debug("CMP call 2"),r.removeListener(e.listenerId),n();switch(e.eventStatus){case"useractioncomplete":case"tcloaded":clearTimeout(r.timeout),r.debug("CMP call 1"),n(),r.removeListener(e.listenerId);break;case"cmpuishown":clearTimeout(r.timeout)}}})}}])&&a(t.prototype,n),r&&a(t,r),Object.defineProperty(t,"prototype",{writable:!1}),e}();function s(t,e){var n,r=Object.keys(t);return Object.getOwnPropertySymbols&&(n=Object.getOwnPropertySymbols(t),e&&(n=n.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),r.push.apply(r,n)),r}function f(r){for(var e=1;e<arguments.length;e++){var o=null!=arguments[e]?arguments[e]:{};e%2?s(Object(o),!0).forEach(function(e){var t,n;t=r,n=o[e=e],e in t?Object.defineProperty(t,e,{value:n,enumerable:!0,configurable:!0,writable:!0}):t[e]=n}):Object.getOwnPropertyDescriptors?Object.defineProperties(r,Object.getOwnPropertyDescriptors(o)):s(Object(o)).forEach(function(e){Object.defineProperty(r,e,Object.getOwnPropertyDescriptor(o,e))})}return r}var g=window,p=document,b=(p.getElementById.bind(p),[].concat(["AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GI","GR","HR","HU","IE","IS","IT","LI","LT","LU","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK"],["US","CH"])),h={US:["CA"]};function v(){return Math.max(p.body.scrollHeight,p.documentElement.scrollHeight,p.body.offsetHeight,p.documentElement.offsetHeight,p.documentElement.clientHeight)}function m(e){var t,n,r=1<arguments.length&&void 0!==arguments[1]?arguments[1]:{},r=f(f({},{async:!0,defer:!1,callback:null,injectOnce:!0,debug:null}),r),o=p.createElement("script");o.src=e,o.async=!!r.async,o.defer=!!r.defer,t="nn-"+function(e){var t,n=0;if(0===e.length)return n;for(t=0;t<e.length;t++)n=(n<<5)-n+e.charCodeAt(t),n|=0;return Math.abs(n).toString(16)}(e),o.id=t,null==r.debug&&(r.debug=i.a),!1!==r.injectOnce&&p.getElementById(t)?r.debug("%c(WARN) Script Already Present: %c%s","color:red","font-weight:bold",e,t):(o.onload=function(){return r.debug("Script Loaded: %c%s","font-weight:bold",e,t)},"function"==typeof r.callback&&(o.onload=r.callback.call(this,e)),(n=p.getElementsByTagName("script")[0]).parentNode.insertBefore(o,n))}function y(e,t){var n,r,o=2<arguments.length&&void 0!==arguments[2]?arguments[2]:null;if(g.fetch&&"function"==typeof AbortController)return n=new AbortController,o&&(r=setTimeout(function(){n.abort()},o)),fetch(e,{signal:n.signal}).then(function(e){return clearTimeout(r),e.ok?e.json():Promise.reject(e)}).then(function(e){t(e)}).catch(function(e){t(e),console.error("nnAds:"+e)});var i=new XMLHttpRequest;i.overrideMimeType("application/json"),i.open("GET",e,!0),o&&(i.timeout=o),i.onerror=function(){t(new Error("Error occurred while fetching data"))},i.onreadystatechange=function(){4===i.readyState&&(200<=i.status&&i.status<300?t(JSON.parse(i.responseText)):(console.log("error",i),t(new Error("State Ready: Error occurred while fetching data"))))},i.send(null)}function w(e){"loading"!==p.readyState?e():p.addEventListener("DOMContentLoaded",function(){e()})}function O(){var e=0<arguments.length&&void 0!==arguments[0]?arguments[0]:"",t=p.createElement("img");t.width=1,t.height=1,t.style.display="none",t.src=nnads.script.domain+"/images/blank.png",t.src+=e?"?nnbranch=".concat(e):"?expire",w(function(){p.body.appendChild(t)})}function k(){return Math.max(g.innerWidth||p.documentElement.clientWidth||p.body.clientWidth)}function P(e){e=e.getBoundingClientRect();return e.top<window.innerHeight&&e.left<window.innerWidth&&0<e.top+e.height&&0<e.left+e.width}function j(e){var t=1<arguments.length&&void 0!==arguments[1]?arguments[1]:50;return 100===t?P(e):t<=r.elementInViewport(e)}},2:function(e,t,n){"use strict";n.d(t,"e",function(){return u}),n.d(t,"c",function(){return r}),n.d(t,"b",function(){return l}),n.d(t,"d",function(){return o}),n.d(t,"a",function(){return i}),n.d(t,"f",function(){return c}),n.d(t,"g",function(){return d});var t=n(3),u=function(e){var t=1<arguments.length&&void 0!==arguments[1]&&arguments[1],n=-1<window.location.search.indexOf(e);return t?new URLSearchParams(window.location.search).get(e):n},n=(u("nndebug=1")&&Object(t.c)("nndebug","1",{"max-age":3600}),u("nndebug=0")&&Object(t.c)("nndebug","",{"max-age":0}),"1"===Object(t.b)("nndebug"));function a(){for(var e,t=0<arguments.length&&void 0!==arguments[0]?arguments[0]:{},n=void 0===t.title?"":t.title,r=void 0===t.style?"font-weight:bold;font-size:13px;color:green":t.style,t=void 0===t.dlevel?"log":t.dlevel,o=arguments.length,i=new Array(1<o?o-1:0),c=1;c<o;c++)i[c-1]=arguments[c];var u="string"==typeof i[0]?" "+i.shift():"";(e=console)[t].apply(e,["%c"+n+"%c"+u,r,""].concat(i))}var r=u("nndebug"),l=r&&!u("nndebug="),o=(l=l||n,u("nntest"));function i(){for(var e=arguments.length,t=new Array(e),n=0;n<e;n++)t[n]=arguments[n];l&&a.apply(void 0,[{title:"nn__DEBUG:"}].concat(t))}function c(e){for(var t=arguments.length,n=new Array(1<t?t-1:0),r=1;r<t;r++)n[r-1]=arguments[r];a.apply(void 0,[{title:e,dlevel:"info",style:"font-weight:bold;color:blue"}].concat(n))}function d(e){for(var t=1<arguments.length&&void 0!==arguments[1]?arguments[1]:{},n=void 0===t.title?"":t.title,t=void 0===t.style?"font-weight:bold;font-size:13px;color:green":t.style,r=u(e),o=arguments.length,i=new Array(2<o?o-2:0),c=2;c<o;c++)i[c-2]=arguments[c];(r||l)&&a.apply(void 0,[{title:n||e.toUpperCase(),style:t}].concat(i))}i("Version:","1.26.3")},3:function(e,t,n){"use strict";function i(t,e){var n,r=Object.keys(t);return Object.getOwnPropertySymbols&&(n=Object.getOwnPropertySymbols(t),e&&(n=n.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),r.push.apply(r,n)),r}function o(r){for(var e=1;e<arguments.length;e++){var o=null!=arguments[e]?arguments[e]:{};e%2?i(Object(o),!0).forEach(function(e){var t,n;t=r,n=o[e=e],e in t?Object.defineProperty(t,e,{value:n,enumerable:!0,configurable:!0,writable:!0}):t[e]=n}):Object.getOwnPropertyDescriptors?Object.defineProperties(r,Object.getOwnPropertyDescriptors(o)):i(Object(o)).forEach(function(e){Object.defineProperty(r,e,Object.getOwnPropertyDescriptor(o,e))})}return r}n.d(t,"b",function(){return r}),n.d(t,"c",function(){return u}),n.d(t,"a",function(){return a});var c=document;function r(e){e=c.cookie.match("(^|[^;]+)\\s*"+e+"\\s*=\\s*([^;]+)");return e?e.pop():null}function u(e,t){var n=2<arguments.length&&void 0!==arguments[2]?arguments[2]:{},r=(r=(r=(r="")+(e+"="+t)+(";path="+(n=o(o({},{path:"/","max-age":60,samesite:"Lax",secure:!0}),n)).path))+(";max-age="+n["max-age"]))+(";samesite="+n.samesite);n.secure&&(r+=";secure"),c.cookie=r}function a(e){u(e,null,{"max-age":-1})}},66:function(e,t,n){e.exports=n(67)},67:function(e,t,n){"use strict";n.r(t);function r(){for(var e=arguments.length,t=new Array(e),n=0;n<e;n++)t[n]=arguments[n];return o.p.apply(void 0,["nndebug=pubstasck",{title:"nn__PUBSTACK:",style:"font-weight:bold;font-size:13px;color:#8b69ac;"}].concat(t))}var o=n(0),i=n(1);o.c.push("pubstack",function(){var e;"pubstack"in o.d.modules&&((e=(null==(e=o.d.modules.pubstack)?void 0:e.id)||null)&&Object(i.m)("https://boot.pbstck.com/v1/tag/"+e,{debug:r}))})}});