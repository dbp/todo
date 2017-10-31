<!DOCTYPE html>
<html>
  <head>
    <title>LogTodo</title>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1, minimum-scale=1">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <link rel="stylesheet" href="/static/6/css/main.css"/>
    <link rel="stylesheet" href="/static/css/pickadate.css"/>
    <link rel="stylesheet" href="/static/css/pickadate.date.css"/>
    <link rel="stylesheet" href="/static/css/pickadate.time.css"/>
    <link rel="icon" type="image/png" href="/static/icon.png">
    <link rel="apple-touch-icon" href="/static/icon.png">
    <link rel="apple-touch-startup-image" href="/static/launch.png">
    <script type="text/javascript">
     // from https://gist.github.com/kylebarrow/1042026#gistcomment-37145
     (function(document,navigator,standalone) {
       // prevents links from apps from oppening in mobile safari
       // this javascript must be the first script in your <head>
       if ((standalone in navigator) && navigator[standalone]) {
         var curnode, location=document.location, stop=/^(a|html)$/i;
         document.addEventListener('click', function(e) {
           curnode=e.target;
           while (!(stop).test(curnode.nodeName)) {
             curnode=curnode.parentNode;
           }
           // Conditions to do this only on links to your own app
           // if you want all links, use if('href' in curnode) instead.
           if('href' in curnode && ( curnode.href.indexOf('http') || ~curnode.href.indexOf(location.host) ) ) {
             e.preventDefault();
             location.href = curnode.href;
           }
         },false);
       }
     })(document,window.navigator,'standalone');
    </script>
  </head>
  <body>

    <apply-content/>

  </body>
  <script type="text/javascript"  src="/static/js/jquery-3.2.1.min.js"></script>
  <script type="text/javascript"  src="/static/js/picker.js"></script>
  <script type="text/javascript"  src="/static/js/picker.date.js"></script>
  <script type="text/javascript"  src="/static/js/picker.time.js"></script>

  <script type="text/javascript" src="/static/1/app.js"></script>
</html>



