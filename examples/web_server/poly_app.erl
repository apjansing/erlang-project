-module(poly_app).
-export([start/0, service/3]).

start() -> 
    inets:start(httpd, [ 
      {modules, [ 
         mod_alias, 
         mod_auth, 
         mod_esi, 
         mod_actions, 
         mod_cgi, 
         mod_dir,
         mod_get,
         mod_head, 
         mod_log, 
         mod_disk_log 
      ]}, 
      
      {port,8081}, 
      {server_name,"poly_app"}, 
      {server_root,"./"}, 
      {document_root,"./htdocs"}, 
      {erl_script_alias, {"/erl", [poly_app]}}, 
      {error_log, "error.log"}, 
      {security_log, "security.log"}, 
      {transfer_log, "transfer.log"}, 
      
      {mime_types,[ 
         {"html","text/html"}, {"css","text/css"}, {"js","application/x-javascript"} ]} 
   ]). 

service(SessionID, _Env, _Input) -> mod_esi:deliver(SessionID, [ 
   "Content-Type: text/html\r\n\r\n", 
   "<html  lang=\"en\">
      <head>
         <meta charset=\"UTF-8\">
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
         <meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\">
         <title>Alexander Jansing</title>
      </head>
      <script>
         function includeHTML() {
         var z, i, elmnt, file, xhttp;
         /* Loop through a collection of all HTML elements: */
         z = document.getElementsByTagName(\"*\");
         for (i = 0; i < z.length; i++) {
            elmnt = z[i];
            /*search for elements with a certain atrribute:*/
            file = elmnt.getAttribute(\"w3-include-html\");
            if (file) {
               /* Make an HTTP request using the attribute value as the file name: */
               xhttp = new XMLHttpRequest();
               xhttp.onreadystatechange = function() {
               if (this.readyState == 4) {
                  if (this.status == 200) {elmnt.innerHTML = this.responseText;}
                  if (this.status == 404) {elmnt.innerHTML = \"Page not found.\";}
                  /* Remove the attribute, and call this function once more: */
                  elmnt.removeAttribute(\"w3-include-html\");
                  includeHTML();
               }
               }
               xhttp.open(\"GET\", file, true);
               xhttp.send();
               /* Exit the function: */
               return;
            }
         }
         }
      </script>


      <body>
         <div w3-include-html=\"../index.html\"></div>
         <script>
         includeHTML();
         </script>   
      </body>
   </html>" ]).

% http://erlang.org/doc/man/mod_esi.html
% mongodb(SessionID, _Env, _Input) -> mod_esi:deliver(SessionID, []).