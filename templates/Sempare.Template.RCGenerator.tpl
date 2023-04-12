<%- for filename in files *%>
<% RCName(filename) %> RCDATA "<% EscapeBackslash(filename) %>"
<%- end *%>
