<% include ('helper') %>

<% extends ("template") %>
        <% block "header" *%>
        <html>
                <head>
                        <title>Welcome to my <% Company %></title>
                </head>
                <body>
        <% end *%>

        <% block "body" %><% include("TForm") %><% end %>

        <% block "footer" %>
                    <p>Copyright (c) <% CopyrightYear %> <% Company %></p>
                </body>
        </html>
        <% end %>
<% end %>
