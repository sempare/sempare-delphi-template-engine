<% include ('helper') %>

<% extends ("template") %>
        <% block "header" *%>
        <html>
                <head>
                        <title>Welcome to my <% Company %></title>
                </head>
                <body>
        <% end *%>

        <% block "body" %>
            <h1>Form</h1>
            <p>This is a sample form generated from templates defined in helper.tpl.</p>
            <p>You can submit the information on this form. The information will simply be echoed by the handler.<p>
            <% include("TForm") %>
        <% end %>

        <% block "footer" %>
                    <p>Copyright (c) <% CopyrightYear %> <% Company %></p>
                </body>
        </html>
        <% end %>
<% end %>
