<% extends ("template") %>

        <% block "body" %>

        <p>The info:</p>

        <table>
                <tr><th>First Name</th><td><% firstname %></td></tr>
                <tr><th>Last Name</th><td><% lastname %></td></tr>
                <tr><th>E-mail</th><td><% FormDecode(email) %></td></tr>
        </table>

        <% end %>

<% end %>
