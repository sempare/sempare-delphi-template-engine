<% extends ( 'template' ) %>
        <%- block 'body' *%>

                <p>
                Here are some numbers from 1 to 10:
                </p>
                <ul>
                <%- for i := 1 to 10 *%>
                        <li><% i %></li>
                <%- end *%>
                </ul>
                <a href="/form">example form</a>
                <a href="/random">random eror 404</a>
        <%- end *%>
<% end *%>
