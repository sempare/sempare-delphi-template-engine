<% template 'resolve_current_demo' %>
        <% for demo of _ %>
                <% if demo.Current %><% demo.name%><% break %><% end %>
        <% end %>
<% end %>

<% extends ( 'template' ) %>
        <%- block 'body' *%>
                <div>
                <img style="height: 45px; vertical-align: middle" src="https://www.sempare.ltd/assets/img/sempare-logo.png">
                <h1 style="display:inline; vertical-align: middle">Welcome to the <i>Sempare Template Engine Version</i> <% include('resolve_current_demo') %> Demo</h1>
                </div>
                <h1>Documentation</h1>
                <p>For more documentation on the <i>Sempare Template Engine Version</i>, please review the <a target="_blank" href="https://github.com/sempare/sempare-delphi-template-engine">GitHub repository</a>.</p>
                <p>This demo is using <i>Sempare Template Engine Version</i> <b><% SEMPARE_TEMPLATE_ENGINE_VERSION %></b>.</p>
                <h1>Demos</h1>
                <p><b>This</b> is the <b><% include('resolve_current_demo') %> Demo</b>.<p>
                <p>Also have a look at other demos, such as:
                        <% for demo of _ %>
                                <li><a target="_blank" href="<% demo.FrameworkUrl %>"><% demo.Name %></a> <a target="_blank" href="<% demo.Url %>">Demo</a><% if demo.Current %> <b>(Current)</b><% end %>
                        <% onbegin %>
                            <ul>
                        <% onend %>
                            </ul>
                        <% end %>
                </p>
                <h1>Functionality Demo</h1>
                <p>This is random functionality to illustrate the dynamic nature of the template engine.</p>
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
