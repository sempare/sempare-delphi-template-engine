      
<% ignorenl %>
    <% v := '#' %>
    <% size := 5 %>
    <% for j := 1 to size %>
        <% for i := j to (size -1)*3 %>#<% end %>
             <% v := v + '**' %><% v %>
        <% for i  := j to (size -1)*3 %>#<% end %><br>
    <% end %>
<% end %>