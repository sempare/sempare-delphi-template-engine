<% if firstname %>
 	hello <% firstname %>.
<% end %> 
      
<% ignorenl %>
<% for i := 1 to 10 %><% if i mod 2 = 1 %><% continue %><% end %><% i %> <% end %>
<% end %>      
<% if lastname %>	Your last name is <% lastname %>.<% end %>

	The date is <% fmtdt('yyyy-mm-dd HH:MM:SS', dtnow()) %>.

<% if firstname = 'conrad' %>	You are the author. <% end %>

<% script %>
<br>
<br>
      
<% ignorenl %>
    <% v := '#' %>
    <% size := 5 %>
    <% for j := 1 to size %>
        <% for i := j to (size -1)*3 %>#<% end %>
             <% v := v + '**' %><% v %>
        <% for i  := j to (size -1)*3 %>#<% end %><br>
    <% end %>
<% end %>