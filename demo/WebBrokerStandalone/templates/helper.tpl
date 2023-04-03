<% template "TEdit" %><tr><td><% Caption %></td><td><input name="<% name %>"></td></tr><% end %>

<% template "TEmail" %><tr><td><% Caption %></td><td><input type="email" name="<% name %>"></td></tr><% end %>

<% template "TButton" %><input type="button" name="<% name %>" value="<% caption %>"><% end %>

<% template "TResetButton" %><input type="reset" name="<% name %>" value="<% caption %>"><% end %>

<% template "TSubmitButton" %><input type="submit" name="<% name %>" value="<% caption %>"><% end %>

<% template "TForm" %>
<h1><% Title %></h1>
<form method="POST" name="<% FormName %>" action="<% FormAction %>">
  <table>
     <% for field of fields %>
         <% include(field.FieldType, field)%>
     <% end %>
     <tr>
         <td colspan="2" align="right">
           <% for button of buttons %>
             <% include(button.FieldType, button) %>
           <% end %>
         </td>
     </tr>
  </table>
</form>
<% end %>