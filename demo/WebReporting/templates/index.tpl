
<h1>High Scores</h1>




<% for person of HighScores %>
        <tr>
                <td><% person.firstname %></td>
                <td><% person.lastname %></td>
                <td><% person.score %></td>
        </tr>
        <% onbegin %>
            <table>
        <% onend %>
             </table>
        <% onempty %>

         There are no high scores available.
<% end %>



