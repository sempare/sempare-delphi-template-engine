<% extends 'layout' %>
        <% block 'header' %>
                <title>The high scores for the Dev Days 2024 demo</title>
        <% end %>
        <% block 'body' %>
                <h1>High Scores</h1>

                <% for person of HighScores %>
                        <tr>
                                <td><% JoinNames(person) %></td>
                                <td><% person.score %></td>
                        </tr>
                        <% onbegin %>
                            <table>
                        <% onend %>
                             </table>
                        <% onempty %>

                         There are no high scores available.
                <% end %>
        <% end %>


<% end %>
