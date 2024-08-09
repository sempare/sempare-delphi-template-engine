<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Invoice</title>
</head>
<body>
        <h1 style="text-align: center;">Invoice</h1>
        <div>
                <table style="margin: auto" >
                        <tr>
                                <td style="width: 300px">
                                         <img style="max-width: 100%; height: auto;" src="file:///<% invoice.LogoPath %>" >
                                </td>
                                <td style="width: 100px">
                                </td>
                                <td style="width: 600px" >
                                       <h3><% invoice.Company %></h3>

                                        <p><% invoice.CompanyAddress1 %></p>
                                        <p><% invoice.CompanyAddress2 %></p>
                                        <p>Company No:<% invoice.CompanyNo %></p>
                                </td>
                        </tr>
                </table>
        </div>

        <div style="height: 100px">
        </div>

        <div>
                <table  style="margin: auto"  >
                        <tr>
                                <td>
                                        <b>Invoice Date:</b> <% fmtdt('yyyy-mm-dd',invoice.InvoiceDate) %>
                                </td>
                                <td style="width: 300px"> </td>
                                <td>
                                        <b>Invoice No:</b> <% invoice.InvoiceNo %>
                                </td>
                        </tr>
                </table>

        </div>

        <div style="height: 100px">
        </div>

        <div style="text-align: center">
                <b>Client:</b> <b><% invoice.Client %></b>, <% invoice.ClientAddress1 %>, <% invoice.ClientAddress2 %>
        </div>

          <div style="height: 100px">
        </div>

        <table  style="margin: auto; width: 800px"  class="invoice-table">
            <thead>
                <tr>
                    <th>Description</th>
                    <th>Details</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>Billing Period</td>
                    <td style="text-align: right"><% fmtdt('yyyy-mm-dd', invoice.BillingPeriodStart) %> to <% fmtdt('yyyy-mm-dd', invoice.BillingPeriodEnd) %></td>
                </tr

                <tr>
                    <td>Worked Hours</td>
                    <td style="text-align: right"><% fmt('%8.2f', invoice.HoursWorked) %></td>
                </tr>
                <tr>
                    <td>Hourly Rate</td>
                    <td style="text-align: right"><% fmt('%8.2f', invoice.HourlyRate) %></td>
                </tr>
                <tr>
                    <td>Work Total</td>
                    <td style="text-align: right"><% fmt('%8.2f', invoice.WorkTotal()) %></td>
                </tr>
                <tr>
                    <td>Expenses Total</td>
                    <td style="text-align: right"><% fmt('%8.2f', invoice.Expenses) %></td>
                </tr>
                <tr>
                    <td><b>Total<b></td>
                    <td style="text-align: right"><b><% fmt('%8.2f', invoice.Total()) %></b></td>
                </tr>
            </tbody>
        </table>


        <div style="margin: 0 auto; text-align: center; width: fit-content;">

                <h3>Banking Details</h3>
            <p><b><% invoice.Bank %></b></p>
            <p><% invoice.BankDetails1 %></p>
            <p><% invoice.BankDetails2 %></p>

        </div>

        <div style="height: 100px">
        </div>


        <div style="margin: 0 auto; text-align: center; width: fit-content;">
            <p>Thank you for your business!</p>
        </div>
    </div>
    <script>
        function printPage() {
                window.print();
        }
    </script>
</body>
</html>
