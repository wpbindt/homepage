** 18-8-2022

Apparently two key conditions REST APIs are supposed to satisfy is that they be hypermedia-driven and self-describing. In particular, a typical response might look like
><html>
>    <body>
>        <div>Account number: 12345</div>
>        <div>Balance: $100.00 USD</div>
>        <div>Links:
>            <a href="/accounts/12345/deposits">deposits</a>
>            <a href="/accounts/12345/withdrawals">withdrawals</a>
>            <a href="/accounts/12345/transfers">transfers</a>
>            <a href="/accounts/12345/close-requests">close-requests</a>
>        </div>
>    <body>
></html>
The idea is that the response contains all possible next endpoints which might be called (this is what is meant by being self-describing).
For example, if the balance in the example above would drop below 0, the response would no longer include a link to the withdrawals endpoint.

This means that the APIs that are usually sold as REST APIs, namely the ones returning json objects, violate these two conditions, and are therefore not REST APIs.
They are better described as JSON-based RPC.

