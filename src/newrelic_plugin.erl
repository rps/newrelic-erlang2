-module(newrelic_plugin).

-define(BASE_URL, "https://platform-api.newrelic.com/platform/v1/metrics").

push(Hostname, Metrics, Errors) ->
    io:put_chars("Pushing data to new relic plugin api (not really... but some day)\n"),

    io:put_chars("--------------Metrics----------------"),
    erlang:display(Metrics),
    io:put_chars("--------------Errors-----------------"),
    erlang:display(Errors),
    ok.


request(Url) ->
    request(Url, <<"[]">>).

request(Url, Body) ->
    lhttpc:request(Url, post, [{"Content-Encoding", "identity"}], Body, 5000).


