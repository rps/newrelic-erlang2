-module(newrelic_plugin).

-define(BASE_URL, "https://platform-api.newrelic.com/platform/v1/metrics").

push() ->
    ok.





request(Url) ->
    request(Url, <<"[]">>).

request(Url, Body) ->
    lhttpc:request(Url, post, [{"Content-Encoding", "identity"}], Body, 5000).


