-module(newrelic_plugin).

-export([push/3]).

-define(BASE_URL, "https://platform-api.newrelic.com/platform/v1/metrics").

push(Hostname, Metrics, Errors) ->
    io:put_chars("Pushing data to new relic plugin api (not really... but some day)\n"),

    Headers = [
      {"X-License-Key", "<< license key goes here >>"},
      {"Content-Type", "application/json"},
      {"Accept", "application/json"}
    ],

    Body = test_data(),
    
    Result = request(?BASE_URL, Headers, Body),

    io:put_chars("--------------Result-----------------\n"),
    erlang:display(Result),

    ok.


request(Url, Headers) ->
    request(Url, Headers, <<"[]">>).

request(Url, Headers, Body) ->
    io:put_chars("--------------Headers----------------\n"),
    erlang:display(Headers),

    Json = jsx:encode(Body),

    io:put_chars("--------------Body-------------------\n"),
    erlang:display(binary_to_list(Json)),

    io:put_chars("Sending Request...\n"),
    lhttpc:request(Url, post, Headers, Json, 5000).

test_data() ->
    Data = 
        [
            {<<"agent">>, [
                {<<"host">>, <<"customer_service.bapi.onetechnologies.net">>},
                {<<"pid">>, 1234},
                {<<"version">>, <<"1.0.0">>}
            ]},
            {<<"components">>, [
                [
                    {<<"name">>, <<"CustomerServiceBapi">>},
                    {<<"guid">>, <<"net.onetechnologies.CustomerServiceTest">>},
                    {<<"duration">>, 60},
                    {<<"metrics">>, [
                        {<<"Component/ProductionDatabase[Queries/Second]">>, 100},
                        {<<"Component/AnalyticsDatabase[Queries/Second]">>, [
                            {<<"min">>, 2},
                            {<<"max">>, 10},
                            {<<"total">>, 12},
                            {<<"count">>, 2},
                            {<<"sum_of_squares">>, 104}
                        ]}
                    ]}
                ],
                [
                    {<<"name">>, <<"CustomerServiceBapi2">>},
                    {<<"guid">>, <<"net.onetechnologies.CustomerServiceTest">>},
                    {<<"duration">>, 60},
                    {<<"metrics">>, [
                        {<<"Component/GetCallNotes[Queries/Second]">>, [
                            {<<"min">>, 2},
                            {<<"max">>, 10},
                            {<<"total">>, 12},
                            {<<"count">>, 2},
                            {<<"sum_of_squares">>, 104}
                        ]}
                    ]}
                ]
            ]}
        ].
