-module(newrelic_plugin).

-export([push/3]).

-define(BASE_URL, "https://platform-api.newrelic.com/platform/v1/metrics").


push(Hostname, Metrics, Errors) ->

    % io:put_chars("--------------Metrics----------------\n"),
    % erlang:display(Metrics),

    % io:put_chars("--------------Errors------------------\n"),
    % erlang:display(Errors),

    % Body = test_data(),
    Body = construct_body(Metrics, Errors),

    Json = jsx:encode(Body),

    % io:put_chars("--------------Body-------------------\n"),
    % erlang:display(binary_to_list(Json)),

    case application:get_env(newrelic, license_keys) of
        {ok, Accounts} -> 
            push_to_account(Accounts, Json),
            ok;
        undefined ->
            {warning, "Newrelic configuration is missing license keys."}
    end.

construct_body(Metrics, Errors) ->
    ConvertedMetrics = convert_metrics(Metrics),
    ConvertedErrors = convert_errors(Errors),

    [
        {<<"agent">>, [
            {<<"host">>, <<"customer_service.bapi.onetechnologies.net">>},
            {<<"pid">>, list_to_integer(os:getpid())},
            {<<"version">>, <<"1.0.0">>}
        ]},
        {<<"components">>, ConvertedMetrics }
    ].

convert_metrics([]) -> 
    [];
convert_metrics([[{[{name,Name},{scope,_Scope}]},[_,_,_,_,_,_]]|T]) ->
    Metric = [
        {<<"name">>, Name},
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
    [Metric|convert_metrics(T)].

convert_errors([]) ->
    [];
convert_errors([H|T]) ->

    convert_errors(T).


push_to_account([], _) ->
    ok;
push_to_account([H|T], Json) ->
    Headers = [
      {"X-License-Key", H},
      {"Content-Type", "application/json"},
      {"Accept", "application/json"}
    ],

    Result = request(?BASE_URL, Headers, Json),

    io:put_chars("--------------Result-----------------\n"),
    erlang:display(Result),

    push_to_account(T, Json).


request(Url, Headers) ->
    request(Url, Headers, <<"[]">>).

request(Url, Headers, Json) ->
    % io:put_chars("Sending Request...\n"),
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