rebar3_reloader
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_reloader, {git, "https://host/user/rebar3_reloader.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_reloader
    ===> Fetching rebar3_reloader
    ===> Compiling rebar3_reloader
    <Plugin Output>
