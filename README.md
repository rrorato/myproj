myproj
=====

An OTP application

Build
-----

    $ rebar3 compile


Run
----
    $ cd .\_build\default\lib\myproj\ebin\
    $ erl
    1> myproj_app:hello().
    2> q().

Release
----

    $ rebar3 new release myrel
    $ rebar3 release
