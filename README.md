
socks [![Build Status](https://travis-ci.org/henglinli/socks.svg?branch=master)](https://travis-ci.org/henglinli/socks) 
=====

An SOCKS5 OTP application

Build
-----

    $ rebar3 compile
    
    
Info
------
Need inet:tcp_module/2, so require OTP-19.0 or above.

Only ipv4 CONNECT.

support user/password auth, see [socks.app.src commented line][line].

Listen at port 1080 and {active, 32}.

[line]: https://github.com/henglinli/socks/blob/master/apps/socks/src/socks.app.src#L12
