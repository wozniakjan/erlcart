erlcart
=======

This effort aims to create convenient support for Erlang web server for redhat's OpenShift. Development is easier due to OpenShift's scaling, automated builds and deployment but also limits the structure and usage of application.

Exhaustive documentation on www.openshift.com provides essential information for usage and deployment. Few additional steps required by erlang cartridge are described below.

What this cartridge currently offers?
-------------------------------------
- Erlang/OTP 17R01 - pre-build
- rebar 2.0.0 - tool for application build

What it supports?
-----------------
Currently cartridge has been tested with [Cowboy web server](https://github.com/ninenines/cowboy), thanks to uniform OTP development and file structure, it is assumed that any web server following OTP principles is supported as well.

How do I create the application?
--------------------------------
Create the application in [OpenShift](www.openshift.com), add [manifest](https://raw.githubusercontent.com/wozniakjan/erlcart/master/metadata/manifest.yml) from this repository, clone applications repository and push your OTP application's code. The cartridge will take care of automated build and deployment. In case you are not familiar with Cowboy and web development in OTP, there is barebone app created for your convenience in
[/template/test\_app](#todo).

Are there any examples running around?
--------------------------------------
Apart from others, there are two apps running:

1) [test](http://mynewcart-wozj.rhcloud.com/) application from /template/test\_app


2) [camchat](#todo) HTML5 based web video conference (which is more proof of concept than actuall application, work is in progress and far from being finished, but hopefully testable)

That's it?
----------
yep.
