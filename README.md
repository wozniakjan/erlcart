erlcart feat. Phoenix
=====================

The support for [phoenix web server](https://github.com/phoenixframework/phoenix), is rather experimental. Due to couple of problems and limitations, the deployment is not fully automatic. The limitations are:

- Phoenix and its dependencies take very long time to build
- some of the prebuilt binaries consist of absolute paths, hence cross compilation and deployment are not possible
- OpenShift has many timeouts, some configurable, some unfortunatelly not
- the deployment usually times out and requires manual actions to finish

#### How do I create the application?

Create the application in [OpenShift](www.openshift.com), add [manifest](https://raw.githubusercontent.com/wozniakjan/erlcart/phoenix/metadata/manifest.yml) from this repository, clone applications repository and push your OTP application's code. The cartridge will take care of automated build and deployment. In case you are not familiar with phoenix and web development in OTP, there is barebone app created for your convenience in
[/template/](https://github.com/wozniakjan/erlcart/tree/phoenix/template).

    rhc create-app erlapp https://raw.githubusercontent.com/wozniakjan/erlcart/phoenix/metadata/manifest.yml

- then ssh to your app
- try to build the app few times
- restart the app

#### Are there any examples running around?

soon will be
