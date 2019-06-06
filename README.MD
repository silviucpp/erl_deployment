erl_deployment
=================

*Erlang tiny framework for template configs and deb/rpm packages*

# Overview

This is a very small framework based on `ant` scripts and `fpm` which allows you to:

- package your erlang release into a deb or rpm package (currently only deb is is implemented)
- define `sys.config` as a template with variables which have different values based on the environment where you deploy
- supports hot-swapping the changed modules when updating the deb package

# Dependencies

In order to use this you need to install [ant][1] and [fpm][2] and your erlang app should use `rebar3`:

### Mac OS

```bash
brew install ant gnu-tar
gem install --no-ri --no-rdoc fpm
```

### Debian like systems (Debian, Ubuntu, etc):

```bash
apt-get install ruby ruby-dev rubygems build-essential
gem install --no-ri --no-rdoc fpm
```

# Integration into a new app

In order to integrate this into your app add it in `rebar.config` as dependency.

Create into your project root a subfolder and add there:

- `setup.properties` : inside this file you will put the minimum required properties or override default one
- the `app.config` template (mandatory to keep this name), and also all other config files for each app included in the
rebar dependencies (in case you want to keep those separate).
- the `app.properties` - the file where you will define all the values of the variables from your `app.config` template
- `vm.args` - the vm.args file template

Also in your project root you need to have the `build.xml` template.

A minimum config for the above files :

#### build.xml

This file is the main ant script, used to include the other scripts which are part of `erl_deployment`.
Basically this should sit into the root of your project in order to be able to run from there all `ant` commands.
But you can put it anywhere you want.

```xml
<?xml version="1.0" encoding="utf-8"?>
<project name="Build app">
    <property file="deployment/setup.properties"/>
    <import file="${project.deployment_scripts}/build.xml" as="nested"/>
</project>
```

#### vm.args

The `vm.args` template of your application. `NODE_IP` is detected when the service starts `sudo service mypackage start`.
The other variable `${ERLANG_COOKIE}` is replaced when package is generated based on your configs.

```erl
-name ${APP_NAME}@${NODE_IP}
-setcookie ${ERLANG_COOKIE}
```

#### setup.properties

This is the file where you will setup at least the mandatory variables like : `package.name`, `project.deployment_scripts` and
`conf.app_path`. Also you can change the default value of all other properties. See: "All configs available" section for
all other properties available:

```
# package configs

package.name=my_project
package.version=1.0
package.description="My project description"
package.maintainer="John Doe"
package.install_path=/usr/local

# project configs

project.deployment_scripts=path_to_erl_deployment/deployment

# configs scripts

conf.app_path = deployment/conf
```

#### app.properties

In this script you will store all the variables values used into your `app.config` template

```erl

# erlang

erlang.cookie=DGKJHKEJHKJDBJHGEDKJKJH

# main app

app.dependencies = lager
app.key=default value
prod.app.key= value for prod environment

```

#### app.config

Your main `app.config` template used to generate the final application config.

```erl
[

${app.dependencies}

{my_app, [
    {key, <<"${key}">>}

]}

].

```

### rebar.config

The `rebar.config` should include the following for relx (important is `sys_config` and `vm_args`):

```erlang
{relx, [
    {release, {app_name, "1.0" }, [app_name]},
    {sys_config, "./_build/configs/default/sys.config"},
    {vm_args,    "./_build/configs/default/vm.args"},
    {dev_mode, true},
    {include_erts, false},
    {extended_start_script, true}
]}.

{profiles, [
    {release, [
        {relx, [
            {sys_config, "./_build/configs/release/sys.config"},
            {vm_args,    "./_build/configs/release/vm.args"},
            {dev_mode, false},
            {include_erts, true}
        ]}
    ]}
]}.
```

### Project structure

A recommended project structure will be:

```bash
project_root
    deployment
        conf
            app.config
            app.properties
            vm.args
        setup.properties
    build.xml     
    rebar.config   
```

Example of a skeleton project can be found here: **TODO**

# All configs available

You can override the default values of different properties by defining them into the `setup.properties`

- `package.name` - the name of your package (mandatory)
- `package.install_path` - where the package will gone be installed (mandatory)
- `package.version` - version of your package (default: 1.0)
- `package.description` - description of your package (default: dummy description)
- `package.maintainer`- maintainer of your package (default: dummy stuff)
- `package.home` - the full path where your app will be installed. No reason to change this: (default: `${package.install_path}/${project.release.appdir}`)
- `package.app` - the full path where your app binary will be located. No reason to change this: (default: `${package.home}/bin/${project.release.appdir}`)

- `project.environments`- list of all environments you support. Put anything you want here. default(dev|beta|prelive|prod)
- `project.release.location`- where the erlang release is located. Defaults to: `${project.root}/_build/release/rel`
- `project.release.appdir`- the name of your erlang app (make sense in case it's different than your package name). defaults `${package.name}`
- `project.deployment_scripts` - Location to the `erl_deployment` app (deployment folder). mandatory

- `conf.base_path` - set this only in case you want to store the config files of your deps in a separate location. For example you have a base
project with all your common deps and their configs which you share with all your apps. in this folder you should place all your `appname.config`
files and also a `base.properties` one where you define the variables which have to be replaced
- `conf.temp_path` - temporary path where configs are preprocessed internally. defaults to `${project.root}/_build/configs`
- `conf.app.profile.default` - your app default profile used during development. default to `${project.root}/_build/configs/default`
- `conf.app.profile.release` - your release profile used with your relx release. default to `${project.root}/_build/configs/release`

- `deb.post_build_target` - in case you want to execute a custom ant target after the release is done (just before creating the deb package).
- `deb.post_install`- in case you want to execute a custom bash script after package installing. defaults to `${project.deployment_scripts}/deb/postinst.sh`
- `deb.pre_remove` - in case you want to execute a custom bash script before package is removed. defaults to `${project.deployment_scripts}/deb/prerm.sh`
- `deb.upstart_script` - in case you want to use another custom upstart script. defaults to `${project.deployment_scripts}/deb/upstart.conf`
- `deb.systemd` - in case you want to use another custom systemd script. defaults to `${project.deployment_scripts}/deb/systemd.service`
- `deb.use_upstart` - use upstart instead of systemd. (default: false). Ubuntu < 16.04 supports only upstart
- `deb.deps` - set all the debian packages (comma separated) that needs to exist for your package to work properly.
- `deb.reload_modified_beams` - enable code hot-swapping when deb is updated. (default is false)

- `release.use_erlang_heart` - Can be used to disable the Erlang Heart (http://erlang.org/doc/man/heart.html) which is enabled by 
default in rebar3 releases. Can be done also based on environment (ex: `release.prod.use_erlang_heart=false`)
- `release.node_name` - This value will replace the `${APP_NAME}` token into `vm.args` 

- `misc.ip.osx` - allow the user to set it's own command for getting the IP address on a OSX system
- `misc.ip.linux` - allow the user to set it's own command for getting the IP address on a Linux based system
- `misc.reload_command` - the command used to hot-swap the code

# Use variables based on environment

In case you have variables linked to the environment you are able to expand them based on the environment you are building for.
What you need to do is:

- Add in the `app_name.config` the variable under the format `${variable_name}`
- In `app.properties` or `base.properties` you declare what values will have based on the environment. Example: app_name.variable_name=value (default value), env.app_name.variable_name=value. Will overwrite the value for that environment. Example: `beta.lager.graylog_host = 127.0.0.1`
Note: all variables used for the main app should start with `app.` for example: `app.my_var=default_value` or `beta.app.my_var=beta_value`

### Available ant commands

All above commands should be executed from the folder where `build.xml` is located:

- `ant help` - will print all available commands
- `ant genconf -Denv=..` where `env` is one of the values you defined. This will build the `sys.config` for your
application and place it by default in `_build/configs/default/sys.config`
- `ant make_deb -Denv=...` where `env` is one of the values you defined. This will generate a deb package for your app with
the config expanded for the desired environment.


[1]:https://ant.apache.org/
[2]:https://fpm.readthedocs.io/en/latest/index.html
