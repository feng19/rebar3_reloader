rebar3_reloader
=====

A rebar3 plugin for auto running compile on source file change reloading modules in the shell.

Inspired by [`rebar3_auto`](https://github.com/vans163/rebar3_auto).

## Prerequisite

- Mac: [fsevent](https://github.com/thibaudgg/rb-fsevent)
- Linux: [inotify](https://github.com/rvoicilas/inotify-tools/wiki)
- Windows: [inotify-win](https://github.com/thekid/inotify-win)

## Feature

* Support custom valid extensions: `reloader_extra_exts` (default: `[]`);
* Support custom compile cmd: `reloader_compile_cmd` (default: `rebar3 compile`);
* Support custom monitor extra file directories: `reloader_extra_dirs` (default:`[src, c_src]`)
* Support custom compile interval: `reloader_wait_stop_change_time`(default: `500` millisecond)
* Strategy of auto reloading modules:
  * blacklist of application: `reloader_excluded_apps`(default: `[]`);
  * blacklist of module:  `reloader_excluded_modules`(default: `[]`);
  * save the `beam` file's compile time ,and if had new file will reload.

## Use

Add the plugin to your `rebar.config` or your global config `~/.config/rebar3/rebar.config`:

```erlang
{plugins, [
    {rebar3_reloader, "0.1.0"}
]}.
```

Then just call `rebar3 reloader` directly in an existing application:


```shell
$ rebar3 reloader
1>
```

It will start a `eshell` just like use `rebar3 shell`, but different is this time it will auto compile and load the new code when you change your source code file.

### Translations

[Chinese](README_Zh.md)