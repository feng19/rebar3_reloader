rebar3_reloader
=====

一个能帮助你在编辑完代码之后自动编译并加载代码的 `rebar3` 插件,受 [`rebar3_auto`](https://github.com/vans163/rebar3_auto) 激发而来.

### 必备

- Mac: [fsevent](https://github.com/thibaudgg/rb-fsevent)
- Linux: [inotify](https://github.com/rvoicilas/inotify-tools/wiki)
- Windows: [inotify-win](https://github.com/thekid/inotify-win)

## 功能

* 支持自定义变动文件后缀名: `reloader_extra_exts` (默认为: `[]`);
* 支持自定义编译命令: `reloader_compile_cmd` (默认为: `rebar3 compile`);
* 支持自定义监听额外的文件目录: `reloader_extra_dirs` (默认为: `[src, c_src]`)
* 支持自定义两次编译间隔时间: `reloader_wait_stop_change_time`(默认为: `500`毫秒)
* 代码自动加载策略:
  * 应用黑名单: `reloader_excluded_apps`(默认为: `[]`);
  * 代码模块黑名单:  `reloader_excluded_modules`(默认为: `[]`);
  * 保存`beam`文件时间,有更新才重新加载

## 使用

将这个插件加入到你的 `rebar.config` 后者全局配置文件 `~/.config/rebar3/rebar.config`中:

```erlang
{plugins, [
    {rebar3_reloader, "0.1.0"}
]}.
```

只需要在你的应用下调用 `rebar3 reloader`:


```shell
$ rebar3 reloader
1>
```

就会启动一个 `eshell` ,跟使用 `rebar3 shell` 的效果一样,但不同之处在于你修改了源代码之后`reloader`会帮你自动编译源码并加载到`vm`中.

### 翻译

[英文](README.md)