# rebar3_cargo 使用指南

## 简介

`rebar3_cargo` 是一个 [rebar3](https://www.rebar3.org/) 插件，用于在 Erlang 应用程序中自动构建 Rust crates。该插件能够构建项目中的所有 Rust crates，并将编译输出复制到 `priv/crates/<cratename>/<binary>` 目录中。

这个插件特别适用于以下场景：
- 开发 Erlang NIFs (Native Implemented Functions)
- 创建 Erlang Port 程序
- 在 Erlang 项目中集成 Rust 代码

## 安装和配置

### 基本配置

在你的 `rebar.config` 文件中添加以下配置：

```erlang
{plugins, [rebar3_cargo]}.

{provider_hooks, [
    {pre, [
        {compile, {cargo, build}}
    ]},
    {post, [
        {clean, {cargo, clean}}
    ]}
]}.
```

这个配置会：
- 在编译前自动构建 Rust crates
- 在清理后清理 Rust 构建产物

### 高级配置选项

你可以通过 `cargo_opts` 配置项自定义构建行为：

```erlang
{cargo_opts, [
    {release, true},     % 强制使用 release 模式
    {debug, true},       % 强制使用 debug 模式
    {skip, false},       % 跳过 cargo 构建
    {src_dir, "."},      % Rust 源码目录，默认为项目根目录
    {load_from_app, my_app} % 指定从哪个应用加载 NIF
]}.
```

**注意：** `release` 和 `debug` 选项互斥。如果都不指定，插件会根据 rebar3 的 profile 自动选择（`prod` profile 使用 release 模式）。你可以使用 `{debug, true}` 来强制使用 debug 模式。

## 项目结构

### 基本目录结构

```
my_erlang_app/
├── src/
│   ├── my_app.erl
│   └── my_app.app.src
├── rust_src/           % 或者直接在根目录
│   ├── my_nif/
│   │   ├── Cargo.toml
│   │   └── src/
│   │       └── lib.rs
│   └── my_port/
│       ├── Cargo.toml
│       └── src/
│           └── main.rs
├── Cargo.toml         % Workspace 配置
└── rebar.config
```

### Cargo Workspace 配置

在项目根目录创建 `Cargo.toml` 文件来定义 workspace：

```toml
[workspace]
members = [
    "rust_src/my_nif",
    "rust_src/my_port"
]
resolver = "3"
```

## 开发 NIF (Native Implemented Functions)

### Rust NIF 实现

创建 NIF crate 的 `Cargo.toml`：

```toml
[package]
name = "my_nif"
version = "0.1.0"
edition = "2021"

[lib]
name = "my_nif"
crate-type = ["cdylib"]

[dependencies]
rustler = "0.36"
```

在 `src/lib.rs` 中实现 NIF 函数：

```rust
use rustler::{Env, Term, NifResult, Encoder};

// 导出 NIF 函数
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

// 初始化 NIF 模块
rustler::init!("my_nif", [add]);
```

### Erlang NIF 模块

创建对应的 Erlang 模块 `src/my_nif.erl`：

```erlang
-module(my_nif).
-include("cargo.hrl").

-export([add/2]).
-on_load(init/0).

%% NIF 初始化
init() ->
    ?load_nif_from_crate(my_nif, 0).

%% NIF 函数声明（如果 NIF 加载失败会调用这些函数）
add(_A, _B) ->
    exit(nif_library_not_loaded).
```

### 使用 NIF

使用`rebar3 shell`进行测试。
```erlang
% 调用 NIF 函数
1> my_nif:add(1,1).
2
```

## 开发 Port 程序

### Rust Port 实现

创建 Port 程序的 `Cargo.toml`：

```toml
[package]
name = "my_port"
version = "0.1.0"
edition = "2021"

[dependencies]
byteorder = "1.5"
```

在 `src/main.rs` 中实现 Port 程序：

```rust
use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use std::io::{stdin, stdout, Read, Write};

fn read_cmd() -> std::io::Result<Option<Vec<u8>>> {
    let len = match stdin().read_u16::<BigEndian>() {
        Ok(len) => len as usize,
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    };
    
    let mut buf = vec![0u8; len];
    stdin().read_exact(&mut buf)?;
    Ok(Some(buf))
}

fn write_cmd(data: &[u8]) -> std::io::Result<()> {
    stdout().write_u16::<BigEndian>(data.len() as u16)?;
    stdout().write_all(data)?;
    stdout().flush()
}

fn main() -> std::io::Result<()> {
    loop {
        match read_cmd()? {
            Some(cmd) => {
                match cmd[0] {
                    1 => {
                        // 处理命令 1
                        let result = cmd[1] + 1;
                        write_cmd(&[result])?;
                    },
                    2 => {
                        // 处理命令 2
                        let result = cmd[1] * 2;
                        write_cmd(&[result])?;
                    },
                    _ => break,
                }
            },
            None => break, // EOF reached, exit gracefully
        }
    }
    Ok(())
}
```

### Erlang Port 接口

创建 Erlang 模块来与 Port 程序通信：

```erlang
-module(my_port).
-include("cargo.hrl").

-export([start/0, stop/0, foo/1, bar/1]).

start() ->
    PortPath = filename:join([
        code:priv_dir(?CARGO_LOAD_APP), 
        "crates", 
        "my_port", 
        "my_port"
    ]),
    spawn(fun() -> init(PortPath) end).

stop() ->
    my_port ! stop.

foo(X) ->
    call_port([1, X]).

bar(X) ->
    call_port([2, X]).

call_port(Data) ->
    my_port ! {call, self(), Data},
    receive
        {my_port, Result} -> Result
    end.

init(PortPath) ->
    register(my_port, self()),
    Port = open_port({spawn, PortPath}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
        {call, Caller, Data} ->
            Port ! {self(), {command, Data}},
            receive
                {Port, {data, Result}} ->
                    Caller ! {my_port, Result}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} -> ok
            end
    end.
```

## 构建和测试

### 构建项目

```bash
# 普通构建（debug 模式）
rebar3 compile

# 生产构建（release 模式）
rebar3 as prod compile

# 清理构建产物
rebar3 clean
```

### 运行测试

```bash
# 运行 Erlang 测试
rebar3 eunit

# 只运行 Rust 测试，这个有点小问题，因为erlang-cargo的cargo test要使用+nightly。建议直接在cargo内部test，避开erlang调用。
rebar3 cargo test
```

**注意：** 如果你想在运行 `rebar3 eunit` 时自动运行 Rust 测试，可以在 `rebar.config` 中添加：
```erlang
{provider_hooks, [
    {pre, [
        {compile, {cargo, build}}
    ]},
    {post, [
        {clean, {cargo, clean}},
        {eunit, {cargo, test}}
    ]}
]}.
```

### 手动 Cargo 操作

```bash
# 手动构建 Rust crates
rebar3 cargo build

# 手动强制构建 release 的 Rust crates
rebar3 as prod cargo build

# 清理 Rust 构建产物
rebar3 cargo clean

# 运行 Rust 测试
rebar3 cargo test
```

## 高级用法

### 编译模式控制

rebar3_cargo 支持三种编译模式：

1. **auto（默认）**：根据 rebar3 profile 自动选择
   - `prod` profile → release 模式
   - 其他 profile → debug 模式

2. **release**：强制使用 release 模式
   ```erlang
   {cargo_opts, [{release, true}]}
   ```

3. **debug**：强制使用 debug 模式
   ```erlang
   {cargo_opts, [{debug, true}]}
   ```

### 跳过 Cargo 构建

如果你想暂时跳过 Rust 代码的构建：

```erlang
{cargo_opts, [{skip, true}]}
```

### 自定义源码目录

默认情况下，插件会在项目根目录查找 `Cargo.toml`。你可以指定不同的目录：

```erlang
{cargo_opts, [{src_dir, "rust_src"}]}
```



## 测试和调试

### 单元测试

在 Erlang 模块中添加测试：

```erlang
-include_lib("eunit/include/eunit.hrl").

my_nif_test() ->
    ?assertEqual(7, my_nif:add(3, 4)).

my_port_test_() ->
    {setup,
     fun() -> my_port:start() end,
     fun(_) -> my_port:stop() end,
     [?_assertEqual([13], my_port:foo(12)),
      ?_assertEqual([24], my_port:bar(12))]}.
```

### 调试技巧

1. **查看构建输出**：使用 `rebar3 compile -v` 查看详细构建信息

2. **检查生成的文件**：构建后的二进制文件位于 `priv/crates/<cratename>/` 目录

3. **Rust 日志**：在 Rust 代码中使用 `eprintln!` 进行调试输出

4. **Erlang 调试**：使用 `io:format/2` 或 `rebar_log:log/3` 输出调试信息

## 常见问题

### Q: 构建失败，提示找不到 Rust 工具链
A: 确保已安装 Rust 工具链：
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Q: NIF 加载失败
A: 检查以下几点：
- 确保 `Cargo.toml` 中设置了 `crate-type = ["cdylib"]`
- 确保 Erlang 模块名与 Rust crate 名匹配
- 检查 `cargo.hrl` 文件是否正确生成

### Q: Port 程序无法启动
A: 检查以下几点：
- 确保 Port 程序有执行权限
- 检查路径是否正确
- 确保 Port 程序能正确处理 packet 协议

### Q: 跨平台编译问题
A: rebar3_cargo 目前主要在 Linux和Windows 上测试。对于其他平台：
- macOS：通常工作良好
- 交叉编译：目前不支持 `--target` 参数

## 最佳实践

1. **项目组织**：将 Rust 代码放在 `rust_src/` 目录中，保持项目结构清晰

2. **版本管理**：在 `Cargo.toml` 中明确指定依赖版本

3. **错误处理**：在 NIF 中正确处理错误，避免崩溃整个 Erlang 虚拟机

4. **性能优化**：在生产环境使用 release 模式编译

5. **测试**：为 Rust 和 Erlang 代码都编写充分的测试

6. **文档**：为复杂的 NIF 函数编写清晰的文档

## 更多资源

- [rebar3_cargo GitHub 仓库](https://github.com/rusterlium/rebar3_cargo)
- [Rustler 文档](https://docs.rs/rustler/)
- [Erlang NIFs 官方文档](https://www.erlang.org/doc/man/erl_nif.html)
- [Erlang Ports 官方文档](https://www.erlang.org/doc/reference_manual/ports.html)

## 许可证

rebar3_cargo 使用 Apache-2.0 和 MIT 双许可证。