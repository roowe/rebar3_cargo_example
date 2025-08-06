# rebar3_cargo Example

📖 **中文完整指南**: [rebar3_cargo使用指南.md](rebar3_cargo使用指南.md)

## Overview

This project demonstrates how to use `rebar3_cargo`, a rebar3 plugin that enables seamless integration of Rust code into Erlang applications. The plugin automatically builds Rust crates and copies compiled binaries to `priv/crates/<cratename>/` for use in Erlang.

## Key Features

- **Automatic Rust Integration**: Build Rust crates as part of your Erlang build process
- **NIF Support**: Create high-performance Native Implemented Functions using Rust
- **Port Programs**: Develop external programs that communicate with Erlang via ports
- **Flexible Configuration**: Support for debug/release modes and custom build options

## Quick Start

### 1. Configuration

Add to your `rebar.config`:

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

### 2. Project Structure

```
my_erlang_app/
├── src/
│   ├── my_app.erl
│   └── my_app.app.src
├── rust_src/
│   ├── my_nif/          % Rust NIF crate
│   │   ├── Cargo.toml
│   │   └── src/lib.rs
│   └── my_port/         % Rust port program
│       ├── Cargo.toml
│       └── src/main.rs
├── Cargo.toml           % Workspace configuration
└── rebar.config
```

### 3. Building

```bash
# Debug build
rebar3 compile

# Release build
rebar3 as prod compile

# Clean
rebar3 clean
```

## Development Approaches

### NIFs (Native Implemented Functions)

NIFs allow you to implement performance-critical functions in Rust while maintaining seamless integration with Erlang. Key considerations:

- **Performance**: Ideal for CPU-intensive operations
- **Safety**: Rust's memory safety helps prevent VM crashes
- **Integration**: Direct function calls from Erlang
- **Use Cases**: Mathematical computations, data processing, cryptography

### Port Programs

Port programs run as separate OS processes and communicate with Erlang via message passing:

- **Isolation**: Crashes don't affect the Erlang VM
- **Flexibility**: Can handle complex I/O operations
- **Communication**: Uses packet-based protocol
- **Use Cases**: External integrations, long-running processes, system interactions

## Configuration Options

Advanced configuration through `cargo_opts`:

```erlang
{cargo_opts, [
    {release, true},        % Force release mode
    {debug, true},          % Force debug mode
    {skip, false},          % Skip cargo build
    {src_dir, "."},         % Rust source directory
    {load_from_app, my_app} % Specify app for NIF loading
]}.
```

**Build Modes:**
- **Auto (default)**: Debug for dev, release for `prod` profile
- **Release**: Optimized builds for production
- **Debug**: Development builds with debug info

## Testing

```bash
# Run Erlang tests
rebar3 eunit

# Manual Rust operations
rebar3 cargo build
rebar3 cargo test
rebar3 cargo clean
```

## Best Practices

1. **Project Organization**: Keep Rust code in `rust_src/` directory
2. **Error Handling**: Properly handle errors in NIFs to avoid VM crashes
3. **Performance**: Use release mode for production deployments
4. **Testing**: Write comprehensive tests for both Rust and Erlang components
5. **Documentation**: Document complex NIF interfaces clearly
6. **Version Management**: Pin dependency versions in Cargo.toml

## Common Use Cases

- **High-Performance Computing**: Mathematical operations, data processing
- **System Integration**: Interfacing with system libraries
- **Cryptography**: Implementing secure algorithms
- **Data Serialization**: Fast encoding/decoding operations
- **External Tools**: Wrapping existing Rust libraries

## Troubleshooting

- **Build Failures**: Ensure Rust toolchain is installed
- **NIF Loading Issues**: Check crate-type configuration and module naming
- **Port Communication**: Verify packet protocol implementation
- **Cross-Platform**: Primarily tested on Linux and Windows

## Resources

- [rebar3_cargo GitHub Repository](https://github.com/rusterlium/rebar3_cargo)
- [Rustler Documentation](https://docs.rs/rustler/)
- [Erlang NIFs Official Documentation](https://www.erlang.org/doc/man/erl_nif.html)
- [Erlang Ports Official Documentation](https://www.erlang.org/doc/reference_manual/ports.html)

## License

This project uses MIT licensing.
