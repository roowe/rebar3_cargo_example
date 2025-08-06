use rustler::{Env, Term, NifResult, Encoder};

// 导出 NIF 函数
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

// 初始化 NIF 模块
rustler::init!("my_nif", [add]);