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