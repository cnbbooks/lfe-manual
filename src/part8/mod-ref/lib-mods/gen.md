# lfe_gen.erl - Dynamic Code Generation

**Purpose**: Runtime code generation and dynamic function creation.

**Location**: `src/lfe_gen.erl`
**Size**: 121 LOC, 3.9KB

**Module Classification**: Library, metaprogramming

#### Public API

```erlang
new_module(ModuleName) -> ModuleState
add_exports(Exports, ModuleState) -> ModuleState
add_function(Name, Arity, Lambda, ModuleState) -> ModuleState
finish_module(ModuleState) -> {ok, Module, Binary, Warnings}
```

Programmatically build modules. Located at `lfe_gen.erl:42-78`.

#### Module Building

**Process**:

1. Create new module
2. Add exports
3. Add function definitions
4. Finish and compile

**Example**:

```erlang
% Build a simple module
M1 = lfe_gen:new_module(mymod),
M2 = lfe_gen:add_exports([{hello, 1}], M1),
M3 = lfe_gen:add_function(hello, 1,
                          [lambda, [name],
                           [list, "Hello", name]], M2),
{ok, mymod, Bin, []} = lfe_gen:finish_module(M3),

% Load it
code:load_binary(mymod, "mymod.beam", Bin),

% Use it
mymod:hello("World").  % → "Hello World"
```

#### Implementation

**Module State** (internal record):

```erlang
-record(module, {
    name,         % Module name
    exports=[],   % Export list
    functions=[]  % Function definitions
}).
```

**Finish Module** (`finish_module/1` at lines 98-121):

1. Generate `define-module` form
2. Generate `define-function` forms
3. Call `lfe_comp:forms/2` to compile
4. Return binary

#### Dependencies

**LFE modules**:

- `lfe_comp` - Compilation
- `lfe_env` - Environment

#### Used By

- Metaprogramming code
- Code generators
- DSL implementations

#### Special Considerations

**Runtime Generation**: Modules can be created at runtime.

**Performance**: Compilation is slow; use sparingly.

**Dynamic Loading**: Generated modules can be loaded/unloaded dynamically.
