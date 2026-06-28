# lfe_qlc.erl - Query List Comprehension

**Purpose**: Transform LFE QLC syntax to Erlang's qlc module for database queries.

**Location**: `src/lfe_qlc.erl`
**Size**: 51 LOC, 1.9KB

**Module Classification**: Library, QLC integration

#### Public API

```erlang
transform(QlcForm) -> QlcExpression
```

Transform LFE QLC to Erlang qlc calls. Located at `lfe_qlc.erl:39-42`.

#### QLC Syntax

**Purpose**: Query databases (Mnesia, ETS, DETS) with comprehension-like syntax.

**Example**:

```lisp
(qlc (lc ((<- p (: mnesia table person))
          (> (person-age p) 18))
      (person-name p)))
```

**Translates to**:

```erlang
qlc:q([person_name(P) || P <- mnesia:table(person),
                          person_age(P) > 18]).
```

#### Transformation

**Simple Delegation** (`transform/1` at lines 45-51):

Actually delegates to standard LFE list comprehension transformation, then wraps in `qlc:q/1`.

```erlang
transform([qlc|ListComp]) ->
    ['call', 'qlc', 'q', transform_lc(ListComp)].
```

#### Dependencies

**Erlang stdlib**:

- `qlc` module (for runtime execution)

#### Used By

- User code querying Mnesia/ETS/DETS
- Database applications

#### Special Considerations

**Thin Wrapper**: Minimal module, mostly delegates to standard comprehension.

**Runtime Dependency**: Requires `qlc` application to be loaded.

**Optimization**: QLC can optimize queries across multiple tables (joins, filtering).
