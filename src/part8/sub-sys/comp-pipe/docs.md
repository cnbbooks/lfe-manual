# Stage 5: Documentation Extraction (lfe_docs.erl)

**Purpose**: Extract documentation from code and format it per EEP-48 (Erlang documentation standard).

**Module**: `lfe_docs.erl` (362 LOC) - Location: `src/lfe_docs.erl`

**EEP-48 format**:

```erlang
-record(docs_v1, {
    anno :: erl_anno:anno(),
    beam_language :: atom(),
    format :: binary(),
    module_doc :: doc_content(),
    metadata :: map(),
    docs :: [doc_element()]
}).
```

**Documentation extraction process**:

1. **Module docs**: From module metadata `(defmodule name "docstring" ...)`
2. **Function docs**: From function metadata `(defun name "docstring" ...)`
3. **Type docs**: From type definitions
4. **Spec extraction**: Function specifications

**Docstring formats supported**:

- Binary: `#"Documentation string"`
- Charlist: `"Documentation string"`
- Hidden: `false` (function exists but not documented)

**Integration**: Docs stored in `.beam` file as chunk `"Docs"`, accessible via `code:get_doc/1`.
