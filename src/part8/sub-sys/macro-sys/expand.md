# Macro Expansion Algorithm

**Expansion process** (from src/lfe_macro.erl:pass_form/3):

```text
1. Check if form is progn → recursively expand all subforms
2. Check if form is eval-when-compile → evaluate at compile time
3. Check if form is include-file/include-lib → read and expand file
4. Check if form is define-macro → add macro to environment
5. Check if form head is macro → expand macro call
6. Otherwise: recursively expand subforms
7. Repeat until no macros remain
```

**Macro lookup** (priority order):

1. **Local macros** (defined in current module)
2. **Imported macros** (from other modules)
3. **Built-in macros** (predefined)

**Most recently defined wins** (LIFO).

**Expansion safety**:

- **Infinite loop detection**: Track expansion depth, error if exceeds limit
- **Error handling**: Collect errors, continue expanding other forms
- **Hygiene**: Manual (LFE has no `gensym` - developers are required to protect their macro variables from conflicting with user-space names)
