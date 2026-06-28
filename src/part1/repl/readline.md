# `readline` Support

The LFE REPL, being built atop the Erlang shell, can benefit from GNU Readline support to provide enhanced line editing capabilities including command history, keyboard shortcuts, and tab completion. This section covers how to enable and configure these features for optimal development experience.

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Background
  </h4>
  <p class="mb-0">
    The GNU Readline library provides a common interface for line editing and history management across many interactive programs. Originally developed for the Bash shell, it has become the de facto standard for command-line editing in Unix-like systems.
  </p>
</div>

## Built-in History Support (Erlang/OTP 20+)

Starting with Erlang/OTP 20, persistent shell history is available out of the box, though it's disabled by default. This feature provides basic command history persistence across sessions.

### Enabling Shell History

To enable persistent history for all Erlang-based shells (including LFE), add the following to your shell's configuration file (`~/.bashrc`, `~/.zshrc`, etc.):

```bash
# Enable Erlang shell history
export ERL_AFLAGS="-kernel shell_history enabled"
```

### Advanced History Configuration

You can customize the history behavior using additional kernel parameters:

```bash
# Complete history configuration
export ERL_AFLAGS="+pc unicode"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history enabled"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history_path '\"$HOME/.erl_history\"'"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history_file_bytes 1048576"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history_drop '[\"q().\",\"init:stop().\",\"halt().\"]'"
export ERL_AFLAGS
```

Where:

- `shell_history_path` - Custom path for history file (default: `~/.erlang-history.*)`)
- `shell_history_file_bytes` - Maximum history file size in bytes (default: 512KB, minimum: 50KB)
- `shell_history_drop` - Commands to exclude from history (e.g., exit commands)

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
    Note on String Escaping
  </h4>
  <p class="mb-0">
    Erlang application parameters must be passed as proper Erlang terms. Strings require double quotes, which must be escaped in shell environment variables as shown above.
  </p>
</div>

## Built-in Tab Completion

The Erlang shell (and by extension, LFE) includes sophisticated built-in tab completion functionality through the `edlin` and `edlin_expand` modules. LFE extends this with its own `lfe_edlin_expand` module to provide Lisp-aware completion capabilities. Starting with Erlang/OTP 26, the auto-completion feature has been vastly improved, supporting auto-completion of variables, record names, record field names, map keys, function parameter types, and file names.

### LFE-Specific Tab Completion

LFE provides enhanced tab completion that understands Lisp syntax and LFE-specific constructs. When you type in the LFE REPL, tab completion works seamlessly with:

- **Module names**: Type `(code` and press TAB to see `code` and `code_server`
- **Function names with arities**: Type `(code:` and press TAB to see all available functions with their arities displayed in a paged format
- **LFE syntax**: Completion works properly with LFE's S-expression syntax
- **Standard Erlang modules**: Full access to Erlang's standard library with completion

### What Tab Completion Covers

The standard expansion function is able to expand strings to valid Erlang terms, including module names, and automatically add commas or closing parentheses when no other valid expansion is possible. Specifically, tab completion works for:

- **Module names**: Type `li` and press TAB to complete to `lists:`
- **Function names**: Type `lists:` and press TAB to see all available functions
- **Variables**: Complete previously defined shell variables by typing the first letter(s) and pressing TAB
- **Record names**: Type `#` and TAB to see available records
- **Record fields**: Complete field names within record syntax
- **Map keys**: Complete keys when working with maps
- **Built-in functions (BIFs)**: Complete standard Erlang functions
- **File names**: When working with file operations

### Using Tab Completion

- **Single TAB**: Autocomplete the current word, or show 5 lines of possible completions
- **Double TAB**: Output all possible tab completions
- **Navigation**: Use move_expand_up and move_expand_down to navigate through completion lists in the expand area

### LFE Tab Completion Examples

Here are some examples of LFE's tab completion in action:

```lisp
;; Module completion
lfe> (code<TAB>
code           code_server

;; Function completion with paged display
lfe> (code:<TAB>
add_path/1                 add_path/2                 add_patha/1
add_patha/2                add_paths/1                add_paths/2
add_pathsa/1               add_pathsa/2               add_pathsz/1
add_pathsz/2               add_pathz/1                add_pathz/2
...
rows 1 to 7 of 8

;; LFE syntax-aware completion
lfe> (defun my-func<TAB>
;; Will complete user-defined functions and LFE constructs
```

The completion system understands LFE's parenthesized syntax and provides contextually appropriate suggestions.

### Customizing Tab Completion

You can customize tab completion by setting an expand function using `io:setopts/1,2` with the `{expand_fun, expand_fun()}` option:

```lisp
;; In LFE, you would set this via Erlang interop
(: io setopts `(#(expand_fun ,(lambda (input)
                               ;; Custom completion logic here
                               '#(yes "completion" ("option1" "option2"))))))
```

## Full Readline Integration

For a more complete readline experience with features like reverse search (`Ctrl-R`), customizable key bindings, and improved line editing beyond the built-in capabilities, you have several options.

### Option 1: Using `rlwrap`

`rlwrap` (readline wrapper) is a utility that adds GNU Readline capabilities to any command-line program. It's the most straightforward way to add full readline support to LFE.

#### Installing rlwrap

On most Unix-like systems:

```bash
# Ubuntu/Debian
sudo apt-get install rlwrap

# macOS (Homebrew)
brew install rlwrap

# CentOS/RHEL/Fedora
sudo yum install rlwrap  # or dnf install rlwrap
```

#### Basic Usage

```bash
# Start LFE with readline support
rlwrap rebar3 lfe repl

# Or if using the lfe executable directly
rlwrap lfe
```

#### Advanced rlwrap Configuration

For optimal LFE/Lisp experience, you can customize rlwrap with specific options:

```bash
# Enhanced configuration for Lisp-like languages
rlwrap -r -m -q '"' -b "(){}[],^%#@\\;:'" rebar3 lfe repl
```

Where:

- `-r` puts all words seen on input and output on the completion list
- `-m` enables multi-line editing
- `-q '"'` handles double quotes specially
- `-b "(){}[],^%#@\\;:'"` defines characters that break words for completion

#### Creating a Wrapper Script

For convenience, create a wrapper script in your `$PATH` (e.g., `~/bin/lfe-repl`):

```bash
#!/bin/bash
# LFE REPL with readline support

# Basic version
rlwrap rebar3 lfe repl

# Or enhanced version with better Lisp support
rlwrap -r -m -q '"' -b "(){}[],^%#@\\;:'" rebar3 lfe repl "$@"
```

Make it executable:

```bash
chmod +x ~/bin/lfe-repl
```

### Tab Completion vs rlwrap

**Important Note**: Using rlwrap with the `-a` option (always use readline) will disable the Erlang shell's built-in tab completion functionality. This is a trade-off between rlwrap's enhanced features (like persistent history and colored prompts) and the shell's native completion capabilities.

If you prioritize tab completion:

- Use the built-in shell history (`ERL_AFLAGS="-kernel shell_history enabled"`)
- Use rlwrap without the `-a` flag, though this may provide a less integrated experience

If you prioritize rlwrap's features:

- Accept that built-in tab completion will be disabled
- Consider creating custom completion files for rlwrap (advanced)

### Option 2: Terminal-Specific Configuration

Many modern terminal emulators provide their own line editing features that can complement or replace readline functionality.

#### Using `.inputrc` Configuration

The `~/.inputrc` file configures GNU Readline behavior system-wide. Here's a useful configuration for Lisp development:

```bash
# ~/.inputrc - GNU Readline configuration

# Use vi or emacs editing mode
set editing-mode emacs

# Enable case-insensitive completion
set completion-ignore-case on

# Show completion matches immediately
set show-all-if-ambiguous on
set show-all-if-unmodified on

# Enable colored completion
set colored-stats on
set colored-completion-prefix on

# Better history search
"\e[A": history-search-backward
"\e[B": history-search-forward

# Alternative: use Ctrl-P/Ctrl-N for history search
# "\C-p": history-search-backward
# "\C-n": history-search-forward

# Disable terminal bell
set bell-style none

# Enable bracketed paste mode (safe pasting)
set enable-bracketed-paste on

# Show mode indicator for vi mode (if using vi editing-mode)
set show-mode-in-prompt on
```

After editing `~/.inputrc`, you can reload it with:

```bash
bind -f ~/.inputrc
```

## Environment Variable Summary

For a complete readline-enabled LFE environment, add these to your shell configuration:

```bash
# ~/.bashrc, ~/.zshrc, etc.

# Enable Erlang shell history
export ERL_AFLAGS="-kernel shell_history enabled"

# Optional: customize history location and behavior
# export ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history_path '\"$HOME/.lfe_history\"'"
# export ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history_file_bytes 1048576"

# Create an alias for LFE with readline
alias lfe-repl='rlwrap rebar3 lfe repl'

# Or for the lfe executable
alias lfe='rlwrap lfe'
```

## Troubleshooting

### History Not Persisting

If command history isn't being saved between sessions:

1. Ensure `shell_history` is enabled in `ERL_AFLAGS`
2. Check that the history directory is writable
3. Wait a moment before exiting the shell (history is written asynchronously)
4. Try setting a custom history path with proper permissions

### rlwrap Issues

If rlwrap isn't working correctly:

1. Verify rlwrap is installed: `which rlwrap`
2. Check that your terminal supports readline properly
3. Try with minimal options first: `rlwrap rebar3 lfe repl`
4. On some systems, you may need to omit the `-q` option

### Tab Completion Problems

If tab completion behaves unexpectedly:

1. Try adjusting the word-breaking characters in rlwrap's `-b` option
2. Check your `~/.inputrc` configuration
3. Some terminal emulators may require specific settings for completion

### Performance Issues

If the REPL feels sluggish with readline enabled:

1. Reduce history file size with `shell_history_file_bytes`
2. Use fewer rlwrap options for minimal overhead
3. Consider using built-in history only without rlwrap for simple use cases
