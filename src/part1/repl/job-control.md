# Job Control

Thanks to Erlang, when working in the LFE REPL you have access to a powerful job control system that allows you to manage multiple shell sessions, interrupt running processes, and switch between different contexts. This system is accessed through Job Control Language (JCL) mode, which becomes available when you press `<CTRL-G>`.

## Entering JCL Mode

When in the LFE REPL, typing `<CTRL-G>` will detach the current job and activate JCL mode:

```
lfe> (set ltuae (* 2 (+ 1 2 3 4 5 6)))
42
lfe>
User switch command (enter 'h' for help)
 -->
```

At the JCL `-->` prompt, you may get help text by typing `?` or `h`:

```text
 --> h
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message
 -->
```

## Understanding Jobs

In the context of the Erlang/LFE shell, a **job** refers to a single evaluator process along with any local processes it spawns. When you start the LFE REPL, you begin with one job that acts as your interactive shell session.

Each job maintains its own:

- Variable bindings
- Process dictionary
- Record definitions
- Evaluation context

Only the currently connected job can perform operations with standard I/O, while detached jobs are blocked from using standard I/O (though they can continue running background computations).

## JCL Commands

Each JCL command serves a specific purpose in managing your shell sessions:

- **`c [nn]`** - **Connect to job**: Connects to job number `nn` or the current job if no number is specified. This resumes the standard shell and allows you to interact with that job's evaluation context.

- **`i [nn]`** - **Interrupt job**: Stops the evaluator process for job number `nn` or the current job, but preserves the shell process. Variable bindings and the process dictionary are maintained, so you can reconnect to the job later. This is particularly useful for interrupting infinite loops or long-running computations.

- **`k [nn]`** - **Kill job**: Completely terminates job number `nn` or the current job. All spawned processes in the job are killed (provided they haven't changed their group leader and are on the local machine). This permanently destroys the job's context.

- **`j`** - **List all jobs**: Displays all current jobs with their numbers and descriptions. The currently connected job is marked with an asterisk (`*`).

- **`s [shell]`** - **Start local shell**: Creates a new job with a fresh shell environment. If no shell module is specified, it starts the default shell (LFE shell in our case).

- **`r [node [shell]]`** - **Start remote shell**: Starts a remote job on the specified node. This is used in distributed Erlang to control applications running on other nodes in the network.

- **`q`** - **Quit Erlang**: Completely exits the Erlang runtime system. Note that this command may be disabled if Erlang was started with the `+Bi` system flag.

## Running Multiple Shells: A Practical Example

Here's a practical demonstration of using multiple jobs to maintain separate evaluation contexts:

```
$ lfe
lfe> (set ltuae (* 2 (+ 1 2 3 4 5 6)))
42
lfe> ^G
User switch command (enter 'h' for help)
 --> j
   1* {lfe_shell,start,[]}
 --> s lfe_shell
 --> j
   1  {lfe_shell,start,[]}
   2* {lfe_shell,start,[]}
 --> c 2
lfe> ltuae
** exception error: symbol ltuae is unbound

lfe> (set arch (erlang:system_info 'system_architecture))
"aarch64-apple-darwin24.4.0"
lfe> ^G
User switch command (enter 'h' for help)
 --> j
   1  {lfe_shell,start,[]}
   2* {lfe_shell,start,[]}
 --> c 1
lfe> ltuae
42
lfe> arch
** exception error: symbol arch is unbound

lfe> ^G
User switch command (enter 'h' for help)
 --> c 2
lfe> arch
"aarch64-apple-darwin24.4.0"
```

This example demonstrates how each job maintains its own variable bindings:

- Job 1 has `ltuae` defined but not `arch`
- Job 2 has `arch` defined but not `ltuae`
- Each job represents a completely separate evaluation environment

## Common Use Cases

### 1. Interrupting Infinite Loops

If your code gets stuck in an infinite loop:

```lisp
lfe> (defun infinite-loop () (infinite-loop))
infinite-loop
lfe> (infinite-loop)
;; Process gets stuck here - press Ctrl-G
User switch command
 --> i
 --> c
lfe> ;; Back to a responsive shell
```

### 2. Experimental Development

Use separate jobs for different experiments:

```lisp
;; Job 1: Working on feature A
lfe> (set feature-a-data '(1 2 3 4))

;; Switch to Job 2 for feature B
User switch command
 --> s lfe_shell
 --> c 2
lfe> (set feature-b-data '(a b c d))

;; Switch back to continue feature A work
User switch command
 --> c 1
lfe> feature-a-data
(1 2 3 4)
```

### 3. Remote Development

Connect to remote nodes in distributed systems:

```lisp
User switch command
 --> r other_node@other_host
;; Now connected to a shell on the remote node
```

## Important Notes

- **Variable Isolation**: Each job maintains completely separate variable bindings and evaluation contexts
- **Process Preservation**: Using `i` (interrupt) preserves the job's state, while `k` (kill) destroys it permanently
- **I/O Blocking**: Only the connected job can use standard I/O; detached jobs will block if they attempt I/O operations
- **Record Definitions**: Each job has its own set of record definitions, though some default records may be loaded automatically

## Customizing JCL Behavior

The behavior of the shell escape key (`Ctrl-G`) can be modified using the STDLIB application variable `shell_esc`:

- `jcl` (default): Activates JCL mode
- `abort`: Terminates the current shell and starts a new one instead of entering JCL mode

This is set with: `erl -stdlib shell_esc abort`

Job Control in LFE provides a powerful way to manage multiple development contexts, handle problematic code execution, and work with distributed systems, making it an essential tool for effective LFE development.
