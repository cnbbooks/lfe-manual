# Global Variables Revisited

In traditional programming languages, global variables are widely considered dangerous and bug-prone. They introduce several critical problems:

- **Race conditions**: Multiple threads can modify global variables simultaneously, leading to unpredictable state and difficult-to-reproduce bugs
- **Hidden side effects**: Functions that modify globals create non-obvious dependencies, making code harder to understand and maintain
- **Debugging difficulties**: When any function can modify a global variable, tracing the source of bugs becomes extremely challenging
- **Testing challenges**: Global state creates hidden dependencies that make isolated unit testing nearly impossible

Erlang fundamentally solves these problems through **immutable data**. When you create a data structure in Erlang, its contents cannot be modified in-place. Operations create entirely new data structures, preserving the original. For example, updating a tuple element creates a new tuple rather than modifying the existing one. This eliminates race conditions, makes data flow explicit and predictable, and enables safer concurrent programming without the pitfalls of shared mutable state.

However, real-world applications still need to maintain state across function calls or share data between processes. Erlang provides several safe alternatives, each with its own trade-offs and appropriate use cases.

## The Process Dictionary

The **process dictionary** is a key-value storage mechanism unique to each Erlang process. It provides simple functions like `put/2`, `get/1`, and `erase/1` for storing and retrieving values within a process's lifetime.

**Benefits**: The process dictionary offers quick access to process-local data without passing it explicitly through function parameters. Each process has its own isolated dictionary, preventing interference between processes. It's particularly useful for cross-cutting concerns like request IDs in logging or maintaining parse state in complex recursive operations.

**Weaknesses**: Using the process dictionary breaks functional programming principles by introducing hidden mutable state. This makes code less transparent, harder to test, and more difficult to debug, as data flow becomes implicit rather than explicit. Most Erlang style guides recommend avoiding it except in specific cases where its convenience outweighs these concerns, such as instrumenting third-party libraries or maintaining debug context.

## ETS Tables

**ETS** (Erlang Term Storage) provides in-memory database tables that can be shared across multiple processes. ETS supports several table types: `set` (unique keys), `ordered_set` (keys in sorted order), `bag` (multiple objects per key), and `duplicate_bag` (allowing duplicate objects).

**Benefits**: ETS tables provide extremely fast access—constant time O(1) for `set` tables and logarithmic time O(log N) for `ordered_set` tables. They support concurrent access with atomic and isolated operations, making them ideal for caching, session storage, counters, and high-performance in-memory data structures. Tables can store large amounts of data efficiently within the Erlang runtime, and access patterns don't require message passing between processes.

**Weaknesses**: ETS tables have no automatic garbage collection—they persist until explicitly deleted or their owner process terminates. Select and match operations can be expensive as they typically scan the entire table unless properly indexed. Memory management requires careful attention, as tables exist outside normal process memory and don't benefit from Erlang's generational garbage collector. Overly complex match specifications can also become difficult to maintain.

## State and OTP Servers

While huge chunks of this book are dedicated to OTP servers and state management, here's the essential concept: **gen_server** is Erlang's standard behavior for implementing stateful server processes.

A gen_server initializes state through an `init/1` callback and maintains that state by passing it through callback functions like `handle_call/3` and `handle_cast/2`. State updates work by creating new copies of data structures with the desired changes, then returning that new immutable data as the current state. This approach combines the benefits of persistent, mutable-seeming state with the safety guarantees of immutable data.

**Benefits**: OTP servers provide structured, predictable state management with built-in support for supervision, debugging, and code upgrades. State changes are centralized in a single process, making the system easier to reason about. The gen_server behavior handles message queuing, timeouts, and system messages automatically.

**Weaknesses**: All requests must pass through a single process, which can become a bottleneck under high load. Each state update creates new data structures, which can impact performance for very large or frequently-updated state. Care must be taken to avoid blocking operations that could make the server unresponsive.

## External Databases

Erlang applications often connect to external databases for persistent storage. **Mnesia**, Erlang's built-in distributed database, provides a native solution with strong integration into the Erlang ecosystem. It offers ACID transactions, table replication across nodes, and the ability to store tables in RAM, on disk, or both.

For other databases, Erlang has drivers and libraries for PostgreSQL, MySQL, MongoDB, Redis, and more. These typically use connection pools and message-passing patterns to maintain safety, with processes dedicated to managing database connections and handling queries asynchronously.

**Benefits**: External databases provide durable persistence across system restarts, ACID transaction guarantees, and the ability to handle datasets larger than available RAM. They enable data sharing across multiple applications and programming languages, and often include sophisticated query capabilities, indexes, and analytical tools. Databases like PostgreSQL offer mature backup, replication, and disaster recovery solutions.

**Weaknesses**: Database operations introduce network latency and I/O overhead, becoming a potential bottleneck in high-throughput systems. They create external dependencies that can affect system reliability and require careful connection management to avoid resource leaks. The impedance mismatch between Erlang's term-based data model and SQL's relational model can complicate data mapping. Database connections are limited resources that must be pooled and managed carefully.

## Choosing the Right Approach

Each alternative serves different needs:

- **Process Dictionary**: Use sparingly for cross-cutting concerns or when integrating with third-party code
- **ETS Tables**: Ideal for shared, high-performance in-memory data that multiple processes need to access
- **OTP Servers**: The default choice for managing application state with clear ownership and structure
- **External Databases**: Essential for persistent data, large datasets, or sharing data across systems

The key insight is that Erlang doesn't prevent you from maintaining state—it just ensures you do so safely and explicitly, avoiding the pitfalls of traditional global variables.
