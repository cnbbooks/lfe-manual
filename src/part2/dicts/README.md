# The Ancient Key-Value Triumvirate: A Historical Retrospective

## In Which We Examine The Data Structures That Came Before Maps (And Occasionally Still Matter)

Long before maps arrived with their sophisticated syntax and first-class citizenship, Erlang programmers navigated the murky waters of associative data storage using three venerable modules: `dict`, `orddict`, and `gb_trees`. Think of them as the founding members of a particularly earnest folk trio—each with their own distinctive sound, each convinced they were doing it the right way, and each absolutely correct within their specific context.

These modules represent different philosophical approaches to the eternal problem of "I have a key, I have a value, now what?" The `orddict` believes in transparency and ordered simplicity. The `dict` believes in pragmatic efficiency through mysterious internal structures. And `gb_trees` believes in balanced hierarchies and giving you enough rope to either hang yourself or construct a magnificent suspension bridge—the choice, as always, is yours.

## The Historical Context (Or: Why These Still Exist When Maps Are So Nice)

Maps were introduced in Erlang R17 with considerable fanfare and immediately rendered several of these modules somewhat obsolete, rather like how the automobile made the horse-drawn carriage industry reconsider its investment strategies. However, unlike horse-drawn carriages, these modules haven't disappeared into museums. They persist for three excellent reasons:

1. **Legacy Code**: Vast swathes of Erlang code were written before maps existed, and rewriting working code simply to chase fashion is the sort of thing that leads to regret and missing deadlines.

2. **Specific Use Cases**: Each module has particular strengths that occasionally make it the optimal choice, even in a post-maps world.

3. **API Stability**: These modules have been stable for decades. When you need absolutely zero surprises, vintage sometimes beats shiny.

The key philosophical difference between these modules and maps is that maps are a native data type—the BEAM understands them intrinsically. These modules, on the other hand, are *implementations*—they're libraries that construct their associative arrays using the primitive building blocks of tuples and lists, then wrap them in opaque types to prevent you from peeking at the machinery.

## Comparing The Triumvirate: A Buyer's Guide

Before we dive into the details of each, let's establish when you might actually want to use these modules instead of maps:

### orddict: The Transparent Pragmatist

**What it is**: A sorted list of `{Key, Value}` tuples. That's it. That's the whole mystery.

**When to use it**:
- You have fewer than ~75 key-value pairs
- You want a human-readable data structure you can examine without tools
- You need deterministic ordering by keys
- You're debugging and want to see exactly what's in your data structure
- You're interoperating with code that expects lists

**When to avoid it**:
- You have more than 75 pairs (performance degrades linearly)
- You need fast lookups on large datasets
- Someone might judge you for using something so delightfully simple

### dict: The Pragmatic Workhorse (Now Mostly Retired)

**What it is**: A hash-based dictionary with an opaque internal structure.

**When to use it**:
- You have legacy code using it already
- You're working in a pre-R17 codebase (bless your heart)
- You have 75+ key-value pairs and can't use maps for some reason

**When to avoid it**:
- You're writing new code (use maps instead)
- You need any feature maps provide
- You value your future self's time

**Historical note**: The `dict` module was the go-to for large key-value stores before maps arrived. It's now in the awkward position of being "perfectly functional but superseded," rather like a flip phone in the smartphone era—it makes calls just fine, but nobody's impressed.

### gb_trees: The Structured Perfectionist

**What it is**: General Balanced Trees implementing Prof. Arne Andersson's balanced binary tree algorithm.

**When to use it**:
- You need to efficiently find minimum/maximum keys
- You want to iterate through keys in order
- You need the specific performance characteristics of balanced trees
- You're implementing an algorithm that specifically requires tree operations
- You enjoy the aesthetic of logarithmic complexity

**When to avoid it**:
- You just need basic key-value storage (use maps)
- The learning curve for its dual-mode API seems excessive
- You prefer the simplicity of hash-based lookups

## Performance Characteristics: A Rough Guide

The performance of these structures varies based on size and operation type:

**For small datasets (< 75 elements)**:
- `orddict`: Perfectly acceptable, human-readable, simple
- `dict`: Overkill, slower than orddict for small sizes
- `gb_trees`: Also overkill, but faster than dict
- `maps`: Optimal in almost all cases

**For medium datasets (75-10,000 elements)**:
- `orddict`: Unacceptably slow
- `dict`: Good performance, but maps are better
- `gb_trees`: Excellent performance, competitive with maps
- `maps`: Generally optimal

**For large datasets (10,000+ elements)**:
- `orddict`: Don't even think about it
- `dict`: Acceptable but deprecated
- `gb_trees`: Excellent, sometimes faster than maps for ordered operations
- `maps`: Generally optimal

The actual performance depends on your read/write ratio, whether you need ordered access, and whether you're running a benchmark or actual production code (these can differ by surprising margins).
