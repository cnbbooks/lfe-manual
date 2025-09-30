# Part III - Data as Code

Where Part II explored how LFE represents information through data structures, Part III reveals how those same structures spring to life as executable programs. In languages such as Lisp, data structures can be directly entered and evaluated as code, and LFE exemplifies this principle through its evaluation model and execution semantics.

The transition from data to code occurs through evaluation—the process by which static data structures become dynamic computations. Part III begins with expressions and functions, showing how lists like (factorial 5) transform from mere data into function calls that produce results. We explore how LFE's evaluator breathes life into these structures, turning symbols into function references and nested lists into complex program flows.

The concept of treating code as data and the manipulation and evaluation thereof becomes particularly powerful when we examine processes and message passing. In LFE, not only can you construct programs as data, but you can also send code between processes, enabling sophisticated distributed computing patterns. A process might receive a data structure that represents a function, evaluate it locally, and send the results elsewhere in the system.

The later chapters of Part III demonstrate how data-as-code principles scale to larger programming constructs. Modules and packages organize collections of functions and data, while still maintaining the fundamental property that program structure can be analyzed and manipulated as data. This makes metaprogramming easier than in a language without this property, as you can write LFE programs that generate, modify, or analyze other LFE programs using the same data manipulation techniques you use for ordinary programming tasks.

By Part III's conclusion, you will understand not _just_ how to write LFE programs, but how LFE programs can write themselves—the ultimate expression of the data-as-code philosophy that makes Lisp family languages uniquely powerful for both practical programming and theoretical computer science.RetryClaude can make mistakes. Please double-check cited sources.
