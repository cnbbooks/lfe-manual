# Priorities

When making decisions about how to write a given piece of code, aim for the following -ilities in this priority order:

* Usability by the customer
* Debuggability/Testability
* Readability/Comprehensibility
* Extensibility/Modifiability
* Efficiency (of the LFE code at runtime)

Most of these are obvious.

Usability by the customer means that the system has to do what the customer requires; it has to handle the customer's transaction volumes, uptime requirements; etc.

For the LFE efficiency point, given two options of equivalent complexity, pick the one that performs better. (This is often the same as the one that conses less, i.e. allocates less storage from the heap.)

Given two options where one is more complex than the other, pick the simpler option and revisit the decision only if profiling shows it to be a performance bottleneck.

However, avoid premature optimization. Don't add complexity to speed up something that runs rarely, since in the long run, it matters less whether such code is fast.
