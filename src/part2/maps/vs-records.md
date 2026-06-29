# Maps vs. Records: A Comparison of Philosophies

Records are compile-time constructs—tuples with aspirations. They're efficient, yes, but they're also rigid. Add a field to a record definition, and suddenly every module that uses it needs recompilation. It's like trying to add a room to your house and discovering you need to rebuild the entire neighborhood.

Maps, on the other hand, are runtime entities. They're flexible, introspective, and they don't require a memo to the compiler every time you want to add a key. Use records when you know precisely what fields you need and performance is paramount. Use maps when flexibility matters or when you're building dynamic data structures.

As one of the source texts wisely notes: "For the core of your process loop, when you know all keys that should exist, a record would be a smart choice, performance-wise."
