# Maps vs Records: The Eternal Question

You might be wondering: "If records are just syntactic sugar over tuples, and we have maps, why use records at all?"

Excellent question. Here's the philosophical breakdown:

**Use Records When:**
- You know all possible fields at compile time
- The structure is relatively small (< 20 fields)
- Performance matters (records are slightly faster than maps)
- You want compile-time field checking
- The fields are always atoms

**Use Maps When:**
- Fields are dynamic or unknown at compile time
- You need arbitrary keys (not just atoms)
- You're working with JSON or external data
- You want more flexibility at the cost of less compile-time checking
- You're building something where the data structure evolves frequently

Records are like strongly-typed structs. Maps are like flexible dictionaries. Choose according to your needs and tolerance for compiler complaints.
