# Tagged Tuples: A Convention Posing as Wisdom

Here's where things get interesting in that particularly Erlangy way that makes you wonder if the original implementers were secretly running a grand social experiment on future programmers.

Consider this: you have a tuple `#(182 "joe")`. Is it a person's height and name? Their weight in kg and favorite cartoon character? The number of pages in a book about someone called Joe? The universe, in its infinite inscrutability, refuses to tell you.

The solution, arrived at through centuries of trial, error, and probably some amount of head-scratching, is to stick an atom at the front as a kind of helpful label:

```lfe
#(person "joe" 182)
```

Ah! Now it's obviously a person, followed by their name, followed by... well, some number. Is it their height? Age? The number of times they've watched reruns of *Red Dwarf*? This is where convention enters, stage left, wearing a fake mustache.

The generally accepted approach (and by "generally accepted" we mean "people will look at you funny if you don't do it this way") is to make your tuples even more explicit:

```lfe
#(person #(name "joe") #(height 182) #(shoe-size 42) #(eye-color 'brown))
```

Notice how we've nested tuples like Russian dolls, except these dolls contain data instead of slightly smaller dolls. Each nested tuple starts with an identifying atom, making it blatantly obvious what each field represents. This is called "good style" in some circles and "bleeding obvious common sense" in others.
