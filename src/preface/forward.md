# Forward

The original Lisp programming language was implemented nearly 65 years ago[^1] at MIT (Massachusetts Institute of Technology), as part of the work done by John McCarthy and Marvin Minsky at the then-nascent AI (Artificial Intelligence) Lab. In the intervening half-century, the original Lisp evolved and experienced significant transformation in a technological diaspora originally fuelled by an explosion of research in the field of artificial intelligence. Through this, the industry witnessed 40 years of development where countless independent Lisp implementations were created. A small sampling of these number such favourites as Lisp 1.5, MacLisp, ZetaLisp, Scheme, Common Lisp, and ISLISP. However, the early 1990s saw the beginning of what would become the AI winter, and Lisp declined into obscurity – even notoriety – as graduating computer scientists were ushered into the “enterprise” world of Java, never to look back.

Except some did. By the early to mid-2000s, groundwork was being laid for what is now being recognized as a “Lisp renaissance.” The rediscovery of Lisp in the new millennium has led to the creation of a whole new collection of dialects, many implemented on top of other languages: Clojure on Java; LFE (Lisp Flavoured Erlang) and Joxa on Erlang; Hy on Python; Gherkin on Bash. New languages such as Go and Rust have several Lisp implementations, while JavaScript seems to gain a new Lisp every few years. While all of these ultimately owe their existence to the first Lisp, conceived in 1956[^2] and defined in 1958[^3], they represent a new breed and a new techno-ecological niche for Lisp: bringing the power of meta-programming and the clarity of near syntaxlessness[^4] to established language platforms. Whereas in the past Lisp has been an either-or choice, the new era of Lisps represents a symbiotic relationship between Lisp and the language platforms or VMs (virtual machines) upon which they are implemented; you now can have both, without leaving behind the accumulated experiences and comfort of your preferred platform.

Just as the Lisp implementations of the 1960s were greatly impacted by the advent of time-sharing computer systems, the new Lisps mentioned above like Clojure and LFE have been deeply influenced not only by their underlying virtual machines, but – more importantly – by the world view which the creators and maintainers of those VMs espoused. For the Erlang ecosystem of BEAM (Bogdan/Björn's Erlang Abstract Machine) languages, the dominant world view is the primacy of highly-concurrent, fault-tolerant, soft real-time, distributed systems. Erlang was created with these requirements in mind, and LFE inherits this in full. As such, LFE is more than a new Lisp; it is a language of power designed for creating robust services and systems. This point bears some discussion in order to properly prepare the intrepid programming language enthusiast who wishes to travel through the dimensions of LFE. 

Unlike languages whose prototypical users were developers working in an interactive shell engaged in such tasks as solving math problems, Erlang's prototypical “user” was a telecommunications device in a world where downtime was simply unacceptable. As such, Erlang's requirements and constraints were very unusual when compared to most other programming languages of its generation.[^5] Erlang, and thus its constellation of dialects, was designed from the start to be a programming language for building distributed systems, one where applications created with it could survive network and systems catastrophes, millions of processes could be sustained on a single machine, where tens and hundreds of thousands of simultaneous network connections could be supported, where even a live, production deployment could have its code updated without downtime. Such is the backdrop against which the Erlang side of the LFE story unfolds – not only in the teaching and learning of it, but in its day-to-day use, and over time, in the minds of its developers.

When bringing new developers up to speed, this perspective is often overlooked or set aside for later. This is often done intentionally, since one doesn't want to overwhelm or discourage a newcomer by throwing them into the “deep end” of distributed systems theory and the critical minutia of reliability. However, if we ignore the strengths of LFE when teaching it, we do our members as well as ourselves a disservice that leads to much misunderstanding and frustration: “Why is LFE so different? You can do X so much more simply in language Y”. It should be stated quite clearly in all introductory materials that the BEAM languages are not like other programming languages; in many ways, the less you rely upon your previous experiences with C, Java, Python, Ruby, etc., the better off you will be.

When compared to mainstream programming languages, Erlang's development is akin to the divergent evolution of animals on a continent which has been completely isolated for hundreds of millions of years. For instance, programming languages have their “Hello, world”s and their “first project”s. These are like lap dogs for newcomers to the programming world, a distant and domesticated version of their far more powerful ancestors. Though each language has its own species of puppy to help new users, they are all loyal and faithful companions which share a high percentage of common genetic history: this is how you print a line, this is how you create a new project. Erlang – the Down Under of programming languages – has its “Hello, world”s and “first project”s, too. But in this case, the lapdog does not count the wolf in its ancestral line. It's not even a canid.[^6] It's a thylacine[^7] with terrifying jaws and and unfamiliar behaviours. Its “hello world” is sending messages to thousands of distributed peers and to nodes in supervised, monitored hierarchies. It has just enough familiarity to leave one feeling mystified by the differences and with the understanding that one is in the presence of something mostly alien.

_That_ is the proper context for learning Erlang when coming from another programming language. 

In LFE, we take that a step further by adding Lisp to the mix, supplementing a distributed programming language platform with the innovation laboratory that is Lisp. Far from making the learning process more difficult, this algebraic, time-honoured syntax provides an anchoring point, a home base for future exploration: it is pervasive and familiar, with very few syntactical rules to remember. We have even had reports of developers more easily learning Erlang via LFE. 

In summary, by the end of this book we hope to have opened the reader's eyes to a new world of promise: distributed systems programming with a distinctly 1950s flavour – and a capacity to create applications that will thrive for another 50 years in our continually growing, decentralized technological world.

<i>
Duncan McGreggor<br>
2015, Lakefield, MN &<br>
2023, Sleepy Eye, MN
</i>

----

#### Notes

[^1] The first draft of this forward which was written in 2015 said "almost 60 years ago" but was never published. Today, at the end of 2023, this content is finally seeing the light of day, with the origins of Lisp receding further into the past ...

[^2] See McCarthy's 1979 paper History of Lisp, in particular the section entitled “LISP prehistory - Summer 1956 through Summer 1958”.

[^3] Ibid., section “The implementation of LISP”.

[^4] As you learn and then take advantage of Lisp's power, you will find yourself regularly creating new favourite features that LFE lacks. The author has gotten so used to this capability that he has applied this freedom to other areas of life. He hopes that you can forgive the occasional English language hack.

[^5] In fact, in the 1980s when Erlang was born, these features were completely unheard of in mainstream languages. Even today, the combination of features Erlang/OTP (Open Telecom Platform) provides is rare; an argument can be made that Erlang (including its dialects) is still the only language which provides them all.

[^6] The family of mammals that includes dogs, wolves, foxes, and jackals, among others.

[^7] An extinct apex predator and marsupial also known as the Tasmanian tiger or Tasmanian wolf.
