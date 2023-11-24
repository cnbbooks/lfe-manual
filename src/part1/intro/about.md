# About LFE

LFE is a modern programming language of two lineages, as indicated by the expansion of its acronym: _Lisp Flavoured Erlang_. In this book we aim to provide the reader with a comprehensive reference for LFE and will therefore explore both parental lines. Though the two language branches which ultimately merged in LFE are separated by nearly 30 years, and while LFE was created another 20 years after that, our story of their origins reveals that age simply doesn't matter. More significantly, LFE has unified Lisp and Erlang with a grace that is both simple and natural. This chapter is a historical overview on how Lisp, Erlang, and ultimately LFE came to be, thus providing a broad context for a complete learning experience.

## What Is LFE?

LFE is a Lisp dialect which is heavily flavoured by the programming language virtual machine upon which it rests, the Erlang VM.[^1] Lisps are members of a programming language family whose typical distinguishing visual characteristic is the heavy use of parentheses and what is called _prefix notation_.[^2] To give you a visual sense of the LFE language, here is some example code:

``` lisp
(defun remsp
  (('())
   '())
  (((cons #\  tail))
   (remsp tail))
  (((cons head tail))
   (cons head (remsp tail))))
```

This function removes spaces from a string that it passed to it. We will postpone explanation and analysis of this code, but in a few chapters you will have the knowledge necessary to understand this bit of LFE. 

Besides the parentheses and prefix notation, other substantial features which LFE shares with Lisp languages include the interchangeability of data with code and the ability to write code which generates new code using the same syntax as the rest of the language. Examples of other Lisps include Common Lisp, Scheme, and Clojure.

Erlang, on the other hand, is a language inspired most significantly by _Prolog_ and whose virtual machine supports not only Erlang and LFE, but also newer _BEAM_ languages including Joxa, Elixir, and Erlog. BEAM languages tend to focus on the key characteristics of their shared VM: fault-tolerance, massive scalability, and the ability to easily build soft real-time systems.

One way of describing LFE is as a programming language which unites these two. LFE is a happy mix of the serious, fault-tolerant philosophy of Erlang combined with the flexibility and extensibility offered by Lisp dialects. It is a _homoiconic_ distributed systems programming language with Lisp _macros_ which you will soon come to know in much greater detail.

## A Brief History

To more fully understand the nature of LFE, we need to know more about Lisp and Erlang – how they came to be and even more importantly, how they are used today. To do this we will cast our view back in time: first we'll look at Lisp, then we'll review the circumstances of Erlang's genesis, and finally conclude the section with LFE's beginnings.

### The Origins of Lisp

Lisp, originally spelled LISP, is an acronym for "**LIS**t **P**rocessor". The language's creator, John McCarthy, was inspired by the work of his colleagues who in 1956 created IPL (Information Processing Language), an assembly programming language based upon the idea of manipulating lists. Though initially intrigued with their creation, McCarthy's interests in artificial intelligence required a higher-level language than IPL with a more general application of its list-manipulating features. So after much experimentation with FORTRAN, IPL, and heavy inspiration from Alonzo Church's lambda calculus,[^3] McCarthy created the first version of Lisp in 1958.

In the 1950s and 1960s programming languages were actually created on paper, due to  limited computational resources. Volunteers, grad students, and even the children of language creators were used to simulate registers and operations of the language. This is how the first version of Lisp was “run”.[^4] Furthermore, there were two kinds of Lisp: students in the AI lab wrote a form called S-expressions, which was eventually input into an actual computer. These instructions had the form of nested lists, the syntax that eventually became synonymous with Lisp. The other form, called M-expressions, was used when McCarthy gave lectures or presented papers.[^5] These had a syntax which more closely resembles what programmers expect. This separation was natural at the time: for over a decade programmers had entered instructions using binary or machine language while describing these efforts in papers using natural language or pseudocode. McCarthy's students programmed entirely in S-expressions and as their use grew in popularity, the fate of M-expressions was sealed: they were never implemented.[^6]

The Lisp 1.5 programmer's manual, first published in 1962, used M-expressions extensively to introduce and explain the language. Here is an example function[^7] for removing spaces from a string input defined using M-expressions:

``` algol
remsp[string] = [null[string]→F;
              eq[car[string];" "]→member[cdr[string]];
              T→cons[car[string];remsp[cdr[string]]]]
```

The corresponding S-expression is what the Lisp programmer would actually enter into the IBM 704 machine that was used by the AI lab at MIT:[^8]

``` lisp
DEFINE ((
  (REMSP (LAMBDA (STRING)
    (COND ((NULL STRING)
           F)
          ((EQ (CAR STRING) " ")
           (REMSP (CDR STRING)))
          (T
           (CONS (CAR STRING)
                 (REMSP (CDR STRING)))))))))
```

The period from 1958 to 1962, when Lisp 1.5 was released, marked the beginning of a new era in computer science. Since then Lisp dialects have made an extraordinary impact on the design and theory of other programming languages, changing the face of computing history perhaps more than any other language group. Language features that Lisp pioneered include such significant examples as: homoiconicity, conditional expressions, _recursion_, meta-programming, _meta-circular evaluation_, _automatic garbage collection_, and first class functions. A classic synopsis of these accomplishments was made by the computer scientist of great renown, Edsger Dijkstra in his 1972 Turing Award lecture, where he said the following about Lisp:

<blockquote>
“With a few very basic principles at its foundation, [Lisp] has shown a remarkable stability. Besides that, Lisp has been the carrier for a considerable number of, in a sense, our most sophisticated computer applications. Lisp has jokingly been described as ‘the most intelligent way to misuse a computer’. I think that description a great compliment because it transmits the full flavour of liberation: it has assisted a number of our most gifted fellow humans in thinking previously impossible thoughts.”
</blockquote>

Lisp usage is generally described as peaking in the 80s and early 90s, experiencing an adoption setback with the widespread view that problems in artificial intelligence were far more difficult to solve than originally anticipated.[^9] Another problem which faced Lisp was related hardware requirements: specialized architectures were developed in order to provide sufficient computational power to its users. These were expensive with few vendors, and a slow product cycle.

In the midst of this Lisp cold-spell, two seminal Lisp books were published: On Lisp, and ANSI Common Lisp, both by famed entrepreneur Paul Graham.[^10] Despite a decade of decline, these events helped catalyse a new appreciation for the language by a younger generation of programmers and within a few short years, the number of Lisp books and Lisp-based languages began growing, giving the world the likes of _Practical Common Lisp_ and _Let Over Lambda_ in the case of the former, and Clojure and LFE, in the case of the latter.

### Constructing Erlang

Erlang was born in the heart of Ericsson's Computer Science Laboratory,[^11] just outside of Stockholm, Sweden.[^12] The lab had the general aim “to make Ericsson software activities as efficient as possible through the purposeful exploitation of modern technology.” The Erlang programming language was the lab's crowning achievement, but the effort leading up to this was extensive with a great many people engaged in the creation of many prototypes and the use of numerous of programming languages.

One example of this is the work that Nabiel Elshiewy and Robert Virding did in 1986 with Parlog, a concurrent logic programming language based on Prolog. Though this work was eventually abandoned, you can read the paper _The Phoning Philosopher's Problem or Logic Programming for Telecommunications Applications_ and see the impact its features had on the future development of Erlang. The paper provides some examples of Parlog usage; using that for inspiration we can envision what our space-removing program would looking like:[^13]

``` prolog
remsp([]) :-
  [].

remsp([$ |Tail]) :-
  remsp(Tail).

remsp([Head|Tail]) :-
  [Head|remsp(Tail)].
```

Another language that was part of this department-wide experimentation was Smalltalk. Joe Armstrong started experimenting with it in 1985 to model a telephone exchanges and used this to develop a telephony algebra with it. A year later, when his colleague Roger Skagervall showed him the equivalence between this and logic programming, Prolog began its rise to prominence, and the first steps were made towards the syntax of Erlang as the world now knows it. In modern Erlang, our program has the following form:

``` prolog
remsp([]) ->
  [];
remsp([$ |Tail]) ->
  remsp(Tail);
remsp([Head|Tail]) ->
  [Head|remsp(Tail)].
```

The members of the Ericsson lab who were tasked with building the next generation telephone exchange system, and thus involved with the various language experiments made over the course of a few years, came to the following conclusions:

* Small languages seemed better at succinctly addressing the problem space. 
* The functional programming paradigm was appreciated, if sometimes viewed as awkward. 
* Logic programming provided the most elegant solutions in the given problem space. 
* Support for concurrency was viewed as essential.
    
If these were the initial guideposts for Erlang development, its guiding principles were the following:

* To handle high-concurrency 
* To handle soft real-time constraints
* To support non-local, distributed computing 
* To enable hardware interaction 
* To support very large scale software systems 
* To support complex interactions 
* To provide non-stop operation
* To allow for system updates without downtime 
* To allow engineers to create systems with only seconds of down-time per year 
* To easily adapt to faults in both hardware and software

These were accomplished using such features as _immutable data structures_, _light weight processes_, no shared memory, message passing, _supervision trees_, and _heartbeats_. Furthermore, having adopted message-passing as the means of providing high-concurrency, Erlang slowly evolved into the exemplar of a programming paradigm that it essentially invented and even today, dominates: _concurrent functional programming_.

After four years of development from the late 80s into the early 90s, Erlang matured to the point where it was adopted for large projects inside Ericsson. In 1998 it was released as open source software, and has since seen growing adoption in the wider world of network- and systems-oriented programming.

### The Birth of LFE

One of the co-inventors of Erlang, and part of the Lab's early efforts in language experimentation was Robert Virding. Virding first encountered Lisp in 1980 when he started his PhD in theoretical physics at Stockholm University. His exposure to the language came as a result of the physics department's use in performing symbolic algebraic computations. Despite this, he spent more time working on micro-processor programming and didn't dive into it until a few years later when he was working at Ericsson's Computer Science Laboratory. One of the languages evaluated for use in building telephony software was Lisp, but to do so properly required getting to know it in-depth – both a the language level as well as the operating system level.[^14] It was in this work that Virding's passion for Lisp blossomed and he came to appreciate deeply its functional nature, macros, and homoiconicity – all excellent and time-saving tools for building complicated systems.

Though the work on Lisps did not become the focus of Erlang development, the seeds of LFE were planted even before Erlang itself had come to be. After 20 years of contributions to the Erlang programming language, these began to bear fruit. In 2007 Virding decided to do something fun in his down time: to see what a Lisp would look like if written on top of the Prolog-inspired Erlang VM. After several months of hacking, he announced a first version of LFE to the Erlang mail list in early 2008.

A few years latter, when asked about the origins of LFE and the motivating elements behind his decision to start the project, Virding shared the following on the LFE mail list: 

* It had always been a goal of Robert's to make a Lisp which could fully interact with Erlang/OTP, to see what it would look like and how it would run.
* He was looking for some interesting programming projects that were not too large to do in his spare time. 
* He thought it would be a fun, open-ended problem to solve with many interesting parts.
* He likes implementing languages. 

We showed an example of LFE at the beginning of this chapter; in keeping with our theme for each language subsection, we present it here again, though in a slightly altered form:

``` lisp
(defun remsp
  (('())
   '())
  ((`(32 . ,tail))
   (remsp tail))
  ((`(,head . ,tail))
   (cons head (remsp tail))))
```

## What is LFE Good For?

Very few languages have the powerful capabilities which Erlang offers – both in its standard library as well as the set of Erlang libraries, frameworks, and patterns that are provided in _OTP_. This covers everything from fault-tolerance, scalability, soft real time capacity, and high-availability to proper design, component assembly, and deployment in distributed environments.

Similarly, despite the impact that Lisp has had on so many programming languages, its full suite of features is still essentially limited to Lisp dialects. This includes the features we have already mentioned: the ability to treat code as data, easily generate new code from data, as well as the interrelated power of writing macros – the last allows developers to modify the language to suit their needs. These rare features from two different language branches are unified in LFE and there is no well-established language that provides the union of these.

As such, LFE gives developers everything they need to envision, prototype, and then build distributed applications – ones with unique requirements that no platform provides and which can be delivered thanks to LFE's language-building capabilities. 

To paraphrase and augment the opening of Chapter 1 in _Designing for Scalability with Erlang/OTP_:

<blockquote>
“You need to implement a fault tolerant, scalable soft real time system with requirements for high availability. It has to be event driven and react to external stimulus, load and failure. It must always be responsive. You also need language-level features that don't exist yet. You would like to encode your domain's best practices and design patterns seamlessly into your chosen platform.”
</blockquote>

LFE has everything you need to realize this dream ... and so much more.

## In Summary

### What LFE Is!

Here's what you can expect of LFE:

* A proper Lisp-2, based on the features and limitations of the Erlang VM
* Compatibility with vanilla Erlang and OTP
* It runs on the standard Erlang VM

Furthermore, as a result of Erlang's influence (and LFE's compatibility with it), the following hold:

* there is no global data
* data is not mutable
* only the standard Erlang data types are used
* you get pattern matching and guards
* you have access to Erlang functions and modules
* LFE has a compiler/interpreter
* functions with declared arity and fixed number of arguments
* Lisp macros

### What LFE Isn't

Just to clear the air and set some expectations, we'll go a step further. Here's what you're _not_ going to find in LFE:

* An implementation of Scheme
* An implementation of Common Lisp
* An implementation of Clojure

As such, you will not find the following:

* A Scheme-like single namespace
* CL packages or munged names faking packages
* Access to Java libraries


----

#### Notes

[^1] Robert Virding, the creator of LFE and one of the co-creators of the Erlang programming language, has previously stated that, were he to start again, he would name his Lisp dialect EFL, since it truly is a Lisp with an Erlang flavour, rather than the other way round.

[^2] We will be covering prefix notation when we cover symbolic expressions later in the book.

[^3] Alonzo Church was one of McCarthy's professors at Princeton. McCarthy did not use all of the lambda calculus when creating Lisp, as there were many esoteric aspects for which he had no practical need.

[^4] In the case of Lisp, university students were the primary computer hardware ... and sometimes even high school students (see REPL footnote below).

[^5] This approach was not uncommon at the time: the ALGOL 58 specification defined a syntax for the language reference, one for publications, and a third for implementation.

[^6] The single greatest contributor to the ascendance of the S-expression is probably the invention of the REPL by L Peter Deutsch, which allowed for interactive Lisp programming. This was almost trivial in S-expressions, whereas a great deal of effort would have been required to support a similar functionality for M-expressions.

[^7] The function we use in this chapter to demonstrate various syntaxes and dialects was copied from the cover of Byte Magazine's August 1979 issue which focused on Lisp and had part of a Lisp 1.5 program on its cover.

[^8] The formatting applied to the S-expression version of the function is a modern convention, added here for improved readability. There was originally no formatting, since there was no display – a keypunch was used to enter text on punchcards, 80 characters at a time. As such, a more historically accurate representation would perhaps be: `DEFINE (((REMSP (LAMBDA (STRING) (COND ((NULL STRING) F) ((EQ (CAR STRING) " ") (REMSP (CDR STRING))) (T (CONS (CAR STRING) (REMSP (CDR STRING)))))))))`

[^9] This time period is commonly referred to as the “AI winter”.

[^10] Paul Graham sold his Lisp-based e-commerce startup to Yahoo! In 1998.

[^11] The majority of this section's content was adapted from Joe Armstrong's paper “A History of Erlang” by, written for the HOPL III conference in 2007.

[^12] The Computer Science Laboratory operated from 1982 to 2002 in Älvsjö, Stockholm.

[^13] We've taken the liberty of envisioning the Parlog of 1986 as one that supported pattern matching on characters.

[^14] One of Virding's project aims was to gain a deeper understanding of Lisp internals. As part of this, he ported the Lisp Machine Lisp object framework _Flavors_ to Portable Standard Lisp running on UNIX. His work on this project contributed to his decision to use _Flavour_ as part of the name for LFE (spelling divergence intentional).
