# Conventions

## Typography

### Key Entry

We use the angle bracket convention to indicated typing actual key entries on the keyboard. For instance, when the reader sees `<ENTER>` they should interpret this as an actual key they should type. That that all keys are given in upper-case. If the reader is expected to use an upper-case "C"  rather than a lower-case "c", they will be presented with the key combination `<SHIFT><C>`.

### Code
Color syntax highlighting is used in this text to display blocks of code. The formatting of this display is done in such a way as to invoke in the mind of the reader the feeling of a terminal, thus making an obvious visual distinction in the text. For instance:

```lisp
(defun fib
  ((0) 0)
  ((1) 1)
  ((n)
    (+ (fib (- n 1))
       (fib (- n 2)))))
```

Examples such as this one are readily copied and may be pasted without edit into a file or even the LFE REPL itself.

For interactive code, we display the default LFE prompt the reader will see when in the REPL:

```lisp
lfe> (integer_to_list 42 2)
;; "101010"
```

We also distinguish the output from the entered LFE code using code comments.

For shell commands, the commands to enter at the prompt are prefixed by a `$` for the prompt. Input and any relevant output are provided as comment strings:

```shell
$ echo "I am excited to learn LFE"
## I am excited to learn LFE
```

## LiffyBot

<img class="liffy-bot-mascot" src="../../images/LiffyBot-5-x500-bold-color.png"/>This is a severly hoopy frood. With an attitude. He pops up from time to time, generally with good advice.

<br/>

## Messages of Note

From time to time you will see call-out boxes, aimed at drawing your attention to something of note. There are four differnt types of these:

* ones that share useful info (blue)
* ones that highlight something of a momentus nature (green)
* ones that offer warnings to tred carefully (orange)
* ones that beg you not to follow a particular path (red)

These messages will take the following forms:

<div class="alert alert-info">
  <h4 class="alert-heading">
    <i class="fa fa-info-circle" aria-hidden="true"></i>
    Information
  </h4>
  <p class="mb-0">
    Here you will see a message of general interest that could have a useful or even positive impact on your experience in programming LFE.

The icon associated with this type of message is the "i" in a circle.
  </p>
</div>

<br/>

<div class="alert alert-success">
  <h4 class="alert-heading">
    <img class="liffy-bot-alert" src="../../images/LiffyBot-5-x64-bold-black-solid.png"/>
    Amazing!
  </h4>
  <p class="mb-0">
    Here you will see a message of general celebration for sitations that warrant it, above and beyond the general celebration you will feel writing programs in a distributed Lisp.

The icon assocated with this type of message is that of LiffyBot.
  </p>
</div>

<br/>

<div class="alert alert-warning">
  <h4 class="alert-heading">
    <i class="fa fa-exclamation-triangle" aria-hidden="true"></i>
    Warning!
  </h4>
  <p class="mb-0">
    Here you will see a message indicating a known isssue or practice you should avoid if possible.

The icon assocated with this type of message is the "!" in a caution triangle.
  </p>
</div>

<br/>

<div class="alert alert-danger">
  <h4 class="alert-heading">
    <i class="fa fa-minus-circle" aria-hidden="true"></i>
    Danger!
  </h4>
  <p class="mb-0">
    Here you will see a message indicating something that could endanger the proper function of an LFE system or threaten the very existence of the universe itself.

The icon assocated with this type of message is "do not enter".
  </p>
</div>
