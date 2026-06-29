# Proplists in the Wild

## File Operations

```lfe
;; Open a file with options
(file:open "guide.txt" '(read binary (read_ahead 4096)))

;; Write with encoding
(file:write_file "data.txt" content '((encoding utf8)))
```

## Network Options

```lfe
;; Socket options
(inet:setopts socket '((active true) 
                       binary 
                       (packet 4) 
                       (nodelay true)))

;; HTTP client options
(httpc:request 'get 
               (tuple url '()) 
               '((timeout 5000) (connect_timeout 2000)) 
               '())
```

## Process Options

```lfe
;; Spawn with options
(spawn_opt fun '(link monitor (priority high)))

;; System info with options
(erlang:system_info '(version wordsize))
```

The Erlang standard library uses proplists extensively for optional arguments, a pattern that has proven robust enough to survive multiple decades and the introduction of competing data structures. This is either a testament to their utility or an example of path dependence, depending on one's philosophical stance toward legacy code.
