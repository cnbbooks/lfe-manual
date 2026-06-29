# The Default Values Pattern

One of the most elegant uses of proplists is the default-override pattern:

```lfe
(defun start-server (user-opts)
  (let ((defaults '((port 8080) 
                    (host "localhost") 
                    (timeout 30000) 
                    (max-connections 100))))
    (let ((opts (++ user-opts defaults)))
      ;; Use opts, knowing all required keys exist
      ;; User options override defaults due to left-to-right precedence
      (listen-on (proplists:get_value 'host opts)
                 (proplists:get_value 'port opts)))))
```

Because `get_value/2` returns the first matching value, prepending user options to defaults creates an elegant override mechanism. Compare this to the map equivalent:

```lfe
(defun start-server (user-opts)
  (let ((defaults (map 'port 8080 
                       'host "localhost" 
                       'timeout 30000 
                       'max-connections 100)))
    (let ((opts (maps:merge defaults user-opts)))
      ;; Use opts
      (listen-on (mref opts 'host)
                 (mref opts 'port)))))
```

Both patterns work well. The map version has cleaner access syntax; the proplist version has been doing this job for longer than some programmers have been alive.
