# Example 3: Log File Format with Binary Efficiency

Let's create a binary log file format that's both efficient and easy to parse:

```lfe
(defmodule binary-logger
  (export (create-log-file 1)
          (write-log-entry 4)
          (read-log-file 1)))

;; Log file format:
;; Header: <<"BLOG", Version:8, Flags:8, Reserved:16>>
;; Each entry: <<Timestamp:64, Level:8, Length:16, Message/binary>>

(defun file-header-magic () <<"BLOG">>)
(defun file-version () 1)

(defun create-log-file (filename)
  "Creates a new binary log file with appropriate header."
  (let ((header (binary
                  ((file-header-magic) binary)
                  ((file-version) 8)
                  (0 8)   ; Flags (currently unused)
                  (0 16)))) ; Reserved
    (file:write_file filename header)))

(defun write-log-entry (filename level msg-string context)
  "Appends a log entry to the file.
  Level: 'debug | 'info | 'warning | 'error | 'critical
  Returns: ok | error"
  (let* ((timestamp (erlang:system_time 'millisecond))
         (level-num (level-to-number level))
         (msg-bin (term-to-binary (tuple msg-string context)))
         (msg-len (byte_size msg-bin))
         (entry (binary
                  (timestamp 64)
                  (level-num 8)
                  (msg-len 16)
                  (msg-bin binary))))
    (case (file:open filename '(append raw binary))
      ((tuple 'ok device)
       (let ((result (file:write device entry)))
         (file:close device)
         result))
      ((tuple 'error reason)
       (tuple 'error reason)))))

(defun level-to-number (level)
  "Converts log level atom to numeric representation."
  (case level
    ('debug 1)
    ('info 2)
    ('warning 3)
    ('error 4)
    ('critical 5)
    (_ 0)))  ; Unknown

(defun number-to-level (num)
  "Converts numeric level to atom."
  (case num
    (1 'debug)
    (2 'info)
    (3 'warning)
    (4 'error)
    (5 'critical)
    (_ 'unknown)))

(defun read-log-file (filename)
  "Reads all entries from a binary log file.
  Returns list of entries or error."
  (case (file:read_file filename)
    ((tuple 'ok binary)
     (case (parse-log-header binary)
       ((tuple 'ok rest)
        (parse-log-entries rest '()))
       ('error
        (tuple 'error 'invalid-header))))
    ((tuple 'error reason)
     (tuple 'error reason))))

(defun parse-log-header (binary)
  "Validates log file header."
  (case binary
    ((binary
       ((magic 4 binary) (version 8) (_flags 8) (_reserved 16)
        (rest binary)))
     (if (and (== magic (file-header-magic))
              (== version (file-version)))
         (tuple 'ok rest)
         'error))
    (_ 'error)))

(defun parse-log-entries
  ;; Base case: no more data
  (('() acc)
   (tuple 'ok (lists:reverse acc)))
  ;; Parse next entry
  ((binary acc)
   (case binary
     ;; Successfully parse an entry
     ((binary (timestamp 64) (level-num 8) (msg-len 16)
              (msg-bin msg-len binary) (rest binary))
      (let* ((level (number-to-level level-num))
             ((tuple msg-str context) (binary-to-term msg-bin))
             (entry (map 'timestamp timestamp
                        'level level
                        'message msg-str
                        'context context)))
        (parse-log-entries rest (cons entry acc))))
     ;; Not enough data for a complete entry
     (_
      (tuple 'ok (lists:reverse acc))))))
```

Example usage:

```lfe
lfe> ;; Create a log file
lfe> (binary-logger:create-log-file "ship.log")
ok

lfe> ;; Write some log entries
lfe> (binary-logger:write-log-entry "ship.log" 'info
...>   "Improbability drive activated"
...>   (map 'drive-factor 'infinite 'status 'improbable))
ok

lfe> (binary-logger:write-log-entry "ship.log" 'warning
...>   "Tea dispenser offline"
...>   (map 'reason 'almost-but-not-quite-entirely-unlike-tea))
ok

lfe> (binary-logger:write-log-entry "ship.log" 'critical
...>   "Vogon poetry detected"
...>   (map 'threat-level 'severe 'evasive-action 'recommended))
ok

lfe> ;; Read the entire log
lfe> (binary-logger:read-log-file "ship.log")
#(ok
  (#M(timestamp 1730841234567
      level info
      message "Improbability drive activated"
      context #M(drive-factor infinite status improbable))
   #M(timestamp 1730841234789
      level warning
      message "Tea dispenser offline"
      context #M(reason almost-but-not-quite-entirely-unlike-tea))
   #M(timestamp 1730841235012
      level critical
      message "Vogon poetry detected"
      context #M(threat-level severe evasive-action recommended))))
```

## Why This Approach Wins

1. **Compact**: Binary encoding is much denser than JSON or XML
2. **Fast**: No parsing of text formats, just direct binary unpacking
3. **Typed**: Using `term-to-binary` preserves exact types
4. **Extensible**: The context map can contain anything
5. **Sequential**: Perfect for log files that are written once, read sequentially

This pattern appears in:

- Database transaction logs
- Event sourcing systems
- Network packet captures
- Embedded system logging
