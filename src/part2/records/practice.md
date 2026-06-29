# Records in Practice: A Complete Example

Let's build something vaguely useful—a system for managing robots, because robots are inherently more interesting than people and less likely to file complaints with Human Resources:

```lfe
(defmodule robots
  (export all))

(defrecord robot
  name
  (type 'industrial)
  hobbies
  (details '()))

(defun crusher ()
  (make-robot 
    name "Crusher"
    hobbies '("Crushing people" "Petting cats")))

(defun car-factory (corp-name)
  (make-robot 
    name corp-name
    hobbies "Building cars"))

(defun repairman (rob)
  (let ((details (robot-details rob)))
    (tuple 'repaired
           (set-robot-details rob 
             (cons "Repaired by repairman" details)))))

(defun show-robot
  (((match-robot name n hobbies h details d))
   (io:format "Robot: ~p~nHobbies: ~p~nDetails: ~p~n" 
              (list n h d))))
```

Using this module would look approximately like this:

```lfe
lfe> (set crusher (robots:crusher))
#(robot "Crusher" industrial ("Crushing people" "Petting cats") ())

lfe> (robots:show-robot crusher)
Robot: "Crusher"
Hobbies: ("Crushing people" "Petting cats")
Details: ()
ok

lfe> (set fixed-crusher (robots:repairman crusher))
#(repaired #(robot "Crusher" industrial ...))
```
