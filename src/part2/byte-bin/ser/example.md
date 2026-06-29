# A More Sophisticated Example: The Deep Space Signal

Let's imagine we're building a system for the Sirius Cybernetics Corporation's new line of spacecraft. We need to serialize complex telemetry data:

```lfe
(defun create-telemetry (ship-name velocity position)
  "Creates a telemetry packet for a spacecraft.
  Because even paranoid androids need to know where they are."
  (tuple 'telemetry
         (tuple 'timestamp (erlang:system_time 'millisecond))
         (tuple 'ship ship-name)
         (tuple 'velocity velocity)  ; meters per second
         (tuple 'position position)  ; (tuple x y z) in astronomical units
         (tuple 'mood 'vaguely-anxious)))

(defun serialize-telemetry (telemetry)
  "Converts telemetry to a transmissible format.
  Note: Does not include the answer to life, universe, and everything.
  That's a different packet."
  (term-to-binary telemetry))

(defun deserialize-telemetry (binary)
  "Reconstructs telemetry from binary format.
  May contain traces of existential dread."
  (binary-to-term binary))
```

Let's use these functions:

```lfe
lfe> (set heart-of-gold-pos (tuple 42.0 0.0 -100.0))
#(42.0 0.0 -100.0)

lfe> (set telemetry (create-telemetry
...>   "Heart of Gold"
...>   299792.458  ; Nearly the speed of light
...>   heart-of-gold-pos))
#(telemetry
  #(timestamp 1730841234567)
  #(ship "Heart of Gold")
  #(velocity 299792.458)
  #(position #(42.0 0.0 -100.0))
  #(mood vaguely-anxious))

lfe> (set binary-packet (serialize-telemetry telemetry))
#B(131 104 6 100 0 9 116 101 108 101 109 ...)

lfe> (set recovered (deserialize-telemetry binary-packet))
#(telemetry
  #(timestamp 1730841234567)
  #(ship "Heart of Gold")
  #(velocity 299792.458)
  #(position #(42.0 0.0 -100.0))
  #(mood vaguely-anxious))
```

The round-trip is perfect. Everything that went in comes back out, including the existential anxiety.
