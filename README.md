# Timer in mode-line

Count down time and show remainder time at mode-line.
This is useful for schedule management and making instant noodle.


## Screenshot

![mode-line-timer](image/mode-line-timer.png)


## Interface

#### `mode-line-timer-start`

Input minutes and start timer.

#### `mode-line-timer-stop`

Stop timer.

#### `mode-line-timer-done`

Same as `mode-line-timer-stop` except running hook(`mode-line-expire-hook`).

## Customization

#### `mode-line-expire-hook`

A hook which runs after timer expired.
