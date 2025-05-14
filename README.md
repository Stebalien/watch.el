# watch.el

Watch a command and display the output in a buffer, similar to the Unix `watch` command but with Emacs integration.

## Overview

The `watch.el` package repeatedly runs a command, displaying the output in an Emacs buffer. Unlike the Unix `watch` command, it offers additional features:

1. Pause, resume, and immediately refresh the output.
2. Updates are temporarily paused while selecting text.
3. Adjust the refresh interval without restarting.
4. Supports ANSI escape sequences.

## Installation

### Use Package VC

```elisp
(use-package watch
  :vc (:url "https://github.com/Stebalien/watch.el"))
```

### Elpaca

Directly:

```elisp
(elpaca (watch :host github :repo "Stebalien/watch.el"))
```

Or with `use-package`:

```elisp
(use-package
  :ensure (watch :host github :repo "Stebalien/watch.el"))
```

### Straight

```elisp
(use-package watch
  :straight (watch :type git :host github :repo "Stebalien/watch.el"))
```

## Usage

`M-x watch RET <command> RET`

You'll be prompted for a command to watch and an interval between updates (default: 2 seconds).

## Key Bindings

In a watch buffer:

| Key | Description |
|-----|-------------|
| `q` | Quit the watch buffer |
| `g` | Refresh immediately |
| `i` | Change the update interval |
| `p` | Toggle pause/resume |

## Status Indicators

The header line shows the current status of the watch buffer:

- `<P>` - Paused
- `<R>` - Running (command is executing)
- `<S>` - Sleeping (between command runs)
- `<!>` - Failed (command exited with non-zero status)

## Customization

You can customize the various status indicators:

```elisp
(setopt watch-paused-indicator "[PAUSED]"
        watch-running-indicator "[RUNNING]"
        watch-sleeping-indicator "[SLEEPING]"
        watch-failed-indicator "[FAILED]")
```

For a complete list of options, run `M-x customize-group RET watch RET`.

## Requirements

- Emacs 29.1 or later

## License

GPL-3.0
