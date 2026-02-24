#!/bin/bash
set -euo pipefail

OUTFILE="${1:-tmux_layout_$(date +%Y%m%d_%H%M%S).tmux}"

SESSION="$(tmux display-message -p '#S')"

{
    echo "#!/bin/bash"
    echo "tmux new-session -d -s \"$SESSION\""

    tmux list-windows -F '#I:#W:#F' | while IFS=: read -r index name flags; do
        if [[ "$index" != "0" ]]; then
            echo "tmux new-window -t \"$SESSION:$index\" -n \"$name\""
        else
            echo "tmux rename-window -t \"$SESSION:$index\" \"$name\""
        fi

        # Get layout string (includes pane sizes)
        layout=$(tmux list-windows -t "$SESSION:$index" -F '#{window_layout}')
        echo "tmux select-layout -t \"$SESSION:$index\" '$layout'"
    done

    # Select active window & pane
    active_window=$(tmux display-message -p '#I')
    active_pane=$(tmux display-message -p '#P')

    echo "tmux select-window -t \"$SESSION:$active_window\""
    echo "tmux select-pane -t \"$SESSION:$active_window.$active_pane\""
    echo "tmux attach -t \"$SESSION\""

} > "$OUTFILE"

chmod +x "$OUTFILE"

echo "Saved tmux layout to: $OUTFILE"
