# RLM

Two things:

1. **An experimental Common Lisp take on [Recursive Language Models](https://alexzhang13.github.io/blog/2025/rlm/)** ([paper/repo](https://github.com/alexzhang13/recursive-lm)) — an inference strategy where an LLM operates inside a REPL sandbox, writing code to explore context programmatically rather than consuming it directly in the prompt. The original work is in Python with fine-tuned models. This reimplements the architecture in Common Lisp using off-the-shelf models via OpenRouter.

2. **An agentic editing mode for [Lem](https://lem-project.github.io/)** — a text editor written in Common Lisp. The `rlm/mode` system integrates the agent loop into Lem as an interactive chat buffer with tools for reading, editing, and navigating buffers. Think Cursor/Copilot-style AI assistance, but inside Lem.

The core RLM idea is that the model receives a question and knows context exists as a variable, but never sees it raw. Instead it writes code to inspect, filter, and transform the context across multiple iterations until it reaches an answer. This prevents context window saturation and lets the model develop adaptive strategies (grepping, chunking, recursive sub-queries) at inference time.

**Status:** Experimental. Without a fine-tuned model, general-purpose LLMs don't naturally take full advantage of the REPL sandbox — they tend to reason in prose rather than writing code to explore context programmatically. The architecture works, but getting the most out of it likely requires model-level training as described in the paper. 

The *Lem* integration is more immediately useful — it works well as an interactive coding and research assistant regardless of how "recursive" the model's behavior is.

## Architecture

```
src/                    # "rlm" system — core library
├── client.lisp         # OpenRouter API client (*api-key*, *model*, chat-completion)
├── sandbox.lisp        # Restricted CL execution (whitelisted symbols, safe readtable)
├── repl.lisp           # Environment: tools, variables, history, helper functions
├── prompts.lisp        # System/iteration message builders
├── tools.lisp          # Built-in tools (read-file, list-directory, web-read, web-search)
├── rlm.lisp            # Agent loop (query → LLM → extract code → execute → repeat)
└── cli.lisp            # Interactive terminal REPL with /commands

mode/                   # "rlm/mode" system — Lem editor integration
├── rlm-mode.lisp       # Major mode, commands, event streaming, listener buffer
└── rlm-tools.lisp      # Editor tools (read-buffer, edit-buffer, insert-at, ask-user, etc.)
```

## Setup

Requires [SBCL](http://www.sbcl.org/) and [qlot](https://github.com/fukamachi/qlot).

```bash
cd ~/rlm
qlot install
```

Set your API key (OpenRouter by default):

```bash
export OPENROUTER_API_KEY=sk-or-...
```

### CLI usage

```bash
sbcl --load rlm-cli.lisp
```

### Lem integration

Add to `~/.lem/init.lisp`:

```lisp
(pushnew (merge-pathnames "rlm/" (user-homedir-pathname))
         asdf:*central-registry* :test #'equal)
(asdf:load-system "rlm/mode")
```

This registers both the `"rlm"` and `"rlm/mode"` ASDF systems from the single `rlm.asd` file.

## Lem commands

| Command | Description |
|---------|-------------|
| `rlm-prompt` | Open the `*RLM*` chat buffer |
| `rlm-ask-about-buffer` | Send current buffer as context, ask a question |
| `rlm-ask-about-region` | Send selected region as context, ask a question |
| `rlm-perform-task` | Agentic editing — the model makes edits to the current buffer |
| `rlm-switch-model` | Change model with completion |
| `rlm-toggle-yeet-mode` | Toggle auto-accept edits (off by default) |
| `rlm-cancel` | Cancel a running query |

### Chat buffer commands

Type these in the `*RLM*` buffer prompt:

- `/reset` — clear the environment and start fresh
- `/model` — show current model
- `/model <name>` — switch model

### Edit confirmation

When the agent proposes a buffer edit, you get a single-keypress prompt:

- **a** — accept the edit
- **s** — skip (undo, agent moves on)
- **c** — request changes (undo, type feedback, agent revises)
- **h** — halt (undo, cancel the entire query)

Toggle `rlm-toggle-yeet-mode` to auto-accept all edits without prompting.

## Editor tools available to the agent

The agent can call these from within the REPL sandbox:

- `read-buffer` — read buffer contents with line numbers
- `edit-buffer` — replace a range of lines
- `insert-at` — insert text before a line
- `list-buffers` — list all open buffers
- `open-file` — open a file into a buffer
- `save-buffer` — save a buffer to disk (prompts for confirmation)
- `present` — show markdown content in a read-only buffer
- `ask-user` — ask the user a question
- `read-file` / `list-directory` — filesystem access
- `web-read` / `web-search` — web access (requires Jina API key)

## License

GPLv3. See [LICENSE](LICENSE).

## References

- [Recursive Language Models (Alex Zhang, 2025)](https://alexzhang13.github.io/blog/2025/rlm/)
- [Lem editor](https://lem-project.github.io/)
