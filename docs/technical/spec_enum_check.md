# spec_enum_check.py — pins, extraction assumptions, and failure semantics

`python/spec_enum_check.py` is the gate check ("spec enums" in `scripts/gate.sh`) that keeps
the CS spec's machine-checkable enumerations in sync with code. Adopted 2026-08-06 with the v6
reissue, after manual sync failed structurally twice (two pattern atoms at v5.2; six subsystems
by v6). This note covers what a gate RED means, which code edits perturb the extraction, and
the two silent edges the checker does NOT cover.

## What it does

The spec (`docs/commitment_systems/commitment_systems_sketch_v6.md`) marks each enumeration
with a fenced sentinel block:

```
<!-- spec-enum: cs_terminals -->
``` (fence)
axiom_foreclosure
...
``` (fence)
<!-- /spec-enum -->
```

The checker holds a manifest of 8 required block names, extracts each block, re-derives the
same enumeration from a code pin, and diffs. `--check` (gate mode) first runs its own positive
controls on mutated copies of the real spec — add-atom, remove-atom, deleted-sentinel — then
the real diff. `--list` dumps the code-side extraction for debugging.

## The pin table

| Block | Code pin | Extraction |
|---|---|---|
| `cs_terminals` | `tests/test_cs_drift_engine.pl` | `PinnedTerminals = [...]` list |
| `cs_gap_directions` | `tests/test_cs_drift_engine.pl` | `PinnedDirections = [...]` list |
| `cs_gap_magnitudes` | `tests/test_cs_drift_engine.pl` | first `member(M, [...])` |
| `cs_patterns` | `cs_corpus_analysis.pl` | `all_cs_patterns([...])` list |
| `cs_verdicts` | `cs_pattern_detection.pl` | `^cs_verdict(Var, atom)` clause heads |
| `cs_obstruction_statuses` | `cs_kernel_registry.pl` | every `Status = <atom>` in the file |
| `cs_trifurcation_types` | `cs_trifurcation.pl` | `trif_dispatch/4` third-arg literals + every `Type = <atom>` |
| `cs_attractor_table` | `cs_drift_engine.pl` | `cs_terminal_attractor/4` clause heads + `\=` guards, normalized |

Terminals/directions pin to the TEST's lists, not the engine's clauses — deliberate: the
`terminal_set_pinned` plunit tripwire already keeps test == engine, so the checker inherits
that guarantee and both tripwires fire together on a table edit.

Attractor-table normalization: variables render `*`, a `Var \= atom` guard renders `*!=atom`,
row format `direction | magnitude | ack -> terminal`, compared as sorted sets. 16 rows tile the
42-combo space; disjointness/coverage are proven by the plunit suite, NOT by this checker —
this checker only pins row CONTENT.

## Edits that perturb extraction (read before editing the pinned files)

- **`cs_kernel_registry.pl`:** the obstruction pin is every `Status = <atom>` occurrence in
  the WHOLE file. Writing a new predicate that binds a variable literally named `Status` to an
  atom adds that atom to the code-side enum — gate RED with a confusing "in code, not in spec"
  naming your unrelated atom (or, if it coincides with an existing status atom, a silent
  no-op). Rename the variable or update the extractor.
- **`cs_trifurcation.pl`:** same shape for `Type = <atom>` plus literal third arguments of
  `trif_dispatch(` clauses.
- **`cs_pattern_detection.pl`:** verdict heads are matched at line start (`^cs_verdict(`).
  A clause head reformatted to not start at column 0, or split before the verdict atom, drops
  out of the extraction — RED as "in spec, not in code" for a verdict that still exists.
- **`tests/test_cs_drift_engine.pl`:** renaming `PinnedTerminals`/`PinnedDirections` or
  reordering so a different `member(M, [...])` comes first breaks or repins magnitude/terminal
  extraction. "code pin not found" RuntimeErrors fail the gate loudly.
- **Prolog comments are stripped** (naive `%`-split; no quoted-atom handling — fine for these
  files, revisit if a pinned file ever holds `%` inside a quoted atom).
- **The spec side:** blocks are matched by the exact sentinel pair; block content is one
  atom/row per line, order-insensitive. A sentinel with a typo'd name yields TWO errors
  (missing required block + unknown enum name) — fix the name, don't add a manifest entry.

## Failure semantics

- Any missing sentinel block → `MISSING SENTINEL` RED (a rewritten section that drops its
  block can never go silently green on nothing-to-diff).
- Divergent content → `DIVERGENT` RED naming the enum and both one-sided diffs.
- Spec file absent → RED (this is why the checker landed one commit BEFORE the v6 doc; it is
  wired into the gate only from the doc's commit onward).
- If the real spec already lacks the selftest's target block, the selftest reports itself
  inapplicable and falls through to the main check's clean `MISSING SENTINEL` (not a traceback).

## The two silent edges (what the checker does NOT cover)

1. **New enumerations are opt-in.** The manifest lists 8 names. A NEW CS enumeration (a new
   subsystem's status vocabulary, say) is unguarded until you add BOTH its sentinel block in
   the spec AND its extractor + manifest entry in the checker, in the same change. Same
   silent-escape shape as `reading_registry.pl` registration (CLAUDE.md, Running the System).
2. **Prose is not pinned.** The checker pins enumerations only. A spec sentence that
   mis-describes semantics (which row maps where is pinned; WHY it maps there is not) drifts
   silently — that is what the write-time re-verification discipline in the v6 preamble is for.

## Provenance

Built at the v6 reissue (commits `81d561bc` checker, `01fad750` doc+wiring); witnessed RED on
all three mutation shapes and GREEN on the real doc same-day (KNOWN_STATE 2026-08-06). Operator
review bar that shaped it: tripwire over manual diff; the deleted-sentinel selftest is the one
that matters most; the largest enumeration (the attractor table) must not be the one exempt
from the instrument built for enumerations.
