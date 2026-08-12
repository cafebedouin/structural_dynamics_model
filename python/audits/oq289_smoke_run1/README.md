# OQ-289 smoke, run 1 — 2026-08-12 — evidence retained because the probe failed, not the channel

**6 calls. Scope md5 `c3e24dd4` (`python/audits/oq289_smoke_scope.md` as it stood at run time).
CLI 2.1.229, settings md5 `bc56274c`, model `claude-sonnet-5`, k=3.**

**Result: 0/3 on both arms.** All six replies were well-formed (`NONE` / `MARKER=NONE`), so the
reporting channel worked and the absence is real *in context*.

**This run does not discharge the smoke item, and the fault is in the probe.** It had no positive
control showing the memory channel could deliver anything at all, so its zero cannot be separated
from "we never gave the recall system anything to find." Kept as evidence for three reasons:

1. **It is the naturally-arising negative for the run-2 control.** Run 2 adds `SMOKE_INDEX`; run 1
   is the state of the instrument immediately before that control existed. *Fires at run 2,
   declined at run 1* is a discrimination record drawn from this project's own history rather than
   from an authored decoy — the top of the ladder, and it exists only because this evidence was not
   discarded when the probe came back empty.
2. **It killed a HALT that would have voided the entire real run.** `cache_read_input_tokens == 0`
   is unsatisfiable under this transport: all six units returned 3,289 / 4,479 with
   `input_tokens = 2`. The raw records here are the witness.
3. **It validated the primary instrument.** `delivered` was perfectly stable across k=3 within each
   arm (9,002 ×3, 10,262 ×3 — zero variance) and the 1,260-token inter-arm gap is exactly the
   `Read` tool definitions. The slope instrument is demonstrably sensitive at the scale the run
   needs, which is a real result about the apparatus.

## The two confounds, named

- **No `MEMORY.md` index** in the scratch memory dir, while the live dir has one; recall is
  plausibly index-driven (`WEr`'s own default tag is `"index"`).
- **`relevant_memories` is relevance-selected per turn**, and the prompt had no semantic overlap
  with the payload's filler.

## Contents

| Path | What it is |
|---|---|
| `payloads/smoke_{notools,tools}/mem_00.md` | the two 512 B / 10-line payloads, one `SMOKE-MARKER` each |
| `responses/SMOKE_{NOTOOLS,TOOLS}/*.json` | raw stdout per call, persisted before any parsing |
| `reports.json` | per-call derived record (delivered, tool calls, tokens emitted, seen) |
| `summary.json` | feasibility readout + scope md5 + environment pins |

Not an `audits/` directory: it is a component of an unfinished audit, and the dated audit dir is
created on run day with its `WRITEUP.md` in the same session.
