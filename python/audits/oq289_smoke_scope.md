# OQ-289 SMOKE — declared scope, written BEFORE the probe

**Status:** operator-approved 2026-08-12. This is a **feasibility** probe, not a first look
at the hypothesis. Its md5 is persisted with every smoke artifact by
`oq289_recall_canary.py:assert_smoke_go()`.

## Why this runs before the freeze

The pre-registration names Arm A. **Whether Arm A is runnable at all depends on a fact
nobody has**: does `--tools ""` also suppress the `relevant_memories` attachment, and does
per-turn recall fire under `-p` at all? Freezing a prereg that names an unrunnable test
forces an amendment, and **an amended freeze is a weaker instrument than one frozen a day
later.** So: smoke first, freeze after.

That reordering is only legitimate if smoke carries **no information about the
hypothesis.** Everything below exists to make that true.

## Run 1 (2026-08-12) — executed, and it did NOT discharge this item

Two arms, 6 calls, scope md5 `c3e24dd4`. Result: **0/3 on BOTH arms** (`NONE` returned
verbatim and well-formed each time, so the reporting channel was working).

**That null was UNINTERPRETABLE, and the fault was in the probe.** Nothing in it showed the
memory channel could deliver anything at all, so "the marker did not arrive" could not be
separated from "we never gave the recall system anything to find." Two live confounds:

1. **No `MEMORY.md` index in the scratch memory dir.** The live dir has one; recall is
   plausibly index-driven (`WEr`'s own default tag is `"index"`). There may have been
   nothing to select *from*.
2. **`relevant_memories` is relevance-selected per turn**, and the prompt had no semantic
   overlap with the payload's filler.

*"I didn't find it" is a fact about the search until the search is shown to find.* The probe
had no positive control and therefore could not license its own negative.

**Two things run 1 DID establish, both about the instrument rather than the hypothesis, and
both in scope:**

- **The token-slope instrument is sound and demonstrably sensitive.** `delivered` was
  *perfectly* stable across k=3 within each arm — 9,002 ×3 and 10,262 ×3, zero variance —
  and the 1,260-token gap between arms is exactly the `Read` tool definitions.
- **The `cache_read_input_tokens == 0` assertion is UNSATISFIABLE under this transport.**
  All six units returned `cache_read` of 3,289 / 4,479 with `input_tokens = 2`; the CLI
  caches the system prompt. As specified that HALT would have voided **every rung** of the
  real run — a gate that cannot pass. Replaced by `DELIVERED_UNSTABLE_ACROSS_K`, which is
  satisfiable, strictly better at the isolation worry it encoded, and which run 1 shows
  passing cleanly.

## Run 2 design — THREE arms, and the third is the control that was missing

| Arm | Marker location | Path exercised |
|---|---|---|
| `SMOKE_NOTOOLS` | sibling file, `--tools ""` | attachment |
| `SMOKE_TOOLS` | sibling file, `--tools Read` | attachment |
| **`SMOKE_INDEX`** | **the scratch `MEMORY.md` itself** | **always-loaded — the positive control** |

Every arm now also gets a `MEMORY.md` index naming its siblings, mirroring the live dir.

`SMOKE_INDEX` still carries no threshold information: its payload is the same 512 B / 10
lines, far under every candidate constant, and it rides the always-loaded path, which
OQ-286 already retracted as a truncation site. It exists solely to make a zero readable.

## What this probe may conclude

| INDEX | no-tools | tools | Reading |
|---|---|---|---|
| 3/3 | 0/3 | 3/3 | `--tools ""` suppresses the attachment. **Arm A as designed returns a null that means nothing** and must be redesigned before the freeze. |
| 3/3 | 0/3 | 0/3 | The always-loaded path works; **the attachment path specifically does not fire under `-p`.** Arm A needs a different transport. |
| 3/3 | 3/3 | 3/3 | The attachment arrives under both. **Arm A is runnable as designed.** |
| 0/3 | 0/3 | 0/3 | The memory subsystem is not engaging under `-p` at all. **The transport is wrong, not the flag** — and no arm of the run is currently runnable. |
| any other split | | | Inconclusive at k=3; report the split, conclude nothing, do not average it away. |

Secondary, and free: whether the response carries a parseable `usage` block at all — the
primary instrument of the real run has no input without one. (Run 1: 6/6 did.)

## What this probe may NOT conclude

- **Nothing about where truncation begins.** The payload is **512 B / 10 lines**, far under
  every candidate constant on both axes (4,096 B / 25,000 B / 200 lines). It is not near a
  boundary and cannot report one.
- **Nothing about how much of a file survives.** The question is *does the attachment
  arrive*, never *how much of it*.
- **Nothing about which constant pair governs.** That is the run's job, and the secondary
  discriminator for it (the truncation-notice text) cannot appear here — an untruncated
  file carries no notice.
- **Nothing that may be cited as a result.** No verdict from the outcome table is rendered
  for smoke, and the driver structurally does not compute one in `--smoke` mode.

## How the payload is kept uninformative

- **512 B / 10 lines** — far under every candidate constant on both axes.
- **ONE marker, prefix `SMOKE-MARKER`** — not a START/MIDDLE/END triple, so there is no
  position signal to read. A distinct prefix so smoke artifacts can never be mistaken for
  run canaries in any later sweep.
- The marker is a fresh 16-hex token, minted per unit, never in the prompt — the same
  2⁻⁶⁴ inference foreclosure the run uses, for the same reason.

## The seeded-draw hazard this is guarding against

`amnesiac_institution_v0_6.md` §7.5: **generation is stochastic, and a second prompt written
after seeing the first result is a seeded draw.** The ordinary defence against a fluke —
"run it again" — is unavailable in its usual form, because the second run inherits the first
result through the prompt. A smoke probe that touched a threshold would seed the real run's
design with a threshold observation, and the freeze would then be covering a prompt that had
already seen data.

## Cost

Run 1: 6 calls (2 arms × k=3), spent. Run 2: 9 calls (3 arms × k=3), `--max-budget-usd 0.50`
each, ceiling **$4.50**. Isolation, HALTs,
persistence, and the live-substrate guards are identical to the run's — smoke is cheaper,
not looser.
