# The stub run: `payloads_stub/` and `responses_stub/` — SYNTHETIC, and the witness that the capture path works

**These are not results. Every response in `responses_stub/` is fabricated by a stub transport.**
One canonical explanation for both directories, kept here rather than duplicated into each (a
second copy would be a P2 fork inside the audit measuring P2).

## What the stub run is

| field | value |
|---|---|
| `run_id` | `stub-4118f64e-b8848e90` |
| `mode` | **`stub`** |
| `expected` | 219 |
| stamped | `2026-08-11T20:56:11Z` |

Both directories carry that `_run.json`. The stamp is **~34 minutes after** the failed live run
(`edc90409`, committed 15:22 local / 20:22Z), which is the whole point: **this is the repaired
driver exercised end-to-end at the full 219-unit scale, after the failure, without spending
anything.** Responses are literal tokens (`{"raw": "P1"}`) emitted by a substituted transport, not
model output.

## Why they are in the record

`SPEC_next_preregistration.md` §2.1 row 6 claims gate 4 — the **output-side** gate — is built and
two-sidedly witnessed, listing five refusals plus end-to-end broken variants. **Those two
directories are that row's evidence.** Without them the row is a claim a cold reader cannot check,
which is the shape this whole audit exists to prevent. They also demonstrate write-then-verify
closing at production scale: `payloads_stub/` is what went in, `responses_stub/` is what landed and
was read back from disk — one side alone shows neither.

Retained deliberately for OQ-277's close, when the real run's matrices will need a
known-good baseline of the pipeline's mechanics separated from its content.

## The confusion risk, closed as a REQUIREMENT rather than a warning

The live directories are `payloads/` and `responses/`; these are `*_stub`. A label saying "do not
confuse them" is a warning comment, and this project's rule is to remove the sharp edge instead:

> **REQUIREMENT on the scorer and every future consumer** (added to
> `SPEC_next_preregistration.md` §3): before scoring any response set, **read the sibling
> `_run.json` and assert `mode == "live"`.** Refuse on `mode == "stub"`, and refuse on a missing
> or unparseable `_run.json` rather than proceeding — absence fails closed. A scorer that silently
> accepts stub data would report a clean 219-unit result computed entirely from fabricated tokens:
> success-shaped output from a run that measured nothing, which is Pattern 6 with this audit's own
> name on it.

`mode` is machine-readable and present in both directories precisely so this check is one line.

## What is NOT here

`responses/` (the live run) is **empty by defect** — see `responses/EMPTY_BY_DEFECT.md`. Do not
substitute the stub responses for it, in analysis or in narrative. The live run persisted nothing;
that absence is a dated fact and this directory does not soften it.
