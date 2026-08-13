# Audit log — RQ-d recognition vs enumeration

Ordering record. Per `audits/README.md`, the freeze must be **witnessable, not narrated**.

## The ordering weakness in this audit, stated first because it is a defect

The pre-registration was **authored before any call** — but that fact is witnessed only by the
session transcript, which is exactly the chat-not-substrate witness this project rules out. The
substrate witnesses disagree with the narration for 4 of 60 units:

| event | substrate timestamp |
|---|---|
| earliest response written (pilot unit 1 of 4) | `2026-08-13 01:25:07 -0500` |
| PREREGISTRATION.md committed (`e685df2d`) | `2026-08-13 01:29:29 -0500` |
| latest response written (unit 60) | `2026-08-13 01:39:42 -0500` |

**So: the commit precedes 56 of 60 units and postdates the 4 pilot units by ~4 minutes.** An
earlier turn in this session stated the freeze was "committed before the data"; that is true of
the full run and false of the pilot, and is corrected here rather than left standing.

**What this does and does not undermine.** The pilot units are in the analysis (they are 4 of the
60), so this is not cosmetic. It does not create a degrees-of-freedom problem in fact — the
outcome rules in §6 were written before the pilot and were not touched afterwards, and the pilot
was run on 2 specs at reps=1 purely to prove the pipeline persisted and scored. But *"in fact"* is
narration again. The checkable statement is the weaker one: **§6's outcome rules are witnessed
frozen only from `e685df2d` onward, and 4 units predate that.**

**Fix for the next spend, in one line:** write the md5 into this file and commit it *before the
first call*, pilot included — a pilot is a call.

## Recorded md5s, at their positions

| when | artifact | md5 |
|---|---|---|
| at freeze (in PREREGISTRATION §7) | `rqd_materials.py` | `87881ddc78f299f29c4976b76dbb5df3` |
| at freeze (in PREREGISTRATION §7) | `rqd_scorer.py` | `978b4c7c9d44f9ed91ea12ad6e89f702` |
| at freeze (in PREREGISTRATION §7) | `rqd_driver.py` | `1bcd70fd20779d9803c569b3990c5a45` |
| after Amendment 1 (mid-run, declared §9) | `rqd_scorer.py` | `12772cca393a56cd54549fdc798fbb5e` |
| written after freeze, declared in §7 | `rqd_analyze.py` | `4ea01f427c5b26f124adc6c4e3acfa2e` |
| post-amendment | `PREREGISTRATION.md` | `ae40f3f3878159f67f160f8358e8d1a2` |

`rqd_materials.py` and `rqd_driver.py` are **unchanged from freeze** — the two artifacts that
determine what was asked and of what. Only the scoring path moved, and only after the raw data
was on disk, which is the sole reason the amendment was possible at all.

## Sequence

1. Instruments written; `rqd_materials` + `rqd_scorer` selftests GREEN (6 controls).
2. `--dry-run` over all 60 units: 0 files written, verified by the output gate rather than by the
   help text.
3. PREREGISTRATION.md authored with the three freeze md5s.
4. **Pilot: 4 calls** (2 specs × 2 protocols × 1 rep). Output gate GREEN.
5. `rqd_analyze.py` written; pilot proved scoreable end-to-end — the check the previous arc
   (219 calls, nothing scoreable) did not have.
6. Full run launched (56 remaining units).
7. **Amendment 1** — scorer defect found by reading real pilot output against the scorer's
   verdict; sentence splitter broke on `vs.`. Repaired mid-run, declared in PREREGISTRATION §9,
   two controls added (one a verbatim real-output regression fixture).
8. `e685df2d` — prereg + instruments committed.
9. Run completed: `issued=56 skipped=4 failed=0`; output gate GREEN, 60/60 non-empty and parsing.
10. Analysis run under the frozen §6 rules. Verdict recorded, then qualified by the instrument
    audit in WRITEUP.md §4.
