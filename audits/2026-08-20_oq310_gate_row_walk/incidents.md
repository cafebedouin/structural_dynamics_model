# Phase 5 — the retrospective walk, and the two standing candidates

## The two `apparatus`-row candidates, adjudicated against the frozen screens

Both were named at plan time. Neither verdict was pre-rendered. Screen 0's calibration
(`screen0_calibration.md`, eliminates 0 of 11) was run and pasted before either was touched.

### Candidate 1 — the memory feedback channel at 34/33 (`ISSUES.md` OQ-289/OQ-290 region)

The channel hit 34 entries against a cap of 33 and "turned the `apparatus` gate row RED."
The firing arm is `apparatus_instrument.py:149`, `if n > FEEDBACK_CAP` — a count against a
constant, a pure value check. If this counted, falsifier A would fire.

**SCREEN 0 ELIMINATES IT.** A cap breach is a **budget breach, not a defect**. Nothing was
wrong with any entry; there were too many entries, and a threshold said so. That is the gate
doing its declared job — the channel is a *gated exchange* (retire one to admit one), so
exceeding it is the mechanism operating, not a malfunction.

**The elimination is principled, not fitted, and that is now shown rather than asserted.**
Screen 0 was drawn with this candidate in view — declared at Phase 1 and still declared. But
run against the eleven §7.4 derivation instances, material it was not fitted to, it eliminates
**none of them**. A screen that admits 11 of 11 cases it was not built on, and excludes this
one, is discriminating on the stated criterion rather than on the answer wanted.

**Verdict: NOT AN INSTANCE. Falsifier A does not fire on it.**

### Candidate 2 — the `apparatus` row RED on a malformed `Fired:` line (2026-08-19)

**Screen 0 ADMITS it** (a record failing to match its required grammar is a defect, not a
budget breach). Screens 1 and 2 pass. Screen 3 does not apply in the falsifying direction
because **the catching arm is not a value check**: `check_catch_bits` partitions
parseable / present-but-unparseable / absent by construction, so a malformed bit cannot satisfy
it by any route. See `forward_window.md` W1 for the full adjudication.

**Verdict: an INSTANCE, and a CATCH — but by an invariant-asserting arm. Falsifier A does not
fire on it. It is instead the rule's first independence-screened, non-retrodictive
confirmation.**

Both candidates were on the `apparatus` row; neither fires falsifier A; they fail for
*different* reasons, which is the frozen screens doing real work rather than one screen
absorbing both.

---

## The pre-window record

**Method.** The `Fired:` ledger (18 pre-window `WRITEUP.md` bits at HEAD), plus a targeted
sweep for writeups naming a gate row as the catcher:

```
/usr/bin/grep -inE 'gate row|scripts/gate\.sh|turned .* (RED|red)|row went red' audits/*/WRITEUP.md
```

Eleven pre-window dirs matched. Nine of the eleven are *wiring* records (a row was added, the
gate was green) rather than catches. Two are catches:

**`2026-08-18_oq296_consumer_honesty:148-167` — two rows RED, "both mine."**
- `bound selector` (Phase-2 shape: **mixed**) fired on the author's own provenance prose, which
  contained `constraint_signature(C, natural_law)` inside a Python string literal. The writeup
  judges it *"RED, and CORRECTLY so"* and rewords the prose rather than widening the checker.
- `module bounds` (Phase-2 shape: **invariant**) fired on a new test calling a non-exported
  helper; resolved with a declared allowlist row.

**Screen 2 is where both weaken.** Neither defect is in a control, gate, checker, manifest or
selftest: one is in audit prose, one is in test code resolved by declaring a bypass. They are
hygiene violations of declared rules, not apparatus self-instances. **Recorded, not counted.**
Also both are dated 2026-08-18 — the rule's own minting day — so their independence would need
adjudicating even if screen 2 had passed them.

**No pre-window value-checking gate row is recorded as having caught an apparatus
self-instance.** Falsifier A does not fire retrospectively either.

---

## Falsifier B, and the power of the search for it

**Result: zero invariant-asserting rows found to have missed an apparatus self-instance.**

**This is a declared null with stated power, not a clean negative**, for three reasons the
population itself supplies:

1. **Exposure.** Median row-exposure is **7 days**; 8 of 26 rows are ≤ 3 days old; the whole
   stratum-1 population totals **695 row-days**. A row with one day of exposure and no incident
   is evidence about neither side, and 18 of the 26 rows are invariant-asserting — so most of
   the falsifier-B population is also most of the young population.
2. **Jurisdiction.** A miss requires the row to have had the defect *in scope*. Checked
   explicitly and found clean: `audit writeup` does not assert `Fired:` grammar (its own code
   says *"malformed names are audit_writeup_gate's finding, not ours"*), and `omega selftest`
   asserts nothing about its own printed banner. Jurisdiction is cleanly partitioned, so no
   in-scope miss was available to find.
3. **Instrument-only rows.** 6 of 26 rows (`python env st`, `omega selftest`, `claim cites st`,
   `axis boundary`, `cli selftest`, `tripwire hook`) assert nothing about the substrate at all.
   They cannot miss a substrate self-instance because they never look at one.

   **And this compounds with (1) rather than sitting beside it.** Those six rows' exposure-days
   count toward the 695-row-day total, which is the denominator the falsifier-B power claim is
   stated over — so part of the exposure the null is credited with is exposure to a population the
   rows structurally cannot watch. Their contribution, computed rather than estimated:
   `python env st` 2 + `omega selftest` 67 + `claim cites st` 7 + `axis boundary` 58 +
   `cli selftest` 54 + `tripwire hook` 7 = **195 of 695 row-days, 28%.** The effective
   substrate-watching exposure is **500 row-days over 20 rows**, and the null is weaker than the
   headline figure by that much. Declared in the direction already declared, not discovered later.

   *(The first draft of this paragraph asserted "207 of 695, 30%" and "488 over 20" — written
   before the sum was run, and wrong. The sum was run in the same turn and corrected before the
   file was committed. Noted rather than quietly fixed: it is the defect class this whole pass is
   about, committed inside the pass's own power analysis, and the only reason it did not ship is
   that the number was computed instead of estimated. The per-row terms are shown so the total is
   checkable without trusting the writer.)*

**The discriminating control for this null is W5**, and it is a real one: the search *did*
surface a defect in an invariant-shaped instrument (the OQ-242 md5-of-md5s fingerprint). The
probe is therefore demonstrably not blind to invariant instruments — it found a **false fire**
rather than a miss. Per the frozen ladder that is a naturally-arising case drawn from the
population, not an authored decoy.

**A failure mode the binary does not name.** W5's invariant-shaped instrument produced a
spurious positive. §7.4's rule is about detection (catch / miss) and says nothing about
**false fires** by invariant assertions. That is a third cell the binary has no room for, and
it arrived from the record rather than from argument.
