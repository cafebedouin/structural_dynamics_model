# Phase 4 — the forward window (2026-08-19/20): the primary arm

Every incident postdates §7.4's minting (2026-08-18). This is the only place a
non-retrodictive witness can come from. Screens applied in the frozen order 0 → 1 → 2 → 3.

**Two tallies, never one** (screening rule 3): catches by instruments **predating** the rule
(candidate witnesses) and catches by instruments **built under** it (compliance data). A
pooled number would be this pass committing the class it audits.

---

## W1 — the `apparatus` gate row, RED on a malformed `Fired:` bit (2026-08-19) — **THE WITNESS**

**Source:** `audits/2026-08-19_oq285_mode3_measurement_arm/audit_log.md:40-49`.

**The defect.** OQ-285's own `WRITEUP.md` carried `**Fired:** **latent** — …` (token bolded).
`apparatus_instrument.py`'s `FIRED_RE` reads that as malformed; `FIRED_ANY_RE` still matches.
The bit was **present and unparseable**. The audit's own log: *"a same-session instance of this
audit's own subject matter: a machine checker distinguishing present-but-unreadable from
absent, and being the only thing that noticed."*

**Screen 0 — is it a defect?** ADMIT. Not a budget or threshold breach: nothing was "too much
of something." A record in an apparatus artifact failed to match the grammar the artifact
requires. §7.4's own population already contains record-level defects of exactly this kind
(instance 6, a commit message; instance 9, a summary sentence).

**Screen 1 — construction or catch?** CATCH. `apparatus_instrument.py` was minted 2026-08-10
for the catch-bit convention generally, not in response to this specific instance D. This is a
*later, new* instance of the class it guards.

**Screen 2 — self-instance or substrate?** SELF-INSTANCE. The `Fired:` bit is the apparatus's
own instrument (CLAUDE.md: *"the apparatus's own instrument: the rolling live/latent/no rate is
the falsifier for the verification ritual itself"*), and the malformation was committed inside
an audit pass, by that pass, about that pass. **This is the pass's one genuinely marginal
screening call and it is recorded as such**: the defect is in an apparatus *record*, not in
apparatus *code*. It is admitted because §7.4's table admits record-level defects (6, 9).

**Screen 3 — independence.** This is a **confirmation**, so it IS gated.
- (a) Does the catching instrument predate 2026-08-18? **YES** — 2026-08-10, eight days before.
- (b) Was its author demonstrably working from the rule? **NO — impossible.** The rule did not
  exist. Its docstring cites the 2026-08-10 operator ruling and `build_discipline.md`'s
  *"don't answer 'does the apparatus pay for itself?' by producing more apparatus"*; it does
  not and cannot cite §7.4.
- **Independence: PASSES.** Instrument 8 days before the rule; incident 1 day after it.

**Shape of the catching arm — `check_catch_bits`, `apparatus_instrument.py:118-127`:**

```
tok = fired_token(text)                       # FIRED_RE: ^\*\*Fired:\*\*\s+(live|latent|no)\b
if tok:                     tally[tok] += 1
elif FIRED_ANY_RE.search(text):               # ^\*\*Fired:\*\*   — the strict prefix
    problems.append("malformed **Fired:** line (want live|latent|no)")
elif dir_date > CATCH_ADOPTION:
    problems.append("WRITEUP.md missing its **Fired:** line")
```

**INVARIANT-ASSERTING.** `FIRED_ANY_RE` is the strict prefix of `FIRED_RE`, so the three cases —
present-and-parseable / present-but-unparseable / absent — **partition the space by
construction**. There is no route by which a malformed bit reads as fine: it cannot satisfy
`FIRED_RE`, and it cannot escape as absent. This is the Build-Discipline spine implemented
(*carry the provenance bit with the value so absence and success stop collapsing at the read
site*), and it is what §7.4 means by an invariant the defect cannot satisfy.

**Scoped honestly: it is an invariant over the bit's GRAMMAR, not over the bit's TRUTH.** A
well-formed bit carrying the *wrong* token (`no` where `live` was true) satisfies this arm.
The claim is: within the grammar, a defect cannot satisfy it — and the defect was a grammar
defect. The witness is reported at that altitude and no higher.

**Evidence grade: NATURAL CATCH ON AN UNRELATED DEFECT** — the top of the frozen ladder. The
defect arose in OQ-285's pass, whose subject (absence semantics of a purity component) has
nothing to do with §7.4. Nothing was planted; no before/after pair was constructed.

### Why this is stronger than the candidate §7.4 already rejected

§7.4 rejected its one candidate (`amnesiac_carriage_check.py`'s selftest fixtures silently
failing to perturb) on the ground that the catching assertion *"was authored under §7.3's
standing two-sided-control discipline **before** this rule was formulated, and would have been
written identically had the rule never existed."*

That reasoning answers a **different question** than OQ-310's frozen screen 3 asks.
§7.4 asked *did the rule PREDICT something?*; screen 3 asks *is the catch EVIDENTIALLY
INDEPENDENT of the rule?* Under screen 3, predating the rule is exactly what makes a catch
independent rather than what disqualifies it — and screen 3 is what governs this pass, having
been frozen before any incident was read.

The separation is also cleaner here on both sides:

| | §7.4's rejected candidate | W1 |
|---|---|---|
| instrument authored | 2026-08-18 — **same day** as the rule | 2026-08-10 — **8 days before** |
| incident occurred | same pass that produced the rule | 2026-08-19 — **1 day after** |
| author could have had the rule in view | yes | **no — it did not exist** |

---

## W2 — `dangle_count.sh`'s control arm caught two defects in its own probe (2026-08-20)

**Source:** `audits/2026-08-20_oq287_limb1_extraction/WRITEUP.md:16-22`, `dangle_count.sh:6-15`.

Two apparatus self-instances: (1) an anchor pattern written against v0.6's capitalisation
returned ABSENT on the *intact* paper — a false negative that "read exactly like the deletion
working"; (2) an anchor straddling a hard line break made a line-based grep return 0 on a
phrase plainly present — the wrap trap.

Screens 0/1/2: ADMIT / CATCH / SELF-INSTANCE. Both are defects in the test's own probe.

**Catching shape: INVARIANT-ASSERTING** — the two-sided control arm asserts *every anchor must
be PRESENT in the intact paper*, which a broken probe cannot satisfy.

**Screen 3 — independence: FAILS.** Authored 2026-08-20, two days after the rule, by an author
who on the same day wrote `deletion_test_arm.md:300-308` and `APTNESS.md:33` discussing §7.4's
invariant rule and instance 11 directly. **Compliance datum, not a witness.** The script's own
header cites §7.3's two-sided discipline as its basis, which is the same ground §7.4 used to
reject its earlier candidate — but the co-authored §7.4 discussion makes (b) unarguable here.

## W3 — `standing_check.sh`: a check that could not fail, caught by its own two-sided control (2026-08-20)

**Source:** `audits/2026-08-20_oq287_limb2_discharge/WRITEUP.md:13-25`.

The replacement arms were written against `$norm` — `normalized()`'s collapse of the **whole
file** to one line — so both asserted phrases matched elsewhere in the paper and the arms were
**green regardless of what §2.9's marker said**. Stripping the phrase left them PASS; stripping
the reversion trigger left them PASS.

Screens 0/1/2: ADMIT / CATCH / SELF-INSTANCE.
**Catching shape: INVARIANT-ASSERTING** — `selftest()` asserts *the arms must DISCRIMINATE, not
merely fire*, and a non-discriminating arm cannot satisfy it. Its arm-3 record is built on the
operator's before-commit rule (fires at `c3667f75`, declines at HEAD) — the strongest control
grade the repo defines.
**Screen 3 — independence: FAILS.** Authored 2026-08-20 in the same limb pair as W2.
**Compliance datum.**

## W4 — `checks.sh` row 3 asserted one end of a pointer and nothing checked the far end (2026-08-20)

**Source:** `audits/2026-08-20_oq287_limb1_extraction/WRITEUP.md:144-158`.

*"Delete or renumber §III and both markers dangle **with the gate green**."* Pattern 1 on the
pointer substrate, committed by the pass that built the pointers.

Screens 0/1/2: ADMIT / n-a (a MISS, not a catch) / SELF-INSTANCE.
**Shape of the thing that missed it: VALUE-CHECKING** — row 3 asserted the presence of marker
phrases on v0.6's side. **Caught by a person**, noticing the pointers were written after the
check was.
**Bearing: CONSISTENT WITH THE RULE.** A value check missed a self-instance. Does not fire
falsifier B, which needs an *invariant* row to miss one.

## W5 — the OQ-242 corpus fingerprint FALSE-FIRED (2026-08-19)

**Source:** `audits/2026-08-19_oq242_absence_semantics/WRITEUP.md`, "Instrument note".

The md5-of-md5s frozen-corpus fingerprint hashes `md5sum`'s output **lines**, which contain path
strings; a relative-vs-absolute mismatch *"produced a spurious 'corpus moved' reading during
this audit before being identified."*

Screens 0/1/2: ADMIT / n-a / SELF-INSTANCE. **Caught by a person.**

**This is the falsifier-B search's discriminating control, and it is why the B null is not a
didn't-look.** The instrument here is **invariant-shaped** (a stratum-2 integrity line:
*the corpus must be byte-identical across the diff pair*), and the search found a defect in it.
So the probe is demonstrably not blind to invariant-shaped instruments. What it found was a
**FALSE FIRE**, not a miss — a failure mode §7.4's binary does not name at all.

## W6 — §7.4's own self-contradicting gate-row count (2026-08-20)

`audits/2026-08-20_oq328_v04_residue`. Caught by **a person reading**. Pre-scored by OQ-310 as
a confirming miss, not a test. Adjudicated at Phase 6 (instance-12 call).

## W7 — row 19's landed tally, folded in, not re-derived

§7.4's 2026-08-20 blockquote: the control architecture has caught **two untracked paths and one
real leak, and zero of five instrument defects.** Measured on controls rather than gate rows,
and the same split the rule predicts: controls catch what flows *through* instruments and miss
what is *wrong with* them.

---

## Forward-window tallies — reported separately, never pooled

| tally | n | items |
|---|---|---|
| **Catches by instruments PREDATING the rule (candidate witnesses)** | **1** | W1 |
| Catches by instruments BUILT UNDER the rule (compliance data) | 2 | W2, W3 |
| Self-instances caught by a person, no gate involved | 3 | W4, W5, W6 |
| Value-checking things that MISSED a self-instance | 1 | W4 |
| Invariant-asserting things that MISSED a self-instance | **0** | — |
| Invariant-asserting things that FALSE-FIRED | 1 | W5 |

**Falsifier A (a value-checking row that caught a self-instance): does NOT fire in this window.**
The only gate-row catch, W1, is by an invariant-asserting arm.

**Falsifier B (an invariant-asserting row that missed one): does NOT fire in this window**, with
its power declared below.
