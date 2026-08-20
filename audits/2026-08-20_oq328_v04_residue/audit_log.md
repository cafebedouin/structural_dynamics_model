# audit_log — OQ-328 V04 residue landing

**OPEN** 2026-08-20
`git rev-parse HEAD` at OPEN: `8ccace6fbd86c9a2e138a4723265ff0b21a21d98`

Baseline observed before any edit (not predicted):

- `./scripts/gate.sh` — **GREEN, 26 rows**.
- `.venv/bin/python python/amnesiac_carriage_check.py` — **15/15 ok**, selftest 6/6.
  Notably `5.4 correction count expected=1 actual=1` and
  `7.4 numbered rows expected=11 actual=11`.

Plan's Package 0 predicted an uncommitted OQ-287 limb-2 unit needing a first commit.
**Observed: the tree was already clean at OPEN** — that unit landed as `8ccace6f` /
`cdd1a92d` before this session started. No pre-commit was needed. Recorded as a
plan-vs-observed mismatch, per the plan's own instruction to report the baseline observed.

## Log

### 2026-08-20 — Package S, phantom quotation: REFUSED before any edit

The plan's fourth finding ("a phantom quotation, carried at two sites") is false. The phrase
*"the losses are independent"* IS in §5.4 at `:1357-1358`, split across a hard wrap inside a
blockquote. Line-based grep returns 0; the plan's own wrap-safe verification probe returns 1.
Both cited citations are correct and were left untouched.

Evidence + two-sided control on the probe: `phantom_quotation_refused.md` (written before the
paper was edited; this log entry commits ahead of the paper diff so the ordering is git's, not a
reconstruction).

Downstream effect: Package C row 11's hazard 1 inverts — the collision it was told might not
exist DOES exist at that exact wording, so row 11 quotes it.

### 2026-08-20 — Package A: enumeration run, zero classified, row marked

A1 enumeration: eight indices against Wu's class-A criterion — `A1_classA_enumeration.md`.
Vocabulary probe with two-sided control — `A1_classA_vocab_probe.md` (reported at its true and
LOW altitude: it searches Wu's words, not Wu's class, and its 0 is a concept-to-surface false
absence; the enumeration, not the probe, discharges the zero).

A2: **tested absence**, scoped to the claim *the taxonomy has no member expressing Wu's A*.
The row's OTHER claim — *this institution has no dev/prod split of the relevant kind* — was
never tested and is **falsified** by the same pass (four splits, one silent for months).
So the plan's second branch was live for the row AS WRITTEN; the row becomes WITNESSED only
in the corrected form.

A3: row A tier-marked `[WITNESSED]`; Appendix B `6.2` marked pointer row added; a dated
discharge block appended after the deferral note, leaving the against-interest paragraph intact
above it per the house form.

A4: ratchet test re-run — `A4_ratchet_retest.md`. §14's clause re-read and holds unchanged;
deliberately not edited, because the false clause is at §6.2 and spreading the correction to a
site that is not wrong is the ruling-lands-at-one-site failure in reverse.

### 2026-08-20 — Package S, and CLOSE

Package S dispositions:

- **Phantom quotation: REFUSED**, not fixed. See the entry above and
  `phantom_quotation_refused.md`. Filed as **wrap-trap instance 7** at
  `build_discipline.md` → *A textual probe's zero is a fact about the probe*.
  **Correction to my own first framing:** I initially numbered it instance 6; the ledger
  already numbers item 31's paraphrase instance 6, so this is **7**, and the *sixth of the
  storage-form species*. Corrected at all four sites in one pass.
- **ISSUES OQ-328** → `resolved`, with the per-row disposition table, row 22-A's split
  result, and the refusal recorded.
- **ISSUES OQ-310** → two inputs added (row 19 carries no 11 → 12 obligation; the 21/22
  contradiction is a *deferred candidate* for its walk, with the note that it was caught by a
  person and so would land in the ten-of-eleven column, not the gate column).
- **KNOWN_STATE.md** entry added, tier `correction-key`.
- **Promotion test, run as its own pass — verdict: NO PROMOTION.** The one candidate is the
  wrap-trap (a line-oriented probe silently returning 0 on this hard-wrapped, blockquoted
  document). It fails the test not on silence but on siting: it is already carried at
  `build_discipline.md:2691` with a seven-instance ledger and a disjoint-fix table, and at
  `python/amnesiac_carriage_check.py:37` in the normaliser's own header — both sites an editor
  of this document reaches, and the second is delivered by `known_state_status.py --file`.
  Promoting it to CLAUDE.md would duplicate a documented rule and defeat the token purpose.
  Bias to history, per CLAUDE.md.
- **Router regenerated** (`omega_resolver.py index`) after the ISSUES edits.

Every ISSUES/KNOWN_STATE edit in this pass was made through Bash, which does NOT fire the
PreToolUse tripwire hook (matcher is `Edit|Write` only). `known_state_status.py --file` was
therefore run **manually, before** the edits, on both `ISSUES.md` and the paper. It surfaced the
2026-08-20 paraphrase-vs-wrap-trap correction-key entry, which is what corrected this pass's own
species classification — the hook's value was realised, and only because the query was dispatched
by hand.

**CLOSE** 2026-08-20
`git rev-parse HEAD` at CLOSE: `1e5f3e262e3f23675dae4c83d3516bf45408d2f3`

**Comparison:** OPEN was `8ccace6fbd86c9a2e138a4723265ff0b21a21d98`; CLOSE is `1e5f3e262e3f23675dae4c83d3516bf45408d2f3`. **The
stamps DIFFER, and every intervening commit is this audit's own** — `28e72378`, `393b4a3d`,
`db06fc79`, `60e6bac0`, `1e5f3e26`, plus this closing commit. No concurrent writer. Per
audits/README.md this is a HEAD-stamp pair whose difference is fully attributed, so OQ-297's
falsifier does **not** fire here: there was no catch, because there was nothing to catch.

### 2026-08-20 — AMENDMENT after close, on operator review: the cell's second clause

The close above was premature on one point and the operator caught it. Row A's second sentence
joins two clauses; the Package A correction retired the first (*"no dev/prod split"*) and left the
second (*"its analogue is regime boundaries and corpus resets, which the trifurcation types as
Type A drift"*) standing — **while the tier-mark I wrote claimed the correction retired the whole
sentence.** That over-claim is the defect: a pointer asserting a discharge its target does not
perform (P8 at one remove, inside my own marking).

Substantively the second clause fails twice: its warrant is gone (it was a substitute for an
absence that no longer exists), and it was **mistyped when written** — §2.8 glosses Type A as
*"the framing expired"*, a temporal discontinuity, against Wu's locational one. The four splits
are **Type B**, confirmed by OQ-57's actual 2026-06-04 repair matching Type B's declared repair.

Both clauses now retired in the paper; tier-mark corrected to name both. Gate GREEN, carriage
15/15 after the amendment. **The wrap-safe anchor check was run on this amendment** — the check
I failed to run on the finding that produced the refusal.
