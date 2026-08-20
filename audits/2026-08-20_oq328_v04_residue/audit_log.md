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

