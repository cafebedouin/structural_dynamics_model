# UKE_S v0.1 [Universal Knowledge Evaluator - Scoring Protocol]
## Created: 2026-07-22
## Companion to: `uke_write_v2.2.md` (§1.6 discipline, §6.1 block format — the Forecast Register producer)

---

## §0. PURPOSE AND SCOPE

You are scoring predictions, not prose. Input: one or more essays (or archived copies) whose
Forecast Registers are due or past due. Output: one scorecard per essay plus an aggregate
(§5). You read ONLY the register blocks plus whatever public evidence resolves them — not the
essays. If you need the essay to understand a question, the row is defective (UNRESOLVABLE);
repairing it is not your job.

**Core invariants:**
* **Two columns, never pooled.** Mechanism-identification and magnitude-estimation are
  separable skills that fail independently. A pooled score punishes a correct joint-finding
  call for a timeline miss and teaches the author to hedge timelines — degrading exactly what
  structural analysis is good for.
* **Skill over baseline, not raw accuracy.** Headline `brier_baseline − brier_essay`. Raw
  Brier rewards hedging; skill-over-baseline zeroes it out.
* **Fail-closed on vagueness.** A row needing charitable interpretation is UNRESOLVABLE — a
  protocol defect to report, never to repair. Do not resolve by inferring what the author
  "must have meant."
* **Triage precedes grading, and the triage is reported.** An essay with no register
  (pre-v2.2, or defective) is UNSCOREABLE — recorded, never silently skipped. The scoreable
  ratio is itself a finding: it dates when the writing practice acquired the discipline.

---

## §1. EXTRACTION AND TRIAGE

1. Locate the fenced block carrying the `FORECAST REGISTER v1` marker. Absent → essay is
   **UNSCOREABLE** (record slug, date, reason: `no_register`). Do NOT reconstruct forecasts
   from prose — prose reconstruction is a separate, human-adjudicated exercise; doing it
   silently here launders unscoreable essays into the scored set.
2. Parse as YAML. Malformed → **UNSCOREABLE** (reason: `parse_failure`).
3. Per row, check field completeness: `question`, `resolution_date`, `resolver`, `p_essay`,
   `p_baseline`, `column`, `direction`. Any missing → row is **UNRESOLVABLE**
   (reason: `missing_field:<name>`); count it, do not score it.
4. Rows with `resolution_date` in the future → **PENDING**; report, do not score — even when
   the outcome "seems settled" early.

## §2. RESOLUTION PASS (per due row)

1. Consult the row's named `resolver` first. If it did not produce the data, widen to
   Tier-1-quality public sources (uke_write §3.2 hierarchy) and say so in the citation.
2. Resolve: **YES** (outcome = 1) / **NO** (outcome = 0) / **UNRESOLVABLE** (criterion
   defective, resolver silent, or outcome genuinely ambiguous under the row's own criterion —
   reason: `ambiguous_criterion`).
3. Every resolution cites its evidence (source + date). An uncited resolution is invalid.
4. UNRESOLVABLE is a finding about the ROW (a §5.7 gate escape in the producing essay). It is
   never averaged into scores.

## §3. SCORING (per resolved row)

```
brier_essay    = (p_essay    − outcome)²
brier_baseline = (p_baseline − outcome)²
skill          = brier_baseline − brier_essay    # positive = the structural read added information
```

**Signed direction of miss** (only for rows where `p_essay` fell on the wrong side of 0.5
relative to outcome): record `over_fragility` (direction tag `fragility`, world held — or tag
`stability` inverted accordingly) or `over_stability` (predicted holding, world broke).

## §4. AGGREGATION

Per essay and across essays, ALWAYS separated by column:

- **Mechanism column:** n resolved, mean `brier_essay`, mean `skill`
- **Magnitude column:** n resolved, mean `brier_essay`, mean `skill`
- **Direction distribution of misses:** `over_fragility` vs `over_stability` counts, by column
- **Triage stats:** scoreable/unscoreable essays; resolved/unresolvable/pending rows

**The direction distribution is the standing hypothesis readout (ISSUES.md OQ-229):**
structural analysis is pre-registered as biased toward over-predicting fragility
(joint-finding is what the method does; redundancy leaves no structural signature). Misses
clustering in `over_fragility`, concentrated in the magnitude column with the mechanism column
clean → supports the hypothesis. Misses scattering both directions → the kill condition fired:
calibration problem, not theory defect. **Report the counts; do not adjudicate the hypothesis
in the scorecard** — adjudication is OQ-229's call, made over accumulated scorecards.

**Low-n discipline:** at small n report counts, not percentages-with-authority. The direction
sign distribution pays out before the Brier means do.

## §5. OUTPUT FORMAT

```markdown
# Forecast Scorecard: [essay slug]
Scored: [YYYY-MM-DD] | Register written: [date_written] | Rows: [n] ([resolved]/[pending]/[unresolvable])

| id | col | question (abbrev) | outcome | p_essay | p_base | brier | skill | miss dir |
|----|-----|-------------------|---------|---------|--------|-------|-------|----------|
| F1 | mech | ... | YES | 0.80 | 0.50 | 0.04 | +0.21 | — |
| F2 | mag  | ... | NO  | 0.75 | 0.40 | 0.56 | −0.40 | over_fragility |

## Resolutions
- F1: [YES/NO] — [evidence citation: source, date]
- F2: ...

## Summary (this essay)
- Mechanism: n=…, mean brier=…, mean skill=…
- Magnitude: n=…, mean brier=…, mean skill=…
- Misses: over_fragility=…, over_stability=… (by column)

## Defects
- [row id]: UNRESOLVABLE — [reason code + one line]

## Triage note (aggregate runs only)
- Essays scoreable: X of Y; unscoreable: [slug — reason, …]
```

## §6. WHAT THE SCORER MUST NOT DO

- Never repair, reinterpret, or narrow a question to make it resolvable
- Never pool columns, and never headline raw Brier without skill beside it
- Never score PENDING rows early
- Never omit UNSCOREABLE/UNRESOLVABLE counts — silence launders the triage
- Never adjudicate the OQ-229 hypothesis inside a scorecard; emit the counts and stop

---

## END OF UKE_S v0.1

**Status:** Untested — no register-bearing essay exists yet; first live application is the
witness (see OQ-229 forward arm).
**Purpose:** Standard rubric letting any subsequent model grade a Forecast Register without
human triage and without reading the essay.
