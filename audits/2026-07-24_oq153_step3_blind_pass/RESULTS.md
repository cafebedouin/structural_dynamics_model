# OQ-153 step 3 — blind pass RESULTS (verdicts UNRULED, for operator)

Blind subagent (general-purpose, no OQ/husk context; exact prompt: `subagent_prompt.md`, the
blindness witness) classified 20 anonymized items (`subagent_input.md`) against the rubric.
Join to the sealed key (`key.json`) → `results.txt`. Distribution: 16 licensed_revisable,
3 absent_diffuse, 2 frozen, 0 unauthored.

## Verdicts (computed, not eyeballed)

- **Kill A — FIRES on `frozen` (1 institution: Church magisterium).** licensed_revisable on 10
  institutions (PASS); absent_diffuse on 3 (Bitcoin, Bitcoin-whitepaper, Math/CS — PASS, though the
  two Bitcoin count as arguably one institution); unauthored absent. **The selected-for-frozen
  institutional-superseded items did NOT read frozen:** Versailles ("revised by the Reparations
  Commission"), Bretton Woods ("IMF institutional governance"), Balfour ("mandatory authority could
  revise") all → licensed_revisable; bitcoin_whitepaper (predicted frozen) → absent_diffuse. `frozen`
  landed ONLY on closed-canon (magisterium) items. **My superseded⇒frozen selection premise was
  wrong** — the subagent read each superseded instrument's ORIGINAL governing body's revision power.
- **Kill B — appears PASS (operator to confirm quote quality).** Every call is justified by an
  institutional quote + a named Q0/Q1/Q2; **no Q0/Q1 confusion** (church_turing → absent_diffuse via
  Q1, as the corrected prediction); no call required husk-reasoning. The ~9 uncertain items resolved
  to institutionally-defensible values.
- **Kill C — PASS.** Both free same-institution cross-stratum pairs returned the SAME value across the
  stratum split (US Const: licensed_revisable/licensed_revisable; Bitcoin: absent_diffuse/
  absent_diffuse). `frozen ⟺ naturalized∧forecloses` is falsified by construction — the field does
  NOT track the naturalization signature.
- **Test–retest — PASS (clean, weak per pre-registration).** All 5 cross-leg replicates AGREE with
  their sonnet primaries (incl. both biblical → frozen). No dispositive disagreement. (Agreement is
  weak evidence — recognition may drive it — but zero disagreement is the clean outcome.)

## The load-bearing finding: `frozen` is the fragile value

`frozen` was assigned only to closed canon (magisterium). Every superseded/final *treaty* read as
`licensed_revisable` because its original governing body had a revision procedure. So condition-5
(frozen update-authority), as operationalized, separates **closed-canon from everything else** — but
the sample held only ONE canon institution, so Kill A fires.

## The adjudication (operator's, per the sealed Kill-A clause)

Kill A firing is ambiguous by pre-registration: "field does no separating work" (→ abort to option 2)
vs "sample homogeneous in this value" (→ wider sample). The evidence points to the **second**: `frozen`
DID separate (canon vs rest, cleanly, and test-retest-stably), but only one true-`frozen` institution
(canon) was present. Superseded treaties are institutionally *revisable*, not frozen — a correct read,
not a field failure. **Indicated fix: a wider sample with multiple closed-canon institutions**
(biblical, Quranic, a constitutionally-entrenched unamendable clause, a final-published spec), NOT an
abort. Operator to rule.

---

## CORRECTIONS (operator ruling, 2026-07-24) — read these over the section above

1. **Count:** 15 licensed_revisable / 3 absent_diffuse / 2 frozen = 20 (the "16" above was an
   arithmetic error; 16+3+2=21 does not reconcile).
2. **Kill A ruling — NEITHER branch: a RUBRIC DEFECT (tense), amend + re-run.** The drift to
   `licensed_revisable` is not a supply problem: the subagent read *superseded* instruments
   (Bretton Woods post-1971, repudiated Versailles, superseded Balfour mandate) as revisable because
   the FOUNDING TEXT recites a procedure — i.e. the field read **historical** amendment authority, not
   present-tense. Covariate witness: the one `founding_problem_status=dead` item (R3
   `bretton_woods@kimi`) read `licensed_revisable` — a direct tense violation. Husk teeth: a husk is a
   dead-founding-problem kernel that persists; a rubric reading amendment off the founding text can
   NEVER see the present-tense frozen state husking requires, and would return `licensed_revisable`
   on exactly the constraints most likely to be husks. Widening the canon stratum would have hidden
   this behind a satisfied Kill A. **Fix:** Q1 present-tense + superseded/defunct → `unauthored`
   (rubric case 7, `frozen` reserved for a live kernel with foreclosed amendment).
3. **Kill B is weaker than "appears PASS."** Kill B tests whether a call is *justifiable from its
   quote*, not whether it is right. The Versailles call carried a clean institutional quote and was
   WRONG under the corrected rubric — the pass's main defect walked through Kill B. Record the limit.
4. **Test–retest agreement is base-rate-inflated.** With 15/20 on one value, replicate agreement is
   near-free; "zero dispositive disagreement" is thin, not a clean pass.
5. **Kill C's real witness is the REPLICATE, not the free pairs.** `biblical_authority@flash` is
   out-of-stratum and read `frozen` — a direct falsifier of "`frozen` occurs only in-stratum" (the
   laundering direction that matters). The free pairs falsify the biconditional formally but contain
   no `frozen` item, so they cannot test that direction. Credit the replicate set on the axis it was
   added for.

---

## PATTERN FOR THE NEXT INSTRUMENT BUILD (ledger note, 2026-07-24)

The recurring failure mode across every leg of this arc was the **same one: selection on the outcome
variable** — engineering the sample so a needed value would be present, which severs the sample from
the question. Four instances, all caught before they contaminated a result:
1. the all-in-stratum sample (Kill C would have been a column, not a 2×2);
2. the immutability-language `frozen` swaps (collinear with the naturalization signal in substance);
3. the canon-widening proposal for Kill A (would have padded `frozen` with a known answer);
4. the thin shape-test pool (3/4 candidates failed to instantiate the shape — the null was about the
   pool, not the field).

It recurs the moment someone needs a value to be **present** (to satisfy a variance gate, to populate a
cell, to complete a 2×2). Guard for the next instrument build: whenever a sample is enriched for a
value, **pre-register that the value is selected-for and exclude it from any test that reads presence
as evidence** (the supply-only / Kill-B-denominator move used here). Selecting for a value forfeits the
right to read its presence as a finding — only its *absence under enrichment* (e.g. `dead∧frozen=0/8`)
carries information. Three of the four were caught by the operator after the first was flagged.
