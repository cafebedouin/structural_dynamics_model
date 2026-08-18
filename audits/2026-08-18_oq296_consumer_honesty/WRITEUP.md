# OQ-296 — making the consumers of a constant-zero detector honest

**Executed:** 2026-08-18
**OQ:** OQ-296 (spawns OQ-313 … OQ-319)
**Verdict (scoped altitude):** Every consumer of the dead `natural_law` /
`coordination_scaffold` signatures is now honest, retired, or declared dark — **the detector is
unchanged and still 0-firing**, and the disposition that keeps it is sunset-bound (OQ-317,
2026-11-17, retire-by-default). **Three of ~20 roster entries were wrong**, and none of the errors
was visible from the roster.
**Fired:** live
**Manifest cite:** `pipeline_output.json` manifest `pipeline_run_at` 2026-08-18T20:31:06Z,
`n_constraints` 279, `schema_version` 2. (CLAUDE.md's disk-verified 259 for `testsets/` dates to
2026-08-12; the corpus grew. OQ-296's own body cites n=276 for the same reason — cite the
manifest, never a memorized count.)

**HEAD stamp pair:** OPEN `79661ee6` (recorded in `PHASE0_REWITNESS.md` before the first probe) →
CLOSE `fe64033d`. The four intervening commits are this session's own Phase 1–4 commits, all
authored `cafebedouin`; **no concurrent writer**. This is detection, not prevention.

**Prior-art grep** (`docs/technical/build_discipline.md`, same pass as the findings):
- `has_viable_alternatives` / `natural_law_signature` → **HIT** (`:780-782`) — the fail-close making
  the `== false` leg permanently unsatisfiable is documented. The *detector deadness is prior art*
  and was already carried by OQ-296; nothing here re-discovers it.
- `coordination_scaffold` → **HIT** (`:156`) — listed among the 8 zero-firing signatures under
  *Unwired ≠ worthless*. That entry also lists `piton`, which is why **OQ-315 inherits no cause**
  but does inherit a lead: the same passage argues zero-firing ≠ worthless.
- `container_typology`, `is_constructed`, `category_of` → **no hit.** These findings are new.
- `total_on_domain` → 2 hits, neither about vacuity-on-a-constant. OQ-313's gate defect is new.

---

## Evidence map

| file | what it holds |
|---|---|
| `PHASE0_REWITNESS.md` | Kill-condition HALT gate: OQ-113 suite 3/3, unbound `once/1` census. Carries the OPEN head stamp. |
| `PHASE1_LINTER.md` | The naturally-arising discrimination pair (1 fires / 313 decline) and the before/after advisory text. |
| `PHASE2_SITE1_BLOCKER.md` | Why `container_typology_analysis.py` was reclassified out — the evidence that voided the plan's premise for that site. |
| `PHASE2_WITNESS.md` | Consumer checks that gated both schema shapes; both output diffs. |
| `PHASE3_WITNESS.md` | Roster verification result, comment-only diff, 8/8 suite, clean-vs-edited pipeline pair. |
| `PHASE4_RETIREMENT.md` | Both deadnesses measured; old-vs-new diff; clean load. |
| `CATEGORY_OF_CENSUS.md` | Two-leg census + dispatch control for OQ-316, run **before** minting it. |

---

## What was actually found

### 1. The kill condition holds (HALT gate passed)

OQ-113 suite 3/3; unbound `once/1` census on the live leg returned `natural_law` 0 and
`coordination_scaffold` 0 **against a non-degenerate 7-signature histogram**. The histogram is the
discrimination: it shows the census dispatches, so the zero is measured-empty rather than
didn't-look.

### 2. Three roster entries were wrong — and the pattern in *how* they were wrong

| site | recorded as | actually |
|---|---|---|
| `classification_audit.py:61` | consumer of the constant | validates authored `constraint_claim` values — legitimately non-empty |
| `container_typology_analysis.py` | "spot-verified by direct read", reading a constant 0 | serves `natural_law_pct` **up to 0.9808** from a 2026-05-16 artifact over a retired corpus |
| `diagnostic_summary.pl:437` | partial note needing extension | already fully annotated; nothing partial |

The middle one is the instructive one. It *was* verified — by reading the code, where
`sd.get("natural_law", 0) / n` plainly evaluates to 0 when the detector is dark. What was never
checked is **what the file on disk actually contains**. The input is a frozen recon artifact
(`total_constraints: 3369`, the chimera-era `original_v6` corpus) that has never refreshed across
the 2026-06-05 reset, because neither it nor its analysis script is wired into `run_pipeline.py`.

**A code-read is not an output-read.** Both are "direct reads"; only one of them looks at what the
consumer serves.

### 3. The artifact read as this OQ's own halt condition

`outputs/container_candidates.json` reports `natural_law_pct` at 0.9808 with no indication of
provenance. It does **not** trip OQ-296's falsifier — those are pre-reset values over a different
corpus, not a firing at HEAD, and Phase 0 discharged the real gate — but nothing at the artifact
said so. Closing OQ-296 while leaving an unstamped file that looks like evidence *against* the
close was the worst available outcome: the next reader would have to re-derive the whole
distinction before knowing whether the ruling stood. Hence the stamp, whose main job is that one
line.

### 4. `is_constructed` carries a second, independent defect

Constant `True` 279/279 — expected, given the dark detector. But `sig not in ('natural_law',)` is
also True for the **`unknown` honest-abstain** signature (26/279 rows), so those rows assert
*"constructed"* on the strength of having no data. That is a distinct defect and would survive
powering the detector.

**The obvious fix is refused, and the reason generalizes.** Emitting `None` for the abstaining rows
makes the JSON more truthful and the **read less truthful**: `boolean_independence.py:169` does
`bool(c.get("is_constructed"))`, and `bool(None)` is `False`, so an abstain arrives as an asserted
negative — the same defect, sign flipped. Half a tri-state, delivered into a consumer that can only
see two values, is worse than not starting. → OQ-318, where the consumer set is the unit of work.

### 5. `category_of/2` is constant, but not for the assumed reason

279/279 and 1106/1106 `unknown_novel`. The plan called it a collapsed classifier returning a
degraded near-constant. The dispatch control says otherwise: a planted
`constraint_claim(_, natural_law | physical_law)` yields `physical_natural`; absence declines. The
predicate **works**; its input vocabulary is authored **0 times in ~5,311 story files**. Pattern 5
at the consumer end, not misclassification — and "restore the classifier" versus "author the
vocabulary or retire the axis" are different repairs.

Measured on **two** legs before minting OQ-316, because a one-leg constancy claim would have
reproduced, inside this OQ's own spawn, the error its SCOPING GUARD warns about
(`natural_law_without_beneficiary/1` looks dead on the live leg and fires 30 times on kernel_v1).

### 6. Two dead-code shapes that look alive

- `compute_signature_confidence(_, coordination_scaffold, _)` — unreachable twice, and the inner
  reason is the interesting one: even if reached, `high` is **arithmetically** unreachable, because
  one of its three indicators can never hold. It would silently cap at `medium` — a degraded scale
  that still looks like a working one.
- `reading_registry.pl:115` — `has_viable_alternatives/2` registered `total_on_domain`, **green for
  its whole life while certifying nothing**: a constant function is trivially total. The gate proves
  exactly-one-answer-per-key and cannot tell a reading that measures from one that returns the same
  token forever. → OQ-313. Deliberately **not** deregistered; that would hide the defect.

---

## Method notes worth keeping

**Roster inheritance was abandoned mid-execution.** After the second reclassification the operator
ruled that every Phase 3 annotation must state what was confirmed at that site at edit time, and
that a site which does not read the predicate is reclassified out rather than annotated. The
reasoning: a comment-only phase verifies nothing by the act of editing, so a dark declaration on a
non-reading site is a false annotation in exactly the `MISSING_NL_PROFILE` class this audit spent
Phase 1 removing. Two for two on close inspection was enough to stop trusting the other eighteen.

**One edit was written, run, diffed, and reverted unshipped.** The planned
`formalization_provenance` flag asserted `natural_law_pct` is "structurally zero on every corpus."
For the values actually served that is false. Shipping it would have minted a confidently-wrong
annotation in the same session spent removing one.

**An annotation must not pre-commit an open disposition.** `category_of/2`'s three consumer modules
carry *measurement-and-pointer* notes, not dark declarations. OQ-316 has not ruled; if it rules
*repair*, nine "dark by construction" comments would become confidently wrong.

**Every claimed absence carried a control.** The linter change has a naturally-arising pair (1 fires
/ 313 decline). The `container_candidates.json` reader sweep has a positive control (the identical
sweep returns 109 hits for `corpus_data.json`). The retirement's 0-consumer sweep has one (9
external hits for the sibling `category_of/2`, which is why it was kept). The new regression tests
include a *decline* case, not just a plant — test (e) shows the probe declines on `unknown`, the
value the engine actually produces, so the corpus zero does not rest on a one-sided control.

---

## Gate interaction worth recording

The first full `./scripts/gate.sh` after Phase 5 came back **RED on two rows, both mine**, and both
were worth the trip:

**`bound selector` — RED, and CORRECTLY so.** Two lines of my own provenance prose contained the
literal text `constraint_signature(_, natural_law)` / `constraint_signature(C, natural_law)`. They
sit inside **Python string literals**, not comments, so the checker's `_is_comment` carve-out
(which handles `#` and `%`) did not apply. **The right fix was to reword my prose, not to widen the
checker.** A blanket "skip Python string literals" rule would create a real blind spot: Python
scripts in this repo legitimately build swipl goals as strings, and such a string IS a call site.
The checker is drawing the line in the right place; my prose was quoting a call shape it should
have described instead. Reworded; the meaning survives intact.

**`module bounds` — RED, resolved with a declared allowlist row.** The new tests call
`signature_detection:coordination_scaffold_signature/1`, a non-exported helper. Its sibling
`natural_law_signature/1` already carries a `ROLE=helper-static` row for the *same test file* and
the same reason. An accessor — the checker's default suggestion — would only rename the bypass
here, because the signature predicate **is the thing under test**. Declared with that reasoning,
not silenced: if `coordination_scaffold` ever fires, those tests go red first.

Final: **GATE GREEN** (24 rows), after regenerating both Phase 2 outputs so the shipped artifacts
match the reworded source.

---

## The generalization this session earned (candidate for `build_discipline.md`)

**Dead-by-empty-INPUT and dead-by-CONSTRUCTION present identically at the call site and repair
differently.** Two instances landed in this one pass:

| | dead by construction | dead by empty input |
|---|---|---|
| `has_viable_alternatives/2` | the `false` branch — no clause can emit it | the `true` branch — only `intent_viable_alternative/3` supplies it, and that table is empty (GAP-08) |
| `domain_priors:category_of/2` | — | `physical_natural` dispatches correctly under a planted claim; the claim vocabulary is authored 0 times in ~5,311 files |

At the call site both look the same: a predicate that returns one value forever. The repairs are
not even in the same category — construction-dead needs a **code** change (or a ruling that the
branch should not exist), input-dead needs **data** or a decision to retire the axis that consumes
it. Guessing wrong sends the work to the wrong place: OQ-296's plan described `category_of/2` as a
collapsed classifier needing repair, when the predicate is fine and its feed is empty.

**The discriminator is cheap and should be routine: plant the input the branch needs and see
whether the predicate fires.** A predicate that fires on a planted input is input-dead; one that
cannot fire on any input is construction-dead. That single probe separated the two cases here and
took one query. Run it *before* characterizing any constant-valued predicate — the characterization
is what routes the fix.

Prior-art grep: `build_discipline.md` documents `has_viable_alternatives`'s fail-close (`:780-782`)
but does not carry this distinction as a named diagnostic. Recorded here as a promotion candidate
rather than minted directly — the promotion is the operator's call, and two instances in one
session is the evidence for it, not a decision.

## Caveat on the roster's remaining accuracy — read this before citing any Phase 3 annotation

Three of ~20 roster entries were wrong. **That is not a 15% error rate on the roster; it is a 3/3
hit rate on the subset that got close attention.** All three were found in sites that were read,
run, or diffed. The eighteen Phase 3 sites got **comment-only** treatment under the
confirm-at-edit-time rule, which is genuinely weaker evidence than a read-and-diff: it confirms the
atom appears in a live code path, not that the surrounding claim about the site is right.

**Treat the remaining roster accuracy as UNMEASURED, not as good.** If any of those eighteen sites
ever becomes load-bearing for a claim, re-verify it directly — do not cite this session's
annotation as confirmation. The annotation records what was confirmed at edit time and nothing
beyond it, which is exactly why each one states its own scope.
