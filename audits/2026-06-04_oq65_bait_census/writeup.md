# OQ-65 Detector-Bait Census — Writeup (2026-06-04)

**Question (ISSUES.md OQ-65):** How much of FSM's (false-summit detector's) corpus-wide
firing measures authored convention rather than detected naturalization — and is the
bait-vs-omega-routed split a usable committer-axis discriminator? Hard constraint from the
2026-06-03 session: phrase-level greps UNDERCOUNT (maxwell's bait says "included to
evaluate"; "included to trigger" appears in exactly one file) — the census must read
extracted text per file.

**Provenance:** corpus n=1106 (`prolog/testsets/`). Extraction manifest:
pipeline 2026-06-04T06:30:31Z commit 1f61da4 (firing bit only). Firing-crossing manifest:
fresh run 2026-06-04T13:46:40Z commit c463b17, code_dirty=true where the dirty files are
CLAUDE.md + one docs/technical note + the untracked census script — engine files clean.
Tool: `python/audits/oq65_bait_census.py` (re-runnable; writes its working set to `outputs/`
— this archive holds the canonical 2026-06-04 artifacts, moved from the workspace after
completion per the audits/ location mandate).

## Method

Five **read** channels over comment text and balanced-paren-captured omega terms, plus two
**mechanical** channels (witnessed sizes at census time):

| Ch | Definition | Files | Items |
|---|---|---|---|
| A | sentences with `beneficiar*` + FSM-family term (consecutive `%`-lines joined) | 492 | 810 |
| B | sentences with `beneficiar*` + listed purpose-verb, no FSM term (+1 routed seam hit) | 6 | 9 |
| C-ben | balanced-captured omega/intractable terms mentioning `beneficiar*` | 381 | 611 |
| D | ±200-char proximity windows, BOTH anchors, files not in A∪B | 79 | 160 |
| F | ALL `beneficiar*` sentences in no-FSM-mention files | 148 | 891 |
| C-fsm (flag only) | FSM-family omegas, beneficiary-free | 349 | 503 |
| E (assert only) | FSM-mention lines outside the union | 158 | 278 |

Partition 800 + 158 + 148 = 1106; cross-check 741 + 158 = 899 FSM-mentioning files.
Self-test: 10 assertions green (4 real positive controls incl. channel placement of both
omega-routed exemplars; B-isolating synthetic; omega-truncation assertion; E beneficiary-free
assertion; partition + cross-check; value-atom zero-assertion + maxwell positive control;
unlisted-verb seam probe + synthetic positive control; decoy marker-strip; C-ben population
pin). Every read item (~2,500 across 845 stream entries) was read and classified by a model
over 26 chunks; per-file verdicts with evidence quotes in `oq65_census_verdicts.jsonl`;
final assembly (`--assemble`) reconciles to exactly 1106 with `verdict_source` per file.

**Reader controls (blind decoys):** no-marker bait (derived from maxwell's real bait,
marker-stripped + topic-swapped) → correctly classified `explicit_bait`; substantive
false-positive control → correctly `substantive`. The omega-routed decoy is **VOID by
construction flaw** (derived from env_instability's *first* omega — the substantive-shaped
`natural_law_vs_distributed_extraction`, not `false_summit_beneficiary_ambiguity`); the
reader classified the derived text correctly while blind and flagged the flaw in its
evidence note. Key corrected with full documentation (`decoy_key.json`). Omega-routed
discrimination is independently witnessed: both real exemplars (environmental_instability,
nuclear_impossibility) classified omega_routed in-stream.

**Classification rules** (converged during the read; contrast pairs witnessed in verdicts):
- `explicit_bait` — commentary states the beneficiary is authored to exercise/trigger/
  evaluate the detector.
- `omega_routed` — FSM/detector/signature/engine NAMED with a fires/triggers-class verb tied
  to the omega's resolution. Passive "false summit detected/confirmed/reclassifies" without
  a named engine = substantive (pairs: sid 173 vs 592; 545 vs 535; 495-corrected vs 610).
- Predictions-of-correct-firing on authored data ("engine flags this: beneficiaries exist")
  = substantive, no flag.
- Prose-vehicle FSM-as-evaluator misconception = flag `adjudication_expectation_prose`.
- `substantive` resolves to fsm_aware_substantive / no_fsm_commentary via file-level
  fsm_mention at assembly.

## Results

| Category (headline, priority-resolved) | n | % |
|---|---|---|
| explicit_bait | 2 | 0.18% |
| omega_routed | 75 | 6.8% |
| fsm_aware_substantive | 558 | 50.5% |
| fsm_aware_no_beneficiary_link | 264 | 23.9% |
| no_fsm_commentary | 207 | 18.7% |

Flags (independent): omega_routed 75; adjudication_expectation_prose 13 (10 prose-only);
**expectation-authored union 87/1106 = 7.9%**; bait_adjacent_phrasing 5;
template_rule2_citation 3; fsm_aware_nonauthoring 1; nonagent_referent_candidate 29;
engine_gate_tied 1.

**1. Bait population (OQ-63 consumer):** explicit_bait = maxwell_demon_impossibility +
total_war_winnability_post1945__structural_contraction_reading ONLY. Bait-authored values =
{entropic_universe_hypothesis, hypothetical_survivors_counterfactual} — exactly the two
already ruled/registered. **Zero new bait found by a complete read.** OQ-63's
suffix-probe-undercount scope qualifier is closed; the filtering-ruling precondition is met.

**2. FSM-statistics validity (firing crossing):** fresh firing set = 10 (zero delta vs the
morning run). **6/10 expectation-authored**: 1 bait (total_war) + 5 omega_routed
(animal_moral_status__property, environmental_instability, nuclear_impossibility,
press_reformation_causality__technological_inevitability,
technology_reformation_causality__technological_determinism). Remainder: article_27 +
humane_treatment fsm_aware_substantive; reformation_composite__technological_mediation
no-beneficiary-link (empty extraction); statutory_debt_ceiling__constitutional_nullity no
FSM text at all. This supersedes 4/12 (2026-06-03): the agent_beneficiary registry fix
removed maxwell/humane from the firing set, RAISING the expectation-authored share to 60%.

**3. Discriminator readout (observation, adoption escalated):** the postures are textually
distinguishable. Bait = NL-physics hosts + non-agent values + authored-purpose statement +
FSM-as-extraction-evaluator misconception. Omega-routed = named-engine-behavior tied to an
omega's empirical resolution; template-supported (3 verbatim "Rule 2" / "FSM schema gate"
citations prove the generation template mandates the routing); domain-spanning. Both differ
from the corpus-dominant descriptive pattern. The press_reformation gate-two host (OQ-66) is
omega_routed with authored-open omegas — consistent with its hold.

**Side findings:** 29 nonagent-referent beneficiary candidates (incl. explicit
"Non-agent beneficiary" and "Abstract beneficiary" authored labels — preparedness_retention,
kodashim study_as_exercise, jcpoa binding, article_27 coordination) feed OQ-64.
tsunami_stone__catastrophe_validation explains *deliberate non-authoring* of beneficiaries
(the bait mirror-image). One new seam hit (predictive_surveillance, china run) was
auto-routed to the read stream and classified substantive.

## Method corrections (witnessed; recorded for re-runs)

1. Recon's omega regex `omega_variable\([^)]*\)` truncates at the first inner `)`; balanced
   capture found 345 omega terms whose beneficiary mention the truncating regex cut off
   (C-ben 162→381 files). The pre-fix channel table is superseded-unreproducible.
2. Channel D must anchor on BOTH token families (beneficiar-anchored-only failed the E
   assertion at ~200-char boundaries; 2 witnessed violations).
3. The 2026-06-03 "445 files" figure did not reproduce (461 at n=1106; growth ≈3 files):
   the ad-hoc scan is superseded; this census's method is saved and re-runnable.
