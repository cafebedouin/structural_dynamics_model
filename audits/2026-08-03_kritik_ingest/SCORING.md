# Phase 2 — Manifest scoring against PROPOSAL.md (read-only deciding pass)

Date: 2026-08-03. Inputs: the four manifests committed at `47085548` (evidence copies in
this dir); predicted lists and decision rule are PROPOSAL.md's, committed at `1bd57a84`
BEFORE any run. Scoring altitude: subject+stance matching per the pinned rule; contestable
calls are marked and the verdict's robustness to them is stated.

## Coherence vs the fresh control

Control (`emotives.md`, macintyre manifest): 1 contested kernel, 3 readings pairwise
distinct by subject+stance, 2 independent non-kernel axes, 4 omegas, populated fracture
scan. Both replicate manifests MEET OR EXCEED this bar: 5–6 pairwise-distinct kernel
readings each, explicit axiom-contradiction pairs (3 each, with stated bases), deferred
axes with individual reasons, self-critical omegas. Neither is mush; readings are
pairwise distinguishable by subject+stance throughout. **Coherent: YES for both.**

## Cap K NW (N = 10 predicted; manifest = 5 kernel readings + 1 selected axis)

Per manifest reading (hit → predicted #; idiom):

| Manifest reading | Hit | Idiom | Note |
|---|---|---|---|
| accumulation_reading | #1 (1NC core K) | tag-leaning | Echoes 1NC tag ("built-in output… not a policy failure") + card authors (Das, Waitzkin) |
| market_exchange_reading | #8 (Sustainability) | mixed | Decoupling/innovation/K-curve = tag names; Hayek/knowledge-problem frame = card layer |
| world_system_reading | — (no block) | card | Pure read-through: Ajl/Schmelzer unequal-exchange position; NO block heading names it |
| neoliberal_regime_reading | #7 (Aff Link/Perm: single payer solves) | card | Reframes the perm as neoliberalism-vs-capitalism-as-such — a literature distinction, not a tag |
| growth_process_reading | — (no block) | card | Pure read-through: degrowth "growth imperative separable from capitalism" — no block counterpart |
| framework_competitiveness (axis) | #2 + #6 (both Framework blocks, one axis) | tag | The two-sided framework dispute as one axis |

- **Precision = 4/6 (0.67)** ≥ 1/2 ✓
- **Recall (strict, selected readings only): #1,#2,#6,#7,#8 surfaced = 5/10 (0.50)** ≥ 1/3 ✓
  (lenient +#10: `transition_war_risk` surfaced as a DEFERRED axis with correct
  subject+stance → 6/10; #3/#4/#5/#9 absorbed into reading deltas, not distinct — misses)
- **Idiom among hits: ~2 tag / 1 card / 1 mixed — NOT majority tag.** The two non-hit
  readings are unambiguous card-idiom (positions the tag layer never names).

(b) fails on the majority-tag-idiom conjunct; (d) fails on recall (0.50 ≥ 1/3);
(a) fails on coherence. → **(f) Partial recovery** via the mixed-idiom clause.

## Biopower NW (N = 9 predicted; manifest = 6 kernel readings selected)

| Manifest reading | Hit | Idiom | Note |
|---|---|---|---|
| totalizing_biopolitical_reading | #1 (1NC core K) | tag-leaning | 1NC governmentality tag + Agamben/Mbembe/Esposito extension |
| reformist_iatrogenic_reading | #7 (Aff Link/Perm) — CONTESTABLE | card | Stance match (reform possible); Illich frame is card-layer; no tag names iatrogenesis |
| counter_conduct_reading | #5 (Neg Alt) | tag | Alt blocks name Counter-Conduct verbatim |
| coalition_governmentality_reading | #9 (Aff Alt-fails) | card | "Purist counter-conduct is anti-Foucauldian" = Alt-fails stance via Ilott archival reading |
| empirical_falsification_reading | #8 (Aff Impact) | tag-leaning | "Biopower = Wrong" / "Non-Unique" block names appear nearly verbatim (Connolly, Meloni) |
| post_political_totalization_critique_reading | #8 (Aff Impact: AT Necropolitics) | card | Kioupkiolis/Airewele/Schwartz positions, deeper than the tag |

- **Precision = 6/6 (1.0); conservative (drop the contestable reformist call) 5/6 (0.83)** ≥ 1/2 ✓ either way
- **Recall (strict): #1,#5,#7,#8,#9 = 5/9 (0.56)** ≥ 1/3 ✓ (conservative without #7: 4/9 =
  0.44, still ≥ 1/3 ✓; lenient +#2/#6 via the deferred framework axis: 7/9)
- **Idiom among hits: ~3 tag-leaning / 3 card — MIXED, not majority tag.**

Same routing: (b) fails only on idiom; (d) fails on recall; (a) fails on coherence.
→ **(f) Partial recovery.**

## Replicate agreement

Both replicates land in **(f)** → **(e) does NOT obtain.** Same outcome class, same
failure pattern (numeric (b)-thresholds pass, idiom mixed), same structural signature
(kernel readings scaffolded on the block layer but populated from the card literature,
plus 2 extra pure read-through readings each with no block counterpart). Under the
executing design version (same camp + format), this is format-STABLE behavior. The
verdict is robust to every contestable sub-call above (no flip within their ranges).

## AT Fiat K (meta-layer only; NO arsenal weight, per (c))

**Pre-registered (c) — flat-routing — did NOT occur.** SCOPE produced a 6-reading
contested kernel (`fiat_efficacy_kernel`) from the single-voice answers-only file,
one reading per card's distinct authority (social-movement empiricism, Galea,
Badiou/McGee, Bagg, Mauri, McGee & Romanelli), and itself flagged the one-sidedness
("all cards argue FOR… none against") — a grounds-contest kernel rather than a
conclusion-contest kernel. Caveats: UNGROUNDED (uniform --skip-search; the research
grounding the plan wanted for this file never ran), and by design this observation
distinguishes nothing on the arsenal question. As a single-voice behavior check: the
documented under-routing tripwire did not fire under the primed prompt on this file
(one observation, ungrounded — not a resolution of the tripwire).

## Notable unscored observations (for the writeup)

1. **SCOPE recognized the arsenal form on its own** — Biopower's
   `omega_debate_genre_distortion` warns the readings may be strategically exaggerated
   versions of the theory and tells non-debate consumers to verify against primary
   texts; Cap's fracture notes say each tradition is "independently sourced from the
   file rather than invented."
2. **Kernel choice is layer-crossing in both replicates:** the kernels are definitional
   contests (`capitalism_referent`: what does 'capitalism' name; 
   `biopower_health_administration`: what is state health administration) — not the
   Neg-vs-Aff block dichotomy. The block layer supplies subjects/stances; the card layer
   supplies the reading structure.
3. Cap K NW ingested at 339,501 tok — the largest single-document SCOPE ingest witnessed
   in this repo; no degradation signature relative to the 103k Biopower run is apparent
   at manifest level.
4. Emphasis ruling (A) binds: none of this may be claimed as a property of the format;
   it is a property of emphasis-blind ingestion. The extractor + re-run remains the
   named discriminator.

## Verdict and recommendation (per the pre-registered rule)

**Outcome: (f) Partial recovery, replicate-stable.** Prescribed action: graduate ONE
meta-layer file with the caveat attached; do NOT expand the K-file corpus on this
evidence.

**Recommendation: graduate `AT Fiat K` for the Phase 3 full run** (meta-layer priority;
its frozen manifest `fiat_efficacy_kernel_2026_20260803_102258` is ready), caveat
attached: (f)-grade evidence, emphasis-blind, ungrounded. `T Framework` (584K) is the
alternative meta-layer candidate but has no dry-run manifest and would need its own
dry-run first. The two arsenal replicates do NOT graduate.

**OPERATOR GATE: no writes past this point without your go.**
