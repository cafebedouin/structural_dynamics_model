# CC Audit — Does the Prolog substrate confirm or falsify the seat/orientation invariant?

**Date:** 2026-06-16
**Code commit:** `9f433218` (worktree branch `seat-invariant-audit`)
**Corpus tiers loaded (overlay-took-effect witnessed per process):**
`testsets` (corpus_constraint=71), `testsets_flash` (=960), breadth `archives/datasets/kernel_v1` (=1106).
**Mode:** read-only falsification pass. No implementation file modified. Spec-vs-code drift reported, never closed.
**Evidence:** all raw output and probe sources under `./evidence/`.

> **Stance carried throughout.** "I didn't find it" ≠ "it isn't there." Every negative finding below
> carries a positive control showing the probe *fires* on a case it must flag. Each theory-killer is
> reported as a **located clause + routing**, NOT confirmed/denied — the IF-gated definitional half is
> escalated to the operator, never silently resolved in either direction.

---

## 0. What the audit actually fired (verification controls)

| Control | Status | Witness |
|---|---|---|
| Overlay-took-effect precedes every census | ✓ | `corpus_constraint` count printed per tier before any verdict count (evidence/out_*). |
| Enumeration completeness = two converging probes | ✓ | grep of `^cs_verdict(` heads (7) **converges with** runtime `clause/2` enumeration (7, identical set). |
| Condition-false control on the authored-only FNL path | ✓ | 7 `natural_law_constraint` patterns in flash; only the 4 **with** a beneficiary fire `false_natural_law_constraint`; the 3 **without** stay silent (evidence/out_fnl_control_flash.txt). |
| Killer-1 positive control (trace surfaces a computed dep where one exists) | ✓ | same trace method surfaces `boltzmann_compliant` on `signature_detection:false_natural_law/2` while finding only authored facts on `cs_verdict` FNL. |
| Seat-test both-direction control (can-fire ∧ won't-over-call) | ✓ | 2×2 one-field edit: ε moves `dr_type` not `cs_pattern`; AG moves `cs_pattern` not `dr_type`; restore verified equal to baseline (evidence/out_seat_test_flash.txt). |
| P7 probe positive control (the 0 is not a dead query) | ✓ | `constraint_signature/2` fires 960/960 on flash returning 7 distinct atoms — `natural_law` simply not among them. |
| Twin/breadth replication with absence-labelling | ✓ | `testsets` (small/current), `testsets_flash` (the only tier authoring `self_enforcing`), `kernel_v1` (breadth). Divergences labelled corpus-content vs substrate. |

**Key corpus-content fact that shapes everything below:** the headline tension clause
(`cs_verdict false_natural_law_constraint`) is **dormant on `testsets`** — *no* live constraint authors
`self_enforcing` authority, so the `natural_law_constraint` pattern never arises (0/71). The only tier that
authors it is `testsets_flash` (7 files). This is the absent-vs-condition-false distinction made load-bearing:
the FNL controls below run on flash because that is the only place the condition can be true.

```
# evidence/out_verdict_surface_testsets.txt  (71 constraints)
cs_verdict firings: 28  → false_interpretive_accretion:18, false_diffuse_reconstruction:7,
                          false_anchored_fixity_accretion:2, false_implicit_practice:1
cs_pattern: NO natural_law_constraint present (patterns: interpretive_accretion 25, no_pattern_match 32, ...)
# evidence/out_verdict_surface_flash.txt  (960 constraints)
cs_pattern natural_law_constraint: 7   cs_verdict false_natural_law_constraint: 4
```

---

## 1. Per-prediction verdicts P1–P9 (each with inline witness)

### P1 — `self_enforcing` ruled out of drift/cover-story. **PARTIAL → leans FAIL as literally stated.**
Witness (flash, evidence/out_p1_p5_p7_p8.txt), the 7 `self_enforcing` constraints:
```
ai_governance_… | - - -            article_27_veto_… | - - -
monetary_anchor_… | - - -          nuclear_impossibility_… | - - -
technology_reformation_… | - masking -     total_war_reachability_… | - - -
total_war_winnability_… | - masking -
```
- `cs_cover_story_active`: **structurally ruled out** for `self_enforcing` — clause body (cs_pattern_detection.pl:306–311)
  requires `cs_authority_grounding(C, extraction)`; `self_enforcing ≠ extraction`, so it cannot fire. 0/7. ✓ for the cover-story half.
- `cs_drift_mismatch`: 0/7. (None author the drift/foreclosure structure.)
- **`cs_authority_masking`: fires 2/7** — its body (cs_pattern_detection.pl:294–299) requires `AG \= extraction` + a
  computed extraction signature; `self_enforcing` qualifies. So `self_enforcing` is **not** immune to the masking concealment detector.

Verdict: if P1 means "immune to *cover-story*," it holds (structural). If it means "immune to *all* concealment
detection," it is **false** — `authority_masking` is a live exposure path for `self_enforcing`. Reported straight.

### P2 — verdicts fire only on mismatch (claim vs contradicting datum). **HOLDS in form; substrate split is the finding.**
All 7 `cs_verdict` clauses are claim-contradiction-gated (each fires `cs_pattern_is(C, claimed) , <contradicting datum>`).
The **substrate of the contradicting datum splits**:
- 6 of 7 threshold an authored `constraint_metric` (suppression_requirement / theater_ratio) or a `coordination_type` against the claimed pattern.
- **`false_natural_law_constraint` (cs_pattern_detection.pl:247–249) checks mere *presence* of an authored `constraint_beneficiary`** — no threshold, and **no computed signal at all**.
- **No `cs_verdict` clause consults the computed `constraint_signature/2`.** The genuine computed cross-check lives in a *separate*
  layer: `cs_authority_masking` / `cs_cover_story_active` / `cs_displaced_beneficiary` / `cs_grounding_mismatch` (which read
  `signature_detection:constraint_signature/2`), and `signature_detection`'s own FNL/FCR/FSM (Boltzmann-gated).

Positive control that the gate checks a *condition*, not presence (flash):
```
natural_law_constraint pattern | beneficiary | cs_verdict_FNL
ai_governance_…       | yes | fires      article_27_veto_…   | no | silent
nuclear_impossibility_… | yes | fires    monetary_anchor_…   | no | silent
technology_reformation_… | yes | fires   total_war_reachability_… | no | silent
total_war_winnability_… | yes | fires
```
So P2's "mismatch-only" is satisfied *in form* by every verdict; but the **substrate** of the FNL mismatch is
authored-vs-authored, not claim-vs-computed. This is **the headline tension** and feeds Killer-1.

### P3 — two gaps, two mechanisms (instant vs temporal). **HOLDS (with a third-family route, below).**
- **Instant family:** `signature_detection:constraint_signature/2` — single-snapshot, computed from `constraint_metric` + domain priors + structure (cs_pattern_detection.pl:274 comment; fires 960/960 flash).
- **Temporal family:** `cs_drift_engine:cs_drift_trajectory/3` (cs_drift_engine.pl:47) reads the authored `cs_drift_state(UID,_,gap(Dir,Mag,Ack))` t0→t1 gap vector and extends it to a terminal attractor. *(Caveat: the "series" is an authored one-shot gap summary, not a computed multi-snapshot trajectory — the input represents change-over-time, but it is itself a single authored fact.)*
- **Kill A** (a "drift" from a single snapshot): not found — every drift/foreclosure path requires `cs_drift_state` (the gap), absent ⇒ no drift verdict.
- **Kill B** (a third irreducible family): see **Killer-2** — routed, not flat-resolved.

### P5 — beneficiary is the physics-washing discriminator. **HOLDS.**
`false_natural_law_constraint` reads `constraint_beneficiary` (247–249, confirmed). `cs_naturalized_mountain`
(cs_pattern_detection.pl:258–265) reads `constraint_beneficiary` **and** `constraint_victim` + low ε + extraction/diffuse_epistemic AG.
Census: `cs_naturalized_mountain` fires **25** on flash, 0 on testsets. The beneficiary fact is a live discriminator input. ✓

### P6 — no self-opacity (no drift/keeping verdict from a single snapshot's authored fields). **HOLDS.**
The only single-snapshot authored-field verdict is `cs_verdict FNL` — and it is an **instant pattern-contradiction**, not a
drift/keeping verdict. All drift/keeping verdicts (`cs_drift_trajectory`, `cs_axiom_foreclosed`) require the authored `cs_drift_state`
gap; none is computable from a lone snapshot of `cs_authority_grounding`/`cs_kernel_codification`/`constraint_beneficiary`. ✓

### P7 — no genuine-seat-free + concealing constraint. **VACUOUSLY HOLDS — antecedent EMPTY everywhere (absence-bounded).**
`genuine natural_law` = `signature_detection:constraint_signature(C, natural_law)` (the strict, fail-closed
`natural_law_signature/…` profile, signature_detection.pl:113–116).
```
genuine natural_law signature:  testsets 0/71 | flash 0/960 | kernel_v1 0/1106   (≈ 0 / 2137)
P7 probe positive control: constraint_signature fires 960/960 on flash; distinct atoms =
  [ambiguous, constructed_high_extraction, constructed_low_extraction, coupling_invariant_rope,
   false_ci_rope, false_natural_law, false_summit_mountain]  — natural_law simply never qualifies.
```
There is **no genuine-seat-free constraint in any available corpus** to test against. P7 holds only because its
antecedent is unrealized. **Labelled as absence, not as a demonstration** that such a constraint would not conceal —
the discriminating case does not exist on disk and was not constructed (out of read-only scope). Feeds **Killer/P7** routing.

### P8 — `epistemic_consensus` is a type, spawns no new detector. **HOLDS.**
`cs_classify` emits `epistemic_consensus` (cs_pattern_detection.pl:165) but **no `cs_verdict` clause names it** (runtime
enumeration: the 7 verdict atoms do not include it). Census: pattern fires 1 (testsets) / 26 (flash); **0 carry any `cs_verdict`**
(probe explicitly checked and reported "carries NO cs_verdict"). A captured-consensus case would route through the existing
instant-mismatch surface (`cs_grounding_mismatch`), not a novel detector. ✓

### P9 — reflexive 7→9 pattern-atom drift is closed. **HOLDS (both halves).**
- (a) **code emits 9 atoms** — census across testsets+flash observed all 9: `marked_revision, interpretive_accretion,
  diffuse_reconstruction, implicit_practice, anchored_fixity_with_accretion, anchored_fixity_brittle, natural_law_constraint,
  epistemic_consensus, no_pattern_match`. ✓
- (b) **spec v5.2 enumerates the same 9** — `commitment_systems_sketch_v5_2.md:18–23`: *"The implementation has always emitted
  nine pattern atoms from `cs_pattern/3`; the sketches enumerated seven … the two uncounted atoms — `natural_law_constraint`
  and `epistemic_consensus` … now acknowledged."* The 7→9 drift is **closed in v5.2**. No spec-vs-code drift on the atom set.

---

## 2. The three theory-killers + P7 — located clause + routing (NOT confirmed/denied)

### Killer-1 / P2 — assertion-only pathology verdict. **Route: `DISPOSITIVE-IF[independent-substrate ruling]`.**
- **Located clause:** `cs_verdict(C, false_natural_law_constraint) :- cs_pattern_is(C, natural_law_constraint), narrative_ontology:constraint_beneficiary(C, _), !.` (cs_pattern_detection.pl:247–249).
- **Code-fact (settled here, by transitive read + live trace):** the body reaches **no** computed signal.
  `cs_pattern_is → cs_pattern → cs_classify` reads only `cs_kernel_codification` + `cs_authority_grounding` (authored,
  UID-keyed); `constraint_beneficiary` is authored. **Zero reach to `constraint_signature`/Boltzmann.**
  **Positive control for the trace method:** the *same* method run on `signature_detection:false_natural_law/2`
  (signature_detection.pl:987–1005) *does* surface `boltzmann_compliant` + `cross_index_coupling` — so a clean trace on
  the cs path is not an artifact of a method that cannot see dependencies.
- **Live divergence witness (the two FNLs disagree on the same constraint):**
  on `ai_governance_legitimacy__market_libertarian_reading`, `cs_verdict` FNL **fires** (authored self_enforcing + beneficiary=entrepreneurs)
  while `signature_detection` FNL is **silent because `boltzmann_compliant = compliant(0)`** — the computed layer says "no forgery,"
  the authored layer says "false natural law." The authored-only verdict fires *where the computed check declines*.
- **Theory-definition (ESCALATED — operator's call):** does authored beneficiary-presence count as the *independent substrate*
  P2 requires? Two readings, presented side by side, neither picked:
  - *Role-level independent* (operator's stated lean = NOT dispositive): the beneficiary fact is the Q1 *who-benefits*
    fact, distinct in role from the seat-holder's *self-description* (`self_enforcing`). It is mismatch-gated, just with both
    sides hand-authored. → invariant survives this joint.
  - *Counter-reading* (stated beside it): a detector whose contradicting substrate is a hand-authored beneficiary is only as
    strong as someone bothering to author it — an adversary omits the beneficiary and the verdict goes quiet. A genuine
    weakness, but not assertion-only *firing*.
- **Kill condition (unmet unless the ruling lands "not-independent"):** confirmed assertion-only emission **AND**
  authored-beneficiary ruled *not* the independent substrate. Code-fact half = assertion-only path **confirmed**; weight waits on the ruling.

### Killer-2 / P3-Kill-B — a third irreducible detection family. **Route: `DISPOSITIVE-IF[P3-count-is-exact ruling]` — but the code-fact defuses the own-field face.**
- **Located clause:** `cs_drift_mismatch(UID, Source)` (cs_drift_mismatch.pl:56–69).
- **Code-fact (settled here):** it is a **conjunction** —
  `cs_any_foreclosed(UID, Traj, AxFc)` [CS-foreclosure] **AND** `cs_is_metric_stable(C)` [network stability].
  - The CS-foreclosure side is **reducible to the two named families**: `cs_drift_trajectory` (temporal gap → terminal) and
    `cs_axiom_foreclosed` (instant: authored axiom + grounding + gap). Neither is novel.
  - Its only **novel ingredient** is `cs_is_metric_stable/1`, which calls `network_dynamics:detect_network_drift/3` +
    `network_drift_velocity/4` — and `detect_network_drift` traverses **neighbours** (`drifting_neighbors`, `ContagionList`,
    `effective_purity`): the **between-constraints NETWORK layer**.
- **Scope discriminator (from the plan's declared scope):** the network layer (relations between constraints) is
  **out of the one-seat retyping's scope** by declaration. So `cs_drift_mismatch`'s novelty is **not a third face of the seat** —
  it is a cross of (CS-axis foreclosure) × (network-axis stability). Its "own-field" face reduces to instant∨temporal.
  Census: fires 10 (testsets) / 18 (flash).
- **Theory-definition (ESCALATED):** is P3's "two" an **exact count** (a third own-field family → dispositive) or a **floor**
  ("at least these two" → graded)? Operator lean: probably a floor; routed DISPOSITIVE-IF anyway, per the symmetric-with-killer-1
  ruling (do not bet on intent at a dispositive joint).
- **Net:** even if "two" is exact, the located clause is **not** an own-field third family (its novelty is the out-of-scope
  network conjunct), so the kill condition ("irreducible own-field third family") is **unmet on the code-fact** — independent of the count ruling.

### Killer-3 / P4 — a genuine second seat. **Route: `DISPOSITIVE-IF[axes range over independent content-spaces]`. This is the live falsification candidate.**
- **Located axes:** observer `drl_core:dr_type/3` vs committer/CS `cs_pattern`/`cs_axiom_foreclosed`.
- **Coupling discriminator — 2×2 one-field seat-test** (real flash constraint, throwaway process, restore verified;
  evidence/out_seat_test_flash.txt):
```
BASELINE  (eps=0.25 KC=implicit AG=self_enforcing) | dr_type=rope    | cs_pattern=natural_law_constraint
vary OBSERVER input ε:  0.02 → dr_type=rope    cs_pattern=natural_law_constraint
                        0.95 → dr_type=unknown cs_pattern=natural_law_constraint   (ε moves dr_type, NOT cs_pattern)
vary CS input AG:    lineage → dr_type=rope    cs_pattern=no_pattern_match
                  extraction → dr_type=rope    cs_pattern=no_pattern_match
                    practice → dr_type=rope    cs_pattern=implicit_practice         (AG moves cs_pattern, NOT dr_type)
RESTORED → dr_type=rope cs_pattern=natural_law_constraint  (== baseline; harness verify_restore passed)
```
  This is simultaneously **can-fire** (each axis's verdict moves under its own input) and **won't-over-call** (neither axis
  moves under the other's input). The two axes respond to **disjoint authored inputs** and never perturb each other.
  Corroborated by architecture: v7 Axiom 7 / Theorem 7 declare the committer axis a **co-equal second axis with detection
  independence**; `dr_type` reads metrics/signature, the CS pattern reads `cs_*` fields — disjoint input sets, not the same
  content under observer rotation.
- **The empirical lean is toward FALSIFICATION:** disjoint inputs + disjoint response = the axes range over **independent
  content-spaces**, which is exactly Killer-3's dispositive IF condition. This is *not* "one seat under gauge rotation"
  (gauge rotation = *same* input set, rotated observer context — here the input sets are different).
- **Theory-definition (ESCALATED — the one the operator must rule):** does the CS/committer axis count as a **second SEAT**
  (→ dispositive; one-seat retyping falsified at P4), or is it the **orientation face** of the *one* seat — the holder's
  relation to its own commitment (keeping/showing) — which the invariant *already grants* (→ corroborates one-seat)? The 2×2
  proves there are two independently-varying classification axes; whether the second is a *seat* or the *orientation* the
  invariant reserves is the definitional call the code cannot make.

### P7 cross-substrate contradiction sweep. **Route: `DISPOSITIVE-IF[genuinely-seat-free ∧ unresolved ruling]`.**
- **Code-fact:** the apparent antecedent (genuine `natural_law` signature) is **empty** across 2,137 constraints (§1 P7).
  There is no constraint that is both seat-free-by-computed-signature and concealing — because there is no seat-free-by-computed-signature
  constraint at all. Whatever *claims* naturality and fails routes through `false_natural_law` (the resolved path), of which the
  computed `signature_detection` form fires 1 (testsets) / 8 (flash).
- **Theory-definition (ESCALATED):** "*genuinely* seat-free" is the operator's definitional call; the code only witnesses that
  the strict signature never certifies one here. Absence-bounded: routes-through-`false_natural_law` is checkable and clean; "genuinely
  seat-free exists but uncertified" is not falsifiable on this corpus.

---

## 3. Vocabulary map — including every place the audit's guessed names failed to map

| Audit guess | Resolves to | File:line | Note |
|---|---|---|---|
| `cs_pattern/3` | ✓ | cs_pattern_detection.pl:106 | 9 pattern atoms; reads authored fields only. |
| `cs_authority_grounding` | ✓ accessor | :72 | UID-keyed join; **static** predicate (not dynamic — see §5 note). |
| `cs_authority_masking/3` | ✓ | :294 | authored AG × computed `constraint_signature`. Fires 46 (testsets)/803 (flash). |
| `cs_cover_story_active/2` | ✓ | :306 | triple corroboration; requires AG=extraction (rules out self_enforcing). |
| `cs_displaced_beneficiary/1` | ✓ | :322 | authored AG + computed sig + typed `forecloses` edge. |
| `false_natural_law_constraint` | ✓ verdict atom | :247 | **authored-only** (Killer-1). |
| `constraint_signature/2` | ✓ | signature_detection.pl:74,113 | metric-computed; `natural_law` value never fires on corpus. |
| `dr_type/3` | ✓ | drl_core.pl:435 | **module-qualified `drl_core:` required** — not callable bare (audit guess would `existence_error`). |
| FNL/FCR/FSM | ✓ | signature_detection.pl:987/1244/1503 | Boltzmann-gated computed mismatch. |
| `epistemic_consensus` | ✓ pattern atom | :165 | **no verdict predicate** (P8 confirmed). |

**Mapping failures the audit's vocabulary did not anticipate:**
- **Two-FNL name collision** — `signature_detection:false_natural_law/2` (computed, Boltzmann) **vs**
  `cs_verdict(_, false_natural_law_constraint)` (authored). Different predicates, near-identical names, and they **disagree on the
  same constraint** (§2 Killer-1). The audit treated "FNL" as one thing.
- **`cs_drift_mismatch/2` was unguessed entirely** — the cross-axis (CS × network) detector central to Killer-2.
- **The computed cross-check is not in the verdict layer** — the audit's mental model put the computed signal inside `cs_verdict`;
  in fact `cs_verdict` is authored-vs-authored throughout, and the computed cross-check is a *separate* family
  (`cs_authority_masking`/`cs_cover_story_active`/`cs_displaced_beneficiary`/`cs_grounding_mismatch`).
- **`cs_drift_mismatch/2` clause moved** to :56 (audit/recon said :48).

---

## 4. Spec-vs-code drift (report only — not closed)

- **None on the pattern-atom set:** spec v5.2 (`commitment_systems_sketch_v5_2.md:18–23`) acknowledges all 9 atoms incl. the two
  additions; code emits exactly those 9. P9 currency check is **clean**.
- **`cs_pattern_detection.pl:140–143` records a latent inconsistency in source** (the `interp_layer_implied` branch-A collision on
  `privilege_waiver_threshold`): an authored `interpretation_layer_present` fact is *not* read by the `interpretive_accretion`
  classifier, which derives `interp_layer_implied` instead. Self-documented "no fix this round." **Reported, not touched.**
- **No drift found between the §2/§3 located clauses and their doc comments.** (Findings that merely confirm docs are low-value;
  the high-value items above are the *contradictions* — the two-FNL disagreement and the disjoint-axis 2×2.)

---

## 5. Synthesis — a conditional decision-tree, NOT a net vote

The DISPOSITIVE-IF lattice forbids a single for/against verdict: writing one would secretly resolve the escalated definitions.
The settle-able **code-facts are resolved** (witnessed above); only the **theory-definitions** remain as branch variables.

**Code-facts established (witnessed, not awaiting anyone):**
1. `cs_verdict` is **authored-vs-authored throughout**; `false_natural_law_constraint` is presence-gated and consults no computed
   signal; it diverges live from the Boltzmann-computed `signature_detection` FNL on the same constraint. *(binary, not graded — no
   gap-magnitude carried; supports the verification-specificity reading, not a cross-substrate forgery-cost measure.)*
2. `cs_drift_mismatch`'s CS side reduces to the two named families; its only novelty is the **out-of-scope network conjunct**.
3. `dr_type` and the CS pattern axis respond to **disjoint authored inputs** and never perturb each other (2×2 restore-verified).
4. Genuine `natural_law` signature is **empty across 2,137 constraints**; P7's antecedent is unrealized.

**The tree (leaves keyed to operator rulings):**

```
INVARIANT HOLDS  iff  ALL of:
  R1 independent-substrate = role-level  (Killer-1 not-dispositive; operator lean: YES)
  R2 P3 "two" = floor                    (Killer-2 graded; operator lean: YES — and code-fact already
                                          shows cs_drift_mismatch is not an own-field third family regardless)
  R3 CS/committer axis = ORIENTATION face of one seat, not a second seat  (Killer-3; operator lean: UNSTATED)
  R4 genuine-seat-free treated as unrealized-absence, not a concealed open (P7; operator lean: YES, absence-bounded)

FALLS AT JOINT P4  iff  R3 = "second seat":
  the 2×2 shows two classification axes over independent content-spaces; under the
  two-seat reading the one-seat retyping is FALSIFIED here. To absorb it, the theory must
  either (a) reclassify the committer axis as the orientation it already grants, or
  (b) widen "one seat" to "one seat + one committer coordinate" (a schema change, not a patch).

FALLS AT JOINT P2  iff  R1 = "not-independent":
  the authored-only FNL emission becomes assertion-only pathology; the gap-detection core
  loses its independent substrate. Absorb by requiring a computed cross-check on the FNL path
  (the structural-diagnostics layer already provides one — wiring it into the verdict would close it).
```

**The single highest-value next query (the one that most narrows the tree):** construct the discriminating case for **R3** —
a constraint where the observer `dr_type` and the committer `cs_axiom_foreclosed` make a *content* claim about the **same** axiomatic
question, and pre-register what "they conflict about the same content" (→ orientation/one-seat) vs "they answer different questions"
(→ second seat) looks like *before* running it. The 2×2 here shows independence of *inputs*; R3 needs a case probing independence of
*content-questions*. This is the one discriminator the read-only sweep could not manufacture from the corpus as authored.

**Shortest list of operator rulings that collapses the tree to a single verdict:** **R3** alone is very nearly sufficient —
R1, R2, R4 all carry operator leans toward HOLD and R2 is additionally defused on the code-fact. If **R3 = orientation-face**, the
invariant **holds** across all joints on this substrate. If **R3 = second seat**, the invariant **falls at P4**, and P2/Killer-1 become
the secondary repair question. The audit does not rule R3 — it is a declared seat, the operator's to own.
