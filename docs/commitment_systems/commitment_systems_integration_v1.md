# Commitment Systems → Engine Integration: Evidence Inventory and Analysis

**Investigation date:** 2026-05-17
**Sketch source:** `docs/commitment_systems_sketch_v4.md` (138 lines)
**Engine sources:** `prolog/drl_core.pl`, `prolog/constraint_indexing.pl`, `prolog/transition_paths.pl`, `prolog/narrative_ontology.pl`, `prolog/config.pl`
**Schema source:** `python/constraint_story_schema.json`
**Corpus data:** `outputs/corpus_profile.json`
**Paper source:** `docs/deferential_realism_paper_v6.11.md`

---

## Turn 1 — Evidence Inventory

*No findings, no recommendations. This section records what sources say, with code citations. Section ordering: §1 and §2 establish the two domains. §3 (doc verification) runs next to calibrate source reliability before §§4–5 draw on those sources. §§4–5 are written only after §3 confirms or caveats the sources.*

---

### §1 — What the Commitment Systems Sketch Proposes

*Source: `docs/commitment_systems_sketch_v4.md`, read directly. All anchors are line numbers. No page numbers — this is a markdown file.*

#### Three primitives (sketch lines 7–17)

A **kernel** is "the stabilized arrangement of commitments that constitutes the system's nominal core. This may be a text (a constitution, a foundational paper, a creed), a body of practice (the methodology of a craft tradition), a narrative (an identity's organizing story), a relationship (the implicit terms a marriage rests on), or some combination. The kernel is what participants in the system point to when asked what the system is." (line 11)

An **authority structure** is "the mechanism that determines what counts as legitimate interpretation of the kernel. Authority structures may be external (a priesthood, a judiciary, an academic discipline, a board), distributed (the parties to a relationship, a community of practice, a peer-review system), internal (an individual's self-interpretive practices), or composite. Authority structures have incentives, resource flows, and stakes in preserving their standing; treating them as politically neutral interpretive machinery misses what they are." (line 13)

**Drift** is "unmarked mutation of operational practice relative to the kernel. Drift is intrinsic because the environment the kernel was stabilized within continues to change after the kernel stabilizes. No commitment system preserves perfect fidelity to its kernel across time. The question is not whether drift occurs but whether the system's authority structure can acknowledge drift without losing legitimacy." (line 15)

**Frame-relativity:** "The kernel is frame-relative. At any moment T1, the kernel is whatever participants in the system point to as the reference for that analysis. Drift between T1 and T2 is measured against the T1 kernel, not against some eternal fixed reference. The kernel itself can change between T1 and T2, and that change is itself drift to be acknowledged or denied." (line 17)

#### The structural problem (sketch lines 19–25)

"The people with authority to officially acknowledge drift are usually the same people whose standing depends on pretending there isn't any." (line 21) This is the framework's central observation.

#### Five response patterns (sketch lines 27–45)

The patterns are "claims about attractor states in the configuration space of possible commitment system arrangements, not partitions of all possible systems." (line 29)

**Marked revision** (line 31): Precisely specified kernel. Voluntary authority grounded in expertise. Drift formalized as revision: proposal → check → absorption. "Acknowledgment is marked and legible rather than absorbed silently." Stable when acknowledgment capacity matches environmental change rate.

**Interpretive accretion** (line 33): Fixed text. Authority grounded in continuity with founding text. No functioning formal revision mechanism. "Drift migrates entirely into interpretation. Everyone insists the kernel controls while the operational meaning shifts substantially." Durable across millennia when the interpretive layer absorbs operational drift.

**Diffuse reconstruction** (line 35): Under-specified or intentionally ambiguous kernel. No centralized authority structure. Many parties produce mutually incompatible readings claiming the same source. Persists indefinitely, lacks operational coherence. "Often serves strategic purposes for parties who benefit from operational ambiguity."

**Implicit practice** (line 37): No codified kernel — "the kernel is whatever the system does." Authority grounded in practice itself. "Drift is the mechanism rather than a failure of it." Stable as long as practice remains coherent. "Breakdown is severe when practice loses coherence because there is no fixed referent to reconstruct from."

**Anchored fixity** (lines 39–45): Kernel formalized. Authority structure grounds its legitimacy in the kernel's unchangeability and extracts substantial benefit from preventing kernel revision. "Drift denial is the source of authority rather than a side effect." Two subtypes:

- *With interpretive-accretion layer:* can persist millennia (Hindu Vedic-Brahmanical system, post-development Catholic doctrine). Unrevisable kernel paired with a substantial interpretive substructure that absorbs drift.
- *Without interpretive-accretion layer:* "structurally brittle." Kernel supposed to govern operational practice directly. "Produces accumulating gap and catastrophic breakdown when environmental change exceeds what the kernel can govern." Spartan breakdown at Leuctra is the canonical case. "The framework's strongest cross-domain claim is that anchored fixity without an interpretive-accretion layer produces brittle-fixity failure identically across organizational, identity, and relational systems." (line 45)

#### Why systems fail (sketch lines 47–53)

"Systems do not usually fail because participants are bad or stupid. They fail because the mechanism for admitting change is broken or captured by people who benefit from things staying officially the same." (line 49) Each pattern has a characteristic failure mode following from the pattern plus the environmental rate plus the authority structure's incentives.

#### Different positions see different things (sketch lines 55–63)

Some positions "are systematically unable to see drift they participate in producing, because seeing it would threaten the authority structure that grants them their position." (line 57)

The framework references "presheaf machinery developed in companion work — the framework treats observer positions as forming a category whose morphisms encode structural transitions, with classification data forming a presheaf that often fails the sheaf gluing axiom." (line 61) Described as optional detail; the structural insight is that disagreement between positions is measurable.

#### Four diagnostic questions (sketch lines 65–75)

1. What is the kernel and the authority structure?
2. Is drift occurring, and is it acknowledged?
3. Which pattern is the system in, and is the pattern functional in this environment?
4. Who has standing to acknowledge drift, and do they benefit from pretending it does not exist?

#### Appendix: Provisional Structural Refinements (sketch lines 117–137)

*"Working notes, not theoretical commitments."*

**Drift decomposition** (line 121): Source axis — environmental (response to environment), authority (personnel/institutional evolution), or endogenous (authority incentives produce reinterpretation independent of environmental pressure). Relationship axis — unrecognized or exploited.

**Three structural principles** (line 123): (1) Authority extends only as far as enforcement. (2) Drift is intrinsic. (3) Acknowledgment that changes the system requires standing within the system.

**Lifecycle phases** (line 125): Formation, stabilization, operation, atrophy, renewal-or-dissolution. "The lifecycle dimension is invisible from inside the system — participants cannot know which phase they are in."

**Cross-cutting features** (line 127): *Decoupled formalization* — formal kernel for legibility/ceremony while operational authority sits elsewhere. *Ritualized renewal* — scheduled obligatory moments reconstituting kernel-practice relationship; failure mode is performative vs. exercised renewal. *Velocity mismatch* — operational drift and authority drift at substantially different rates. *Distribution of acknowledgment authority* — concentrated (cathedral) vs. distributed (bazaar). *Kernel encoding substrate* — text/notation vs. embodied/tacit knowledge; mismatches produce apparent acknowledgment without actual drift processing.

**Default conditions** (line 129): *Nesting* — almost every commitment system is nested in larger commitment systems; the single-level system is the limit case. *Coupling* — most commitment systems coupled to other commitment systems; "environmental coupling the framework names is partly coupling to physical environment but mostly coupling to other commitment systems."

**Observer parameters** (line 131): Power (consequences producible), exit options (costs of leaving), time horizons (what temporal patterns a position can observe), spatial-temporal scope (what extent of the system a position sees).

**Trajectory channels** (line 133): *Operational separation* — inner systems continue without outer-system participation, producing parallel commitment systems with overlapping membership. *Dormant-container activation* — system previously suspended with preserved kernel and authority structure reactivated as alternative legitimacy when active outer container loses standing. Meiji Restoration, France's Estates-General 1789, and identity deconstruction patterns are offered as candidate instances.

**Cover story analysis** (line 135): "For a given configuration and position, the framework can in principle predict what drift the position should be unable to see and what cover story will replace it. The form would be: given X configuration plus Y features plus Z position, the predicted blindness is W with cover story V, and observation U would settle the question."

---

### §2 — What the Existing Apparatus Does

*Sources: direct reads of code and schema files, with line-number citations. No reliance on documentation claims.*

#### Engine classification machinery

**Primary predicate: `classify_from_metrics/6`** (`prolog/drl_core.pl:300–385`)

Signature: `classify_from_metrics(+C, +BaseEps, +Chi, +Supp, +Context, -Type)`

Inputs:
- `C`: constraint identifier
- `BaseEps` (ε): base extractiveness — observer-independent
- `Chi` (χ): power-scaled effective extractiveness — computed upstream via sigmoid
- `Supp`: suppression/enforcement requirement
- `Context`: `context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S))`

Outputs — one of eight atoms: `mountain | rope | snare | tangled_rope | scaffold | piton | naturalized | unknown`

**Classification gates in priority order** (each with `!`):

| Line | Type | Key conditions |
|------|------|----------------|
| 300 | mountain | `Supp ≤ mountain_suppression_ceiling`, `BaseEps ≤ mountain_extractiveness_max`, `emerges_naturally(C)`, `effective_immutability_for_context(Context, mountain)` |
| 314 | piton (pre-check) | `coordination_dead(C)`, `BaseEps > piton_epsilon_floor`, `theater_ratio ≥ piton_theater_floor` |
| 323 | snare | `\+ natural_law_without_beneficiary(C)`, `Chi ≥ snare_chi_floor`, `BaseEps ≥ snare_epsilon_floor`, `Supp ≥ snare_suppression_floor`, `snare_immutability_check(Context)` |
| 333 | scaffold | `Chi ≤ scaffold_extraction_ceil`, `has_coordination_function(C)`, `scaffold_temporality_check(C)` |
| 341 | rope | `Chi ≤ rope_chi_ceiling`, `BaseEps ≤ rope_epsilon_ceiling` (if Chi > 0), `effective_immutability_for_context(Context, rope)` OR `emerges_naturally(C)` |
| 352 | tangled_rope | `\+ natural_law_without_beneficiary(C)`, `Chi ∈ [tangled_rope_chi_floor, tangled_rope_chi_ceil]`, `BaseEps ≥ tangled_rope_epsilon_floor`, `Supp ≥ tangled_rope_suppression_floor`, `requires_active_enforcement(C)`, `has_coordination_function(C)`, `has_asymmetric_extraction(C)` |
| 366 | piton (fallback) | `Chi ≤ piton_extraction_ceiling`, `BaseEps > piton_epsilon_floor`, `theater_ratio ≥ piton_theater_floor` |
| 379 | naturalized | `BaseEps > rope_epsilon_ceiling`, `Chi < tangled_rope_chi_floor` |
| 385 | unknown | fallthrough |

**Two-hub perspectival architecture:**

Hub 1 (sigmoid, power-scaling): `constraint_indexing.pl:265–340`. χ = ε × f(d) × σ(S) where f is a sigmoid over directionality d ∈ [0,1]. Canonical d values (`config.pl:139–144`): powerless=1.00, moderate=0.6459, powerful=0.4804, organized=0.3990, institutional=0.00, analytical=0.7250. Sigmoid params: L=−0.20, U=1.50, D0=0.50, K=6.00.

Hub 2 (effective immutability, discrete table): `constraint_indexing.pl:191–224`. Maps (time_horizon, exit_options) → {mountain, rope}. Examples: (immediate, trapped) → mountain; (biographical, identity_locked) → rope; (generational, constrained) → rope; (historical, _) → rope. Civilizational + analytical is non-deterministic (both mountain and rope valid).

**Key threshold values** (`config.pl`): rope_chi_ceiling=0.35 (line 228), rope_epsilon_ceiling=0.45 (line 229), snare_chi_floor=0.66 (line 232), scope_modifier_universal=1.0 (line 120), scope_modifier_national=1.0 (line 117), scope_modifier_global=1.2 (line 119).

**Observer index** (`constraint_indexing.pl:83–124`): Agent power — powerless, moderate, powerful, organized, institutional, analytical. Time horizon — immediate, biographical, generational, historical, civilizational. Exit options — trapped, identity_locked, constrained, mobile, arbitrage, analytical. Spatial scope — local, regional, national, continental, global, universal.

**delegation**: classify_from_metrics/6 is called internally by `is_mountain/3`, `is_rope/3`, etc. (drl_core.pl:122–157). External callers verified include: `boltzmann_compliance.pl:253`, `data_validation.pl:248`, `covering_analysis.pl:501`, `drl_composition.pl:182` (deprecated), `transition_paths.pl:137` (deprecated).

**Config params:** 178 total `param/2` facts (`config.pl`, verified by count). Drift-related params: boltzmann_floor_drift_threshold (line 406), network_drift_velocity_threshold (line 463), network_drift_hub_escalation (line 472), abductive_stress_drift_mode (line 519), trajectory_distance_pathology_weight (line 542). No params containing: revision, kernel, authority, commitment, acknowledgment, accretion, renewal, lifecycle, pattern.

**Drift detection** (`prolog/transition_paths.pl`):
- `transition_path/4` (lines 28–94): detects rope→tangled_rope, tangled_rope→snare, rope→piton, scaffold→piton, scaffold→snare, scaffold→tangled_rope, snare→piton, snare→false_mountain
- `degradation_chain/3` (lines 96–110): detects multi-step chains from measurement history; calls `classify_snapshot/3` (deprecated)
- `predicted_terminal_state/3` (lines 139–164): assigns confidence levels to predicted endpoints (piton/snare/tangled_rope/stable)

**Deprecated predicates:** Both `classify_snapshot/3` (`transition_paths.pl:112–137`) and `dr_type_at/4` (`drl_composition.pl:167–182`) carry explicit DEPRECATED markers (audit date 2026-03-12). Both use `constraint_indexing:power_modifier(Power, Modifier)` and direct multiplication `Chi is E * Modifier` instead of the sigmoid pipeline. Both nonetheless call `drl_core:classify_from_metrics/6` for the final gate logic.

#### Constraint story schema

*Source: `python/constraint_story_schema.json`, read directly.*

**Required top-level fields:** `header` (constraint_id, version, generated_date, status), `base_properties` (extractiveness, suppression, theater_ratio, claimed_type, human_readable, topic_domain), `perspectives` (array, minItems: 2), `interval` (start, end).

**Conditional fields:** `omegas` required when extractiveness > 0.46 or mountain with beneficiaries. `measurements` required when extractiveness > 0.46 (minItems: 6). `base_properties.mandatrophy_resolved` required when extractiveness > 0.70.

**Optional fields:** `commentary` (narrative_context, key_agents, logic_rationale, perspectival_gap, directionality_logic, mandatrophy_analysis), `boltzmann` (coordination_type), `network` (affects_constraints, dual_formulation_note), `directionality_overrides`, `uke_scope`.

**Type enum:** mountain | rope | tangled_rope | snare | scaffold | piton.

**Type constraint rules** (allOf section): mountain requires emerges_naturally=true, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, extractiveness ≤ 0.25, suppression ≤ 0.05. Tangled_rope requires requires_active_enforcement=true, beneficiaries, victims. Snare requires victims. Scaffold with requires_active_enforcement=true requires has_sunset_clause=true. Piton requires theater_ratio ≥ 0.70.

**Fields absent from schema:** kernel_type, authority_structure_type, acknowledgment_mechanism, cs_pattern, drift_source, lifecycle_phase.

#### Pipeline protocols

Six stages (from `agent/c-orchestrator.py` survey): (1) research, (2) UKE_SCOPE decompose → manifest, (3) generate → JSON to `json/`, Prolog to `prolog/testsets/`, (4) corpus update → 12 Prolog subsystems → `outputs/pipeline_output.json`, (5) reports → per-constraint markdown, (6) essay synthesis.

#### Corpus-level analysis

*Source: `outputs/corpus_profile.json`, read directly.*

`corpus_size: 3254`. Type distribution (claimed): tangled_rope 2221, snare 560, mountain 401, rope 46, piton 14, scaffold 11. Modal resolved: tangled_rope 2411, mountain 401, naturalized 253, snare 141, scaffold 29, piton 11, rope 7. Signal base rates: with_drift_events_pct=99.5%, critical_drift_pct=85.4%, h1_gt_0_pct=25.2%, critical_extraction_accumulation_pct=78.3%. Verdict distribution: yellow 2801, green 444, red 9.

No corpus-level CS pattern distribution exists. No corpus-level kernel/authority/acknowledgment statistics exist.

#### Other docs bearing on this

*Cited by filename and section only.*

`docs/asymmetry_of_failure_types.md`: Trifurcation — Type A (drift paradoxes, unmarked state mutation), Type B (axiomatic inconsistency), Type C (indexical underspecification). Drift is the Type A failure mode.

`docs/observers_not_humans_v2.md`: DR is "structural — applies to any system whose internal states update differentially under position-relative net flows." Mountain status is terminal (earned by stress-testing). Fragility split: parametric (Fisher curvature) vs. epistemic (cover-story mechanisms including FCR/FSM/FNL).

`docs/when_splitting_isnt_solving.md`: Sheaf/presheaf boundary as formal criterion. "Structured splits" (presheaf): local classifications cohere but don't globally glue — incompatibility is structural, not residual.

`docs/debugging_philosophy.md`: Type A (drift paradoxes), Type B (axiomatic inconsistency), Type C (indexical underspecification). Diagnostic order: C → A → B.

---

### §3 — Doc Verification Appendix

*This section was executed before §§4–5 were written. Its purpose is to calibrate source reliability before the parallel/gap analysis draws on those sources. Three claims are verified against code. Divergences are findings. If docs proved unreliable, §§4–5 rely on code alone.*

*Agent line-number claims verified during Phase 0:*
- classify_from_metrics/6 at lines 300–385: **confirmed** ✓
- Hub 2 at lines 191–224: **confirmed** ✓
- Observer index at lines 83–124: **confirmed** ✓
- transition_path/4 at lines 28–94: **confirmed** ✓
- 178 param/2 facts: **confirmed** ✓
- corpus_size 3,368 (agent claim): **diverges** — corpus_profile.json shows 3254. This is likely a data-currency issue (corpus is actively growing); not a fabrication.

---

**Claim A** — Paper v6.11 §2 (line 64): "The classification engine already accepts arbitrary (P, T, E, S) context tuples — `classify_from_metrics/6` is fully context-parameterized."

Code check: `classify_from_metrics/6` at `drl_core.pl:300` — Context is the 5th argument. Mountain gate (line 306) uses `effective_immutability_for_context(Context, mountain)`. Snare gate (line 331) uses `snare_immutability_check(Context)`. Rope gate (line 348) uses `effective_immutability_for_context(Context, rope)`. Scaffold, tangled_rope, piton, and naturalized gates do not use Context directly — they depend only on metric values and structural predicates.

**Verdict: Matches** — classify_from_metrics/6 accepts context and uses it in the immutability-sensitive gates. "Fully context-parameterized" is accurate in that all context-sensitive paths use the Context parameter. Note: context does not affect scaffold, tangled_rope, piton, or naturalized outcomes.

---

**Claim B** — Paper v6.11 §2, Axiom 2 (line 78): "At the canonical institutional d=0.00, f(d) ≈ −0.12 — making χ negative, pushing the institutional observer below the rope threshold."

Code check: `canonical_d_institutional = 0.00` (`config.pl:143`) ✓. Sigmoid params: L=−0.20, U=1.50, D0=0.50, K=6.00. Computation: f(0.00) = −0.20 + 1.70 / (1 + e^(−6×(0.00−0.50))) = −0.20 + 1.70/(1 + 20.086) = −0.20 + 0.081 ≈ −0.119.

**Verdict: Matches** — paper states f(d=0.00) ≈ −0.12; code computation yields −0.119. At any positive ε, χ = ε × (−0.119) < 0 < rope_chi_ceiling=0.35 (config.pl:228). Causal claim holds.

---

**Claim C** — CLAUDE.md (project instructions): "The universal exclusion is not cosmetic: σ(universal) = 1.0 at the analytical observer position drops χ below `rope_chi_ceiling`, causing systematic sheaf→presheaf crossings."

Code check:
- `scope_modifier_universal = 1.0` (`config.pl:120`) ✓
- `scope_modifier_national = 1.0` (`config.pl:117`) — identical value
- `rope_chi_ceiling = 0.35` (`config.pl:228`) ✓
- `canonical_d_analytical = 0.7250` (`config.pl:144`)
- f(0.7250) = −0.20 + 1.70/(1 + e^(−6×(0.725−0.50))) ≈ 1.150 (positive)
- At analytical with σ(universal)=1.0: χ = ε × 1.150 × 1.0 — same as σ(national)=1.0; no differential effect
- Actual code comment for product site exclusion (`constraint_indexing.pl:954–955`): "Excluded scope values: regional, continental, universal (non-canonical; scope_modifier params are less calibrated)"
- No mention of sheaf→presheaf crossings.

**Verdict: Diverges** — σ(universal) = 1.0 is confirmed, rope_chi_ceiling = 0.35 is confirmed. The causal claim is not supported: σ(universal) = σ(national) = 1.0, so including universal scope makes no χ difference relative to national. The code's own comment gives a different (calibration) reason for the exclusion. This is an incorrect claim in CLAUDE.md.

**Calibration conclusion:** Claims A and B (from the paper) match code. Claim C (from CLAUDE.md) diverges. §§4–5 below cite code directly, not CLAUDE.md.

---

### §4 — Vocabulary Parallels

*Uses §§1–2 and §3 verification. Each entry: CS sketch term (with line anchor) | DR analog (with code citation) | SO (same object) or SV (similar vocabulary).*

| CS Sketch concept (line) | DR apparatus analog | Code citation | Status |
|---|---|---|---|
| Observer parameters: power, exit options, time horizons, spatial scope (line 131) | Context tuple: agent_power, time_horizon, exit_options, spatial_scope | `constraint_indexing.pl:134–146` | SO — same four dimensions, same value sets |
| Drift as unmarked mutation of operational practice (line 15) | Drift events; transition_path/4; Type A failure mode | `transition_paths.pl:28–94` | SV — DR drift is metric drift in constraint values over time; CS drift is relative to a kernel; objects partially overlap but DR has no kernel as first-class object |
| Cover story prediction (line 135): given config + features + position, predict blindness W | false_natural_law, false_ci_rope, false_summit_mountain signature detectors | `signature_detection.pl` | SO — both predict which positions cannot see extraction; DR implements this as structural detectors |
| Trajectory channels (line 133) | degradation_chain/3, predicted_terminal_state/3 | `transition_paths.pl:96–164` | SV — DR detects multi-step degradation chains; CS trajectory channels include dormant-container activation (no DR analog) |
| Anchored fixity without interpretive-accretion layer (lines 39, 43) | snare + coordination_dead → piton degradation path | `drl_core.pl:314–321`; `transition_paths.pl:84–88` | SV — anchored fixity is a system pattern in CS; DR snare+piton are constraint-level types; structural shape is similar but unit of analysis differs |
| Authority grounding legitimacy in kernel's unchangeability (line 39) | Mountain gate: emerges_naturally(C) + effective_immutability | `drl_core.pl:300–306` | SV — DR mountain is a constraint type, not a system pattern; both describe something perceived as structurally immutable |
| Beneficiary/victim asymmetry as source of drift denial (throughout) | constraint_beneficiary/2, constraint_victim/2; has_asymmetric_extraction/1 | `narrative_ontology.pl:65–66` | SV — DR captures who benefits/is victimized per constraint; CS extends to who controls interpretation of the kernel |
| Ritualized renewal; failure mode: performative vs. exercised (line 127) | has_sunset_clause/1; sunset_violation drift event; scaffold classification | `narrative_ontology.pl:38`; `transition_paths.pl:57–61` | SO — scaffold + has_sunset_clause captures time-limited constraints; sunset_violation detects the performative renewal failure |
| Velocity mismatch (line 127): operational drift and authority drift at different rates | theater_ratio (performative overhead) | schema line 187–189 | SV — theater_ratio measures performative overhead but not the rate difference between two drift processes |
| Presheaf machinery (line 61): positions as category, classification as presheaf, sheaf axiom violated | H¹ cohomological obstruction; sheaf_analysis.pl; site_contexts_product/1 | `constraint_indexing.pl:54` | SO — same formalism; sketch explicitly cross-references "companion work" which is the DR paper |
| Lifecycle phases (line 125): formation, stabilization, operation, atrophy, renewal-or-dissolution | predicted_terminal_state/3 (piton/snare/tangled_rope/stable); transition_path/4 | `transition_paths.pl:139–164` | SV — DR detects trajectory and terminal state but has no formal state machine for system-level lifecycle |
| Stability conditional on acknowledgment capacity matching environmental change rate (line 31) | No direct analog | — | No analog found |

---

### §5 — Concepts With No Apparent Home

*Each entry includes search evidence. "No analog" means no predicate, no schema field, no config param found.*

**1. Kernel as first-class object** (sketch line 11)
The kernel is the object a commitment system points to as reference. DR treats each constraint story as the atomic unit; no predicate exists for "what is the kernel of this constraint family." The `network.affects_constraints` field links constraints in a family but does not designate any as the kernel.
- Search: `grep -r "kernel" prolog/config.pl` → only code comments; no predicate.

**2. Authority structure as institution with incentives and stakes** (sketch line 13)
`constraint_beneficiary/2` captures who benefits from a constraint, not who has authority to interpret it or what that authority stands to lose from acknowledging drift. The distinction between "beneficiary of a constraint" and "holder of interpretive authority over the kernel" has no predicate.

**3. Interpretive accretion as mechanism** (sketch line 33)
DR detects whether constraints have high theater_ratio (performative overhead) but does not compute whether drift is being absorbed via interpretation, denied directly, or fragmented into incompatible readings. The five CS patterns are distinguished by acknowledgment mechanism; DR types are distinguished by extraction and enforcement levels. These are orthogonal classification axes.

**4. Acknowledgment capacity vs. environmental drift rate** (sketch lines 31, 109)
The sketch treats the ratio of acknowledgment capacity to environmental drift rate as a key stability condition. DR computes no such ratio. 99.5% of constraints show drift events (corpus_profile.json), which indicates widespread drift, but no metric compares drift rate to the system's bandwidth for acknowledging it.

**5. Pattern classification — the five attractor patterns** (sketch line 29)
The five patterns classify commitment systems by their drift-acknowledgment mechanism. DR classifies individual constraints by extraction and enforcement properties. A single DR constraint type (e.g., snare) can correspond to different CS patterns depending on whether the authority structure acknowledges the extraction or denies it. The classifications are orthogonal: a snare can be anchored fixity or interpretive accretion depending on authority structure behavior.

**6. Drift source decomposition** (sketch line 121)
Environmental, authority, or endogenous source of drift. DR detects drift events (metric changes, coordination loss, sunset violation) but does not classify their source. The engine cannot distinguish whether a theater_ratio increase is environmental (changed context) or endogenous (authority incentives producing reinterpretation).

**7. Decoupled formalization** (sketch line 127)
Formal kernel exists for legibility/ceremony while operational authority sits elsewhere. theater_ratio captures performative overhead but is a ratio within a single constraint — it does not detect that formal kernel and operational authority are structurally decoupled across an institution.

**8. Kernel encoding substrate** (sketch line 127)
Text vs. embodied/tacit knowledge. No schema field for how the kernel is encoded. The lycurgan_kernel_unrevisability.json story discusses oral vs. written law in commentary but there is no formal field.

**9. Dormant-container activation** (sketch line 133)
A previously suspended commitment system reactivated as alternative legitimacy. No predicate anywhere in engine or schema. Some JSON commentaries discuss this pattern but they are narrative, not formal.

**10. Nesting as default condition** (sketch line 129)
DR's `network.affects_constraints` captures constraint-to-constraint dependencies but not commitment-system-to-commitment-system nesting. The level of analysis differs: DR operates at constraint level; nesting is a system-level concept.

**11. Acknowledgment standing** (sketch line 123)
"Acknowledgment that changes the system requires standing within the system." No predicate for who holds standing to ratify acknowledgment. constraint_beneficiary/2 identifies beneficiaries; the engine has no notion of interpretive authority.

**12. Pattern stability condition** (sketch line 31)
"The pattern is stable when acknowledgment capacity matches environmental change rate." No metric in the corpus for this condition.

---

## Turn 2 — Analysis and Integration Options

*Written from Turn 1 evidence only. §3 calibration: Claims A and B (paper) match code; Claim C (CLAUDE.md) diverges. §§4–5 are treated as reliable (they cite code directly).*

---

### Post-Turn-1 Space Check

The four pre-structured options in the plan remain. Turn 1 reveals two findings that affect the space:

**Finding: Observer parameters are not analogous — they are identical.** §4 marks observer parameters as SO (same object). The sketch at line 131 lists "power, exit options, time horizons, spatial-temporal scope" — the same four dimensions as the DR context tuple. The sketch cross-references "companion work" (line 61) using presheaf machinery, which is the DR paper. The sketch was written with the DR apparatus in mind.

**Finding: Level-of-analysis gap.** The five CS patterns classify commitment *systems* (institutions governed by a kernel and an authority structure). The DR apparatus classifies individual *constraints* by extraction and enforcement properties. This gap affects Options 2, 3, and 4, which would need to either resolve the gap or accept that CS pattern fields at constraint level conflate levels.

**Fifth option warranted:** Integration at the research/workflow level (orchestrator prompts, essay framing) rather than at the engine level. The CS sketch is explicitly positioned as enabling "the research program" (sketch line 5), not as providing engine components.

---

### Option 1: Vocabulary Mapping (zero code)

**What it does:** Produces a mapping document cross-referencing CS sketch terms with DR apparatus vocabulary. No code changes.

**What it leaves unchanged:** All engine code, schema, pipeline, corpus.

**What it adds:** A cross-reference document.

**CS concepts accommodated:** All SO-marked parallels from §4 (observer parameters, drift detection, cover story prediction, ritualized renewal, presheaf machinery).

**CS concepts not accommodated:** All §5 no-home items — kernel, authority with stakes, interpretive accretion as mechanism, acknowledgment capacity metric, pattern classification, drift source decomposition, dormant-container activation.

**Implementation cost:** Document writing. No code changes.

**Failure modes:** The vocabulary overlap is mostly already visible to anyone who has read both documents. A mapping doc addresses the easy part (observer parameters, drift, presheaf) and leaves the structural gap (kernel, authority, pattern classification) untouched.

**Turn 1 support:** §4 shows substantial vocabulary overlap at SO level. §5 shows substantial gaps at system level.

---

### Option 2: Post-Pipeline Python Pattern Classifier (new Python layer)

**What it does:** A Python script reads `outputs/pipeline_output.json` after the pipeline runs and attempts to assign a CS pattern to each constraint based on existing signal fields (claimed_type, extractiveness, theater_ratio, drift events, coordination_vitality, has_sunset_clause, signature, h1_gt_0).

**What it leaves unchanged:** Engine, schema, Prolog, pipeline stages 1–5.

**What it adds:** A new post-pipeline script; new output field or file.

**CS concepts accommodated:** Five patterns (inferred from constraint-level signals); cover-story detection (already implemented as false_natural_law, false_ci_rope, false_summit_mountain — a narrow version of CS cover-story prediction).

**CS concepts not accommodated:** Kernel as first-class object; authority structure incentives; acknowledgment capacity metric; drift source decomposition; velocity mismatch.

**Implementation cost:** ~150–250 Python lines. No Prolog or schema changes. No data migration.

**Failure modes:** Level-of-analysis problem. CS patterns characterize commitment *systems*; DR signals characterize *constraints*. Classifying each of 3254 individual constraints as "marked revision" or "interpretive accretion" conflates levels. A labor regulation's theater_ratio being high does not mean the system governing labor regulations is operating under interpretive accretion — those are different objects.

A narrow version is more structurally valid: classify constraint *families* (network.affects_constraints groups) rather than individual constraints. But most 3254 corpus constraints are standalone — most would be unclassifiable under the narrow version until more families are defined.

**Turn 1 support:** §4's SV-marked entries show partial signal mapping. §5 item 5 (pattern classification) and §5 item 10 (nesting/level of analysis) undercut the broad version.

---

### Option 3: Schema + Prolog Extension (medium integration)

**What it does:** Adds fields to `python/constraint_story_schema.json` for kernel_type, authority_structure_type, acknowledgment_mechanism, and cs_pattern. Adds Prolog predicates for CS pattern detection.

**What it leaves unchanged:** Core classification gates (classify_from_metrics/6, lines 300–385). Threshold architecture. config.pl params.

**What it adds:** New schema fields (LLM-generated at story time). New Prolog module for CS pattern detection. Generation prompts updated to elicit CS fields. Updated report template.

**CS concepts accommodated:** Five patterns as tagged fields; kernel_type as authored annotation; authority structure type.

**CS concepts not accommodated:** Acknowledgment capacity as a *computed* metric (would still be authored). Lifecycle phases as formal state machine. Nesting as first-class predicate.

**Implementation cost:** Schema change (~30–50 new lines). New Prolog module (~100–200 lines). Updated generation prompts. Updated report template.

**Data migration:** 3254 existing constraints have none of the new fields. Options: (a) accept corpus split — new constraints have CS fields, old do not; (b) LLM backfill (expensive, uncertain quality); (c) treat new fields as optional.

**Failure modes:** Schema extension without formal predicate semantics means CS pattern fields would be LLM assertions at generation time with no engine validation. The same problem the DR apparatus was built to solve (claimed type ≠ computed type) reappears for CS patterns — 86% YELLOW verdicts (corpus_profile.json) show the scale. §3 Claim C divergence illustrates that even existing documentation drifts from code; adding unvalidated fields increases this risk.

**Turn 1 support:** §5 items 1 and 3 show that kernel and interpretive accretion have no formal specifications sufficient for Prolog predicates. The sketch explicitly defers formalization to "operational work."

---

### Option 4: CS Framework as Primary Classification Axis (deep restructure)

**What it does:** CS pattern becomes the top-level classification output. DR computation (ε, χ, observer context) becomes one component feeding the CS pattern predictor rather than being terminal.

**What it leaves unchanged:** Core sigmoid computation. Observer index. config.pl threshold params.

**What it modifies:** classify_from_metrics/6 output vocabulary would change or be wrapped. DR types become subordinate to CS patterns.

**What it adds:** Kernel and authority structure as first-class Prolog predicates. CS pattern as primary output type.

**CS concepts accommodated:** All five patterns; all appendix material as first-class architecture candidates.

**Implementation cost:** High. Engine restructure. Schema overhaul. All 3254 existing constraints need CS-pattern annotations. All downstream reports and corpus analysis scripts need updating.

**Failure modes:** The CS sketch is "working notes, not theoretical commitments" (sketch line 119). The five patterns are "attractor states in configuration space" with no formal predicate definitions. Implementing them as Prolog predicates requires resolving ambiguities the sketch explicitly defers. Building an engine around underspecified foundations risks producing a system that classifies by vocabulary rather than by computation.

**Turn 1 support:** §5 items 1–3 show kernel, authority structure, and interpretive accretion have no formal specification for Prolog predicates. The sketch's own appendix treats them as provisional.

---

### Option 5: Workflow/Research Integration (surfaced by Turn 1)

**What it does:** Integrates the CS framework as a research lens at the workflow level — specifically in the UKE_SCOPE decompose prompt (Stage 2) and the essay synthesis prompt (Stage 6). Research questions would be reframed using CS concepts: what is the kernel of this institution? what is the authority structure and how does it benefit from kernel preservation? what drift-acknowledgment pattern does this exhibit?

**What it leaves unchanged:** Engine, schema, Prolog, pipeline stages 3–5, corpus analysis.

**What it adds:** Updated orchestrator prompts. Essay synthesis framing. Documentation of how CS concepts apply when interpreting corpus results.

**CS concepts accommodated:** All five patterns (as research questions and interpretive vocabulary). Kernel, authority structure, drift decomposition (as questions to investigate). Cover story prediction (engine already implements this; CS vocabulary names what the false_natural_law detector is doing). Lifecycle phases, nesting, coupling (as research framing).

**CS concepts not accommodated:** Formal predicate semantics for any CS concept. Corpus-level statistics on CS pattern distribution. Automated CS pattern assignment.

**Implementation cost:** Prompt engineering in `agent/c-orchestrator.py`. Documentation. No code changes.

**Failure modes:** CS pattern claims in essays are not auditable corpus data. If an essay says a system exhibits "anchored fixity," that claim cannot be verified by re-running the pipeline. The DR apparatus's value is precisely that its classifications are verifiable; Option 5 doesn't extend that verifiability to CS concepts.

**Turn 1 support:** The sketch explicitly positions itself as enabling "the research program" (sketch line 5), not as providing engine components. §5's finding that CS concepts lack formal predicate specifications supports treating them as research framing rather than engine architecture.

---

### Honest Recommendation

*Clearly separated from option descriptions.*

**Recommended path: Option 5 now; narrow Option 2 when the corpus has defined constraint families.**

**Rationale:**

The §3 Claim C finding — that CLAUDE.md contains an incorrect causal claim about σ(universal) causing presheaf crossings — illustrates a recurring risk in this project: claims about why the apparatus works can drift from what the apparatus actually does. The code comment at `constraint_indexing.pl:954–955` gives the actual reason for the exclusion (calibration, not sheaf topology), and CLAUDE.md has a plausible-sounding but wrong causal story. Before adding new layers, the marginal cost of fixing known documentation drift is low.

The CS sketch's most valuable contribution is not the five-pattern taxonomy (which requires system-level analysis the corpus doesn't currently support, and which the sketch itself defers to "operational work") — it is the **diagnostic questions** (sketch lines 65–75) and the **structural framing** of why systems fail (sketch line 49). These reframe the DR corpus's existing findings: 85.4% critical_drift_pct is not just an extraction-accumulation statistic — it is a signal that authority structures at this scale systematically cannot acknowledge drift. The commitment systems framework provides vocabulary for saying what that means.

Integrating these questions into the orchestrator's generation and essay prompts (Option 5) would make the CS framework operationally useful immediately, without requiring formal predicate semantics that the sketch explicitly defers.

The narrow Option 2 follow-on: once the corpus has a meaningful number of constraint families defined via `network.affects_constraints`, a post-pipeline classifier that assigns CS patterns to *families* (not individual constraints) becomes structurally valid. This is worth building then, not now.

**What not to do yet:** Options 3 and 4 require resolving the level-of-analysis gap before adding schema fields or restructuring the engine. Resolving it requires defining "commitment system" as a first-class corpus object, or specifying what kernel and authority structure mean as Prolog predicates. The sketch defers both. Building before that specification produces unverifiable fields — the opposite of the DR apparatus's design goal.

---

### Independent Findings

*Weaknesses in the existing apparatus worth addressing regardless of integration path.*

**Finding 1 — CLAUDE.md claim is incorrect:** The claim that σ(universal) = 1.0 at the analytical observer position drops χ below rope_chi_ceiling, causing systematic sheaf→presheaf crossings, is not supported by code. σ(universal) = σ(national) = 1.0; no differential effect exists. The actual exclusion reason in the code (`constraint_indexing.pl:954–955`) is calibration-based, not topology-based. Recommend correcting CLAUDE.md.

**Finding 2 — Two deprecated predicates remain live with incorrect χ computation:** `dr_type_at/4` (`drl_composition.pl:170–182`) and `classify_snapshot/3` (`transition_paths.pl:112–137`) carry DEPRECATED markers (audit date 2026-03-12) with TODO notes to migrate to the sigmoid pipeline. Both use `power_modifier/2` direct multiplication for χ instead of `derive_directionality/3 → sigmoid_f/2`. Both call `classify_from_metrics/6` for gate logic but supply incorrect χ inputs. `degradation_chain/3` at `transition_paths.pl:96–110` calls `classify_snapshot/3`, meaning chain detection uses the deprecated χ path. The migration noted in the TODO has not occurred in the ~2 months since the audit date.

**Finding 3 — corpus_profile.json corpus size and currency:** The exploration agents cited corpus sizes in the 3,368 range; corpus_profile.json shows 3,254. The manifest in pipeline_output.json should be checked for `pipeline_run_at` before citing corpus-level figures in any analysis. The corpus is actively growing; stale profiles produce incorrect base rates.

**Finding 4 — Product site exclusion documentation gap:** The product site excludes "regional, continental, universal" scopes with the reason "non-canonical; scope_modifier params are less calibrated" (`constraint_indexing.pl:954–955`). What "less calibrated" means is unspecified — what criterion would make them calibrated enough to include? The absence of this specification is what allowed CLAUDE.md to substitute a plausible-sounding but wrong causal story. Documenting the actual calibration criterion would prevent this class of documentation drift.

---

**Resolution note (2026-05-17):** Findings 1 and 2 were addressed; Findings 3 and 4 remain open.

- **Finding 1 resolved:** Corrected `CLAUDE.md` lines 99–103. Removed the false claim that σ(universal) = 1.0 causes sheaf→presheaf crossings. Replacement text states σ(universal) = σ(national) = 1.0 (no differential χ effect) and gives the actual calibration-based exclusion reason.

- **Finding 2 resolved:** Migrated both deprecated χ paths to the sigmoid pipeline. `classify_snapshot/3` in `transition_paths.pl` removed; replaced by `snapshot_type/3` using `derive_directionality → sigmoid_f → scope_modifier`. `dr_type_at/4` in `drl_composition.pl` removed; replaced by `classify_at_time/4` using the same formula. Callers (`degradation_chain/3`, `constraint_history/3`) updated. Regression test at `prolog/tests/test_snapshot_migration.pl` documents the expected ~20% χ divergence at global scope and confirms the snare-boundary class corrected by the migration.

---

**CS Pattern Detector implementation note (2026-05-17):**

Full Turn 3 implementation complete. The audit → design → implement cycle produced:

- `prolog/cs_pattern_detection.pl` — new module; exports `cs_pattern/3`, `cs_verdict/2`, `cs_has_fields/1`. Pattern discriminator matrix maps (kernel_codification × authority_grounding × interpretation_layer_present) to 6 pattern atoms. Verdict layer emits `false_*` atoms when LLM-asserted pattern contradicts computed structural signals. Bug discovered and fixed during testing: verdict clauses must call `cs_pattern/3` with an unbound Pattern variable (via `cs_pattern_is/2` helper), not with a pre-bound atom, because `cs_classify/5` uses pattern-matching dispatch and pre-bound atoms route to the wrong clause.

- `python/constraint_story_schema.json` and `agent/data/constraint_story_schema.json` — added optional `cs_structure` object with `kernel_codification` enum, `authority_grounding` enum, and optional `interpretation_layer_present` boolean. allOf rule validates that `interpretation_layer_present` requires `kernel_codification=formalized`. Legacy constraints validate without modification.

- `python/generate_constraint_pl.py` — extended `_build_multifile_declarations` and `generate_pl` to emit `narrative_ontology:cs_kernel_codification/2`, `narrative_ontology:cs_authority_grounding/2`, and `narrative_ontology:cs_interpretation_layer_present/1` facts when `cs_structure` block is present.

- `prompts/constraint_story_generation_prompt_json.md` — appended CS Structure section (~40 lines) documenting when to apply, the enum values with their meanings, and the interpretation_layer_present guard condition.

- `prolog/stack.pl` — added `use_module(cs_pattern_detection, [])` after `signature_detection`.

- `prolog/json_report.pl` — added `cs_pattern`, `cs_pattern_signals`, `cs_verdicts` fields to per-constraint output; added `cs_pattern_distribution` block to validation section; added `tally_cs_patterns/5` tally helper.

- `python/enhanced_report.py` — added `_CS_PATTERN_PROSE` and `_CS_VERDICT_PROSE` dicts, `build_cs_pattern_section` function, CS distribution line in `build_header`, and `l2_cs_pattern` inserted into L2 sections after theorem instantiation.

- `prolog/tests/test_cs_pattern_detection.pl` — 22 test cases covering all patterns, all 6 verdict conditions, clean-fire cases, and signal content. All 22 pass; full Prolog validation suite passes (0 errors, 0 warnings).

Coverage gaps (by design — not detectable without new authored fields, correctly produce `no_pattern_match`): none; all five patterns are now detectable via the authored `cs_structure` fields. The audit-phase finding that diffuse_reconstruction and implicit_practice were undetectable from existing metrics is resolved by having the LLM assert the classification rather than inferring it from signals.
