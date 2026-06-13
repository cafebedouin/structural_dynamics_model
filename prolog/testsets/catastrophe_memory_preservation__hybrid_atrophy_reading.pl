% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Ritual (Atrophy Reading)
 *   domain: religious/social/cultural
 *
 * SUMMARY:
 *   This constraint is one reading of a contested kernel about catastrophe
 *   memory rituals. The hybrid_atrophy_reading characterizes rituals that
 *   ONCE preserved survival-competence (threat-recognition, protocol, group
 *   cohesion under duress) but have ATROPHIED into purely mourning and
 *   identity-marking practice in modernity, when the original threat context
 *   is gone and the knowledge is no longer operationally relevant. The
 *   constraint persists — time-costly, material-costly, socially enforced —
 *   because ritual authority is invested in its continuation and because
 *   survivors and descendants carry intergenerational obligation. But the
 *   functional payoff has evaporated, the theater-ratio has risen
 *   (performance increasingly decorates identity rather than transmitting
 *   survival knowledge), and extractiveness is declining as younger
 *   generations increasingly treat it as burdensome rather than protective.
 *   This reading COEXISTS WITH two sibling readings: the
 *   survival_competence_reading (which asserts the knowledge transmission is
 *   still active and protective), and the mourning_practice_reading (which
 *   asserts the ritual was always primarily mourning, never operational
 *   survival-transmission). Each reading is instantiated in a separate
 *   constraint story with its own ε, beneficiary/victim structure, and type.
 *   This story is NEITHER the mourning reading NOR the survival reading; it
 *   is the atrophy narrative — the arc of decay from one function to another.
 *
 * KEY AGENTS:
 *   - catastrophe_survivors_descendants: Organized beneficiary (identity authority, intergenerational obligation) — moderate power, generational time_horizon, identity_locked to survivor status
 *   - present_generation_practitioners: Dual payer/beneficiary (bear cost, gain identity) — moderate power, biographical time_horizon, identity_locked to participation
 *   - ritual_authority_keepers: Agenda-setter (administers, enforces, benefits from authority) — powerful, generational time_horizon, mobile exit (could change practice but don't)
 *   - younger_non_practitioners: Excluded (would advocate for simplification, structurally marginalized) — moderate power, biographical time_horizon, constrained exit
 *   - external_academic_observers: Analytical seat (measure atrophy, document function decay, measure theater-ratio drift) — analytical power, biographical time_horizon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Ritual (Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious/social/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'ee70584f-0d33-47bf-b976-fde1d51d1c7b').
narrative_ontology:cs_kernel_codification('ee70584f-0d33-47bf-b976-fde1d51d1c7b', implicit).
narrative_ontology:cs_authority_grounding('ee70584f-0d33-47bf-b976-fde1d51d1c7b', lineage).
narrative_ontology:cs_interpretation_layer_present('ee70584f-0d33-47bf-b976-fde1d51d1c7b').
narrative_ontology:cs_reading_relation('ee70584f-0d33-47bf-b976-fde1d51d1c7b', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('ee70584f-0d33-47bf-b976-fde1d51d1c7b', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('ee70584f-0d33-47bf-b976-fde1d51d1c7b', foundational, ritual_function_decay_hypothesis).
narrative_ontology:cs_axiom_status(ritual_function_decay_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('ee70584f-0d33-47bf-b976-fde1d51d1c7b', ritual_function_decay_hypothesis, empirically_contingent).
narrative_ontology:cs_axiom('ee70584f-0d33-47bf-b976-fde1d51d1c7b', foundational, intergenerational_obligation_persistence).
narrative_ontology:cs_axiom_status(intergenerational_obligation_persistence, holdable).
narrative_ontology:cs_axiom_grounding('ee70584f-0d33-47bf-b976-fde1d51d1c7b', intergenerational_obligation_persistence, deontological).
narrative_ontology:cs_reference_frame('ee70584f-0d33-47bf-b976-fde1d51d1c7b', operational_survival_transmission).
narrative_ontology:cs_drift_state('ee70584f-0d33-47bf-b976-fde1d51d1c7b', contemporary_threat_absence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee70584f-0d33-47bf-b976-fde1d51d1c7b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_holders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_authority_keepers).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits PITON SIGNATURE across all metrics: (1) Extractiveness is MODERATE, not high (0.38–0.48), and DECLINING OVER TIME — the cost burden was higher when the founding problem was still live and the knowledge transfer was operationally relevant; as modernity advances and the threat recedes, the extractive burden of the ritual becomes harder to justify and younger generations allocate cost elsewhere. (2) Theater-ratio is RISING SHARPLY (0.52 → 0.72 across the interval) — the classic piton signal of functional atrophy masked by performative maintenance; ritual elements that once encoded threat-recognition (specific reenactments of escape routes, naming of hiding places, protocol drills) now function purely as ceremonial markers with no adaptive content. (3) Suppression is MODERATE and STABLE — the constraint persists not through external coercion but through internalized obligation and social expectation; family and community pressure maintains participation without overt enforcement. (4) Accessibility collapse is LOW (0.48) — alternatives DO exist (secular commemoration, simplified rituals, pure identity markers without enactment), but they are framed as disloyalty to ancestors, so the constraint persists through moral machinery rather than structural barriers. The piton classification rests on: (a) an atrophied primary function (survival-knowledge transmission), (b) persistence through inertia and authority investment rather than because anyone is hurt enough to fix it or benefits enough to maintain it at current cost, and (c) a theater-ratio trajectory that shows performative overhead increasingly dominating functional content. No party is captured and extracting sustainably (if they were, it would be a snare or tangled_rope); the agenda-setter CAN change the practice (mobile exit option) but faces enough intergenerational pressure that the cost to change exceeds the benefit, so it remains — piton, not capture.
 *
 * PERSPECTIVAL GAP:
 *   Ritual authority keepers and survivors perceive the constraint as continuity, obligation, and protective identity; they experience participation as honoring ancestors and maintaining group cohesion. Present-generation practitioners perceive it as identity investment with declining adaptive payoff — they honor ancestors AND suffer the cost, producing a dual experience. Younger non-practitioners perceive it as obsolete burden — they experience the time/cost without the identity anchor and feel only the extraction. External observers perceive it as atrophy and functional decay — they see the ritual's original survival function DISSOLVING into pure performance and identity marking. The engine computes these divergences from the stakeholder power/exit data: survivors and authority are 'powerful' or 'organized' with generational time_horizons, so their seats compute toward coordination/identity-preservation readings; present practitioners are 'moderate' with 'identity_locked' exit, so their seats compute toward cost-bearing; younger non-practitioners are 'excluded' and 'constrained', so their seats register dissent. The perspectival gap is NOT a measurement error — it is the structural evidence of piton dynamics: the constraint persists despite cost because different seats experience it differently, and no seat has enough power to resolve the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality on this constraint is ANTI-INTUITIVE because the performance of identity is not the same as the experience of extraction. (1) Survivors/authority (in_group_identity_holders, ritual_authority_keepers): Beneficiary classification, but directionality is MODERATE, not full-beneficiary. They benefit from the constraint (identity authority, role security, intergenerational legitimacy), but they also carry the cost of maintaining it (time investment, defensive discourse against critics, risk of constraint decay). Derived d ≈ 0.25–0.35, reflecting low net extraction FROM them but moderate structural dependence ON them. (2) Present practitioners (payer role + identity_locked exit): Highest directionality TOWARD extraction, d ≈ 0.65–0.75. They bear the time/material cost, they are locked into participation by identity fusion (leaving the ritual means questioning survivor status, risking family relationships, questioning belonging), and they receive no adaptive payoff. They are the constraint's principal targets. (3) Younger non-practitioners (excluded): High directionality, d ≈ 0.70–0.80. They feel the constraint (social friction, exclusion from decision-making, intergenerational moral pressure) without participating, so they bear suppression without ostensible benefit. The directionality structure explains why the constraint can be PITON (low extractiveness, inertial persistence) while STILL being substantially costly for specific seats: the cost is distributed across present practitioners and younger non-practitioners, but no single party captures enough benefit to warrant active maintenance, so authority keepers let inertia and intergenerational obligation carry it. No override needed — the derivation from beneficiary/victim + exit_options + power yields the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (threat-knowledge transmission) is DEAD in modernity — the threats are gone, the knowledge is irrelevant, the adaptive payoff is nil. But the constraint persists because: (1) ritual authority is invested in its continuation (agenda-setter's power), (2) intergenerational obligation remains moralized (survivors and descendants carry the identity commitment), and (3) no party is hurt ENOUGH or benefits ENOUGH to drive change (piton signature: cost-to-fix exceeds benefit-to-any-seat). The classification resists misidentification as rope (genuine coordination mechanism for survivors — FALSE; the coordination function is purely identity/mourning, not adaptive survival) or tangled_rope (asymmetric extraction with active enforcement — FALSE; suppression is internalized obligation, not external coercion). The piton classification PROTECTS against misframing mandatrophy as a 'solved problem' — the constraint's persistence is not a feature of successful coordination, but a feature of inertia, authority investment, and the absence of sufficient power to force change. This reading explicitly rejects the survival_competence_reading's claim that the knowledge transmission is still operationally live; it asserts the founding problem is dead, not live, and the constraint's persistence is theatrical, not functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_timing_ambiguity,
    'When did the ritual''s primary function transition from operational survival-transmission to ceremonial identity-marking? Was it gradual (over generations as the threat context changed) or sharp (at a discrete historical moment)?',
    'Oral history interviews tracking how different generations report the ritual''s purpose and content; archival evidence of how instruction was transmitted; comparison of ritual forms across generations showing what adaptive content was lost.',
    'If atrophy was gradual and ongoing, theater-ratio should show steady increase and younger practitioners should show increasingly identity-locked attachment (which the measurements show). If sharp, there should be a historical inflection point after which ceremony dominated. Gradual atrophy supports the piton classification; sharp transition might indicate a deliberate shift from survival-practice to mourning-practice (different constraint type).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_timing_ambiguity, empirical, 'Timing of functional transition from survival-transmission to ceremonial identity').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the suppression measured in the constraint (0.42–0.48 steady-state) primarily internalized obligation (identity fusion, intergenerational guilt) or structural (social exclusion, material punishment, institutional barriers)?',
    'Post-exit trajectory analysis: if younger practitioners who stop participating report persistent guilt/identity confusion after exit, suppression is internalized; if exit is clean and friction diminishes after, suppression is structural. Interview data on coercion mechanisms — what specifically makes people feel obligated?',
    'If internalized, the constraint''s effective suppression is HIGHER than the metric suggests — the target carries the suppression with them after exit, limiting exit freedom. If structural, exit becomes viable once barriers are removed, and the constraint might shift from piton (inertial) to something more deliberately maintained. Affects the evaluation of exit_options validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Suppression mechanism: internalized obligation vs. structural barrier').

omega_variable(
    sibling_reading_empirical_gap,
    'How can this reading (atrophy from survival-transmission to mourning) be differentiated empirically from the mourning_practice_reading (ritual is fundamentally mourning, not operationally survival-transmissive)?',
    'Historical analysis: did the ritual''s content and transmission change over time (supporting atrophy), or has it been functionally constant (supporting pure mourning reading)? Archaeological evidence of threat-response encoded in ritual? Comparison with non-atrophied survival-transmission rituals in other communities.',
    'If atrophy evidence is strong, this reading''s ε is justified (~0.38, declining). If the ritual has always been mourning, the ε should be reread as the mourning_practice_reading''s constraint (likely lower extractiveness, theater-ratio closer to 1.0, different type). This is a constraint-family coherence question — the three siblings must have structurally distinct ε values to justify separate stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_gap, empirical, 'Differentiation of atrophy reading from pure mourning reading via ritual content and transmission history').

omega_variable(
    authority_keeper_exit_option_validity,
    'Are ritual authority keepers truly ''mobile'' in exit options, or are they identity-locked like practitioners? Can they actually exit/reform the practice without losing authority status?',
    'Case studies of communities where ritual has been reformed or simplified, tracking what happened to authority-keeper status and institutional role. Interview data on constraints felt by authority keepers.',
    'If authority keepers are functionally identity-locked (their power and status rest entirely on ritual administration), they have NO real exit, and the piton classification should shift toward the payer seats. If they truly are mobile (can change practice without losing power), then the persistent constraint reflects their choice, not inertia, which would argue for a different classification. Current authored exit=''mobile'' is an assumption that needs testing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_keeper_exit_option_validity, empirical, 'Whether ritual authority keepers have genuine exit options or are identity-locked like other practitioners').

omega_variable(
    survivor_identity_vs_adaptive_payoff_fusion,
    'Do survivors and descendants benefit from the ritual primarily because it maintains identity/social position (emotional/relational payoff), or because it genuinely transfers survival knowledge (adaptive payoff)? Can these be separated?',
    'Comparative analysis: communities where ritual has been replaced with identity-preserving secular ceremonies (narratives, commemorative holidays, symbols) but no enactment. Did survivor identity persist? Was social cohesion maintained? Did descendants report sense of knowledge transmission? Do communities with high-theater rituals (identity-dominant) show different survival outcomes than low-theater ones?',
    'If survival knowledge transfer is real and adaptive (not just symbolic), this reading''s classification as piton is wrong — it should be tangled_rope (coordination + extraction). If the payoff is purely identity/emotional, the piton classification stands. This omega affects whether the constraint is fundamentally about displaced adaptive function or about identity-marking that became displaced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survivor_identity_vs_adaptive_payoff_fusion, conceptual, 'Whether survivor benefit is from adaptive knowledge transfer or relational/identity value, and whether these are separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 5, 0.56).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 15, 0.63).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 25, 0.67).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 30, 0.68).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.72).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_memory_preservation kernel. The three readings differ in their claim about what the ritual accomplishes — survival-knowledge transmission vs. mourning-practice vs. atrophied former-survival now-mourning — and thus have different ε values, beneficiary/victim structures, and types. Each reading is a separate constraint story. This reading (hybrid_atrophy) depends on BOTH siblings: it asserts that the ritual WAS what the survival_competence_reading claims (operational transmission) but HAS BECOME what the mourning_practice_reading claims (ceremonial identity-marking). The sibling stories are linked by network.affects_constraints to form the constraint family. See commentary.kernel_context for the full decomposition and the empirical questions that would differentiate the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
