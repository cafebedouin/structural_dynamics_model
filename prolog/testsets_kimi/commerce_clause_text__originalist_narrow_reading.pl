% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Originalist Narrow Reading of the Commerce Clause
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   This constraint is the originalist narrow reading of the Commerce Clause
 *   kernel: the claim that federal power extends only to trade crossing state
 *   borders and the instrumentalities of interstate movement, leaving
 *   intrastate economic activity to state police power. It is one of three
 *   structurally distinct readings of the same constitutional text,
 *   differentiated by its core semantic axiom about original public meaning.
 *   The reading coordinates federalism by delineating a hard boundary between
 *   federal and state authority, while simultaneously extracting regulatory
 *   protection from federal legislators and intrastate workers who would
 *   benefit from national standards. The engine will compute divergent seat
 *   types from this single structural description.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/constrained) â retains police power and regulatory autonomy.
 *   - federalism_advocates: Secondary beneficiary (organized/mobile) â gains ideological vindication and influence.
 *   - originalist_judiciary: Agenda-setter (institutional/constrained) â enforces the narrow reading through judicial review.
 *   - federal_legislators: Primary payer/target (institutional/constrained) â loses jurisdiction over intrastate activity.
 *   - intrastate_workforce: Primary payer/target (powerless/trapped) â denied federal floor protections.
 *   - national_regulatory_advocates: Excluded voice (organized/constrained) â preferred national solutions rendered constitutionally unreachable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.68).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Originalist Narrow Reading of the Commerce Clause").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, 'c11c15e0-18c5-44ed-bbbb-61a05b432a05').
narrative_ontology:cs_kernel_codification('c11c15e0-18c5-44ed-bbbb-61a05b432a05', fixed_text).
narrative_ontology:cs_authority_grounding('c11c15e0-18c5-44ed-bbbb-61a05b432a05', lineage).
narrative_ontology:cs_interpretation_layer_present('c11c15e0-18c5-44ed-bbbb-61a05b432a05').
narrative_ontology:cs_reading_relation('c11c15e0-18c5-44ed-bbbb-61a05b432a05', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('c11c15e0-18c5-44ed-bbbb-61a05b432a05', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('c11c15e0-18c5-44ed-bbbb-61a05b432a05', foundational, original_public_meaning_of_commerce).
narrative_ontology:cs_axiom_status(original_public_meaning_of_commerce, holdable).
narrative_ontology:cs_axiom_grounding('c11c15e0-18c5-44ed-bbbb-61a05b432a05', original_public_meaning_of_commerce, empirically_contingent).
narrative_ontology:cs_axiom('c11c15e0-18c5-44ed-bbbb-61a05b432a05', foundational, state_police_power_reserved).
narrative_ontology:cs_axiom_status(state_police_power_reserved, holdable).
narrative_ontology:cs_axiom_grounding('c11c15e0-18c5-44ed-bbbb-61a05b432a05', state_police_power_reserved, conventional).
narrative_ontology:cs_reference_frame('c11c15e0-18c5-44ed-bbbb-61a05b432a05', founding_era_federalism_balance).
narrative_ontology:cs_drift_state('c11c15e0-18c5-44ed-bbbb-61a05b432a05', contemporary_national_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c11c15e0-18c5-44ed-bbbb-61a05b432a05', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, federalism_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, federal_legislators).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, intrastate_workforce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain police power and regulatory autonomy over intrastate economic activity; shielded from federal preemption in areas like labor, health, and safety where commerce is classified as purely local.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Legal scholars, politicians, and advocacy organizations who argue for constitutionally limited federal power; they gain ideological vindication and institutional influence when courts adopt the narrow reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Federal judges who interpret the Commerce Clause according to original public meaning; they enforce the narrow reading by invalidating federal statutes that regulate non-commercial intrastate activity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, originalist_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Members of Congress who seek to enact uniform national standards for labor, environment, or health; their legislative jurisdiction is truncated by judicial holdings that confine commerce to border-crossing trade.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_legislators, payer,
    institutional, biographical, constrained, national).

% Workers in industries deemed purely local; denied federal minimum wage, overtime, or safety protections when courts classify their activity as beyond the commerce power.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, intrastate_workforce, payer,
    powerless, biographical, trapped, national).

% Environmental and labor organizations seeking national regulatory floors; their preferred policy instruments are structurally excluded from the constitutional conversation under the narrow reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_regulatory_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, state_governments).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a constitutional balance between federal and state sovereignty by delineating a limited federal commerce power; prevents federal overreach into local police powers and maintains a structural boundary for state regulatory experimentation.
% TRANSFER_FUNCTION: Transfers regulatory authority over intrastate economic activity from federal legislators to state governments and to the judiciary that polices the boundary.
% ABSENT_VOICES: Federal agency staff, intrastate workers in non-unionized states, and environmental groups seeking national emission floors are excluded from the originalist interpretive framework; their policy preferences are rendered constitutionally unreachable.
% DISAPPEARANCE_RATIONALE: Federal statutes previously invalidated under Lopez, Morrison, or similar narrow-reading precedents would survive review; national labor, environmental, and civil rights floors would extend to previously local activity; states would lose their constitutional shield against federal preemption.
% FOUNDING_PROBLEM: The Founders needed to empower the federal government to prevent state trade barriers and regulate cross-border trade, while preserving state autonomy over purely local affairs.
% FOUNDING_PROBLEM_CORROBORATION: State governments and originalist legal scholars attest the problem is maintaining federalism boundaries against federal overreach. Progressive legal historians and federal administrative agencies attest the narrow reading is obsolete and the founding problem of fragmented interstate trade has been solved by national integration. Corroboration is split along ideological lines with no neutral party commanding consensus.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the asymmetric loss of federal regulatory protection for workers and the truncation of federal legislative capacity. Suppression (0.68) is high because the reading persists only through active judicial enforcement striking down statutes; it does not survive as a passive equilibrium. Theater ratio (0.42) captures the increasing performative dimension of originalist methodology â the reading must continuously re-enact historical frames to justify striking down modern legislation. Accessibility collapse (0.62) is moderate-to-high: once the narrow reading governs a policy domain, expansive federal alternatives are constitutionally barred for that domain, though doctrinal contestation keeps them conceptually alive. Resistance (0.72) is high because the reading blocks major federal legislative agendas and provokes sustained opposition from Congress, administrative agencies, and national advocacy coalitions.
 *
 * PERSPECTIVAL GAP:
 *   From the state government seat, the reading is protective coordination that preserves federalism boundaries; from the intrastate workforce seat, it is extraction that withholds federal protections; from the federal legislator seat, it is a snare-like truncation of democratic capacity. The originalist judiciary experiences it as methodological fidelity. The engine computes these divergences from the same structural data â the claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and federalism_advocates are structural beneficiaries (low d), collecting regulatory autonomy and ideological vindication. Federal legislators and intrastate_workforce are structural targets (high d), bearing the cost of truncated federal protection. The originalist_judiciary sits near symmetric: they enforce but do not personally extract; their institutional authority is bound to the reading's persistence but they do not accrue its material gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the reading as pure coordination (rope) by insisting on the presence of identifiable victims who bear asymmetric costs â the intrastate workforce and federal legislators. It also prevents mislabeling as snare by acknowledging the genuine coordination function: state governments do receive a real structural benefit in retained police power. The founding problem (preserving state autonomy against federal overreach) is contested as to whether it remains live in an integrated national economy; if dead, the reading risks mandatrophy, but the current judicial revival (Lopez, Morrison, ongoing litigation) suggests active enforcement rather than inertia, ruling out piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commerce_clause_kernel_decomposition,
    'Does the originalist narrow reading of the Commerce Clause constitute a structurally distinct constraint from the expansive federal reading, such that epsilon-invariance requires separate stories?',
    'Compare base extractiveness, beneficiary and victim sets, and foundational axioms across sibling readings; material divergence validates decomposition.',
    'If structurally distinct, the kernel decomposition holds and the constraint family is correctly modeled; if not, the epsilon-invariance principle is violated and the readings should be merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commerce_clause_kernel_decomposition, conceptual, 'Whether this reading is structurally distinct from its siblings to warrant separate constraint stories').

omega_variable(
    original_public_meaning_veracity,
    'Does the historical record of the founding era support the claim that ''commerce'' was limited to trade crossing borders and instrumentalities of movement?',
    'Linguistic corpus analysis of eighteenth-century usage, constitutional convention records, and ratification debates.',
    'If the empirical claim fails, the reading''s foundational axiom collapses and its authority erodes; if supported, the empirically contingent foundation is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_veracity, empirical, 'Historical accuracy of the narrow reading''s core semantic claim').

omega_variable(
    federalism_externality_tradeoff,
    'Does the state autonomy preserved by the narrow reading produce greater social welfare than the interstate externalities and regulatory gaps it creates?',
    'Cross-state comparative policy analysis measuring environmental, labor, and health outcomes under divergent state standards versus uniform federal floors.',
    'If externalities dominate, the reading functions as net extraction from interstate populations; if autonomy benefits dominate, the coordination function is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_externality_tradeoff, empirical, 'Welfare balance between federalism autonomy and interstate externality control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(comm_tr_t1900, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.4).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1789, 0.1).
narrative_ontology:measurement(comm_be_t1900, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.7).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1789, 0.05).
narrative_ontology:measurement(comm_su_t1900, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.8).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is the originalist narrow reading of the Commerce Clause kernel. It is structurally distinct from the expansive federal reading and the substantial effects limited reading because its base extractiveness derives from confining federal authority rather than expanding it. Each reading has different beneficiary and victim sets, different foundational axioms, and different empirical warrants. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
