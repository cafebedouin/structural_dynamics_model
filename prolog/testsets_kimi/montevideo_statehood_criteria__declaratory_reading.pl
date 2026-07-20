% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Statehood Criteria â Declaratory Reading
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the declaratory reading of the Montevideo
 *   statehood criteria kernel: the legal doctrine that meeting four objective
 *   criteria (permanent population, defined territory, government, capacity
 *   to enter relations with other states) establishes statehood as an
 *   objective legal fact independent of recognition. It coordinates the
 *   international system by supplying a non-discretionary standard for state
 *   identity, but it asymmetrically extracts gatekeeping leverage from parent
 *   states and great powers, who lose the ability to condition legal
 *   personality on political alignment. De facto authorities are the
 *   doctrine's intended beneficiaries, yet recognition denial in practice
 *   places them in a victim position when the constraint is suppressed by
 *   non-compliant powerful states. The claim and metrics are independently
 *   authored: the constraint is claimed as tangled_rope because it mixes
 *   genuine coordination with the extraction of great-power discretion, while
 *   the metrics describe moderate-high extractiveness and resistance without
 *   tuning to match the claim.
 *
 * KEY AGENTS:
 *   - de_facto_authorities: Intended beneficiaries (powerless/trapped) â gain legal personality under the doctrine but suffer recognition denial in practice
 *   - parent_states: Primary payers (powerful/constrained) â lose leverage to block secessionist entities that meet objective criteria
 *   - great_power_gatekeepers: Secondary payers (powerful/constrained) â lose recognition policy as a discretionary foreign policy instrument
 *   - icj: Agenda-setter (institutional/analytical) â enforces declaratory interpretation through advisory opinions and contentious judgments
 *   - third_party_states: Beneficiaries (organized/constrained) â gain predictability in diplomatic relations and treaty membership
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.6).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.6).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Statehood Criteria â Declaratory Reading").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, 'b3467350-bcb8-47a2-a6f3-00a0bfc62bd1').
narrative_ontology:cs_kernel_codification('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', fixed_text).
narrative_ontology:cs_authority_grounding('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', lineage).
narrative_ontology:cs_interpretation_layer_present('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1').
narrative_ontology:cs_reading_relation('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', foundational, objective_criteria_suffice_for_statehood).
narrative_ontology:cs_axiom_status(objective_criteria_suffice_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', objective_criteria_suffice_for_statehood, conventional).
narrative_ontology:cs_axiom('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', foundational, recognition_lacks_constitutive_legal_force).
narrative_ontology:cs_axiom_status(recognition_lacks_constitutive_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', recognition_lacks_constitutive_legal_force, conventional).
narrative_ontology:cs_reference_frame('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', objective_statehood_by_criteria).
narrative_ontology:cs_drift_state('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', post_cold_war_contested_statehood, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b3467350-bcb8-47a2-a6f3-00a0bfc62bd1', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, third_party_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, great_power_gatekeepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities that control territory and population and meet the four Montevideo criteria but lack widespread diplomatic recognition. Under the declaratory reading they possess statehood as an objective legal fact, yet they are excluded from UN membership, treaty regimes, and bilateral relations when powerful states withhold recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities, beneficiary,
    powerless, generational, trapped, national).

% Recognized states facing secessionist entities that meet objective statehood criteria. Under the declaratory reading they lose the ability to condition recognition on territorial concessions, political subordination, or extended autonomy negotiations, forfeiting a traditional instrument of statecraft.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states, payer,
    powerful, generational, constrained, national).

% Major powers, including permanent members of the UN Security Council, that have historically used recognition policy to reward allies and isolate adversaries. The declaratory doctrine constrains this discretion by asserting that objective criteria alone establish statehood regardless of strategic preference.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, great_power_gatekeepers, payer,
    powerful, generational, constrained, global).

% International Court of Justice and affiliated judicial bodies that apply and enforce the declaratory reading through advisory opinions and contentious cases, treating statehood as a matter of objective fact and recognition as declaratory rather than constitutive.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, icj, agenda_setter,
    institutional, civilizational, analytical, global).

% States not directly involved in particular statehood disputes. They benefit from reduced legal ambiguity and clear standards for diplomatic relations, treaty participation, and international organization membership, though they remain bound by the same customary framework.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, third_party_states, beneficiary,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides objective, verifiable criteria for determining which entities possess international legal personality as states, thereby reducing ambiguity in diplomatic relations, treaty-making, and international organization membership.
% TRANSFER_FUNCTION: Transfers the authority to determine statehood from the political discretion of existing states, parent states, and great powers to an objective factual assessment of four criteria; transfers legal standing, diplomatic capacity, and treaty eligibility to criteria-meeting entities.
% ABSENT_VOICES: Populations of de facto authorities are excluded from the UNSC and bilateral recognition negotiations where their status is effectively decided. Constitutive theorists are present in academia but structurally marginalized in formal treaty bodies and courtrooms dominated by declaratory precedent. Parent-state domestic opposition movements are heard only through the parent state's own representation.
% DISAPPEARANCE_RATIONALE: If the declaratory reading vanished, dozens of entities would lose their strongest legal claim to statehood, parent states would regain leverage to condition recognition on political concessions, great powers would resume unconstrained gatekeeping over system membership, and the interstate order would shift from objective criteria to pure political recognition.
% FOUNDING_PROBLEM: The collapse of empires and the proliferation of new entities in the early twentieth century created uncertainty over which communities qualified as sovereign states; the inter-American system sought to prevent great-power recognition politics from destabilizing newly independent republics and to establish an objective, anti-imperial standard.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the 1933 Montevideo Conference and inter-American jurists attest the anti-imperial, anti-gatekeeping intent. However, post-Cold War hybrid theorists and human rights advocates contest whether objective criteria alone ever fully addressed the problem; corroboration from purely non-legal seats is limited because the problem was defined by the international legal profession itself.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60) is moderate-high because the doctrine strips powerful actors of discretionary gatekeeping authority and transfers legal standing to weaker entities. Suppression (0.60) reflects the active force required to maintain the declaratory rule against constitutive practice and great-power resistance. Theater_ratio (0.45) captures the growing performative gap between ICJ assertions of objective statehood and the continued practice of political recognition in the Security Council and bilateral relations. Accessibility_collapse (0.50) acknowledges that while the declaratory reading dominates legal textbooks and convention text, the constitutive alternative remains operationally accessible to powerful states. Resistance (0.65) is high because parent states and P5 members actively contest the automaticity of statehood in admission politics and bilateral recognition.
 *
 * PERSPECTIVAL GAP:
 *   The ICJ and third-party states compute the constraint as legal coordination that reduces ambiguity and stabilizes interstate relations. Parent states and great powers compute it as an illegitimate constraint on sovereign discretion and foreign policy flexibility. De facto authorities occupy a split seat: structurally beneficiaries of the legal rule, but practically victims of its non-enforcement when powerful states ignore or override it.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities and third_party_states are declared beneficiaries (low d, effective extraction damped or inverted into subsidy). Parent_states and great_power_gatekeepers are declared victims (high d, effective extraction amplified). The ICJ, as the enforcing institution with analytical exit options, sits near the beneficiary end because its authority and institutional function are subsidized by the constraint's stability. Great powers have constrained exit (cannot unilaterally exit customary international law) coupled with victim status, pushing d toward the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â stabilizing state identity after imperial collapse and decolonization â is partially solved. The doctrine persists beyond its founding moment because it coordinates ongoing secession and state formation, but it has accumulated extractive tension as great powers resist its application to contemporary contested cases. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) signals mandatrophy risk: the arrangement would fundamentally rearrange interstate relations if removed, yet its original problem is contested, suggesting the constraint now serves both coordination and the extraction of gatekeeping privilege simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_constitutive_logical_relation,
    'Does the declaratory reading logically foreclose the constitutive reading in international legal practice, or do they function as permanently coexisting interpretations of the same kernel?',
    'Examine state practice and ICJ jurisprudence for instances where both premises are simultaneously asserted by the same legal actor or institution.',
    'If foreclosed, the declaratory constraint functions as a stronger legal norm with higher suppression of constitutive alternatives; if coexisting, the measured extraction from gatekeepers is lower because constitutive practice remains structurally legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_constitutive_logical_relation, conceptual, 'Whether declaratory and constitutive readings are logical contradictories or coexisting doctrinal positions.').

omega_variable(
    human_rights_as_implicit_fifth_criterion,
    'Has state practice effectively introduced human rights and democratic governance as an unwritten fifth criterion, overriding the pure declaratory axiom that objective facts alone suffice?',
    'Comparative analysis of recognition practice since 1990 for entities meeting Montevideo criteria but lacking democratic governance or human rights compliance.',
    'If so, the drift_state is closer to axiom_overriding and the constraint is functionally nearer the hybrid reading than the pure declaratory reading suggests; if not, the declaratory reading retains its structural integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_as_implicit_fifth_criterion, empirical, 'Whether empirical recognition practice has added normative conditions beyond the four criteria.').

omega_variable(
    enforcement_gap_vs_extractive_design,
    'Is the gap between declaratory legal status and practical recognition an enforcement failure, or is the declaratory doctrine itself designed to legitimize a system that leaves de facto authorities in permanent limbo?',
    'Measure the rate of UN admission and treaty participation for entities meeting criteria but opposed by a P5 member versus those without P5 opposition.',
    'If the gap is systematic and P5-dependent, the constraint may be a tangled rope where legal form coordinates weak parties while great-power practice extracts from them; if random, it is a rope with weak enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_gap_vs_extractive_design, conceptual, 'Whether non-recognition is a bug or a structural feature of the declaratory regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(mont_tr_t60, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(mont_tr_t90, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 90, 0.45).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(mont_be_t60, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(mont_be_t90, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 90, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(mont_su_t60, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(mont_su_t90, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 90, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Montevideo statehood criteria kernel. The constitutive and hybrid readings are structurally distinct constraints with different epsilon values and should be authored as separate stories. Decomposition follows the epsilon-invariance principle: the declaratory reading's epsilon (moderate-high extraction from gatekeepers) differs from the constitutive reading's epsilon (high extraction from entities denied recognition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
