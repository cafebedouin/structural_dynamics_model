% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Legal Partition (Partition Reading)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'partition reading' of territorial
 *   legitimacy in the Israeli-Palestinian conflict, grounded in UN Resolution
 *   181 and the concept of internationally recognized borders (specifically
 *   the 1948 partition lines, often referenced with the 1967 lines as a basis
 *   for negotiation). It views both Israeli and Palestinian states as
 *   legitimate within these borders, and settlements beyond 1967 as
 *   illegitimate. The two-state solution is structurally possible under this
 *   reading. This is one reading of the 'territorial_legitimacy' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.65).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.7).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Legal Partition (Partition Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, 'a72af2c5-b2c8-4642-b385-ce2e5a13abfa').
narrative_ontology:cs_kernel_codification('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', formalized).
narrative_ontology:cs_authority_grounding('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', lineage).
narrative_ontology:cs_interpretation_layer_present('a72af2c5-b2c8-4642-b385-ce2e5a13abfa').
narrative_ontology:cs_reading_relation('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', foundational, international_law_as_basis_for_statehood).
narrative_ontology:cs_axiom_status(international_law_as_basis_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', international_law_as_basis_for_statehood, conventional).
narrative_ontology:cs_axiom('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', foundational, two_state_solution_as_just_outcome).
narrative_ontology:cs_axiom_status(two_state_solution_as_just_outcome, holdable).
narrative_ontology:cs_axiom_grounding('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', two_state_solution_as_just_outcome, deontological).
narrative_ontology:cs_reference_frame('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', un_resolution_181_framework).
narrative_ontology:cs_drift_state('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', contemporary_era_of_settlement_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a72af2c5-b2c8-4642-b385-ce2e5a13abfa', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, state_of_palestine).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, united_nations).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlers_beyond_67_lines).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives its international legal recognition and territorial claims from UN Resolution 181 and subsequent statehood. Benefits from the framework's emphasis on recognized borders, but faces pressure regarding settlements beyond 1967 lines.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, state_of_israel, beneficiary,
    institutional, generational, constrained, national).

% Seeks full statehood and territorial integrity based on the 1948 partition plan and 1967 borders. Benefits from the international legal framework's recognition of its right to self-determination and a sovereign state, but struggles with enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, state_of_palestine, beneficiary,
    institutional, generational, constrained, national).

% The primary institutional body that authored and continues to uphold the partition framework as the basis for a two-state solution. Its legitimacy is tied to the persistence of this framework, even if enforcement is challenging.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, united_nations, agenda_setter,
    institutional, civilizational, analytical, global).

% Displaced by the 1948 conflict, their right of return is often seen as conflicting with the partition framework's emphasis on recognized state borders. They bear the cost of statelessness and displacement within this legal interpretation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Their presence in territories beyond the 1967 lines is deemed illegitimate by this reading of international law. They face legal challenges and international condemnation, bearing the cost of non-recognition within this framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settlers_beyond_67_lines, payer,
    moderate, biographical, constrained, local).

% Analyze and interpret international law, including UN resolutions and their application to territorial disputes. They provide critical commentary on the consistency and enforceability of the partition framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an internationally recognized legal framework for the establishment of two sovereign states (Israel and Palestine) within defined borders, aiming to resolve territorial disputes and ensure regional stability.
% TRANSFER_FUNCTION: Transfers legal recognition and territorial claims to both Israeli and Palestinian states, while implicitly transferring the burden of displacement and non-recognition to those whose claims fall outside the defined borders (e.g., Palestinian refugees, Israeli settlers beyond 1967 lines).
% ABSENT_VOICES: Those who reject the two-state solution entirely, whether from a maximalist Israeli perspective (claiming all land for Israel) or a maximalist Palestinian perspective (claiming all land for Palestine), are marginalized by this framework. Also, indigenous voices whose claims predate the partition logic are often not fully integrated.
% DISAPPEARANCE_RATIONALE: If the international legal framework for partition and state recognition vanished, the entire basis for the two-state solution would collapse. Existing states' legitimacy would be challenged, territorial claims would revert to pure power dynamics, and the conflict would intensify without a recognized legal pathway for resolution.
% FOUNDING_PROBLEM: The problem of competing national aspirations and territorial claims in Mandate Palestine following the end of British rule, requiring an internationally sanctioned framework for self-determination and statehood.
% FOUNDING_PROBLEM_CORROBORATION: The United Nations and most international bodies, as well as many states globally, continue to affirm that the core problem of unresolved territorial claims and the need for a two-state solution remains live. This is corroborated by ongoing diplomatic efforts and the persistent conflict itself, which demonstrates the problem has not been fully resolved.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate the establishment of two states (beneficiaries: State of Israel, State of Palestine, UN) but simultaneously extracts from and suppresses alternative claims (victims: Palestinian refugees, Israeli settlers beyond 1967 lines). Extractiveness is high (0.65) because the framework, while offering a solution, has not fully resolved the underlying conflict and has led to ongoing displacement and non-recognition for significant populations. Suppression (0.7) is also high, as the framework actively delegitimizes and suppresses claims that fall outside its defined parameters. The theater ratio (0.2) reflects that while diplomatic efforts continue, the practical implementation and enforcement of the partition principles have often been performative or stalled, especially regarding settlement expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the UN and states adhering to this reading, it is a necessary framework for peace and stability. From the perspective of Palestinian refugees, it is a framework that codified their displacement. From the perspective of Israeli settlers beyond 1967, it is a framework that delegitimizes their presence. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and the State of Palestine are beneficiaries as their existence and claims are legitimized by this framework. The United Nations acts as the agenda-setter, promoting and enforcing this framework. Palestinian refugees and Israeli settlers beyond 1967 lines are victims, as their claims or presence are either not fully accommodated or actively delegitimized by this reading. International legal scholars act as observers, analyzing the framework's application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforceability_of_partition,
    'To what extent is the international legal partition framework genuinely enforceable, given persistent violations and lack of political will?',
    'Analysis of UN Security Council actions, international court rulings, and state compliance over time. If enforcement mechanisms consistently fail, the framework''s practical efficacy is low.',
    'If enforceability is low, the constraint''s effective suppression is lower than stated, and its theater_ratio is higher, pushing it closer to a Piton or even a Snare if the coordination function is entirely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_of_partition, empirical, 'The gap between the legal framework''s intent and its practical enforcement.').

omega_variable(
    two_state_solution_viability,
    'Is the two-state solution, as envisioned by the partition reading, still a viable and achievable outcome, or have facts on the ground (e.g., settlement expansion) rendered it impossible?',
    'Empirical assessment of demographic trends, territorial contiguity, and political will for a negotiated settlement. If territorial contiguity is irreversibly fragmented, the viability is low.',
    'If the two-state solution is no longer viable, the partition reading''s coordination function becomes purely theatrical, increasing its theater_ratio and potentially reclassifying it as a Piton or Snare, as its stated purpose can no longer be achieved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(two_state_solution_viability, empirical, 'The practical feasibility of the partition reading''s core outcome.').

omega_variable(
    partition_vs_indigenous_claims,
    'Does the partition reading adequately address or inherently suppress claims of indigenous continuity and the right of return for Palestinian refugees?',
    'Comparative legal analysis of international refugee law and indigenous rights frameworks against the partition framework. If the frameworks are in direct, irreconcilable conflict, suppression is higher.',
    'If the partition reading inherently suppresses indigenous claims, its effective suppression is higher for those populations, and its classification leans more towards a Snare for them, as their claims are structurally excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_indigenous_claims, conceptual, 'The conceptual tension between partition logic and indigenous/refugee rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__partition_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__partition_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.65).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__partition_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__partition_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.75).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__partition_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__partition_reading, suppression_requirement, 2015, 0.8).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel. This 'partition_reading' focuses on international legal frameworks and recognized borders. It coexists with and influences the 'security_necessity_reading' and 'indigenous_continuity_reading', which offer alternative bases for legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
