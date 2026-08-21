% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Federal Authority Over Secession (Grievance Threshold Reading)
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'grievance_threshold_reading' of
 *   the 'secession_legitimacy_boundary' kernel. It describes the federal
 *   authority over secession as a Snare when federal actions have crossed a
 *   threshold of structural injustice, rendering the union illegitimate from
 *   the perspective of the aggrieved region. The constraint's persistence
 *   relies on active federal enforcement and suppression of alternatives,
 *   rather than on the consent of the governed in the aggrieved territory.
 *   The 'regardless of constitutional text' clause is central to this
 *   reading, prioritizing a moral claim to justice over a purely legalistic
 *   interpretation of union.
 *
 * KEY AGENTS:
 *   - federal_government: Agenda setter (institutional/arbitrage) — enforces unity, benefits from status quo.
 *   - aggrieved_secessionist_region: Payer/Victim (organized/identity_locked) — bears costs of injustice, seeks exit.
 *   - loyalist_states: Beneficiary (organized/constrained) — benefits from union, supports federal authority.
 *   - international_legal_bodies: Observer (analytical/analytical) — evaluates legitimacy claims.
 *   - constitutional_scholars: Observer (analytical/analytical) — interprets legal and moral arguments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.85).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.9).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, snare).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Federal Authority Over Secession (Grievance Threshold Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'b28a86c4-6344-4c72-a62e-a6e817a1d98c').
narrative_ontology:cs_kernel_codification('b28a86c4-6344-4c72-a62e-a6e817a1d98c', fixed_text).
narrative_ontology:cs_authority_grounding('b28a86c4-6344-4c72-a62e-a6e817a1d98c', lineage).
narrative_ontology:cs_interpretation_layer_present('b28a86c4-6344-4c72-a62e-a6e817a1d98c').
narrative_ontology:cs_reading_relation('b28a86c4-6344-4c72-a62e-a6e817a1d98c', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('b28a86c4-6344-4c72-a62e-a6e817a1d98c', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('b28a86c4-6344-4c72-a62e-a6e817a1d98c', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('b28a86c4-6344-4c72-a62e-a6e817a1d98c', foundational, legitimacy_derives_from_justice).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_justice, holdable).
narrative_ontology:cs_axiom_grounding('b28a86c4-6344-4c72-a62e-a6e817a1d98c', legitimacy_derives_from_justice, deontological).
narrative_ontology:cs_axiom('b28a86c4-6344-4c72-a62e-a6e817a1d98c', foundational, federal_authority_is_conditional).
narrative_ontology:cs_axiom_status(federal_authority_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('b28a86c4-6344-4c72-a62e-a6e817a1d98c', federal_authority_is_conditional, conventional).
narrative_ontology:cs_reference_frame('b28a86c4-6344-4c72-a62e-a6e817a1d98c', just_federal_compact).
narrative_ontology:cs_drift_state('b28a86c4-6344-4c72-a62e-a6e817a1d98c', era_of_structural_injustice, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b28a86c4-6344-4c72-a62e-a6e817a1d98c', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, loyalist_states).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_secessionist_region).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts perpetual union and constitutional supremacy, actively enforcing its authority over all constituent regions. Benefits from the continued extraction of resources and political power from all regions, including those claiming structural injustice. Views any secession attempt as illegal rebellion.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Bears the costs of federal policies perceived as structurally unjust, leading to economic, social, or cultural marginalization. Its identity is increasingly defined by its grievance against the federal center, making exit (secession) a core political objective despite immense costs and risks.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_secessionist_region, payer,
    organized, generational, identity_locked, regional).

% Benefit from the stability and shared resources of the federal union, often at the expense of aggrieved regions. They support federal enforcement against secession, viewing it as a threat to their own prosperity and the national order. Their options are constrained by their integration into the federal system.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, loyalist_states, beneficiary,
    organized, generational, constrained, national).

% Monitor and evaluate claims of self-determination and structural injustice against existing states, often referencing international law and human rights principles. Their pronouncements can lend legitimacy to secessionist movements or reinforce state sovereignty, but they lack direct enforcement power.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_legal_bodies, observer,
    analytical, generational, analytical, global).

% Analyze the legal and historical arguments for and against secession, often debating the original intent of founding documents versus evolving principles of justice and self-determination. Their interpretations influence public discourse and judicial reasoning but do not directly determine policy.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified federal state, ensuring shared defense, a common market, and a single foreign policy across diverse regions.
% TRANSFER_FUNCTION: Transfers political authority, economic resources, and cultural influence from constituent regions to the federal center. When the grievance threshold is crossed, this transfer is perceived as an unjust extraction from the aggrieved region.
% ABSENT_VOICES: Historically marginalized groups within the aggrieved region who may have different priorities than the secessionist leadership, or other regions with similar grievances but less political power to articulate them.
% DISAPPEARANCE_RATIONALE: If the federal claim to perpetual union vanished overnight, the state would likely fragment, leading to new political entities, borders, and economic arrangements, especially in regions where grievances are high. The entire geopolitical landscape would be redrawn.
% FOUNDING_PROBLEM: Establishing a stable, unified federal republic from disparate, often competing, states, balancing central authority with regional autonomy.
% FOUNDING_PROBLEM_CORROBORATION: The federal government and loyalist states assert the problem of maintaining unity is live. The aggrieved region, supported by some international legal scholars and human rights advocates, argues that the founding problem has been superseded by the federal government's failure to uphold justice, rendering the original compact illegitimate for them.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.90) reflect the situation *after* the grievance threshold has been crossed. From the perspective of the aggrieved region, the federal system is no longer a legitimate coordination mechanism but a coercive structure extracting resources and sovereignty unjustly. Federal enforcement is required to maintain this arrangement against active resistance (0.90). The low theater ratio (0.10) indicates that the conflict is genuine and existential, not performative. Accessibility collapse is moderate (0.60) because while secession is extremely difficult and costly, it remains a conceptual and political option, albeit one with severe consequences.
 *
 * PERSPECTIVAL GAP:
 *   The federal government and loyalist states perceive the constraint as a legitimate exercise of authority, a 'Rope' or 'Mountain' of perpetual union. The aggrieved secessionist region, however, experiences it as a 'Snare' due to the perceived structural injustice and the federal government's active suppression of their right to self-determination. The engine's classification will highlight this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and loyalist states are beneficiaries, as they gain from the continued integrity and resource flow of the union. The aggrieved secessionist region is the primary target/victim, bearing the costs of perceived injustice and facing severe suppression for seeking exit. International legal bodies and constitutional scholars act as analytical observers, assessing the situation without direct benefit or cost from the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grievance_threshold_objectivity,
    'How can ''structural injustice'' and the ''threshold'' for legitimate secession be objectively defined and measured, independent of the aggrieved party''s subjective claims?',
    'Development of internationally recognized criteria for self-determination, or a binding arbitration process by a neutral third party to assess the claims of injustice.',
    'If objective criteria are established and met, the legitimacy of secession is strengthened, potentially shifting international support and increasing pressure on the federal government. If not, the claim remains contested and less likely to gain external validation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grievance_threshold_objectivity, conceptual, 'The objective definition of structural injustice and the threshold for legitimate secession.').

omega_variable(
    federal_action_causality,
    'To what extent are the perceived structural injustices a direct result of federal actions, versus pre-existing regional disparities or internal governance failures within the aggrieved region?',
    'Independent forensic economic and social analysis comparing the aggrieved region''s trajectory under federal policy to counterfactuals or similar regions under different governance structures.',
    'If federal actions are demonstrably the primary cause, the claim of injustice is strengthened. If internal factors are dominant, the legitimacy of secession based on federal overreach is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_action_causality, empirical, 'Causal attribution of structural injustice to federal actions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the federal government''s suppression of secession structural (legal barriers, military force) or internalized (loyalty narratives, economic dependency) by elements within the aggrieved region?',
    'Post-conflict analysis of political behavior and economic choices in the event of a temporary relaxation of federal control. If resistance persists, it suggests structural suppression; if it dissipates, internalized factors are more prominent.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. If purely structural, removing federal enforcement would lead to immediate political reorganization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in federal-secessionist conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'secession_legitimacy_boundary' kernel, focusing on the conditions under which federal authority over secession loses legitimacy due to structural injustice. It is distinct from readings emphasizing constitutional text, popular sovereignty, or treaty primacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
