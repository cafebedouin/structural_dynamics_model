% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation of New Norms (Imperial Context)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the historical process by which new imperial
 *   norms achieved legitimacy not through pure top-down coercion or pure
 *   bottom-up adoption, but through a hybrid mechanism. This involved the
 *   symbolic transfer of authority from a charismatic imperial figure (e.g.,
 *   an emperor's example) combined with institutional incentives and
 *   pressures from the state apparatus. This reading emphasizes stratified
 *   adoption, with elites adopting first, followed by the masses, and
 *   moderate enforcement costs as the norms gradually became internalized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.55).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation of New Norms (Imperial Context)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'd7a476d7-706d-4254-b8d4-766ccc769d60').
narrative_ontology:cs_kernel_codification('d7a476d7-706d-4254-b8d4-766ccc769d60', formalized).
narrative_ontology:cs_authority_grounding('d7a476d7-706d-4254-b8d4-766ccc769d60', lineage).
narrative_ontology:cs_interpretation_layer_present('d7a476d7-706d-4254-b8d4-766ccc769d60').
narrative_ontology:cs_reading_relation('d7a476d7-706d-4254-b8d4-766ccc769d60', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7a476d7-706d-4254-b8d4-766ccc769d60', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('d7a476d7-706d-4254-b8d4-766ccc769d60', foundational, legitimacy_through_imperial_charisma).
narrative_ontology:cs_axiom_status(legitimacy_through_imperial_charisma, holdable).
narrative_ontology:cs_axiom_grounding('d7a476d7-706d-4254-b8d4-766ccc769d60', legitimacy_through_imperial_charisma, conventional).
narrative_ontology:cs_axiom('d7a476d7-706d-4254-b8d4-766ccc769d60', foundational, norm_adoption_via_incentives).
narrative_ontology:cs_axiom_status(norm_adoption_via_incentives, holdable).
narrative_ontology:cs_axiom_grounding('d7a476d7-706d-4254-b8d4-766ccc769d60', norm_adoption_via_incentives, empirically_contingent).
narrative_ontology:cs_reference_frame('d7a476d7-706d-4254-b8d4-766ccc769d60', unified_imperial_order).
narrative_ontology:cs_drift_state('d7a476d7-706d-4254-b8d4-766ccc769d60', post_imperial_collapse, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('d7a476d7-706d-4254-b8d4-766ccc769d60', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, state_apparatus).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, local_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, mass_population).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_authority_doctrine).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, social_cohesion_through_shared_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ruling class that initiates and benefits from the new norms, leveraging imperial charisma and institutional power to consolidate control and unify the realm. They accrue symbolic and material gains from the new order.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% The administrative and enforcement bodies of the empire responsible for implementing the new norms through a combination of institutional incentives (e.g., tax breaks, promotions) and moderate coercive measures. Their power and function are enhanced by the new unified order.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, state_apparatus, agenda_setter,
    institutional, biographical, constrained, national).

% Local power holders who are incentivized or pressured to adopt the new imperial norms first. They bear initial costs of conformity and adaptation but also gain status and access to imperial resources by aligning. They then serve as models for the mass population.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, local_elites, payer,
    powerful, biographical, constrained, regional).

% The general populace who gradually adopt the new norms, influenced by the example of local elites and the incentives/pressures from the state apparatus. They bear the social and cultural costs of abandoning traditional practices and conforming to the new order.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, mass_population, payer,
    powerless, immediate, constrained, local).

% Local religious, tribal, or customary leaders whose authority is gradually undermined by the imposition of new imperial norms. They are not typically met with overt violence but are marginalized by the new legitimation mechanisms and institutional structures.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, traditional_authorities, excluded,
    moderate, generational, constrained, local).

% Scholars who study the historical processes of norm imposition and legitimation, analyzing the interplay of symbolic authority, coercion, and social adoption in state formation. They seek to understand the structural mechanisms at play.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies diverse local populations under a common set of imperial norms, fostering social cohesion, administrative efficiency, and shared identity across a vast territory, thereby strengthening the imperial state.
% TRANSFER_FUNCTION: Transfers symbolic capital and social legitimacy from the imperial center (e.g., the emperor's charisma) to the new norms, and transfers compliance and loyalty from local populations to the imperial state, often through institutional incentives and stratified adoption.
% ABSENT_VOICES: Traditional local authorities and communities whose pre-existing norms and customs are superseded. They would advocate for the preservation of local autonomy and cultural diversity but are systematically marginalized by the imperial project's legitimation strategies.
% DISAPPEARANCE_RATIONALE: If these hybrid legitimation mechanisms had not been effective, the imperial social order would have remained fragmented, leading to persistent local resistance, administrative inefficiency, and ultimately, the potential collapse or failure of the imperial state to consolidate its power.
% FOUNDING_PROBLEM: Fragmented local customs, diverse loyalties, and a lack of unified social cohesion that hindered effective imperial administration and consolidation of state power across a heterogeneous population.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, archaeological findings, and independent analyses by historical sociologists and anthropologists consistently describe the challenges of imperial consolidation and the strategies employed to overcome local particularisms, corroborating the existence and resolution of this founding problem.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).
:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.45) is moderate because while there are costs imposed on local populations to conform, the process relies significantly on symbolic legitimation and incentives rather than brute force. `Suppression` (0.55) is also moderate, reflecting the active role of the state apparatus in promoting and enforcing the norms, but not through overwhelming violence. `Theater ratio` is low (0.15) because the legitimation process is genuinely functional in establishing a new social order, not merely performative. Over time, as norms become internalized, both extractiveness and suppression requirements decrease, indicating a successful, albeit initially costly, process of norm establishment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the imperial elite, this process is a necessary and legitimate act of state-building and social unification. From the perspective of local populations, particularly traditional authorities, it represents an imposition that erodes local customs and autonomy, even if not purely coercive. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial elite and state apparatus are clear beneficiaries, gaining consolidated power and legitimacy. Local elites and the mass population are payers, bearing the costs of cultural adaptation and conformity, though local elites may also gain secondary benefits from alignment with the imperial center. Traditional authorities are excluded, as their power is diminished without direct extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dominant_mechanism_ambiguity,
    'Was the legitimation truly hybrid, or was one mechanism (symbolic authority, institutional incentives, or residual coercion) ultimately dominant in practice?',
    'Detailed micro-historical studies analyzing local adoption patterns, resistance levels, and the specific application of incentives versus force over time and across different regions.',
    'If one mechanism is found to be overwhelmingly dominant, the constraint might reclassify towards a ''rope'' (if bottom-up/symbolic) or ''snare'' (if coercive) reading, rather than ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_mechanism_ambiguity, empirical, 'Ambiguity regarding the relative weight of different legitimation mechanisms.').

omega_variable(
    symbolic_authority_quantification,
    'How can the ''transfer of symbolic authority'' be empirically measured or quantified in historical contexts to assess its contribution to norm legitimation?',
    'Development of new methodologies in historical sociology for analyzing cultural artifacts, ritual practices, and discourse to trace the diffusion and acceptance of imperial symbols and narratives.',
    'A clearer understanding of symbolic authority''s impact would refine the ''extractiveness'' and ''suppression'' metrics, potentially lowering them if symbolic power is highly effective, or raising them if it''s found to be a weak cover for coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_authority_quantification, conceptual, 'Challenge in empirically measuring the effect of symbolic authority transfer.').

omega_variable(
    reading_distinction_clarity,
    'Are the ''hybrid_legitimation_reading'', ''endogenous_climb_reading'', and ''exogenous_override_reading'' sufficiently distinct to warrant separate constraint stories, or do they represent points on a continuum?',
    'Further theoretical work on the structural conditions under which each mechanism becomes primary, and empirical tests to identify clear historical cases that exemplify each reading without significant overlap.',
    'If the readings are found to be less distinct, the kernel might be simplified, or the relationships between readings (e.g., ''coexists_with'') might shift to ''influences'' or even ''forecloses'' if one reading is found to subsume another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinction_clarity, conceptual, 'Conceptual clarity of distinctions between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(impo_tr_t33, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 33, 0.15).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(impo_tr_t66, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 66, 0.15).
narrative_ontology:measurement(impo_tr_t83, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 83, 0.15).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(impo_be_t33, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 33, 0.55).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(impo_be_t66, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 66, 0.49).
narrative_ontology:measurement(impo_be_t83, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 83, 0.47).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(impo_su_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(impo_su_t33, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 33, 0.65).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(impo_su_t66, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 66, 0.59).
narrative_ontology:measurement(impo_su_t83, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 83, 0.57).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
