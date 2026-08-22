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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation of New Norms (Imperial Example)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes a 'hybrid legitimation' mechanism for new norms
 *   within a historical imperial context. It posits that new norms gain
 *   acceptance not purely through grassroots adoption (endogenous climb) nor
 *   solely through state coercion (exogenous override), but through a
 *   combination of symbolic authority transfer (e.g., the emperor's example)
 *   and institutional incentives. This leads to stratified adoption, with
 *   elites leading the way, and moderate enforcement costs. The constraint is
 *   claimed as a Tangled Rope because it genuinely coordinates (unifying
 *   norms) but also extracts (cultural assimilation, administrative control)
 *   through active, though not overwhelming, enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.55).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation of New Norms (Imperial Example)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '3b1c510c-61ed-4202-abb3-3cc113fb494d').
narrative_ontology:cs_kernel_codification('3b1c510c-61ed-4202-abb3-3cc113fb494d', implicit).
narrative_ontology:cs_authority_grounding('3b1c510c-61ed-4202-abb3-3cc113fb494d', lineage).
narrative_ontology:cs_interpretation_layer_present('3b1c510c-61ed-4202-abb3-3cc113fb494d').
narrative_ontology:cs_reading_relation('3b1c510c-61ed-4202-abb3-3cc113fb494d', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b1c510c-61ed-4202-abb3-3cc113fb494d', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('3b1c510c-61ed-4202-abb3-3cc113fb494d', foundational, legitimacy_from_symbolic_transfer_and_incentives).
narrative_ontology:cs_axiom_status(legitimacy_from_symbolic_transfer_and_incentives, holdable).
narrative_ontology:cs_axiom_grounding('3b1c510c-61ed-4202-abb3-3cc113fb494d', legitimacy_from_symbolic_transfer_and_incentives, conventional).
narrative_ontology:cs_axiom('3b1c510c-61ed-4202-abb3-3cc113fb494d', secondary, stratified_adoption_is_normative).
narrative_ontology:cs_axiom_status(stratified_adoption_is_normative, holdable).
narrative_ontology:cs_axiom_grounding('3b1c510c-61ed-4202-abb3-3cc113fb494d', stratified_adoption_is_normative, instrumental).
narrative_ontology:cs_reference_frame('3b1c510c-61ed-4202-abb3-3cc113fb494d', imperial_cultural_unification).
narrative_ontology:cs_drift_state('3b1c510c-61ed-4202-abb3-3cc113fb494d', contemporary_historical_analysis, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3b1c510c-61ed-4202-abb3-3cc113fb494d', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, elite_adopters).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, local_tradition_holders).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, common_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and champions new norms, leveraging imperial charisma and symbolic power to set an example for the populace. Benefits from increased social cohesion and centralized authority.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Implements institutional incentives and moderate enforcement to encourage adoption of new norms. Benefits from a more standardized and governable populace, enhancing administrative efficiency.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, state_bureaucracy, agenda_setter,
    organized, biographical, constrained, regional).

% Adopts new norms early, often gaining social status, political favor, or economic advantages through alignment with imperial directives. Their adoption provides a model for the broader populace.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, elite_adopters, beneficiary,
    powerful, biographical, mobile, local).

% Experience pressure to abandon or modify long-standing local customs and traditions in favor of the new imperial norms. They bear the cultural cost of assimilation, with limited options for resistance due to institutional incentives.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, local_tradition_holders, payer,
    moderate, generational, identity_locked, local).

% Gradually adopts new norms, influenced by elite example and institutional incentives. They face social pressure and minor penalties for non-compliance, but direct coercion is less prevalent than for the 'exogenous override' reading.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, common_populace, payer,
    powerless, immediate, constrained, local).

% Examines historical records to reconstruct the mechanisms of norm imposition and legitimation, seeking to understand the interplay of symbolic authority, institutional power, and social adoption.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified set of social and cultural norms across a diverse empire, facilitating governance, communication, and a shared imperial identity.
% TRANSFER_FUNCTION: Transfers cultural capital and social legitimacy from traditional local practices to new imperial norms, with associated benefits (status, access) flowing to adopters and costs (cultural erosion, social friction) borne by resisters.
% ABSENT_VOICES: Scholars and practitioners of suppressed local traditions, whose perspectives on the value and resilience of pre-existing norms are marginalized or actively silenced by the imperial project.
% DISAPPEARANCE_RATIONALE: If the hybrid legitimation mechanism vanished, the imperial project would likely fragment, with local traditions reasserting themselves and the new norms losing their coherence and enforcement. The social and political landscape would revert to a more decentralized, culturally diverse state.
% FOUNDING_PROBLEM: The problem of governing a vast, culturally diverse empire with disparate local customs, leading to administrative inefficiencies and potential challenges to central authority.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts from imperial chroniclers and administrative records attest to the ongoing challenge of cultural integration. Modern historical analyses, while critical of the methods, corroborate the existence of this foundational problem for imperial states.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is moderate (0.45) as the new norms impose costs of cultural change but also offer benefits of integration and order. Suppression is also moderate (0.55), reflecting institutional incentives and social pressure rather than overt, widespread violence. The theater ratio is low (0.20) because the symbolic and institutional mechanisms are genuinely functional in achieving adoption, not merely performative. The initial rise in extractiveness and suppression reflects the active phase of norm imposition, with a slight decline as norms become more embedded.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Imperial Court, this is a successful Rope, coordinating a diverse populace. From the perspective of Local Tradition Holders, it is a Snare, eroding their cultural autonomy. The engine's computation of per-seat classification will reflect these divergences based on the declared power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   The Imperial Court and State Bureaucracy are clear beneficiaries and agenda-setters, gaining centralized authority and administrative efficiency. Elite Adopters also benefit from aligning with the new norms, gaining status and favor. Local Tradition Holders and the Common Populace are payers, bearing the costs of cultural change and adaptation. Their exit options are constrained by social pressure and institutional incentives, but not entirely trapped, distinguishing this from a pure Snare.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_material_leverage,
    'What was the precise balance between symbolic authority (imperial charisma, elite example) and material incentives/disincentives (institutional rewards, minor penalties) in driving norm adoption?',
    'Detailed historical case studies comparing regions with varying degrees of imperial presence and local elite integration, analyzing adoption rates and resistance levels.',
    'If symbolic authority was dominant, the constraint leans closer to a Rope (voluntary coordination); if material incentives were more coercive, it leans closer to a Snare (extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_leverage, empirical, 'Ambiguity in the relative weight of symbolic vs. material drivers of norm adoption.').

omega_variable(
    hybrid_vs_sibling_distinction,
    'Is this ''hybrid legitimation'' mechanism truly distinct from a sequential combination of ''endogenous climb'' and ''exogenous override'', or is it merely a descriptive label for a complex process that could be decomposed into those simpler mechanisms?',
    'Comparative analysis with historical instances clearly fitting the ''endogenous climb'' or ''exogenous override'' models, looking for unique structural features of the ''hybrid'' case that cannot be reduced to a sequence of the others.',
    'If reducible, this reading might be superseded by a more granular, time-indexed analysis using the sibling constraints. If irreducible, it stands as a distinct mechanism of norm imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_sibling_distinction, conceptual, 'Conceptual distinction between hybrid legitimation and a sequence of simpler imposition mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(impo_tr_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(impo_be_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 75, 0.48).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(impo_su_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.08).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imposition_mechanism_kernel', focusing on a hybrid legitimation process. It is linked to sibling readings that emphasize endogenous climb or exogenous override, as these represent alternative interpretations of how new norms achieve legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
