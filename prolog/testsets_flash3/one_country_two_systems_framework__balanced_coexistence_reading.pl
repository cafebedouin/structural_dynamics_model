% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems: Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint describes the 'balanced coexistence' reading of the 'One
 *   Country, Two Systems' framework for Hong Kong. In this reading, neither
 *   PRC sovereignty nor Hong Kong's autonomy is absolute; instead, the
 *   framework mandates ongoing substantive negotiation and political
 *   accommodation to resolve contested boundaries. It functions as a tangled
 *   rope, providing a coordination mechanism for two distinct systems while
 *   involving asymmetric extraction and requiring active enforcement to
 *   manage the inherent tensions and prevent either side from asserting
 *   absolute dominance. The metrics reflect a medium-epsilon regime with
 *   periodic crises triggering renegotiation, where both legal systems
 *   acknowledge limits and civil society retains some bargaining power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.45).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.55).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems: Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '4e6417c1-7740-4b3e-b204-fd59d39cec1b').
narrative_ontology:cs_kernel_codification('4e6417c1-7740-4b3e-b204-fd59d39cec1b', formalized).
narrative_ontology:cs_authority_grounding('4e6417c1-7740-4b3e-b204-fd59d39cec1b', lineage).
narrative_ontology:cs_interpretation_layer_present('4e6417c1-7740-4b3e-b204-fd59d39cec1b').
narrative_ontology:cs_reading_relation('4e6417c1-7740-4b3e-b204-fd59d39cec1b', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e6417c1-7740-4b3e-b204-fd59d39cec1b', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('4e6417c1-7740-4b3e-b204-fd59d39cec1b', foundational, sovereignty_and_autonomy_are_negotiable).
narrative_ontology:cs_axiom_status(sovereignty_and_autonomy_are_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('4e6417c1-7740-4b3e-b204-fd59d39cec1b', sovereignty_and_autonomy_are_negotiable, conventional).
narrative_ontology:cs_axiom('4e6417c1-7740-4b3e-b204-fd59d39cec1b', foundational, political_accommodation_is_primary_resolution_mechanism).
narrative_ontology:cs_axiom_status(political_accommodation_is_primary_resolution_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('4e6417c1-7740-4b3e-b204-fd59d39cec1b', political_accommodation_is_primary_resolution_mechanism, conventional).
narrative_ontology:cs_reference_frame('4e6417c1-7740-4b3e-b204-fd59d39cec1b', basic_law_negotiated_settlement).
narrative_ontology:cs_drift_state('4e6417c1-7740-4b3e-b204-fd59d39cec1b', contemporary_geopolitical_tensions, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4e6417c1-7740-4b3e-b204-fd59d39cec1b', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, international_observers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereignty over Hong Kong, but acknowledges the need for accommodation to maintain stability and economic prosperity. Engages in political negotiation to resolve boundary disputes, leveraging its ultimate authority while seeking to avoid direct confrontation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Operates within the framework, balancing local autonomy with central government directives. Acts as an intermediary, translating central government expectations to local populace and advocating for Hong Kong's interests within the 'One Country' principle. Its legitimacy depends on maintaining this balance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government, agenda_setter,
    organized, biographical, constrained, regional).

% Experiences the framework as a constant negotiation over rights and freedoms. Benefits from retained autonomy but pays the cost of political accommodation, often through concessions on democratic development or civil liberties. Retains some bargaining power through public protest and international appeal.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, local).

% Monitors the implementation of 'One Country, Two Systems' against international agreements and human rights standards. Provides diplomatic pressure and commentary, influencing the political accommodation process without direct enforcement power.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the governance of Hong Kong by allowing a high degree of autonomy under Chinese sovereignty, preventing direct administrative integration while maintaining a unified state. It provides a framework for managing the inherent tensions between two distinct political and legal systems.
% TRANSFER_FUNCTION: Transfers political influence and decision-making power between the PRC Central Government and the Hong Kong Special Administrative Region, with the balance shifting based on political accommodation. It also transfers economic benefits from Hong Kong's unique status to the mainland, and vice-versa.
% ABSENT_VOICES: Those advocating for absolute sovereignty or absolute autonomy are structurally marginalized, as this reading emphasizes negotiation and compromise. Their absence from direct decision-making ensures the 'balanced' nature of the accommodation.
% DISAPPEARANCE_RATIONALE: If the framework vanished, Hong Kong would either be fully integrated into the PRC (losing its distinct legal and economic systems) or declare full independence (triggering a major geopolitical crisis). The entire regional and international political economy would be fundamentally reshaped.
% FOUNDING_PROBLEM: The problem was how to reintegrate Hong Kong into China after British colonial rule without destroying its capitalist economy, common law system, and civil liberties, which were seen as essential for its prosperity and international standing.
% FOUNDING_PROBLEM_CORROBORATION: Both the PRC Central Government and the Hong Kong Government, as well as many international observers, acknowledge that the core tension between sovereignty and autonomy remains a live issue requiring ongoing management. The framework's continued existence is predicated on addressing this tension.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).
:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) as both sides make concessions, but Hong Kong civil society often bears the cost of political accommodation. Suppression is also moderate (0.55) as active enforcement is required to prevent either side from overstepping, but civil society retains some avenues for resistance. The theater ratio is low (0.25) because the negotiation and accommodation are substantive, not merely performative. The cyclical nature of the measurements reflects periods of increased tension and extraction followed by periods of accommodation and reduced pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the PRC Central Government, this framework is a successful coordination mechanism for managing a complex reintegration. From the perspective of Hong Kong civil society, it is a constant struggle to preserve autonomy against sovereign encroachment, where 'accommodation' often means concessions. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC Central Government and the Hong Kong Government are beneficiaries and agenda-setters, as they actively manage and benefit from the stability the framework provides. Hong Kong civil society and international observers are victims/payers, bearing the costs of concessions or experiencing the erosion of promised autonomy, even if they also benefit from the overall stability. The framework extracts from the autonomy of Hong Kong in favor of the 'One Country' principle, but not absolutely.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the framework as pure extraction (snare) by acknowledging the genuine coordination function of managing two distinct systems. It also prevents mislabeling it as a pure coordination (rope) by recognizing the active enforcement and asymmetric extraction involved in political accommodation. The 'live' status of the founding problem indicates that the mandate has not atrophied, but the 'contested' corroboration highlights ongoing disputes over its implementation and fairness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_point_drift,
    'Is the ''balance'' point between sovereignty and autonomy drifting systematically towards sovereignty, or does it genuinely oscillate around a stable equilibrium?',
    'Longitudinal analysis of legislative changes, judicial decisions, and central government interventions over several decades, quantifying the shift in power distribution.',
    'If a systematic drift towards sovereignty is confirmed, the constraint''s effective extractiveness for Hong Kong civil society would be higher, potentially reclassifying it closer to a Snare. If a stable oscillation is confirmed, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_point_drift, empirical, 'Whether the power balance is stable or shifting over time.').

omega_variable(
    international_leverage_efficacy,
    'How effective is international pressure and observation in influencing the political accommodation process and preserving Hong Kong''s autonomy?',
    'Case studies of specific crises and interventions, correlating international responses with subsequent policy changes or retractions by the central government.',
    'If international leverage is found to be largely ineffective, Hong Kong civil society''s exit options would be more constrained, increasing their effective extractiveness. If effective, it would reinforce their bargaining power within the framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_leverage_efficacy, empirical, 'The actual impact of external actors on the framework''s operation.').

omega_variable(
    reading_framing_legitimacy,
    'Is this ''balanced coexistence'' reading a genuinely held interpretive framework, or a rhetorical device to legitimize a de facto shift towards sovereignty primacy?',
    'Analysis of official statements, policy documents, and legal interpretations from both PRC and HKSAR officials, compared against actual outcomes and the ''sovereignty primacy'' reading''s predictions.',
    'If found to be primarily rhetorical, the constraint would be reclassified closer to the ''sovereignty_primacy_reading'' (higher extraction, higher suppression), indicating a false framing. If genuine, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Whether the ''balanced coexistence'' framing accurately reflects the operational reality or serves as a legitimizing narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(one__tr_t18, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(one__tr_t24, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(one__tr_t30, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(one__be_t18, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(one__be_t24, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(one__be_t30, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(one__su_t18, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(one__su_t24, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(one__su_t30, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'One Country, Two Systems' kernel. It describes the framework as a dynamic balance between sovereignty and autonomy, requiring ongoing political accommodation. Sibling readings include 'sovereignty_primacy_reading' (higher extraction, less autonomy) and 'autonomy_primacy_reading' (lower extraction, more autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
