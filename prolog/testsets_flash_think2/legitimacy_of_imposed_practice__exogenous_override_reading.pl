% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree as Exogenous Override of Prior Practice
 *   domain: Political History / State Formation / Cultural Imposition
 *
 * SUMMARY:
 *   This constraint describes the 'exogenous override' reading of the
 *   'legitimacy of imposed practice' kernel. From this perspective, state
 *   decree authority is inherently sufficient to displace prior practices,
 *   and compliance is expected to follow legal mandate regardless of the
 *   degree of internalization by the affected populations. This reading
 *   emphasizes top-down imposition and coercive enforcement as primary
 *   mechanisms for social change, often in the context of state-building or
 *   modernization projects in post-colonial or newly formed nations. The
 *   constraint is claimed as a 'snare' due to its high extractiveness and
 *   suppression, reflecting the coercive nature of the imposition from the
 *   perspective of those subjected to it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.8).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, snare).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree as Exogenous Override of Prior Practice").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "Political History / State Formation / Cultural Imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '65b3afa7-f180-42cb-bec8-aa3f7bb19d31').
narrative_ontology:cs_kernel_codification('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', formalized).
narrative_ontology:cs_authority_grounding('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', extraction).
narrative_ontology:cs_reading_relation('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, forecloses).
narrative_ontology:cs_axiom('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', state_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', foundational, legal_mandate_sufficient_for_compliance).
narrative_ontology:cs_axiom_status(legal_mandate_sufficient_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', legal_mandate_sufficient_for_compliance, instrumental).
narrative_ontology:cs_reference_frame('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', uncontested_state_supremacy).
narrative_ontology:cs_drift_state('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', post_resistance_movements, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65b3afa7-f180-42cb-bec8-aa3f7bb19d31', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that issues decrees, designs new legal frameworks, and enforces compliance. It benefits from the consolidation of power, the implementation of its modernization agenda, and the increased control over diverse populations.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the direct costs of forced compliance, including the abandonment of traditional practices, economic disruption, and social dislocation. They have limited means to resist or exit the state's jurisdiction, often facing coercive enforcement.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, immediate, trapped, local).

% Former local leaders or influential figures whose authority and status are directly undermined by the state's decrees. While they may possess some resources or networks to adapt or subtly resist, their power is significantly diminished, and they bear the cost of losing their traditional roles.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites, payer,
    powerful, biographical, constrained, regional).

% The abstract goal or ideological framework that the state claims to be advancing through its decrees. It 'benefits' by seeing its principles enacted, even if the practical outcomes for populations are negative.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).

% Academics, NGOs, or other states that analyze the state's policies, their implementation, and their human rights implications. They can document the impact of the decrees but have no direct power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To rapidly unify diverse local practices under a single, centrally mandated legal framework, ensuring uniform compliance with state-defined norms for national integration and development.
% TRANSFER_FUNCTION: Transfers authority, legitimacy, and control over daily life from local, traditional structures to the central state, extracting compliance, resources, and cultural conformity from populations.
% ABSENT_VOICES: Local community leaders, traditional authorities, and cultural practitioners whose practices are being overridden are structurally excluded from the decision-making process. Their perspectives are not sought, and their resistance is met with suppression.
% DISAPPEARANCE_RATIONALE: If the state's decree authority and enforcement vanished overnight, the imposed practices would likely collapse, and prior local practices, customs, and traditional authority structures would re-emerge, leading to a fragmentation of the state's imposed order and a reassertion of local autonomy.
% FOUNDING_PROBLEM: The perceived fragmentation and inefficiency of diverse local practices hindering state-building, national unity, and modernization efforts, often framed as a need to 'civilize' or 'develop' traditional societies.
% FOUNDING_PROBLEM_CORROBORATION: The state bureaucracy and its ideologues consistently attest that the problem of non-uniformity and traditional 'backwardness' is still live, justifying ongoing central control. However, independent historians, anthropologists, and human rights organizations often document the resilience of local practices and the coercive nature of the imposition, suggesting the 'problem' is often a pretext for power consolidation and resource extraction, with the original problem largely superseded by new forms of state control.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because the state actively extracts compliance and resources from populations, often without their consent or benefit, to achieve its own agenda. Suppression is very high (0.9) as the core premise is that compliance is mandated regardless of internalization, necessitating strong, often coercive, enforcement to overcome resistance and suppress alternatives. The theater ratio is low (0.1) because the enforcement is direct and functional, aimed at achieving actual displacement and compliance, rather than merely performative maintenance. Accessibility collapse is high (0.85) due to legal abolition of prior practices, while resistance is also high (0.7) reflecting the ongoing pushback from affected populations.
 *
 * PERSPECTIVAL GAP:
 *   The state bureaucracy, as the agenda-setter, perceives this as a legitimate and necessary mechanism for national development and unity. In contrast, rural populations and traditional elites experience it as a highly extractive and suppressive snare, forcing them to abandon deeply ingrained practices and bear significant adjustment costs. The engine's classification will highlight this divergence from the claimed 'snare' type for the victims versus a potentially 'rope' or 'scaffold' type from the state's self-perception.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy and its modernization agenda are the primary beneficiaries, gaining increased control, legitimacy, and resources. Rural populations and traditional elites are the clear victims, bearing the costs of forced compliance and loss of autonomy. International observers hold an analytical seat, documenting the impacts without direct participation. The high suppression and limited exit options for victims drive their directionality towards the full target end.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_pure_decree,
    'Is state decree authority truly sufficient to displace prior practice and ensure compliance without internalization, or does it merely create superficial conformity?',
    'Longitudinal ethnographic studies tracking the persistence of prior practices in private or informal spheres despite legal prohibition, or comparative studies of states employing different strategies for practice displacement.',
    'If pure decree proves insufficient for genuine displacement, the effective suppression and extractiveness of this constraint would be lower than measured, as the state''s efforts would be less effective, and the ''snare'' classification might shift towards a ''piton'' (theatrical enforcement) or ''tangled_rope'' (partial, contested compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_pure_decree, empirical, 'Whether legal mandate alone achieves genuine practice displacement or only superficial compliance.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s claim to absolute authority for practice displacement grounded in a genuine social contract or in its capacity for coercive extraction?',
    'Historical analysis of the state''s formation and its relationship with diverse populations, examining evidence of consent, consultation, or genuine representation versus pure imposition.',
    'If legitimacy is primarily derived from coercion, the ''snare'' classification is reinforced. If a genuine (though perhaps contested) social contract is identified, the constraint might be re-evaluated as a ''tangled_rope'' or even a ''rope'' from a different perspective, acknowledging a coordination function alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'The true source of the state''s authority to impose new practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 1900, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(legi_tr_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement(legi_be_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1910, 0.73).
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1920, 0.76).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1930, 0.78).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1940, 0.79).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1950, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(legi_su_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1910, 0.83).
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1920, 0.86).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1930, 0.88).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1940, 0.89).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1950, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, national_identity_formation).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, centralized_taxation_system).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, state_education_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
