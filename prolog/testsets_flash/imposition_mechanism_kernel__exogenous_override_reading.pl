% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Imposed Norms (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the process by which new social, legal, or
 *   economic norms are imposed by a central state authority, with legitimacy
 *   primarily derived from its coercive capacity (monopoly on violence)
 *   rather than pre-existing cultural acceptance. This is the 'exogenous
 *   override' reading of the imposition mechanism kernel, where state power
 *   actively suppresses alternative norms and enforces compliance. The high
 *   extractiveness and suppression reflect the coercive nature of this
 *   process, characteristic of early modern state formation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.85).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Imposed Norms (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, 'eb077872-5ed4-4a36-a14d-bae4f93d7204').
narrative_ontology:cs_kernel_codification('eb077872-5ed4-4a36-a14d-bae4f93d7204', formalized).
narrative_ontology:cs_authority_grounding('eb077872-5ed4-4a36-a14d-bae4f93d7204', extraction).
narrative_ontology:cs_interpretation_layer_present('eb077872-5ed4-4a36-a14d-bae4f93d7204').
narrative_ontology:cs_reading_relation('eb077872-5ed4-4a36-a14d-bae4f93d7204', imposition_mechanism_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('eb077872-5ed4-4a36-a14d-bae4f93d7204', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('eb077872-5ed4-4a36-a14d-bae4f93d7204', foundational, state_monopoly_on_violence_is_primary_legitimator).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_primary_legitimator, holdable).
narrative_ontology:cs_axiom_grounding('eb077872-5ed4-4a36-a14d-bae4f93d7204', state_monopoly_on_violence_is_primary_legitimator, conventional).
narrative_ontology:cs_axiom('eb077872-5ed4-4a36-a14d-bae4f93d7204', secondary, cultural_acceptance_is_secondary_or_contingent).
narrative_ontology:cs_axiom_status(cultural_acceptance_is_secondary_or_contingent, holdable).
narrative_ontology:cs_axiom_grounding('eb077872-5ed4-4a36-a14d-bae4f93d7204', cultural_acceptance_is_secondary_or_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('eb077872-5ed4-4a36-a14d-bae4f93d7204', rational_state_coercion_model).
narrative_ontology:cs_drift_state('eb077872-5ed4-4a36-a14d-bae4f93d7204', contemporary_postcolonial_critique, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('eb077872-5ed4-4a36-a14d-bae4f93d7204', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, local_communities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that promulgates new norms (e.g., taxation, conscription, legal codes) and enforces them through its monopoly on violence. Benefits from the consolidation of power and resources these norms enable.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The political and economic beneficiaries of the new state-imposed norms, gaining wealth, status, and control through their alignment with the central authority. They actively support the enforcement of these norms.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite, beneficiary,
    powerful, generational, mobile, national).

% The primary targets of the new norms, forced to comply with unfamiliar laws, taxes, or social regulations that often contradict established local customs. They bear the direct costs of compliance and face severe penalties for non-compliance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, local_communities, payer,
    powerless, biographical, trapped, local).

% Local leaders, elders, or religious figures whose authority is undermined or directly challenged by the state's imposition of new norms. They are often caught between state demands and community resistance, with their traditional legitimacy eroding.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_authorities, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, traditional_authorities, excluded).

% The direct enforcers of the new norms, responsible for suppressing resistance and ensuring compliance. Their power and resources are directly tied to the state's ability to impose its will.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, military_and_police, agenda_setter,
    institutional, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal and administrative framework across a diverse territory, enabling centralized governance, resource mobilization (e.g., taxation, conscription), and the suppression of internal dissent.
% TRANSFER_FUNCTION: Transfers authority, resources, and social control from local, traditional structures to the central state apparatus and its aligned elites, by imposing new norms backed by coercive force.
% ABSENT_VOICES: Local populations and traditional authorities, whose customary laws and practices are overridden, are not consulted in the formulation of these norms. Their objections are met with suppression rather than negotiation.
% DISAPPEARANCE_RATIONALE: If the state's capacity to impose these norms vanished, local communities would likely revert to traditional practices, tax collection would collapse, and the central authority's control would fragment. The social and political order would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of consolidating power, unifying diverse territories, and extracting resources for state-building (e.g., funding armies, infrastructure) in the face of entrenched local autonomy and resistance.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and ruling elite attest that the problem of maintaining central authority and resource extraction is perpetually live, citing ongoing challenges to state power. Historical accounts and sociological analyses from independent scholars corroborate that these norms were indeed imposed to solve problems of state consolidation, though they often highlight the coercive nature of this solution.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the norms are designed to channel resources and power to the state and its elites, often at the expense of local populations. Suppression is very high (0.9) as the state actively represses resistance and alternative forms of authority, relying on military and police force. Theater ratio is low (0.2) because the state's claims of legitimacy (e.g., 'divine right,' 'civilizing mission') are secondary to the direct application of force; the performance of legitimacy is less critical than the raw capacity to enforce. Accessibility collapse is moderate (0.7) as alternatives are actively suppressed but local resistance and traditional practices may persist in attenuated forms. Resistance is high (0.75) due to the direct challenge to existing social orders.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state apparatus, these norms are necessary for order and progress, a legitimate exercise of sovereign power. From the perspective of local communities, they are an alien imposition, a form of violent extraction that disrupts traditional ways of life. The engine's classification will reflect this divergence, likely showing a Snare for the local communities and a more Rope-like (or even Mountain-like, if the state's self-justification is strong enough) classification for the state, despite the high overall extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and ruling elite are clear beneficiaries (d near 0.0), as the norms consolidate their power and wealth. Local communities and traditional authorities are the primary targets (d near 1.0), bearing the costs of compliance and losing autonomy. The military and police, while enforcers, also benefit from their enhanced role and resources within the state structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'To what extent did the state-imposed norms eventually gain genuine cultural acceptance, rather than merely coerced compliance?',
    'Longitudinal sociological studies tracking changes in public opinion, adherence to norms in the absence of direct enforcement, and the internalization of state ideology over generations.',
    'If significant cultural acceptance emerged, the constraint''s effective suppression would decrease over time, and its classification might drift towards a Tangled Rope or even Rope, as the coordination function becomes more salient than pure coercion. If compliance remained purely coerced, the Snare classification would persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Distinguishing between coerced compliance and internalized legitimacy for state-imposed norms.').

omega_variable(
    exogenous_vs_endogenous_framing,
    'Is the ''exogenous override'' reading the most accurate framing, or does it overstate the role of coercion by neglecting pre-existing local factors that facilitated state integration?',
    'Comparative historical analysis across different regions, examining the degree of local resistance, the speed of norm adoption, and the presence of local elites who actively collaborated with the state prior to direct coercion.',
    'If local factors played a more significant role, the ''endogenous climb'' or ''hybrid legitimation'' readings might be more appropriate, leading to lower extractiveness and suppression scores, and a classification closer to a Rope or Tangled Rope. This would shift the understanding of state formation from pure imposition to a more complex, negotiated process.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_framing, conceptual, 'The conceptual framing of norm imposition as purely exogenous versus incorporating endogenous factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1600, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(impo_tr_t1650, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1650, 0.12).
narrative_ontology:measurement(impo_tr_t1700, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement(impo_tr_t1750, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1750, 0.18).
narrative_ontology:measurement(impo_tr_t1800, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(impo_tr_t1850, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(impo_tr_t1900, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1900, 0.2).

% Extraction over time
narrative_ontology:measurement(impo_be_t1600, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1600, 0.7).
narrative_ontology:measurement(impo_be_t1650, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1650, 0.75).
narrative_ontology:measurement(impo_be_t1700, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1700, 0.8).
narrative_ontology:measurement(impo_be_t1750, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1750, 0.82).
narrative_ontology:measurement(impo_be_t1800, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1800, 0.84).
narrative_ontology:measurement(impo_be_t1850, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1850, 0.85).
narrative_ontology:measurement(impo_be_t1900, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1900, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1600, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1600, 0.8).
narrative_ontology:measurement(impo_su_t1650, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1650, 0.85).
narrative_ontology:measurement(impo_su_t1700, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1700, 0.88).
narrative_ontology:measurement(impo_su_t1750, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1750, 0.9).
narrative_ontology:measurement(impo_su_t1800, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1800, 0.9).
narrative_ontology:measurement(impo_su_t1850, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1850, 0.9).
narrative_ontology:measurement(impo_su_t1900, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1900, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imposition_mechanism_kernel'. It focuses on the coercive, top-down imposition of norms by the state, contrasting with readings that emphasize bottom-up acceptance or hybrid legitimation processes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
