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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree Authority as Exogenous Override
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint represents the 'exogenous override' reading of state
 *   authority in cultural imposition, where legal mandates are considered
 *   sufficient to displace prior practices, and compliance is expected
 *   regardless of internalization. This reading emphasizes the state's power
 *   to unilaterally define and enforce new norms, often seen in early
 *   20th-century modernization campaigns (e.g., calendar reforms, dress
 *   codes). The state's 'modernization agenda' is the primary beneficiary,
 *   while rural populations bear the costs of adjustment and coercive
 *   enforcement, often resorting to practical workarounds that create a gap
 *   between formal compliance and actual practice. The claimed type is
 *   'snare' because the coordination story (national unity, efficiency) is
 *   cover for the extraction of compliance and legitimacy by the state,
 *   enforced through suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.75).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, snare).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Authority as Exogenous Override").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '9e77f788-c41e-4f36-99bc-82e38da09392').
narrative_ontology:cs_kernel_codification('9e77f788-c41e-4f36-99bc-82e38da09392', formalized).
narrative_ontology:cs_authority_grounding('9e77f788-c41e-4f36-99bc-82e38da09392', extraction).
narrative_ontology:cs_interpretation_layer_present('9e77f788-c41e-4f36-99bc-82e38da09392').
narrative_ontology:cs_reading_relation('9e77f788-c41e-4f36-99bc-82e38da09392', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('9e77f788-c41e-4f36-99bc-82e38da09392', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('9e77f788-c41e-4f36-99bc-82e38da09392', foundational, state_decree_is_law).
narrative_ontology:cs_axiom_status(state_decree_is_law, holdable).
narrative_ontology:cs_axiom_grounding('9e77f788-c41e-4f36-99bc-82e38da09392', state_decree_is_law, conventional).
narrative_ontology:cs_axiom('9e77f788-c41e-4f36-99bc-82e38da09392', foundational, compliance_is_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(compliance_is_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9e77f788-c41e-4f36-99bc-82e38da09392', compliance_is_sufficient_for_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('9e77f788-c41e-4f36-99bc-82e38da09392', unilateral_state_sovereignty).
narrative_ontology:cs_drift_state('9e77f788-c41e-4f36-99bc-82e38da09392', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9e77f788-c41e-4f36-99bc-82e38da09392', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces decrees, believing in the inherent authority of the state to reshape society. Benefits from the expansion of state power and the perceived success of modernization efforts. Their careers and legitimacy are tied to the effective implementation of state policy.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).

% The abstract goal of transforming society through top-down legal and administrative means. Benefits from the perceived efficiency and universality of state-imposed practices, even if actual compliance is superficial.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).

% Bear the direct costs of adjusting to new practices, often without understanding their rationale or having a voice in their formulation. Experience coercive enforcement and practical workarounds, leading to a gap between formal compliance and actual internalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, local).

% Lose their customary authority and social standing as state decrees displace traditional practices. They may resist passively or seek to subvert enforcement, but their power is diminished by the state's direct imposition.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites, payer,
    moderate, generational, constrained, regional).

% Analyze the effectiveness and human rights implications of state-led modernization, often highlighting the gap between legal mandate and social reality. Their reports can influence international aid or diplomatic pressure.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal behavior around a new set of state-sanctioned practices, replacing diverse local customs with a uniform national standard, thereby facilitating state administration and control.
% TRANSFER_FUNCTION: Transfers authority and legitimacy from traditional local institutions and practices to the centralized state, extracting compliance and resources from local populations to serve the state's modernization agenda.
% ABSENT_VOICES: Local community leaders and cultural practitioners, whose knowledge and authority are explicitly devalued by the state's top-down approach. Their perspectives on the value of traditional practices and the feasibility of imposed changes are systematically ignored.
% DISAPPEARANCE_RATIONALE: If the state's authority to impose practices vanished, many traditional customs would likely re-emerge or adapt, and local forms of governance might regain influence. The state's administrative capacity would be severely hampered, and the 'modernized' practices would likely unravel without coercive enforcement.
% FOUNDING_PROBLEM: The state perceived traditional, diverse local practices as obstacles to national unity, administrative efficiency, and 'progress,' leading to fragmentation and hindering centralized control.
% FOUNDING_PROBLEM_CORROBORATION: State officials and proponents of modernization attest that the problem of 'backwardness' and fragmentation remains live, justifying continued top-down intervention. International development agencies sometimes corroborate the need for standardized practices, though often with caveats about local participation.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the state imposes significant costs on populations forced to abandon established practices, often without commensurate benefits. Suppression is also high (0.75) as active enforcement (fines, arrests, propaganda) is required to maintain even superficial compliance. The theater ratio (0.4) reflects the gap between formal adherence and actual internalization; much of the 'compliance' is performative, masking continued traditional practices or workarounds. Resistance is moderate (0.5) due to passive non-compliance and local subversion, but rarely overt challenge given the high suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, this is a necessary 'rope' for national development, coordinating society towards a modern future. From the perspective of rural populations, it is a 'snare' that extracts their cultural autonomy and resources through coercion. The engine's classification as 'snare' reflects the structural reality of asymmetric extraction and suppression, despite the state's 'rope' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy and its modernization agenda are clear beneficiaries, as the constraint expands their power and legitimizes their vision (low directionality). Rural populations and traditional elites are targets, bearing the costs of displacement and enforcement (high directionality). International observers are analytical, neither benefiting nor paying directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (national unity, efficiency) is presented as live, but the high extractiveness and theater ratio suggest a significant portion of its function has atrophied into rent-seeking (extraction of compliance for state power) rather than genuine coordination. The persistence relies on active enforcement rather than internalized legitimacy, indicating a snare-like dynamic where the founding problem is used as cover for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_vs_formal_compliance,
    'What is the true extent of internalization versus superficial compliance with state-imposed practices?',
    'Longitudinal ethnographic studies and covert observation of daily life, comparing declared adherence with actual behavior over generations.',
    'If internalization is low, the constraint''s effective suppression and theater ratio are higher than measured, indicating a more severe snare. If internalization is high, the constraint might be reclassified towards a tangled rope or even rope, as genuine coordination emerges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_vs_formal_compliance, empirical, 'Distinguishing between coerced compliance and genuine adoption of new practices.').

omega_variable(
    state_authority_grounding_ambiguity,
    'Is the state''s authority to impose practices grounded in a genuine social contract or in coercive power alone?',
    'Historical analysis of state formation, examining the role of popular consent, revolutionary legitimacy, or colonial imposition in establishing state power.',
    'If grounded in coercion, the constraint is a pure snare. If a genuine social contract exists, the constraint might have a latent rope function, even if currently extractive, suggesting a tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_authority_grounding_ambiguity, conceptual, 'Ambiguity regarding the foundational legitimacy of state authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, police enforcement) or internalized (fear, social pressure to conform)?',
    'Post-decree relaxation studies: if compliance persists after active enforcement is removed, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the snare more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in cultural imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 1920, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1930, 0.35).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1960, 0.43).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1970, 0.41).
narrative_ontology:measurement(legi_tr_t1980, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1980, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1930, 0.6).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1960, 0.67).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1970, 0.66).
narrative_ontology:measurement(legi_be_t1980, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1980, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1950, 0.78).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1960, 0.77).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1970, 0.76).
narrative_ontology:measurement(legi_su_t1980, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1980, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, state_taxation_system).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, national_education_curriculum).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimacy_of_imposed_practice' kernel, focusing on the state's capacity for exogenous override. Other readings explore endogenous adoption and hybrid scaffolding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
