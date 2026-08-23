% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree as Sufficient Displacement of Prior Practice (Exogenous Override Reading)
 *   domain: political/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint story instantiates the exogenous_override_reading of the
 *   contested kernel legitimacy_of_imposed_practice. The standing arrangement
 *   is a state modernization project that imposes new calendars and dress
 *   codes by legal decree, treating state authority as sufficient to displace
 *   prior practice without requiring internalization by affected populations.
 *   Rural communities bear adjustment costs and engage in non-compliance and
 *   workarounds, while the state bureaucracy collects administrative
 *   unification and centralized cultural authority. The arrangement is
 *   actively enforced because compliance does not emerge spontaneously; the
 *   genuine coordination function (national legal uniformity) is tangled with
 *   asymmetric extraction (rural populations pay the cultural and economic
 *   costs of displacement without consultation).
 *
 * KEY AGENTS:
 *   - state_modernization_apparatus (institutional/arbitrage) â primary agenda setter and beneficiary; imposes and enforces legal mandates
 *   - rural_communities (powerless/constrained) â primary payer; bear adjustment costs and maintain covert workarounds
 *   - traditional_authority_structures (moderate/constrained) â excluded voices; customary and religious leaders absent from policy formulation
 *   - comparative_historians (analytical/analytical) â analytical observers comparing imposition outcomes across regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree as Sufficient Displacement of Prior Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, 'c7a4d6c0-d167-4bc5-8fb4-26586decc5a9').
narrative_ontology:cs_kernel_codification('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', formalized).
narrative_ontology:cs_authority_grounding('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', extraction).
narrative_ontology:cs_interpretation_layer_present('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9').
narrative_ontology:cs_reading_relation('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, forecloses).
narrative_ontology:cs_axiom('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', foundational, imposition_without_internalization_sufficient).
narrative_ontology:cs_axiom_status(imposition_without_internalization_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', imposition_without_internalization_sufficient, conventional).
narrative_ontology:cs_axiom('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', secondary, legal_mandate_creates_obligation_regardless_of_consent).
narrative_ontology:cs_axiom_status(legal_mandate_creates_obligation_regardless_of_consent, holdable).
narrative_ontology:cs_axiom_grounding('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', legal_mandate_creates_obligation_regardless_of_consent, conventional).
narrative_ontology:cs_reference_frame('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', modernizing_imposition_framework).
narrative_ontology:cs_drift_state('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', post_imposition_empirical_assessment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7a4d6c0-d167-4bc5-8fb4-26586decc5a9', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposes legal mandates to abolish traditional calendars and dress codes. Enforces through administrative and coercive mechanisms. Collects administrative unification and centralized cultural authority as the benefit of nationwide legal uniformity.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus, beneficiary).

% Bear adjustment costs of abandoning long-established practices without consultation. Maintain covert workarounds and non-compliance where enforcement is porous, especially around calendar observance. Face penalties for visible non-compliance such as traditional dress.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_communities, payer,
    powerless, biographical, constrained, local).

% Local religious leaders, clan elders, and customary authorities whose legitimacy derives from preserving traditional practice. Excluded from policy formulation and would object to displacement, but lack formal channels to influence state decree.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_authority_structures, excluded,
    moderate, generational, constrained, regional).

% Analyze state formation processes across cases, comparing exogenous imposition with endogenous adoption pathways. Neither collect from nor pay into this specific constraint.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a uniform national legal and cultural regime across diverse territories, eliminating fragmented local practices that impede centralized administration, taxation, and military mobilization.
% TRANSFER_FUNCTION: Moves cultural and administrative compliance from rural communities to the state modernization apparatus; moves adjustment costsâeconomic, social, and psychologicalâonto rural populations without compensation.
% ABSENT_VOICES: Rural populations themselves were not consulted in the decree; traditional authority structures and local religious leaders are absent from the policy-setting conversation and would object if present.
% DISAPPEARANCE_RATIONALE: If the legal mandate and its enforcement vanished overnight, rural communities would revert to prior calendar and dress practices; the state's centralized cultural uniformity project would fragment, and the administrative unification benefit would dissolve.
% FOUNDING_PROBLEM: The need to unify disparate territories under a common administrative and cultural framework to build a modern nation-state capable of mobilization, taxation, and international recognition.
% FOUNDING_PROBLEM_CORROBORATION: State bureaucrats and modernization theorists attest to the need for unification. Anthropologists and comparative historians from outside the benefiting party note that endogenous adoption pathways often succeed where imposition fails; there is no corroboration from neutral parties that pure decree is effective, and substantial evidence that it generates resistance and covert non-compliance.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because rural populations bear real adjustment costs (economic, social, psychological) without compensation or consultation, and the state captures centralized authority. Suppression (0.70) is high because the constraint persists only through active enforcement; without it, non-compliance and workarounds would expand. Theater ratio (0.40) reflects moderate performative enforcement: the state maintains a public narrative of successful modernization while actual practice drifts in private (calendar workarounds). Accessibility collapse (0.60) captures that prior practices are legally abolished and socially penalized, yet persist covertly. Resistance (0.55) reflects documented rural non-compliance and covert practice maintenance, which falls short of organized rebellion but is persistent.
 *
 * PERSPECTIVAL GAP:
 *   The state_modernization_apparatus seat experiences the constraint as a necessary coordination mechanism for nation-building; the engine will compute a low directionality and damped effective extraction from this seat. The rural_communities seat experiences the same arrangement as coercive cultural extraction; the engine will compute high directionality and amplified effective extraction. The divergence is structural: same constraint, same metrics, opposite positions in the beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_modernization_apparatus is declared as agenda_setter and beneficiary: it collects administrative unification and centralized authority, has institutional power, arbitrage-grade exit (can alter policy instruments), and national scope. This drives d toward the beneficiary end. Rural_communities are declared as payer (victim): they bear costs, are powerless, have constrained exit (geographically and economically bound), and local scope. This drives d toward the target end. Traditional authorities are excluded and thus outside the directionality derivation for extraction. The gap in power and exit between the state and rural communities is what produces the seat-divergent classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy interview and beneficiary/victim declarations, this constraint could be misread as a Scaffold (transitional support for modernization) or a Rope (national coordination). Declaring the victims (rural_communities), the active enforcement requirement, and the contested founding problem prevents that mislabeling. The coordination function (uniform legal regime) is real, but it is inseparable from the asymmetric cost-shifting onto non-consenting populations. That hybrid structure is exactly what Tangled Rope captures: genuine coordination plus extraction, held together by enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_locus,
    'Does the exogenous_override_reading''s claim that decree authority is sufficient logically foreclose the hybrid_scaffolding_reading''s reliance on quasi-endogenous pull, or do these readings merely coexist as different interpretations of the same historical processes?',
    'Analysis of whether any single policy framework can consistently hold both that compliance follows purely from legal mandate AND that ideological messaging generates necessary supplementary pull.',
    'If foreclosed, the readings are mutually exclusive and represent distinct constraint types; if coexisting, they are competing framings of the same hybrid mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_locus, conceptual, 'Logical relationship between exogenous override and hybrid scaffolding readings').

omega_variable(
    internalization_trajectory,
    'Does the imposed practice eventually become internalized, transforming the constraint from pure exogenous override toward hybrid scaffolding or endogenous climb, or does it remain dependent on perpetual enforcement?',
    'Longitudinal study of compliance rates and cultural practice persistence following enforcement withdrawal, regime change, or generational turnover.',
    'If internalization occurs, the constraint drifts toward a lower-extraction coordination type; if not, it remains a tangled rope requiring perpetual enforcement, with rising theater as gaps between law and practice widen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_trajectory, empirical, 'Whether imposed practice achieves endogenous uptake over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t4, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(legi_tr_t12, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(legi_be_t4, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(legi_be_t12, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t4, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(legi_su_t12, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 24, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% The kernel legitimacy_of_imposed_practice decomposes into three structurally distinct readings: endogenous_climb_reading (internalization-required), exogenous_override_reading (decree-sufficient), and hybrid_scaffolding_reading (reinforced imposition). Each reading carries a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
