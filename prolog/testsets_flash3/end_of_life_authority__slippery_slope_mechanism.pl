% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: End-of-Life Authority: Slippery Slope Mechanism
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint describes the 'slippery slope' mechanism in end-of-life
 *   policy, where initial autonomy-based frameworks, intended for competent,
 *   terminal patients, empirically expand to include incompetent and
 *   non-terminal populations. This reading views the expansion as a
 *   structural drift that transforms a coordination mechanism into an
 *   extractive one, where vulnerable populations become victims. The claimed
 *   type 'tangled_rope' reflects the dual function: it still coordinates
 *   difficult decisions, but with significant, asymmetric extraction from
 *   those who cannot consent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.75).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority: Slippery Slope Mechanism").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '6e2afd5f-a92e-4fe3-b872-c9bb4db14b20').
narrative_ontology:cs_kernel_codification('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', formalized).
narrative_ontology:cs_authority_grounding('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', practice).
narrative_ontology:cs_interpretation_layer_present('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20').
narrative_ontology:cs_reading_relation('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', foundational, autonomy_expands_beyond_competence).
narrative_ontology:cs_axiom_status(autonomy_expands_beyond_competence, holdable).
narrative_ontology:cs_axiom_grounding('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', autonomy_expands_beyond_competence, empirically_contingent).
narrative_ontology:cs_axiom('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', secondary, terminality_criteria_drift).
narrative_ontology:cs_axiom_status(terminality_criteria_drift, holdable).
narrative_ontology:cs_axiom_grounding('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', terminality_criteria_drift, empirically_contingent).
narrative_ontology:cs_reference_frame('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', initial_autonomy_framework).
narrative_ontology:cs_drift_state('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', contemporary_policy_landscape, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6e2afd5f-a92e-4fe3-b872-c9bb4db14b20', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, healthcare_systems).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, some_families).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers end-of-life policies, balancing resource allocation, legal risk, and public perception. Benefits from frameworks that streamline difficult end-of-life decisions, potentially reducing long-term care costs, but also bears the cost of public scrutiny and legal challenges.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, healthcare_systems, agenda_setter,
    institutional, generational, constrained, national).

% Are the primary targets of the 'slippery slope' mechanism, as their lack of capacity makes them vulnerable to decisions made by others under expanding criteria. They bear the ultimate cost of having their lives ended without their explicit, current consent.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, local).

% Experience the expansion of end-of-life criteria from terminal illness to chronic suffering, potentially facing pressure or having their options narrowed by the perceived availability of life-ending interventions. Their suffering, while profound, is not immediately life-threatening.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients, payer,
    moderate, biographical, constrained, local).

% Groups (e.g., elderly, disabled, mentally ill) whose lives may be devalued or whose access to care may be subtly shifted by the expansion of end-of-life frameworks. They are identity-locked by societal perceptions and systemic biases.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, vulnerable_populations, payer,
    powerless, generational, identity_locked, national).

% May find relief from the burden of caregiving or emotional distress when end-of-life options expand, particularly for incompetent or non-terminal loved ones. They benefit from the perceived 'solution' to prolonged suffering, though this can be a complex and ethically fraught benefit.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, some_families, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the ethical implications and societal impacts of expanding end-of-life frameworks. They observe the drift in criteria and advocate for safeguards or alternative approaches, but do not directly administer the constraint.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, bioethicists_and_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for managing complex end-of-life decisions, aiming to reduce suffering and provide a sense of control for patients and families, and clarity for healthcare providers.
% TRANSFER_FUNCTION: Transfers the authority to make life-ending decisions from the individual (in cases of competence and terminal illness) to proxies or medical panels (for incompetent patients) and expands the scope of 'unbearable suffering' to include non-terminal conditions, effectively transferring the burden of prolonged life from the system/family to the patient.
% ABSENT_VOICES: The direct voices of incompetent patients are absent by definition; their interests are represented by proxies whose interpretations of 'best interest' may align with the expanding framework. Future vulnerable populations, whose lives may be subtly devalued by these expansions, are also absent.
% DISAPPEARANCE_RATIONALE: If the 'slippery slope' mechanism vanished, the expansion of end-of-life criteria would halt or reverse. Decisions would revert to stricter interpretations of autonomy, competence, and terminal illness, leading to a re-evaluation of care for incompetent and non-terminal patients, and potentially increasing the burden on families and healthcare systems.
% FOUNDING_PROBLEM: The initial problem was to respect individual autonomy in the face of terminal illness and unbearable suffering, allowing competent adults to make choices about the timing and manner of their death.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for patient autonomy and many patients/families attest that the problem of unbearable suffering at the end of life remains live. Critics of the slippery slope mechanism, including some bioethicists and disability rights advocates, corroborate the initial problem but argue that the solution has expanded beyond its original scope, creating new problems.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the expansion of criteria allows decisions to be made for vulnerable individuals who cannot consent, leading to outcomes that may not align with their best interests. Suppression is high because the framework, once established, creates legal and medical precedents that are difficult to resist, particularly for those without capacity. Theater ratio is moderate, as the 'autonomy' justification for the expanded framework becomes increasingly performative when applied to non-autonomous individuals. The temporal measurements show a clear increase in extractiveness and suppression over time, reflecting the observed expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of healthcare systems and some families, the expanded framework is a necessary evolution to address suffering. From the perspective of vulnerable populations and their advocates, it represents a dangerous erosion of safeguards. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Healthcare systems and some families are beneficiaries, as the framework provides solutions to difficult situations. Incompetent patients, non-terminal patients, and vulnerable populations are victims, as the framework's expansion targets them for decisions they cannot make or resist. Bioethicists and advocates act as observers, analyzing the drift.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_drift_causality,
    'Is the observed expansion of end-of-life criteria a direct causal consequence of the initial autonomy framework (a true ''slippery slope''), or is it driven by independent societal factors (e.g., resource scarcity, changing demographics)?',
    'Comparative analysis of jurisdictions with similar initial autonomy frameworks but different societal pressures; longitudinal studies tracking policy changes and their drivers.',
    'If directly causal, it strengthens the argument for inherent risks in autonomy-based frameworks. If driven by independent factors, it suggests the need for different policy interventions to address those underlying pressures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_drift_causality, empirical, 'Determining the primary drivers of the observed expansion in end-of-life criteria.').

omega_variable(
    autonomy_vs_sanctity_reconciliation,
    'To what extent does the expansion of autonomy-based frameworks become a vehicle for underlying sanctity-of-life concerns (e.g., ending lives deemed ''not worth living'' by others), rather than genuine patient self-determination?',
    'Qualitative studies of decision-making processes for incompetent patients, analysis of proxy motivations, and examination of public discourse framing around ''dignity'' and ''quality of life''.',
    'If sanctity concerns are found to be a significant driver, it reclassifies the constraint''s underlying motivation, potentially shifting it closer to a ''snare'' for vulnerable populations, as the coordination story (autonomy) becomes a cover for a different value system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_reconciliation, conceptual, 'Assessing whether autonomy frameworks are co-opted by sanctity-of-life concerns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.28).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.35).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.38).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, sanctity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'end_of_life_authority' kernel, focusing on the empirical expansion of autonomy-based frameworks. It is linked to the 'autonomy_reading' and 'sanctity_reading' as sibling interpretations of the same core authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
