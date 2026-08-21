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
 *   This constraint describes the 'slippery slope' mechanism observed in
 *   end-of-life policy, where initial autonomy-based frameworks, designed for
 *   competent, terminal patients, empirically expand to include incompetent
 *   and non-terminal populations. This reading views the expansion as a
 *   structural drift that transforms a coordination mechanism into an
 *   extractive one, where the original justification (autonomy) becomes a
 *   vehicle for broader, potentially unacknowledged, policy goals or values.
 *   The claimed type is 'tangled_rope' because it still claims a coordination
 *   function (respecting autonomy) but operates with significant, increasing
 *   extraction and suppression as its scope expands.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.72).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.8).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority: Slippery Slope Mechanism").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'aa90a7a7-7b1a-449f-8304-cf57b1a93011').
narrative_ontology:cs_kernel_codification('aa90a7a7-7b1a-449f-8304-cf57b1a93011', formalized).
narrative_ontology:cs_authority_grounding('aa90a7a7-7b1a-449f-8304-cf57b1a93011', lineage).
narrative_ontology:cs_interpretation_layer_present('aa90a7a7-7b1a-449f-8304-cf57b1a93011').
narrative_ontology:cs_reading_relation('aa90a7a7-7b1a-449f-8304-cf57b1a93011', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('aa90a7a7-7b1a-449f-8304-cf57b1a93011', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_axiom('aa90a7a7-7b1a-449f-8304-cf57b1a93011', foundational, autonomy_principles_are_inherently_expansive).
narrative_ontology:cs_axiom_status(autonomy_principles_are_inherently_expansive, holdable).
narrative_ontology:cs_axiom_grounding('aa90a7a7-7b1a-449f-8304-cf57b1a93011', autonomy_principles_are_inherently_expansive, empirically_contingent).
narrative_ontology:cs_axiom('aa90a7a7-7b1a-449f-8304-cf57b1a93011', secondary, suffering_justifies_life_ending_beyond_competence_and_terminality).
narrative_ontology:cs_axiom_status(suffering_justifies_life_ending_beyond_competence_and_terminality, holdable).
narrative_ontology:cs_axiom_grounding('aa90a7a7-7b1a-449f-8304-cf57b1a93011', suffering_justifies_life_ending_beyond_competence_and_terminality, instrumental).
narrative_ontology:cs_reference_frame('aa90a7a7-7b1a-449f-8304-cf57b1a93011', initial_autonomy_framework_for_competent_terminal).
narrative_ontology:cs_drift_state('aa90a7a7-7b1a-449f-8304-cf57b1a93011', contemporary_practice_and_policy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa90a7a7-7b1a-449f-8304-cf57b1a93011', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, medical_authorities).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, proponents_of_expanded_euthanasia).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients_with_chronic_suffering).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, families_opposed_to_expanded_criteria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and interprets clinical guidelines for end-of-life decisions. Benefits from clear protocols and reduced legal ambiguity, even as the scope expands. Administers the framework and enforces its application.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, medical_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Actively advocates for the expansion of end-of-life options beyond competent, terminal patients. Benefits from the empirical expansion of the framework, seeing it as a fulfillment of broader compassionate goals.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, proponents_of_expanded_euthanasia, beneficiary,
    organized, biographical, mobile, national).

% Are brought into the scope of end-of-life decisions without their current capacity for consent. Their fate is determined by the expanded framework, often against the wishes of their families or prior expressed intent. They bear the ultimate cost of the expansion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, local).

% Find themselves eligible for life-ending interventions due to chronic suffering, even if not terminally ill. While some may seek this, the expansion of the framework creates pressure and options that might not align with their full range of preferences or values, and they may feel subtly coerced by the availability of such options.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients_with_chronic_suffering, payer,
    moderate, biographical, constrained, local).

% Bear the emotional and legal burden of contesting end-of-life decisions made under the expanded framework for their loved ones, especially incompetent or non-terminal patients. Their preferences may be overridden by the framework's broader application.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, families_opposed_to_expanded_criteria, payer,
    organized, biographical, constrained, local).

% Fundamentally oppose any expansion of life-ending interventions, viewing human life as intrinsically valuable. They are often marginalized in the policy-making process that drives the expansion, their concerns dismissed as religious or non-scientific.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, sanctity_of_life_advocates, excluded,
    organized, generational, constrained, national).

% Initially championed autonomy for competent, terminal patients but now observe the framework's expansion with concern, fearing it distorts the original intent and creates unintended harms. They analyze the drift but may lack direct power to halt it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, autonomy_advocates_original_scope, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, legally sanctioned framework for making end-of-life decisions, initially intended to respect the autonomy of competent, terminal patients facing unbearable suffering.
% TRANSFER_FUNCTION: Transfers decision-making authority from individual patient/family to a broader medical-legal framework, and potentially transfers the 'right to die' from a narrow, self-determined context to a broader, institutionally-mediated one, encompassing vulnerable populations.
% ABSENT_VOICES: Incompetent patients, whose 'autonomy' is interpreted by others, and families whose values conflict with the expanded framework but lack the power to resist. Also, those who would argue for a more restrictive interpretation of autonomy to protect vulnerable lives.
% DISAPPEARANCE_RATIONALE: If the expanded framework and its enforcement vanished, end-of-life decisions for incompetent or non-terminal patients would revert to a state of greater legal and ethical uncertainty, likely leading to more restrictive practices and a re-evaluation of the scope of autonomy in these contexts. The medical and legal systems would need to reorganize their approach to these cases.
% FOUNDING_PROBLEM: The ethical and legal challenge of respecting individual autonomy and alleviating unbearable suffering for competent, terminally ill patients, while preventing prolonged, undignified dying.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethicists and legal scholars generally corroborate the initial problem. However, its current status is contested: proponents of expansion argue it's still live and requires broader application, while critics argue the problem has been over-solved or distorted, leading to unintended consequences for vulnerable populations. Independent bioethics commissions often highlight this divergence.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.72, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.72) and suppression (0.80) are high because the framework, as it expands, increasingly applies to individuals who cannot genuinely consent or exit, effectively extracting their agency or even their lives under the guise of autonomy. The theater ratio (0.45) reflects that while some genuine autonomy-respecting functions remain, a significant portion of the framework's operation becomes performative justification for its expanded, more extractive scope. The temporal measurements show a clear increase in extractiveness, suppression, and theatricality over time, consistent with an 'empirical expansion' and 'slippery slope' dynamic.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of proponents, the expansion is a natural, compassionate evolution of autonomy. From the perspective of victims and critics, it is an overreach that subverts the original intent and imposes decisions on vulnerable populations. The engine's classification will highlight this divergence by computing a high effective extraction for the victim seats, contrasting with the claimed 'rope' (coordination) function.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical authorities and proponents of expanded euthanasia are beneficiaries, as the framework provides clear protocols and advances their policy goals. Incompetent and non-terminal patients, along with their dissenting families, are victims, as they bear the costs of the framework's expansion without full agency or consent. Sanctity-of-life advocates are excluded, as their core premise is directly contradicted by the expansion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_of_expansion,
    'Is the observed expansion an inevitable logical consequence of the autonomy principle itself, or is it driven by external social, economic, or cultural factors?',
    'Comparative analysis of jurisdictions with similar autonomy frameworks but different social/cultural contexts: if expansion varies significantly, it suggests external drivers; if uniform, it suggests inherent logical pressure.',
    'If inevitable, the ''slippery slope'' is a structural feature of autonomy frameworks, making the initial ''rope'' claim inherently unstable. If externally driven, interventions could target the external factors without abandoning the core autonomy principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_of_expansion, empirical, 'Whether the expansion is inherent to autonomy or externally driven.').

omega_variable(
    autonomy_as_cover_for_sanctity_concerns,
    'To what extent does the expanded framework, while ostensibly based on autonomy, become a vehicle for underlying ''sanctity of life'' concerns (e.g., preventing prolonged suffering at all costs, even if against patient''s wishes) or other non-autonomy values?',
    'Qualitative analysis of policy debates and clinical decision-making rationales, identifying explicit or implicit appeals to values beyond individual patient autonomy in expanded cases.',
    'If autonomy is largely a cover, the constraint''s true ''coordination function'' is distorted, increasing its effective extraction and potentially reclassifying it closer to a ''snare'' or a ''piton'' where the original mandate is lost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_as_cover_for_sanctity_concerns, conceptual, 'Autonomy as a cover for other values in expanded end-of-life frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(end__tr_t8, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 8, 0.28).
narrative_ontology:measurement(end__tr_t16, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 16, 0.35).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 24, 0.4).
narrative_ontology:measurement(end__tr_t32, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 32, 0.43).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(end__be_t8, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(end__be_t16, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(end__be_t32, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(end__su_t8, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(end__su_t16, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(end__su_t32, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'end_of_life_authority' kernel, describing the empirical 'slippery slope' mechanism. It influences how the 'autonomy_reading' is applied and directly forecloses the 'sanctity_reading' by expanding life-ending options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
