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
 *   This constraint is a reading of the 'end_of_life_authority' kernel,
 *   specifically instantiating the 'slippery_slope_mechanism' perspective. It
 *   describes how initial autonomy-based frameworks for end-of-life decisions
 *   empirically expand beyond competent, terminal cases to include
 *   incompetent and non-terminal populations, often becoming a vehicle for
 *   underlying sanctity-of-life concerns. The constraint itself is the
 *   observed tendency of this expansion, which, while framed as protective,
 *   can become extractive for vulnerable groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.75).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.8).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.75).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority: Slippery Slope Mechanism").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '57764bfb-b1c1-440d-b45f-9036aaecc90e').
narrative_ontology:cs_kernel_codification('57764bfb-b1c1-440d-b45f-9036aaecc90e', formalized).
narrative_ontology:cs_authority_grounding('57764bfb-b1c1-440d-b45f-9036aaecc90e', practice).
narrative_ontology:cs_interpretation_layer_present('57764bfb-b1c1-440d-b45f-9036aaecc90e').
narrative_ontology:cs_reading_relation('57764bfb-b1c1-440d-b45f-9036aaecc90e', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('57764bfb-b1c1-440d-b45f-9036aaecc90e', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('57764bfb-b1c1-440d-b45f-9036aaecc90e', foundational, autonomy_expands_to_vulnerable).
narrative_ontology:cs_axiom_status(autonomy_expands_to_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('57764bfb-b1c1-440d-b45f-9036aaecc90e', autonomy_expands_to_vulnerable, empirically_contingent).
narrative_ontology:cs_axiom('57764bfb-b1c1-440d-b45f-9036aaecc90e', secondary, framework_co_opts_for_other_ends).
narrative_ontology:cs_axiom_status(framework_co_opts_for_other_ends, holdable).
narrative_ontology:cs_axiom_grounding('57764bfb-b1c1-440d-b45f-9036aaecc90e', framework_co_opts_for_other_ends, empirically_contingent).
narrative_ontology:cs_reference_frame('57764bfb-b1c1-440d-b45f-9036aaecc90e', initial_autonomy_framework_competent_terminal).
narrative_ontology:cs_drift_state('57764bfb-b1c1-440d-b45f-9036aaecc90e', contemporary_bioethics_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('57764bfb-b1c1-440d-b45f-9036aaecc90e', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, medical_authorities).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, legal_frameworks).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, advocates_for_sanctity_of_life).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients_with_chronic_suffering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply end-of-life frameworks, making decisions for patients based on evolving guidelines and legal precedents. They benefit from a clear, if expansive, framework that guides their practice and reduces legal ambiguity, even if it means expanding their authority.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, medical_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Codify and enforce the expanded end-of-life frameworks, providing the legal basis for medical decisions. They benefit from the stability and predictability of an established framework, even as its scope broadens.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, legal_frameworks, agenda_setter,
    institutional, civilizational, constrained, national).

% Benefit from the expansion of end-of-life frameworks beyond strict autonomy, as it aligns with their view of the intrinsic value of human life and the need for protection, even for those who cannot express their will. They actively lobby for such expansions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, advocates_for_sanctity_of_life, beneficiary,
    organized, generational, mobile, global).

% Bear the costs of decisions made under the expanded framework, often without their explicit consent or against their presumed wishes. Their autonomy is effectively overridden by the broader interpretation of the framework.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, local).

% May find their options for ending suffering limited or prolonged by the expanded framework, which might prioritize life preservation over relief from chronic, non-terminal conditions. They are subject to decisions made by others.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients_with_chronic_suffering, payer,
    powerless, biographical, trapped, local).

% Would object to the expansion of end-of-life frameworks beyond competent, terminal cases, arguing it erodes individual self-determination. They are often marginalized in policy debates that prioritize broader protective measures.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, autonomy_advocates, excluded,
    organized, biographical, mobile, global).

% Analyze the ethical implications and societal impact of the expanding end-of-life frameworks, documenting the drift and its consequences without directly participating in policy enforcement or suffering its effects.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, analytical_ethicists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Initially, to provide a clear and ethically sound framework for respecting individual autonomy in end-of-life decisions for competent, terminal patients, ensuring their wishes are honored and suffering is alleviated.
% TRANSFER_FUNCTION: Transfers decision-making authority from individual patients (especially incompetent or non-terminal) to medical and legal authorities, and to societal norms that prioritize life preservation, potentially prolonging life or limiting choices for those patients.
% ABSENT_VOICES: Competent patients who fear a loss of control over their end-of-life decisions if the framework expands to include non-autonomous cases without clear safeguards; their concerns are often overshadowed by protective narratives.
% DISAPPEARANCE_RATIONALE: If the 'slippery slope' mechanism vanished, the autonomy framework would likely revert to its original, narrower scope, leading to different end-of-life policies, potentially more individual control for a wider range of patients, and a re-evaluation of the role of medical and legal authorities in such decisions.
% FOUNDING_PROBLEM: To provide a humane and ethically sound process for individuals to exercise control over their dying process, particularly in the face of unbearable suffering and terminal illness, ensuring dignity and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Autonomy advocates attest the founding problem is still live but the solution is being misapplied and expanded beyond its original intent. Advocates for sanctity of life attest the founding problem was always broader than individual autonomy and the expansion is a necessary correction. Analytical ethicists corroborate the observed expansion and its ethical dilemmas through empirical studies and conceptual analysis.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it possesses a genuine coordination function (the original autonomy-respecting framework) but also exhibits asymmetric extraction. The 'slippery slope' describes how the framework's application expands, leading to higher extractiveness (0.75) from vulnerable patients whose autonomy is overridden. Suppression (0.80) is high due to the institutional and legal enforcement of these expanded interpretations, limiting alternatives for affected patients. The theater ratio (0.40) reflects that while the rhetoric of 'autonomy' may persist, a significant portion of the framework's operation serves other, often unstated, protective or life-preserving goals. The temporal measurements show a consistent increase in extractiveness, suppression, and theatricality over the interval, reflecting the observed expansion and drift.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of medical authorities and sanctity-of-life advocates, the expansion of the framework is a necessary and ethical evolution to protect vulnerable life. From the perspective of autonomy advocates and affected patients, it represents an erosion of individual rights and an imposition of external values. The engine's classification will highlight this divergence by computing different effective extraction values for these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical and legal authorities, along with advocates for the sanctity of life, are structural beneficiaries, as the expanded framework provides clear guidance and aligns with their protective goals. Incompetent and non-terminal patients with chronic suffering are the primary targets/victims, as their choices are limited and decisions are made for them under the expanded framework. Autonomy advocates are excluded, as their original framing is diluted by the expansion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slippery_slope_inevitability,
    'Is the ''slippery slope'' an inevitable empirical mechanism inherent to autonomy-based end-of-life frameworks, or a contingent outcome of specific policy choices and societal values?',
    'Comparative analysis of jurisdictions with varying policy safeguards and cultural contexts: if some jurisdictions maintain strict autonomy limits without expansion, it suggests contingency.',
    'If inevitable, the constraint is more ''mountain-like'' in its persistence; if contingent, it is more amenable to policy intervention and reclassification as a Snare or a more tightly controlled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_inevitability, empirical, 'Whether the expansion mechanism is an inherent property or a policy choice.').

omega_variable(
    autonomy_vs_protection_boundary,
    'At what point does the expansion of an autonomy framework, intended to protect vulnerable populations, become a form of extraction from those populations by overriding their (potential) will?',
    'Ethical consensus building among diverse stakeholders, informed by patient experience narratives and legal precedent, to define clear boundaries for ''best interest'' vs. ''substituted judgment''.',
    'Clarifying this boundary would allow for more precise classification of specific policies as either protective (Rope/Scaffold) or extractive (Snare/Tangled Rope), depending on which side of the boundary they fall.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_protection_boundary, conceptual, 'Defining the ethical line between protection and extraction in end-of-life decisions.').

omega_variable(
    intent_vs_outcome_of_expansion,
    'Is the observed expansion of the framework primarily driven by a genuine desire to protect life (sanctity concerns) or by institutional inertia, risk aversion, and a desire for broader control by authorities?',
    'Qualitative research into decision-making processes of medical and legal bodies, examining stated rationales versus actual outcomes and incentives.',
    'If driven by genuine protective intent, the constraint might be re-evaluated as a more complex Rope; if by institutional self-interest, its extractive nature (Tangled Rope/Snare) is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_outcome_of_expansion, empirical, 'Distinguishing the underlying motivations for framework expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.25).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.3).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.35).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 40, 0.38).
narrative_ontology:measurement(end__tr_t50, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 40, 0.73).
narrative_ontology:measurement(end__be_t50, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 50, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(end__su_t50, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% This constraint describes the empirical mechanism by which an autonomy-based end-of-life framework expands its scope, influencing both pure autonomy and sanctity-of-life readings by altering their practical application and perceived limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
