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
 *   domain: medical_ethics/bioethics/policy
 *
 * SUMMARY:
 *   This constraint describes the 'slippery slope' argument in end-of-life
 *   policy: an initial framework, grounded in individual autonomy for
 *   competent, terminal patients, empirically expands its application to
 *   include incompetent patients and those with chronic but non-terminal
 *   suffering. This expansion is seen as a mechanism that subtly shifts the
 *   underlying ethical justification, potentially leading to outcomes that
 *   contradict the original intent of respecting individual choice. The
 *   constraint is claimed as a Tangled Rope because it still performs a
 *   coordination function (managing end-of-life decisions) but with
 *   significant, increasing extraction from vulnerable populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.65).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.7).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority: Slippery Slope Mechanism").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'd2a6b863-b722-46eb-a9cb-44ad142825e2').
narrative_ontology:cs_kernel_codification('d2a6b863-b722-46eb-a9cb-44ad142825e2', formalized).
narrative_ontology:cs_authority_grounding('d2a6b863-b722-46eb-a9cb-44ad142825e2', lineage).
narrative_ontology:cs_interpretation_layer_present('d2a6b863-b722-46eb-a9cb-44ad142825e2').
narrative_ontology:cs_reading_relation('d2a6b863-b722-46eb-a9cb-44ad142825e2', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('d2a6b863-b722-46eb-a9cb-44ad142825e2', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('d2a6b863-b722-46eb-a9cb-44ad142825e2', foundational, autonomy_framework_is_vulnerable_to_drift).
narrative_ontology:cs_axiom_status(autonomy_framework_is_vulnerable_to_drift, holdable).
narrative_ontology:cs_axiom_grounding('d2a6b863-b722-46eb-a9cb-44ad142825e2', autonomy_framework_is_vulnerable_to_drift, empirically_contingent).
narrative_ontology:cs_axiom('d2a6b863-b722-46eb-a9cb-44ad142825e2', secondary, expansion_to_incompetent_patients_is_problematic).
narrative_ontology:cs_axiom_status(expansion_to_incompetent_patients_is_problematic, holdable).
narrative_ontology:cs_axiom_grounding('d2a6b863-b722-46eb-a9cb-44ad142825e2', expansion_to_incompetent_patients_is_problematic, deontological).
narrative_ontology:cs_reference_frame('d2a6b863-b722-46eb-a9cb-44ad142825e2', initial_autonomy_framework_for_competent_terminal).
narrative_ontology:cs_drift_state('d2a6b863-b722-46eb-a9cb-44ad142825e2', contemporary_policy_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d2a6b863-b722-46eb-a9cb-44ad142825e2', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, healthcare_systems).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, some_families).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients_with_chronic_suffering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers end-of-life protocols, often under pressure to manage resources and patient flow. Benefits from clear, expanded criteria for end-of-life decisions, which can reduce long-term care burdens. Faces legal and ethical challenges if criteria are perceived as too broad.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, healthcare_systems, agenda_setter,
    institutional, generational, constrained, national).

% Are the primary targets of the slippery slope mechanism. Their end-of-life decisions are made by proxies, and the expanded framework allows for termination of life-sustaining treatment or active euthanasia based on criteria that may not align with their past expressed wishes or best interests, as interpreted by some. They have no direct agency in the decision.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, local).

% May find themselves eligible for end-of-life interventions under expanded criteria, even if their condition is not immediately terminal. While some may seek this, others may feel pressured or find their care options shifting towards life-ending interventions rather than palliative care, due to the expanded framework. Their autonomy is subtly undermined by the broadened scope.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients_with_chronic_suffering, payer,
    moderate, biographical, constrained, local).

% May experience relief from the burden of long-term care for severely ill or incompetent relatives, or from witnessing prolonged suffering. The expanded framework provides a legal and ethical pathway for decisions they might otherwise struggle with. However, other families may resist these expanded options.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, some_families, beneficiary,
    moderate, biographical, constrained, local).

% Actively debate and monitor the expansion of end-of-life criteria. They analyze policy changes, legal precedents, and empirical outcomes, advocating for either stricter safeguards or broader access, depending on their ethical grounding. Their influence is primarily through discourse and policy recommendations.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, bioethicists_and_advocacy_groups, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for making difficult end-of-life decisions, aiming to coordinate medical practice, legal protections, and patient/family wishes in complex situations, particularly when patients lack capacity or face prolonged suffering.
% TRANSFER_FUNCTION: Transfers the authority to make life-ending decisions from the individual (in the autonomy reading) to proxies or the medical system, and shifts the criteria for such decisions from terminal illness to broader categories of suffering or incapacity.
% ABSENT_VOICES: Patients who become incompetent and whose prior wishes are ambiguous or unrecorded are effectively absent; their 'voice' is interpreted through the expanded framework, which may not align with their true interests. Future generations, who might inherit a system with significantly broadened end-of-life criteria, are also absent.
% DISAPPEARANCE_RATIONALE: If the expanded framework for end-of-life decisions vanished, healthcare systems would face immense legal and ethical uncertainty regarding incompetent and non-terminal patients. Decisions would revert to stricter interpretations, potentially leading to prolonged suffering for some and increased burdens on families and care providers, forcing a re-evaluation of existing practices.
% FOUNDING_PROBLEM: The original problem was to respect individual autonomy in the face of terminal illness and unbearable suffering, allowing competent adults to make choices about the timing and manner of their death.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the slippery slope mechanism argue that the original problem has been 'solved' for competent, terminal patients, but the framework has drifted to address other problems (resource management, chronic suffering) under the guise of autonomy. Opponents (autonomy advocates) argue the original problem is still live and the expansion is a necessary evolution. Bioethicists and legal scholars outside the immediate beneficiaries corroborate the drift in application and the expansion of criteria beyond the original intent.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) is high because the expanded framework allows for decisions that may not align with the true interests or past wishes of vulnerable patients, effectively extracting their remaining life or agency. Suppression (0.70) is also high, as the institutional and legal structures enforce these expanded criteria, making it difficult for individual patients or their advocates to resist. Theater ratio (0.20) is moderate, reflecting that while the original autonomy justification is still invoked, a growing portion of the framework's operation serves other, less explicit functions like resource management or a broader interpretation of 'suffering.' The increasing trend in extractiveness and suppression over time reflects the observed empirical expansion of these frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of healthcare systems, the expanded framework might appear as a necessary evolution to address complex cases and provide compassionate care, thus a coordination mechanism. From the perspective of vulnerable patients and their advocates, it is an extractive mechanism that leverages an initial coordination principle to justify broader interventions. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Healthcare systems and some families are beneficiaries, as the expanded framework provides clear pathways for difficult decisions, potentially easing burdens. Incompetent patients and non-terminal patients with chronic suffering are victims, as their agency is diminished or their lives are ended under criteria that have drifted from the original autonomy principle. Bioethicists and advocacy groups act as observers, analyzing and contesting the drift.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has drifted from its original function. While initially intended to empower individual autonomy, the 'slippery slope' mechanism suggests it has become a vehicle for other concerns (e.g., resource allocation, societal comfort with suffering) under the guise of autonomy. This prevents mislabeling it as pure coordination by highlighting the victims of this expanded application.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_for_slope,
    'Is the ''slippery slope'' an empirically demonstrable phenomenon (i.e., do expanded end-of-life criteria reliably lead to the inclusion of more vulnerable populations), or is it a theoretical concern?',
    'Longitudinal studies comparing jurisdictions with different end-of-life policies, tracking the demographics and conditions of patients receiving interventions over decades.',
    'Strong empirical evidence would validate this reading''s core premise, strengthening arguments for stricter safeguards. Lack of evidence would weaken this reading, pushing classification closer to the ''autonomy_reading'' for those who see the expansion as a natural evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_for_slope, empirical, 'Whether the ''slippery slope'' is an observed reality or a hypothetical risk.').

omega_variable(
    autonomy_vs_sanctity_framing,
    'To what extent does the expansion of end-of-life criteria, even if framed in terms of autonomy, implicitly serve underlying ''sanctity of life'' concerns (e.g., avoiding prolonged suffering at all costs, even if it means ending life prematurely for those who cannot consent)?',
    'Qualitative analysis of policy debates and medical discourse, examining the implicit values and justifications used when expanding criteria to vulnerable populations.',
    'If the expansion is found to be driven by implicit sanctity concerns, it would highlight a deeper ethical tension, potentially reclassifying the constraint as a more complex Tangled Rope or even Snare, where the autonomy narrative is a cover for other values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_framing, conceptual, 'The true ethical grounding of expanded end-of-life criteria.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, institutional protocols) or internalized (patients/families feeling societal pressure to choose end-of-life options for ''dignity'' or to avoid ''burden'')?',
    'Post-exit suppression trajectory: if pressure persists after legal/institutional barriers are removed, reclassify as partially internalized. Qualitative studies on patient/family decision-making processes.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true ''choice'' more elusive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in end-of-life decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.12).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.15).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.18).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 40, 0.19).
narrative_ontology:measurement(end__tr_t50, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(end__be_t50, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(end__su_t50, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
