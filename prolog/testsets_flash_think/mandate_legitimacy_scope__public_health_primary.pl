% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Authority to Compel Vaccination (Public Health Primary Reading)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of state
 *   authority to compel vaccination, asserting its legitimacy when necessary
 *   to protect vulnerable populations from serious harm. From this
 *   perspective, the mandate is a necessary and justified public health
 *   intervention, with the burden on unvaccinated individuals viewed as a
 *   legitimate cost for collective protection. The constraint is classified
 *   as a Tangled Rope due to its clear coordination function (protecting
 *   public health) and asymmetric extraction (compulsion of individuals).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.55).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.7).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Authority to Compel Vaccination (Public Health Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'f7809ef9-39c7-4188-ad4c-bc95aa989550').
narrative_ontology:cs_kernel_codification('f7809ef9-39c7-4188-ad4c-bc95aa989550', formalized).
narrative_ontology:cs_authority_grounding('f7809ef9-39c7-4188-ad4c-bc95aa989550', lineage).
narrative_ontology:cs_interpretation_layer_present('f7809ef9-39c7-4188-ad4c-bc95aa989550').
narrative_ontology:cs_reading_relation('f7809ef9-39c7-4188-ad4c-bc95aa989550', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('f7809ef9-39c7-4188-ad4c-bc95aa989550', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('f7809ef9-39c7-4188-ad4c-bc95aa989550', foundational, collective_welfare_trumps_individual_liberty_in_public_health_crises).
narrative_ontology:cs_axiom_status(collective_welfare_trumps_individual_liberty_in_public_health_crises, holdable).
narrative_ontology:cs_axiom_grounding('f7809ef9-39c7-4188-ad4c-bc95aa989550', collective_welfare_trumps_individual_liberty_in_public_health_crises, deontological).
narrative_ontology:cs_axiom('f7809ef9-39c7-4188-ad4c-bc95aa989550', foundational, duty_to_protect_vulnerable_from_preventable_harm).
narrative_ontology:cs_axiom_status(duty_to_protect_vulnerable_from_preventable_harm, holdable).
narrative_ontology:cs_axiom_grounding('f7809ef9-39c7-4188-ad4c-bc95aa989550', duty_to_protect_vulnerable_from_preventable_harm, deontological).
narrative_ontology:cs_reference_frame('f7809ef9-39c7-4188-ad4c-bc95aa989550', public_health_imperative_framework).
narrative_ontology:cs_drift_state('f7809ef9-39c7-4188-ad4c-bc95aa989550', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f7809ef9-39c7-4188-ad4c-bc95aa989550', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_system).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting the health of the population, they assert and enforce the authority to compel vaccination when deemed necessary to prevent serious harm to vulnerable groups. They frame this as a core duty.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals (e.g., immunocompromised, infants) who cannot be vaccinated or for whom vaccines are less effective, and thus rely on herd immunity for protection. They are direct beneficiaries of the mandate's protective effect.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Individuals who are compelled to vaccinate against their will or preference. They bear the direct burden of the mandate, including potential side effects and perceived loss of bodily autonomy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Administer vaccines and counsel patients, often acting as the front line of mandate enforcement. They balance individual patient concerns with public health directives.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, healthcare_providers, agenda_setter,
    organized, biographical, constrained, local).

% Groups and individuals who argue that medical interventions, including vaccination, should always be voluntary and based on informed consent, regardless of collective benefit. Their core premise is foreclosed by this reading.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, bodily_autonomy_advocates, excluded,
    organized, biographical, mobile, national).

% Analyze the legal and ethical boundaries of state power, individual rights, and public health. They observe the contest between competing readings of state authority.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish collective immunity and reduce the transmission of serious infectious diseases, thereby protecting vulnerable populations who cannot benefit from individual vaccination.
% TRANSFER_FUNCTION: Transfers the risk and burden of vaccination from vulnerable populations (who would otherwise face severe illness) to unvaccinated individuals (who are compelled to participate in collective protection).
% ABSENT_VOICES: Bodily autonomy advocates are structurally excluded from the core premise of this reading, as their absolute claim to individual medical choice is deemed secondary to the collective duty to protect the vulnerable.
% DISAPPEARANCE_RATIONALE: If the state's authority to compel vaccination vanished, serious infectious diseases would likely spread more widely, leading to increased morbidity and mortality among vulnerable populations, overwhelming healthcare systems, and causing significant societal disruption.
% FOUNDING_PROBLEM: The historical problem of widespread infectious disease outbreaks causing mass mortality and morbidity, particularly among the vulnerable, before effective public health interventions like vaccination.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological data, historical public health records, and the consensus of independent medical and scientific bodies consistently attest to the ongoing threat of infectious diseases and the efficacy of vaccination in mitigating them.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the significant burden placed on individuals who are compelled to vaccinate, but it is not higher because, from this reading's perspective, this burden is a justified and necessary cost for a vital public good. Suppression (0.70) is high due to the coercive nature of mandates. Theater ratio (0.10) is low, as the constraint's function is direct and not primarily performative. Accessibility collapse (0.80) is high for unvaccinated individuals, as the option to remain unvaccinated is removed. Resistance (0.60) is moderate, reflecting ongoing opposition to such mandates.
 *
 * PERSPECTIVAL GAP:
 *   This reading prioritizes collective welfare and the protection of the vulnerable, viewing individual autonomy as secondary in specific public health crises. Other readings (e.g., bodily autonomy primary) would experience this constraint as pure extraction or a snare, highlighting the fundamental perspectival gap inherent in the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations are clear beneficiaries (d=0.0) as they receive protection. Unvaccinated individuals are targets (d=1.0) as they bear the direct cost of compulsion. State public health authorities and healthcare providers act as agenda-setters and enforcers, benefiting from the effective functioning of the public health system (d near beneficiary end).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_necessity_threshold,
    'What objective criteria define ''necessary to protect vulnerable populations from serious harm'' and are these criteria consistently applied?',
    'Independent epidemiological review, public health ethics panels, and judicial oversight to establish and verify the threshold conditions for mandate implementation.',
    'If the criteria are found to be inconsistently applied or overly broad, the perceived legitimacy and coordination function of the mandate would diminish, increasing its effective extraction and potentially reclassifying it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_necessity_threshold, empirical, 'Ambiguity in the conditions that trigger mandate legitimacy.').

omega_variable(
    mandate_legitimacy_kernel_reading,
    'This constraint is one reading of the ''mandate_legitimacy_scope'' kernel. How would the classification change under sibling readings?',
    'Analyze the ''bodily_autonomy_primary'' and ''proportionality_reading'' constraints (sibling files) to compare their structural properties and classifications.',
    'The ''bodily_autonomy_primary'' reading would likely classify the mandate as a Snare due to its absolute rejection of compelled medical intervention. The ''proportionality_reading'' would likely classify it as a Tangled Rope or Scaffold, but with a stronger emphasis on the context-dependent balancing of harms and benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_legitimacy_kernel_reading, conceptual, 'Impact of alternative kernel readings on constraint classification.').

omega_variable(
    extraction_from_mandate_absence,
    'What is the effective extraction from vulnerable populations if the mandate is absent?',
    'Epidemiological modeling of disease burden and healthcare system strain in the absence of mandates, focusing on outcomes for vulnerable groups.',
    'This reading asserts that the absence of the mandate leads to high extraction (harm) from vulnerable populations. Empirical corroboration of this counterfactual is crucial for the ''public_health_primary'' reading''s justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_from_mandate_absence, empirical, 'Quantifying harm to vulnerable populations in the absence of mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(mand_tr_t15, mandate_legitimacy_scope__public_health_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(mand_be_t15, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(mand_su_t15, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'mandate_legitimacy_scope' kernel. It focuses on the public health imperative, while 'bodily_autonomy_primary' and 'proportionality_reading' offer alternative framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
