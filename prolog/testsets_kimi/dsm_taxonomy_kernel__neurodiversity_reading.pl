% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Pathologization of Neurodivergence for Institutional Conformity
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction
 *
 * SUMMARY:
 *   This constraint story captures the neurodiversity reading of the DSM
 *   taxonomy kernel. It holds that DSM categories do not neutrally describe
 *   objective disease entities but instead pathologize natural neurological
 *   variationâparticularly autism and ADHDâto enforce conformity with
 *   institutional behavioral norms. The constraint extracts
 *   self-determination and legitimacy from neurodivergent individuals and
 *   transfers it to schools, employers, carceral systems, and the psychiatric
 *   profession, which benefit from predictable, medicalized populations. This
 *   is one of three readings of the dsm_taxonomy_kernel; siblings are the
 *   biomedical_reading (empirical disease-entity mapping) and
 *   critical_psychiatry_reading (pharma-market construction).
 *
 * KEY AGENTS:
 *   - neurodivergent_individuals: Primary target (powerless/trapped) â bears pathologization, denial of self-determination, coercive normalization
 *   - school_systems: Primary beneficiary (institutional/constrained) â benefits from compliant student populations and medicalized gatekeeping for services
 *   - employers: Primary beneficiary (powerful/constrained) â benefits from diagnostic cover for rejecting non-conforming workers
 *   - carceral_systems: Primary beneficiary (institutional/constrained) â benefits from medicalized behavioral control vocabulary
 *   - psychiatric_profession: Agenda-setter (institutional/arbitrage) â administers taxonomy, derives professional authority
 *   - neurodiversity_advocates: Excluded voice (organized/constrained) â offers alternative framing but excluded from revision governance
 *   - medical_insurance_systems: Secondary beneficiary (institutional/constrained) â requires DSM codes for reimbursement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.83).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.83).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Pathologization of Neurodivergence for Institutional Conformity").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'f2bb00c7-bd17-40c4-8760-c40882c20515').
narrative_ontology:cs_kernel_codification('f2bb00c7-bd17-40c4-8760-c40882c20515', fixed_text).
narrative_ontology:cs_authority_grounding('f2bb00c7-bd17-40c4-8760-c40882c20515', expertise).
narrative_ontology:cs_interpretation_layer_present('f2bb00c7-bd17-40c4-8760-c40882c20515').
narrative_ontology:cs_reading_relation('f2bb00c7-bd17-40c4-8760-c40882c20515', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('f2bb00c7-bd17-40c4-8760-c40882c20515', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('f2bb00c7-bd17-40c4-8760-c40882c20515', foundational, neurological_difference_is_natural_variation).
narrative_ontology:cs_axiom_status(neurological_difference_is_natural_variation, holdable).
narrative_ontology:cs_axiom_grounding('f2bb00c7-bd17-40c4-8760-c40882c20515', neurological_difference_is_natural_variation, empirically_contingent).
narrative_ontology:cs_axiom('f2bb00c7-bd17-40c4-8760-c40882c20515', foundational, self_determination_over_institutional_conformity).
narrative_ontology:cs_axiom_status(self_determination_over_institutional_conformity, holdable).
narrative_ontology:cs_axiom_grounding('f2bb00c7-bd17-40c4-8760-c40882c20515', self_determination_over_institutional_conformity, deontological).
narrative_ontology:cs_reference_frame('f2bb00c7-bd17-40c4-8760-c40882c20515', medical_model_neurological_normality).
narrative_ontology:cs_drift_state('f2bb00c7-bd17-40c4-8760-c40882c20515', contemporary_neurodiversity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2bb00c7-bd17-40c4-8760-c40882c20515', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, school_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, employers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, carceral_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, medical_insurance_systems).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, biomedical_model_of_psychiatry).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, behavioral_normality_as_institutional_baseline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive DSM diagnoses that frame natural neurological variation as disorder; subjected to coercive behavioral normalization, denial of self-determination, and institutional gatekeeping where accommodations require a medical label. Exit is limited because rejecting the diagnosis often means losing access to services, legal protections, and workplace accommodations.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, trapped, global).

% Use DSM categories to classify students, allocate special education resources, and justify behavioral compliance demands. Benefit from a medicalized vocabulary that transfers accommodation responsibility from the institution to the diagnosed individual.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, school_systems, beneficiary,
    institutional, generational, constrained, national).

% Rely on DSM diagnostic criteria to screen, manage, and exclude employees who deviate from productivity and social interaction norms. Benefit from diagnostic cover when denying accommodations or terminating non-conforming workers.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, employers, beneficiary,
    powerful, biographical, constrained, national).

% Use psychiatric taxonomy to manage incarcerated populations, pathologize defiance and noncompliance, and justify behavioral control regimes within detention without addressing institutional conditions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, carceral_systems, beneficiary,
    institutional, generational, constrained, national).

% Administers and revises the DSM through the American Psychiatric Association; professional authority, research funding, and clinical legitimacy depend on maintaining the diagnostic taxonomy as the legitimate framework for understanding neurological and behavioral difference.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession, agenda_setter,
    institutional, generational, arbitrage, global).

% Argue that autism, ADHD, and related profiles represent natural human variation rather than pathology. Structurally excluded from APA governance and diagnostic revision processes despite representing the population being diagnosed.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% Require DSM codes to authorize reimbursement for services, medications, and accommodations. Benefit from standardized diagnostic criteria that constrain care pathways to billable categories and reduce individualized case review.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, medical_insurance_systems, beneficiary,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, diffuse).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized medical language that allows schools, employers, and carceral systems to classify and manage neurological and behavioral variation without developing individualized accommodation frameworks.
% TRANSFER_FUNCTION: Moves self-determination, social legitimacy, and access to institutional accommodations from neurodivergent individuals to institutional systems, in exchange for diagnostic labels that gatekeep services and justify conformity demands.
% ABSENT_VOICES: Neurodivergent self-advocates and neurodiversity scholars who frame autism and ADHD as natural human variation are structurally excluded from APA governance and diagnostic revision processes.
% DISAPPEARANCE_RATIONALE: Without DSM categories, schools would lose the primary medicalized gatekeeping mechanism for special education services; employers would lack diagnostic cover for rejecting non-conforming workers; carceral systems would need new vocabularies for behavioral control; neurodivergent individuals would gain self-determination but lose the currently required diagnostic key to access most accommodations and services.
% FOUNDING_PROBLEM: Mid-20th century institutions lacked a common taxonomy to classify and manage cognitive and behavioral diversity across schools, asylums, and courts, creating unpredictability in institutional populations.
% FOUNDING_PROBLEM_CORROBORATION: Historians of psychiatry corroborate the original institutional management problem. Disability rights scholars and neurodiversity advocates outside the psychiatric profession contest that the problem required a medical taxonomy rather than accommodation and rights-based frameworks; no corroboration from outside the benefiting parties supports the current framing as still live.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.83, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.83) because the constraint extracts self-determination and social legitimacy from neurodivergent individuals by converting natural variation into pathology. Suppression is high (0.78) because the constraint persists through institutional gatekeeping (diagnosis required for services) and active exclusion of neurodiversity framings from diagnostic governance. Theater ratio is moderately high (0.68) because a substantial share of diagnostic activity performs institutional sorting rather than medical treatment. Accessibility collapse is high (0.75) because once an individual enters the diagnostic system, non-medical framings become inaccessible within institutional contexts. Resistance is moderate (0.45) because the neurodiversity movement has generated substantial pushback, preventing full collapse of alternatives. The measurement series runs on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The institutional beneficiary seats and the neurodivergent payer seats compute different types. From the institutional perspective, the DSM is a necessary coordination tool for managing diverse populations; from the neurodivergent perspective, it is an extractive enforcement mechanism. The engine computes this divergence from structural data. The psychiatric profession experiences a narrower gap than the other beneficiaries because its authority depends partly on the taxonomy's legitimacy, creating a partial capture dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   School systems, employers, and carceral systems are structural beneficiaries (low d, subsidized by the constraint's conformity production). Neurodivergent individuals are structural targets (high d, extraction amplified by trapped exit). The psychiatric profession sits near the beneficiary end (derives authority), though with a less extreme d than institutional beneficiaries because its professional credibility is partially dependent on the taxonomy's contested status. Medical insurance systems are passive beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring explicit victim identification and suppression metrics. A purely coordination-focused reading would miss the extraction; the neurodiversity reading explicitly names victims and the transfer of self-determination to institutional systems, satisfying the snare gate. The founding problem (institutional management of behavioral diversity) is contested, suggesting the constraint's persistence is not obviously tied to a live coordination failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint a reading of the DSM taxonomy kernel (neurodiversity reading) or an independent structural claim about psychiatric power?',
    'Comparison with sibling readings biomedical_reading and critical_psychiatry_reading; if the same structural facts are compatible with multiple readings, the constraint is kernel-dependent.',
    'Determines whether classification should route through commitment-system machinery or standalone extraction analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'This constraint is one reading of a contested kernel; sibling readings would change the victim/beneficiary structure.').

omega_variable(
    label_vs_accommodation_harm,
    'Does the extractive harm originate primarily from the diagnostic label itself (epistemic violence, self-concept damage) or from institutional refusal to accommodate without a medicalized gatekeeping document?',
    'Cross-cultural comparison of jurisdictions where accommodations are available without DSM diagnosis; measure outcomes for neurodivergent individuals who hold the identity without the diagnosis versus those with diagnosis in low-accommodation contexts.',
    'If harm is label-originated, the constraint is snare (taxonomy as weapon). If harm is accommodation-gatekeeping, the constraint is tangled_rope (genuine coordination function for resource allocation captured by institutional convenience).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(label_vs_accommodation_harm, empirical, 'Whether DSM categories are inherently harmful or only harmful as gatekeeping instruments.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression structural (institutional gatekeeping requires diagnosis for services) or internalized (neurodivergent individuals adopt the pathologized frame as identity)?',
    'Longitudinal study of individuals who exit diagnostic categories or reject labels: do accommodation needs and self-worth recover immediately (structural) or persist with delayed trajectory (internalized)?',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates as cognitive capture; if purely structural, it is an external enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in psychiatric taxonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_nd_tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dsm_nd_tr_t18, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(dsm_nd_tr_t28, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 28, 0.48).
narrative_ontology:measurement(dsm_nd_tr_t42, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 42, 0.58).
narrative_ontology:measurement(dsm_nd_tr_t58, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 58, 0.65).
narrative_ontology:measurement(dsm_nd_tr_t70, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 70, 0.68).

% Extraction over time
narrative_ontology:measurement(dsm_nd_be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm_nd_be_t18, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(dsm_nd_be_t28, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 28, 0.76).
narrative_ontology:measurement(dsm_nd_be_t42, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 42, 0.82).
narrative_ontology:measurement(dsm_nd_be_t58, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 58, 0.85).
narrative_ontology:measurement(dsm_nd_be_t70, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 70, 0.83).

% Suppression requirement over time
narrative_ontology:measurement(dsm_nd_su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(dsm_nd_su_t18, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(dsm_nd_su_t28, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 28, 0.68).
narrative_ontology:measurement(dsm_nd_su_t42, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 42, 0.76).
narrative_ontology:measurement(dsm_nd_su_t58, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 58, 0.8).
narrative_ontology:measurement(dsm_nd_su_t70, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 70, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dsm_taxonomy_kernel, which decomposes into structurally distinct claims: biomedical_reading (empirical disease-entity mapping), critical_psychiatry_reading (pharma-market construction), and neurodiversity_reading (institutional conformity enforcement). Each reading has different epsilon, stakeholders, and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
