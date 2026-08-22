% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Public Health Mandate Proportionality Principle
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   mandate legitimacy. It asserts that mandates are legitimate only when
 *   they are proportional to the public health threat, considering disease
 *   severity, vaccine safety/efficacy, and the availability of less
 *   restrictive alternatives. This reading aims to balance collective good
 *   with individual rights, making mandate legitimacy conditional rather than
 *   absolute. The metrics reflect a constraint that can be moderately
 *   extractive and suppressive, but whose legitimacy is actively contested
 *   and requires ongoing justification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.45).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.6).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Public Health Mandate Proportionality Principle").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '567a88ff-1a9b-4022-aaa5-df841427f17b').
narrative_ontology:cs_kernel_codification('567a88ff-1a9b-4022-aaa5-df841427f17b', formalized).
narrative_ontology:cs_authority_grounding('567a88ff-1a9b-4022-aaa5-df841427f17b', lineage).
narrative_ontology:cs_interpretation_layer_present('567a88ff-1a9b-4022-aaa5-df841427f17b').
narrative_ontology:cs_reading_relation('567a88ff-1a9b-4022-aaa5-df841427f17b', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('567a88ff-1a9b-4022-aaa5-df841427f17b', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('567a88ff-1a9b-4022-aaa5-df841427f17b', foundational, mandate_proportionality_required).
narrative_ontology:cs_axiom_status(mandate_proportionality_required, holdable).
narrative_ontology:cs_axiom_grounding('567a88ff-1a9b-4022-aaa5-df841427f17b', mandate_proportionality_required, deontological).
narrative_ontology:cs_axiom('567a88ff-1a9b-4022-aaa5-df841427f17b', secondary, least_restrictive_alternative_principle).
narrative_ontology:cs_axiom_status(least_restrictive_alternative_principle, holdable).
narrative_ontology:cs_axiom_grounding('567a88ff-1a9b-4022-aaa5-df841427f17b', least_restrictive_alternative_principle, instrumental).
narrative_ontology:cs_reference_frame('567a88ff-1a9b-4022-aaa5-df841427f17b', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('567a88ff-1a9b-4022-aaa5-df841427f17b', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('567a88ff-1a9b-4022-aaa5-df841427f17b', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, healthcare_providers).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, healthcare_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they issue mandates based on scientific evidence. Their legitimacy is tied to demonstrating proportionality and necessity.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals (e.g., immunocompromised, infants) who cannot be vaccinated or for whom vaccines are less effective, relying on herd immunity for protection. They benefit directly from mandates that increase community immunity.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Bear the direct cost of mandates, including vaccination, testing, or exclusion from certain activities. Their autonomy is constrained, but the constraint is justified by proportionality.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate, payer,
    moderate, immediate, constrained, local).

% Monitor mandates for overreach, advocating for individual rights and less restrictive alternatives. They challenge mandates that fail the proportionality test.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Administer vaccines and manage public health crises. They benefit from reduced disease burden but may bear administrative costs and face ethical dilemmas regarding individual autonomy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, healthcare_providers, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, healthcare_providers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual actions (vaccination) to achieve collective immunity, protecting vulnerable populations and reducing overall disease burden, but only when the intervention is proportional to the threat.
% TRANSFER_FUNCTION: Transfers a degree of individual medical autonomy from citizens to public health authorities, in exchange for collective health benefits, but only when the severity of the disease, vaccine efficacy/safety, and lack of alternatives justify it.
% ABSENT_VOICES: Individuals who would be disproportionately affected by mandates (e.g., those with rare medical contraindications, or those for whom the mandate imposes severe economic hardship without clear public health gain) are often marginalized in policy discussions.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, public health mandates could become arbitrary or overreaching, leading to widespread resistance, erosion of public trust, and potentially ineffective or harmful interventions. The balance between individual rights and collective good would be lost, reorganizing public health governance.
% FOUNDING_PROBLEM: How to balance individual liberty with collective health needs, ensuring that state interventions are justified and do not impose undue burdens.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars, bioethicists, and public health ethicists consistently affirm the ongoing challenge of balancing these values, with court cases and policy debates regularly invoking proportionality as a core principle. This corroboration comes from outside the direct beneficiaries of mandates.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).
:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because mandates impose real costs on individuals, but these are justified by collective benefits under this reading. Suppression (0.6) is also moderate, reflecting the active enforcement required for mandates, but tempered by the need to demonstrate proportionality. The theater ratio (0.1) is low, as the justification for mandates is generally genuine, though it can be challenged. The temporal measurements show a rise in extractiveness and suppression during periods of heightened public health crisis (e.g., 2010-2020), followed by a decrease as the immediate crisis subsides and proportionality is re-evaluated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this constraint is a necessary tool for coordination, ensuring collective well-being. From the perspective of individuals subject to mandates, it is an extractive force that curtails autonomy. The proportionality principle attempts to bridge this gap by setting conditions under which the extraction is deemed legitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters and beneficiaries, as they gain the ability to protect public health. Vulnerable populations are clear beneficiaries, relying on the mandates for protection. Individuals subject to mandates are payers, bearing the direct costs of compliance. Civil liberties advocates act as observers, challenging mandates that fail the proportionality test. Healthcare providers are both beneficiaries (reduced disease burden) and payers (administrative burden, ethical conflicts).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent mandatrophy by requiring ongoing justification based on proportionality. If the founding problem (balancing liberty and health) were to become 'dead' (e.g., if a disease became trivial or a vaccine unsafe), the proportionality principle would demand the mandate's removal, preventing it from becoming a piton or snare. The 'contested' status of the founding problem reflects the ongoing need for this analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How are ''disease severity'', ''vaccine safety/efficacy'', and ''less restrictive alternatives'' objectively measured and weighted to determine proportionality?',
    'Development of standardized, transparent, and publicly debated metrics and weighting schemes, potentially through independent expert panels or legislative processes.',
    'Lack of clear metrics allows for subjective interpretation, potentially leading to mandates that are either over- or under-restrictive, shifting the constraint''s effective extractiveness and suppression. Clear metrics would stabilize the constraint''s operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, conceptual, 'Ambiguity in the operationalization of proportionality criteria.').

omega_variable(
    shifting_social_contract,
    'Does the public''s willingness to accept health mandates shift over time, and how does this affect the perceived legitimacy and resistance to the proportionality principle?',
    'Longitudinal sociological studies and public opinion surveys tracking trust in institutions and attitudes towards collective action in health crises.',
    'A decline in public trust or willingness to comply could increase resistance and suppression requirements, potentially pushing the constraint towards a snare if enforcement becomes disproportionately coercive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shifting_social_contract, empirical, 'The dynamic nature of the social contract underlying public health interventions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t1900, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(mand_tr_t1950, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(mand_tr_t2000, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(mand_tr_t2010, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(mand_tr_t2020, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(mand_tr_t2024, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t1900, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(mand_be_t1950, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(mand_be_t2000, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(mand_be_t2010, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(mand_be_t2020, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(mand_be_t2024, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t1900, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(mand_su_t1950, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(mand_su_t2000, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(mand_su_t2010, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(mand_su_t2020, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(mand_su_t2024, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of readings for the 'mandate_legitimacy_scope' kernel. Its ε value is distinct from sibling readings because it conditions legitimacy on proportionality, leading to different victim sets and levels of extraction depending on the specific public health context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
