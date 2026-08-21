% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Boundary: Potential for Rational Agency Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents a reading of the personhood boundary kernel,
 *   specifically one that grounds personhood in the potential for rational
 *   agency. This allows for the exclusion of severely disabled infants from
 *   full moral standing, granting parents and medical authorities the power
 *   to make decisions that might otherwise be considered violations of
 *   universal personhood. The constraint is framed as a Tangled Rope due to
 *   its genuine coordination function (providing a framework for difficult
 *   decisions) alongside significant asymmetric extraction (from the infants
 *   themselves).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.65).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.78).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Boundary: Potential for Rational Agency Reading").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'e3f1f186-642d-4dd3-9532-8749c10d0dc9').
narrative_ontology:cs_kernel_codification('e3f1f186-642d-4dd3-9532-8749c10d0dc9', formalized).
narrative_ontology:cs_authority_grounding('e3f1f186-642d-4dd3-9532-8749c10d0dc9', lineage).
narrative_ontology:cs_interpretation_layer_present('e3f1f186-642d-4dd3-9532-8749c10d0dc9').
narrative_ontology:cs_reading_relation('e3f1f186-642d-4dd3-9532-8749c10d0dc9', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3f1f186-642d-4dd3-9532-8749c10d0dc9', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('e3f1f186-642d-4dd3-9532-8749c10d0dc9', foundational, rational_agency_potential_is_personhood_criterion).
narrative_ontology:cs_axiom_status(rational_agency_potential_is_personhood_criterion, holdable).
narrative_ontology:cs_axiom_grounding('e3f1f186-642d-4dd3-9532-8749c10d0dc9', rational_agency_potential_is_personhood_criterion, deontological).
narrative_ontology:cs_axiom('e3f1f186-642d-4dd3-9532-8749c10d0dc9', secondary, parental_medical_discretion_in_potential_assessment).
narrative_ontology:cs_axiom_status(parental_medical_discretion_in_potential_assessment, holdable).
narrative_ontology:cs_axiom_grounding('e3f1f186-642d-4dd3-9532-8749c10d0dc9', parental_medical_discretion_in_potential_assessment, conventional).
narrative_ontology:cs_reference_frame('e3f1f186-642d-4dd3-9532-8749c10d0dc9', enlightenment_rationalist_personhood).
narrative_ontology:cs_drift_state('e3f1f186-642d-4dd3-9532-8749c10d0dc9', contemporary_disability_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e3f1f186-642d-4dd3-9532-8749c10d0dc9', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_medical_authorities).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, resource_allocators).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors hold the authority to assess an infant's potential for rational agency and make life-and-death decisions, including withdrawal of care, based on that assessment. They benefit from the moral latitude this reading provides.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_medical_authorities, agenda_setter,
    institutional, biographical, constrained, local).

% These infants are the direct targets of this constraint, potentially being denied full moral standing and the protections that come with it, based on a judgment about their future capacities. They have no agency to resist or exit.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% These are healthcare systems and public policy makers who benefit from the potential for reduced resource allocation to individuals deemed outside the personhood boundary, freeing up resources for other priorities. They are not directly involved in individual assessments but benefit from the framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, resource_allocators, beneficiary,
    institutional, generational, mobile, national).

% These groups actively challenge the potential-based reading, arguing for universal personhood regardless of capacity or potential. They are often excluded from the initial decision-making process but exert pressure through legal and public advocacy.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_advocates, excluded,
    organized, generational, constrained, national).

% These academics analyze and debate the philosophical underpinnings and implications of different personhood criteria. They do not directly enforce or benefit from the constraint but shape the intellectual discourse around it.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, ethicists_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for making difficult ethical decisions regarding the moral status and care of severely disabled infants, aiming to balance compassion with resource realities and parental autonomy.
% TRANSFER_FUNCTION: Transfers moral authority and decision-making power regarding life-sustaining treatment from a universal standard of personhood to a judgment based on potential for rational agency, impacting the allocation of care and resources.
% ABSENT_VOICES: Severely disabled infants themselves are inherently voiceless. Disability advocates and those who hold a universal personhood view are often excluded from the direct decision-making process, only able to intervene post-facto or through systemic challenges.
% DISAPPEARANCE_RATIONALE: If this reading vanished, decisions regarding severely disabled infants would default to a universal personhood standard, requiring life-sustaining care unless specific, universally accepted criteria for withdrawal (e.g., brain death) are met. This would significantly alter medical practice, parental choices, and resource allocation.
% FOUNDING_PROBLEM: The problem of making difficult ethical decisions at the margins of life, particularly concerning infants with severe disabilities where the prognosis for a 'meaningful life' (often implicitly tied to rational agency) is poor, and where medical interventions can prolong suffering without hope of recovery.
% FOUNDING_PROBLEM_CORROBORATION: Medical professionals and some parents attest to the ongoing difficulty of these decisions, seeking frameworks that allow for compassionate withdrawal of care in extreme cases. Disability advocates and some ethicists contest the framing of the problem itself, arguing it implicitly devalues certain lives.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because it denies fundamental protections to a vulnerable group, allowing others to make life-altering decisions based on a contested criterion. Suppression (0.78) is also high, as the targets (infants) are utterly powerless, and external resistance from advocates is often marginalized in individual cases. The theater ratio is low (0.1) because the decisions made under this framework are often genuinely agonizing, not merely performative. The slight decrease in extractiveness and suppression towards the end of the interval reflects increased advocacy and legal challenges, making such decisions more scrutinized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of parents and medical authorities, this reading provides a necessary, albeit difficult, framework for compassionate decision-making in tragic circumstances. From the perspective of disability advocates and the infants themselves, it is a deeply extractive and suppressive mechanism that devalues certain lives. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parents and medical authorities are agenda-setters and beneficiaries, gaining moral and practical latitude. Resource allocators also benefit from potential cost savings. Severely disabled infants are the primary victims, bearing the full cost of this exclusion. Disability advocates are excluded voices, actively resisting the framework. Ethicists and philosophers act as observers, analyzing the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_assessment_objectivity,
    'How objectively and reliably can ''potential for rational agency'' be assessed, especially in early infancy?',
    'Longitudinal studies tracking outcomes of infants initially assessed as lacking potential, compared against those receiving full intervention, alongside inter-rater reliability studies of medical/ethical panels.',
    'If assessment is highly subjective or prone to bias, the constraint''s extractiveness and suppression are higher than measured, as the ''potential'' criterion becomes a discretionary gate. If objective, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_assessment_objectivity, empirical, 'Ambiguity in assessing potential for rational agency.').

omega_variable(
    personhood_kernel_framing,
    'Is the ''personhood_boundary'' kernel fundamentally about biological facts, philosophical criteria, or social recognition?',
    'Conceptual analysis of historical and cross-cultural definitions of personhood, and the role of social institutions in conferring or denying status.',
    'If primarily biological, this potential-based reading is less defensible. If primarily philosophical, its coherence depends on the strength of its arguments. If primarily social, its legitimacy depends on consensus and inclusion, which this reading often lacks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_kernel_framing, conceptual, 'The fundamental nature of the personhood kernel itself.').

omega_variable(
    sibling_reading_impact,
    'What would be the structural impact if the ''birth_threshold_reading'' or ''fitness_contingent_reading'' were universally adopted instead?',
    'Counterfactual analysis of legal and medical systems under alternative personhood definitions, focusing on changes in victim sets, beneficiary groups, and resource allocation.',
    'If ''birth_threshold_reading'' were adopted, the victim set of this constraint would vanish, and resource allocation would shift dramatically. If ''fitness_contingent_reading'' were adopted, the victim set could expand to include individuals who lose capacities later in life, making this reading less extractive by comparison.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact, conceptual, 'Impact of alternative personhood readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(pers_be_t1970, personhood_boundary__potential_based_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(pers_be_t1985, personhood_boundary__potential_based_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(pers_be_t2000, personhood_boundary__potential_based_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(pers_be_t2010, personhood_boundary__potential_based_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(pers_be_t2024, personhood_boundary__potential_based_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1970, personhood_boundary__potential_based_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(pers_su_t1985, personhood_boundary__potential_based_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(pers_su_t2000, personhood_boundary__potential_based_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(pers_su_t2010, personhood_boundary__potential_based_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(pers_su_t2024, personhood_boundary__potential_based_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
