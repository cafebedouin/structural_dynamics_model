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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Boundary: Potential for Rational Agency Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents a reading of the personhood boundary where
 *   moral standing is contingent on the potential for rational agency. It
 *   primarily affects severely disabled infants, whose personhood may be
 *   denied based on medical assessments, granting parents and medical
 *   authorities significant power over their fate. This reading is contested
 *   by universal personhood advocates and stands in contrast to other
 *   readings of the personhood kernel.
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
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, snare).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Boundary: Potential for Rational Agency Reading").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '603354cc-e4a2-4b67-b82b-569df1b4aa9d').
narrative_ontology:cs_kernel_codification('603354cc-e4a2-4b67-b82b-569df1b4aa9d', implicit).
narrative_ontology:cs_authority_grounding('603354cc-e4a2-4b67-b82b-569df1b4aa9d', practice).
narrative_ontology:cs_interpretation_layer_present('603354cc-e4a2-4b67-b82b-569df1b4aa9d').
narrative_ontology:cs_reading_relation('603354cc-e4a2-4b67-b82b-569df1b4aa9d', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('603354cc-e4a2-4b67-b82b-569df1b4aa9d', personhood_boundary__fitness_contingent_reading, influences).
narrative_ontology:cs_axiom('603354cc-e4a2-4b67-b82b-569df1b4aa9d', foundational, rational_agency_potential_confers_personhood).
narrative_ontology:cs_axiom_status(rational_agency_potential_confers_personhood, holdable).
narrative_ontology:cs_axiom_grounding('603354cc-e4a2-4b67-b82b-569df1b4aa9d', rational_agency_potential_confers_personhood, deontological).
narrative_ontology:cs_axiom('603354cc-e4a2-4b67-b82b-569df1b4aa9d', secondary, absence_of_potential_justifies_exclusion).
narrative_ontology:cs_axiom_status(absence_of_potential_justifies_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('603354cc-e4a2-4b67-b82b-569df1b4aa9d', absence_of_potential_justifies_exclusion, instrumental).
narrative_ontology:cs_reference_frame('603354cc-e4a2-4b67-b82b-569df1b4aa9d', enlightenment_rationality_framework).
narrative_ontology:cs_drift_state('603354cc-e4a2-4b67-b82b-569df1b4aa9d', contemporary_disability_rights_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('603354cc-e4a2-4b67-b82b-569df1b4aa9d', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_medical_authorities).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, resource_allocators).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, advocates_for_universal_personhood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors are empowered to make judgments about an infant's potential for rational agency, which determines their moral standing. They benefit from the flexibility and reduced burden of care for infants deemed not to possess personhood, but also bear the moral weight of such decisions.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_medical_authorities, agenda_setter,
    institutional, biographical, constrained, local).

% These infants are the primary targets of this constraint. Their moral standing, and thus their right to life and care, is contingent on an assessment of their potential for rational agency. If deemed to lack this potential, they may be denied life-sustaining treatment or full legal protections.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% These entities (e.g., healthcare systems, social welfare programs) benefit from the reduced demand on resources when severely disabled infants are excluded from full personhood. This allows for reallocation of resources to other areas, but they face public scrutiny for such policies.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, resource_allocators, beneficiary,
    institutional, generational, mobile, national).

% These groups actively resist the potential-based reading, arguing for universal personhood regardless of capacity or potential. They bear the cost of continuous advocacy and moral struggle against a system that denies standing to vulnerable populations.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, advocates_for_universal_personhood, payer,
    organized, generational, constrained, global).

% Legal systems interpret and apply personhood definitions, often reflecting prevailing moral philosophies. They observe the contestation and may codify or challenge the potential-based reading through case law and legislation.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, legal_systems, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for making difficult ethical and medical decisions regarding the care and treatment of severely disabled infants, particularly concerning life-sustaining interventions and resource allocation.
% TRANSFER_FUNCTION: Transfers the burden of care and resource allocation away from infants deemed to lack potential for rational agency, to other areas or individuals, while transferring moral authority for these decisions to parents and medical professionals.
% ABSENT_VOICES: The severely disabled infants themselves are inherently absent from the conversation. Future generations who might develop more inclusive understandings of personhood are also absent, as are those who would argue for a 'precautionary principle' regarding moral standing.
% DISAPPEARANCE_RATIONALE: If this reading of personhood vanished, medical and parental decisions for severely disabled infants would be radically altered, likely leading to universal life-sustaining care where medically possible, regardless of potential. Resource allocation in healthcare would shift dramatically, and the moral landscape of end-of-life care for infants would be fundamentally reshaped.
% FOUNDING_PROBLEM: The problem of allocating scarce medical resources and making difficult ethical decisions for infants with profound disabilities, where the prognosis for a 'meaningful life' (often implicitly tied to rational agency) is extremely poor.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethicists and some parent groups attest that the problem of making difficult decisions for severely disabled infants remains live. Advocates for universal personhood acknowledge the difficulty but dispute the solution, arguing that the 'problem' is framed to justify exclusion, not to genuinely resolve ethical dilemmas.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is high because it denies fundamental rights and care to a vulnerable population based on a contingent assessment. Suppression (0.78) is severe due to the inherent powerlessness of the affected infants and the institutional backing of the decision-makers. Theater ratio is low (0.1) as the decisions made under this reading are generally direct and consequential, with little performative cover for other functions. Resistance is high (0.8) due to strong advocacy from disability rights groups and universal personhood proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of parents and medical authorities, this reading offers a necessary framework for difficult decisions and resource management. From the perspective of the infants and their advocates, it is a deeply extractive and suppressive mechanism that denies inherent moral worth. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parents and medical authorities act as agenda-setters and beneficiaries, gaining moral and practical flexibility, while severely disabled infants are the primary victims, losing fundamental rights. Resource allocators also benefit from reduced demand. Advocates for universal personhood bear the costs of resistance and moral struggle. Legal systems observe and mediate the contestation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_assessment_objectivity,
    'How objectively and reliably can ''potential for rational agency'' be assessed, especially in early infancy?',
    'Longitudinal studies tracking outcomes of infants initially deemed to lack potential, and inter-rater reliability studies among medical professionals making such assessments.',
    'If assessment is highly subjective or unreliable, the constraint''s extractiveness and suppression are amplified, as exclusion becomes arbitrary. If objective, the constraint''s justification gains empirical ground, though its ethical implications remain contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_assessment_objectivity, empirical, 'Uncertainty regarding the objectivity of assessing potential for rational agency.').

omega_variable(
    moral_status_grounding,
    'Is moral status fundamentally contingent on capacities (actual or potential), or is it an inherent property of being human?',
    'Philosophical consensus shifts, or a societal re-evaluation of foundational ethical principles regarding vulnerability and inherent worth.',
    'If moral status is inherent, this reading of personhood is fundamentally flawed and highly extractive. If contingent, the debate shifts to which capacities are relevant and how they are assessed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_grounding, conceptual, 'Fundamental conceptual disagreement on the grounding of moral status.').

omega_variable(
    resource_allocation_justification,
    'To what extent is the potential-based reading driven by genuine ethical considerations versus resource scarcity and societal burden?',
    'Economic analysis of healthcare systems with and without this personhood reading, and qualitative studies of decision-making processes in medical ethics committees.',
    'If primarily driven by resource allocation, the constraint''s extractive nature is more pronounced and its coordination function is revealed as a cover for economic efficiency. If genuinely ethical, its classification might shift towards a more complex tangled rope, acknowledging a coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_justification, empirical, 'Ambiguity regarding the underlying motivations for the potential-based personhood reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1970, personhood_boundary__potential_based_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(pers_tr_t1985, personhood_boundary__potential_based_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(pers_tr_t2000, personhood_boundary__potential_based_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(pers_tr_t2010, personhood_boundary__potential_based_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(pers_tr_t2024, personhood_boundary__potential_based_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t1970, personhood_boundary__potential_based_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(pers_be_t1985, personhood_boundary__potential_based_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(pers_be_t2000, personhood_boundary__potential_based_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(pers_be_t2010, personhood_boundary__potential_based_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(pers_be_t2024, personhood_boundary__potential_based_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1970, personhood_boundary__potential_based_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(pers_su_t1985, personhood_boundary__potential_based_reading, suppression_requirement, 1985, 0.73).
narrative_ontology:measurement(pers_su_t2000, personhood_boundary__potential_based_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(pers_su_t2010, personhood_boundary__potential_based_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(pers_su_t2024, personhood_boundary__potential_based_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, resource_allocation_in_healthcare).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, disability_rights_legislation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
