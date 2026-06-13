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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Boundary (Potential-Based Reading)
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint defines personhood based on the potential for rational
 *   agency, a reading of the broader 'personhood_boundary' kernel. It grants
 *   parents and medical professionals the authority to make critical
 *   decisions, including withdrawal of care, for severely disabled infants
 *   deemed to lack this potential. The constraint is actively enforced
 *   through medical protocols and legal precedents, leading to substantial
 *   extraction from the infants themselves, who are denied full moral
 *   standing. The claimed type is 'tangled_rope' because it attempts to
 *   coordinate difficult decisions while simultaneously extracting from a
 *   vulnerable population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.65).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.75).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Boundary (Potential-Based Reading)").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '5c6d7f1c-846e-43bd-a157-37a89aad1873').
narrative_ontology:cs_kernel_codification('5c6d7f1c-846e-43bd-a157-37a89aad1873', distributed).
narrative_ontology:cs_authority_grounding('5c6d7f1c-846e-43bd-a157-37a89aad1873', expertise).
narrative_ontology:cs_interpretation_layer_present('5c6d7f1c-846e-43bd-a157-37a89aad1873').
narrative_ontology:cs_reading_relation('5c6d7f1c-846e-43bd-a157-37a89aad1873', personhood_boundary__fitness_contingent_reading, influences).
narrative_ontology:cs_reading_relation('5c6d7f1c-846e-43bd-a157-37a89aad1873', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_axiom('5c6d7f1c-846e-43bd-a157-37a89aad1873', foundational, potential_for_rational_agency_confers_personhood).
narrative_ontology:cs_axiom_status(potential_for_rational_agency_confers_personhood, holdable).
narrative_ontology:cs_axiom_grounding('5c6d7f1c-846e-43bd-a157-37a89aad1873', potential_for_rational_agency_confers_personhood, deontological).
narrative_ontology:cs_axiom('5c6d7f1c-846e-43bd-a157-37a89aad1873', secondary, absence_of_potential_justifies_differential_treatment).
narrative_ontology:cs_axiom_status(absence_of_potential_justifies_differential_treatment, holdable).
narrative_ontology:cs_axiom_grounding('5c6d7f1c-846e-43bd-a157-37a89aad1873', absence_of_potential_justifies_differential_treatment, instrumental).
narrative_ontology:cs_reference_frame('5c6d7f1c-846e-43bd-a157-37a89aad1873', potential_based_moral_standing).
narrative_ontology:cs_drift_state('5c6d7f1c-846e-43bd-a157-37a89aad1873', contemporary_disability_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5c6d7f1c-846e-43bd-a157-37a89aad1873', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_and_guardians).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, medical_professionals).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Granted significant authority to make life-and-death decisions for infants deemed to lack potential for rational agency, often relieving them of long-term care burdens and emotional distress. This reading provides a moral framework for such decisions.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_and_guardians, beneficiary,
    powerful, biographical, constrained, local).

% Act as gatekeepers, assessing an infant's potential for rational agency and advising parents on the moral and legal implications. This reading empowers them to make critical judgments about an infant's moral standing and treatment options, including withdrawal of life support.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, medical_professionals, agenda_setter,
    institutional, generational, constrained, national).

% Are the primary targets of this constraint. If deemed to lack potential for rational agency, they may be denied full moral standing, leading to decisions about their care that prioritize the interests of others over their continued existence or well-being. They have no agency to resist.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Actively challenge the potential-based reading, arguing that all human life has inherent value regardless of cognitive potential. They advocate for full moral standing and rights for severely disabled individuals, but their voices are often marginalized in medical and parental decision-making processes under this framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, advocacy_groups_for_disabled, excluded,
    organized, generational, constrained, national).

% Provides the framework for adjudicating disputes and setting precedents regarding personhood and parental/medical authority. It enforces the boundaries of this reading through case law and regulatory guidance, often deferring to medical expertise.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for making difficult ethical and medical decisions regarding the care and moral standing of severely disabled infants, coordinating the actions of parents, medical professionals, and the legal system.
% TRANSFER_FUNCTION: Transfers the burden of care and decision-making authority from the infant (who is denied full standing) to parents and medical professionals, allowing for interventions or non-interventions that would be impermissible for a 'person'.
% ABSENT_VOICES: Severely disabled infants themselves are inherently absent. Advocacy groups for the disabled are often excluded from direct decision-making processes, despite their strong moral objections to the potential-based criteria.
% DISAPPEARANCE_RATIONALE: If this reading of personhood vanished, the legal and ethical landscape for severely disabled infants would fundamentally shift. Medical and parental decisions would require a different moral justification, likely leading to increased legal challenges and a re-evaluation of care protocols, potentially extending life support or intensive care to all infants regardless of perceived potential.
% FOUNDING_PROBLEM: The problem of how to ethically and legally manage the care of infants with severe disabilities, particularly those with conditions historically considered 'incompatible with life' or lacking future cognitive capacity, balancing parental burdens with medical resources and moral obligations.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethics committees and legal scholars outside the immediate beneficiaries continue to grapple with these complex cases, indicating the problem remains live. However, advocacy groups for the disabled contest the 'solution' offered by this reading, arguing it creates more problems than it solves by devaluing human life.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).

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
 *   Extractiveness is high (0.65) because the constraint directly impacts the life and death of individuals, denying them fundamental rights based on a perceived lack of potential. Suppression is also high (0.75) as the infants themselves cannot resist, and advocacy for them is often marginalized by the authority of medical and parental decision-makers. Theater ratio is low (0.1) as the decisions made under this framework are genuinely consequential, not merely performative. The slight decrease in extractiveness and suppression towards the end of the interval reflects increasing legal and ethical challenges from disability rights advocates, though the core framework remains.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of parents and medical professionals, this reading provides a necessary, albeit difficult, framework for compassionate decision-making in tragic circumstances. From the perspective of severely disabled infants and their advocates, it is a deeply unjust and extractive mechanism that devalues human life based on ability.
 *
 * DIRECTIONALITY LOGIC:
 *   Parents and medical professionals are beneficiaries/agenda-setters, gaining moral and legal authority to make decisions that alleviate burdens. Severely disabled infants are the clear victims, bearing the ultimate cost of denied personhood. Advocacy groups are excluded, their arguments often not directly shaping individual decisions within this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to resolve difficult ethical dilemmas remains live. However, the 'solution' it offers is increasingly contested, with critics arguing that it has drifted from a coordination function to an extractive one, where the 'coordination' of parental and medical authority comes at the cost of the infant's fundamental rights. The classification as a Tangled Rope reflects this hybrid nature, acknowledging the coordination problem while highlighting the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_assessment_objectivity,
    'How objectively and reliably can ''potential for rational agency'' be assessed, especially in early infancy and with complex disabilities?',
    'Development of standardized, intersubjectively verifiable neurological and developmental assessment criteria, or a consensus among medical and philosophical experts on the limits of such assessment.',
    'If assessment is found to be highly subjective or unreliable, the basis for denying personhood is weakened, increasing the effective extractiveness and suppression of the constraint. If objective criteria are established, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_assessment_objectivity, empirical, 'The reliability of assessing ''potential for rational agency''.').

omega_variable(
    moral_status_of_potential,
    'Is ''potential for rational agency'' a sufficient and necessary condition for full moral personhood, or do other criteria (e.g., being human, sentience) also confer moral status?',
    'Philosophical consensus on the foundational criteria for moral personhood, or a societal shift in ethical values that prioritizes different attributes.',
    'If other criteria are deemed sufficient, the victim set of this constraint expands, and its justification for denying standing collapses, reclassifying it as a Snare. If potential remains the dominant criterion, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_of_potential, conceptual, 'The philosophical grounding of personhood in potential.').

omega_variable(
    parental_medical_authority_scope,
    'To what extent should parental and medical authority override the inherent moral claims of an infant, even one deemed to lack potential for rational agency?',
    'Legal precedents that explicitly limit parental/medical authority in cases of severe disability, or a societal re-evaluation of the balance between individual autonomy and the state''s interest in protecting vulnerable life.',
    'If authority is significantly curtailed, the constraint''s suppression mechanism is weakened, and resistance from advocacy groups becomes more effective, potentially shifting the classification towards a more benign form or even dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_medical_authority_scope, preference, 'The scope of parental/medical authority in personhood decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1970, personhood_boundary__potential_based_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(pers_tr_t1985, personhood_boundary__potential_based_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(pers_tr_t2000, personhood_boundary__potential_based_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(pers_tr_t2010, personhood_boundary__potential_based_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(pers_tr_t2024, personhood_boundary__potential_based_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t1970, personhood_boundary__potential_based_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(pers_be_t1985, personhood_boundary__potential_based_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(pers_be_t2000, personhood_boundary__potential_based_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(pers_be_t2010, personhood_boundary__potential_based_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(pers_be_t2024, personhood_boundary__potential_based_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1970, personhood_boundary__potential_based_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(pers_su_t1985, personhood_boundary__potential_based_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(pers_su_t2000, personhood_boundary__potential_based_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(pers_su_t2010, personhood_boundary__potential_based_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(pers_su_t2024, personhood_boundary__potential_based_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'personhood_boundary' kernel. Its structural properties and classification are distinct from other readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
