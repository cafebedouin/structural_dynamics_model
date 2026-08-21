% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood Boundary: Birth Threshold Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'birth threshold' reading of the
 *   personhood boundary kernel. It asserts that personhood, and thus full
 *   moral and legal standing, begins universally at the moment of birth for
 *   all humans. This reading is widely embedded in legal systems and ethical
 *   frameworks, treating it as a foundational, almost natural, principle. The
 *   metrics reflect its near-universal acceptance and the minimal extraction
 *   it imposes on those it governs, primarily through the costs of universal
 *   care.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.05).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.95).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary: Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'b66456dc-7054-4f8f-8664-37363ac15aa8').
narrative_ontology:cs_kernel_codification('b66456dc-7054-4f8f-8664-37363ac15aa8', formalized).
narrative_ontology:cs_authority_grounding('b66456dc-7054-4f8f-8664-37363ac15aa8', lineage).
narrative_ontology:cs_interpretation_layer_present('b66456dc-7054-4f8f-8664-37363ac15aa8').
narrative_ontology:cs_reading_relation('b66456dc-7054-4f8f-8664-37363ac15aa8', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('b66456dc-7054-4f8f-8664-37363ac15aa8', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('b66456dc-7054-4f8f-8664-37363ac15aa8', foundational, all_born_humans_possess_inherent_moral_standing).
narrative_ontology:cs_axiom_status(all_born_humans_possess_inherent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('b66456dc-7054-4f8f-8664-37363ac15aa8', all_born_humans_possess_inherent_moral_standing, deontological).
narrative_ontology:cs_axiom('b66456dc-7054-4f8f-8664-37363ac15aa8', foundational, birth_is_the_universal_and_non_arbitrary_threshold_for_personhood).
narrative_ontology:cs_axiom_status(birth_is_the_universal_and_non_arbitrary_threshold_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('b66456dc-7054-4f8f-8664-37363ac15aa8', birth_is_the_universal_and_non_arbitrary_threshold_for_personhood, conventional).
narrative_ontology:cs_reference_frame('b66456dc-7054-4f8f-8664-37363ac15aa8', universal_human_rights_framework).
narrative_ontology:cs_drift_state('b66456dc-7054-4f8f-8664-37363ac15aa8', contemporary_bioethics_debates, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b66456dc-7054-4f8f-8664-37363ac15aa8', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_humans).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, legal_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, advocates_for_disability_rights).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, medical_professionals).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, families_of_severely_disabled_infants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All born humans are granted full moral and legal standing, protecting them from arbitrary harm and ensuring their rights. This status is inherent and not contingent on any further criteria.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_humans, beneficiary,
    powerless, biographical, identity_locked, universal).

% Legal systems universally recognize birth as the threshold for personhood, structuring laws around this principle (e.g., homicide laws, rights protections). They enforce this boundary, treating all born humans as subjects of law.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, global).

% Are bound by ethical and legal obligations to treat all born humans with full moral standing, regardless of their health status or developmental stage. This can impose significant resource and emotional costs in cases of severe disability.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, medical_professionals, payer,
    moderate, immediate, constrained, local).

% Bear the full responsibility and emotional burden of caring for severely disabled infants, who are granted full personhood and require extensive, often lifelong, support. Their options are limited by the legal and moral imperative to sustain life.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, families_of_severely_disabled_infants, payer,
    powerless, biographical, identity_locked, local).

% Benefit from the universal recognition of personhood at birth, as it provides a strong foundation for advocating for the rights and inclusion of all individuals, regardless of ability. This reading aligns with their core mission.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, advocates_for_disability_rights, beneficiary,
    organized, generational, mobile, national).

% Are excluded from the dominant legal and ethical discourse that universally grants personhood at birth. They would argue for a more nuanced approach where personhood is contingent on certain capacities, which would alter the moral standing of some born humans.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, proponents_of_fitness_contingent_personhood, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, unambiguous, and universally applicable boundary for moral and legal personhood, simplifying legal frameworks and ensuring consistent treatment of all born human beings.
% TRANSFER_FUNCTION: Transfers full moral and legal standing to all born humans, ensuring their protection and rights, while transferring the costs of care and protection to society and families, particularly for those with severe disabilities.
% ABSENT_VOICES: Proponents of fitness-contingent personhood are excluded; they would argue that the universal birth threshold imposes undue burdens and misallocates resources by granting full standing to entities lacking certain capacities. Their arguments are largely marginalized in mainstream legal and ethical discourse.
% DISAPPEARANCE_RATIONALE: If the birth threshold for personhood vanished, legal systems would face immediate chaos regarding homicide laws, rights protections, and medical ethics. The moral status of infants, particularly those with severe disabilities, would become highly contested, leading to a fundamental reorganization of societal norms and legal structures.
% FOUNDING_PROBLEM: To establish a clear, non-arbitrary, and universally recognizable point at which a human being gains full moral and legal standing, preventing arbitrary deprivation of life and ensuring consistent rights protection.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and medical ethicists universally corroborate the ongoing need for a clear personhood boundary to prevent arbitrary harm and ensure consistent legal application. While the specific threshold is debated, the problem of defining personhood remains live.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, ExtMetricName, E),
    domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that this constraint primarily grants rights and protections, with costs being diffuse societal obligations rather than concentrated extraction. Suppression (0.95) is high because the principle is deeply ingrained and actively enforced by legal systems, with very little tolerance for deviation. Theater ratio (0.01) is negligible as the function is direct and universally applied. Accessibility collapse (0.98) is near total, as alternatives to this boundary are largely foreclosed in mainstream discourse. Resistance (0.02) is minimal, as the principle is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   While the constraint is largely seen as a 'mountain' by legal systems and human rights advocates, those bearing the direct costs of universal care for severely disabled infants may experience it as a 'rope' or even a 'tangled rope' due to the significant burdens and lack of alternatives. However, the structural classification from the analytical seat remains Mountain due to its universal, fixed nature and low base extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans are direct beneficiaries, gaining universal rights and protections. Legal systems are agenda-setters, enforcing this boundary. Medical professionals and families of severely disabled infants bear costs due to the universal obligation to sustain life, regardless of prognosis. Proponents of fitness-contingent personhood are excluded, as their views challenge the universality of the birth threshold.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the universal personhood at birth a genuine natural law, or a deeply entrenched social construct that benefits identifiable agents (e.g., legal systems, human rights frameworks)?',
    'Comparative historical analysis of societies with different personhood thresholds, examining the emergence and persistence of the birth threshold in diverse cultural and legal contexts.',
    'If primarily a social construct, the constraint''s ''mountain'' classification would be re-evaluated as a ''false summit'' (tangled_rope), highlighting the constructed nature of its universality and the beneficiaries of that construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between natural law and social construct for personhood at birth.').

omega_variable(
    cost_burden_distribution,
    'Is the distribution of costs associated with universal personhood (e.g., care for severely disabled infants) equitable, or does it disproportionately burden specific families and medical systems?',
    'Socioeconomic analysis of healthcare expenditures and family support systems for individuals with severe disabilities, comparing outcomes across different policy regimes.',
    'If costs are found to be inequitably distributed, the ''low extractiveness'' metric might be re-evaluated from the perspective of the most burdened parties, potentially shifting the constraint towards a ''tangled rope'' for those seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_burden_distribution, empirical, 'Equity of cost distribution for universal personhood.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (near-universal acceptance of birth as personhood) structural (legal barriers, medical ethics) or internalized (deeply held moral beliefs, identity fusion with ''humanity'')?',
    'Post-exit suppression trajectory: if challenges to the birth threshold persist after legal barriers are removed (e.g., in philosophical debates), reclassify as partially internalized. If challenges are quickly suppressed by legal/institutional means, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the moral imperative is carried by individuals, making deviation unthinkable even without explicit legal enforcement. This reinforces its ''mountain'' character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for personhood at birth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(pers_tr_t25, personhood_boundary__birth_threshold_reading, theater_ratio, 25, 0.01).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__birth_threshold_reading, theater_ratio, 50, 0.01).
narrative_ontology:measurement(pers_tr_t75, personhood_boundary__birth_threshold_reading, theater_ratio, 75, 0.01).
narrative_ontology:measurement(pers_tr_t100, personhood_boundary__birth_threshold_reading, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(pers_be_t25, personhood_boundary__birth_threshold_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__birth_threshold_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(pers_be_t75, personhood_boundary__birth_threshold_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(pers_be_t100, personhood_boundary__birth_threshold_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(pers_su_t25, personhood_boundary__birth_threshold_reading, suppression_requirement, 25, 0.95).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__birth_threshold_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(pers_su_t75, personhood_boundary__birth_threshold_reading, suppression_requirement, 75, 0.95).
narrative_ontology:measurement(pers_su_t100, personhood_boundary__birth_threshold_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
