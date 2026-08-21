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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood Boundary: Birth Threshold Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint defines personhood as beginning at birth, granting all
 *   born humans full moral and legal standing. It is a foundational principle
 *   in most modern legal systems and human rights frameworks, establishing a
 *   universal baseline for protection. While its primary function is moral
 *   coordination, it requires significant active enforcement through legal
 *   systems to prevent violations. This constraint instantiates the
 *   'birth_threshold_reading' of the 'personhood_boundary' kernel. Sibling
 *   readings include 'fitness_contingent_reading' and
 *   'potential_based_reading', which propose alternative, potentially
 *   exclusionary, criteria for personhood.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.15).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.9).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary: Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'c20fdc13-d4c2-4b7d-9d25-1c587a5479ab').
narrative_ontology:cs_kernel_codification('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', formalized).
narrative_ontology:cs_authority_grounding('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', lineage).
narrative_ontology:cs_interpretation_layer_present('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab').
narrative_ontology:cs_reading_relation('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', foundational, birth_is_sufficient_for_personhood).
narrative_ontology:cs_axiom_status(birth_is_sufficient_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', birth_is_sufficient_for_personhood, deontological).
narrative_ontology:cs_axiom('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', foundational, all_born_humans_have_equal_moral_standing).
narrative_ontology:cs_axiom_status(all_born_humans_have_equal_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', all_born_humans_have_equal_moral_standing, deontological).
narrative_ontology:cs_reference_frame('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', universal_human_rights_framework).
narrative_ontology:cs_drift_state('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c20fdc13-d4c2-4b7d-9d25-1c587a5479ab', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_humans).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, society).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As infants, they are the primary recipients of moral and legal standing, protection, and care under this constraint. They cannot exit this status.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_humans, beneficiary,
    powerless, biographical, trapped, universal).

% Benefits from a clear, universal moral baseline for human treatment, which underpins legal systems and social order. It also bears the collective responsibility for upholding this standard through its institutions.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, society, agenda_setter,
    institutional, generational, constrained, universal).

% Enforces the moral and legal status of born humans as persons, prosecuting violations (e.g., homicide) and establishing rights. It is the primary mechanism for upholding the constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Operate under strict ethical and legal obligations to preserve the life and well-being of born humans, regardless of their health status. This imposes significant professional and resource costs.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, medical_professionals, payer,
    moderate, biographical, constrained, local).

% Analyze, debate, and critique the foundations and implications of personhood criteria, including the birth threshold. Their work influences, but does not directly set, the legal and social constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, philosophers_ethicists, observer,
    analytical, civilizational, analytical, global).

% Propose personhood criteria other than birth (e.g., cognitive capacity, potential for agency) that would exclude some born humans from full moral standing. Their views are not enshrined in the dominant legal and moral framework this constraint represents.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, advocates_for_alternative_personhood, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal, and non-arbitrary moral and legal baseline for the treatment of all born human beings, preventing arbitrary discrimination and ensuring their protection and rights within society.
% TRANSFER_FUNCTION: Transfers moral and legal obligations from society and individuals to all born humans, granting them inherent rights and protections. It also directs societal resources towards their care and well-being.
% ABSENT_VOICES: Advocates for personhood criteria contingent on demonstrated fitness or potential for rational agency are excluded from the legal and widely accepted moral framework that defines personhood at birth. They would argue for a more nuanced, but potentially exclusionary, approach.
% DISAPPEARANCE_RATIONALE: If the constraint that personhood begins at birth vanished overnight, the foundational legal and moral framework for human rights, protection of infants, and the concept of homicide would collapse, leading to profound societal reorganization and ethical chaos.
% FOUNDING_PROBLEM: To establish a clear, universal, and non-arbitrary criterion for moral and legal personhood, ensuring the protection and rights of all human beings once they are physically present and separate from the mother, preventing infanticide and arbitrary discrimination.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law (e.g., Universal Declaration of Human Rights, Convention on the Rights of the Child), national constitutions, and widespread social norms corroborate the ongoing need for a universal personhood criterion. While philosophical debates exist, the legal and social imperative for protecting all born humans remains strong, attested by legal scholars and human rights organizations outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).
:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the constraint primarily grants status and protection, rather than extracting resources in an asymmetric way. The 'costs' are diffuse societal obligations. Suppression is high (0.90) due to the robust legal and social enforcement against harming born humans. Theater ratio is very low (0.05) as the commitment to protecting born humans is genuine and deeply embedded in societal structures. Accessibility collapse is high (0.85) because legal and social alternatives to granting personhood at birth are largely foreclosed in most jurisdictions. Resistance is low (0.10) as the core principle is widely accepted, despite ongoing philosophical debates about its precise grounding.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of born humans and society, this constraint is a fundamental 'rope' that provides essential coordination and protection. From the perspective of those advocating for alternative personhood criteria, it might be seen as a 'snare' that arbitrarily includes entities they believe lack the necessary attributes for full personhood, imposing undue obligations.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans are full beneficiaries, gaining moral and legal status. Society and legal systems act as agenda-setters and beneficiaries, gaining a stable moral order and clear legal framework. Medical professionals are payers, bearing the ethical and legal obligations of care. Advocates for alternative personhood criteria are excluded, as their views are not incorporated into the dominant legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_threshold_ambiguity,
    'Is the birth threshold for personhood a natural moral truth, a social convention, or an instrumental choice for societal stability?',
    'Deep philosophical and ethical inquiry, potentially informed by neuroscientific understanding of consciousness, though ultimate resolution may remain conceptual.',
    'If a natural moral truth, the constraint''s ''mountain'' aspect would be strengthened. If a convention or instrumental choice, its ''rope'' classification would be reinforced, highlighting its constructed nature and potential for revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_threshold_ambiguity, conceptual, 'The fundamental nature of the birth threshold for personhood.').

omega_variable(
    alternative_personhood_impact,
    'What would be the societal and ethical impact if a ''fitness_contingent_reading'' or ''potential_based_reading'' of personhood were legally adopted?',
    'Detailed ethical thought experiments, legal analysis of potential consequences for vulnerable populations, and comparative studies of historical or theoretical societies with such criteria.',
    'If such readings led to the exclusion of some born humans from legal protection, the ''victims'' set of the ''personhood_boundary'' kernel would change dramatically, and the overall extractiveness of the system (from those excluded) would increase, likely shifting the classification of the dominant reading towards a ''snare'' for those newly excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_personhood_impact, empirical, 'Impact of adopting alternative personhood criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1900, personhood_boundary__birth_threshold_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(pers_tr_t1930, personhood_boundary__birth_threshold_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(pers_tr_t1960, personhood_boundary__birth_threshold_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(pers_tr_t1990, personhood_boundary__birth_threshold_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(pers_tr_t2024, personhood_boundary__birth_threshold_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(pers_be_t1900, personhood_boundary__birth_threshold_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(pers_be_t1930, personhood_boundary__birth_threshold_reading, base_extractiveness, 1930, 0.13).
narrative_ontology:measurement(pers_be_t1960, personhood_boundary__birth_threshold_reading, base_extractiveness, 1960, 0.14).
narrative_ontology:measurement(pers_be_t1990, personhood_boundary__birth_threshold_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(pers_be_t2024, personhood_boundary__birth_threshold_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1900, personhood_boundary__birth_threshold_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(pers_su_t1930, personhood_boundary__birth_threshold_reading, suppression_requirement, 1930, 0.87).
narrative_ontology:measurement(pers_su_t1960, personhood_boundary__birth_threshold_reading, suppression_requirement, 1960, 0.89).
narrative_ontology:measurement(pers_su_t1990, personhood_boundary__birth_threshold_reading, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(pers_su_t2024, personhood_boundary__birth_threshold_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
