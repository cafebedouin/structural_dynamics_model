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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   This constraint defines personhood as beginning at birth, granting full
 *   moral and legal standing to all born humans. It is presented as a
 *   foundational principle in many legal and ethical systems, ensuring
 *   universal protection regardless of cognitive capacity or developmental
 *   stage. This story instantiates one reading of the broader
 *   'personhood_boundary' kernel, focusing on the birth threshold as the
 *   definitive moment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.05).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.02).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary: Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'b5254a76-de88-4fc7-9784-e4248c5f148b').
narrative_ontology:cs_kernel_codification('b5254a76-de88-4fc7-9784-e4248c5f148b', formalized).
narrative_ontology:cs_authority_grounding('b5254a76-de88-4fc7-9784-e4248c5f148b', lineage).
narrative_ontology:cs_interpretation_layer_present('b5254a76-de88-4fc7-9784-e4248c5f148b').
narrative_ontology:cs_reading_relation('b5254a76-de88-4fc7-9784-e4248c5f148b', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('b5254a76-de88-4fc7-9784-e4248c5f148b', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('b5254a76-de88-4fc7-9784-e4248c5f148b', foundational, birth_as_universal_threshold).
narrative_ontology:cs_axiom_status(birth_as_universal_threshold, holdable).
narrative_ontology:cs_axiom_grounding('b5254a76-de88-4fc7-9784-e4248c5f148b', birth_as_universal_threshold, deontological).
narrative_ontology:cs_axiom('b5254a76-de88-4fc7-9784-e4248c5f148b', foundational, intrinsic_moral_worth_of_born_human).
narrative_ontology:cs_axiom_status(intrinsic_moral_worth_of_born_human, holdable).
narrative_ontology:cs_axiom_grounding('b5254a76-de88-4fc7-9784-e4248c5f148b', intrinsic_moral_worth_of_born_human, deontological).
narrative_ontology:cs_reference_frame('b5254a76-de88-4fc7-9784-e4248c5f148b', universal_human_rights_framework).
narrative_ontology:cs_drift_state('b5254a76-de88-4fc7-9784-e4248c5f148b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b5254a76-de88-4fc7-9784-e4248c5f148b', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, born_humans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, advocates_for_vulnerable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All born humans, regardless of cognitive capacity or developmental stage, are recognized as having full moral standing and legal protection. They 'pay' by being subject to the moral and legal obligations that come with personhood, but primarily benefit from its protections. This constraint ensures their right to life and protection from harm.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_humans, payer,
    powerless, biographical, trapped, universal).

% These systems codify and enforce the moral standing of born humans, defining homicide and other protections. They administer the legal framework that flows from this personhood definition, ensuring its universal application within their jurisdiction. They are constrained by the widely accepted moral intuition that personhood begins at birth.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, state_legal_systems, agenda_setter,
    institutional, generational, constrained, global).

% Academically analyze and debate the foundations and implications of personhood, including the birth threshold. Their work influences, but does not directly set, legal or societal norms. They observe the practical application and theoretical coherence of this constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, moral_philosophers, observer,
    analytical, civilizational, analytical, universal).

% These groups benefit from the clear and universal moral standing afforded to all born humans, as it provides a strong foundation for their advocacy for the rights and protection of infants, the disabled, and other vulnerable populations. The constraint simplifies their task by removing the need to argue for basic moral status.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, advocates_for_vulnerable, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally applicable, and easily verifiable boundary for moral and legal personhood, coordinating societal recognition of rights and protections for all born humans.
% TRANSFER_FUNCTION: Transfers universal moral and legal protections to all born humans, and imposes corresponding obligations on others not to harm them. It also transfers the burden of proof for moral standing from the individual to the societal framework.
% ABSENT_VOICES: While this reading is widely accepted, those who advocate for personhood beginning earlier (e.g., at conception) or later (e.g., based on cognitive capacity) are absent from the direct 'setting' of this specific constraint, though their arguments exist in broader philosophical and legal debates.
% DISAPPEARANCE_RATIONALE: If the birth threshold for personhood vanished, the legal and moral status of infants would become immediately ambiguous and contested. Laws against infanticide would lose their clear grounding, and the entire framework of human rights would require re-evaluation, leading to profound societal reorganization.
% FOUNDING_PROBLEM: To establish a clear, unambiguous, and universally applicable criterion for when a human being acquires full moral and legal standing, ensuring consistent protection and rights across diverse societies and individuals.
% FOUNDING_PROBLEM_CORROBORATION: International human rights declarations, national legal codes, and widespread moral intuitions across cultures corroborate the ongoing need for a clear personhood boundary. While the precise timing is debated, the necessity of such a boundary for social order and justice is widely affirmed by legal scholars and ethicists outside of specific advocacy groups.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The constraint's extractiveness is very low (0.05) because it primarily confers benefits (rights, protections) rather than extracting resources or imposing burdens beyond basic moral obligations. Suppression is minimal (0.02) as its persistence relies on widespread moral consensus and legal codification, not active coercion against dissenters. Theater ratio is negligible (0.01) as its function is direct and universally recognized. Accessibility collapse is high (0.95) because, within this framework, there are virtually no legitimate alternatives to recognizing born humans as persons. Resistance is low (0.01) because this specific reading is broadly accepted, even by those who argue for earlier or later personhood, as a practical and humane minimum.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of born humans, this constraint is a fundamental protection. From the perspective of legal systems, it is a necessary and largely settled principle for maintaining social order and justice. There is little perspectival gap regarding the core function of this specific reading, though its implications are debated by those who hold different personhood thresholds.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans are the primary 'victims' in the sense that they are the objects of the constraint's definition, but they are overwhelmingly beneficiaries of the protections it confers. State legal systems act as agenda-setters, codifying and enforcing this boundary. Moral philosophers observe and analyze, while advocates for the vulnerable are beneficiaries, as the constraint provides a clear basis for their work.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine natural law, or a constructed constraint that benefits identifiable agents?',
    'Analysis of cross-cultural legal and ethical systems: if the birth threshold is universally adopted without significant historical contestation, it leans towards natural law; if it varies significantly or is a product of specific historical developments, it leans towards a constructed constraint.',
    'If purely natural law, its classification as Mountain is robust. If constructed, the presence of ''born_humans'' as a ''victim'' (object of definition) could trigger False Summit Mountain detection, reclassifying it as a Tangled Rope if extraction were higher, or a Rope if coordination benefits were primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Ambiguity between natural law and constructed constraint for a foundational moral principle.').

omega_variable(
    sibling_reading_impact,
    'How would the adoption of a sibling reading (e.g., ''fitness_contingent_reading'') structurally alter the victim set and protections of this constraint?',
    'Comparative legal analysis of jurisdictions or historical periods where alternative personhood criteria were applied, examining changes in legal protections and societal treatment of specific human groups.',
    'A ''fitness_contingent_reading'' would remove some born humans (e.g., severely disabled infants) from the victim set of this constraint, effectively denying them moral standing and legal protection, leading to a reclassification of the ''personhood_boundary'' kernel''s overall impact on vulnerable populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact, conceptual, 'Impact of alternative personhood readings on the scope and beneficiaries/victims of moral standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1800, personhood_boundary__birth_threshold_reading, theater_ratio, 1800, 0.01).
narrative_ontology:measurement(pers_tr_t1900, personhood_boundary__birth_threshold_reading, theater_ratio, 1900, 0.01).
narrative_ontology:measurement(pers_tr_t2024, personhood_boundary__birth_threshold_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(pers_be_t1800, personhood_boundary__birth_threshold_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(pers_be_t1900, personhood_boundary__birth_threshold_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(pers_be_t2024, personhood_boundary__birth_threshold_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1800, personhood_boundary__birth_threshold_reading, suppression_requirement, 1800, 0.02).
narrative_ontology:measurement(pers_su_t1900, personhood_boundary__birth_threshold_reading, suppression_requirement, 1900, 0.02).
narrative_ontology:measurement(pers_su_t2024, personhood_boundary__birth_threshold_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
