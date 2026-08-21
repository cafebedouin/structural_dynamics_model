% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause: Antisubordination Reading
 *   domain: constitutional_law/civil_rights/education_policy
 *
 * SUMMARY:
 *   This constraint represents the antisubordination reading of the Equal
 *   Protection Clause, which interprets the clause as primarily prohibiting
 *   state action that entrenches a caste-like system of racial hierarchy. It
 *   permits race-conscious measures designed to dismantle such hierarchy and
 *   does not extend equal protection claims to dominant groups challenging
 *   remedial policies. This reading is one of several competing
 *   interpretations of the Equal Protection Clause, each with distinct
 *   implications for civil rights and state power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.45).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.3).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause: Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/civil_rights/education_policy").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '2c00136a-54be-40f7-b2be-c385480ce893').
narrative_ontology:cs_kernel_codification('2c00136a-54be-40f7-b2be-c385480ce893', fixed_text).
narrative_ontology:cs_authority_grounding('2c00136a-54be-40f7-b2be-c385480ce893', lineage).
narrative_ontology:cs_interpretation_layer_present('2c00136a-54be-40f7-b2be-c385480ce893').
narrative_ontology:cs_reading_relation('2c00136a-54be-40f7-b2be-c385480ce893', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c00136a-54be-40f7-b2be-c385480ce893', equal_protection_kernel__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('2c00136a-54be-40f7-b2be-c385480ce893', foundational, equal_protection_forbids_caste).
narrative_ontology:cs_axiom_status(equal_protection_forbids_caste, holdable).
narrative_ontology:cs_axiom_grounding('2c00136a-54be-40f7-b2be-c385480ce893', equal_protection_forbids_caste, deontological).
narrative_ontology:cs_axiom('2c00136a-54be-40f7-b2be-c385480ce893', foundational, state_may_dismantle_hierarchy).
narrative_ontology:cs_axiom_status(state_may_dismantle_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('2c00136a-54be-40f7-b2be-c385480ce893', state_may_dismantle_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('2c00136a-54be-40f7-b2be-c385480ce893', post_reconstruction_era_anti_caste_principle).
narrative_ontology:cs_drift_state('2c00136a-54be-40f7-b2be-c385480ce893', contemporary_judicial_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2c00136a-54be-40f7-b2be-c385480ce893', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, state_actors_entrenching_hierarchy).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, equal_citizenship_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, remedial_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from state actions designed to dismantle caste-like subordination, such as affirmative action or targeted educational programs. Their ability to exit subordination is directly tied to the enforcement of this reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% These actors (e.g., legislatures, executive agencies, courts) implement policies that use race-conscious measures to address and dismantle systemic subordination. They operate under the legal framework provided by this reading, which permits such actions.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_actors_dismantling_hierarchy, agenda_setter,
    institutional, biographical, constrained, national).

% These actors are forbidden from taking actions that entrench racial hierarchy. They bear the cost of having their policies struck down or being compelled to adopt remedial measures. Their 'exit' is to cease such actions or face legal challenge.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_actors_entrenching_hierarchy, payer,
    institutional, biographical, constrained, national).

% Under this reading, dominant groups cannot claim equal protection against remedial measures designed to benefit historically subordinated groups. They are excluded from the victim class of the Equal Protection Clause when challenging such measures.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_social_groups, excluded,
    powerful, biographical, mobile, national).

% The ultimate arbiter of constitutional meaning, whose interpretations shape the application and enforcement of the Equal Protection Clause. Its rulings determine which reading prevails and how state action is constrained.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action to actively dismantle caste-like subordination and promote substantive equality for historically oppressed groups, providing a legal framework for race-conscious remedies.
% TRANSFER_FUNCTION: Transfers legal legitimacy and protection to state actions aimed at uplifting subordinated groups, while withdrawing it from actions that perpetuate hierarchy. It also transfers social and political capital to historically marginalized communities.
% ABSENT_VOICES: Proponents of a strictly colorblind interpretation, who would argue that any race-conscious measure is unconstitutional, are structurally excluded from the beneficiary class of this reading, as their claims are not recognized as valid under an antisubordination framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state actors would lose a crucial legal justification for race-conscious remedial policies. Historically subordinated groups would lose a key constitutional protection against systemic inequality, leading to a re-entrenchment of existing hierarchies and a significant rearrangement of civil rights jurisprudence and social policy.
% FOUNDING_PROBLEM: The Equal Protection Clause was enacted to address the systemic racial subordination that persisted after the abolition of slavery, aiming to prevent the re-establishment of a caste system.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, legal scholars focused on critical race theory, and historical analyses of systemic inequality corroborate that the problem of caste-like subordination remains live. This is attested by ongoing disparities in education, wealth, and justice, documented by independent research and advocacy groups outside the direct beneficiaries of specific remedial policies.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the ongoing struggle to implement and defend antisubordination policies against challenges from other readings. Suppression (0.30) is relatively low, as this reading aims to empower historically oppressed groups rather than suppress them, though it does suppress state actions that would entrench hierarchy. Resistance (0.70) is high, indicating the significant legal and political opposition this reading faces from proponents of colorblind or narrower remedial interpretations. Accessibility collapse (0.40) is moderate, as alternative legal arguments (colorblindness) remain viable and actively pursued.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups, this reading is a vital tool for achieving substantive equality. From the perspective of dominant social groups, it may be seen as a form of 'reverse discrimination' or an overreach of state power, leading to a fundamental disagreement on the constraint's purpose and effect. The Supreme Court's role as arbiter means its perspective is crucial in determining the effective classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are the primary beneficiaries, as the reading legitimizes policies designed to benefit them. State actors who implement such policies are agenda-setters. State actors who would entrench hierarchy are victims, as their actions are forbidden. Dominant social groups are 'excluded' from the victim class when challenging remedial measures, reflecting the core tenet of this reading that equal protection is not a shield for privilege.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_subordination,
    'What constitutes ''caste-like subordination'' in contemporary society, and how is it empirically measured?',
    'Sociological and economic studies demonstrating persistent, systemic disparities linked to historical oppression, coupled with judicial consensus on the evidentiary standards for proving such subordination.',
    'A clear definition and measurement would strengthen the application of this reading, making it easier to identify and remedy prohibited state actions. Ambiguity weakens its enforcement and makes it vulnerable to challenges from other readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_subordination, empirical, 'Defines the scope of the problem this reading addresses.').

omega_variable(
    reading_legitimacy_contest,
    'Is the antisubordination reading a legitimate interpretation of the Equal Protection Clause''s original intent and evolving constitutional meaning, or is it an activist interpretation?',
    'Ongoing legal scholarship, judicial precedent, and public discourse that either solidifies or undermines its constitutional grounding. A shift in Supreme Court composition could decisively alter its status.',
    'If deemed illegitimate, this reading''s influence would wane, potentially leading to its formal rejection and a reclassification of related policies. If solidified, it would become a more robust and less contested constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, conceptual, 'Contest over the constitutional legitimacy of the antisubordination framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.3).
narrative_ontology:measurement(equa_be_t1970, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(equa_be_t2024, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(equa_su_t1970, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(equa_su_t2024, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, affirmative_action_policies).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, voting_rights_act_enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
