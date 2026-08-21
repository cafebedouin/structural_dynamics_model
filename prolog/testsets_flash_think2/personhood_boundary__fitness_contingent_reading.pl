% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes a historical and philosophical reading of
 *   personhood where moral standing is not inherent but contingent upon an
 *   individual demonstrating certain 'fitness' criteria (e.g., cognitive
 *   capacity, physical ability, social utility). Entities that do not meet
 *   these criteria, such as severely disabled infants or those deemed 'unfit'
 *   by state authority, are denied full personhood and moral consideration.
 *   This reading often served to legitimize eugenic policies and
 *   discriminatory practices, actively creating victims by stripping them of
 *   fundamental rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.9).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.85).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '8f92c4e7-ae23-4d10-ad7f-a60460dc4067').
narrative_ontology:cs_kernel_codification('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', formalized).
narrative_ontology:cs_authority_grounding('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', extraction).
narrative_ontology:cs_interpretation_layer_present('8f92c4e7-ae23-4d10-ad7f-a60460dc4067').
narrative_ontology:cs_reading_relation('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', foundational, moral_standing_is_earned_not_given).
narrative_ontology:cs_axiom_status(moral_standing_is_earned_not_given, holdable).
narrative_ontology:cs_axiom_grounding('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', moral_standing_is_earned_not_given, empirically_contingent).
narrative_ontology:cs_axiom('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', secondary, social_utility_determines_moral_inclusion).
narrative_ontology:cs_axiom_status(social_utility_determines_moral_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', social_utility_determines_moral_inclusion, instrumental).
narrative_ontology:cs_reference_frame('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', community_defined_by_demonstrated_capacity).
narrative_ontology:cs_drift_state('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8f92c4e7-ae23-4d10-ad7f-a60460dc4067', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fit_community_members).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_entities).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_infants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the criteria for personhood, granting or denying moral standing based on demonstrated fitness. Benefits from consolidating power and resources within the 'fit' community and excluding those deemed unproductive or burdensome.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Possess full moral standing and rights, often benefiting from the exclusion of others. Their status is affirmed by the constraint, reinforcing their social and political position within the community.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fit_community_members, beneficiary,
    powerful, biographical, mobile, national).

% Lack moral standing and rights until they demonstrate 'fitness'. They are subject to the will of the 'fit' community and can be treated as property or resources, experiencing the ultimate form of extraction: denial of personhood.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_entities, payer,
    powerless, immediate, trapped, local).

% Are denied moral standing due to their inability to demonstrate the required 'fitness'. They are vulnerable to instrumentalization and neglect, as their existence is not recognized as having intrinsic value.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Critique the concept of contingent personhood, arguing for universal and inherent moral standing. They work to challenge the legal and philosophical underpinnings of this constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Advocate for personhood beginning at birth, regardless of fitness. Their view is fundamentally incompatible with the fitness-contingent reading, and they are excluded from the decision-making process that defines personhood in this framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, birth_threshold_proponents, excluded,
    organized, generational, constrained, global).

% Argue for personhood based on the potential for rational agency. While their position might exclude some, it is still foreclosed by the stricter 'demonstrated fitness' requirement of this reading, placing them outside its legitimate discourse.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, potential_based_proponents, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the boundaries of the moral and legal community, allocating rights and resources based on a shared understanding of who 'counts' as a person, thereby coordinating social order and resource distribution among the 'fit'.
% TRANSFER_FUNCTION: Transfers moral standing, rights, and access to resources from entities deemed 'unfit' to the 'fit' community and the state authority, consolidating power and legitimizing the instrumental use of the excluded.
% ABSENT_VOICES: The 'pre-fitness entities' and 'severely disabled infants' themselves are inherently voiceless within this framework. Advocates for universal personhood are actively marginalized or excluded from the discourse that defines these boundaries.
% DISAPPEARANCE_RATIONALE: If personhood were no longer contingent on demonstrated fitness, the entire legal, social, and ethical framework of the society would collapse. The status of many individuals would fundamentally change, requiring a complete re-evaluation of rights, responsibilities, and resource allocation.
% FOUNDING_PROBLEM: To establish a strong, productive, and 'pure' moral community by defining clear, measurable criteria for membership and excluding those perceived as burdens or threats to its collective strength.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the state authority and segments of the 'fit' community claim the problem of maintaining a strong community and managing 'unfit' populations is still live. However, human rights advocates and proponents of other personhood readings widely dispute this, arguing the constraint serves primarily to legitimize oppression and resource hoarding, with corroboration from historical analyses of eugenics and discriminatory practices.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.9) because the constraint denies the most fundamental right: personhood itself, from a significant portion of the population. Suppression is also very high (0.85) as the state actively enforces these criteria, often through legal and medical systems, with no recourse for those deemed 'unfit'. Theater ratio is low (0.1) because the exclusion is direct and functional, not merely performative. Accessibility collapse is near total (0.9) for those denied personhood, as there are no alternatives to gaining moral standing within this framework. Resistance is moderate (0.65), reflecting the growing opposition from human rights movements and other philosophical traditions during the specified interval.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'state_authority' and 'fit_community_members', this constraint might be framed as a necessary measure for social order and collective strength. However, from the perspective of the 'victims' and 'human_rights_advocates', it is a clear Snare, designed for pure extraction and oppression. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'state_authority' and 'fit_community_members' are clear beneficiaries, gaining power, resources, and social cohesion from defining and enforcing who belongs. 'Pre_fitness_entities' and 'severely_disabled_infants' are the primary victims, experiencing the ultimate extraction of moral standing. Human rights advocates act as observers, while proponents of other personhood readings are excluded from the legitimate discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criteria_ambiguity,
    'What specific criteria constitute ''demonstrated fitness'', and how are these criteria objectively measured and applied without bias?',
    'Detailed historical analysis of legal codes, medical assessments, and philosophical texts from the period, alongside critical sociological studies of their application.',
    'If criteria are vague, arbitrary, or applied discriminatorily, it strengthens the Snare classification by revealing the constructed and coercive nature of the ''fitness'' standard. If criteria are consistently applied, it highlights the philosophical basis of the exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criteria_ambiguity, empirical, 'Ambiguity in the definition and application of ''fitness'' criteria for personhood.').

omega_variable(
    moral_standing_source_ambiguity,
    'Is moral standing an inherent property of all human beings, or is it a social construct granted by a community or authority?',
    'Conceptual analysis within moral philosophy and legal theory, examining the foundational arguments for inherent vs. contingent personhood.',
    'If moral standing is inherent, this constraint is a clear violation of fundamental rights, reinforcing its Snare classification. If it is purely a social construct, the constraint''s legitimacy hinges on the authority''s justification for its chosen criteria.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_standing_source_ambiguity, conceptual, 'Whether moral standing is inherent or socially granted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 1850, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1850, personhood_boundary__fitness_contingent_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(pers_tr_t1870, personhood_boundary__fitness_contingent_reading, theater_ratio, 1870, 0.12).
narrative_ontology:measurement(pers_tr_t1890, personhood_boundary__fitness_contingent_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(pers_tr_t1910, personhood_boundary__fitness_contingent_reading, theater_ratio, 1910, 0.08).
narrative_ontology:measurement(pers_tr_t1930, personhood_boundary__fitness_contingent_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(pers_tr_t1950, personhood_boundary__fitness_contingent_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t1850, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1850, 0.75).
narrative_ontology:measurement(pers_be_t1870, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1870, 0.8).
narrative_ontology:measurement(pers_be_t1890, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1890, 0.85).
narrative_ontology:measurement(pers_be_t1910, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1910, 0.88).
narrative_ontology:measurement(pers_be_t1930, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1930, 0.9).
narrative_ontology:measurement(pers_be_t1950, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1950, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1850, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(pers_su_t1870, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1870, 0.75).
narrative_ontology:measurement(pers_su_t1890, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(pers_su_t1910, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1910, 0.83).
narrative_ontology:measurement(pers_su_t1930, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1930, 0.85).
narrative_ontology:measurement(pers_su_t1950, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1950, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
