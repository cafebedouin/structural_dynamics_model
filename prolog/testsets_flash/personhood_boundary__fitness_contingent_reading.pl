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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint defines personhood as contingent on demonstrated
 *   'fitness' (e.g., cognitive capacity, social contribution), denying moral
 *   and legal standing to entities that do not meet these criteria. It is a
 *   reading of the broader 'personhood_boundary' kernel. This reading allows
 *   state authorities and dominant social groups to exclude certain
 *   populations, such as severely disabled infants or those with profound
 *   cognitive impairments, from the moral community, thereby reducing
 *   obligations and justifying differential treatment. The high
 *   extractiveness and suppression reflect the severe consequences for those
 *   excluded and the active enforcement required to maintain such a boundary.
 *
 * KEY AGENTS:
 *   - state_authority: Primary agenda-setter (institutional/arbitrage) — defines and enforces fitness criteria.
 *   - dominant_social_groups: Primary beneficiary (organized/mobile) — benefits from exclusion, supports the framework.
 *   - pre_fitness_entities: Primary target/victim (powerless/trapped) — denied moral standing, vulnerable.
 *   - severely_disabled_infants: Specific target/victim (powerless/trapped) — denied personhood due to inability to meet fitness criteria.
 *   - human_rights_advocates: Observer (organized/analytical) — challenges the framework, advocates for universal personhood.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.9).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.95).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '1019cc6e-6cd3-4080-81c7-f6c0afe4fca9').
narrative_ontology:cs_kernel_codification('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', formalized).
narrative_ontology:cs_authority_grounding('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', extraction).
narrative_ontology:cs_interpretation_layer_present('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9').
narrative_ontology:cs_reading_relation('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', foundational, moral_standing_requires_demonstrated_fitness).
narrative_ontology:cs_axiom_status(moral_standing_requires_demonstrated_fitness, holdable).
narrative_ontology:cs_axiom_grounding('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', moral_standing_requires_demonstrated_fitness, conventional).
narrative_ontology:cs_axiom('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', foundational, state_has_authority_to_define_personhood).
narrative_ontology:cs_axiom_status(state_has_authority_to_define_personhood, holdable).
narrative_ontology:cs_axiom_grounding('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', state_has_authority_to_define_personhood, conventional).
narrative_ontology:cs_reference_frame('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', fitness_based_social_order).
narrative_ontology:cs_drift_state('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1019cc6e-6cd3-4080-81c7-f6c0afe4fca9', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, dominant_social_groups).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_entities).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_infants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the criteria for personhood, granting or denying moral and legal standing based on demonstrated fitness. Benefits from the flexibility to exclude certain populations from rights and protections, reducing resource obligations and maintaining social hierarchies.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the exclusion of 'unfit' entities, which can include access to resources, social status, and the avoidance of care burdens. They actively support and propagate the fitness-contingent definition of personhood.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, dominant_social_groups, beneficiary,
    organized, generational, mobile, national).

% These are individuals (e.g., infants, those with severe cognitive impairments) who have not yet, or cannot, demonstrate the 'fitness' criteria. They are denied moral standing, legal rights, and protections, making them vulnerable to exploitation or neglect.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_entities, payer,
    powerless, immediate, trapped, local).

% Specifically targeted by this reading, as their condition may prevent them from ever meeting the 'fitness' criteria. They are denied personhood from birth, leading to a lack of legal protection and potential for instrumentalization.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Challenge the fitness-contingent definition, arguing for universal moral standing based on inherent humanity. They document abuses and advocate for legal reforms, but operate outside the direct enforcement mechanism of the state authority.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit exclusionary, boundary for who counts as a moral and legal subject, simplifying resource allocation and social obligations by defining a 'non-person' category.
% TRANSFER_FUNCTION: Transfers resources, care obligations, and moral consideration away from entities deemed 'unfit' towards those who meet the criteria, primarily benefiting the state and dominant social groups.
% ABSENT_VOICES: The pre-fitness entities and severely disabled infants themselves are inherently unable to voice their objections. Their interests are represented, if at all, by external advocates who are often marginalized by the dominant framework.
% DISAPPEARANCE_RATIONALE: If personhood were universally granted regardless of fitness, the state would face immense new obligations for care and protection, resource allocation would need to be fundamentally rethought, and the social hierarchies built on exclusion would collapse. The legal and moral landscape would be profoundly reshaped.
% FOUNDING_PROBLEM: To manage social burdens and resource scarcity by defining a subset of human life that does not qualify for full moral and legal protection, often in contexts of historical eugenics or social control.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and dominant social groups continue to assert the necessity of such distinctions for social order and resource management. Human rights advocates, however, contest the legitimacy of this 'problem' itself, arguing it is a pretext for discrimination and extraction.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is extremely high because it denies fundamental rights and even existence to a class of beings, allowing for their instrumentalization or neglect. Suppression is also very high, as the excluded have no voice or means of resistance, and the state actively enforces their non-personhood. Theater ratio is low because the 'fitness' criteria are genuinely applied, even if their moral justification is contested. The claimed type is 'snare' because the coordination story (managing social burdens) is a cover for the severe, asymmetric extraction from the most vulnerable, maintained by active enforcement and suppression of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state authority and dominant social groups, this constraint is a necessary mechanism for social order and resource management, perhaps even a 'rope' or 'mountain' reflecting natural distinctions. From the perspective of the excluded and human rights advocates, it is a clear 'snare' designed to extract resources and avoid obligations by denying personhood.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority and dominant social groups are clear beneficiaries, as they gain flexibility and reduced obligations. Pre-fitness entities and severely disabled infants are clear victims, bearing the full cost of exclusion. Human rights advocates are observers, attempting to shift the moral and legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (to define and enforce personhood based on fitness) is actively maintained by its beneficiaries. The problem it 'solves' (managing social burdens by exclusion) is still 'live' for those who benefit, even if its moral legitimacy is contested. The classification as a snare prevents mislabeling it as a legitimate coordination mechanism by highlighting the severe, asymmetric extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criteria_objectivity,
    'Are the ''fitness'' criteria for personhood objectively measurable and universally applicable, or are they culturally contingent and subject to arbitrary interpretation?',
    'Cross-cultural comparative ethics and philosophical analysis of the criteria''s grounding. Empirical studies on the variability of ''fitness'' definitions across different societies and historical periods.',
    'If arbitrary, the constraint''s legitimacy as a ''natural'' boundary collapses, reinforcing its classification as a constructed snare. If objective, it might lend a (contested) ''mountain'' flavor to the boundary, though its extractive consequences would remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criteria_objectivity, conceptual, 'Ambiguity regarding the objectivity and universality of ''fitness'' criteria for personhood.').

omega_variable(
    moral_standing_vs_social_utility,
    'Is the denial of personhood based on a genuine lack of moral standing, or is it primarily driven by considerations of social utility and resource management?',
    'Ethical analysis of the justifications provided by proponents versus the observed outcomes and benefits to the enforcing parties. Examination of historical contexts where similar criteria were applied.',
    'If primarily utility-driven, the extractive nature of the constraint is further emphasized, solidifying its ''snare'' classification. If genuinely based on a philosophical argument for lack of standing, it shifts the debate to the validity of that philosophical premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_standing_vs_social_utility, conceptual, 'Distinguishing between philosophical justification and utilitarian motives for denying personhood.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the ''personhood_boundary'' kernel, or an instrumentalization of the concept for extractive purposes?',
    'Comparative analysis with other readings of the personhood kernel, examining consistency with broader philosophical traditions and the presence of identifiable beneficiaries.',
    'If an instrumentalization, it highlights the ''snare'' aspect and the performative nature of its philosophical claims. If a genuine, albeit contested, reading, it frames the conflict as a deeper philosophical disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''personhood_boundary'' kernel. A sibling reading, ''birth_threshold_reading'', posits personhood at birth, and ''potential_based_reading'' grounds it in potential for agency. This reading differs by requiring demonstrated fitness, leading to the exclusion of pre-fitness entities from the moral community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1900, personhood_boundary__fitness_contingent_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(pers_tr_t1930, personhood_boundary__fitness_contingent_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(pers_tr_t1960, personhood_boundary__fitness_contingent_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(pers_tr_t1990, personhood_boundary__fitness_contingent_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(pers_tr_t2024, personhood_boundary__fitness_contingent_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t1900, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(pers_be_t1930, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1930, 0.9).
narrative_ontology:measurement(pers_be_t1960, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1960, 0.85).
narrative_ontology:measurement(pers_be_t1990, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1990, 0.92).
narrative_ontology:measurement(pers_be_t2024, personhood_boundary__fitness_contingent_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1900, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(pers_su_t1930, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1930, 0.95).
narrative_ontology:measurement(pers_su_t1960, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(pers_su_t1990, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1990, 0.98).
narrative_ontology:measurement(pers_su_t2024, personhood_boundary__fitness_contingent_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
