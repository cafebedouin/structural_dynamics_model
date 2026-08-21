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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Personhood Begins at Birth (Birth Threshold Reading)
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'birth threshold' reading of the
 *   'personhood_boundary' kernel, asserting that personhood begins
 *   universally at birth, granting all born humans full moral and legal
 *   standing. It is presented as a foundational moral principle, widely
 *   codified in law and ethics, which imposes duties of non-harm and
 *   protection on society. The metrics reflect its status as a highly
 *   suppressive but low-extraction moral 'mountain', where the 'extraction'
 *   is the necessary cost of maintaining a just moral order, not
 *   rent-seeking. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as a mountain (a foundational moral truth) while the authored
 *   metrics describe its actual operation as a highly suppressive, actively
 *   enforced principle — the engine measures that divergence; do not
 *   reconcile the claim to the metrics.
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
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Begins at Birth (Birth Threshold Reading)").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).
domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'a4507d69-50f4-45ee-a092-c4701379ede6').
narrative_ontology:cs_kernel_codification('a4507d69-50f4-45ee-a092-c4701379ede6', formalized).
narrative_ontology:cs_authority_grounding('a4507d69-50f4-45ee-a092-c4701379ede6', lineage).
narrative_ontology:cs_interpretation_layer_present('a4507d69-50f4-45ee-a092-c4701379ede6').
narrative_ontology:cs_reading_relation('a4507d69-50f4-45ee-a092-c4701379ede6', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('a4507d69-50f4-45ee-a092-c4701379ede6', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('a4507d69-50f4-45ee-a092-c4701379ede6', foundational, birth_confers_personhood).
narrative_ontology:cs_axiom_status(birth_confers_personhood, holdable).
narrative_ontology:cs_axiom_grounding('a4507d69-50f4-45ee-a092-c4701379ede6', birth_confers_personhood, deontological).
narrative_ontology:cs_axiom('a4507d69-50f4-45ee-a092-c4701379ede6', foundational, all_born_humans_equal_moral_standing).
narrative_ontology:cs_axiom_status(all_born_humans_equal_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('a4507d69-50f4-45ee-a092-c4701379ede6', all_born_humans_equal_moral_standing, deontological).
narrative_ontology:cs_reference_frame('a4507d69-50f4-45ee-a092-c4701379ede6', universal_post_birth_personhood).
narrative_ontology:cs_drift_state('a4507d69-50f4-45ee-a092-c4701379ede6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a4507d69-50f4-45ee-a092-c4701379ede6', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_humans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, parents_and_caregivers).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, parents_and_caregivers).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_human_rights).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, sanctity_of_life_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All individuals born into the human species, from infancy through adulthood. They are the primary beneficiaries of this constraint, as it grants them inherent moral and legal standing, protecting them from arbitrary harm and ensuring their rights are recognized. They cannot exit their status as born humans.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_humans, beneficiary,
    powerless, generational, trapped, universal).

% Bear the moral and legal duties to protect and nurture born humans, especially infants and vulnerable individuals. They also benefit from the societal framework that protects their own children and ensures a stable moral order.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, parents_and_caregivers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, parents_and_caregivers, beneficiary).

% Enforce the principle of personhood at birth through laws against homicide, child abuse, and discrimination. They benefit from the social cohesion and moral order that this foundational principle provides, but are constrained by the need to maintain legitimacy and consistency with other moral principles.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, legal_and_social_institutions, agenda_setter,
    institutional, civilizational, constrained, global).

% Analyze, debate, and refine the conceptual underpinnings and implications of personhood, including the birth threshold. They do not directly benefit or pay, but their work influences the societal understanding and application of the constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, philosophers_and_ethicists, observer,
    analytical, generational, analytical, universal).

% Those who argue for personhood to be contingent on factors other than birth (e.g., cognitive capacity, 'fitness'). Their views are largely excluded from mainstream legal and ethical frameworks that uphold birth as the threshold for personhood, though they continue to engage in philosophical debate.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, advocates_for_contingent_personhood, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, unambiguous baseline for moral and legal standing for all members of the human species from the moment of birth, preventing arbitrary harm and providing a foundation for social order and human rights.
% TRANSFER_FUNCTION: Transfers the universal duty of respect, protection, and non-harm from society and individuals to all born humans, regardless of their developmental stage or perceived 'fitness'.
% ABSENT_VOICES: Historically, those who practiced infanticide or denied personhood based on perceived 'fitness' (e.g., severe disability). In contemporary discourse, advocates for fitness-contingent or potential-based personhood are largely excluded from the legal and ethical frameworks that uphold birth as the universal threshold.
% DISAPPEARANCE_RATIONALE: If the principle of personhood at birth vanished overnight, the entire legal and moral framework for protecting born humans would collapse. Laws against homicide and child abuse would lose their foundational justification, leading to profound social disorder and a re-evaluation of fundamental human rights. The concept of universal moral standing would be severely undermined.
% FOUNDING_PROBLEM: The problem of preventing arbitrary killing, abuse, and exploitation of vulnerable born humans, and establishing a clear, universal basis for their moral and legal protection within society.
% FOUNDING_PROBLEM_CORROBORATION: International human rights declarations (e.g., Universal Declaration of Human Rights), national legal codes prohibiting infanticide and child abuse, medical ethics, and widespread moral intuitions across diverse cultures all corroborate the ongoing relevance of this founding problem and the necessity of the birth threshold for personhood. This corroboration comes from sources outside the direct beneficiaries (born humans) and enforcers (legal institutions).
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the constraint primarily establishes a baseline for moral obligation rather than extracting rents for a specific party; the 'cost' is the inherent duty of moral agents in a society. Suppression is very high (0.9) because the denial of personhood to born humans is strongly prohibited by law and social norms. Theater ratio is very low (0.05) as the constraint is almost entirely functional, underpinning fundamental legal and ethical systems. Accessibility collapse is high (0.95) as the alternative of treating a born human as a non-person is almost entirely foreclosed. Resistance is low (0.1) due to broad societal consensus against infanticide and for universal human rights. The measurement series track the historical consolidation of this principle, showing stable, high suppression and low extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of born humans, this constraint is a pure benefit, a fundamental right. From the perspective of those who must adhere to the duties it imposes (e.g., parents, society), it is a non-negotiable moral obligation. The engine computes these different experiences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans are the clear beneficiaries, receiving universal moral and legal protection. Parents and caregivers bear the duties of care and non-harm, making them payers, but also benefit from the societal protection of their children. Legal and social institutions act as agenda-setters, enforcing the constraint and benefiting from the social order it provides. Advocates for contingent personhood are structurally excluded from the mainstream application of this principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_mountain_vs_social_construct,
    'Is the principle of personhood at birth a genuine moral ''mountain'' (an irreducible moral truth) or a deeply entrenched social construct that benefits identifiable agents (born humans and the society that protects them)?',
    'Cross-cultural anthropological studies of moral universals, philosophical arguments for objective moral truths, or analysis of the historical contingency of this principle''s adoption.',
    'If a genuine moral mountain, its classification as such is robust. If primarily a social construct, it might be reclassified as a highly stable ''rope'' or ''tangled_rope'' that coordinates social order and protects vulnerable populations, but is ultimately revisable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_mountain_vs_social_construct, conceptual, 'Ambiguity between inherent moral truth and socially constructed norm.').

omega_variable(
    contingent_personhood_challenge,
    'How would the adoption of a ''fitness_contingent_reading'' or ''potential_based_reading'' structurally alter the victim set and enforcement mechanisms of this constraint?',
    'Analysis of proposed legal frameworks for contingent personhood, including criteria for ''fitness'' or ''potential'' and their implications for legal protections for infants and disabled individuals.',
    'If a contingent reading were adopted, the victim set would expand to include those born humans deemed ''unfit'' or lacking ''potential'', and the constraint''s protective function would be severely eroded for those groups. This would fundamentally shift the constraint''s classification from a protective ''mountain'' to a potentially extractive ''snare'' for the newly excluded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contingent_personhood_challenge, conceptual, 'Impact of alternative personhood readings on victim status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 1700, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1700, personhood_boundary__birth_threshold_reading, theater_ratio, 1700, 0.06).
narrative_ontology:measurement(pers_tr_t1780, personhood_boundary__birth_threshold_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(pers_tr_t1860, personhood_boundary__birth_threshold_reading, theater_ratio, 1860, 0.05).
narrative_ontology:measurement(pers_tr_t1940, personhood_boundary__birth_threshold_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(pers_tr_t2020, personhood_boundary__birth_threshold_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(pers_be_t1700, personhood_boundary__birth_threshold_reading, base_extractiveness, 1700, 0.12).
narrative_ontology:measurement(pers_be_t1780, personhood_boundary__birth_threshold_reading, base_extractiveness, 1780, 0.13).
narrative_ontology:measurement(pers_be_t1860, personhood_boundary__birth_threshold_reading, base_extractiveness, 1860, 0.14).
narrative_ontology:measurement(pers_be_t1940, personhood_boundary__birth_threshold_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(pers_be_t2020, personhood_boundary__birth_threshold_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1700, personhood_boundary__birth_threshold_reading, suppression_requirement, 1700, 0.8).
narrative_ontology:measurement(pers_su_t1780, personhood_boundary__birth_threshold_reading, suppression_requirement, 1780, 0.85).
narrative_ontology:measurement(pers_su_t1860, personhood_boundary__birth_threshold_reading, suppression_requirement, 1860, 0.88).
narrative_ontology:measurement(pers_su_t1940, personhood_boundary__birth_threshold_reading, suppression_requirement, 1940, 0.9).
narrative_ontology:measurement(pers_su_t2020, personhood_boundary__birth_threshold_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
