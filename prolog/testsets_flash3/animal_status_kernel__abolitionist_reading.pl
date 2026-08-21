% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Status: Animals as Moral Persons, Not Property
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal status,
 *   asserting that animals are moral persons with a fundamental right not to
 *   be property. From this perspective, the current legal and social status
 *   of animals as property is the core injustice, making all instrumental use
 *   categorically impermissible, regardless of welfare conditions. This
 *   reading views the existing system as a Snare, extracting the very
 *   personhood and lives of animals. The high extractiveness and suppression
 *   reflect the systemic nature of animal exploitation and the deep
 *   entrenchment of property status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Abolitionist Reading of Animal Status: Animals as Moral Persons, Not Property").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '1c1ceffa-d11e-478a-bfb8-d1c26cb92d70').
narrative_ontology:cs_kernel_codification('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', formalized).
narrative_ontology:cs_authority_grounding('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', lineage).
narrative_ontology:cs_interpretation_layer_present('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70').
narrative_ontology:cs_reading_relation('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_axiom('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', foundational, property_status_is_injustice).
narrative_ontology:cs_axiom_status(property_status_is_injustice, holdable).
narrative_ontology:cs_axiom_grounding('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', property_status_is_injustice, deontological).
narrative_ontology:cs_reference_frame('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', universal_moral_personhood).
narrative_ontology:cs_drift_state('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', contemporary_legal_framework, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1c1ceffa-d11e-478a-bfb8-d1c26cb92d70', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_as_property).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animal_advocates_abolitionist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As legal property, animals are subjected to instrumental use across all domains (food, research, entertainment). They bear the full cost of this property status, which denies their moral personhood and right to bodily integrity. Exit is structurally impossible under the current legal framework.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_as_property, payer,
    powerless, immediate, trapped, universal).

% Advocate for the complete abolition of animal property status and all forms of animal use. They bear the costs of resistance against a deeply entrenched system, facing legal, social, and economic barriers to their goals. Their 'payment' is the effort and resources expended in activism and legal challenges.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_advocates_abolitionist, payer,
    moderate, generational, constrained, global).

% Benefit directly from the property status of animals, which allows for their instrumental use in agriculture, research, and entertainment. They actively lobby to maintain this status and resist any legal or social changes that would challenge it, setting the agenda for animal-related legislation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_use_industries, agenda_setter,
    institutional, generational, arbitrage, global).

% Codify and enforce the property status of animals, providing the foundational legal framework for their use. While theoretically capable of change, legal systems are slow to adapt and heavily influenced by economic and social inertia, making them de facto enforcers of the current status quo.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% While seeking to improve animal conditions, their focus on welfare reform (rather than abolition of property status) is seen by abolitionists as implicitly legitimizing animal use and delaying fundamental change. They are 'excluded' from the abolitionist conversation as their approach is viewed as a strategic impediment.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist perspective, this constraint (animal property status) does not solve a legitimate coordination problem but rather coordinates the exploitation of animals for human benefit, facilitating industries that rely on their instrumentalization.
% TRANSFER_FUNCTION: Transfers the moral status, bodily autonomy, and lives of animals to humans, who then claim ownership and the right to use them as resources. This enables the transfer of economic value from animal bodies and labor to human industries.
% ABSENT_VOICES: The animals themselves are the primary absent voices, unable to articulate their objection to being property. Additionally, future generations who might adopt an abolitionist stance are currently unrepresented in the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, the global food system, pharmaceutical research, entertainment industries, and pet ownership models would undergo a radical, immediate, and profound reorganization. The economic, legal, and social structures built upon animal exploitation would collapse, necessitating entirely new paradigms for human-animal interaction.
% FOUNDING_PROBLEM: The problem of how to manage and utilize animals for human benefit, particularly for food, labor, and companionship, while establishing clear lines of ownership and control.
% FOUNDING_PROBLEM_CORROBORATION: Animal use industries and many legal scholars attest that the problem of managing animal resources for human benefit is still live and requires the current property framework. Abolitionist advocates, however, argue that the 'problem' itself is a construct of an unjust system, and the true problem is the moral status of animals, which the current framework fails to address.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the constraint fundamentally denies moral personhood and enables the total instrumentalization of animals, extracting their lives and autonomy. Suppression is also very high (0.98) due to the legal and cultural entrenchment of animal property status, which systematically silences animal agency and makes resistance extremely difficult. Theater ratio is low (0.05) because, from this reading, there is little performative 'welfare' that genuinely mitigates the fundamental injustice of property status; any such efforts are seen as tangential to the core problem. Accessibility collapse is low (0.1) because the abolitionist reading posits that alternatives (a world without animal property) are conceptually available, even if practically suppressed. Resistance is high (0.85) because there is active and growing opposition to animal exploitation, even if it faces immense structural barriers.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both the property and welfare readings. While the property reading sees animals as mere resources and the welfare reading seeks to mitigate suffering within the property framework, the abolitionist reading rejects property status itself as the source of injustice. This creates a deep perspectival gap where what one reading considers 'coordination' (e.g., efficient animal agriculture) another sees as 'pure extraction' (systemic violence against moral persons).
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the ultimate targets (d=1.0) as they bear the full cost of being property. Abolitionist advocates are also targets (d high) as they expend significant resources fighting the system. Animal use industries and legal systems are the primary beneficiaries/agenda-setters (d low) as they profit from and enforce animal property status. Welfare advocates are structurally 'excluded' from the abolitionist frame, as their incremental approach is seen as reinforcing the property paradigm.
 *
 * MANDATROPHY ANALYSIS:
 *   From the abolitionist perspective, the mandate of animal property status (to manage animals for human benefit) has not atrophied but is itself the problem. The classification as a Snare prevents mislabeling this as a coordination mechanism, highlighting the fundamental extraction inherent in treating moral persons as property. The 'mandate' is seen as a cover for exploitation, not a genuine problem-solving function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_personhood_empirical_basis,
    'Is the claim of animal moral personhood empirically grounded (e.g., in cognitive science, sentience research) or primarily a deontological/philosophical assertion?',
    'Continued scientific research into animal consciousness, cognition, and emotional complexity, alongside philosophical debate on the criteria for personhood.',
    'Strong empirical grounding could shift public and legal discourse, increasing the perceived extractiveness and suppression of the property status. If purely philosophical, the debate remains in the conceptual domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_personhood_empirical_basis, empirical, 'The epistemic grounding of animal moral personhood.').

omega_variable(
    welfare_reform_strategic_impact,
    'Does incremental welfare reform (as advocated by the welfare_reading) ultimately advance or delay the abolition of animal property status?',
    'Longitudinal sociological and legal studies tracking the impact of welfare legislation on public attitudes, industry practices, and the legal status of animals over decades.',
    'If welfare reform delays abolition, the abolitionist reading''s assessment of welfare advocates as ''excluded'' (due to their counterproductive impact) is strengthened. If it advances abolition, the strategic tension between the readings might lessen.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reform_strategic_impact, empirical, 'The strategic effect of welfare reforms on abolitionist goals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of animal agency structural (legal property status, economic systems) or internalized (cultural norms, speciesism)?',
    'Post-legal-change analysis: if animal exploitation persists after property status is removed in some jurisdictions, reclassify as partially internalized cultural suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the cultural norms would persist after legal changes, making abolition harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for animal exploitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__abolitionist_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__abolitionist_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__abolitionist_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__abolitionist_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__abolitionist_reading, base_extractiveness, 10, 0.92).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__abolitionist_reading, base_extractiveness, 20, 0.93).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__abolitionist_reading, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__abolitionist_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__abolitionist_reading, suppression_requirement, 10, 0.96).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__abolitionist_reading, suppression_requirement, 20, 0.97).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__abolitionist_reading, suppression_requirement, 30, 0.97).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.98).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__abolitionist_reading, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_status_kernel'. This abolitionist reading focuses on the categorical injustice of animal property status, distinct from the property-centric and welfare-centric sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
