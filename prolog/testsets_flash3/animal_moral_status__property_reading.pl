% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animals as Property/Resources (Property Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of animal moral status,
 *   where animals are legally and ethically defined as property or resources,
 *   with no independent moral standing. Their interests are subordinate to
 *   human interests by definition. This reading is presented as a 'mountain'
 *   due to its deep entrenchment in legal systems and social practices,
 *   making it appear unchangeable and natural. The low extractiveness (0.05)
 *   reflects the view that property rights themselves are not extractive from
 *   the perspective of the property owner, and the high suppression (0.95)
 *   reflects the near-complete legal and social suppression of animal
 *   interests. The constraint is claimed as a mountain, and the metrics align
 *   with this claim from the perspective of the property reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.05).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.95).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animals as Property/Resources (Property Reading)").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '427d84c8-efe1-406e-8c28-9351fcce9b55').
narrative_ontology:cs_kernel_codification('427d84c8-efe1-406e-8c28-9351fcce9b55', formalized).
narrative_ontology:cs_authority_grounding('427d84c8-efe1-406e-8c28-9351fcce9b55', lineage).
narrative_ontology:cs_interpretation_layer_present('427d84c8-efe1-406e-8c28-9351fcce9b55').
narrative_ontology:cs_reading_relation('427d84c8-efe1-406e-8c28-9351fcce9b55', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('427d84c8-efe1-406e-8c28-9351fcce9b55', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('427d84c8-efe1-406e-8c28-9351fcce9b55', foundational, animals_are_chattel).
narrative_ontology:cs_axiom_status(animals_are_chattel, holdable).
narrative_ontology:cs_axiom_grounding('427d84c8-efe1-406e-8c28-9351fcce9b55', animals_are_chattel, conventional).
narrative_ontology:cs_axiom('427d84c8-efe1-406e-8c28-9351fcce9b55', foundational, human_interests_are_paramount).
narrative_ontology:cs_axiom_status(human_interests_are_paramount, holdable).
narrative_ontology:cs_axiom_grounding('427d84c8-efe1-406e-8c28-9351fcce9b55', human_interests_are_paramount, deontological).
narrative_ontology:cs_reference_frame('427d84c8-efe1-406e-8c28-9351fcce9b55', classical_property_law).
narrative_ontology:cs_drift_state('427d84c8-efe1-406e-8c28-9351fcce9b55', contemporary_animal_welfare_movement, gap(stable, minor, false)).
narrative_ontology:cs_created_at('427d84c8-efe1-406e-8c28-9351fcce9b55', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_resource_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds legal title to animals, exercising rights of use, sale, and disposition. Benefits from the legal and social framework that defines animals as property, enabling economic activities like farming, research, and pet ownership without significant moral or legal encumbrance beyond basic anti-cruelty statutes.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_property_owners, beneficiary,
    powerful, generational, arbitrage, global).

% Utilizes animals or animal products for food, clothing, entertainment, or scientific research. Benefits from the availability of animals as resources, with their interests legally subordinate to human needs and desires. Operates within a framework that minimizes moral consideration for animals.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_resource_users, beneficiary,
    institutional, biographical, mobile, global).

% Codifies and enforces the legal status of animals as property, providing the foundational framework for their use. Benefits from the clarity and stability this definition provides for commerce and social practice. Changes to this status are slow and require significant legislative or judicial shifts.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, universal).

% Are legally defined as property, lacking independent moral standing or rights. Their interests are considered only insofar as they align with human interests or prevent waste/damage to property. They bear the full cost of instrumentalization without recourse.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animals_as_property, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(animal_moral_status__property_reading, animals_as_property).

% Argue for improved treatment of animals within the property framework, focusing on minimizing suffering. While their efforts may lead to minor regulatory changes, they do not challenge the fundamental property status of animals, which this reading upholds.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, welfare_advocates, excluded,
    organized, generational, constrained, global).

% Challenge the very concept of animals as property, advocating for their recognition as rights-bearing individuals. Their position is fundamentally foreclosed by this reading, which defines animals as property by definition. They operate outside the dominant legal and ethical framework this constraint represents.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, abolitionist_advocates, excluded,
    moderate, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal framework for human interaction with animals, enabling predictable economic activity, scientific research, and social practices by defining animals as property with subordinate interests.
% TRANSFER_FUNCTION: Transfers full control and use of animals to human owners/users, allowing for the extraction of resources (meat, milk, labor, research data) and services (companionship, entertainment) without significant moral or legal encumbrance.
% ABSENT_VOICES: Abolitionist advocates are structurally excluded from the conversation within this framework, as their core premise (animals as rights-bearing individuals) is foreclosed by the property definition. Animals themselves, lacking legal standing, are also absent from any direct voice.
% DISAPPEARANCE_RATIONALE: If the legal and social definition of animals as property vanished overnight, the global economy, scientific research, and many social practices (e.g., pet ownership, animal agriculture) would undergo a fundamental, immediate, and chaotic reorganization. The concept of 'animal use' as currently understood would cease to exist.
% FOUNDING_PROBLEM: To establish a clear, stable, and universally applicable framework for human dominion over the natural world, enabling resource utilization and social order without moral ambiguity regarding animal interests.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historical analyses attest to the foundational role of property rights in Western legal traditions, extending to animals. The continued reliance on animal products and services across global societies corroborates the ongoing 'live' status of the problem this framework solves for human interests. No corroboration from outside the benefiting parties for the 'naturalness' of this arrangement, only for its historical and legal persistence.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low because, from the perspective of this reading, the definition of animals as property is a foundational legal and ethical principle, not an extractive mechanism. The 'extraction' of resources from animals is seen as a natural consequence of their property status, not a cost imposed by a human-made constraint. Suppression is extremely high because the legal and social framework actively suppresses any independent moral or legal standing for animals, making alternatives (e.g., animal rights) almost completely inaccessible within this framework. Theater ratio is negligible as the system is highly functional in its stated purpose of enabling human use of animals.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of property owners, this is a natural and efficient system. From the perspective of abolitionist advocates, it is a system of profound extraction and violence. This constraint models the property owner's perspective, where the system is seen as a 'mountain' of natural law, not an extractive construct. The engine's classification will reflect this structural difference.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal property owners and resource users are direct beneficiaries, as the constraint grants them extensive rights and minimizes moral obligations. Legal systems act as agenda-setters, codifying and enforcing this status. Animals themselves are the ultimate payers, bearing the full cost of instrumentalization without legal recourse. Welfare and abolitionist advocates are excluded, as their positions challenge or are foreclosed by the core premise of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the definition of animals as property a natural law, or a social and legal construct that benefits identifiable human agents?',
    'Comparative legal anthropology across diverse cultures and historical periods, examining the variability of animal status and rights. Philosophical analysis of the ''naturalness'' of property concepts.',
    'If a social construct, the ''mountain'' classification would be a false summit, reclassifying to a ''tangled_rope'' or ''snare'' for the human beneficiaries, as the constraint would then be seen as actively maintained for their benefit rather than emerging naturally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of animal property status.').

omega_variable(
    scope_of_moral_consideration,
    'Does the ''property reading'' implicitly acknowledge any moral consideration for animals beyond preventing waste or damage to property, even if not legally codified?',
    'Analysis of ''anti-cruelty'' laws and their enforcement: do they reflect a nascent recognition of animal interests, or merely human sensibilities/property value? Public opinion surveys on animal welfare vs. rights.',
    'If implicit moral consideration is found, the ''property reading'' might be seen as having a subtle, unacknowledged ''welfare'' component, potentially shifting its extractiveness slightly upward (as some ''cost'' is borne by owners) or indicating a nascent drift towards the ''welfare_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_moral_consideration, empirical, 'Whether the property reading contains unacknowledged moral considerations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(anim_tr_t25, animal_moral_status__property_reading, theater_ratio, 25, 0.01).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__property_reading, theater_ratio, 50, 0.01).
narrative_ontology:measurement(anim_tr_t75, animal_moral_status__property_reading, theater_ratio, 75, 0.01).
narrative_ontology:measurement(anim_tr_t100, animal_moral_status__property_reading, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(anim_be_t25, animal_moral_status__property_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__property_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(anim_be_t75, animal_moral_status__property_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(anim_be_t100, animal_moral_status__property_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(anim_su_t25, animal_moral_status__property_reading, suppression_requirement, 25, 0.95).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__property_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(anim_su_t75, animal_moral_status__property_reading, suppression_requirement, 75, 0.95).
narrative_ontology:measurement(anim_su_t100, animal_moral_status__property_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
