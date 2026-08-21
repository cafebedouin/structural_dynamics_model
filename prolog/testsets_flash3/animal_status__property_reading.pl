% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Status as Property (Property Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of animal status, where
 *   animals are legal objects without independent moral standing, and human
 *   ownership is largely unrestricted except by minimal welfare statutes.
 *   This reading treats the legal status as a foundational, almost natural,
 *   aspect of human-animal relations, enabling clear property rights and
 *   economic activity. The metrics reflect this: very low extractiveness
 *   (from the perspective of the legal system, it's just how things are),
 *   minimal suppression (as the status is deeply embedded), and negligible
 *   theater. This is one reading of the 'animal_status' kernel, distinct from
 *   welfare or abolitionist readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.02).
domain_priors:theater_ratio(animal_status__property_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, mountain).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Status as Property (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:emerges_naturally(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '4d020d84-d471-4944-8d1c-c070598f31fa').
narrative_ontology:cs_kernel_codification('4d020d84-d471-4944-8d1c-c070598f31fa', formalized).
narrative_ontology:cs_authority_grounding('4d020d84-d471-4944-8d1c-c070598f31fa', lineage).
narrative_ontology:cs_interpretation_layer_present('4d020d84-d471-4944-8d1c-c070598f31fa').
narrative_ontology:cs_reading_relation('4d020d84-d471-4944-8d1c-c070598f31fa', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d020d84-d471-4944-8d1c-c070598f31fa', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('4d020d84-d471-4944-8d1c-c070598f31fa', foundational, animals_are_chattel).
narrative_ontology:cs_axiom_status(animals_are_chattel, holdable).
narrative_ontology:cs_axiom_grounding('4d020d84-d471-4944-8d1c-c070598f31fa', animals_are_chattel, conventional).
narrative_ontology:cs_axiom('4d020d84-d471-4944-8d1c-c070598f31fa', foundational, human_dominion_over_animals).
narrative_ontology:cs_axiom_status(human_dominion_over_animals, holdable).
narrative_ontology:cs_axiom_grounding('4d020d84-d471-4944-8d1c-c070598f31fa', human_dominion_over_animals, theological).
narrative_ontology:cs_reference_frame('4d020d84-d471-4944-8d1c-c070598f31fa', classical_property_law).
narrative_ontology:cs_drift_state('4d020d84-d471-4944-8d1c-c070598f31fa', contemporary_animal_welfare_movement, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('4d020d84-d471-4944-8d1c-c070598f31fa', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_use_industries).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status__property_reading, human_exceptionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds full legal ownership over animals, with rights to use, sell, or dispose of them, limited only by minimal welfare statutes. Benefits from the clarity and enforceability of property rights.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_owners, beneficiary,
    powerful, biographical, arbitrage, local).

% Operates within a legal framework that treats animals as commodities, enabling large-scale production, research, and entertainment. Benefits from low regulatory burden and clear legal standing for their operations.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_use_industries, beneficiary,
    institutional, generational, mobile, national).

% Seeks to improve animal living conditions and reduce suffering, but operates within the legal constraint that animals are property. Their efforts are limited to advocating for minor statutory changes rather than challenging fundamental legal status.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_advocates, excluded,
    moderate, generational, constrained, national).

% Enforces property laws regarding animals, adjudicating disputes between human owners. Its function is to uphold the existing legal framework, not to grant independent standing to animals.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyzes the structural implications of animal legal status, noting how the property framework shapes economic activity, ethical discourse, and legal precedent. Operates outside the direct enforcement or benefit of the constraint.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, enforceable property rights over animals, facilitating their integration into economic systems and providing a stable legal basis for human-animal interactions.
% TRANSFER_FUNCTION: Legally transfers full control and use of animals to human owners, enabling the extraction of labor, products, and services from animals for human benefit.
% ABSENT_VOICES: Animals themselves, who lack legal standing to object. Also, abolitionist advocates, whose core premise of animal personhood is fundamentally excluded from this legal framework.
% DISAPPEARANCE_RATIONALE: If animals ceased to be legal property overnight, the entire structure of animal agriculture, research, and pet ownership would collapse. Economic systems, legal frameworks, and social norms would undergo a profound and immediate reorganization.
% FOUNDING_PROBLEM: To establish clear legal ownership and control over animals for human use, preventing disputes and facilitating their integration into human society and economy.
% FOUNDING_PROBLEM_CORROBORATION: The legal system and animal-use industries attest that clear property status for animals remains essential for economic stability and societal function. While welfare advocates contest the ethical implications, they do not deny the historical problem this framework solved for human society.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status__property_reading),
    narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and suppression (0.02) reflect the perspective of the legal system and property owners, for whom this status is a given, a 'natural law' of human society. The high accessibility collapse (0.95) and low resistance (0.08) further underscore its deeply embedded nature within this framework. The 'mountain' claim is made from this internal, property-centric perspective, where the legal status is seen as an unchangeable foundation, not a human construct. The beneficiaries are those who profit from animal use, and the vindicated propositions are core tenets of property law and human exceptionalism.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between this 'property reading' and alternative readings (welfare, abolitionist). While this reading sees the constraint as a natural, low-extraction 'mountain,' other readings would classify it as highly extractive and suppressive, with animals as primary victims. The engine's classification will highlight this divergence based on the structural data provided for each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   From the perspective of this reading, animal owners and industries are beneficiaries, as the constraint enables their activities. The legal system is the agenda-setter, upholding this framework. Welfare advocates are excluded, as their core concerns are external to the property framework. Animals themselves are not considered agents within this reading's scope, hence their absence from the victim set.
 *
 * MANDATROPHY ANALYSIS:
 *   From the 'property reading' perspective, there is no mandatrophy; the constraint's mandate (to define and enable property rights over animals) is fully live and functional. The problem it solves (establishing clear ownership) is considered ongoing and essential for societal order. Mandatrophy would only be detectable if the underlying societal need for animal property rights were to vanish, which this reading does not anticipate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the legal status of animals as property a ''natural law'' inherent to human society, or a social construct that could be revised?',
    'Comparative legal anthropology across diverse cultures and historical periods: if societies exist/existed where animals are not property, it''s a construct. If universal, it''s closer to natural law.',
    'If a construct, the ''mountain'' classification is a false summit, and the constraint would reclassify as a ''tangled_rope'' or ''snare'' from an external, critical perspective, with animals as victims. If natural, the mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between inherent and constructed nature of animal property status.').

omega_variable(
    victim_status_ambiguity,
    'Are animals ''victims'' of this constraint, even if this reading does not recognize their moral standing?',
    'Adoption of a ''welfare_reading'' or ''abolitionist_reading'' as the primary analytical frame: these frames explicitly recognize animal suffering and exploitation as ''victimization''.',
    'If animals are considered victims, the extractiveness of the constraint would be re-evaluated as significantly higher, and the classification would shift from ''mountain'' to ''snare'' or ''tangled_rope'' from those alternative perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_ambiguity, preference, 'Whether animals should be included in the victim set, depending on the adopted moral framework.').

omega_variable(
    property_rights_scope_ambiguity,
    'Does the ''property reading'' implicitly foreclose the possibility of any meaningful welfare protections, or can robust welfare standards coexist with property status?',
    'Analysis of legal precedents and legislative history in jurisdictions with strong animal welfare laws: if such laws consistently face challenges based on property rights, the foreclosure is strong. If they coexist without fundamental conflict, the relationship is ''coexists_with''.',
    'If property rights consistently override welfare concerns, the ''property reading'' forecloses the ''welfare_reading'' more strongly than currently assumed. If they can coexist, the relationship is less antagonistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_scope_ambiguity, empirical, 'The extent to which property rights inherently limit animal welfare protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(anim_tr_t25, animal_status__property_reading, theater_ratio, 25, 0.01).
narrative_ontology:measurement(anim_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.01).
narrative_ontology:measurement(anim_tr_t75, animal_status__property_reading, theater_ratio, 75, 0.01).
narrative_ontology:measurement(anim_tr_t100, animal_status__property_reading, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(anim_be_t25, animal_status__property_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(anim_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(anim_be_t75, animal_status__property_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(anim_be_t100, animal_status__property_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__property_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(anim_su_t25, animal_status__property_reading, suppression_requirement, 25, 0.02).
narrative_ontology:measurement(anim_su_t50, animal_status__property_reading, suppression_requirement, 50, 0.02).
narrative_ontology:measurement(anim_su_t75, animal_status__property_reading, suppression_requirement, 75, 0.02).
narrative_ontology:measurement(anim_su_t100, animal_status__property_reading, suppression_requirement, 100, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
