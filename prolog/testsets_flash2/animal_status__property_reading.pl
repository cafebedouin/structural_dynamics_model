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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Status: Property Reading
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of animal status, where
 *   animals are legal objects without independent moral standing, and human
 *   ownership is largely unrestricted except by minimal welfare statutes.
 *   From this perspective, the constraint is a foundational 'mountain' of
 *   legal philosophy, with negligible extraction from animals (as they are
 *   not considered moral agents) and minimal suppression (as their status is
 *   largely uncontested within this framework). The beneficiaries are human
 *   animal owners and industries that rely on animal use. This is one reading
 *   of the 'animal_status' kernel.
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
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, mountain).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Status: Property Reading").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:emerges_naturally(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '02b209a4-f90f-4ca6-a184-fc719687cce5').
narrative_ontology:cs_kernel_codification('02b209a4-f90f-4ca6-a184-fc719687cce5', formalized).
narrative_ontology:cs_authority_grounding('02b209a4-f90f-4ca6-a184-fc719687cce5', lineage).
narrative_ontology:cs_interpretation_layer_present('02b209a4-f90f-4ca6-a184-fc719687cce5').
narrative_ontology:cs_reading_relation('02b209a4-f90f-4ca6-a184-fc719687cce5', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('02b209a4-f90f-4ca6-a184-fc719687cce5', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('02b209a4-f90f-4ca6-a184-fc719687cce5', foundational, animals_are_legal_objects).
narrative_ontology:cs_axiom_status(animals_are_legal_objects, holdable).
narrative_ontology:cs_axiom_grounding('02b209a4-f90f-4ca6-a184-fc719687cce5', animals_are_legal_objects, conventional).
narrative_ontology:cs_axiom('02b209a4-f90f-4ca6-a184-fc719687cce5', foundational, independent_moral_standing_requires_human_attributes).
narrative_ontology:cs_axiom_status(independent_moral_standing_requires_human_attributes, holdable).
narrative_ontology:cs_axiom_grounding('02b209a4-f90f-4ca6-a184-fc719687cce5', independent_moral_standing_requires_human_attributes, deontological).
narrative_ontology:cs_reference_frame('02b209a4-f90f-4ca6-a184-fc719687cce5', classical_property_law_framework).
narrative_ontology:cs_drift_state('02b209a4-f90f-4ca6-a184-fc719687cce5', contemporary_animal_welfare_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('02b209a4-f90f-4ca6-a184-fc719687cce5', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_use_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds legal title to animals, with rights to use, sell, or dispose of them, subject only to minimal welfare standards. Benefits from the clear legal status and lack of independent claims from animals.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_owners, beneficiary,
    powerful, biographical, arbitrage, local).

% Operates businesses (agriculture, research, entertainment) that depend on animals being property. Benefits from the legal framework that permits instrumental use and minimizes regulatory burden beyond basic welfare.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_use_industries, beneficiary,
    institutional, generational, mobile, national).

% Advocates for stronger welfare protections within the existing property framework. Their efforts are directed at human-to-human disputes over property treatment, not challenging the property status itself.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_advocates, observer,
    moderate, generational, constrained, national).

% Upholds and enforces the legal status of animals as property. Provides mechanisms for resolving disputes over ownership and for prosecuting violations of welfare statutes, which are framed as property damage or public nuisance.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear legal ownership and transferability of animals, facilitating their use in commerce, agriculture, and companionship by defining them as property.
% TRANSFER_FUNCTION: Legally transfers ownership rights and the benefits derived from animal use (e.g., food, labor, companionship) from animals (as objects) to human owners.
% ABSENT_VOICES: Animals themselves are structurally absent from the legal and ethical conversation, as they are defined as objects without standing. Abolitionist and strong welfare perspectives are also excluded from the core legal definition, operating as external critiques.
% DISAPPEARANCE_RATIONALE: If the legal status of animals as property vanished overnight, the entire legal and economic system built around animal use (agriculture, pet ownership, research) would collapse, requiring a fundamental redefinition of human-animal relationships and property law.
% FOUNDING_PROBLEM: To establish clear legal frameworks for ownership and use of animals, enabling their integration into human society and economy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historical texts corroborate the foundational role of property status in Western legal traditions. Animal owners and industries attest that this framework remains essential for their operations.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is near zero (0.05) because, within this reading, animals are not moral agents from whom extraction can occur; any 'cost' is a property dispute between humans. Suppression is also minimal (0.02) as the legal status is deeply embedded and requires little active enforcement against internal challenges. Accessibility collapse is high (0.95) because the property status fundamentally limits alternatives for animals. Resistance is low (0.05) as challenges to this core status come from outside this legal framework. The claimed type is 'mountain' because, from this perspective, the property status is a foundational, 'natural' legal fact.
 *
 * PERSPECTIVAL GAP:
 *   This reading's classification as a 'mountain' with negligible extraction would be fiercely contested by the 'welfare_reading' and 'abolitionist_reading' siblings, which would classify the same underlying arrangement as highly extractive and suppressive from the animals' perspective. This divergence is precisely what the kernel framework is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners and animal-use industries are clear beneficiaries, as the constraint grants them extensive rights and minimizes obligations. The legal system acts as the agenda-setter, maintaining this foundational status. Welfare advocates are observers, working within the property framework. Animals themselves are not stakeholders in this reading, as they lack legal standing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the ''property_reading'' of animal status a genuine natural law, or a constructed constraint that benefits identifiable human agents?',
    'Analysis of historical legal development and philosophical arguments for animal personhood vs. property status. If the property status is shown to be a contingent legal construct rather than an inherent feature of reality, it would be reclassified.',
    'If reclassified as a construct, the constraint would likely shift from ''mountain'' to ''tangled_rope'' or ''snare'' from the perspective of animals, with significantly higher extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between natural law and social construct for animal property status.').

omega_variable(
    moral_standing_definition,
    'Does the definition of ''moral standing'' inherently exclude animals, or is this exclusion a contingent feature of the ''property_reading''?',
    'Philosophical inquiry into the criteria for moral standing (e.g., sentience, consciousness, autonomy) and whether animals meet these criteria, independent of legal definitions.',
    'If animals are found to meet criteria for moral standing, the ''property_reading'' would be seen as actively suppressing their interests, leading to a re-evaluation of its extractiveness and suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_standing_definition, conceptual, 'Contingency of animal exclusion from moral standing.').


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

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
