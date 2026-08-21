% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Animal Rights Status
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'abolitionist reading' of animal
 *   status, asserting that animals are rights-holders with inherent value
 *   that precludes all instrumental use. From this perspective, the current
 *   global system of animal exploitation is a profound violation of a moral
 *   'mountain'. The high extractiveness and suppression metrics reflect the
 *   severity of this violation, as animals are treated as property and
 *   subjected to systematic use. The claimed type is 'mountain' because the
 *   abolitionist position views animal rights as an inherent, unchangeable
 *   moral truth, not a social construct.
 *
 * KEY AGENTS:
 *   - animals_used_instrumentally: Primary target (powerless/trapped) — bears full extraction
 *   - abolitionist_advocates: Agenda setter (organized/constrained) — pushes for the constraint's recognition
 *   - animal_product_industries: Primary beneficiary (institutional/mobile) — benefits from the violation of the constraint
 *   - research_institutions: Primary beneficiary (institutional/mobile) — benefits from the violation of the constraint
 *   - pet_industry: Primary beneficiary (organized/mobile) — benefits from the violation of the constraint
 *   - welfare_advocates: Excluded (organized/constrained) — seen as legitimizing instrumental use from this reading's perspective
 *   - legal_systems: Agenda setter (institutional/constrained) — currently enforces the violation, could enforce the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, mountain).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Animal Rights Status").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).
domain_priors:emerges_naturally(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'c68ab7af-c5d6-4908-aa21-f220154c5d9b').
narrative_ontology:cs_kernel_codification('c68ab7af-c5d6-4908-aa21-f220154c5d9b', implicit).
narrative_ontology:cs_authority_grounding('c68ab7af-c5d6-4908-aa21-f220154c5d9b', diffuse_epistemic).
narrative_ontology:cs_reading_relation('c68ab7af-c5d6-4908-aa21-f220154c5d9b', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('c68ab7af-c5d6-4908-aa21-f220154c5d9b', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('c68ab7af-c5d6-4908-aa21-f220154c5d9b', foundational, animals_are_sentient_beings).
narrative_ontology:cs_axiom_status(animals_are_sentient_beings, holdable).
narrative_ontology:cs_axiom_grounding('c68ab7af-c5d6-4908-aa21-f220154c5d9b', animals_are_sentient_beings, empirically_contingent).
narrative_ontology:cs_axiom('c68ab7af-c5d6-4908-aa21-f220154c5d9b', foundational, sentience_confers_rights_precluding_use).
narrative_ontology:cs_axiom_status(sentience_confers_rights_precluding_use, holdable).
narrative_ontology:cs_axiom_grounding('c68ab7af-c5d6-4908-aa21-f220154c5d9b', sentience_confers_rights_precluding_use, deontological).
narrative_ontology:cs_reference_frame('c68ab7af-c5d6-4908-aa21-f220154c5d9b', inherent_moral_status).
narrative_ontology:cs_drift_state('c68ab7af-c5d6-4908-aa21-f220154c5d9b', contemporary_instrumental_use, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c68ab7af-c5d6-4908-aa21-f220154c5d9b', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_product_industries).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, pet_industry).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_used_instrumentally).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the full cost of instrumental use, including suffering, confinement, and death. From the abolitionist perspective, they are completely suppressed and have no exit options from their status as property or resources.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_used_instrumentally, payer,
    powerless, immediate, trapped, universal).

% Actively campaign for the recognition of animal rights and the end of all instrumental use. They face significant institutional and economic resistance from industries benefiting from the current system.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, generational, constrained, global).

% Profit from the instrumental use of animals for food, clothing, and other products. They actively resist changes to animal status that would preclude their business model, benefiting from the current lack of rights for animals.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_product_industries, beneficiary,
    institutional, biographical, mobile, global).

% Benefit from the use of animals in scientific research and testing. They resist changes that would restrict their access to animal subjects, benefiting from the current lack of rights for animals.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, research_institutions, beneficiary,
    institutional, biographical, mobile, global).

% Profits from the breeding, sale, and commodification of companion animals. This industry benefits from the property status of animals, which the abolitionist reading fundamentally opposes.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, pet_industry, beneficiary,
    organized, biographical, mobile, global).

% Seek to improve the conditions of animals within instrumental use frameworks. From an abolitionist perspective, their efforts, while well-intentioned, are seen as legitimizing the underlying property status and thus are excluded from the true solution of ending all instrumental use.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_advocates, excluded,
    organized, biographical, constrained, global).

% Currently uphold the property status of animals and regulate their use. They could be reformed to recognize animal rights, but currently serve to enforce the instrumental use framework.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist perspective, the current arrangement coordinates the systematic instrumental use of animals across various industries and research, ensuring a stable supply chain and legal framework for their exploitation. This 'coordination' is the violation of the inherent rights of animals.
% TRANSFER_FUNCTION: Transfers the inherent value, bodily autonomy, and lives of animals to human benefit, primarily economic and scientific, by treating animals as property or resources.
% ABSENT_VOICES: The animals themselves are the primary absent voices, unable to articulate their interests or consent. Future generations, who might inherit a world with different moral norms regarding animals, are also absent from the current decision-making processes.
% DISAPPEARANCE_RATIONALE: If the constraint (animals as rights-holders precluding instrumental use) were universally recognized and enforced overnight, the global animal agriculture, research, and pet industries would collapse or undergo radical transformation, leading to massive economic and social reorganization. Legal systems would need fundamental re-evaluation of property and personhood.
% FOUNDING_PROBLEM: The historical problem was the perceived need for human dominion over nature and the instrumental use of animals for survival, convenience, and scientific advancement, without moral consideration for their sentience or inherent value.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist philosophers and ethicists argue that the 'problem' of animal use is a moral failure, not a necessity, and that the founding problem is 'dead' or based on flawed anthropocentric premises. Industries and many consumers, however, maintain that animal products and research are essential, thus claiming the problem is 'live'. Independent corroboration is primarily philosophical and ethical, challenging the historical framing.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status__abolitionist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status__abolitionist_reading),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.95) is extremely high because the current system extracts everything from animals, including their lives and autonomy, for human benefit. Suppression (0.98) is near-total, as animals are legally and physically unable to resist their instrumentalization. The theater ratio (0.10) is low because, from an abolitionist perspective, 'welfare' reforms are largely performative, serving to legitimize the underlying exploitation rather than genuinely addressing the fundamental rights violation. The claimed type is 'mountain' because the inherent value and rights of animals are considered a moral truth that should emerge naturally, regardless of human recognition or enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the abolitionist's view of animal rights as an inherent moral mountain and the dominant societal view that treats animals as property or resources. The engine's classification will highlight this divergence, showing a claimed mountain operating as a highly extractive snare from the perspective of the animals and abolitionist advocates.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the full targets of the current system (high d), bearing all costs. Industries that profit from animal use are the full beneficiaries (low d), as they gain from the absence of this constraint. Abolitionist advocates are agenda-setters, pushing for the constraint's recognition. Welfare advocates are 'excluded' because their approach, while aiming to reduce suffering, is seen by abolitionists as implicitly validating the instrumental use of animals, thus diverting attention from the core rights issue.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (inherent rights) is not yet widely recognized or enforced. Instead, the current system operates as a snare, actively suppressing the inherent rights of animals. The analysis here focuses on the gap between a moral imperative (the claimed mountain) and the prevailing extractive reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the inherent value and rights of animals a natural moral law (a genuine mountain) or a constructed moral claim that requires societal consensus and enforcement?',
    'Continued philosophical debate, shifts in public moral intuition, and eventual legal codification. If widely adopted and enforced, it would function as a constructed mountain.',
    'If resolved as a constructed claim, its persistence depends on active societal maintenance rather than inherent truth, potentially reclassifying it as a Rope or Tangled Rope if coordination is involved, or a Snare if enforcement is coercive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity regarding the ontological status of animal rights.').

omega_variable(
    sentience_definition_ambiguity,
    'What constitutes sentience, and where is the precise boundary for conferring rights? Does it extend to all organisms, or only those with specific neurological structures?',
    'Ongoing scientific research in animal cognition and neurobiology, combined with philosophical refinement of criteria for moral consideration.',
    'A narrower definition of sentience would reduce the scope of ''animals used instrumentally'' and potentially reduce the perceived extractiveness for certain categories of organisms. A broader definition would expand the victim set and increase the perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_definition_ambiguity, empirical, 'Ambiguity in the scientific and philosophical definition of sentience and its moral implications.').

omega_variable(
    moral_status_enforcement_mechanism,
    'How would a universal moral mountain of animal rights be practically enforced in a world where instrumental use is deeply embedded in economic and social structures?',
    'Development of comprehensive legal frameworks, international treaties, and societal shifts in consumption and production patterns. This is a long-term, multi-generational project.',
    'The feasibility and nature of enforcement would determine whether the realized constraint would be a genuine Rope (coordination), a Tangled Rope (coordination with extraction from non-compliant actors), or a Snare (coercive imposition).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_enforcement_mechanism, preference, 'Uncertainty about the practical implementation and enforcement of animal rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t2000, animal_status__abolitionist_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(anim_tr_t2005, animal_status__abolitionist_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(anim_tr_t2010, animal_status__abolitionist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(anim_tr_t2015, animal_status__abolitionist_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(anim_tr_t2020, animal_status__abolitionist_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(anim_tr_t2025, animal_status__abolitionist_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(anim_tr_t2030, animal_status__abolitionist_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t2000, animal_status__abolitionist_reading, base_extractiveness, 2000, 0.92).
narrative_ontology:measurement(anim_be_t2005, animal_status__abolitionist_reading, base_extractiveness, 2005, 0.93).
narrative_ontology:measurement(anim_be_t2010, animal_status__abolitionist_reading, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(anim_be_t2015, animal_status__abolitionist_reading, base_extractiveness, 2015, 0.94).
narrative_ontology:measurement(anim_be_t2020, animal_status__abolitionist_reading, base_extractiveness, 2020, 0.95).
narrative_ontology:measurement(anim_be_t2025, animal_status__abolitionist_reading, base_extractiveness, 2025, 0.95).
narrative_ontology:measurement(anim_be_t2030, animal_status__abolitionist_reading, base_extractiveness, 2030, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t2000, animal_status__abolitionist_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(anim_su_t2005, animal_status__abolitionist_reading, suppression_requirement, 2005, 0.96).
narrative_ontology:measurement(anim_su_t2010, animal_status__abolitionist_reading, suppression_requirement, 2010, 0.97).
narrative_ontology:measurement(anim_su_t2015, animal_status__abolitionist_reading, suppression_requirement, 2015, 0.97).
narrative_ontology:measurement(anim_su_t2020, animal_status__abolitionist_reading, suppression_requirement, 2020, 0.98).
narrative_ontology:measurement(anim_su_t2025, animal_status__abolitionist_reading, suppression_requirement, 2025, 0.98).
narrative_ontology:measurement(anim_su_t2030, animal_status__abolitionist_reading, suppression_requirement, 2030, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_status' kernel, each representing a distinct structural claim about the moral and legal standing of animals. This abolitionist reading asserts inherent rights precluding all instrumental use, contrasting with the welfare and property readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
