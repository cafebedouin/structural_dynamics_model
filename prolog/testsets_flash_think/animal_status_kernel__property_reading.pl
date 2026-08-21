% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal Property Status (Property Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'property_reading' of the
 *   'animal_status_kernel', where animals are legally defined as property,
 *   and their moral considerability derives solely from ownership rights.
 *   Economic value is the only relevant value in this framework. This reading
 *   enables extensive human use and exploitation of animals without
 *   significant moral or legal impediment, leading to high extraction. The
 *   structural classification identifies animals as the primary target of
 *   this extraction, even though the reading itself denies their moral status
 *   as victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.92).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.88).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status (Property Reading)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '074a0173-72ad-47de-9a34-235b4c91db85').
narrative_ontology:cs_kernel_codification('074a0173-72ad-47de-9a34-235b4c91db85', formalized).
narrative_ontology:cs_authority_grounding('074a0173-72ad-47de-9a34-235b4c91db85', extraction).
narrative_ontology:cs_interpretation_layer_present('074a0173-72ad-47de-9a34-235b4c91db85').
narrative_ontology:cs_reading_relation('074a0173-72ad-47de-9a34-235b4c91db85', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('074a0173-72ad-47de-9a34-235b4c91db85', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('074a0173-72ad-47de-9a34-235b4c91db85', foundational, animals_as_chattel).
narrative_ontology:cs_axiom_status(animals_as_chattel, holdable).
narrative_ontology:cs_axiom_grounding('074a0173-72ad-47de-9a34-235b4c91db85', animals_as_chattel, conventional).
narrative_ontology:cs_axiom('074a0173-72ad-47de-9a34-235b4c91db85', foundational, moral_considerability_from_ownership).
narrative_ontology:cs_axiom_status(moral_considerability_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('074a0173-72ad-47de-9a34-235b4c91db85', moral_considerability_from_ownership, conventional).
narrative_ontology:cs_reference_frame('074a0173-72ad-47de-9a34-235b4c91db85', unrestricted_property_rights_framework).
narrative_ontology:cs_drift_state('074a0173-72ad-47de-9a34-235b4c91db85', contemporary_animal_ethics_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('074a0173-72ad-47de-9a34-235b4c91db85', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, legal_system).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, animals_as_property).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, anthropocentric_value_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess legal ownership of animals, granting them extensive rights to use, sell, or dispose of animals as they see fit. They benefit directly from the economic value derived from animals and the legal protection of their property rights. They actively defend this status.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_owners, agenda_setter,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, animal_owners, beneficiary).

% Industries (e.g., agriculture, pharmaceuticals, entertainment) that rely on the legal status of animals as property to conduct their operations, deriving significant economic value from animal products, labor, or research. They are primary beneficiaries of the lack of moral constraints on animal use.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_use_industries, beneficiary,
    institutional, generational, mobile, global).

% Codifies and enforces the property status of animals, providing the legal framework that enables ownership and use. It benefits from the stability and predictability this framework provides to economic activity, and from the revenue generated by industries operating within it. It resists challenges to this foundational legal premise.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Are legally defined as property, lacking moral or legal standing beyond their instrumental value to owners. They bear the full cost of this status, including exploitation, suffering, and death, without recourse. From a structural perspective, they are the primary target of extraction, even if the reading denies their moral agency.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animals_as_property, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__property_reading, animals_as_property).

% Challenge the property status of animals, arguing for their moral personhood and basic rights. They are largely excluded from the legal and economic frameworks that define animals as property, operating primarily through protest, education, and legislative lobbying for incremental changes. They observe the full extractive structure.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_rights_advocates, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, animal_rights_advocates, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__property_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_status_kernel__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, legally enforceable framework for human interaction with animals, enabling their commodification, trade, and use across various industries by defining them as property.
% TRANSFER_FUNCTION: Transfers the full economic and use value of animals from the animals themselves (as resources) to human owners and industries, without significant moral or legal encumbrance.
% ABSENT_VOICES: Animals themselves are structurally absent from the conversation, unable to articulate their interests. Animal rights advocates are largely excluded from the foundational legal and philosophical debates that uphold property status, their arguments often dismissed as outside the established framework.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, the global economy would undergo a profound and immediate reorganization. Industries reliant on animal use would collapse or be forced to radically transform, legal systems would need to redefine personhood and rights, and human-animal relationships would be fundamentally re-evaluated. The current arrangement is foundational to vast economic and social structures.
% FOUNDING_PROBLEM: To establish clear ownership and control over animals for human benefit, facilitating domestication, agriculture, and resource extraction, and resolving disputes over animal use.
% FOUNDING_PROBLEM_CORROBORATION: Animal owners and industries attest that the problem of managing animal resources and deriving economic value remains live. While animal rights advocates contest the legitimacy of the 'problem' itself, the legal system and historical precedent corroborate that the constraint was established to enable human dominion and resource utilization, which continues to be a 'live' function for its beneficiaries.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because animals are treated as resources with no inherent rights, allowing for maximal exploitation. Suppression is also very high (0.88) as legal and social structures actively suppress any challenge to this property status, making alternatives virtually inaccessible. Theater ratio is low (0.10) because the system is highly functional in its stated purpose of enabling animal use for human benefit; there is little performative maintenance masking a degraded function. The system actively enforces property rights and suppresses alternatives, making it a clear snare.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animal owners and industries, this constraint is a legitimate and necessary framework for resource management and economic activity. From the structural perspective of 'animals_as_property', it is a system of total extraction. Animal rights advocates experience it as a deeply unjust and oppressive system, actively suppressing their attempts to introduce alternative moral frameworks. The engine's classification captures this fundamental divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners and animal use industries are clear beneficiaries, directly profiting from the unrestricted use of animals. The legal system acts as an agenda-setter, codifying and enforcing this status, benefiting from the stability it provides. 'Animals_as_property' are the direct payers and victims, bearing the full cost of their commodification. Animal rights advocates are excluded from the core decision-making processes, acting as observers and external challengers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''animal_status_kernel''. What are the structural implications of this ''property_reading'' compared to its siblings?',
    'Comparative analysis with ''welfare_reading'' and ''abolitionist_reading'' constraints.',
    'This reading establishes animals as property, enabling high extraction. Sibling readings would either constrain this extraction (welfare) or abolish it entirely (abolitionist), leading to fundamentally different classifications and beneficiary/victim structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Identifies this constraint as a specific reading of the animal status kernel.').

omega_variable(
    moral_vs_structural_victimhood,
    'Does the structural classification of ''animals_as_property'' as victims contradict the ''property_reading''s'' denial of animal moral considerability?',
    'Clarification of framework definitions: ''victim'' in DR refers to the structural target of extraction, not necessarily a moral agent within the constraint''s own terms. The engine''s classification is independent of the reading''s internal moral claims.',
    'No change to classification. The divergence highlights the framework''s ability to detect structural extraction even when the constraint''s internal logic denies the moral standing of the extracted party.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_vs_structural_victimhood, conceptual, 'Addresses the tension between the reading''s moral claims and the framework''s structural classification of victims.').

omega_variable(
    anti_cruelty_statute_impact,
    'Do anti-cruelty statutes, which protect ''owner property value, not animal interests,'' introduce a hidden welfare constraint that subtly shifts this reading?',
    'Analysis of enforcement patterns: if anti-cruelty laws are primarily enforced to prevent economic loss to owners or public nuisance, they remain consistent with the property reading. If they are increasingly enforced based on animal sentience, it indicates a drift towards the welfare reading.',
    'If anti-cruelty laws are found to genuinely protect animal interests beyond property value, the extractiveness of this reading would be slightly lower, and its suppression might be marginally less absolute, indicating a subtle shift towards a ''tangled_rope'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_cruelty_statute_impact, empirical, 'Examines whether anti-cruelty laws introduce a hidden welfare dimension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__property_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__property_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__property_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__property_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__property_reading, base_extractiveness, 10, 0.91).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__property_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__property_reading, base_extractiveness, 30, 0.92).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__property_reading, base_extractiveness, 40, 0.92).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__property_reading, suppression_requirement, 10, 0.86).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__property_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__property_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__property_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_status_kernel'. Each reading represents a distinct structural constraint with different ε values and stakeholder positions, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
