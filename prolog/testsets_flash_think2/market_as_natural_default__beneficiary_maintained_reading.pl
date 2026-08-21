% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market as Natural Default (Beneficiary-Maintained Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint describes the active, post-hoc defense of the 'market as
 *   natural default' narrative by incumbent beneficiaries. It is one reading
 *   of the broader 'market_as_natural_default' kernel. This reading
 *   emphasizes that the naturalization is not a passive forgetting of
 *   alternatives, but an engineered closure maintained through PR,
 *   institutional capture, and the suppression of dissenting voices. The
 *   claimed type is 'tangled_rope' because it offers a coordination function
 *   (predictable economic framework) while simultaneously extracting benefits
 *   for specific actors through active enforcement of the narrative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.48).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.65).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market as Natural Default (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '0138befc-8b68-4eb1-8d8f-8e7167c4927b').
narrative_ontology:cs_kernel_codification('0138befc-8b68-4eb1-8d8f-8e7167c4927b', implicit).
narrative_ontology:cs_authority_grounding('0138befc-8b68-4eb1-8d8f-8e7167c4927b', extraction).
narrative_ontology:cs_interpretation_layer_present('0138befc-8b68-4eb1-8d8f-8e7167c4927b').
narrative_ontology:cs_reading_relation('0138befc-8b68-4eb1-8d8f-8e7167c4927b', market_as_natural_default__lapsed_alternative_reading, forecloses).
narrative_ontology:cs_reading_relation('0138befc-8b68-4eb1-8d8f-8e7167c4927b', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('0138befc-8b68-4eb1-8d8f-8e7167c4927b', foundational, market_outcomes_are_natural_and_efficient).
narrative_ontology:cs_axiom_status(market_outcomes_are_natural_and_efficient, holdable).
narrative_ontology:cs_axiom_grounding('0138befc-8b68-4eb1-8d8f-8e7167c4927b', market_outcomes_are_natural_and_efficient, conventional).
narrative_ontology:cs_axiom('0138befc-8b68-4eb1-8d8f-8e7167c4927b', secondary, active_defense_of_market_narrative_is_legitimate).
narrative_ontology:cs_axiom_status(active_defense_of_market_narrative_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('0138befc-8b68-4eb1-8d8f-8e7167c4927b', active_defense_of_market_narrative_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('0138befc-8b68-4eb1-8d8f-8e7167c4927b', unfettered_market_efficiency).
narrative_ontology:cs_drift_state('0138befc-8b68-4eb1-8d8f-8e7167c4927b', contemporary_regulatory_scrutiny, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0138befc-8b68-4eb1-8d8f-8e7167c4927b', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, incumbent_beneficiaries).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_policy_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large financial institutions, corporations, and their associated lobbying groups and think tanks. They actively fund and promote narratives that frame market outcomes as natural, efficient, and inevitable, thereby legitimizing their existing power and wealth. They benefit directly from reduced regulation and public acceptance of the status quo.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, incumbent_beneficiaries, agenda_setter,
    institutional, generational, arbitrage, global).

% Organizations and individuals advocating for market regulation, social safety nets, and alternative economic policies. They bear the cost of constantly challenging the 'natural' market narrative and face significant institutional and media resistance in proposing interventions or alternatives.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_policy_advocates, payer,
    organized, biographical, constrained, national).

% Individuals who experience the negative externalities of unregulated markets (e.g., wealth inequality, environmental degradation, precarious labor) but are often persuaded by or resigned to the 'natural' market narrative, limiting their collective capacity for resistance or demand for alternatives.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, general_public, payer,
    powerless, biographical, constrained, national).

% Academics and researchers who analyze the historical contingency and political construction of markets, challenging the 'natural default' narrative. Their work provides intellectual ammunition for policy advocates but often struggles for mainstream recognition against well-funded counter-narratives.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, critical_economists_historians, observer,
    analytical, generational, analytical, global).

% Proponents of non-capitalist or fundamentally different market structures (e.g., cooperative economics, degrowth, post-growth models). They are often marginalized in mainstream discourse and policy debates, their ideas suppressed by the dominant 'natural market' narrative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, alternative_economic_theorists, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for economic activity by presenting market outcomes as inevitable and efficient, reducing perceived need for intervention and coordinating expectations around existing structures.
% TRANSFER_FUNCTION: Transfers wealth, power, and legitimacy to incumbent beneficiaries by framing existing distributions as natural consequences of an efficient system, thereby suppressing challenges and calls for redistribution or regulation.
% ABSENT_VOICES: Alternative economic theorists and marginalized communities whose lived experiences contradict the 'natural' market narrative are systematically excluded from mainstream policy discourse, their perspectives dismissed as unrealistic or ideological.
% DISAPPEARANCE_RATIONALE: If the belief in the market as a natural default vanished overnight, it would fundamentally alter political and economic structures. Public demand for regulation, redistribution, and alternative economic models would surge, leading to widespread re-evaluation of property rights, corporate power, and the role of the state in the economy.
% FOUNDING_PROBLEM: To provide a coherent, self-justifying narrative for existing economic power structures and to resist calls for intervention or redistribution, particularly after periods of crisis or significant social change.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent beneficiaries and their associated think tanks attest the problem is still live, citing ongoing threats to 'economic freedom.' Critical economists and historians, from outside the benefiting parties, corroborate that the 'problem' is a constructed one, and the narrative's persistence serves specific interests, indicating the problem is 'live' for those who benefit from its 'solution'.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.48, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.48) reflects the substantial, though often indirect, transfer of wealth and power to incumbent beneficiaries by legitimizing existing economic structures. Suppression (0.65) is high due to the active, well-funded efforts to marginalize alternative economic theories and policy proposals, creating a 'natural' perception that limits public imagination and political will for change. The theater ratio (0.40) indicates that while some genuine coordination (e.g., stable expectations) exists, a significant portion of the activity is performative maintenance of the 'natural' narrative, rather than functional problem-solving. The metrics show a slight increase over time, reflecting the ongoing and intensifying effort to maintain this narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent beneficiaries, the 'market as natural default' is a self-evident truth that ensures efficiency and prosperity, requiring only 'education' to maintain. From the perspective of payers and excluded voices, it is a constructed ideology actively enforced to maintain an extractive status quo. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent beneficiaries are the agenda-setters and primary beneficiaries (d near 0.0), actively shaping and profiting from the narrative. Public policy advocates and the general public are payers (d near 1.0), bearing the costs of market outcomes and the effort required to challenge the dominant narrative. Critical economists and historians act as observers, while alternative economic theorists are excluded, their ideas suppressed by the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_passive_naturalization,
    'To what extent is the ''natural default'' status of markets maintained by active, post-hoc defense by beneficiaries versus a passive historical forgetting of alternatives?',
    'Detailed historical analysis of public relations campaigns, lobbying efforts, and academic funding by beneficiary groups, compared against periods of genuine historical amnesia where no active defense is evident.',
    'If primarily active defense, the constraint''s extractiveness and suppression are higher, supporting a Snare or Tangled Rope classification. If primarily passive forgetting, it leans towards a Piton or even a Mountain (if truly forgotten and inert).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_vs_passive_naturalization, empirical, 'Distinguishing active maintenance from passive historical amnesia in market naturalization.').

omega_variable(
    narrative_efficacy_vs_structural_power,
    'Is the persistence of the ''natural market'' narrative primarily due to its persuasive power (ideological capture) or the structural power of its beneficiaries (institutional capture)?',
    'Comparative case studies of jurisdictions where the narrative is weak but structural power remains, versus where the narrative is strong but structural power is challenged. Analysis of the causal pathways between narrative dissemination and policy outcomes.',
    'If primarily narrative efficacy, the suppression might be more internalized. If primarily structural power, the suppression is more external and coercive, potentially increasing the effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_efficacy_vs_structural_power, empirical, 'Assessing the relative contribution of ideological vs. institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1980, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(mark_tr_t2010, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(mark_tr_t2024, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(mark_be_t2010, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement(mark_be_t2024, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(mark_su_t2010, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(mark_su_t2024, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, hybrid_amnesia_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, financial_deregulation_narrative).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, corporate_personhood_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_as_natural_default' kernel. This reading emphasizes active, beneficiary-driven maintenance, contrasting with readings focused on passive historical forgetting or a hybrid of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
