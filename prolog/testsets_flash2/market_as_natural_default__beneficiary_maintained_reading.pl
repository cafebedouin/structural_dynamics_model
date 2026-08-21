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
 *   This constraint describes the active, post-hoc defense of market
 *   naturalization by incumbent beneficiaries. It is a reading of the 'market
 *   as natural default' kernel, focusing on how powerful actors engineer the
 *   perception of naturalness through ideological work, lobbying, and
 *   suppression of alternatives. The constraint is claimed as a Tangled Rope
 *   because it offers a coordination function (a stable framework for
 *   economic activity) but is characterized by significant, actively
 *   maintained extraction and suppression, benefiting specific groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.48).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.75).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market as Natural Default (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '37d83ed7-772d-4a18-9987-c07c5043fec0').
narrative_ontology:cs_kernel_codification('37d83ed7-772d-4a18-9987-c07c5043fec0', implicit).
narrative_ontology:cs_authority_grounding('37d83ed7-772d-4a18-9987-c07c5043fec0', extraction).
narrative_ontology:cs_interpretation_layer_present('37d83ed7-772d-4a18-9987-c07c5043fec0').
narrative_ontology:cs_reading_relation('37d83ed7-772d-4a18-9987-c07c5043fec0', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('37d83ed7-772d-4a18-9987-c07c5043fec0', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('37d83ed7-772d-4a18-9987-c07c5043fec0', foundational, market_efficiency_is_natural).
narrative_ontology:cs_axiom_status(market_efficiency_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('37d83ed7-772d-4a18-9987-c07c5043fec0', market_efficiency_is_natural, empirically_contingent).
narrative_ontology:cs_axiom('37d83ed7-772d-4a18-9987-c07c5043fec0', foundational, alternatives_are_inefficient_or_unfeasible).
narrative_ontology:cs_axiom_status(alternatives_are_inefficient_or_unfeasible, holdable).
narrative_ontology:cs_axiom_grounding('37d83ed7-772d-4a18-9987-c07c5043fec0', alternatives_are_inefficient_or_unfeasible, empirically_contingent).
narrative_ontology:cs_reference_frame('37d83ed7-772d-4a18-9987-c07c5043fec0', self_regulating_market_paradigm).
narrative_ontology:cs_drift_state('37d83ed7-772d-4a18-9987-c07c5043fec0', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37d83ed7-772d-4a18-9987-c07c5043fec0', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_sector).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, large_corporations).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, economic_policy_elites).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, labor_movements).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, small_businesses).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_sector_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively funds think tanks, lobbying efforts, and media campaigns to promote the idea of market mechanisms as the 'natural' or 'most efficient' default for resource allocation, thereby defending its privileged position and resisting regulation. Benefits directly from reduced oversight and expanded market scope.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_sector, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the naturalization of market principles, which often translates into lower labor costs, reduced environmental regulations, and less public scrutiny of their operations. They support the narrative through corporate philanthropy and public relations.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, large_corporations, beneficiary,
    powerful, biographical, mobile, global).

% Academics, policymakers, and advisors whose careers and intellectual frameworks are built around market-centric models. They actively articulate and defend the 'naturalness' of markets, often dismissing alternatives as inefficient or utopian, thereby maintaining their influence and professional standing.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, economic_policy_elites, agenda_setter,
    institutional, generational, identity_locked, national).

% Bear the costs of market naturalization through suppressed wages, weakened collective bargaining power, and the erosion of social safety nets. They actively resist this narrative but face significant institutional and ideological barriers.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, labor_movements, payer,
    organized, generational, constrained, national).

% Often struggle to compete against large, naturalized market players, facing unfavorable regulatory environments and limited access to capital. They are victims of the 'natural' market's scale effects but lack the collective power to challenge the dominant narrative effectively.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, small_businesses, payer,
    moderate, biographical, constrained, local).

% Advocate for public goods and services, but find their proposals consistently framed as 'unnatural' or 'inefficient' interventions in a naturally self-regulating market. They bear the cost of ideological suppression and reduced public investment.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_sector_advocates, payer,
    moderate, biographical, constrained, national).

% Research and document historical periods and societies where non-market forms of allocation were dominant or coexisted with markets, providing evidence against the 'natural default' claim. Their work challenges the ideological foundations of the constraint.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, historical_alternatives_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates economic activity by establishing a common, seemingly inevitable framework for resource allocation, reducing friction for incumbent market actors by delegitimizing alternative systems.
% TRANSFER_FUNCTION: Transfers wealth and power from labor, small enterprises, and the public sector to large corporations and the financial sector, by framing market outcomes as natural and therefore beyond political contestation.
% ABSENT_VOICES: Advocates for planned economies, cooperative models, and strong public provisioning are systematically marginalized in mainstream economic discourse, their perspectives dismissed as 'unrealistic' or 'ideological' by the very institutions that benefit from market naturalization.
% DISAPPEARANCE_RATIONALE: If the belief in markets as a natural default vanished overnight, the political and economic landscape would fundamentally shift. Debates over resource allocation would open to a wider range of alternatives, leading to significant re-regulation, re-nationalization, and a re-evaluation of corporate power, fundamentally altering the distribution of wealth and influence.
% FOUNDING_PROBLEM: To provide a stable, self-regulating framework for economic activity that minimizes political interference and maximizes efficiency, particularly in the post-feudal and industrial eras.
% FOUNDING_PROBLEM_CORROBORATION: Economic policy elites and the financial sector claim the problem of inefficient allocation and political interference is still live. Labor movements and historical alternatives scholars argue the 'problem' was largely a justification for consolidating power and that the current arrangement primarily serves incumbent interests, with corroboration from historical analysis and critical economic theory outside the benefiting parties.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is moderate-to-high (0.48) because the 'natural default' narrative allows incumbent beneficiaries to capture rents by limiting competition and externalizing costs, framing these outcomes as inevitable. Suppression (0.75) is high because the constraint relies on actively delegitimizing and marginalizing alternative economic models and voices. Theater ratio (0.60) is significant, reflecting the performative aspects of 'market efficiency' arguments and the ideological work required to maintain the illusion of naturalness, even as the underlying coordination function becomes secondary to extraction. The metrics show a clear trend of increasing extractiveness and theatricality over the interval, indicating a drift from a more balanced coordination to a more extractive, ideologically maintained structure.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries perceive this as a legitimate, efficient, and even 'natural' system that coordinates complex economic activity. Payers experience it as an imposed, extractive system that suppresses alternatives and concentrates wealth. The engine's classification will highlight this divergence, showing a claimed 'Tangled Rope' that operates with high extraction and suppression from the payer's seat, while the agenda-setters may perceive it as a more benign 'Rope' or even a 'Mountain' of economic law.
 *
 * DIRECTIONALITY LOGIC:
 *   The financial sector and large corporations are clear beneficiaries and agenda-setters, actively shaping the narrative and policy environment (low directionality). Economic policy elites, whose professional identities are tied to market fundamentalism, also act as agenda-setters (identity_locked exit, low directionality). Labor movements, small businesses, and public sector advocates are payers, bearing the costs of this naturalization through reduced power and resources (high directionality). Historical alternatives scholars act as observers, analyzing the structural dynamics without direct participation in the extraction or payment.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as pure extraction by acknowledging the initial coordination function (providing a stable economic framework). However, the high extractiveness and suppression, coupled with the rising theater ratio, indicate that the mandate has drifted significantly from pure coordination towards active rent-seeking and ideological maintenance. The 'beneficiary-maintained' reading emphasizes that the persistence is not due to inherent naturalness but to active defense by those who profit, preventing it from being mislabeled as a Piton (where no party benefits enough to maintain it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is the ''market as natural default'' a genuine emergent property of human interaction, or a constructed ideological framework actively maintained by beneficiaries?',
    'Comparative historical analysis of diverse economic systems, and empirical studies of the funding and influence of market-naturalizing institutions (e.g., think tanks, media outlets).',
    'If genuinely emergent, the constraint would lean towards a Mountain or Rope. If constructed and maintained, it reinforces the Tangled Rope classification, highlighting the active enforcement and ideological work required for its persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, conceptual, 'Ambiguity between natural economic law and ideologically constructed norm.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternatives structural (e.g., legal barriers, resource control) or internalized (e.g., belief in TINA - There Is No Alternative)?',
    'Post-exit trajectory of suppressed alternatives: if alternatives emerge rapidly after structural barriers are removed, suppression was primarily structural. If ideological resistance persists, internalized suppression is significant.',
    'If internalized, the effective suppression is higher than structural measures suggest, as agents carry the suppression with them. This would amplify the extractive nature of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for economic alternatives.').

omega_variable(
    kernel_reading_difference,
    'How would the classification change if the ''lapsed_alternative_reading'' or ''hybrid_amnesia_reading'' of the ''market_as_natural_default'' kernel were adopted?',
    'Detailed structural analysis of each sibling reading, focusing on their distinct beneficiary/victim sets, enforcement mechanisms, and temporal dynamics, as per the ε-invariance principle.',
    'The ''lapsed_alternative_reading'' might suggest a lower suppression requirement and a more ''Piton''-like drift, while the ''hybrid_amnesia_reading'' might emphasize a more complex, multi-stage evolution of extractiveness. This ''beneficiary_maintained_reading'' emphasizes active, ongoing defense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1980, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(mark_tr_t2010, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(mark_tr_t2024, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(mark_be_t2010, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(mark_be_t2024, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(mark_su_t2010, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(mark_su_t2024, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, hybrid_amnesia_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, lapsed_alternative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_as_natural_default' kernel. This 'beneficiary_maintained_reading' emphasizes active, post-hoc defense by incumbents. It influences and coexists with the 'lapsed_alternative_reading' (focus on historical forgetting) and the 'hybrid_amnesia_reading' (focus on initial lapsed closure creating conditions for capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
