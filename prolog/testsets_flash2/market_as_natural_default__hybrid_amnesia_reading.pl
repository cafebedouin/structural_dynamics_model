% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market as Natural Default (Hybrid Amnesia Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint describes the 'market as natural default' as a two-stage
 *   process: an initial period of genuine forgetting of alternatives (roughly
 *   1930s-1970s, post-Keynesian consensus) that created a 'lapsed closure,'
 *   followed by a period (1980s-present) where beneficiaries actively
 *   weaponized this pre-existing amnesia to increase extraction. The
 *   constraint is claimed as a Tangled Rope because it offers a coordination
 *   function (resource allocation) but is deeply intertwined with asymmetric
 *   extraction and requires active enforcement (ideological and policy-based)
 *   to maintain its 'natural' status. Extractiveness and suppression increase
 *   over time as the amnesia is leveraged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.65).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market as Natural Default (Hybrid Amnesia Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '6cf97418-786d-4114-a85e-1271ee0280b0').
narrative_ontology:cs_kernel_codification('6cf97418-786d-4114-a85e-1271ee0280b0', implicit).
narrative_ontology:cs_authority_grounding('6cf97418-786d-4114-a85e-1271ee0280b0', extraction).
narrative_ontology:cs_interpretation_layer_present('6cf97418-786d-4114-a85e-1271ee0280b0').
narrative_ontology:cs_reading_relation('6cf97418-786d-4114-a85e-1271ee0280b0', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('6cf97418-786d-4114-a85e-1271ee0280b0', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('6cf97418-786d-4114-a85e-1271ee0280b0', foundational, market_dominance_from_forgotten_alternatives).
narrative_ontology:cs_axiom_status(market_dominance_from_forgotten_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('6cf97418-786d-4114-a85e-1271ee0280b0', market_dominance_from_forgotten_alternatives, empirically_contingent).
narrative_ontology:cs_axiom('6cf97418-786d-4114-a85e-1271ee0280b0', foundational, amnesia_weaponized_by_incumbents).
narrative_ontology:cs_axiom_status(amnesia_weaponized_by_incumbents, holdable).
narrative_ontology:cs_axiom_grounding('6cf97418-786d-4114-a85e-1271ee0280b0', amnesia_weaponized_by_incumbents, empirically_contingent).
narrative_ontology:cs_reference_frame('6cf97418-786d-4114-a85e-1271ee0280b0', post_keynesian_consensus_era).
narrative_ontology:cs_drift_state('6cf97418-786d-4114-a85e-1271ee0280b0', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6cf97418-786d-4114-a85e-1271ee0280b0', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, market_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoliberal_economists).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, labor_movements).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_sector_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, marginalized_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Corporations and financial institutions that benefit from the market's perceived naturalness, allowing them to operate with minimal regulation and extract rents. They actively fund narratives that reinforce market inevitability.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, market_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Academics and policymakers whose careers and intellectual frameworks are built upon the assumption of market efficiency and naturalness. They provide intellectual justification for policies that reinforce the constraint.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoliberal_economists, beneficiary,
    organized, biographical, identity_locked, global).

% Bear the costs of deregulated labor markets, wage stagnation, and reduced social safety nets. They struggle to articulate alternatives due to the pervasive narrative of market inevitability.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, labor_movements, payer,
    organized, generational, constrained, national).

% Advocate for public goods and services but face an uphill battle against the narrative that private markets are inherently superior and more efficient, leading to underfunding and privatization.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_sector_advocates, payer,
    moderate, biographical, constrained, national).

% Disproportionately affected by market failures, austerity measures, and the erosion of social protections, with limited means to resist or articulate alternative economic arrangements.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, marginalized_communities, payer,
    powerless, immediate, trapped, local).

% Study the historical evolution of economic systems, identifying periods where alternatives were viable and how they were forgotten or suppressed. Their analysis often challenges the 'naturalness' narrative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a seemingly neutral framework for resource allocation and economic activity, reducing the perceived need for political intervention or collective decision-making in economic matters.
% TRANSFER_FUNCTION: Transfers wealth and power from labor and the public sector to capital and private interests, by framing market outcomes as efficient and inevitable rather than politically constructed.
% ABSENT_VOICES: Advocates for historical alternatives (e.g., social democracy, cooperative economics, planned economies) are marginalized in mainstream discourse, often dismissed as utopian or historically failed, preventing a full accounting of the market's constructed nature.
% DISAPPEARANCE_RATIONALE: If the perception of the market as a natural default vanished, it would expose the political choices underpinning current economic arrangements. This would lead to widespread demands for renegotiation of economic rules, redistribution of wealth, and re-evaluation of public vs. private spheres, fundamentally altering global political economy.
% FOUNDING_PROBLEM: The problem of coordinating complex economic activity and allocating resources efficiently in a post-industrial society.
% FOUNDING_PROBLEM_CORROBORATION: Market incumbents and neoliberal economists argue the market still solves this problem efficiently. Labor movements and economic historians argue that while coordination is necessary, the 'natural default' framing obscures power dynamics and suppresses more equitable solutions; historical evidence from outside the benefiting parties (e.g., post-WWII social democratic consensus) corroborates the existence of viable alternatives that were later 'forgotten'.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the substantial wealth transfer enabled by the market's perceived inevitability, allowing for deregulation and reduced social spending. Suppression (0.65) is high due to the pervasive ideological work (media, academia, policy think tanks) that actively marginalizes alternative economic models and frames market outcomes as 'natural' or 'efficient.' Theater ratio (0.20) is moderate; while there's genuine economic coordination, a significant portion of the 'market efficiency' narrative serves to obscure its constructed nature and extractive function. Accessibility collapse (0.70) is high because the dominant narrative makes it difficult to even conceive of viable alternatives, let alone implement them. Resistance (0.30) is present but fragmented, struggling against the powerful ideological current.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries perceive the market as a self-regulating, efficient system that naturally allocates resources, viewing any extraction as a fair return for risk or innovation. Victims experience it as a coercive force that limits their options and extracts their surplus, with the 'naturalness' narrative serving as a justification for their exploitation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Market incumbents and neoliberal economists are clear beneficiaries and agenda-setters, actively shaping the narrative and policy environment (low d). Labor movements, public sector advocates, and marginalized communities are victims, bearing the costs of this 'natural' market (high d). Economic historians act as observers, providing critical analysis but not directly participating in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (efficient resource allocation) is still 'live' but its 'natural default' framing has become a vehicle for extraction. The classification as Tangled Rope prevents mislabeling it as a pure Snare (ignoring the genuine coordination function) or a pure Rope (ignoring the asymmetric extraction and active suppression). The historical amnesia is key: it allows the extractive elements to be presented as inherent to the 'natural' market, rather than as constructed policy choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amnesia_vs_active_suppression,
    'What proportion of the market''s ''natural default'' status is due to genuine historical amnesia versus active, contemporary suppression of alternatives?',
    'Content analysis of economic discourse over time, tracking the frequency and framing of alternative economic models; ethnographic studies of policy-making processes to identify active suppression mechanisms.',
    'If amnesia is dominant, interventions might focus on historical education and re-articulation of alternatives. If active suppression is dominant, interventions would target lobbying, media capture, and policy advocacy, potentially reclassifying elements as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_vs_active_suppression, empirical, 'Distinguishing between passive forgetting and active ideological enforcement.').

omega_variable(
    counterfactual_alternative_viability,
    'Were the ''lapsed'' historical alternatives genuinely viable and equitable, or would they have led to other forms of extraction or inefficiency?',
    'Detailed counterfactual historical analysis and comparative studies of economic systems that pursued different paths, assessing their long-term social and economic outcomes.',
    'If alternatives are shown to be viable, it strengthens the case for the market as a constructed constraint. If not, it lends credence to the ''natural default'' narrative, potentially reducing the perceived extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_alternative_viability, empirical, 'Assessing the true viability of forgotten economic alternatives.').

omega_variable(
    framing_of_market_efficiency,
    'Is the concept of ''market efficiency'' primarily a descriptive economic concept or a normative ideological claim that serves to naturalize specific power relations?',
    'Philosophical analysis of economic theory, tracing the historical evolution of ''efficiency'' concepts and their deployment in policy debates; discourse analysis of how ''efficiency'' is used to justify or oppose interventions.',
    'If primarily descriptive, the constraint''s coordination function is stronger. If primarily normative/ideological, it reinforces the extractive and suppressive aspects, potentially shifting the classification closer to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_market_efficiency, conceptual, 'The conceptual grounding of ''market efficiency'' and its role in naturalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(mark_tr_t1985, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(mark_tr_t2024, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(mark_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(mark_be_t1985, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(mark_be_t2024, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.4).
narrative_ontology:measurement(mark_su_t1950, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(mark_su_t1985, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(mark_su_t2024, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, austerity_policy_mandate).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, privatization_of_public_services).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'market as natural default' kernel. It emphasizes a hybrid process of historical amnesia followed by active beneficiary maintenance. It influences and coexists with other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
