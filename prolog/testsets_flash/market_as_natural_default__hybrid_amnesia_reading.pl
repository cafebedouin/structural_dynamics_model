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
 *   process: an initial period (roughly 1930s-1970s) where alternatives to
 *   market-based solutions were genuinely forgotten or lapsed due to
 *   historical contingencies (e.g., post-war consensus, Cold War ideological
 *   framing), followed by a period (1980s-present) where identifiable
 *   beneficiaries actively maintained and weaponized this pre-existing
 *   amnesia to extract rents. The constraint is claimed as a Tangled Rope
 *   because it combines a genuine (if historically contingent) coordination
 *   function (efficient allocation) with asymmetric extraction enabled by the
 *   suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.6).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.6).
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
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '27fd31b5-1724-46f2-a489-142e9b37bcf2').
narrative_ontology:cs_kernel_codification('27fd31b5-1724-46f2-a489-142e9b37bcf2', implicit).
narrative_ontology:cs_authority_grounding('27fd31b5-1724-46f2-a489-142e9b37bcf2', extraction).
narrative_ontology:cs_interpretation_layer_present('27fd31b5-1724-46f2-a489-142e9b37bcf2').
narrative_ontology:cs_reading_relation('27fd31b5-1724-46f2-a489-142e9b37bcf2', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('27fd31b5-1724-46f2-a489-142e9b37bcf2', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('27fd31b5-1724-46f2-a489-142e9b37bcf2', foundational, market_dominance_from_historical_amnesia).
narrative_ontology:cs_axiom_status(market_dominance_from_historical_amnesia, holdable).
narrative_ontology:cs_axiom_grounding('27fd31b5-1724-46f2-a489-142e9b37bcf2', market_dominance_from_historical_amnesia, empirically_contingent).
narrative_ontology:cs_axiom('27fd31b5-1724-46f2-a489-142e9b37bcf2', foundational, amnesia_enables_beneficiary_capture).
narrative_ontology:cs_axiom_status(amnesia_enables_beneficiary_capture, holdable).
narrative_ontology:cs_axiom_grounding('27fd31b5-1724-46f2-a489-142e9b37bcf2', amnesia_enables_beneficiary_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('27fd31b5-1724-46f2-a489-142e9b37bcf2', post_war_consensus_market_efficiency).
narrative_ontology:cs_drift_state('27fd31b5-1724-46f2-a489-142e9b37bcf2', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27fd31b5-1724-46f2-a489-142e9b37bcf2', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, capital_owners).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, corporate_executives).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, labor_unions).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, social_democratic_parties).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_sector_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the default status of market mechanisms, which favors capital accumulation and limits state intervention. They actively lobby for policies that reinforce this default and suppress alternatives.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, capital_owners, beneficiary,
    powerful, generational, arbitrage, global).

% Their careers and compensation are tied to the success of market-driven enterprises. They benefit from the ideological framing that presents market solutions as inevitable and superior, reducing pressure for alternative models.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, corporate_executives, beneficiary,
    organized, biographical, mobile, global).

% Bear the costs of market liberalization, deregulation, and the erosion of collective bargaining power. They struggle to advocate for non-market solutions or stronger social safety nets against the 'market as natural' narrative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, labor_unions, payer,
    organized, generational, constrained, national).

% Their policy platforms, which often involve significant state intervention and public provision, are delegitimized by the 'market as natural' default. They face an uphill battle in advocating for alternatives.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, social_democratic_parties, payer,
    institutional, generational, constrained, national).

% Advocate for public services and non-market solutions to social problems. They find their proposals consistently framed as inefficient or unnatural compared to market-based approaches, limiting their influence and resource access.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_sector_advocates, payer,
    moderate, biographical, constrained, local).

% Analyze the historical development of economic systems and the contingency of market dominance. They provide critical analysis of the 'naturalness' claim but have limited direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, capital_owners).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates resource allocation and production through price signals and competition, providing a decentralized mechanism for economic activity.
% TRANSFER_FUNCTION: Transfers wealth and power from labor and the public sector to capital owners and corporate entities, by framing market-based solutions as the default and suppressing alternatives.
% ABSENT_VOICES: Advocates for planned economies, cooperative models, and robust public provisioning are marginalized or dismissed as 'unrealistic' or 'ideological,' preventing a full public debate on economic alternatives.
% DISAPPEARANCE_RATIONALE: If the 'market as natural default' narrative and its associated enforcement mechanisms vanished, there would be a rapid re-evaluation of economic policy, a surge in proposals for alternative systems, and a significant shift in power dynamics, leading to a fundamental reorganization of economic and political structures.
% FOUNDING_PROBLEM: The problem of efficient resource allocation and wealth creation in complex societies, particularly after the perceived failures of centrally planned economies and the need for post-war reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: Capital owners and corporate executives claim the problem is still live, requiring market dominance for prosperity. Economic historians and critical theorists, from outside the benefiting parties, argue that while resource allocation remains a problem, the 'natural default' framing has outlived its utility and now serves primarily extractive ends, with viable alternatives being suppressed.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).

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
 *   Extractiveness (0.45) is moderate but rising, reflecting the increasing rent-seeking behavior by beneficiaries over time. Suppression (0.6) is significant, as the 'naturalness' narrative actively discourages and delegitimizes alternative economic models. Theater ratio (0.2) is low, indicating that while there's some performative defense of the market's 'naturalness,' the underlying mechanisms are genuinely functional for resource allocation, even if skewed. The increasing extractiveness and suppression over time, as shown in the measurements, reflect the transition from passive amnesia to active beneficiary capture.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of capital owners and corporate executives, the market's default status is a natural, efficient, and beneficial arrangement. From the perspective of labor unions and public sector advocates, it is a constructed constraint that systematically disadvantages them, maintained through ideological and political means. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Capital owners and corporate executives are clear beneficiaries (d near 0.0) as they profit directly from the market's default status and the suppression of alternatives. Labor unions, social democratic parties, and public sector advocates are victims (d near 1.0) as their policy goals and power are systematically undermined by this default. The general public is a mixed bag, experiencing some coordination benefits but also bearing the costs of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (efficient resource allocation) has been increasingly co-opted by the beneficiaries. The initial 'lapsed closure' phase might have been closer to a Piton or even a Rope, but the subsequent 'beneficiary capture' phase has shifted it to a Tangled Rope. The classification prevents mislabeling the current extractive state as merely 'natural' or 'efficient coordination' by highlighting the active enforcement and identifiable victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amnesia_vs_active_suppression,
    'To what extent is the ''naturalness'' of the market a result of genuine historical amnesia versus active, ongoing suppression of alternatives by beneficiaries?',
    'Historical analysis of policy debates and public discourse: quantify the proportion of arguments for market default based on ''no alternative'' (amnesia) versus ''alternatives are bad'' (active suppression).',
    'If primarily amnesia, the constraint is more ''piton-like'' in its persistence; if primarily active suppression, it is more ''snare-like'' and requires more direct intervention to dismantle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_vs_active_suppression, empirical, 'Distinguishing between historical forgetting and active suppression in maintaining market default.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''hybrid_amnesia_reading'' of the ''market_as_natural_default'' kernel. How would the classification change under the ''lapsed_alternative_reading'' or ''beneficiary_maintained_reading''?',
    'Analyzing the counterfactuals: the ''lapsed_alternative_reading'' would likely yield a lower extractiveness and suppression, leaning towards a Piton or even a degraded Rope, as it emphasizes passive forgetting. The ''beneficiary_maintained_reading'' would likely yield higher extractiveness and suppression, leaning more strongly towards a Snare, as it emphasizes active, conscious defense of the status quo.',
    'The ''hybrid_amnesia_reading'' captures the transition from passive forgetting to active maintenance, resulting in a Tangled Rope classification. Other readings would shift the balance of extraction and suppression, altering the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative readings of the ''market_as_natural_default'' kernel on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'market_as_natural_default' kernel. It emphasizes a hybrid process of initial historical amnesia followed by active beneficiary capture. Other readings focus on passive forgetting or active, post-hoc defense.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
