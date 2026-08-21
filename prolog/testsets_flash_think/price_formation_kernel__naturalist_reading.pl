% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__naturalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Naturalist Reading of Price Formation as Equilibrium
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'naturalist' reading of price formation,
 *   which posits that prices emerge from a natural equilibrium between
 *   objective scarcity and subjective preferences. From this perspective,
 *   prices are discovered, not constructed, and policy interventions
 *   primarily distort this natural process, leading to deadweight loss. This
 *   is authored as a Mountain because, within this reading, the process is
 *   seen as an irreducible feature of economic reality, not a human construct
 *   or an extractive mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.05).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.05).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Naturalist Reading of Price Formation as Equilibrium").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, '5e1650d0-d337-4385-956e-5168e20f9828').
narrative_ontology:cs_kernel_codification('5e1650d0-d337-4385-956e-5168e20f9828', implicit).
narrative_ontology:cs_authority_grounding('5e1650d0-d337-4385-956e-5168e20f9828', diffuse_epistemic).
narrative_ontology:cs_reading_relation('5e1650d0-d337-4385-956e-5168e20f9828', price_formation_kernel__financialization_reading, forecloses).
narrative_ontology:cs_reading_relation('5e1650d0-d337-4385-956e-5168e20f9828', price_formation_kernel__georgist_reading, forecloses).
narrative_ontology:cs_reading_relation('5e1650d0-d337-4385-956e-5168e20f9828', price_formation_kernel__institutional_reading, forecloses).
narrative_ontology:cs_axiom('5e1650d0-d337-4385-956e-5168e20f9828', foundational, prices_reflect_objective_scarcity).
narrative_ontology:cs_axiom_status(prices_reflect_objective_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('5e1650d0-d337-4385-956e-5168e20f9828', prices_reflect_objective_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('5e1650d0-d337-4385-956e-5168e20f9828', foundational, market_clearing_is_natural_state).
narrative_ontology:cs_axiom_status(market_clearing_is_natural_state, holdable).
narrative_ontology:cs_axiom_grounding('5e1650d0-d337-4385-956e-5168e20f9828', market_clearing_is_natural_state, empirically_contingent).
narrative_ontology:cs_reference_frame('5e1650d0-d337-4385-956e-5168e20f9828', perfect_market_equilibrium).
narrative_ontology:cs_drift_state('5e1650d0-d337-4385-956e-5168e20f9828', contemporary_economic_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5e1650d0-d337-4385-956e-5168e20f9828', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__naturalist_reading, policy_makers_aligned_with_naturalism).
narrative_ontology:constraint_victim(price_formation_kernel__naturalist_reading, housing_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulate and defend the theory that prices are natural reflections of scarcity and preference, guiding policy recommendations towards non-intervention.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, naturalist_economists, agenda_setter,
    institutional, generational, analytical, global).

% Justify non-interventionist policies (e.g., deregulation, minimal social housing) by appealing to the natural efficiency of market price signals. They benefit from a simplified policy framework.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, policy_makers_aligned_with_naturalism, beneficiary,
    institutional, biographical, constrained, national).

% Bear the costs of housing markets where prices are seen as 'natural' and therefore not subject to intervention, leading to unaffordability and displacement. They advocate for alternative policy approaches.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, housing_advocates, payer,
    organized, immediate, constrained, local).

% Critique the naturalist view, arguing that prices are heavily shaped by institutions, regulations, and power dynamics rather than pure scarcity and preference.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, institutional_economists, observer,
    institutional, generational, analytical, global).

% Observe and challenge the naturalist view by distinguishing between earned value (improvements) and unearned rent (land value), arguing for different taxation and allocation mechanisms.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, georgist_advocates, observer,
    organized, generational, analytical, national).

% Analyze how financial markets, credit expansion, and asset-price feedback loops distort price formation, challenging the notion of a natural equilibrium driven by fundamentals.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, financialization_critics, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__naturalist_reading, diffuse).
narrative_ontology:fixing_cost_class(price_formation_kernel__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Efficiently allocates scarce resources (like land and housing) to their highest-value uses by aggregating dispersed information about supply and demand into a single price signal.
% TRANSFER_FUNCTION: Moves resources and capital towards sectors and uses where demand is highest and supply is scarcest, guided by price signals, from lower-value to higher-value uses.
% ABSENT_VOICES: Those who experience housing as a human right rather than a commodity, or who believe prices are manipulated by powerful actors, are often excluded from policy debates dominated by naturalist economic frameworks.
% DISAPPEARANCE_RATIONALE: If price formation were not a natural equilibrium process reflecting scarcity and preference, the fundamental mechanism for resource allocation in market economies would be broken. Economic theory, policy, and individual decision-making would need to be entirely rethought, leading to a profound rearrangement of global economic structures.
% FOUNDING_PROBLEM: How to efficiently allocate scarce resources among competing demands in a complex economy without central planning.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream economic theory, historical observation of market behavior, and the persistent problem of scarcity itself corroborate the founding problem. Critics (institutional, Georgist, financialization) dispute the 'natural' solution, not the problem of scarcity.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__naturalist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(price_formation_kernel__naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(price_formation_kernel__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the Mountain classification: extractiveness, suppression, and theater ratio are all very low (0.05) because a natural process is not seen as extracting rents, requiring active enforcement, or being performative. Accessibility collapse is high (0.9) because alternatives to price signals for resource allocation are considered inherently inefficient or impossible. Resistance is low (0.1) because, while some may dispute the 'naturalness' of prices, the underlying economic forces are seen as immutable. The temporal measurements are flat, reflecting the view that this natural process is stable over time.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who see price formation as a natural, immutable process (this reading) and those who see it as constructed, manipulated, or subject to institutional influence (sibling readings). This divergence is captured by the omegas and the cs_structure, not by internal contradictions within this reading's metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   As a genuine Mountain (from this reading's perspective), there are no direct beneficiaries or victims of the price formation process itself. Those who align with this view (naturalist economists, aligned policymakers) benefit from the justification it provides for their preferred policies, but they do not 'collect' from the natural process. Those who bear costs (housing advocates) do so from the *policies* derived from this view, not from the natural process itself. The 'agenda_setter' role for naturalist economists reflects their role in articulating and defending this view, not in enforcing the natural process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_price,
    'Is price formation fundamentally a natural equilibrium process reflecting objective scarcity and preference, or is it primarily a constructed outcome of institutions, power, and financial dynamics?',
    'Empirical studies comparing price behavior in highly regulated vs. deregulated markets, and analysis of the impact of financial instruments on asset prices, alongside conceptual analysis of the role of human agency in market design.',
    'If resolved towards ''constructed,'' this constraint would reclassify from Mountain to a Snare or Tangled Rope, with identifiable beneficiaries and victims of the constructed price regime. If resolved towards ''natural,'' the Mountain classification would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_price, conceptual, 'The fundamental nature of price formation: natural law vs. human construct.').

omega_variable(
    policy_distortion_vs_constitution,
    'Do policy interventions (e.g., zoning, taxes, subsidies) merely distort a natural price equilibrium, or do they actively constitute and shape the very process of price formation?',
    'Comparative analysis of housing markets across jurisdictions with vastly different regulatory frameworks, examining whether price patterns are consistent with a single underlying ''natural'' equilibrium or diverge systematically with institutional design.',
    'If policies are found to constitute price formation, the ''naturalist_reading'' would be undermined, and the ''institutional_reading'' would gain strength, leading to a reclassification of the underlying constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_distortion_vs_constitution, empirical, 'Role of policy: distortion or constitution of price.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1950, price_formation_kernel__naturalist_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(pric_tr_t1970, price_formation_kernel__naturalist_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__naturalist_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__naturalist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__naturalist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(pric_be_t1950, price_formation_kernel__naturalist_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(pric_be_t1970, price_formation_kernel__naturalist_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__naturalist_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__naturalist_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__naturalist_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1950, price_formation_kernel__naturalist_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(pric_su_t1970, price_formation_kernel__naturalist_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__naturalist_reading, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__naturalist_reading, suppression_requirement, 2010, 0.05).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__naturalist_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
