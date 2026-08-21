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
 *   This constraint describes the process by which the market came to be seen
 *   as the 'natural default' for economic organization, not through active,
 *   overt coercion, but through a two-stage process: initial genuine
 *   forgetting of historical alternatives (roughly 1930s-1970s) followed by a
 *   period of defensive rationalization and beneficiary capture (roughly
 *   1980s-present). During the latter stage, market incumbents and neoliberal
 *   ideologues actively weaponize this pre-existing amnesia to legitimize
 *   their extractive practices. The constraint is presented as a
 *   'tangled_rope' because it still performs a coordination function
 *   (resource allocation) but is deeply intertwined with asymmetric
 *   extraction enabled by ideological naturalization.
 *
 * KEY AGENTS:
 *   - market_incumbents: Agenda setter (institutional/arbitrage) — benefits from naturalization
 *   - neoliberal_ideologues: Beneficiary (organized/analytical) — provides intellectual justification
 *   - labor_movements: Payer (organized/constrained) — bears costs, struggles for alternatives
 *   - public_sector_advocates: Payer (moderate/constrained) — advocates for non-market solutions
 *   - marginalized_communities: Payer (powerless/trapped) — most directly harmed by market failures
 *   - economic_historians: Observer (analytical/analytical) — documents historical amnesia
 *   - policy_makers: Agenda setter (institutional/mobile) — can reinforce or challenge the default
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.65).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market as Natural Default (Hybrid Amnesia Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '6539e622-8004-4a67-a320-33e332426718').
narrative_ontology:cs_kernel_codification('6539e622-8004-4a67-a320-33e332426718', implicit).
narrative_ontology:cs_authority_grounding('6539e622-8004-4a67-a320-33e332426718', extraction).
narrative_ontology:cs_interpretation_layer_present('6539e622-8004-4a67-a320-33e332426718').
narrative_ontology:cs_reading_relation('6539e622-8004-4a67-a320-33e332426718', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('6539e622-8004-4a67-a320-33e332426718', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_axiom('6539e622-8004-4a67-a320-33e332426718', foundational, market_efficiency_is_natural).
narrative_ontology:cs_axiom_status(market_efficiency_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('6539e622-8004-4a67-a320-33e332426718', market_efficiency_is_natural, empirically_contingent).
narrative_ontology:cs_axiom('6539e622-8004-4a67-a320-33e332426718', secondary, alternatives_are_historically_obsolete).
narrative_ontology:cs_axiom_status(alternatives_are_historically_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('6539e622-8004-4a67-a320-33e332426718', alternatives_are_historically_obsolete, conventional).
narrative_ontology:cs_reference_frame('6539e622-8004-4a67-a320-33e332426718', post_war_consensus_market).
narrative_ontology:cs_drift_state('6539e622-8004-4a67-a320-33e332426718', contemporary_neoliberal_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('6539e622-8004-4a67-a320-33e332426718', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, market_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoliberal_ideologues).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, labor_movements).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_sector_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, marginalized_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities (e.g., large corporations, financial institutions) that benefit directly from the market's perceived naturalness, as it legitimizes their dominant position and limits challenges to their power. They actively fund and promote narratives that reinforce the market as the only viable system.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, market_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Academics, think tanks, and media figures who develop and disseminate the intellectual justifications for market naturalization. They benefit from the prestige and influence derived from shaping public discourse and policy in line with market-centric views.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoliberal_ideologues, beneficiary,
    organized, biographical, analytical, global).

% Organizations representing workers who bear the costs of market dominance through suppressed wages, precarious employment, and reduced social safety nets. They struggle to advocate for alternatives when the market is presented as an unchangeable natural force.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, labor_movements, payer,
    organized, generational, constrained, national).

% Groups advocating for robust public services, social welfare, and non-market solutions to societal problems. They face an uphill battle against the 'market as default' narrative, which frames public provision as inefficient or illegitimate.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_sector_advocates, payer,
    moderate, biographical, constrained, national).

% Populations most vulnerable to market failures, lacking access to essential services, and experiencing the direct consequences of economic inequality. Their ability to articulate and pursue alternatives is severely limited by their structural position and the dominant ideological frame.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, marginalized_communities, payer,
    powerless, immediate, trapped, local).

% Scholars who research and document the historical contingency of market structures, the existence of past alternatives, and the processes through which certain economic arrangements became naturalized. They provide critical counter-narratives but often operate outside mainstream policy discourse.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% Elected officials and bureaucrats who operate within the prevailing ideological framework. While some may seek to regulate or mitigate market excesses, the 'natural default' narrative often limits the scope of perceived legitimate interventions, making radical alternatives seem politically unfeasible.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, policy_makers, agenda_setter,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, market_incumbents).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a seemingly neutral and efficient mechanism for resource allocation and exchange, reducing perceived transaction costs by presenting itself as the only viable or 'natural' option, thereby coordinating economic activity around a single, dominant model.
% TRANSFER_FUNCTION: Transfers wealth, power, and legitimacy to market incumbents and their ideological proponents by foreclosing consideration of alternative economic systems and legitimizing existing distributions as 'natural' or 'inevitable' outcomes.
% ABSENT_VOICES: Advocates for historical alternatives (e.g., socialized industries, robust public services, worker cooperatives) are marginalized or dismissed as 'unrealistic' or 'ideological' due to the pervasive amnesia regarding their historical efficacy and the active rationalization of market dominance.
% DISAPPEARANCE_RATIONALE: If the idea of the market as a natural default vanished overnight, the political and economic landscape would be radically re-evaluated. This would open significant space for diverse economic models, public interventions, and alternative forms of resource allocation, fundamentally altering power structures and societal priorities.
% FOUNDING_PROBLEM: To create a stable, predictable system for economic exchange and resource allocation after periods of significant instability, such as the Great Depression and World War II, aiming to prevent future crises and foster prosperity.
% FOUNDING_PROBLEM_CORROBORATION: Market incumbents and their associated think tanks assert that the market's naturalness is self-evident and that its foundational problem (efficient allocation) is still live. Critical economic historians and political scientists, drawing on archival research and comparative political economy, corroborate the historical contingency and constructed nature of market dominance, challenging the 'natural default' narrative and arguing the founding problem has been superseded by rent-seeking.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness (0.45) is substantial, reflecting the transfer of wealth and power to market incumbents through the ideological foreclosure of alternatives. Suppression (0.65) is high, as it involves not just active enforcement but also the suppression of historical memory and the marginalization of dissenting economic thought. Theater ratio (0.40) is moderate, indicating that while the market performs a real function, a significant portion of the discourse around its 'naturalness' is performative, serving to rationalize existing power structures. Accessibility collapse (0.75) is high because the historical amnesia makes alternatives seem non-existent or unworkable. Resistance (0.55) is moderate, as various groups actively challenge the market's naturalization, but face significant ideological and institutional barriers. The temporal measurements show a clear increase in extractiveness, suppression, and theatricality over the interval, reflecting the shift from genuine forgetting to active rationalization and capture.
 *
 * PERSPECTIVAL GAP:
 *   Market incumbents and neoliberal ideologues perceive the market as a natural, efficient, and inevitable system, viewing any extraction as a necessary cost of coordination. From the perspective of labor movements, public sector advocates, and marginalized communities, the same structure is a constructed mechanism of extraction, maintained by ideological suppression and historical amnesia, which actively forecloses more equitable alternatives. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Market incumbents and neoliberal ideologues are clear beneficiaries, as the constraint legitimizes their power and intellectual frameworks. Labor movements, public sector advocates, and marginalized communities are targets, bearing the costs of reduced social protections and limited economic alternatives. Policy makers are agenda setters, operating within the constraint's ideological frame but with some capacity to influence its enforcement. Economic historians act as analytical observers, documenting the constraint's historical evolution.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'tangled_rope' prevents mislabeling the market as a 'mountain' (natural law) or a 'rope' (pure coordination). It acknowledges the market's genuine coordination function (resource allocation) while simultaneously highlighting the asymmetric extraction enabled by the 'hybrid amnesia' – the historical forgetting of alternatives, followed by active rationalization and beneficiary capture. The rising extractiveness and theater ratio over time indicate a drift from a potentially more benign coordination mechanism to one where ideological maintenance serves primarily to legitimize rent-seeking, even as the original coordination problem may be contested as 'dead' or 'solved'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amnesia_vs_active_suppression,
    'To what extent is the ''forgetting'' of market alternatives a genuine, passive historical amnesia versus an active, deliberate suppression of counter-narratives by beneficiaries?',
    'Content analysis of historical economic discourse, funding sources for think tanks promoting market naturalization, and examination of educational curricula over time. If active suppression is dominant, the suppression metric should be adjusted upward.',
    'If primarily active suppression, the constraint is more clearly a snare, as the coordination story is a deliberate cover. If primarily passive amnesia, it leans more towards a piton or a degraded rope, where the extraction is a consequence of neglect rather than active malice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_vs_active_suppression, empirical, 'Distinguishing between passive historical forgetting and active ideological suppression.').

omega_variable(
    natural_vs_constructed_market,
    'Is the market fundamentally a natural, emergent phenomenon, or is it a historically constructed and politically maintained institution?',
    'Comparative historical analysis of different economic systems, anthropological studies of non-market societies, and philosophical inquiry into the foundations of economic order. This is a conceptual omega, unlikely to be fully resolved empirically.',
    'If natural, the constraint leans towards a mountain, with minimal extraction inherent to its operation. If constructed, it reinforces the tangled_rope classification, highlighting the political choices embedded in its structure and the potential for alternative designs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_market, conceptual, 'The core ambiguity of the market''s ontological status.').

omega_variable(
    historical_contingency_of_market_dominance,
    'What specific historical junctures and policy choices were most critical in solidifying the market''s ''natural default'' status, and could alternative paths have been taken?',
    'Detailed historical case studies and counterfactual analysis by economic historians. Resolution would involve identifying specific policy decisions, institutional reforms, or ideological shifts that foreclosed alternatives.',
    'If specific, contingent choices are identified, it strengthens the argument for the market as a constructed constraint, making its extractive elements more amenable to policy intervention. If no clear junctures are found, it might suggest a more diffuse, emergent process, making intervention harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_contingency_of_market_dominance, empirical, 'The role of specific historical events in market naturalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1990, 0.33).
narrative_ontology:measurement(mark_tr_t2010, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(mark_tr_t2020, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(mark_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(mark_be_t2010, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(mark_be_t2020, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.4).
narrative_ontology:measurement(mark_su_t1950, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(mark_su_t2010, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(mark_su_t2020, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, financial_deregulation).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, privatization_mandates).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, austerity_policies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_as_natural_default' kernel. This 'hybrid_amnesia_reading' emphasizes a two-stage process of forgetting and rationalization, distinct from readings focused solely on active beneficiary maintenance or passive historical lapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
