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
 *   This constraint describes the 'market as natural default' as a Tangled
 *   Rope, arising from a two-stage process: an initial period of genuine
 *   historical forgetting of alternatives (roughly 1930s-1970s), followed by
 *   active ideological maintenance and weaponization of this amnesia by
 *   beneficiaries (1980s-present). The claimed type is 'tangled_rope' because
 *   it still provides a coordination function (a framework for economic
 *   activity) but is increasingly extractive and requires active enforcement
 *   of its ideological dominance. The metrics reflect this: extractiveness
 *   and suppression rise over time as the amnesia is weaponized.
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
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, 'a0da6de7-698b-433a-824e-f8e00975c396').
narrative_ontology:cs_kernel_codification('a0da6de7-698b-433a-824e-f8e00975c396', implicit).
narrative_ontology:cs_authority_grounding('a0da6de7-698b-433a-824e-f8e00975c396', extraction).
narrative_ontology:cs_interpretation_layer_present('a0da6de7-698b-433a-824e-f8e00975c396').
narrative_ontology:cs_reading_relation('a0da6de7-698b-433a-824e-f8e00975c396', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('a0da6de7-698b-433a-824e-f8e00975c396', market_as_natural_default__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('a0da6de7-698b-433a-824e-f8e00975c396', foundational, economic_history_is_contingent).
narrative_ontology:cs_axiom_status(economic_history_is_contingent, holdable).
narrative_ontology:cs_axiom_grounding('a0da6de7-698b-433a-824e-f8e00975c396', economic_history_is_contingent, empirically_contingent).
narrative_ontology:cs_axiom('a0da6de7-698b-433a-824e-f8e00975c396', foundational, ideology_shapes_economic_reality).
narrative_ontology:cs_axiom_status(ideology_shapes_economic_reality, holdable).
narrative_ontology:cs_axiom_grounding('a0da6de7-698b-433a-824e-f8e00975c396', ideology_shapes_economic_reality, empirically_contingent).
narrative_ontology:cs_reference_frame('a0da6de7-698b-433a-824e-f8e00975c396', post_war_consensus_mixed_economy).
narrative_ontology:cs_drift_state('a0da6de7-698b-433a-824e-f8e00975c396', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0da6de7-698b-433a-824e-f8e00975c396', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoliberal_policymakers).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, labor_unions).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, social_democratic_movements).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_sector_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the perception of market mechanisms as natural and inevitable, which reduces regulatory pressure and justifies their dominant position. They actively fund think tanks and lobbying efforts that reinforce this narrative, inheriting and weaponizing a pre-existing amnesia about alternatives.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Actively promote policies that entrench market dominance and resist alternatives, framing these actions as merely aligning with natural economic laws. They leverage the historical amnesia to dismiss critiques of market failures and advocate for deregulation.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoliberal_policymakers, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the costs of market naturalization through weakened bargaining power, deregulation of labor protections, and the erosion of collective action. They struggle to articulate alternatives in a discourse dominated by market inevitability.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, labor_unions, payer,
    organized, generational, constrained, national).

% Face significant ideological and political barriers to implementing policies that challenge market dominance, as the 'natural default' narrative delegitimizes state intervention and public provision. Their historical successes are often forgotten or reframed as market-compatible.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, social_democratic_movements, payer,
    organized, generational, constrained, national).

% Struggle to secure funding and political will for public services and non-market solutions, as the default assumption favors private provision and market-based approaches. Their arguments for public goods are often dismissed as inefficient or unnatural.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_sector_advocates, payer,
    moderate, biographical, constrained, local).

% Analyze the historical processes through which market arrangements became naturalized, documenting the periods of genuine forgetting and subsequent active ideological maintenance. They provide critical counter-narratives to the 'natural default' claim.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a seemingly stable and efficient framework for resource allocation and economic activity by presenting market mechanisms as the only viable option, reducing the perceived need for complex political coordination.
% TRANSFER_FUNCTION: Transfers wealth and power from labor and the public sector to capital and incumbent firms, by legitimizing market-driven outcomes as natural and inevitable, thereby reducing resistance to extraction.
% ABSENT_VOICES: Historical advocates for alternative economic systems (e.g., guild socialism, cooperative movements, planned economies) are largely absent from contemporary discourse, their ideas having been forgotten or actively suppressed. Their re-introduction would challenge the 'natural default' framing.
% DISAPPEARANCE_RATIONALE: If the perception of the market as a natural default vanished, and the historical contingency of economic arrangements became widely understood, it would open space for radical policy experimentation and a re-evaluation of public vs. private provision, fundamentally altering economic and political structures.
% FOUNDING_PROBLEM: The problem of economic instability and the perceived inefficiencies of non-market coordination mechanisms in the early 20th century, leading to a gradual shift towards market-centric solutions.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent firms and neoliberal policymakers argue that market mechanisms remain the most efficient solution to economic problems. Economic historians and critical theorists, from outside the benefiting parties, corroborate that the initial shift was driven by genuine problems but argue that the 'natural default' status is now maintained for extractive purposes, with the original problems largely superseded by new ones.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is substantial because the 'natural default' narrative allows incumbent firms to capture rents without significant challenge, and policymakers to implement pro-market policies that benefit these firms. Suppression (0.65) is high because alternative economic models are actively marginalized or dismissed as 'unnatural' or 'inefficient,' requiring continuous ideological enforcement. Theater ratio (0.20) is moderate, reflecting that while there's genuine belief in market efficiency, a significant portion of the discourse serves to defend existing power structures rather than purely coordinate. The temporal measurements show a clear increase in extractiveness and suppression as the initial 'lapsed closure' transitions into active beneficiary capture.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries (incumbent firms, neoliberal policymakers) perceive the market as genuinely natural and efficient, viewing any extraction as a fair return for coordination. Payers (labor unions, social democratic movements) experience it as an ideological snare that suppresses alternatives and legitimizes their exploitation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent firms and neoliberal policymakers are beneficiaries and agenda-setters, actively shaping the narrative and policy to their advantage (low directionality). Labor unions, social democratic movements, and public sector advocates are payers, bearing the costs of this naturalization through reduced power and policy options (high directionality). Economic historians act as observers, analyzing the structural dynamics without direct benefit or cost from the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amnesia_vs_active_suppression,
    'What proportion of the observed suppression is due to genuine historical amnesia versus active, contemporary ideological suppression by beneficiaries?',
    'Content analysis of policy debates and public discourse over time, distinguishing arguments based on ''it''s always been this way'' from active refutations of alternatives. Longitudinal studies of public memory regarding economic history.',
    'If amnesia is dominant, interventions might focus on historical education and narrative reconstruction. If active suppression is dominant, interventions would target lobbying, media influence, and political power structures. This would shift the balance of ''theater_ratio'' and ''suppression'' in the base metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_vs_active_suppression, empirical, 'Distinguishing the mechanisms of ideological suppression.').

omega_variable(
    counterfactual_alternative_viability,
    'Would the ''lapsed alternatives'' (e.g., robust public sector, strong unions) have been genuinely viable and efficient in the long run, or were they inherently unstable?',
    'Comparative historical analysis of economies that pursued different paths, or counterfactual modeling of economic development under alternative institutional arrangements.',
    'If alternatives were viable, the ''natural default'' claim is demonstrably false, strengthening the ''snare'' aspect. If they were inherently unstable, the coordination function of the market, even if extractive, gains some legitimacy, pushing it closer to ''tangled_rope'' or even ''rope'' (if extraction were lower).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_alternative_viability, empirical, 'Assessing the inherent viability of forgotten economic alternatives.').

omega_variable(
    reading_framing_choice,
    'Is the ''hybrid_amnesia_reading'' the most accurate framing, or does the evidence better support a ''lapsed_alternative_reading'' (pure forgetting) or ''beneficiary_maintained_reading'' (pure active defense)?',
    'Further historical research to precisely date the transition from passive forgetting to active ideological maintenance, and to quantify the relative contributions of each stage to the constraint''s persistence and extractiveness.',
    'A shift to ''lapsed_alternative_reading'' would lower ''suppression'' and ''theater_ratio'' (less active enforcement). A shift to ''beneficiary_maintained_reading'' would raise ''suppression'' and ''theater_ratio'' (more active, performative defense). This would alter the overall classification and the policy implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Ambiguity in the primary mechanism of market naturalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(mark_tr_t2010, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(mark_tr_t2024, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(mark_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(mark_be_t2010, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(mark_be_t2024, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.4).
narrative_ontology:measurement(mark_su_t1950, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(mark_su_t2010, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(mark_su_t2024, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
