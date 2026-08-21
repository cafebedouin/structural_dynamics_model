% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance (Hybrid Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'hybrid_reading' of the
 *   'market_naturalization' kernel. It posits that market dominance is
 *   sustained by a combination of historical structural advantages (lapsed
 *   elements that no longer require active defense) and ongoing, active
 *   maintenance by incumbent firms and their allies (e.g., lobbying,
 *   strategic acquisitions, control over standards). This reading contrasts
 *   with purely 'lapsed' or purely 'beneficiary-maintained' interpretations,
 *   emphasizing the dynamic interplay of inertia and agency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.68).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.75).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance (Hybrid Reading)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'e73e463e-4abe-4468-9e15-94c0b97752fe').
narrative_ontology:cs_kernel_codification('e73e463e-4abe-4468-9e15-94c0b97752fe', formalized).
narrative_ontology:cs_authority_grounding('e73e463e-4abe-4468-9e15-94c0b97752fe', extraction).
narrative_ontology:cs_interpretation_layer_present('e73e463e-4abe-4468-9e15-94c0b97752fe').
narrative_ontology:cs_reading_relation('e73e463e-4abe-4468-9e15-94c0b97752fe', market_naturalization__lapsed_alternative_reading, forecloses).
narrative_ontology:cs_reading_relation('e73e463e-4abe-4468-9e15-94c0b97752fe', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('e73e463e-4abe-4468-9e15-94c0b97752fe', foundational, market_structure_is_partially_constructed).
narrative_ontology:cs_axiom_status(market_structure_is_partially_constructed, holdable).
narrative_ontology:cs_axiom_grounding('e73e463e-4abe-4468-9e15-94c0b97752fe', market_structure_is_partially_constructed, empirically_contingent).
narrative_ontology:cs_axiom('e73e463e-4abe-4468-9e15-94c0b97752fe', foundational, maintenance_is_selective).
narrative_ontology:cs_axiom_status(maintenance_is_selective, holdable).
narrative_ontology:cs_axiom_grounding('e73e463e-4abe-4468-9e15-94c0b97752fe', maintenance_is_selective, empirically_contingent).
narrative_ontology:cs_reference_frame('e73e463e-4abe-4468-9e15-94c0b97752fe', competitive_market_ideal).
narrative_ontology:cs_drift_state('e73e463e-4abe-4468-9e15-94c0b97752fe', contemporary_era_of_concentration, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e73e463e-4abe-4468-9e15-94c0b97752fe', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, capital_holders).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, new_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These firms benefit from existing market structures, often having shaped regulations and industry standards. They actively maintain barriers to entry and leverage network effects, while also benefiting from historical advantages that no longer require direct defense.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_firms, agenda_setter,
    institutional, generational, arbitrage, global).

% Investors in incumbent firms who profit from their sustained market dominance, through dividends, stock appreciation, and reduced competitive pressure. They may fund lobbying efforts to maintain favorable conditions but do not directly manage the market structure.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, capital_holders, beneficiary,
    powerful, generational, mobile, global).

% Start-up companies and innovators who face significant hurdles due to the entrenched power of incumbent firms, including regulatory capture, control over distribution channels, and high capital requirements. Their ability to compete is severely limited.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, new_entrants, payer,
    powerless, immediate, constrained, national).

% Pay higher prices, experience less innovation, and have fewer choices due to reduced competition. While they benefit from the stability of established markets, the costs of dominance often outweigh these benefits. Their exit options are limited to accepting the market's offerings or abstaining.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers, payer,
    moderate, biographical, constrained, national).

% Faces suppressed wages, reduced bargaining power, and fewer employment opportunities as dominant firms consolidate power and automate. Their ability to organize or seek better employment is often constrained by the limited number of employers in concentrated markets.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, labor, payer,
    organized, biographical, constrained, local).

% Government bodies tasked with ensuring fair competition. They interpret and enforce antitrust laws, but their actions are often influenced by lobbying from incumbent firms and by economic theories that may naturalize existing market structures. They are both enforcers and potential targets of capture.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, competition_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Academics and researchers who study market dynamics. Their theories can either challenge or reinforce the perception of market dominance as natural or constructed, influencing policy debates and public understanding.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, economic_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for large-scale production, distribution, and investment, reducing uncertainty for established economic actors and facilitating complex supply chains.
% TRANSFER_FUNCTION: Transfers economic surplus from consumers (via higher prices), new entrants (via barriers to entry), and labor (via suppressed wages) to incumbent firms and their capital holders, consolidating wealth and power.
% ABSENT_VOICES: Displaced workers, small businesses that failed to compete, and future generations who will inherit less dynamic and equitable markets are largely absent from the policy debates that shape these structures.
% DISAPPEARANCE_RATIONALE: If market dominance, as a hybrid of lapsed and actively maintained elements, vanished overnight, the global economy would undergo significant restructuring. New firms would emerge, pricing structures would shift, labor markets would rebalance, and capital flows would diversify, leading to a more competitive and potentially more equitable distribution of economic activity.
% FOUNDING_PROBLEM: To efficiently organize production, allocate resources, and incentivize innovation within a capitalist framework, ensuring economic stability and growth.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent firms and some mainstream economists argue the founding problem of efficient resource allocation and innovation is still live, and market dominance is a natural outcome. Critical economists, labor unions, and consumer advocates, supported by empirical studies on market concentration and its effects, argue the problem has been perverted into rent-seeking, and the original mandate is largely dead or severely compromised.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) reflects the significant wealth transfer from consumers, new entrants, and labor to incumbent firms. Suppression (0.75) is high due to both the structural barriers (lapsed alternatives) and active enforcement (e.g., legal challenges to new entrants, regulatory capture). The theater ratio (0.45) indicates that a substantial portion of the 'natural market' narrative is performative, masking active rent-seeking, though some genuine market functions persist. The increasing trends in extractiveness and suppression over the interval reflect the growing concentration of market power and the corresponding need for more active defense against challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent firms, the market structure is a natural outcome of efficiency and innovation, requiring only 'fair' competition. From the perspective of new entrants, consumers, and labor, it is an actively maintained system of extraction. This hybrid reading acknowledges both the 'naturalized' aspects (lapsed alternatives) and the 'constructed' aspects (active maintenance), leading to a 'tangled_rope' classification that captures both coordination and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent firms and capital holders are clear beneficiaries (low d) as they directly profit from the market structure. New entrants, consumers, and labor are targets (high d) as they bear the costs of reduced competition and suppressed opportunities. Competition regulators are complex: they are agenda-setters, but their effectiveness can be constrained by political economy dynamics, sometimes acting as beneficiaries of the status quo (e.g., through revolving doors) and sometimes as agents of change.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid reading prevents mislabeling by acknowledging that while some aspects of market dominance might be inertial (suggesting a Piton or even a Mountain if truly natural), the active maintenance component means it is not merely a vestigial structure. Conversely, it avoids overstating active coercion by recognizing that not every aspect of dominance requires constant defense. The 'tangled_rope' classification correctly identifies the dual nature: a market structure that provides some coordination (e.g., stability for large-scale investment) but also actively extracts from specific parties through asymmetric power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_active_maintenance_balance,
    'What is the precise balance between ''lapsed'' structural advantages (requiring no active defense) and ''active maintenance'' (requiring ongoing effort) in sustaining market dominance?',
    'Detailed historical and economic analysis of specific industries, quantifying the resources spent on lobbying, legal defense, strategic acquisitions, and comparing them to the persistence of historical advantages (e.g., network effects, brand loyalty) that are no longer actively cultivated.',
    'A higher proportion of active maintenance would push the classification closer to a Snare, emphasizing deliberate extraction. A higher proportion of lapsed elements would lean towards a Piton, highlighting institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_active_maintenance_balance, empirical, 'Quantifying the relative contribution of passive inertia versus active agency in market dominance.').

omega_variable(
    market_naturalness_ambiguity,
    'To what extent is the observed market structure a ''natural'' outcome of economic forces (e.g., economies of scale, innovation) versus a ''constructed'' outcome of policy choices, regulatory capture, and strategic firm behavior?',
    'Comparative analysis across different regulatory regimes and historical periods, examining how variations in policy (e.g., antitrust enforcement, intellectual property rights) correlate with market concentration and firm behavior, controlling for technological factors.',
    'If predominantly natural, the constraint would lean towards a Mountain (or Rope if coordination is primary). If predominantly constructed, it would reinforce the Tangled Rope or Snare classification, highlighting human agency in its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_naturalness_ambiguity, conceptual, 'Distinguishing natural market outcomes from constructed ones.').

omega_variable(
    reading_impact_on_policy,
    'How would the adoption of the ''lapsed_alternative_reading'' or ''beneficiary_maintained_reading'' by policymakers alter the regulatory response to market dominance?',
    'Scenario modeling and policy analysis, examining how different theoretical framings of market dominance (e.g., ''markets are natural'' vs. ''markets are always constructed'') lead to different policy prescriptions (e.g., deregulation vs. aggressive antitrust).',
    'If the ''lapsed'' reading were adopted, it might lead to less intervention, assuming dominance will naturally erode. If the ''beneficiary-maintained'' reading were adopted, it would likely lead to more aggressive antitrust and regulatory action.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_impact_on_policy, preference, 'Policy implications of alternative readings of market naturalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1980, market_naturalization__hybrid_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(mark_tr_t1990, market_naturalization__hybrid_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(mark_tr_t2000, market_naturalization__hybrid_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(mark_tr_t2010, market_naturalization__hybrid_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(mark_tr_t2024, market_naturalization__hybrid_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_naturalization__hybrid_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(mark_be_t1990, market_naturalization__hybrid_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(mark_be_t2000, market_naturalization__hybrid_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(mark_be_t2010, market_naturalization__hybrid_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(mark_be_t2024, market_naturalization__hybrid_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_naturalization__hybrid_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(mark_su_t1990, market_naturalization__hybrid_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(mark_su_t2000, market_naturalization__hybrid_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(mark_su_t2010, market_naturalization__hybrid_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(mark_su_t2024, market_naturalization__hybrid_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'hybrid_reading' of the 'market_naturalization' kernel, which also includes 'lapsed_alternative_reading' and 'beneficiary_maintained_reading'. Each reading offers a distinct structural interpretation of how market dominance is sustained, with differing implications for extraction and agency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
