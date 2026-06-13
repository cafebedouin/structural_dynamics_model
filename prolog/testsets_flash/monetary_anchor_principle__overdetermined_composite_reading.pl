% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Overdetermined Collapse of the Gold Standard
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story describes the gold standard's collapse as an
 *   overdetermined outcome of multiple, converging structural pressures in
 *   the late 1960s, rather than a single policy choice or a sole dilemma.
 *   These pressures included the Triffin dilemma (inherent conflict between
 *   national and international roles of a reserve currency), escalating
 *   Vietnam War deficits, a prevailing Keynesian policy consensus favoring
 *   fiscal flexibility, and increasing technological capital mobility. This
 *   reading frames the 'monetary anchor principle' as a Tangled Rope that
 *   became unsustainable due to these entangled forces, leading to its
 *   inevitable abandonment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.85).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.75).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Overdetermined Collapse of the Gold Standard").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, 'bc73d115-368b-4856-ae4b-a424085a0488').
narrative_ontology:cs_kernel_codification('bc73d115-368b-4856-ae4b-a424085a0488', implicit).
narrative_ontology:cs_authority_grounding('bc73d115-368b-4856-ae4b-a424085a0488', distributed).
narrative_ontology:cs_reading_relation('bc73d115-368b-4856-ae4b-a424085a0488', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc73d115-368b-4856-ae4b-a424085a0488', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('bc73d115-368b-4856-ae4b-a424085a0488', foundational, monetary_regime_is_emergent_property).
narrative_ontology:cs_axiom_status(monetary_regime_is_emergent_property, holdable).
narrative_ontology:cs_axiom_grounding('bc73d115-368b-4856-ae4b-a424085a0488', monetary_regime_is_emergent_property, empirically_contingent).
narrative_ontology:cs_axiom('bc73d115-368b-4856-ae4b-a424085a0488', foundational, multiple_structural_pressures_converged).
narrative_ontology:cs_axiom_status(multiple_structural_pressures_converged, holdable).
narrative_ontology:cs_axiom_grounding('bc73d115-368b-4856-ae4b-a424085a0488', multiple_structural_pressures_converged, empirically_contingent).
narrative_ontology:cs_reference_frame('bc73d115-368b-4856-ae4b-a424085a0488', gold_standard_as_structurally_constrained_system).
narrative_ontology:cs_drift_state('bc73d115-368b-4856-ae4b-a424085a0488', late_1960s, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('bc73d115-368b-4856-ae4b-a424085a0488', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_makers).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, international_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the removal of the gold constraint, allowing governments to finance deficits without immediate balance of payments crises, expanding fiscal space and enabling counter-cyclical policy.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity, beneficiary,
    institutional, generational, arbitrage, global).

% The principle of monetary discipline, enforced by the gold standard, was a victim of its collapse. The constraint on inflation and government spending was removed, leading to greater monetary flexibility but also potential for inflation.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).

% Advocated for and benefited from the flexibility to use monetary and fiscal policy for demand management, unconstrained by gold reserves. The collapse validated their theoretical framework.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_makers, beneficiary,
    institutional, biographical, mobile, national).

% Suffered from the loss of predictable exchange rates and the increased volatility in international trade and finance that followed the gold standard's demise. Their preferred system was dismantled by the structural pressures.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_advocates, payer,
    organized, biographical, constrained, global).

% Benefited from increased capital mobility and the ability to arbitrage interest rate differentials and exchange rate movements, which were previously constrained by the gold standard's rigidities.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, international_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Often had less influence in the international monetary system and were subject to the consequences of the gold standard's collapse, including increased currency volatility and external debt burdens, without having a voice in the transition.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, developing_nations, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard coordinated international trade and finance by providing a stable, universally accepted monetary anchor, ensuring predictable exchange rates and limiting inflationary pressures across nations.
% TRANSFER_FUNCTION: The collapse transferred the constraint of external monetary discipline from national fiscal and monetary policy to a more flexible, but less anchored, system. It transferred the burden of adjustment from deficit countries (under gold) to a more diffuse, market-driven process.
% ABSENT_VOICES: Advocates for a return to a gold-backed system or alternative fixed-exchange rate regimes, as well as developing nations whose economies were significantly impacted by the volatility, were largely absent from the decision-making processes that led to the collapse.
% DISAPPEARANCE_RATIONALE: The gold standard's collapse fundamentally reshaped global monetary policy, international finance, and the role of national governments in economic management. Its disappearance led to the floating exchange rate era and increased fiscal flexibility, which are still defining features of the global economy.
% FOUNDING_PROBLEM: The gold standard was established to provide a stable and credible international monetary system, preventing currency debasement and facilitating international trade by fixing exchange rates.
% FOUNDING_PROBLEM_CORROBORATION: While some fringe groups still advocate for a return to gold, the consensus among mainstream economists and central bankers, corroborated by decades of economic data and policy experience, is that the original problems the gold standard solved were superseded by new challenges (e.g., need for counter-cyclical policy, global liquidity provision) that the gold standard could not accommodate. The problem it was built for is considered 'dead' in its original form.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the gold standard, while coordinating, imposed severe constraints on national fiscal and monetary policy, especially for the reserve currency issuer. Suppression (0.75) was also high, as maintaining the gold peg required active defense against market pressures and suppression of alternative monetary theories. The theater ratio is low (0.1) because the system was genuinely functional for a long time, but by the late 1960s, the structural pressures made its maintenance increasingly performative and unsustainable. The rising extractiveness and suppression over the interval reflect the increasing strain on the system as these pressures mounted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state fiscal capacity and Keynesian policymakers, the gold standard was an increasingly extractive and suppressive constraint that needed to be overcome for economic stability and growth. From the perspective of monetary discipline and fixed exchange rate advocates, its collapse represented a loss of a vital anchor, leading to instability and inflation. This reading emphasizes the structural inevitability, suggesting that no single actor could have maintained the system against the composite pressures.
 *
 * DIRECTIONALITY LOGIC:
 *   State fiscal capacity and Keynesian policymakers were beneficiaries, as the collapse freed them from constraints. Monetary discipline and fixed exchange rate advocates were victims, losing their preferred framework. International investors benefited from new arbitrage opportunities. Developing nations were largely excluded from the decision-making but bore significant consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing a stable monetary anchor) became mandatrophiated because the underlying structural conditions it was designed to manage (e.g., limited capital mobility, smaller fiscal demands) had fundamentally changed. The system's persistence became increasingly extractive as it struggled against these new realities, leading to its eventual collapse. This classification prevents mislabeling the collapse as a simple policy error or a pure extraction, instead highlighting the overdetermined structural forces at play.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_weight_of_factors,
    'What was the precise causal weighting of each structural pressure (Triffin dilemma, Vietnam War deficits, Keynesian consensus, capital mobility) in making the gold standard''s collapse inevitable?',
    'Counterfactual historical analysis and econometric modeling attempting to isolate the impact of each factor, though definitive resolution is likely impossible due to their entanglement.',
    'A clearer weighting would refine the understanding of the ''overdetermined'' nature, potentially shifting emphasis to one or two dominant factors, but would not change the overall ''tangled_rope'' classification of the gold standard''s operation under these pressures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_weight_of_factors, empirical, 'Quantifying the relative importance of multiple, entangled causal factors in the gold standard''s collapse.').

omega_variable(
    agency_vs_structure_ambiguity,
    'To what extent was the collapse truly ''inevitable'' due to structural pressures, versus being the result of specific policy choices made by powerful actors within those pressures?',
    'Detailed historical and political economy research focusing on decision points and alternative paths considered by policymakers, assessing the feasibility and political cost of maintaining the gold standard.',
    'If agency played a more decisive role, the constraint might lean more towards a ''snare'' (if choices were purely extractive) or a ''rope'' (if choices were genuinely coordinative but failed), rather than a ''tangled_rope'' driven by structural inevitability. This reading emphasizes structure, but the boundary is always contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_structure_ambiguity, conceptual, 'The balance between structural inevitability and agentic choice in the gold standard''s collapse.').

omega_variable(
    reading_distinction_clarity,
    'Is the distinction between the ''overdetermined composite reading'' and the ''Triffin inevitability reading'' sufficiently clear, or does the Triffin dilemma so dominate the composite that they are effectively the same?',
    'Further conceptual analysis and historical evidence to demonstrate that the other factors (Vietnam, Keynesianism, capital mobility) were independently necessary for the collapse, not merely amplifiers of Triffin.',
    'If the distinction collapses, this reading would merge with the ''Triffin inevitability reading'', potentially simplifying the kernel but losing the nuance of multiple causal streams.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinction_clarity, conceptual, 'Clarity of distinction between composite and Triffin-dominant readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1960, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1960, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(mone_tr_t1963, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1963, 0.07).
narrative_ontology:measurement(mone_tr_t1966, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1966, 0.08).
narrative_ontology:measurement(mone_tr_t1969, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1969, 0.09).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1960, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(mone_be_t1963, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1963, 0.65).
narrative_ontology:measurement(mone_be_t1966, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1966, 0.7).
narrative_ontology:measurement(mone_be_t1969, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1969, 0.78).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1960, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(mone_su_t1963, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1963, 0.58).
narrative_ontology:measurement(mone_su_t1966, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1966, 0.65).
narrative_ontology:measurement(mone_su_t1969, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1969, 0.7).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
