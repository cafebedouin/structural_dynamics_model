% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma Structural Inevitability (Monetary Anchor Principle)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the triffin_inevitability_reading of
 *   the monetary_anchor_principle kernel. It treats the collapse of the
 *   Bretton Woods gold-exchange standard not as a policy choice or historical
 *   accident, but as the logical consequence of a structural contradiction: a
 *   reserve currency issuer cannot simultaneously maintain gold
 *   convertibility and supply the global liquidity demanded of it. The
 *   constraint is a mountainâan irreducible feature of the monetary
 *   arrangement that no institutional design within that arrangement could
 *   overcome. There is no beneficiary; the Bretton Woods institutional
 *   framework itself is the victim, destroyed by the contradiction embedded
 *   in its own architecture.
 *
 * KEY AGENTS:
 *   - Bretton Woods institutional framework: structural victim (institutional/trapped) â the arrangement that bears the cost of the logical impossibility and is forced into suspension.
 *   - Reserve currency issuer (US Treasury/Federal Reserve): symmetrically trapped actor (institutional/constrained) â caught in the dilemma but not a concentrated beneficiary of the extraction.
 *   - International liquidity users: benefited from the liquidity provision but are not beneficiaries of the constraint's collapse; they are excluded from the analytical frame of the inevitability reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.02).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma Structural Inevitability (Monetary Anchor Principle)").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '7d49fa6a-7002-4580-ab70-d991d077d3ce').
narrative_ontology:cs_kernel_codification('7d49fa6a-7002-4580-ab70-d991d077d3ce', formalized).
narrative_ontology:cs_authority_grounding('7d49fa6a-7002-4580-ab70-d991d077d3ce', lineage).
narrative_ontology:cs_interpretation_layer_present('7d49fa6a-7002-4580-ab70-d991d077d3ce').
narrative_ontology:cs_reading_relation('7d49fa6a-7002-4580-ab70-d991d077d3ce', monetary_anchor_principle__punctuated_swap_reading, influences).
narrative_ontology:cs_reading_relation('7d49fa6a-7002-4580-ab70-d991d077d3ce', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('7d49fa6a-7002-4580-ab70-d991d077d3ce', foundational, reserve_currency_gold_standard_unsustainable).
narrative_ontology:cs_axiom_status(reserve_currency_gold_standard_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('7d49fa6a-7002-4580-ab70-d991d077d3ce', reserve_currency_gold_standard_unsustainable, empirically_contingent).
narrative_ontology:cs_axiom('7d49fa6a-7002-4580-ab70-d991d077d3ce', foundational, triffin_mechanism_sufficient_for_collapse).
narrative_ontology:cs_axiom_status(triffin_mechanism_sufficient_for_collapse, holdable).
narrative_ontology:cs_axiom_grounding('7d49fa6a-7002-4580-ab70-d991d077d3ce', triffin_mechanism_sufficient_for_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('7d49fa6a-7002-4580-ab70-d991d077d3ce', gold_standard_liquidity_equilibrium).
narrative_ontology:cs_drift_state('7d49fa6a-7002-4580-ab70-d991d077d3ce', post_1971_fiat_transition, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7d49fa6a-7002-4580-ab70-d991d077d3ce', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_framework).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma_theorem).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, reserve_currency_trilemma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The post-war international monetary arrangement pegging major currencies to the US dollar and the dollar to gold. It required the US to supply dollars for global liquidity while maintaining gold convertibility, a structurally unstable dual mandate. The framework could not exit its own design; the Triffin contradiction destroyed it from within.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_framework, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_framework).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable exchange-rate anchor and a global liquidity source by pegging currencies to the dollar and the dollar to gold, solving the immediate post-war payments and reserve shortage through a single reserve currency.
% TRANSFER_FUNCTION: Moved purchasing power and reserve assets from the reserve currency issuer to the global economy via balance-of-payments deficits; simultaneously moved gold reserves out of the issuer as confidence eroded, until convertibility collapsed.
% ABSENT_VOICES: Keynesian critics of the gold-exchange standard (e.g., Triffin) were present in analytical discourse but excluded from the institutional design at Bretton Woods in 1944, where the US preference for a gold-dollar hybrid prevailed. Developing nations with chronic reserve shortages were also marginal to the anchor design.
% DISAPPEARANCE_RATIONALE: If the Triffin contradiction did not exist, the Bretton Woods gold-exchange standard could have persisted indefinitely: the reserve currency issuer could have supplied global liquidity without exhausting gold reserves, and the institutional framework would not have been forced into suspension in 1971. The absence of this constraint would leave the post-war monetary architecture intact.
% FOUNDING_PROBLEM: Post-war international monetary chaos: competitive devaluations, liquidity shortages, and lack of a credible reserve asset to finance reconstruction and trade.
% FOUNDING_PROBLEM_CORROBORATION: Historical consensus among monetary historians outside the Bretton Woods institutional core (e.g., Eichengreen, Bordo) attests that the liquidity shortage and exchange-rate volatility problems were substantially solved by the 1960s, but the solution itself contained the Triffin contradiction that became the new binding constraint. The IMF and US Treasury assert the founding problem remains live, but independent economic historians corroborate that the specific post-war liquidity crisis had passed.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05) because the constraint does not extract rents from agents; it is a logical feature of the gold-exchange standard that systematically exhausts reserves. Suppression is near-zero (0.02) because no coercion is required for the constraint to operateâit functions as a structural necessity. Theater ratio is negligible (0.02): there is no performative maintenance, only the arithmetic of reserves versus liabilities. Accessibility collapse is very high (0.95): once the Triffin mechanism is understood, the impossibility of sustaining the arrangement becomes nearly absoluteâno alternative institutional tweak can resolve the gold/liquidity contradiction within the regime. Resistance is negligible (0.01): the constraint is not resisted because it is not a human imposition but a theorem-like property of the system.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is minimal for a mountain. The analytical observer and the historical institutional actor both confront the same structural contradiction. The only gap is temporal recognition: the institutional actor experienced the constraint as a series of contingent crises (gold drains, balance-of-payments deficits, sterling devaluation) before the analytical recognition of their common cause. From the structural seat, the outcome was always determined; from the experiential seat, it felt like a sequence of policy emergencies.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bretton Woods framework is the sole declared victim (role: payer, agent: false). Its directionality sits at the full-target end (d â 1.0): the constraint extracts the institutional arrangement's viability itself. No beneficiary is declared because no actor collects the extraction; the reserve currency issuer does not profit from the dilemmaâit is symmetrically trapped (d â 0.5). The gains, such as they are, accrue to no one; they are dissipated through the system's structural failure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a case of mandatrophyâan arrangement outliving its useful function. Rather, its function (stable gold-dollar convertibility with global liquidity provision) was structurally impossible from inception. The reading prevents mislabeling by distinguishing between institutional decay (a piton or snare where someone profits from maintenance) and logical impossibility (a mountain where the arrangement cannot persist regardless of maintenance). The Bretton Woods framework did not atrophy; it was consumed by a contradiction that no mandate could resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_alone_vs_composite,
    'Is the collapse of the Bretton Woods gold-exchange standard structurally inevitable due to the Triffin dilemma alone, or does it require an overdetermined composite of structural and political factors?',
    'Counterfactual historical analysis isolating the Triffin mechanism: estimating whether global liquidity demand consistently exceeded US gold reserve coverage under plausible alternative geopolitical and fiscal scenarios.',
    'If the Triffin mechanism alone suffices, this reading retains its mountain classification; if the collapse required additional political factors (Vietnam deficits, policy choices), the reading degrades to a contingent narrative and the mountain claim is falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_alone_vs_composite, conceptual, 'Whether the Triffin dilemma alone is sufficient for collapse or merely one factor among many.').

omega_variable(
    institutional_choice_boundary,
    'Did the August 15, 1971 suspension represent a discrete policy choice among viable alternatives, or was it the terminal recognition of a structurally pre-determined exhaustion with no viable alternative?',
    'Archival analysis of US Treasury and Federal Reserve deliberations in 1969-1971 to determine whether decision-makers perceived any alternative to suspension.',
    'If viable alternatives existed, the punctuated_swap reading is supported and this mountain reading''s inevitability claim is weakened; if no viable alternative existed, the mountain reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_choice_boundary, empirical, 'Whether the Nixon shock was a forced move or a discretionary swap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(triffin_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.02).
narrative_ontology:measurement(triffin_tr_t1950, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement(triffin_tr_t1955, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1955, 0.02).
narrative_ontology:measurement(triffin_tr_t1960, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1960, 0.02).
narrative_ontology:measurement(triffin_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.02).
narrative_ontology:measurement(triffin_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.02).

% Extraction over time
narrative_ontology:measurement(triffin_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.05).
narrative_ontology:measurement(triffin_be_t1950, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(triffin_be_t1955, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1955, 0.05).
narrative_ontology:measurement(triffin_be_t1960, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(triffin_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.05).
narrative_ontology:measurement(triffin_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__triffin_inevitability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel decomposes into three structurally distinct readings of the Bretton Woods collapse. This reading (triffin_inevitability) isolates the logical contradiction in reserve-currency gold standards as a mountain-level constraint. The punctuated_swap reading treats the collapse as a discrete institutional choice. The overdetermined_composite reading treats it as a multi-causal historical outcome. They are not the same constraint viewed from different angles; their epsilon values, beneficiary structures, and classifications differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
