% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Triffin Dilemma Structural Inevitability
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint instantiates the triffin_inevitability reading of the
 *   monetary_anchor_principle kernel. It treats the collapse of the Bretton
 *   Woods gold-exchange standard not as a policy failure or discrete
 *   institutional choice, but as the necessary outcome of a logical
 *   contradiction: a reserve-currency issuer under a gold peg must run
 *   deficits to supply global liquidity, yet those same deficits erode the
 *   gold reserves required to maintain the peg. The Bretton Woods framework
 *   is the institutional victim of this structural arithmetic. The reading
 *   claims Mountain status because the constraint is a physical/logical
 *   impossibility, not a negotiated arrangement. No beneficiary is declared
 *   because the system fails as a whole; the cost is the dissolution of the
 *   framework itself.
 *
 * KEY AGENTS:
 *   - bretton_woods_framework (institutional/payer) â the monetary arrangement that bears the structural cost and collapses under the arithmetic of the Triffin dilemma
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.0).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma Structural Inevitability").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91').
narrative_ontology:cs_kernel_codification('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', formalized).
narrative_ontology:cs_authority_grounding('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', lineage).
narrative_ontology:cs_interpretation_layer_present('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91').
narrative_ontology:cs_reading_relation('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', foundational, reserve_currency_deficit_necessity).
narrative_ontology:cs_axiom_status(reserve_currency_deficit_necessity, holdable).
narrative_ontology:cs_axiom_grounding('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', reserve_currency_deficit_necessity, empirically_contingent).
narrative_ontology:cs_axiom('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', foundational, gold_liquidity_incompatibility).
narrative_ontology:cs_axiom_status(gold_liquidity_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', gold_liquidity_incompatibility, empirically_contingent).
narrative_ontology:cs_reference_frame('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', gold_exchange_stability_framework).
narrative_ontology:cs_drift_state('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', post_convertibility_suspension_1971, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('29c5e0db-b7e5-4d5d-91eb-542a6ef0ae91', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_framework).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma_proposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The international monetary framework established in 1944 that pegged major currencies to the US dollar and the dollar to gold. It was structurally unable to resolve the contradiction between supplying adequate global liquidity (requiring persistent US balance-of-payments deficits) and maintaining gold convertibility (requiring stable or growing US gold reserves). As US gold reserves depleted, the framework bore the full systemic cost of the arithmetic and was abandoned in 1971.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_framework, payer,
    institutional, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The constraint describes a structural logical impossibility inherent in the gold-exchange standard, not a human coordination arrangement.
% TRANSFER_FUNCTION: No agent-to-agent transfer occurs; the 'cost' is systemic instability that accumulates within the reserve-currency issuer's balance of payments and ultimately destroys the institutional framework itself.
% ABSENT_VOICES: Early critics of the gold-exchange standardânotably Robert Triffin and Keynesian structuralist economistsâwho identified the liquidity-convertibility contradiction during the 1950s were marginalized in policy circles dominated by fixed-exchange-rate orthodoxy. Their exclusion meant the dilemma was institutionalized before being widely recognized.
% DISAPPEARANCE_RATIONALE: If the logical contradiction between reserve-currency deficit supply and gold-convertibility requirements did not exist, the Bretton Woods framework could have persisted indefinitely without the terminal depletion of gold reserves; the global monetary architecture of 1944â1971 would not have been forced into abandonment.
% FOUNDING_PROBLEM: The need to reconstruct a stable international monetary order after WWII that provided exchange-rate stability, global liquidity, and confidence while avoiding the competitive devaluations of the interwar period.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians such as Barry Eichengreen and Benn Steil, along with IMF post-mortem analyses, corroborate from analytical seats outside the framework's administrative core that the gold-exchange standard's design contained an internal contradiction which made its dissolution a matter of arithmetic rather than mere policy failure.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is near-zero (0.05) because the constraint is a logical identity, not an extraction mechanism; there is no agent collecting rents from its operation. Suppression is zero because no enforcement is required for a logical contradiction to hold. Accessibility collapse is high (0.95) because once the arithmetic is understood, the impossibility of a stable gold-exchange reserve-currency regime becomes transparent. Resistance is minimal (0.05) because the constraint operates as structural necessity, though political actors resisted acknowledging the implication until 1971. Theater ratio is zero: there is no performative maintenance of a mathematical contradiction.
 *
 * PERSPECTIVAL GAP:
 *   There is limited perspectival gap because the constraint is a logical mountain visible from all analytical seats. However, policymakers within the frameworkâe.g., the US Treasury and Federal Reserveâexperienced the constraint as a political problem to be managed rather than as a logical impossibility, creating a temporary observer-participant divergence that resolved with the 1971 suspension.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary is declared. The sole named seat, bretton_woods_framework, is the institutional payer: it bears the cost of the structural instability and is destroyed by it. Directionality for the framework is near the target end because the constraint extracts the framework's viability until collapse. No directionality overrides are needed because the structural derivation is straightforward: a logical impossibility permits no beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy mislabeling by distinguishing the logical structure of the Triffin dilemma from the institutional scaffolding that attempted to maintain Bretton Woods. The framework itself was a coordination mechanism with a design flaw; the underlying constraint is a Mountain. Classifying the arithmetic as a Snare or Rope would mistake a logical limit for a negotiated arrangement, while treating the framework's collapse as mere policy failure would ignore the structural impossibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_sufficiency_ambiguity,
    'Is the Triffin dilemma sufficient alone to explain the collapse of Bretton Woods, or does its explanatory power depend on conjunctural factors such as Vietnam-era fiscal deficits, accelerating capital mobility, and domestic policy preferences that are external to the pure logical structure?',
    'Historical counterfactual analysis: if global liquidity demand had grown more slowly or US fiscal policy had been more restrictive, would the collapse still have occurred on the same trajectory?',
    'If conjunctural factors were necessary, the constraint is better classified as a tangled_rope or as part of an overdetermined composite rather than as a pure mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_sufficiency_ambiguity, empirical, 'Whether the Triffin dilemma is singly sufficient or contingent on other historical factors.').

omega_variable(
    kernel_reading_contest,
    'Does the triffin_inevitability reading represent a genuine mountain (logical impossibility), or does it function as a false-summit mountain that naturalizes a policy choice by treating the Nixon shock as structurally predetermined?',
    'Examine whether alternative institutional designsâsuch as scaled SDR issuance, multilateral clearing mechanisms, or higher gold pricesâcould have resolved the liquidity-convertibility tension without framework collapse.',
    'If viable alternative designs existed, the inevitability claim naturalizes a policy narrative and the constraint reclassifies toward tangled_rope; if no design could resolve the arithmetic, the mountain classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether Triffin inevitability is a genuine logical limit or a naturalized policy narrative.').

omega_variable(
    institutional_victim_ontology,
    'Can an institutional framework like Bretton Woods be coherently modeled as a victim/payer of a structural logical constraint, or does treating a non-biological system as a victim anthropomorphize the analysis?',
    'Cross-corpus comparison with other institutional-constraint stories where abstract frameworks dissolve under logical or arithmetic pressure.',
    'If institutional victimhood is rejected, the story would require reseating the cost onto human agentsâpolicymakers, taxpayers, or trading nationsâthereby altering directionality and potentially the computed classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_victim_ontology, conceptual, 'Ontological status of institutional frameworks as constraint victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is the triffin_inevitability reading of the monetary_anchor_principle kernel. The colloquial label 'Bretton Woods collapse' conflates structurally distinct causal claims: structural inevitability (this file), discrete institutional choice (punctuated_swap_reading), and overdetermined composite causation (overdetermined_composite_reading). Decomposed per the Îµ-invariance principle; each reading carries its own Îµ, beneficiaries, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
