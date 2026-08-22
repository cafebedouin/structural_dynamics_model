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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma as Structural Inevitability of Gold-Standard Collapse
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the Triffin-inevitability reading of the
 *   monetary_anchor_principle kernel: the claim that the Bretton Woods
 *   gold-dollar system's collapse was structurally forced by an arithmetic
 *   contradiction (reserve-currency liquidity supply necessarily exhausting a
 *   fixed gold base), not a discrete policy choice or an overdetermined
 *   bundle of contingent pressures. Under this reading, the constraint is a
 *   mountain — a mathematical/structural impossibility rather than an
 *   extraction mechanism. No party benefits from the contradiction; the
 *   Bretton Woods framework itself is the structural victim, absorbing the
 *   cost of a design premise that could not survive its own success (growing
 *   world trade demanding growing dollar liquidity). This reading's ε is
 *   authored very low because, on its own terms, there was no policy
 *   discretion to extract through — the outcome followed from the arithmetic
 *   regardless of who administered it. Sibling readings
 *   (punctuated_swap_reading, overdetermined_composite_reading) are separate
 *   constraints, not alternative measurements of this one; per the
 *   ε-invariance principle they are linked via network.affects_constraints
 *   and are not described further here beyond the omega variables that record
 *   the disagreement.
 *
 * KEY AGENTS:
 *   - bretton_woods_institutional_framework: structural victim (institutional/trapped) — absorbs the cost of the internal contradiction
 *   - united_states_monetary_authorities: agenda_setter within the constraint's bounds (institutional/constrained) — administered the peg but did not create the arithmetic
 *   - foreign_central_banks: payer (institutional/constrained) — held the growing dollar claims that constituted the exhaustion pressure
 *   - monetary_historians_and_economists: analytical observer — formalized and traced the dilemma
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.06).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.04).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma as Structural Inevitability of Gold-Standard Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, 'e7719e7e-b497-42fc-a281-deeade5f2c76').
narrative_ontology:cs_kernel_codification('e7719e7e-b497-42fc-a281-deeade5f2c76', distributed).
narrative_ontology:cs_authority_grounding('e7719e7e-b497-42fc-a281-deeade5f2c76', distributed).
narrative_ontology:cs_reading_relation('e7719e7e-b497-42fc-a281-deeade5f2c76', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7719e7e-b497-42fc-a281-deeade5f2c76', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('e7719e7e-b497-42fc-a281-deeade5f2c76', foundational, fixed_metallic_anchor_under_growing_liquidity_demand_is_mathematically_unsustainable).
narrative_ontology:cs_axiom_status(fixed_metallic_anchor_under_growing_liquidity_demand_is_mathematically_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('e7719e7e-b497-42fc-a281-deeade5f2c76', fixed_metallic_anchor_under_growing_liquidity_demand_is_mathematically_unsustainable, empirically_contingent).
narrative_ontology:cs_axiom('e7719e7e-b497-42fc-a281-deeade5f2c76', secondary, no_policy_discretion_could_have_averted_the_outcome_only_its_timing).
narrative_ontology:cs_axiom_status(no_policy_discretion_could_have_averted_the_outcome_only_its_timing, holdable).
narrative_ontology:cs_axiom_grounding('e7719e7e-b497-42fc-a281-deeade5f2c76', no_policy_discretion_could_have_averted_the_outcome_only_its_timing, empirically_contingent).
narrative_ontology:cs_reference_frame('e7719e7e-b497-42fc-a281-deeade5f2c76', bretton_woods_fixed_gold_dollar_anchor).
narrative_ontology:cs_drift_state('e7719e7e-b497-42fc-a281-deeade5f2c76', nixon_shock_1971, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('e7719e7e-b497-42fc-a281-deeade5f2c76', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, united_states_monetary_authorities).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, foreign_central_banks).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma_formalization).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, reserve_currency_liquidity_tradeoff_theorem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The fixed-parity gold-dollar convertibility system built at Bretton Woods in 1944 depended on the United States running the world's reserve currency while also maintaining a credible gold peg. As global trade and dollar-denominated reserves grew relative to US gold stock, the framework's own operating requirements pulled apart: supplying enough dollars to lubricate world trade required deficits, and deficits required by that same liquidity role eroded the credibility of the peg the whole framework rested on. The framework bears the cost of the contradiction it was built on; it has no way to resolve it internally and no exit short of its own dissolution.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).

% As the reserve-currency issuer, the US administers the gold window and sets the peg, but is structurally compelled to run balance-of-payments deficits to supply the dollars the rest of the world needs to trade and hold as reserves. It did not choose the arithmetic; it operated within it until August 1971, when Nixon closed the gold window because the reserve ratio had become mathematically untenable. It set policy at the margins (timing, communication) but not the underlying constraint.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, united_states_monetary_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__triffin_inevitability_reading, united_states_monetary_authorities, payer).

% Held growing dollar reserves as the system's designed liquidity mechanism, which simultaneously constituted a growing claim against a gold stock that could not expand to match it. Each central bank could convert dollars to gold, but a system-wide rush to convert would itself trigger the collapse it feared — a coordination trap inherent to the reserve arrangement, not a policy failure by any single bank.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, foreign_central_banks, payer,
    institutional, generational, constrained, global).

% The expanding volume of world trade needed a growing supply of internationally acceptable liquidity; this need is the demand side of the mathematical contradiction and has no seat at any negotiating table — it is a structural fact about growth outpacing a fixed metallic base, not an actor that could have voted for a different outcome.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, global_trade_and_payments_system, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, global_trade_and_payments_system).

% Formalized the dilemma (Triffin 1960) and traced its arithmetic through the 1960s reserve data. This seat observes the structural contradiction without being a party to its costs or benefits.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, monetary_historians_and_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None at the level of this specific claim — the Triffin dilemma describes a structural impossibility in the gold-exchange system, not a coordination mechanism anyone designed or maintains. There is no genuine coordination function to name here; the arithmetic is a constraint on any coordination attempt built on a fixed metallic reserve serving as global liquidity backing.
% TRANSFER_FUNCTION: No sustained transfer between parties — the dilemma describes an exhaustion process (dollar liabilities growing relative to a fixed gold stock) rather than an extraction flow from one identifiable party to another. What is 'transferred' is credibility: as dollar claims accumulate, the peg's credibility is drawn down until convertibility becomes non-credible for everyone simultaneously.
% ABSENT_VOICES: There is no excluded party with a suppressed alternative — the mathematics does not admit an alternative for anyone to advocate. If there is an absent voice, it is the counterfactual designer who might have built a reserve system without a fixed metallic anchor from the outset, but this is a design-history question, not a voice suppressed by this constraint's operation.
% DISAPPEARANCE_RATIONALE: If the Triffin dilemma's underlying arithmetic were somehow false — if gold reserves could scale with global liquidity demand without limit, or if reserve-currency issuance did not require deficits — the entire postwar monetary architecture's central design tension disappears, and the Bretton Woods system could in principle have persisted indefinitely on its original terms. The framework's structural payer (Bretton Woods itself) exists as a stakeholder precisely because it depends on an assumption this dilemma falsifies.
% FOUNDING_PROBLEM: Bretton Woods was built to give the postwar world a stable, credible international monetary anchor by fixing the dollar to gold and other currencies to the dollar, avoiding the competitive devaluations and monetary chaos of the interwar period.
% FOUNDING_PROBLEM_CORROBORATION: IMF historical staff papers and independent economic historians (Eichengreen, Bordo) outside any government's reserve-currency interest corroborate that the fixed-gold-anchor design became mathematically unsustainable by the late 1960s independent of any single government's policy choices; the founding problem of postwar exchange-rate stability was addressed by successor arrangements (floating rates, SDRs) rather than by the original anchor design, which no serious monetary authority now proposes restoring on its original terms.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.06, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near-zero (0.06 by 1971) because, under this reading, no party is extracting value through the constraint's operation — the contradiction is a structural feature of a fixed-anchor reserve system meeting growing liquidity demand, not a rent-seeking mechanism. Suppression is likewise near-zero (0.04): nothing was suppressed because there was no viable alternative for any actor to pursue once the reserve ratio math turned adverse. Accessibility_collapse is high (0.88): once the dilemma is understood, no alternative arrangement remains open to a reserve issuer under a fixed-metallic-convertibility promise. Resistance is low (0.15): the theoretical inevitability the reading asserts is not something a party can resist by acting differently, only postpone via technical measures (gold pool interventions, swap lines) that show up as the modest theater_ratio rise (0.05 to 0.10) — bureaucratic patching around an arithmetic wall, not genuine alternative-generation.
 *
 * PERSPECTIVAL GAP:
 *   From the US monetary authorities' seat, the constraint looks like a slow-motion forced hand — administering a peg they could delay defending but not indefinitely sustain. From foreign central banks' seat, it looks like a coordination trap: individually rational reserve accumulation collectively guaranteed the reserve ratio's exhaustion. Both seats experience the same underlying mountain differently by position, but neither seat's structural data supports a beneficiary — this is what distinguishes the mountain reading from the sibling readings, which locate discretion (and therefore possible beneficiaries) at different points in the same history.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary is declared, consistent with a genuine mountain reading: nobody collects rents from the exhaustion dynamic. Bretton Woods as an institutional framework is named as payer/victim because the structure bears the cost of its own founding contradiction; the United States and foreign central banks are payers in a secondary sense (constrained exit — they operated within the arithmetic, could not unilaterally escape it, but were not targeted extraction victims of another party's design).
 *
 * MANDATROPHY ANALYSIS:
 *   Because the founding_problem (postwar exchange-rate stability) is authored as dead while the disappearance_verdict is world_rearranges, this story deliberately surfaces the mismatch the R5 interview is designed to catch: the original fixed-anchor design no longer functions and nothing currently proposes restoring it, yet the counterfactual analytic still matters for evaluating successor arrangements (floating rates, SDR frameworks) that were built in direct response to this constraint's structural lesson. This is not treated as evidence of capture — there is no beneficiary collecting from the dead mandate — but as confirmation that a mountain-type constraint can have a fully resolved founding problem without becoming a piton, because pitons require an administrator with something to lose from repair, and this constraint has none.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_sufficiency_vs_composite_causation,
    'Was the Triffin dilemma alone structurally sufficient to force gold-standard abandonment, or was it one necessary component among several jointly sufficient causes (Vietnam deficits, policy choices, capital mobility)?',
    'Counterfactual economic-historical modeling: would gold-dollar convertibility have survived past 1971 absent Vietnam-era deficit spending, holding the Triffin liquidity-demand growth path fixed? Comparative analysis against other reserve-currency episodes without comparable fiscal shocks.',
    'If Triffin dynamics alone were sufficient, this reading''s mountain classification is well-supported and the sibling overdetermined_composite_reading is the weaker account. If Triffin dynamics were necessary but not sufficient without the added fiscal/policy pressures, the composite reading better describes the actual historical mechanism and this reading overstates inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_sufficiency_vs_composite_causation, empirical, 'Whether Triffin dynamics alone, versus a bundle of causes, actually drove the 1971 collapse.').

omega_variable(
    discretion_at_the_margin,
    'Even if the underlying arithmetic was inevitable in the long run, did US monetary authorities and other central banks retain meaningful discretion over the timing, sequencing, and manner of abandonment — discretion that could itself have created winners and losers?',
    'Archival analysis of Federal Reserve and Treasury deliberations 1968-1971; examination of whether alternative sequencing (earlier devaluation, coordinated multilateral adjustment) was seriously considered and rejected for reasons other than pure arithmetic.',
    'If meaningful discretion existed at the margin, some component of what this reading treats as pure structural necessity may in fact be better modeled by the punctuated_swap_reading, which would introduce agency and potentially a beneficiary (whoever''s interests were served by the particular timing chosen) that this mountain reading''s zero-beneficiary structure excludes by construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_at_the_margin, conceptual, 'Whether apparent inevitability concealed meaningful policy discretion at the margins of timing and sequencing.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three kernel readings (triffin_inevitability, punctuated_swap, overdetermined_composite) disagree — is it about the presence of agency, the number of causal factors, or the level of description (structural arithmetic vs. discrete decision vs. multi-causal history)?',
    'This is a committer-structure question, not an empirical one: it is resolved by careful comparison of the three constraint stories'' beneficiary/victim structures and claimed types, not by new data about 1971.',
    'The location of disagreement determines which reading a given analyst or institution finds most useful: the triffin_inevitability_reading is most useful for arguing no policy blame attaches to any specific government; punctuated_swap_reading is most useful for assigning agency and accountability to specific 1971 decision-makers; overdetermined_composite_reading is most useful for historians unwilling to isolate a single cause. None is more ''true'' independent of the analytical purpose; this is exactly the reading-indexation the kernel framework is built to preserve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'The three sibling readings disagree about where agency and causal weight are located in the same historical transition, not about the raw facts of what happened.').

omega_variable(
    mountain_vs_constructed_reserve_system,
    'Is the gold-dollar convertibility contradiction a genuine natural-law-like mathematical inevitability, or was it a constructed feature of choosing gold specifically (rather than some other, more elastic reserve mechanism) as the anchor — making the ''inevitability'' conditional on a prior, non-inevitable design choice?',
    'Comparative institutional analysis: examine whether any reserve-currency-anchored system with an elastic (non-fixed-supply) backing asset would face the same exhaustion dynamic. If elastic-backing systems avoid the trap, the ''inevitability'' is conditional on the 1944 design choice, not a mountain in the strict sense.',
    'If the contradiction is conditional on the specific choice of gold as anchor, this reading''s mountain classification would be better understood as a mountain-within-a-choice: inevitable GIVEN the gold anchor, but the gold anchor itself was a human institutional choice with alternatives. This would not change this story''s authored ε (which is about the arrangement under contest, the gold-anchored system, assessed by this reading''s own lights) but bears on whether ''mountain'' is the right label for the meta-level design question versus this specific operational-level claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_reserve_system, conceptual, 'Whether the mathematical inevitability is unconditional or conditional on the prior choice of a fixed-supply metallic anchor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(mone_tr_t1950, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1958, 0.06).
narrative_ontology:measurement(mone_tr_t1963, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1963, 0.08).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1968, 0.09).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.02).
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1958, 0.03).
narrative_ontology:measurement(mone_be_t1963, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1963, 0.04).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1968, 0.05).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__triffin_inevitability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__triffin_inevitability_reading, 0.15).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the monetary_anchor_principle kernel, decomposed per the ε-invariance principle because the natural-language label 'the 1971 gold standard collapse' covers structurally distinct claims about causation and agency. triffin_inevitability_reading claims a mountain (near-zero ε, no beneficiary, structural necessity). punctuated_swap_reading (sibling, separate file) would claim a discrete institutional decision with identifiable decision-makers and potentially identifiable beneficiaries of the specific timing/terms chosen — a different ε and likely a different type (plausibly tangled_rope or scaffold rather than mountain). overdetermined_composite_reading (sibling, separate file) would claim multi-causal sufficiency with partial contributions from Triffin dynamics, Vietnam deficits, and policy consensus — an intermediate ε reflecting that policy choices layered rent-seeking or misallocation onto a genuinely structural pressure. All three share the same underlying historical event but assign different weight to necessity versus choice, and therefore different beneficiary/victim structures and different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
