% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Overdetermined Collapse of the Bretton Woods Fixed-Rate Regime
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story models the overdetermined_collapse_reading of the
 *   transition_causality kernel: the collapse of the Bretton Woods
 *   fixed-exchange-rate regime (1971-73) as structurally inevitable due to
 *   the Triffin Dilemma and multiple reinforcing contradictions. Under this
 *   reading, the reserve-currency fixed-rate system carries an endogenous,
 *   mountain-like logic whereby the liquidity-supply function systematically
 *   undermines the confidence function, making the transition a deterministic
 *   outcome rather than a contingent policy choice. All regime participants
 *   â including the reserve issuer and peg-nation central banks â are
 *   structurally constrained by this dynamic, with no actor capable of
 *   extracting rents from its operation.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer: Primary payer (institutional/global/constrained) â bears the liquidity-confidence tradeoff of the reserve role.
 *   - peg_nation_central_banks: Secondary payers (organized/national/constrained) â absorb adjustment costs and imported instability.
 *   - international_monetary_fund: Analytical observer (institutional/global/analytical) â administered rules without authority to resolve the reserve asymmetry.
 *   - private_capital_markets: Excluded actor (powerful/global/mobile) â transmitted imbalances while absent from the design table.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.12).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.08).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Overdetermined Collapse of the Bretton Woods Fixed-Rate Regime").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '847b8943-136c-4e19-8e2c-12affb0ad3b2').
narrative_ontology:cs_kernel_codification('847b8943-136c-4e19-8e2c-12affb0ad3b2', distributed).
narrative_ontology:cs_authority_grounding('847b8943-136c-4e19-8e2c-12affb0ad3b2', distributed).
narrative_ontology:cs_reading_relation('847b8943-136c-4e19-8e2c-12affb0ad3b2', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('847b8943-136c-4e19-8e2c-12affb0ad3b2', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('847b8943-136c-4e19-8e2c-12affb0ad3b2', foundational, triffin_contradiction_sufficient).
narrative_ontology:cs_axiom_status(triffin_contradiction_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('847b8943-136c-4e19-8e2c-12affb0ad3b2', triffin_contradiction_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('847b8943-136c-4e19-8e2c-12affb0ad3b2', foundational, counterfactual_avoidability_null).
narrative_ontology:cs_axiom_status(counterfactual_avoidability_null, holdable).
narrative_ontology:cs_axiom_grounding('847b8943-136c-4e19-8e2c-12affb0ad3b2', counterfactual_avoidability_null, empirically_contingent).
narrative_ontology:cs_reference_frame('847b8943-136c-4e19-8e2c-12affb0ad3b2', materialist_overdetermination).
narrative_ontology:cs_drift_state('847b8943-136c-4e19-8e2c-12affb0ad3b2', contemporary_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('847b8943-136c-4e19-8e2c-12affb0ad3b2', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, reserve_currency_issuer).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, peg_nation_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the global reserve currency under a fixed gold-dollar peg. Must run balance-of-payments deficits to supply world liquidity, which erodes gold-cover ratios and undermines convertibility credibility. Exit would require abandoning either the reserve currency role or the fixed rate, both carrying prohibitive geopolitical and systemic costs.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, reserve_currency_issuer, payer,
    institutional, generational, constrained, global).

% Hold dollar reserves and maintain fixed parities against the dollar and gold. Subject to imported inflation when the reserve issuer creates liquidity, and forced into deflationary domestic adjustment or unilateral devaluation to preserve pegs. Individual exit means leaving the fixed-rate club and losing predictable trade access.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, peg_nation_central_banks, payer,
    organized, generational, constrained, national).

% Administered the Bretton Woods Articles of Agreement, supervised exchange-rate parities, and provided balance-of-payments financing. Lacked authority to alter the reserve-currency asymmetry; functioned as an analytical and procedural observer of structural contradictions accumulating beyond its remedial mandate.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Arbitraged interest-rate differentials and convertibility windows across the fixed-rate grid. Their speculative flows transmitted and accelerated structural imbalances, yet they held no seat at the Bretton Woods design table and were formally excluded from the intergovernmental monetary architecture.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, private_capital_markets, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under a reserve-currency fixed-exchange-rate system, provides a stable nominal anchor and a unified liquidity mechanism for postwar trade and reconstruction.
% TRANSFER_FUNCTION: Transfers the cost of global liquidity provision asymmetrically to the reserve-issuer (erosion of gold-convertibility credibility) and to peg nations (imported inflation or deflationary adjustment burdens); concentrates seigniorage at the center while distributing adjustment obligations outward.
% ABSENT_VOICES: Private international capital and speculative markets were excluded from the Bretton Woods design; surplus nations seeking alternative reserve assets were marginalized; future generations bearing the long-run adjustment costs had no representation at the 1944 conference table.
% DISAPPEARANCE_RATIONALE: If the structural inevitability of collapse were removed, the fixed-rate regime could have persisted indefinitely; the 1971 suspension of gold convertibility, the shift to floating exchange rates, and the subsequent monetary order would not have occurred â the world rearranges around continued fixed parities.
% FOUNDING_PROBLEM: Provide a stable, rules-based international monetary order that supports multilateral trade and investment without relying on the automatic deflationary pressures of the classical gold standard.
% FOUNDING_PROBLEM_CORROBORATION: The problem's status is contested: neoclassical economists argue floating rates solved it, while structuralist IPE scholars argue the dollar's continued reserve role reproduces the same tension today. Corroboration from outside the US Treasury complex includes Triffin's original 1960 congressional testimony and subsequent critical international political economy literature from British and French finance ministry analyses during the 1960s gold-pool disputes.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.12 (endpoint) because the structural pressure of the Triffin dynamic mounts as dollar liabilities accumulate, but remains low because no agent captures the extraction â it is a dissipative structural cost. Suppression is 0.08 because a mountain persists without active coercion; the low theater ratio (0.15) captures the limited performative maintenance (gold-pool interventions, confidence rhetoric) relative to the massive structural forces at work. Accessibility collapse is 0.92: once the Triffin logic is understood, the inevitability of the confidence/liquidity contradiction collapses alternatives for sustaining the regime. Resistance is 0.08 because natural structural limits meet negligible active opposition â only denial, not genuine resistance.
 *
 * PERSPECTIVAL GAP:
 *   The reserve issuer and peg-nation central banks both compute as constrained payers, but at different scopes: the issuer faces a global-scope structural trap (its own currency undermines its gold peg), while peg nations face a national-scope trap (imported inflation or forced deflation). The IMF computes as an analytical observer because it had no structural leverage over the reserve-currency asymmetry; its seat is epistemic rather than extractive. Private capital markets, though excluded from the formal architecture, were mobile enough to arbitrage the contradictions, giving them a different directional signature than the trapped official sector.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary declarations are authored because the Triffin dynamic is not a rent-seeking arrangement â no agent structurally gains from the constraint's persistence. All declared stakeholders are either payers (bearing the structural costs of liquidity provision and adjustment) or excluded/mobile observers. Directionality therefore defaults toward the target end for all fixed-rate participants, with the reserve issuer sitting slightly closer to symmetric due to seigniorage offset, though the authored structural data treats the issuer as primarily paying the confidence cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a piton because its function did not atrophy into performance â the regime collapsed, fulfilling the structural prediction. It is not a snare because there is no identifiable agent coercing others to maintain the arrangement for extractive ends. The classification as mountain preserves the analytical distinction between structural necessity (the Triffin mechanism) and agentic extraction (policy choices that might have prolonged or accelerated the timing). Mandatrophy is avoided by recognizing that the constraint's disappearance (1971-73) was the structurally predicted outcome, not a sign of atrophied purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_mountain_or_narrative,
    'Is the Triffin Dilemma a genuine structural invariant of reserve-currency fixed-rate regimes, or a post-hoc narrative construct that naturalizes a contingent historical outcome?',
    'Comparative institutional analysis across historical and counterfactual reserve regimes (e.g., SDR-based, multi-polar) to test whether the dilemma reproduces; bibliometric study of when the narrative crystallized relative to the collapse.',
    'If constructed, reclassification to tangled_rope or snare (ideological constraint benefiting an analytical paradigm); if genuine structural invariant, mountain classification is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_mountain_or_narrative, conceptual, 'Whether the Triffin Dilemma is a natural law of monetary architecture or a constructed historiographical frame.').

omega_variable(
    overdetermination_vs_contingency,
    'Do multiple reinforcing contradictions truly eliminate counterfactual viability, or do they merely raise the probability of collapse while remaining contingent on policy choice?',
    'Deep counterfactual historical analysis and structural equation modeling of Bretton Woods constraints; archival study of policy deliberations in 1968-71 to identify decision points.',
    'If counterfactual viability is non-zero, the constraint degrades from mountain toward hybrid_trigger or contingent_choice territory, reducing accessibility collapse and raising resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overdetermination_vs_contingency, empirical, 'Whether the collapse was strictly inevitable or only highly probable.').

omega_variable(
    reading_suppression_in_academy,
    'Does the overdetermined collapse reading suppress the contingent choice reading through institutionalized historiographical gatekeeping rather than evidentiary superiority?',
    'Citation network analysis, curriculum archaeology in international political economy graduate programs, and review of editorial acceptance patterns at flagship journals.',
    'If suppression is structural, the mountain claim may be a false summit benefiting a dominant academic paradigm; reclassification would route through FSM to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_suppression_in_academy, empirical, 'Whether the overdetermined reading maintains dominance through epistemic capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.02).
narrative_ontology:measurement(tran_tr_t1950, transition_causality__overdetermined_collapse_reading, theater_ratio, 1950, 0.03).
narrative_ontology:measurement(tran_tr_t1958, transition_causality__overdetermined_collapse_reading, theater_ratio, 1958, 0.05).
narrative_ontology:measurement(tran_tr_t1960, transition_causality__overdetermined_collapse_reading, theater_ratio, 1960, 0.06).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.13).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.15).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.05).
narrative_ontology:measurement(tran_be_t1950, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1950, 0.06).
narrative_ontology:measurement(tran_be_t1958, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1958, 0.07).
narrative_ontology:measurement(tran_be_t1960, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.09).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.11).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1944, 0.05).
narrative_ontology:measurement(tran_su_t1950, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(tran_su_t1958, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1958, 0.06).
narrative_ontology:measurement(tran_su_t1960, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1960, 0.06).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.07).
narrative_ontology:measurement(tran_su_t1971, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1971, 0.08).
narrative_ontology:measurement(tran_su_t1973, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1973, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the transition_causality kernel, decomposed from the colloquial label 'Bretton Woods collapse' into three structurally distinct claims: overdetermined collapse (mountain), contingent choice (agentic), and hybrid trigger (mixed causality). Each reading carries a different epsilon, stakeholder structure, and classification. This decomposition follows the epsilon-invariance principle: the colloquial label conflates claims with different empirical bases, failure modes, and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
