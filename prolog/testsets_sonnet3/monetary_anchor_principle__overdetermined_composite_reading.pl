% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Post-Bretton Woods Fiat Discretion as Overdetermined Structural Collapse
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the overdetermined-composite reading of the
 *   monetary_anchor_principle kernel: the claim that the 1971 abandonment of
 *   gold convertibility resulted from the joint, mutually-reinforcing
 *   convergence of the Triffin dilemma, Vietnam War deficit spending, the
 *   Keynesian policy consensus favoring discretionary demand management, and
 *   rising technological capital mobility (Eurodollar markets,
 *   telecommunications-enabled arbitrage). Under this reading, no single
 *   causal stream was sufficient on its own — the collapse was structurally
 *   overdetermined by late the 1960s, meaning any one factor's removal would
 *   likely have delayed but not prevented the transition. This differs
 *   sharply from the punctuated_swap_reading (a discrete institutional choice
 *   on a single date) and the triffin_inevitability_reading (a
 *   single-mechanism inevitability claim). The ε here is authored HIGH
 *   because the overdetermination framing treats the resulting discretionary
 *   fiat regime as effectively irreversible without simultaneously addressing
 *   all four causal streams — a far more entrenched extraction structure than
 *   either sibling reading implies, since reversal would require coordinated
 *   correction across fiscal, monetary, and capital-mobility domains at once.
 *
 * KEY AGENTS:
 *   - federal_fiscal_authorities: institutional beneficiary who gained discretionary monetary capacity
 *   - keynesian_policy_establishment: institutional beneficiary whose policy framework was vindicated
 *   - multinational_capital_mobility_actors: powerful beneficiary positioned to arbitrage the transition regardless of trigger
 *   - fixed_income_savers: powerless payer bearing inflation erosion with no exit
 *   - foreign_dollar_reserve_holders: moderate-power payer absorbing unilateral abrogation of convertibility promise
 *   - wage_earners_under_inflation: powerless payer bearing stagflation costs
 *   - gold_standard_defenders: excluded voice whose retrenchment alternative was treated as foreclosed
 *   - monetary_historians: analytical observer reconstructing causal weights, generating the sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.78).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.58).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Post-Bretton Woods Fiat Discretion as Overdetermined Structural Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '50284ba1-81cb-46bb-8845-1f4b6e515e39').
narrative_ontology:cs_kernel_codification('50284ba1-81cb-46bb-8845-1f4b6e515e39', distributed).
narrative_ontology:cs_authority_grounding('50284ba1-81cb-46bb-8845-1f4b6e515e39', distributed).
narrative_ontology:cs_reading_relation('50284ba1-81cb-46bb-8845-1f4b6e515e39', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('50284ba1-81cb-46bb-8845-1f4b6e515e39', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('50284ba1-81cb-46bb-8845-1f4b6e515e39', foundational, collapse_was_multiply_overdetermined).
narrative_ontology:cs_axiom_status(collapse_was_multiply_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('50284ba1-81cb-46bb-8845-1f4b6e515e39', collapse_was_multiply_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('50284ba1-81cb-46bb-8845-1f4b6e515e39', secondary, no_single_causal_stream_was_individually_sufficient).
narrative_ontology:cs_axiom_status(no_single_causal_stream_was_individually_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('50284ba1-81cb-46bb-8845-1f4b6e515e39', no_single_causal_stream_was_individually_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('50284ba1-81cb-46bb-8845-1f4b6e515e39', bretton_woods_fixed_convertibility_regime).
narrative_ontology:cs_drift_state('50284ba1-81cb-46bb-8845-1f4b6e515e39', post_1971_smithsonian_collapse, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('50284ba1-81cb-46bb-8845-1f4b6e515e39', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, federal_fiscal_authorities).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, multinational_capital_mobility_actors).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_savers).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, wage_earners_under_inflation).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, structural_overdetermination_thesis).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_diagnosis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Freed from the gold-conversion constraint, the U.S. Treasury and Federal Reserve gain discretionary capacity to finance deficits (Vietnam War spending, Great Society programs) through money creation rather than gold-backed discipline. They administer the resulting fiat framework and benefit from expanded fiscal and monetary latitude.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, federal_fiscal_authorities, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, federal_fiscal_authorities, agenda_setter).

% Academic and technocratic consensus favoring countercyclical demand management gains institutional legitimacy once gold-standard constraints are removed. They advised and validated the policy shift and retained influence over subsequent monetary theory and central bank practice.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment, beneficiary,
    institutional, generational, mobile, national).

% Eurodollar markets and multinational banks exploit growing cross-border capital mobility, which both accelerated the Triffin pressure on gold reserves and profits enormously once currencies float. They can arbitrage across jurisdictions and were structurally positioned to gain from the shift regardless of which specific trigger forced it.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, multinational_capital_mobility_actors, beneficiary,
    powerful, biographical, arbitrage, global).

% Holders of bonds, pensions, and cash savings see the implicit anchor against currency debasement removed. Subsequent 1970s inflation erodes real value of their holdings; they have no meaningful exit from currency-denominated savings and did not participate in the policy decision.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Foreign central banks holding dollar reserves under the Bretton Woods gold-convertibility promise absorb the unilateral abrogation of that promise. They can diversify reserves only slowly and at high transition cost; the composite structural pressures left them with no advance warning mechanism proportional to the stakes.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, foreign_dollar_reserve_holders, payer,
    moderate, generational, constrained, global).

% Workers whose wages lag inflation once the monetary anchor is removed bear real income erosion through the 1970s stagflation period. They have essentially no exit — labor markets are geographically and institutionally sticky — and did not participate in the structural-pressure calculus economists later invoke to explain the collapse.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, wage_earners_under_inflation, payer,
    powerless, biographical, trapped, national).

% Economists and policymakers who argued for defending convertibility through fiscal retrenchment or gold price revaluation were structurally sidelined once the composite-pressure narrative took hold; their alternative (address deficits, not abandon the anchor) was treated as politically foreclosed rather than empirically tested.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_defenders, excluded,
    moderate, civilizational, constrained, national).

% Retrospective analysts who reconstruct the causal weights among Triffin dynamics, war deficits, Keynesian consensus, and capital mobility. Their disagreement about which factor was decisive is exactly what generates the sibling readings of this kernel.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, diffuse).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state fiscal and monetary policy space by removing a rigid external constraint (gold convertibility) that had previously required cross-institutional discipline among Treasury, Federal Reserve, and foreign central banks to maintain a fixed exchange peg.
% TRANSFER_FUNCTION: Moves purchasing power and monetary discipline away from savers, wage earners, and foreign reserve holders toward fiscal and monetary authorities and capital-mobile actors, via the inflation and currency-depreciation channel that discretionary money creation opens.
% ABSENT_VOICES: Gold standard defenders and fixed-income savers had no seat in the structural-pressure diagnosis; the overdetermination framing treats the outcome as inevitable, which forecloses the counterfactual question of whether earlier fiscal retrenchment could have preserved convertibility without the same distributional costs.
% DISAPPEARANCE_RATIONALE: If the composite of structural pressures had not existed (i.e., the deficits, Triffin pressure, capital mobility, and Keynesian consensus had not jointly converged), the gold-exchange standard could plausibly have persisted longer or been reformed rather than abandoned outright — global reserve arrangements, inflation trajectories, and the entire architecture of floating exchange rates since 1971 would look substantially different.
% FOUNDING_PROBLEM: The composite of pressures purportedly needed resolution because the Bretton Woods gold-exchange system could not simultaneously supply adequate global dollar liquidity, sustain U.S. deficit spending (military and domestic), and maintain gold convertibility at a fixed price — the system's own internal contradictions made some adjustment path necessary.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians (outside both the fiscal-authority and Keynesian-establishment seats) corroborate that reserve-liquidity tension was real and documented well before 1971, but they dispute the overdetermination reading's implicit claim of inevitability — some argue a narrower Triffin-only account or a discrete-choice account explains the timing better, which is precisely why this kernel has three competing readings.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78) and rising steadily across the interval because the composite-pressure framing legitimizes the removal of an external discipline mechanism (gold convertibility) as an emergent necessity rather than a discretionary choice — this framing itself does extractive work by naturalizing an outcome that benefited fiscal and capital-mobile actors. Suppression is moderate (0.58): the overdetermination narrative suppresses the counterfactual (could earlier fiscal discipline have preserved convertibility?) by treating the outcome as causally inevitable, which forecloses debate more subtly than the punctuated-swap reading's single decisive act would. Theater ratio is moderate (0.42) reflecting the genuine explanatory content of the multi-causal account alongside its use as retrospective justification. Accessibility collapse is high (0.72) — once the composite-pressure narrative is accepted, alternative single-cause or discrete-choice framings appear naive by comparison, collapsing the interpretive space. Resistance is moderate (0.55): gold standard defenders and hard-money economists continued to contest the inevitability claim throughout the measured interval.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of federal fiscal authorities and the Keynesian establishment, this is a genuine tangled_rope: real coordination problem (reconciling global liquidity needs, war financing, and domestic policy) solved by removing a binding constraint, at a real but justified cost. From the seat of fixed-income savers and wage earners, the same structure computes closer to pure extraction — a discipline mechanism was removed specifically because it constrained the policy space of the powerful, and the overdetermination narrative provides retrospective cover by making the outcome sound like physics rather than choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal fiscal authorities, the Keynesian establishment, and capital-mobile multinational actors sit near the beneficiary end of directionality: each gained expanded discretion, institutional validation, or arbitrage opportunity from the transition, and each is structurally positioned to benefit regardless of which specific causal stream is emphasized. Fixed-income savers, foreign reserve holders, and wage earners sit near the target end: they bore the inflationary and currency-depreciation costs of discretionary money creation and had no meaningful voice in the policy calculus, let alone the retrospective causal narrative. Gold standard defenders are excluded rather than harmed directly through the transfer function — their exclusion is from the deliberative process itself, which the overdetermination framing retroactively justifies as pressures too vast for any single policy correction to have averted.
 *
 * MANDATROPHY ANALYSIS:
 *   The overdetermination reading resists mandatrophy-style over-simplification in one direction (it does not claim a single villain or single decisive moment) but risks a different failure: treating structural inevitability claims as excusing agency. Multiple genuine mountains (gold's fixed physical scarcity, the mathematical impossibility of a reserve-currency issuer running permanent surpluses under Triffin logic, the reality of capital mobility once telecommunications matured) DID feed into the policy space — these are real upstream constraints. But the specific POLICY RESPONSE (unlimited discretionary fiat money creation rather than, say, a revalued gold price, an SDR-based reform, or coordinated fiscal retrenchment) was a choice made within that constrained space, not dictated by it. The tangled_rope classification captures this: genuine coordination function (resolving an unsustainable fixed-rate system) coexists with asymmetric extraction (the specific choice of unconstrained fiat discretion disproportionately benefited fiscal/monetary authorities and capital-mobile actors over savers and wage earners).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_single_cause_ambiguity,
    'Was the gold standard collapse genuinely overdetermined by multiple independent structural pressures, or does the overdetermination framing itself serve as retrospective justification that obscures a narrower, more contestable set of policy choices (e.g., the specific decision not to raise the gold price or coordinate fiscal retrenchment)?',
    'Counterfactual economic-historical analysis: model whether removing any single pressure (e.g., Vietnam deficit spending held constant at pre-1965 levels, or a coordinated SDR-based reform adopted in 1965) would have been sufficient to preserve convertibility through the 1970s. If removing any single factor prevents collapse, the composite reading is overclaiming inevitability; if collapse persists across all single-factor counterfactuals, the composite reading is supported.',
    'If a single factor (e.g., Triffin dynamics alone) is shown sufficient, this reading collapses into the triffin_inevitability_reading and the extractiveness/irreversibility claims of THIS reading are overstated. If truly overdetermined, the high ε and tangled_rope classification are well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_single_cause_ambiguity, conceptual, 'Whether structural overdetermination is a genuine causal finding or a retrospective legitimation narrative for discretionary policy choices.').

omega_variable(
    beneficiary_capture_of_composite_narrative,
    'Did the fiscal and monetary authorities who benefited from removing gold convertibility play a role in constructing or popularizing the overdetermination narrative itself, such that the explanatory framework is partly self-serving?',
    'Archival and intellectual-history review of when and by whom the ''overdetermined composite'' framing was first articulated relative to the 1971 decision, and cross-reference against the professional and institutional positions of the framing''s early proponents.',
    'If the composite framing emerged primarily from Federal Reserve, Treasury, and allied academic economists after the fact, this strengthens the reading that the narrative functions partly as retrospective cover for a beneficial policy choice, supporting the tangled_rope classification''s extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_composite_narrative, empirical, 'Whether the overdetermination account originated from, and serves the interests of, the policy actors it exculpates.').

omega_variable(
    kernel_framing_choice_omega,
    'The monetary_anchor_principle kernel could be read as (a) a single discrete institutional swap (punctuated_swap_reading), (b) a single-mechanism inevitability (triffin_inevitability_reading), or (c) a multi-causal overdetermined convergence (this reading). Each framing yields a materially different ε and classification. What guided the choice of the composite framing here, and what would change if a narrower framing were adopted?',
    'This is inherent framing underdetermination, not resolvable by further data alone — it depends on which level of causal granularity the analyst treats as the relevant unit of explanation. Comparative historiographical review across economic historians'' treatments could establish which framing has achieved more disciplinary consensus, but would not eliminate the underlying framing choice.',
    'Adopting the punctuated_swap framing would likely yield a much lower ε (a discrete choice is more reversible and less structurally locked-in than an overdetermined convergence) and could shift the classification toward scaffold or even rope (a deliberate, potentially correctable institutional adjustment) rather than tangled_rope. Adopting the triffin_inevitability framing would isolate a single mountain-like mechanism, potentially supporting a mountain claim for the reserve-currency dilemma itself while treating the broader fiat outcome as a downstream tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_omega, conceptual, 'Alternative framings of the same kernel event produce structurally different classifications; this omega documents the framing choice underlying the composite reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1958, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(mone_tr_t1962, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(mone_tr_t1966, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1966, 0.3).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.4).
narrative_ontology:measurement(mone_tr_t1975, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1975, 0.42).
narrative_ontology:measurement(mone_tr_t1980, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1980, 0.42).

% Extraction over time
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement(mone_be_t1962, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1962, 0.38).
narrative_ontology:measurement(mone_be_t1966, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1966, 0.52).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement(mone_be_t1975, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1975, 0.76).
narrative_ontology:measurement(mone_be_t1980, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1980, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(mone_su_t1962, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1962, 0.32).
narrative_ontology:measurement(mone_su_t1966, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1966, 0.45).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(mone_su_t1975, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(mone_su_t1980, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1980, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the monetary_anchor_principle kernel. overdetermined_composite_reading (this story) claims high ε and tangled_rope classification grounded in irreversible multi-causal convergence. punctuated_swap_reading claims a discrete institutional choice with correspondingly different reversibility properties. triffin_inevitability_reading isolates a single structural mechanism (reserve-currency dilemma) as sufficient cause. All three share the same historical event (August 1971 gold-convertibility suspension) but author different ε values and different beneficiary/victim structures because they model different claims about WHY and HOW INEVITABLY the transition occurred. Per the ε-invariance principle, these are three distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
