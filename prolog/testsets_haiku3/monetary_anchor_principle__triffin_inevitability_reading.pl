% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Triffin Dilemma: Mathematical Impossibility of Dual Gold-Standard Reserve Role
 *   domain: monetary_economics/international_finance
 *
 * SUMMARY:
 *   The Triffin dilemma is a mathematical contradiction embedded in the
 *   Bretton Woods monetary system: a reserve currency issuer (the United
 *   States) that is bound to gold must simultaneously satisfy two
 *   incompatible demands: (1) maintain a fixed gold price and exchange rate
 *   (credibility of the peg) and (2) supply enough U.S. dollar reserves to
 *   the world economy to meet growing international liquidity needs (the
 *   function of a reserve currency). Meeting demand (2) requires running
 *   deficits and issuing dollars; these deficits erode the gold backing and
 *   eventually trigger runs on the gold window, violating demand (1). The
 *   dilemma is framed here as a mathematical impossibility: no policy choice
 *   could sustain both requirements indefinitely. This reading instantiates
 *   the Triffin inevitability thesis—that the 1971 suspension was not a
 *   choice but a structural inevitability. The constraint story treats the
 *   logical structure of the dilemma as the standing arrangement under
 *   contest (the reference frame), not the political decision to abandon gold
 *   in 1971.
 *
 * KEY AGENTS:
 *   - United States (as gold-standard reserve-currency issuer): holds the structural contradiction in its dual role
 *   - Global economic system (as liquidity consumer): demands dollars for trade and reserves
 *   - Bretton Woods institutional framework (as the victim): designed to reconcile the incompatible demands but logically cannot
 *   - Deficit nations and gold-holding countries (as structural pressure points): felt the dilemma acutely as their reserves eroded or their trade flows tightened
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
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma: Mathematical Impossibility of Dual Gold-Standard Reserve Role").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '4b992c3e-719d-4566-82a0-93df1feb620f').
narrative_ontology:cs_kernel_codification('4b992c3e-719d-4566-82a0-93df1feb620f', distributed).
narrative_ontology:cs_authority_grounding('4b992c3e-719d-4566-82a0-93df1feb620f', expertise).
narrative_ontology:cs_interpretation_layer_present('4b992c3e-719d-4566-82a0-93df1feb620f').
narrative_ontology:cs_reading_relation('4b992c3e-719d-4566-82a0-93df1feb620f', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b992c3e-719d-4566-82a0-93df1feb620f', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('4b992c3e-719d-4566-82a0-93df1feb620f', foundational, reserve_currency_gold_standard_incompatible).
narrative_ontology:cs_axiom_status(reserve_currency_gold_standard_incompatible, holdable).
narrative_ontology:cs_axiom_grounding('4b992c3e-719d-4566-82a0-93df1feb620f', reserve_currency_gold_standard_incompatible, empirically_contingent).
narrative_ontology:cs_axiom('4b992c3e-719d-4566-82a0-93df1feb620f', foundational, gold_scarcity_binds_liquidity_supply).
narrative_ontology:cs_axiom_status(gold_scarcity_binds_liquidity_supply, holdable).
narrative_ontology:cs_axiom_grounding('4b992c3e-719d-4566-82a0-93df1feb620f', gold_scarcity_binds_liquidity_supply, empirically_contingent).
narrative_ontology:cs_reference_frame('4b992c3e-719d-4566-82a0-93df1feb620f', bretton_woods_dual_requirement).
narrative_ontology:cs_drift_state('4b992c3e-719d-4566-82a0-93df1feb620f', gold_window_closure_1971, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('4b992c3e-719d-4566-82a0-93df1feb620f', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_gold_standard_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, deficit_nations_and_gold_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional system designed at Bretton Woods to reconcile fixed exchange rates with a functioning global reserve currency. It is the victim of the Triffin dilemma in that its design parameters are logically incompatible. Not an agent (it is an abstract framework), but listed here because the constraint story concerns its structural fate.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_gold_standard_framework, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_gold_standard_framework).

% As the reserve-currency issuer under the gold standard, it faces the dilemma directly: must choose between maintaining the gold peg (which requires restraining deficit spending and dollar issuance) or supplying global liquidity (which requires deficit spending and dollar growth). The dilemma forces the choice between two incompatible objectives; the U.S. chose to abandon the gold peg in 1971.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, united_states_monetary_authority, agenda_setter,
    institutional, generational, mobile, global).

% The aggregate demand for international reserves and liquidity to facilitate global trade and capital flows. This is a structural demand, not a volitional agent, but its growth is what makes the Triffin dilemma acute. As global trade expanded in the 1960s, the demand for dollar reserves grew faster than U.S. gold reserves.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, global_economy_liquidity_demand, observer,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, global_economy_liquidity_demand).

% Nations holding dollar reserves or running trade deficits with the U.S. felt pressure as the value of the dollar declined relative to gold and as their reserves eroded. Their exit options were limited: they could accumulate dollars or gold but could not escape the system without wholesale trade restructuring.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, deficit_nations_and_gold_holders, payer,
    powerful, biographical, constrained, global).

% The IMF and the fixed-exchange-rate framework were designed to prevent the exact problem the Triffin dilemma describes. Their mandate to maintain fixed rates and provide liquidity became impossible to fulfill simultaneously. They were trapped by their own design.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_authority, agenda_setter,
    institutional, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system coordinated global trade and capital flows through a fixed-exchange-rate system anchored to gold, eliminating uncertainty about currency values and enabling long-term international planning.
% TRANSFER_FUNCTION: The arrangement transferred the privilege of seigniorage (the difference between the cost of printing currency and its value) to the U.S., which could issue dollars at will to finance deficits, while other nations had to earn dollars through trade surpluses or gold accumulation.
% ABSENT_VOICES: Gold-producing nations and alternative reserve-currency candidates (sterling-based blocs, nationalist financial systems) were structurally positioned outside the core negotiation. Their interests in a different monetary arrangement were not represented in Bretton Woods design.
% DISAPPEARANCE_RATIONALE: If the Triffin dilemma did not exist (i.e., if gold were infinitely available or if reserve-currency issuance could be unlimited without gold backing), the world would not have needed to abandon the gold standard in 1971. The entire post-Bretton Woods monetary system (floating rates, fiat currencies, SDRs) exists as a rearrangement around the dilemma's reality.
% FOUNDING_PROBLEM: After World War II, the global economy needed a stable reserve currency and fixed exchange rates to facilitate reconstruction and trade, but also needed sufficient liquidity for growing international commerce. Bretton Woods attempted to solve both problems by tying the dollar to gold and making the dollar the reserve currency, assuming that U.S. gold reserves would remain abundant.
% FOUNDING_PROBLEM_CORROBORATION: Triffin's own analysis (1960, 'Gold and the Dollar Crisis') articulated the dilemma clearly before it became acute. Economic historians and monetary economists (Eichengreen, Steil, Stray) have documented the historical progression of gold loss and reserve accumulation that validated Triffin's prediction. The IMF's internal assessments in the late 1960s acknowledged the sustainability crisis. Corroboration comes from outside the Bretton Woods beneficiary set—from economists questioning the system's viability.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is near-zero (0.05) because the constraint is not an extractive arrangement—it is a logical trap. No party 'collects' from the dilemma; the system itself breaks under its own mathematics. Suppression is zero: the mathematical relationship needs no enforcement; it IS the fact. Theater is zero: there is no performance because the constraint is not maintained by policy choice but by the arithmetic of gold flow and deficit spending. Accessibility collapse is high (0.92): once the dilemma is understood, all alternatives collapse—you cannot both peg and supply liquidity at the same time, and you cannot escape this by clever policy if the fundamental parameters are fixed. Resistance is low (0.08): the constraint meets no resistance because it is not a policy choice anyone defends; the resistance comes from trying to escape it, not from maintaining it. The measurement series are flat across time: the logical structure does not change; only the intensity of the contradiction (measured in gold-flow data and reserve ratios) grows, but that is a separate empirical process. This reading treats the Triffin logic as unchanging; the gold drain that forced collapse (the observable consequence) is the historical manifestation, not the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap because the dilemma is not a social arrangement with parties experiencing it differently. The U.S. feels it as pressure to print money or default on gold. Deficit nations feel it as their reserves eroding. The Bretton Woods framework feels it as its design parameters becoming unsustainable. But these are all experiencing the same logical trap from different seats—none can escape it without changing the fundamental parameters (the gold price, the dollar's reserve role, or the liquidity demand). The dilemma treats all seats symmetrically: all are caught.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading does not have directionality in the standard sense (beneficiary/target extraction). The Triffin dilemma is a structural contradiction, not an extraction mechanism. If forced to place directionality, the 'victim' is the Bretton Woods system itself (it is the thing that breaks under the contradiction); there is no beneficiary because no party sustains the constraint as a benefit-extraction mechanism. The constraint persists only as long as the parameters (gold price fixed, dollar as reserve currency, liquidity demand growing) are fixed. When the pressure exceeds gold reserves, the system collapses. This is closest to a mountain in that the system's collapse is a mathematical inevitability given the parameters, but it is a 'mountain' of institutional logic rather than physical law.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply here. The Triffin dilemma has no 'mandate' that outlives its function; the function (explaining the 1971 transition) is the dilemma itself. If the reading is correct (the transition was logically inevitable), then it is not a decayed institutional arrangement; it is a solved mathematical problem (the gold standard did break, as predicted). If the reading is incorrect (the transition was a choice, not an inevitability), then the reading itself is a false summit—a natural-law claim masking a historical contingency—and mandatrophy would apply to the *reading*, not to the constraint it describes. The omega variable 'policy_beneficiary_hidden_in_mountain_claim' addresses this possibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mathematical_necessity_vs_historical_contingency,
    'Is the Triffin dilemma a pure mathematical impossibility (reserve currency issuer + gold standard + global liquidity demand = logical contradiction), or does it describe a contingent historical imbalance that policy choices could have managed differently?',
    'Examination of whether an alternative policy regime (gold pooling, special drawing rights, cooperative credit expansion) could have extended the constraint''s viability without mathematical adjustment. Comparison with other reserve-currency regimes (sterling, dollar-post-Bretton-Woods) to assess whether the dilemma is system-invariant or artifact-specific.',
    'If pure mathematical necessity: classification as mountain is correct; no policy choice could have prevented collapse. If contingent: classification shifts toward tangled_rope or snare—institutional choices (war spending, inflation targeting, capital controls) made the dilemma acute; alternative choices would have avoided it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mathematical_necessity_vs_historical_contingency, conceptual, 'Whether Triffin dilemma is logical impossibility or contingent historical imbalance.').

omega_variable(
    inherent_contradiction_or_delayed_adjustment,
    'Did the dilemma force collapse at a specific date (1971 or earlier), or did it create a pressure that could be deferred indefinitely through institutional adjustment (reserve-currency substitution, gold revaluation, SDR evolution)?',
    'Counterfactual institutional modeling: what if the London Gold Pool had held at $35/oz with cooperative effort? What if the Kennedy Round agreements had included monetary reform coordination? What if gold revaluation had occurred earlier or in a phased manner?',
    'If collapse was forced at a specific date: the dilemma is time-locked and systemic. If adjustment could have been indefinitely deferred: the dilemma describes a persistent pressure, not an inevitability, and the 1971 transition was a choice point where alternatives existed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_contradiction_or_delayed_adjustment, empirical, 'Whether Triffin dilemma forces collapse at a specific time or creates deferrable pressure.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint (the reading instantiated here) a claim about the logical structure of the monetary system (the mathematical dilemma itself), or a claim about historical inevitability (that the dilemma forced the 1971 transition)?',
    'Distinguish between two reference frames: (a) the logical structure of dual gold-standard reserve-currency roles, which is self-contained and time-invariant; (b) the historical claim that this structure caused the Bretton Woods collapse in 1971. The dilemma exists in (a) regardless of when or whether the system actually collapsed. The reading''s referent must be declared.',
    'If referent is (a), classification as mountain is secure and extractiveness remains near-zero. If referent is (b), the reading conflates logical structure with historical causation, and classification depends on whether 1971 was forced or chosen—this omega pins the ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Referent ambiguity: logical structure vs. historical inevitability claim.').

omega_variable(
    policy_beneficiary_hidden_in_mountain_claim,
    'The Triffin dilemma''s mathematical logic benefited certain parties (U.S. monetary sovereignty, inflation-exportable financial system, Bretton Woods institutional authority) while constraining others (deficit nations, gold-holding countries, alternative currency claimants). Does this reading''s claim obscure who benefited from calling the transition ''inevitable'' rather than ''chosen''?',
    'Historical discourse analysis: who invoked the Triffin dilemma as rhetorical cover? Which parties'' interests were served by framing collapse as structurally necessary rather than as a deliberate policy choice? Did the ''inevitability'' framing prevent consideration of alternatives?',
    'If the Triffin framing was weaponized to preclude alternative policies or to legitimize a transition that benefited specific parties, the reading''s mathematical claim becomes a false summit—a mountain invoked strategically to make a constructed constraint appear natural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_beneficiary_hidden_in_mountain_claim, preference, 'Whether invoking Triffin inevitability serves hidden beneficiary interests by naturalizing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(mone_tr_t0, observed).
narrative_ontology:measurement(mone_tr_t5, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 5, 0.0).
narrative_ontology:measurement_basis(mone_tr_t5, observed).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement_basis(mone_tr_t10, observed).
narrative_ontology:measurement(mone_tr_t15, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 15, 0.0).
narrative_ontology:measurement_basis(mone_tr_t15, observed).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement_basis(mone_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(mone_be_t0, observed).
narrative_ontology:measurement(mone_be_t5, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 5, 0.04).
narrative_ontology:measurement_basis(mone_be_t5, observed).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement_basis(mone_be_t10, observed).
narrative_ontology:measurement(mone_be_t15, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement_basis(mone_be_t15, observed).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement_basis(mone_be_t20, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__triffin_inevitability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__triffin_inevitability_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel decomposes into three constraint stories, each claiming a different causal structure for the 1971 transition. The triffin_inevitability_reading (this story) claims the transition was logically inevitable due to mathematical contradiction. The punctuated_swap_reading claims it was a discrete institutional choice. The overdetermined_composite_reading claims multiple historical pressures made collapse inevitable by the late 1960s but not earlier. Each story has a distinct epsilon, distinct beneficiary/victim structure, and distinct claim about the nature of the transition. They are linked via network.affects_constraints to indicate they are alternative explanations of the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
