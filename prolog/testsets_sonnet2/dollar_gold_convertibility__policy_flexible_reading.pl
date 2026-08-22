% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Policy-Subordinate Conditional Obligation
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This story instantiates the policy-flexible reading of the dollar-gold
 *   convertibility kernel: the Article IV commitment to redeem dollars for
 *   gold at $35/oz is treated by U.S. policymakers, in this reading, as
 *   conditional on continued domestic economic stability rather than as an
 *   unconditional peg. Under this reading the constraint's coordination
 *   function (a credible nominal anchor for the Bretton Woods system)
 *   survives, but it is layered with an asymmetric extraction: U.S.
 *   policymakers retain full domestic monetary autonomy while foreign reserve
 *   holders bear the devaluation risk created by that same autonomy,
 *   discovered only when the U.S. invokes domestic conditions to defer or
 *   restrict redemption. This is a different constraint from the
 *   strict_convertibility_reading (where the same historical episode is read
 *   as a binding legal obligation the U.S. is violating) and from the
 *   triffin_structural_reading (where the same episode is read as evidence of
 *   an inherently unsustainable systemic design, prior to any question of who
 *   is honoring what). The ε values differ because the referent differs: here
 *   ε tracks how much the conditional-obligation framing itself extracts from
 *   external creditors, not how much the underlying par-value system as a
 *   whole is unsustainable, nor how much a violated legal commitment costs
 *   its counterparties.
 *
 * KEY AGENTS:
 *   - us_treasury: agenda_setter (institutional/arbitrage) — administers and reinterprets the convertibility obligation
 *   - us_domestic_policymakers: beneficiary (institutional/mobile) — gains monetary autonomy
 *   - foreign_dollar_reserve_holders: payer (moderate/constrained) — bears devaluation risk
 *   - foreign_central_banks: payer/beneficiary (organized/constrained) — dual position, attempts gold runs
 *   - bretton_woods_partner_states: payer (moderate/constrained) — bound by a promise revocable at U.S. discretion
 *   - imf_bretton_woods_secretariat: observer (institutional/analytical) — administers rules, cannot enforce against issuer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.58).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.42).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Policy-Subordinate Conditional Obligation").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'b714b4eb-0e83-4657-9fb0-77fa0a6a41e9').
narrative_ontology:cs_kernel_codification('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', fixed_text).
narrative_ontology:cs_authority_grounding('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', extraction).
narrative_ontology:cs_interpretation_layer_present('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9').
narrative_ontology:cs_reading_relation('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', foundational, domestic_stability_supersedes_external_commitment).
narrative_ontology:cs_axiom_status(domestic_stability_supersedes_external_commitment, holdable).
narrative_ontology:cs_axiom_grounding('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', domestic_stability_supersedes_external_commitment, conventional).
narrative_ontology:cs_axiom('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', secondary, reserve_currency_issuer_retains_interpretive_discretion).
narrative_ontology:cs_axiom_status(reserve_currency_issuer_retains_interpretive_discretion, holdable).
narrative_ontology:cs_axiom_grounding('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', reserve_currency_issuer_retains_interpretive_discretion, instrumental).
narrative_ontology:cs_reference_frame('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', bretton_woods_founding_bargain_1944).
narrative_ontology:cs_drift_state('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', pre_nixon_shock_1971, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b714b4eb-0e83-4657-9fb0-77fa0a6a41e9', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_treasury).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_policymakers).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_export_sector).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_partner_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, domestic_stabilization_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the gold window, setting the terms and tempo of redemption requests and invoking domestic priorities (employment, growth, balance of payments management) to defer, restrict, or ultimately suspend convertibility. Treats the Article IV commitment as conditional on continued domestic stability rather than as an unconditional peg, and retains full discretion over when the condition is deemed unmet.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).

% Pursue expansionary fiscal and monetary policy (Vietnam spending, Great Society programs, employment targets) without being bound by external gold-reserve discipline, because convertibility is read as subordinate to these domestic goals. Gain full use of monetary policy as a domestic tool, insulated from the classical gold-standard constraint.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_policymakers, beneficiary,
    institutional, biographical, mobile, national).

% Benefits indirectly from a dollar whose value is not defended by contractionary domestic policy, since a de facto weaker or unconstrained dollar improves export competitiveness. Has no exposure to the redemption risk that falls on foreign holders.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_export_sector, beneficiary,
    organized, biographical, mobile, global).

% Hold dollar-denominated reserves accumulated through trade surpluses and Cold War dollar recycling, on the understanding the dollar is 'as good as gold.' Under the policy-flexible reading, their claim is contingent on U.S. domestic conditions they cannot observe or influence in real time; when convertibility is deferred or suspended, the value of their holdings is exposed to devaluation with no recourse.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_dollar_reserve_holders, payer,
    moderate, biographical, constrained, global).

% Manage national reserves largely in dollars because the system was built around dollar-gold parity; they benefit from dollar liquidity and access to deep U.S. capital markets but bear the risk that U.S. domestic priorities will override the convertibility promise, converting their reserve asset into a policy-contingent claim rather than a fixed one. Some (e.g., France under de Gaulle) attempt gold conversion runs precisely because they read the obligation as conditional and act to front-run its withdrawal.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, beneficiary).

% Committed to the Bretton Woods par-value system on the premise that the reserve currency issuer bears a corresponding discipline; under this reading, they discover the discipline is unilaterally revocable whenever it conflicts with U.S. domestic stability, leaving their own currencies and trade balances exposed to U.S. policy choices they did not consent to and cannot veto.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_partner_states, payer,
    moderate, generational, constrained, global).

% Administers the formal par-value system and Article IV rules, mediates consultations when convertibility is strained, and produces the record against which the conditional-obligation reading can be assessed. Has no independent enforcement power over the United States.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, imf_bretton_woods_secretariat, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, us_domestic_policymakers).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a nominal anchor (dollars redeemable in gold at $35/oz) that lets Bretton Woods parties price trade, hold reserves, and avoid competitive devaluation cycles — while allowing the reserve-currency issuer room to run domestic stabilization policy without being forced into gold-standard-style contraction every time reserves are drawn down.
% TRANSFER_FUNCTION: Moves monetary-policy flexibility and seigniorage benefit to the United States, and moves devaluation/repricing risk to foreign holders of dollar reserves and to Bretton Woods partner states whose own currencies and trade positions are pegged to a promise the issuer can subordinate to domestic conditions.
% ABSENT_VOICES: Foreign reserve holders and smaller Bretton Woods members had no vote over when 'domestic stability' would be invoked to justify non-convertibility; the conditionality was asserted unilaterally by U.S. policymakers and was never negotiated as an explicit escape clause with reciprocal compensation for those bearing the risk.
% DISAPPEARANCE_RATIONALE: If the conditional-obligation reading were rejected and convertibility treated as strictly binding regardless of domestic conditions, the U.S. would have faced earlier and harder constraints on deficit spending and monetary expansion, foreign holders would have redeemed more freely and confidently, and the eventual break (Nixon Shock, 1971) would likely have occurred earlier, in a different form, or been preempted by earlier domestic austerity.
% FOUNDING_PROBLEM: Bretton Woods needed a credible nominal anchor to prevent the competitive devaluations and currency chaos of the interwar period, while giving the reserve-currency issuer enough policy room that it would not need to impose gold-standard-style domestic deflation whenever external balances tightened.
% FOUNDING_PROBLEM_CORROBORATION: U.S. Treasury officials and domestic policymakers of the era (and later monetarist historians) attest that domestic stabilization needs were real and legitimate. Foreign central banks (notably France's public position under de Gaulle) and independent international economic historians attest from outside the U.S. policymaking apparatus that the conditionality was read into the obligation ex post to justify policy choices already made, rather than negotiated ex ante — the corroborating record for 'genuinely conditional obligation' comes overwhelmingly from the beneficiary side.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily across the interval (0.32 to 0.58) tracking the growing gap between U.S. domestic policy expansion (Vietnam, Great Society) and the gold-backing ratio, as the conditional-obligation reading absorbs increasing strain without collapsing outright until 1971. Suppression is moderate (0.42) — there is no coercive apparatus preventing foreign holders from attempting redemption (France did), but there is real structural pressure (diplomatic, alliance-cohesion, market-disruption costs) discouraging aggressive conversion runs, which functions as a soft suppression mechanism. Theater ratio rises modestly (0.12 to 0.31) as U.S. officials increasingly performed reassurance about dollar stability (Kennedy and Johnson-era statements defending the gold price) while the underlying condition eroded — a Goodhart-style substitution of rhetorical commitment for the declining real gold-reserve backing.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury/policymaker seat, this reads as legitimate exercise of a genuinely conditional commitment — no different in kind from any sovereign's right to manage domestic stability. From the foreign-holder and partner-state seats, the same conduct reads as unilateral risk-shifting dressed in the language of conditionality that was never actually negotiated as such. The engine computes these as structurally different exposures (payer vs. beneficiary directionality) from the same declared facts; this story does not adjudicate which framing is correct, only that the two seats experience the arrangement asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   US_treasury and us_domestic_policymakers sit near the full-beneficiary end: they set the terms of the conditionality, retain arbitrage-grade exit (they can always redefine what counts as 'domestic stability'), and capture the seigniorage/policy-autonomy benefit. Foreign_dollar_reserve_holders and bretton_woods_partner_states sit toward the full-target end: their exit options are constrained (mass redemption risks destabilizing the very system their reserves depend on, and abandoning dollar reserves has high transition costs), and the risk of devaluation is realized specifically when U.S. domestic conditions diverge from external balance — i.e., precisely when the conditional-obligation reading is invoked against them. Foreign_central_banks carry a dual role because they both depend on dollar liquidity (benefit) and bear the conversion risk (payer) — hence the secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a credible nominal anchor without gold-standard-style domestic deflation) was genuinely live in 1944-58; by the mid-1960s, under this reading, the domestic-stability escape valve had become the dominant mode of operation rather than an emergency exception, while the coordination function (credible anchor) was being maintained mostly rhetorically (rising theater_ratio). Classifying this as tangled_rope rather than snare prevents mislabeling a system that DID solve a genuine coordination problem (avoiding 1930s-style devaluation wars) purely as extraction; classifying it as tangled_rope rather than rope prevents ignoring that the coordination benefit and the extraction were riding the same structure and could not be separated by the disadvantaged parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_negotiated_or_asserted,
    'Was the subordination of convertibility to domestic stability a term genuinely understood and accepted by Bretton Woods co-signatories at founding, or was it asserted unilaterally by the U.S. after the fact to rationalize policy choices already made?',
    'Archival review of Bretton Woods negotiation records, IMF Article IV drafting history, and contemporaneous statements by non-U.S. delegations (especially UK and French positions) regarding the scope of the redemption commitment.',
    'If genuinely negotiated, this reading''s coordination claim is stronger and the extraction reads as a foreseen and accepted cost of the bargain. If unilaterally asserted ex post, the conditional-obligation framing itself is closer to a retroactive cover story, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_negotiated_or_asserted, empirical, 'Whether domestic-stability conditionality was an original bargain term or a later reinterpretation.').

omega_variable(
    which_reading_is_the_real_kernel,
    'Among the policy_flexible, strict_convertibility, and triffin_structural readings, is any one of them the ''true'' account of the same historical arrangement, or do they describe genuinely different structural claims that happened to share a text (Article IV) and an event (the Nixon Shock)?',
    'This is the committer-structure question the ε-invariance principle routes to omega rather than resolving inside any single story: compare the beneficiary/victim sets and ε values across all three sibling constraints (linked via network.affects_constraints) to see whether they converge or remain genuinely distinct claims.',
    'If the three readings converge on similar ε and party structure, they may be collapsible into one constraint with observer-relative framing (which the ε-invariance principle forbids without decomposition). If they remain distinct, each should persist as a separate linked story, as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_the_real_kernel, conceptual, 'Whether the three kernel readings are genuinely distinct constraints or one constraint viewed three ways.').

omega_variable(
    gold_run_as_rational_response,
    'Were foreign central banks'' occasional gold-conversion runs (e.g., France 1965-67) a rational hedge against the conditional-obligation risk this reading identifies, or an independent geopolitical action (de Gaulle''s broader anti-dollar-hegemony policy) that would have occurred regardless of the conditionality question?',
    'Comparative analysis of central bank reserve-management behavior across countries with and without independent geopolitical motives to challenge dollar hegemony.',
    'If primarily a rational hedge, it corroborates the payer-side reading that conditionality created real, perceived risk. If primarily geopolitical, the victim-side framing is weaker evidence and more of the extraction is nominal rather than actively contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gold_run_as_rational_response, empirical, 'Whether gold-run behavior corroborates the devaluation-risk claim or reflects separate geopolitical motives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1961, 0.16).
narrative_ontology:measurement(doll_tr_t1964, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1964, 0.2).
narrative_ontology:measurement(doll_tr_t1967, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(doll_tr_t1969, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1969, 0.29).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.31).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1958, 0.32).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1961, 0.4).
narrative_ontology:measurement(doll_be_t1964, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1964, 0.47).
narrative_ontology:measurement(doll_be_t1967, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1967, 0.53).
narrative_ontology:measurement(doll_be_t1969, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1969, 0.56).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1958, 0.22).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1961, 0.28).
narrative_ontology:measurement(doll_su_t1964, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1964, 0.33).
narrative_ontology:measurement(doll_su_t1967, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1967, 0.37).
narrative_ontology:measurement(doll_su_t1969, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1969, 0.4).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dollar_gold_convertibility kernel. strict_convertibility_reading treats the same historical arrangement as a binding legal obligation being violated (U.S. remains in the victim/violator-adjacent structural position as obligor-in-breach); triffin_structural_reading treats it as a structurally doomed design independent of any party's conduct (no policy-flexible escape valve exists in that reading — the system fails regardless of interpretation). This policy_flexible_reading is distinguished by moving the U.S. OUT of the victim/obligor-in-breach set and INTO the beneficiary set, and moving dollar holders INTO the victim set bearing devaluation risk. All three share the same underlying historical episode (1958-1971, Bretton Woods dollar-gold convertibility) but author different ε, different beneficiary/victim sets, and different claimed_type, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
