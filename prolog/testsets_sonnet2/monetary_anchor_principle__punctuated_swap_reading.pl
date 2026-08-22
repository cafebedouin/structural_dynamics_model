% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Bretton Woods Gold-Dollar Convertibility as a Discrete Regime Swap (Nixon Shock, Aug 15 1971)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the punctuated-swap reading of the
 *   monetary_anchor_principle kernel: the claim that the end of dollar-gold
 *   convertibility was, structurally, a discrete institutional choice made
 *   over a single weekend by a small executive group, rather than an
 *   inevitable outcome forced by accumulated structural pressure. On this
 *   reading, Bretton Woods convertibility functioned as genuine multilateral
 *   coordination (a rope) up until the moment of unilateral U.S. defection,
 *   at which point the same structure that had coordinated exchange rates
 *   became the vehicle for a one-sided transfer: continued U.S. fiscal
 *   flexibility purchased at the expense of foreign dollar holders whose
 *   reserves were devalued without renegotiation. Sibling readings
 *   (overdetermined_composite_reading, triffin_inevitability_reading) treat
 *   the same 1971 event as the terminal expression of prior structural forces
 *   rather than as a contingent choice; those are separate constraint stories
 *   in this family, not alternative measurements of this one. This reading's
 *   ε is authored as moderate because the choice, while consequential, was in
 *   principle reversible or renegotiable at the time it was made — the moment
 *   of defection had not yet foreclosed all coordinated alternatives (e.g., a
 *   negotiated multilateral devaluation was in fact attempted four months
 *   later at the Smithsonian).
 *
 * KEY AGENTS:
 *   - us_fiscal_and_monetary_authorities: primary beneficiary and agenda-setter (institutional/arbitrage) — unilaterally suspends convertibility, retains fiscal flexibility
 *   - foreign_central_banks_holding_dollar_reserves: primary target (institutional/constrained) — bears devaluation of dollar reserves without consent
 *   - us_trading_partners_general: secondary actor (organized/constrained) — absorbs adjustment costs of abrupt realignment
 *   - international_monetary_fund: excluded designated coordinator — bypassed rather than consulted
 *   - economic_historians: analytical observer — sees the full contested-kernel structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.48).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.42).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Bretton Woods Gold-Dollar Convertibility as a Discrete Regime Swap (Nixon Shock, Aug 15 1971)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '2cf82644-19a3-4d36-8c7f-35bdff06f25b').
narrative_ontology:cs_kernel_codification('2cf82644-19a3-4d36-8c7f-35bdff06f25b', distributed).
narrative_ontology:cs_authority_grounding('2cf82644-19a3-4d36-8c7f-35bdff06f25b', distributed).
narrative_ontology:cs_reading_relation('2cf82644-19a3-4d36-8c7f-35bdff06f25b', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('2cf82644-19a3-4d36-8c7f-35bdff06f25b', monetary_anchor_principle__triffin_inevitability_reading, forecloses).
narrative_ontology:cs_axiom('2cf82644-19a3-4d36-8c7f-35bdff06f25b', foundational, the_swap_was_a_contingent_executive_decision).
narrative_ontology:cs_axiom_status(the_swap_was_a_contingent_executive_decision, holdable).
narrative_ontology:cs_axiom_grounding('2cf82644-19a3-4d36-8c7f-35bdff06f25b', the_swap_was_a_contingent_executive_decision, empirically_contingent).
narrative_ontology:cs_axiom('2cf82644-19a3-4d36-8c7f-35bdff06f25b', secondary, coordinated_renegotiation_remained_available_at_the_moment_of_defection).
narrative_ontology:cs_axiom_status(coordinated_renegotiation_remained_available_at_the_moment_of_defection, holdable).
narrative_ontology:cs_axiom_grounding('2cf82644-19a3-4d36-8c7f-35bdff06f25b', coordinated_renegotiation_remained_available_at_the_moment_of_defection, empirically_contingent).
narrative_ontology:cs_reference_frame('2cf82644-19a3-4d36-8c7f-35bdff06f25b', bretton_woods_fixed_convertibility_norm).
narrative_ontology:cs_drift_state('2cf82644-19a3-4d36-8c7f-35bdff06f25b', smithsonian_agreement_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('2cf82644-19a3-4d36-8c7f-35bdff06f25b', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_and_monetary_authorities).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks_holding_dollar_reserves).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_trading_partners_general).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, domestic_us_consumers_and_workers).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, us_trading_partners_general).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, domestic_us_consumers_and_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nixon administration and Treasury unilaterally suspended gold convertibility on a single announced date, ending the obligation to redeem foreign-held dollars for gold at $35/oz. This freed U.S. fiscal and monetary policy from the external gold-reserve constraint, permitting continued deficit spending (Vietnam, Great Society) without the discipline the convertibility peg had imposed. The decision was made by a small executive circle over a weekend at Camp David and announced as a fait accompli.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_and_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_and_monetary_authorities, beneficiary).

% Held large dollar reserves accumulated under the Bretton Woods arrangement on the understanding they were convertible to gold at a fixed rate. The suspension, followed by devaluation and float, reduced the real value of those reserves without their consent or renegotiation. Their options were to accept the loss, attempt coordinated diplomatic pressure (Smithsonian Agreement, 1971), or exit the dollar-reserve system entirely — the last option carrying its own severe transition costs given the dollar's entrenched role in trade invoicing and settlement.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks_holding_dollar_reserves, payer,
    institutional, generational, constrained, global).

% Exporters and governments in Europe and Japan faced sudden currency realignment risk and had to renegotiate exchange-rate arrangements on short notice. Some benefited from subsequent dollar depreciation improving their competitiveness in certain sectors, but the abruptness of the swap imposed adjustment costs that a negotiated transition would have spread out or compensated.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_trading_partners_general, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_trading_partners_general, beneficiary).

% Benefited indirectly from continued deficit-financed spending and policy flexibility freed from the gold constraint, but also bore the costs of the 1970s inflation that followed the end of the nominal anchor, without having any voice in the Camp David decision.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, domestic_us_consumers_and_workers, beneficiary,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, domestic_us_consumers_and_workers, payer).

% The multilateral institution formally charged with overseeing the Bretton Woods par-value system was not consulted before the announcement and was left to manage the aftermath (Smithsonian Agreement, subsequent move to floating rates) rather than the transition itself. Its role as designated coordinator was bypassed by the unilateral act.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_fund, excluded,
    institutional, generational, analytical, global).

% Debate whether the event is best read as a discrete policy choice, an overdetermined structural collapse, or a Triffin-inevitable outcome; this story takes the discrete-choice reading and treats the alternative readings as separate constraint stories in the same family.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_and_monetary_authorities).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods convertibility solved a genuine multilateral coordination problem: it gave trading nations a stable nominal anchor and predictable exchange rates, reducing transaction-cost friction and currency-war risk in postwar reconstruction trade.
% TRANSFER_FUNCTION: The August 15, 1971 suspension moved real value from foreign holders of accumulated dollar reserves (whose claims were no longer redeemable at the promised gold rate, and were subsequently devalued) to the U.S. government and domestic economy (which retained fiscal and monetary flexibility it would otherwise have had to sacrifice to defend the peg).
% ABSENT_VOICES: Foreign central banks and the IMF, the formally designated overseer of the par-value system, were not party to the Camp David decision; they learned of the suspension via public announcement, not negotiation. Had they been in the room, they would likely have pressed for a coordinated, phased devaluation or burden-sharing arrangement rather than a unilateral suspension.
% DISAPPEARANCE_RATIONALE: Had the announced suspension not occurred on that date, foreign dollar reserves would have retained their promised convertibility (or the U.S. would have had to negotiate a multilateral adjustment), the timeline and terms of the eventual move to floating rates would differ, and the specific victims of the abrupt devaluation would not have borne that particular loss at that particular time — the discrete-choice framing implies the outcome was contingent on this specific decision, not merely the working-out of prior structural forces.
% FOUNDING_PROBLEM: The Bretton Woods gold-dollar peg was built to give the postwar international trade system a stable, credible nominal anchor after the competitive devaluations and monetary chaos of the interwar period.
% FOUNDING_PROBLEM_CORROBORATION: U.S. Treasury officials of the era (Volcker, Connally) attested that the peg's discipline had become incompatible with domestic fiscal priorities and needed to end; foreign central bankers and IMF staff at the time, and subsequent independent economic historians outside the U.S. policymaking apparatus, attest that the underlying coordination problem (need for a credible reserve anchor) remained live and was not resolved but simply abandoned unilaterally without a replacement mechanism being negotiated in advance.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises sharply around 1971 (0.34 -> 0.48) reflecting the moment of unilateral defection itself, then partially recedes (0.45, 0.40) as the Smithsonian Agreement and subsequent float renegotiated terms multilaterally — consistent with the punctuated-swap reading's claim that the act was a discrete, potentially reversible choice rather than a one-way structural collapse. Theater ratio tracks modestly with the political staging of the announcement (Nixon's television address, the 'temporary' framing of the import surcharge) and recedes once the arrangement stabilizes into the floating-rate era. Suppression (0.42) is moderate: no active coercive enforcement mechanism was required to sustain the new arrangement — no state needed to be physically prevented from re-pegging — but foreign holders had no meaningful recourse once the announcement was made, which is real suppression of alternatives even without enforcement machinery.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. fiscal and monetary authorities sit near the beneficiary end: they made the decision, retained maximal exit and arbitrage options (issuing the reserve currency, unilateral policy control), and captured the flexibility gain. Foreign central banks sit near the target end: institutional power but constrained exit — dumping dollar reserves en masse would have been self-destructive given trade-settlement dependence, so their structural position combined high nominal power with low effective exit, which is exactly the profile the derivation chain treats as high effective extraction despite formal institutional status. This is the seat divergence the punctuated-swap reading foregrounds: the same event computes as rope-like coordination-failure-and-repair from the U.S. seat and as a one-sided imposed cost from the foreign-holder seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The punctuated-swap reading resists two mislabeling failures. First, it does not let the coordination story (Bretton Woods solved a real postwar problem) launder the 1971 act into pure natural necessity — the claimed type is rope, not mountain, because the coordination function was real but the specific act of ending it was a choice with an identifiable beneficiary and victim, not a physical law. Second, it does not collapse into snare, because the constraint did not require ongoing active enforcement to persist — no coercive machinery was needed to prevent renegotiation, and renegotiation in fact happened (Smithsonian, later Jamaica Accords). The moderate ε and absence of `requires_active_enforcement` are the structural markers that keep this story rope-shaped rather than tangled_rope or snare, even though a real, identifiable transfer occurred at the moment of the swap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discrete_choice_vs_structural_inevitability,
    'Was the August 15, 1971 suspension a genuinely contingent policy choice that could have gone otherwise, or was it the point at which accumulated structural pressures (Triffin dilemma, deficit financing, capital mobility) made abandonment unavoidable regardless of which administration was in office?',
    'Counterfactual/comparative analysis: examine whether alternative administrations facing the same reserve position and capital-mobility environment in the same period took materially different paths, and whether contemporaneous internal Treasury/Fed documents show the decision as urgent-but-optional or as foreclosed by prior commitments.',
    'If resolved toward structural inevitability, this reading''s claimed rope classification and moderate ε would not hold for the event itself — the event would belong to the triffin_inevitability_reading''s mountain-leaning framing instead, and the beneficiary/victim structure would need to be recharacterized as a byproduct of structural collapse rather than a chosen transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discrete_choice_vs_structural_inevitability, conceptual, 'Whether the 1971 event is best modeled as a discrete choice (this reading) or a structurally forced outcome (sibling readings).').

omega_variable(
    reversibility_of_the_swap,
    'Was the suspension of convertibility actually reversible at the time it was announced, or had prior gold-reserve depletion already made any return to fixed convertibility infeasible regardless of political will?',
    'Historical reserve-ratio analysis: compare U.S. gold stock to outstanding foreign dollar claims at the moment of suspension against the ratio that would have been needed to sustain convertibility through renegotiated terms.',
    'If reserves were already so depleted that no renegotiated peg was feasible, the ε value authored here as ''moderate, reversible in principle'' overstates the degree of genuine choice available and would need revision toward the Triffin-inevitability reading''s higher-necessity framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_the_swap, empirical, 'Whether the reserve position at the time of the swap left any real reversibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1958, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1958, 0.05).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(mone_tr_t1969, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1969, 0.12).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement(mone_tr_t1973, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1973, 0.15).
narrative_ontology:measurement(mone_tr_t1976, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1976, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1958, 0.12).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1965, 0.22).
narrative_ontology:measurement(mone_be_t1969, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1969, 0.34).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.48).
narrative_ontology:measurement(mone_be_t1973, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1973, 0.45).
narrative_ontology:measurement(mone_be_t1976, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1976, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__punctuated_swap_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the monetary_anchor_principle kernel. overdetermined_composite_reading treats the 1971 collapse as jointly caused by multiple independent structural pressures; triffin_inevitability_reading treats it as forced specifically by the reserve-currency issuer's structural dilemma; this story (punctuated_swap_reading) treats it as a discrete, contingent executive decision. All three share the same historical event but author different ε, different claimed types, and in the Triffin case a different beneficiary/victim emphasis (systemic dilemma vs. specific unilateral actor). Per the ε-invariance principle, these are three distinct constraints, not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
