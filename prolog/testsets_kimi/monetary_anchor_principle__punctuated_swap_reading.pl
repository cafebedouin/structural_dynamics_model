% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Nixon Shock Punctuated Swap Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint is the punctuated_swap_reading of the
 *   monetary_anchor_principle kernel. It treats the August 15, 1971 Nixon
 *   Shock not as structurally inevitable (contra the
 *   triffin_inevitability_reading) or as an overdetermined composite (contra
 *   the overdetermined_composite_reading), but as a discrete, reversible
 *   institutional choice to swap from gold convertibility to a fiat dollar
 *   standard. The U.S. fiscal authority is the primary beneficiary of
 *   restored policy autonomy; foreign dollar holders are the victims of
 *   expropriation through devaluation and subsequent inflation tax. The
 *   reading is contested: sibling readings attribute the transition to
 *   structural necessity, while this reading insists on sovereign agency and
 *   the contingency of the regime.
 *
 * KEY AGENTS:
 *   - us_fiscal_authority: Primary agenda-setter and beneficiary (institutional/arbitrage) â controls the anchor and captures fiscal autonomy
 *   - foreign_dollar_holders: Primary payer and victim (organized/trapped) â bear expropriation through non-redeemable reserves
 *   - international_monetary_institutions: Analytical observer (institutional/analytical) â monitors but cannot alter the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.58).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.45).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Nixon Shock Punctuated Swap Reading").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '890af63e-62f1-4a1a-ac77-cb7c0b7990fa').
narrative_ontology:cs_kernel_codification('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', formalized).
narrative_ontology:cs_authority_grounding('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', self_enforcing).
narrative_ontology:cs_reading_relation('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', monetary_anchor_principle__overdetermined_composite_reading, forecloses).
narrative_ontology:cs_reading_relation('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', monetary_anchor_principle__triffin_inevitability_reading, forecloses).
narrative_ontology:cs_axiom('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', foundational, monetary_regime_as_sovereign_discretion).
narrative_ontology:cs_axiom_status(monetary_regime_as_sovereign_discretion, holdable).
narrative_ontology:cs_axiom_grounding('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', monetary_regime_as_sovereign_discretion, conventional).
narrative_ontology:cs_reference_frame('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', discrete_swap_framework).
narrative_ontology:cs_drift_state('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', contemporary_fiat_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('890af63e-62f1-4a1a-ac77-cb7c0b7990fa', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the suspension of dollar-gold convertibility on August 15, 1971, shifting the monetary anchor from gold to fiat. Gains fiscal autonomy to run deficits, monetize debt, and supply global liquidity without gold reserve constraints. Controls the monetary anchor through Federal Reserve and Treasury policy.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold dollar reserves accumulated under Bretton Woods gold-convertibility promises. Lost the right to redeem dollars for gold at $35/ounce in 1971, bearing expropriation through dollar devaluation and inflation tax. Diversification into other assets is limited by market depth and the dollar's reserve currency role.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    organized, biographical, trapped, global).

% Monitor and report on the international monetary system, providing surveillance and analysis of exchange rate arrangements. Adapted their frameworks after 1971 but do not control the anchor; their recommendations on reserve diversification have limited effect on the constraint.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global liquidity provision by allowing the reserve currency issuer to adjust money supply and fiscal policy without hard gold reserve constraints, solving the Bretton Woods liquidity shortage.
% TRANSFER_FUNCTION: Transfers purchasing power from foreign dollar holders to the U.S. fiscal authority via dollar devaluation, seigniorage, and the inflation tax, in exchange for continued provision of the global reserve asset.
% ABSENT_VOICES: Foreign dollar holders were not consulted in the August 1971 decision; gold-standard advocates and domestic constituencies preferring monetary discipline were excluded from the closed-door Camp David deliberations.
% DISAPPEARANCE_RATIONALE: If the fiat dollar anchor disappeared overnight and convertibility were restored, global reserve portfolios would reallocate, U.S. fiscal policy would face immediate hard constraints, and the international monetary order would revert to a scarce-reserve regime.
% FOUNDING_PROBLEM: The Bretton Woods gold-exchange standard created a coordination failure: the reserve currency issuer could not simultaneously maintain gold convertibility, supply global liquidity, and pursue autonomous domestic fiscal policy.
% FOUNDING_PROBLEM_CORROBORATION: International monetary historians, IMF working papers, and foreign central bank officials outside the U.S. beneficiary seat attest that the liquidity-autonomy tension was the central structural problem of Bretton Woods; the problem persists today as the live coordination challenge of the fiat standard.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because the fiat standard genuinely coordinates global liquidity provision while simultaneously transferring purchasing power to the reserve issuer. Suppression is moderate (0.45): the initial closure of the gold window was unilateral, but ongoing maintenance relies more on network effects and path dependence than on active coercion. Theater ratio is elevated at the interval start (0.55) because the suspension was publicly framed as temporary, then normalized; it declines over time as the regime becomes accepted. Accessibility collapse is high (0.72) because reconstructing a gold anchor or credible alternative reserve system has become institutionally prohibitive after five decades of fiat practice. Resistance is moderate (0.42): episodic de-dollarization efforts and diplomatic objections persist but have not dislodged the constraint. The claim/metric gap is deliberate: claimed as rope while metrics acknowledge real extraction and trapped victims â the engine measures that divergence.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. fiscal authority experiences the constraint as a legitimate coordination mechanism restoring necessary policy autonomy and solving the Bretton Woods liquidity trap. Foreign dollar holders experience it as a unilateral expropriation of their reserve value and a violation of the original Bretton Woods bargain. The engine computes this divergence from the structural data: the agenda-setter has arbitrage-grade exit (it controls the rule) while the payer is trapped in dollar reserves by market depth and the absence of a ready substitute.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. fiscal authority is the structural beneficiary and agenda-setter (d near 0.0): it subsidizes their fiscal capacity and eliminates an external constraint. Foreign dollar holders are the structural targets (d near 1.0): they bear the extraction through accumulated non-redeemable reserves and inflation tax. International monetary institutions sit at analytical distance with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the Bretton Woods liquidity-autonomy trap â was acute in 1971. The fiat standard continues to solve the live coordination problem of global liquidity provision, so the arrangement is not yet mandatrophic. However, the theater of temporary suspension becoming a permanent fifty-year regime creates a secondary piton risk that the prompt does not resolve; the metrics track this through declining theater_ratio but persistent extractiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_of_punctuated_swap,
    'Is the August 1971 regime swap reversible, or has path dependence locked the system into fiat permanence?',
    'Analysis of historical return options to commodity anchoring and measurement of transition costs, including political economy barriers to re-establishing a gold or SDR-based anchor.',
    'If irreversible, the discrete choice framing is historically bounded and the constraint approaches mountain or piton status; if reversible, rope classification is structurally supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_punctuated_swap, empirical, 'Whether the swap remains a reversible institutional choice').

omega_variable(
    coordination_extraction_boundary,
    'Does the fiat dollar standard primarily coordinate global liquidity, or does it systematically expropriate foreign holders via seigniorage and inflation tax?',
    'Long-term analysis of reserve asset returns versus inflation, and assessment of viable alternative reserve arrangements including bilateral currency swaps and CBDCs.',
    'If expropriation dominates, the constraint is tangled_rope or snare rather than rope; if coordination dominates, rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Boundary between liquidity coordination and extraction from reserve holders').

omega_variable(
    unilateral_choice_vs_structural_inevitability,
    'Was the 1971 transition a genuine discrete choice, or was it structurally overdetermined by the Triffin dilemma and other pressures?',
    'Historical analysis of policy alternatives available to U.S. decision-makers in 1968-1971, including fiscal consolidation and exchange-rate adjustment paths not taken.',
    'Resolves whether this reading or its sibling readings capture the structural truth of the transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_choice_vs_structural_inevitability, empirical, 'Contingency versus structural inevitability of the regime transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(mone_tr_t30, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(mone_tr_t40, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(mone_tr_t50, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(mone_be_t30, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(mone_be_t40, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(mone_be_t50, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 50, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__punctuated_swap_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one member of the monetary_anchor_principle constraint family. It shares the kernel with overdetermined_composite_reading and triffin_inevitability_reading, but each reading has a distinct epsilon, beneficiary/victim structure, and causal narrative. The punctuated_swap_reading is not downstream of the others; it is a competing interpretation of the same transition event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
