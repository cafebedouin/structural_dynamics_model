% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Post-Bretton Woods Fiat Dollar Standard (Punctuated Swap Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   On August 15, 1971, the United States suspended gold convertibility,
 *   completing a punctuated swap from the Bretton Woods gold-exchange
 *   standard to a fiat dollar reserve system. Under the
 *   punctuated_swap_reading, this transition is understood as a discrete,
 *   reversible institutional choice rather than a structurally inevitable
 *   collapse. The resulting constraint â the post-1971 monetary anchor
 *   principle â coordinates global liquidity and trade settlement through
 *   U.S. monetary discretion, while simultaneously extracting seigniorage and
 *   inflation-tax purchasing power from foreign dollar holders. The U.S.
 *   fiscal authority acts as agenda-setter and beneficiary; foreign central
 *   banks, sovereign wealth funds, and private holders act as payers bearing
 *   devaluation risk. This reading treats the constraint as a coordination
 *   device (rope), but the authored metrics reflect moderate-to-substantial
 *   extraction and active enforcement, producing a deliberate claim/metric
 *   divergence.
 *
 * KEY AGENTS:
 *   - us_fiscal_authority: Primary beneficiary/agenda-setter (institutional/arbitrage) â sets monetary policy, captures fiscal autonomy and seigniorage.
 *   - foreign_dollar_holders: Primary target (organized/constrained) â bear inflation and devaluation risk with limited exit.
 *   - multilateral_monetary_institutions: Analytical observer (institutional/analytical) â monitor the system without controlling the anchor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.55).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.62).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Post-Bretton Woods Fiat Dollar Standard (Punctuated Swap Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '3b769e0c-4cf5-455c-9d6b-f9c6daf1f604').
narrative_ontology:cs_kernel_codification('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', formalized).
narrative_ontology:cs_authority_grounding('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', lineage).
narrative_ontology:cs_interpretation_layer_present('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604').
narrative_ontology:cs_reading_relation('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', foundational, monetary_sovereignty_as_punctuated_choice).
narrative_ontology:cs_axiom_status(monetary_sovereignty_as_punctuated_choice, holdable).
narrative_ontology:cs_axiom_grounding('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', monetary_sovereignty_as_punctuated_choice, conventional).
narrative_ontology:cs_axiom('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', secondary, regime_reversibility_principle).
narrative_ontology:cs_axiom_status(regime_reversibility_principle, holdable).
narrative_ontology:cs_axiom_grounding('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', regime_reversibility_principle, instrumental).
narrative_ontology:cs_reference_frame('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', discretionary_fiat_anchor).
narrative_ontology:cs_drift_state('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', post_great_financial_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3b769e0c-4cf5-455c-9d6b-f9c6daf1f604', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, fiat_monetary_sovereignty).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, unilateral_adjustment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the issuance of the global reserve currency and sets monetary policy via the Federal Reserve and Treasury. Captures seigniorage, fiscal space, and the ability to run persistent deficits without an immediate balance-of-payments crisis. Can alter the monetary regime unilaterally; exit from the constraint is not meaningful because it defines the system.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold dollar-denominated reserves, sovereign debt, and private assets as the primary store of value and medium for international trade. Bear the risk of purchasing-power erosion through U.S. inflation and episodic devaluation. Exit is constrained by the lack of alternative deep liquid markets and by the network externalities of the dollar standard; coordinated exit risks self-inflicted capital losses.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    organized, generational, constrained, global).

% IMF, BIS, and allied technocratic bodies monitor balance-of-payments and reserve accumulation. They provide analysis and conditional lending but do not control the monetary anchor. Their position is analytical: they observe the constraint's operation without being able to reset the anchor.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, multilateral_monetary_institutions, observer,
    institutional, generational, analytical, global).

% Households and non-financial firms in dollar-dependent economies outside the U.S. bear imported inflation and exchange-rate volatility but have no seat at the monetary-policy tables where the anchor is set.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, dollar_zone_domestic_constituencies, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international trade and liquidity provision by establishing a fiat dollar anchor, replacing the gold-exchange standard with a discretionary U.S.-led monetary regime that supplies a global medium of exchange and settlement.
% TRANSFER_FUNCTION: Moves purchasing power and seigniorage from foreign dollar holders to the U.S. fiscal authority via inflation exposure and devaluation risk, while supplying global liquidity and a trade settlement rail.
% ABSENT_VOICES: Foreign domestic constituencies in dollar-dependent economies who bear imported inflation but are excluded from monetary-policy design; gold-standard advocates and structural-reform proponents who were structurally sidelined after 1971.
% DISAPPEARANCE_RATIONALE: If the fiat dollar anchor vanished overnight, global trade would lose its primary settlement medium, exchange rates would destabilize, U.S. fiscal deficits would face harder constraints, and foreign holders would seek alternative reserve assets â the world rearranges around a new anchor or fragmented liquidity.
% FOUNDING_PROBLEM: The Bretton Woods gold-exchange standard faced a coordination failure: the U.S. could not simultaneously guarantee gold convertibility at $35/ounce and run the deficits needed to supply global liquidity.
% FOUNDING_PROBLEM_CORROBORATION: U.S. Treasury and Federal Reserve officials attest the problem was a binding liquidity-deficit contradiction; foreign creditors and monetary historians attest the problem was soluble through earlier adjustment rather than unilateral abrogation; independent macro-historical analysis from outside the U.S. Treasury supports the adjustment-failure reading.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.62 at interval end) because the U.S. extracts seigniorage and inflation-tax purchasing power from foreign holders, but the constraint also supplies genuine global liquidity. Suppression is moderately high (0.72) because the constraint's persistence depends on the active exclusion of alternative reserve infrastructures and the maintenance of dollar network externalities. Theater is moderate-low (0.32): much of the system's activity is functional, but a growing share of U.S. policy rhetoric frames the dollar's role as a public good while defending private extraction. Accessibility collapse is high (0.70) because once foreign economies are dollarized, exit becomes structurally costly. Resistance is moderate (0.45) because de-dollarization initiatives and regional currency arrangements mount a real but fragmented challenge. The measurement series run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. seat perceives a managed global public good that solved the Bretton Woods liquidity trap; foreign holder seats perceive an expropriative arrangement where purchasing power is transferred via inflation and devaluation without contractual recourse. The divergence is structural, not merely perceptual: the same institution (the Fed) that supplies liquidity also sets the terms of its erosion.
 *
 * DIRECTIONALITY LOGIC:
 *   The us_fiscal_authority sits near the full-beneficiary end: it sets the rules, issues the currency, and collects seigniorage and fiscal space. Foreign_dollar_holders sit near the full-target end: they absorb inflation and devaluation risk, hold non-convertible paper claims, and face constrained exit because alternative reserve markets lack comparable depth. The engine will compute significant seat divergence from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â insufficient global liquidity under the gold standard â was genuinely live in 1971. Whether it remains live today is contested. If the coordination function had atrophied and the constraint persisted by inertia alone, it would drift toward piton. However, ongoing demand for dollar liquidity and the absence of a comparably deep alternative suggest the coordination function is still operative, preventing a simple mandatrophy resolution. The rope claim is protected from being misread as pure extraction only if the coordination surplus exceeds the extraction; the metrics leave that question open, which is why the divergence between claim and metrics is diagnostically valuable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    punctuated_vs_structural_reading,
    'Is the 1971 transition a discrete reversible institutional choice (this reading) or an overdetermined structural inevitability (sibling readings)?',
    'Comparative counterfactual analysis of late-1960s macroeconomic policy space: could alternative adjustment mechanisms (earlier devaluation, SDR expansion, capital controls) have preserved the gold-exchange standard?',
    'If reversible, the constraint is a coordination device maintained by sovereign choice; if inevitable, the constraint is a mountain-like structural feature of reserve-currency economics that would reappear under any similar institutional configuration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(punctuated_vs_structural_reading, conceptual, 'Whether the 1971 transition was discrete choice or structural inevitability.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Does the seigniorage and inflation-tax extraction from foreign dollar holders exceed the value of the global liquidity coordination provided?',
    'Empirical estimation of the global dollar liquidity premium versus cumulative inflation tax and devaluation losses borne by non-U.S. holders.',
    'Would reclassify the constraint from rope to tangled_rope or snare if extraction dominates; would support the rope claim if coordination surplus is larger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether extraction exceeds coordination value.').

omega_variable(
    unilateral_defection_legitimacy,
    'Was the unilateral U.S. defection in 1971 a legitimate exercise of monetary sovereignty or a breach of the Bretton Woods compact that foreign holders were entitled to rely upon?',
    'Legal and normative analysis of the Bretton Woods agreements as binding commitments versus revocable policy frameworks.',
    'If the defection was a breach, the constraint carries a stronger extractive character because the transfer was non-consensual; if legitimate sovereignty, the transfer is framed as a permissible cost of participation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unilateral_defection_legitimacy, preference, 'Normative status of the 1971 unilateral adjustment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1971, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.15).
narrative_ontology:measurement(mone_tr_t1980, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(mone_tr_t1991, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(mone_tr_t2001, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(mone_tr_t2008, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(mone_tr_t2020, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(mone_tr_t2026, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2026, 0.32).

% Extraction over time
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.35).
narrative_ontology:measurement(mone_be_t1980, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(mone_be_t1991, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1991, 0.5).
narrative_ontology:measurement(mone_be_t2001, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2001, 0.48).
narrative_ontology:measurement(mone_be_t2008, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement(mone_be_t2020, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(mone_be_t2026, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.5).
narrative_ontology:measurement(mone_su_t1980, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(mone_su_t1991, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1991, 0.6).
narrative_ontology:measurement(mone_su_t2001, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(mone_su_t2008, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(mone_su_t2020, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(mone_su_t2026, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This constraint is the punctuated_swap_reading of the monetary_anchor_principle kernel, distinct from the overdetermined_composite_reading and triffin_inevitability_reading. All three share the same historical referent (the 1971 transition and subsequent fiat dollar standard) but assign different causal and normative structures, producing different epsilon values and stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
