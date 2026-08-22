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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Conditional Policy Obligation
 *   domain: international_political_economy
 *
 * SUMMARY:
 *   Under the strict letter of Article IV of the IMF Articles of Agreement
 *   (negotiated at Bretton Woods, 1944), the U.S. undertook to exchange
 *   dollars for gold at a fixed rate (35 dollars per fine ounce) at the
 *   request of any national monetary authority. The policy-flexible reading
 *   interprets this commitment as binding in form but subordinate to domestic
 *   macroeconomic stability: the U.S. retains the authority to suspend or
 *   modify convertibility when domestic economic policy requires monetary
 *   expansion incompatible with gold discipline. This reading was
 *   operationalized through the 1960s as U.S. deficits accumulated and gold
 *   redemptions pressured reserves; it culminated in the 1971 'Nixon Shock'
 *   suspension. The structural delta from this reading: foreign dollar
 *   holders enter the victim set (bearing devaluation risk and the cost of
 *   reserve depreciation); the U.S. exits the victim set (regains full
 *   monetary autonomy); extractiveness shifts from domestic constraint to
 *   asymmetric burden on creditors holding dollars. This is the reading held
 *   by U.S. policymakers from the mid-1960s forward; it competes with the
 *   strict-convertibility reading (held by fixed-peg adherents and financial
 *   conservatives) and the structural (Triffin) reading (which sees the
 *   flexibility as a symptom of an impossible trilemma, not a solution).
 *
 * KEY AGENTS:
 *   - U.S. Federal Reserve and Treasury: interpret and enforce the flexible reading; benefit from regained monetary autonomy
 *   - Foreign central banks and dollar-reserve countries: hold dollar liabilities; face devaluation risk and forced carry of expansionary U.S. policy
 *   - Fixed-peg adherents (IMF member states): locked into Bretton Woods; constrained by dollar discipline even as the issuer escapes it
 *   - Bretton Woods institutional structure: observer seat; sees the reading-dependent contradiction built into the architecture
 *   - Gold standard architects (now absent): would have contested the subordination of convertibility to discretionary monetary policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.68).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.72).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Conditional Policy Obligation").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'a49fea8d-114f-4f75-9b87-1179ea7eb411').
narrative_ontology:cs_kernel_codification('a49fea8d-114f-4f75-9b87-1179ea7eb411', fixed_text).
narrative_ontology:cs_authority_grounding('a49fea8d-114f-4f75-9b87-1179ea7eb411', lineage).
narrative_ontology:cs_interpretation_layer_present('a49fea8d-114f-4f75-9b87-1179ea7eb411').
narrative_ontology:cs_reading_relation('a49fea8d-114f-4f75-9b87-1179ea7eb411', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('a49fea8d-114f-4f75-9b87-1179ea7eb411', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('a49fea8d-114f-4f75-9b87-1179ea7eb411', foundational, convertibility_subordinate_to_macroeconomic_autonomy).
narrative_ontology:cs_axiom_status(convertibility_subordinate_to_macroeconomic_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a49fea8d-114f-4f75-9b87-1179ea7eb411', convertibility_subordinate_to_macroeconomic_autonomy, instrumental).
narrative_ontology:cs_axiom('a49fea8d-114f-4f75-9b87-1179ea7eb411', secondary, international_monetary_order_compatible_with_discretionary_policy).
narrative_ontology:cs_axiom_status(international_monetary_order_compatible_with_discretionary_policy, overridden).
narrative_ontology:cs_axiom_grounding('a49fea8d-114f-4f75-9b87-1179ea7eb411', international_monetary_order_compatible_with_discretionary_policy, empirically_contingent).
narrative_ontology:cs_reference_frame('a49fea8d-114f-4f75-9b87-1179ea7eb411', bretton_woods_flexible_convertibility).
narrative_ontology:cs_drift_state('a49fea8d-114f-4f75-9b87-1179ea7eb411', late_1960s_reserve_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a49fea8d-114f-4f75-9b87-1179ea7eb411', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_government).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_dollar_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, fixed_exchange_rate_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, domestic_economic_policy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Federal Reserve and U.S. Treasury interpret the convertibility obligation as binding in law but subordinate to domestic economic stability — retain the option to suspend or modify convertibility when U.S. macroeconomic conditions demand it. Enforce the rule by managing redemption flows and signaling the policy flexibility to international creditors and allies. The interpretation grants the U.S. regimes of monetary policy autonomy unavailable to convertibility-strict readings.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold dollars as reserves and payment instruments, relying on the convertibility commitment; face devaluation risk and redemption delays when the U.S. prioritizes domestic monetary expansion over gold discipline. Their choice set is bounded by the dollar's centrality in post-war trade and finance — exit from dollar holdings means abandoning liquidity and scale advantages; no equally fungible alternative exists at the time.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_dollar_holders, payer,
    powerful, biographical, constrained, global).

% Central banks and treasuries of allied nations maintain fixed pegs to the dollar as the monetary anchor of the Bretton Woods system. Their commitment to the peg forces them to absorb dollar inflows and outflows, constraining their own monetary autonomy. When the U.S. runs deficits and inflates, they face pressure to either maintain the peg (importing inflation) or abandon it (losing the stability anchor).
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, fixed_exchange_rate_adherents, payer,
    organized, biographical, constrained, global).

% U.S. domestic policy constituencies — labor movements, housing advocates, welfare-state builders — benefit from the reading that prioritizes domestic expansion over the gold standard constraint. They argue that using monetary policy for full employment and inflation-tolerant growth is the proper function of central banking, not subordination to external redemption discipline.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, domestic_economic_policy_advocates, beneficiary,
    institutional, biographical, mobile, national).

% The International Monetary Fund and the international negotiating order designed to stabilize post-war currency arrangements. The observer seat notes the reading-dependent interpretation: strict convertibility = binding constraint on U.S. policy; policy-flexible reading = the same text permits autonomous monetary policy when domestic need arises. The structural outcome under the flexible reading is the eventual breakdown of fixed pegs.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_institutional_structure, observer,
    institutional, generational, analytical, global).

% Architects and adherents of the classical gold standard (and its Bretton Woods successor) envisioned disciplinary constraints that would prevent the monetary inflation the policy-flexible reading permits. They are structurally absent from the U.S. policy apparatus by the mid-1960s, unable to contest the reframing of convertibility as subordinate to domestic economic goals.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, gold_standard_architects, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable international monetary order: a common redemption anchor (gold backing the dollar) enables fixed exchange rates, reduces currency transaction costs, and solves the double-coincidence-of-wants problem in multilateral trade. Solves the coordination problem of 'what backs trust in paper claims across borders?'
% TRANSFER_FUNCTION: Moves monetary policy autonomy from the international system (where convertibility discipline constrains the issuer) to the U.S. (where domestic economic goals take priority). Simultaneously transfers the risk of devaluation and redemption uncertainty from the U.S. to foreign dollar holders and fixed-peg adherents who have locked in dollar reserves.
% ABSENT_VOICES: Gold standard purists and international finance architects who prioritized disciplinary constraint are excluded from the policy apparatus by the 1960s. Countries that would benefit from a multi-reserve system or demonetization of gold (removing the constraint entirely) are not at the table; the reading assumes the dollar will remain the settlement currency regardless.
% DISAPPEARANCE_RATIONALE: If the policy-flexible interpretation were not available — if convertibility were strictly binding regardless of domestic macroeconomic conditions — the U.S. would either (a) abandon the commitment entirely (as it eventually did in 1971), or (b) sacrifice domestic monetary policy autonomy to gold discipline. Either way, the post-war Bretton Woods arrangement would restructure around a different monetary anchor or devolve to competitive devaluation.
% FOUNDING_PROBLEM: Post-war monetary order required a stable anchor and a credible medium of exchange for international trade. The gold standard provided the anchor; U.S. willingness to back dollars in gold provided confidence. The founding problem was: how to restore multilateral trade and capital flows after depression and war?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is live in the sense that currency stability and trade facilitation remain persistent international challenges. However, corroboration from outside the U.S. policy seat and the dollar beneficiaries is contested: by the 1960s, economists like Triffin (external to the benefiting parties) were attesting that the founding problem could NOT be solved under the Bretton Woods framework — the 'confidence problem' and 'adjustment problem' were structural, not incidental. The reading assumes the problem remains solvable under a flexible convertibility interpretation; the structural (Triffin) reading contests this.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The policy-flexible reading extracts substantially (0.68 at interval end) because it transfers monetary policy autonomy gains from the international system to the U.S., with costs diffused to foreign creditors and pegged-rate countries. Suppression is high (0.72) because the reading's persistence depends on the U.S. credibly signaling willingness to suspend convertibility if necessary — the threat of suspension suppresses attempts by foreign holders to rush the redemption window. Theater ratio rises from 0.18 to 0.41 over the interval as the reading's operational justification ('protecting domestic stability') increasingly becomes a cover story; by the late 1960s, the real function is enabling deficit spending incompatible with gold discipline. The measurement series tracks the accumulation of U.S. deficits and the rising pressure on gold reserves, which forces greater theatrical commitment to 'maintaining' convertibility even as the underlying U.S. position weakens. The temporal ceiling at t=30 reflects the reading's endpoint: by 1971, the flexibility had extracted as much as it could; the formal suspension admits what the reading had been denying. Accessibility collapse is low (0.48) because fixed-peg countries and gold-standard advocates retained alternatives (other monetary anchors, multilateral negotiation) even though they were constrained in practice; the reading did not make alternatives literally inaccessible, only costlier to exercise.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. policy seat, the reading is coordination: a flexible framework enables both macroeconomic stability AND international monetary order, balancing competing goods. From the foreign dollar-holder seat, the reading is asymmetric extraction: the U.S. has unilaterally reinterpreted a binding commitment to suit its own macroeconomic needs, converting foreign dollar reserves into risk-bearing instruments. From the fixed-peg-adherent seat, the reading is coercion: they are locked into a system where the central issuer has interpreted its own obligations away. The engine computes these divergences from the structural data — beneficiaries, victims, power atoms, and exit options — not from our narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. monetary authorities hold the institutional power and enjoy high exit options (arbitrage: they can reinterpret the rule, suspend it, or abandon it). They are beneficiaries under this reading because the flexible interpretation grants them monetary autonomy they would not have under the strict reading. Foreign dollar holders are powerful institutions (central banks) but face constrained exit options (if they abandon dollar reserves, they lose liquidity and scale). Their directionality is high (near target), and they are victims because the reading imposes devaluation risk on dollar holdings. Fixed-peg adherents are organized but face trapped exit (they cannot abandon the Bretton Woods peg without unilateral devaluation, which breaks the system). They are payers because the reading's flexibility forces them to absorb U.S. inflation and reserve volatility.
 *
 * MANDATROPHY ANALYSIS:
 *   The policy-flexible reading is not mandatroph under the standard test (founding_problem_status='live'). However, the reading exhibits a mandate-drift pattern: the original problem ('how to back post-war trade with stable currency anchors?') remains live, but the reading's solution (convertibility subordinate to domestic U.S. policy) becomes increasingly inadequate over the interval. By t=25-30, the reading is theatrically maintained (theater_ratio=0.41) even as U.S. deficits mount and redemptions pressure reserves. The rising theater and rising suppression requirement (both plateau by t=30) signal that the reading is approaching functional collapse. This is not mandatrophy in the sense of a dead founding problem; it is mandate-drift in the sense of an increasingly unsustainable solution to a live problem. The Triffin reading (the sibling constraint) will classify this as the prediction of a structural impossibility, not a flexible policy option.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flexible_vs_subordinate_interpretation,
    'Does the Article IV text permit the U.S. to interpret convertibility as subordinate to domestic economic stability, or is the subordination a post-hoc reframing used to justify non-compliance?',
    'Textual analysis of Article IV negotiating history and contemporaneous legal commentary; comparison with other explicit sunset or conditional-obligation clauses in the IMF Articles; evidence from U.S. Treasury legal counsel positions over the interval.',
    'If the subordination is a permitted interpretation, the policy-flexible reading is a coherent instantiation of the law. If it is a reframing, the reading masks a de facto default on the original commitment, and the constraint reclassifies as a snare (pure extraction with legal cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexible_vs_subordinate_interpretation, conceptual, 'Whether the flexible reading is a legitimate interpretation or post-hoc justification of non-compliance.').

omega_variable(
    triffin_impossibility_premise,
    'Is the policy-flexible reading a sustainable equilibrium, or does it necessarily collapse when foreign creditors lose confidence in the convertibility commitment?',
    'Empirical outcome: the reading''s operational lifetime and triggering conditions for breakdown (gold-reserve pressure, run on redemptions, or diplomatic renegotiation). Structural analysis: can any quantity of suppression (credible threat of redemption suspension) sustain foreign dollar confidence if the U.S. is running persistent deficits incompatible with gold discipline?',
    'If the Triffin critique is structurally sound, the policy-flexible reading is a temporary extraction mechanism that must eventually fail. If sustainable, the reading is a genuine structural innovation in monetary design. Classification consequence: structural impossibility → tangled_rope (coordination + extraction) becomes piton (extraction maintained by threat until confidence collapses) or snare (pure extraction with temporary cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_impossibility_premise, empirical, 'Whether the policy-flexible reading can sustain a stable equilibrium or necessarily collapses.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression that maintains the reading''s operation primarily external (U.S. credible threat of suspension) or internalized (foreign creditors have accepted the reading as legitimate and no longer believe alternatives are accessible)?',
    'Analysis of foreign-government statements, Federal Reserve communications, and reserve-policy decisions over the interval; comparison of post-1971 behavior (after the reading breaks down) with pre-1965 behavior (before the reading was operationalized).',
    'If suppression is primarily external/threatened, the reading relies on continuous U.S. enforcement capacity and will collapse if the threat becomes non-credible (reserves deplete). If internalized, foreign creditors may maintain dollar holdings even after the suppression mechanism weakens, extending the reading''s functional lifetime. Post-breakdown, the nature of suppression tells us whether the reading was extraction (external) or whether foreign creditors accepted it as a legitimate policy adjustment (internalized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the suppression maintaining the reading is external or internalized.').

omega_variable(
    kernel_reading_contest_premises,
    'Do the three sibling readings (strict-convertibility, policy-flexible, structural-Triffin) rest on incommensurable premises about what international monetary order is possible, or is there an overarching framework that could accommodate all three?',
    'Comparative analysis of the axioms (see cs_structure.axioms): do they share grounding types and assumptions, or do they rest on different premises about causation, law, and political economy? Examine whether a single institutional framework could theoretically hold all three readings, or whether they are logically incompatible within the Bretton Woods architecture.',
    'If the readings are incommensurable, each is a live competitor and the contest is genuine political choice. If there is an overarching framework, one reading may be a special case of another, and the contest is about emphasis rather than logical opposition. This omega tracks whether the three constraints are genuinely alternative designs or variant interpretations of a single coherent structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_premises, conceptual, 'Whether the three readings are logically incommensurable or instances of a single overarching framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dollar_convertibility_policy_flex_tr_t0, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_tr_t0, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_tr_t5, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_tr_t5, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_tr_t10, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_tr_t10, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_tr_t15, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_tr_t15, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_tr_t20, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_tr_t20, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_tr_t25, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_tr_t25, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_tr_t30, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(dollar_convertibility_policy_flex_be_t0, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_be_t0, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_be_t5, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_be_t5, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_be_t10, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_be_t10, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_be_t15, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_be_t15, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_be_t20, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_be_t20, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_be_t25, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_be_t25, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_be_t30, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(dollar_convertibility_policy_flex_su_t0, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_su_t0, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_su_t5, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_su_t5, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_su_t10, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_su_t10, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_su_t15, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_su_t15, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_su_t20, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_su_t20, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_su_t25, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_su_t25, observed).
narrative_ontology:measurement(dollar_convertibility_policy_flex_su_t30, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(dollar_convertibility_policy_flex_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__policy_flexible_reading, 0.22).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% The dollar-gold convertibility kernel decomposes into three structurally distinct constraints: (1) strict_convertibility_reading treats the obligation as binding and constrains U.S. policy; (2) policy_flexible_reading (this one) interprets the obligation as binding in form but subordinate to domestic macroeconomic needs; (3) triffin_structural_reading sees the obligation as structurally impossible and inherently unstable. The three readings share the same formal referent (Article IV) but instantiate different ε values and beneficiary/victim structures. The policy-flexible reading influences both siblings: it constrains the strict reading by showing that flexibility has been operationalized, and it confirms the Triffin reading by demonstrating that the flexibility eventually collapses under the structural pressures the Triffin reading predicts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
