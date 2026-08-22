% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Post-Gold Fiat Reserve System with Eliminated Creditor Veto (Creditor Discipline Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the creditor-discipline reading of the
 *   gold-to-fiat transition kernel. Under this reading, the collapse of the
 *   Bretton Woods gold-exchange standard and the shift to unbacked fiat
 *   reserve currency did not merely replace one technical constraint with
 *   another, but eliminated a concrete creditor veto mechanismânamely, the
 *   threat of gold redemption that enforced balance-of-payments discipline on
 *   debtor nations. The resulting institutional arrangement concentrates
 *   fiscal flexibility and seigniorage extraction in the reserve-currency
 *   issuer (the United States) while stripping creditor nations of their
 *   hard-settlement leverage. Non-reserve debtor nations face a tightened
 *   constraint of dollar dependency and conditional access to liquidity. The
 *   constraint is therefore a hybrid: it coordinates global trade and
 *   macroeconomic stabilization through a single reserve asset, but
 *   asymmetrically extracts purchasing power and policy autonomy from
 *   creditors and peripheral debtors alike. The reading treats the transition
 *   as a geopolitical power shift, not a neutral evolution.
 *
 * KEY AGENTS:
 *   - us_reserve_issuer: Primary beneficiary and agenda-setter (institutional/arbitrage) â exempt from external discipline, collects seigniorage, sets global monetary conditions.
 *   - debtor_nations: Secondary beneficiary (organized/constrained) â gained fiscal flexibility relative to gold standard but remain subordinate to reserve issuer.
 *   - creditor_nations: Primary target (powerful/constrained) â lost gold redemption leverage, forced to accumulate depreciating fiat reserves.
 *   - non_reserve_debtor_nations: Secondary target (powerless/trapped) â face dollar dependency, IMF conditionality, and inflation tax on reserves.
 *   - gold_standard_advocates: Excluded voice (moderate/constrained) â would argue for restored hard-settlement discipline but are outside current policy discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.79).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.72).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.46).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Post-Gold Fiat Reserve System with Eliminated Creditor Veto (Creditor Discipline Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, 'cfe26f95-c94c-46db-bcad-db86527ea7ac').
narrative_ontology:cs_kernel_codification('cfe26f95-c94c-46db-bcad-db86527ea7ac', distributed).
narrative_ontology:cs_authority_grounding('cfe26f95-c94c-46db-bcad-db86527ea7ac', distributed).
narrative_ontology:cs_reading_relation('cfe26f95-c94c-46db-bcad-db86527ea7ac', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('cfe26f95-c94c-46db-bcad-db86527ea7ac', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('cfe26f95-c94c-46db-bcad-db86527ea7ac', foundational, creditor_veto_constitutive_of_monetary_legitimacy).
narrative_ontology:cs_axiom_status(creditor_veto_constitutive_of_monetary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cfe26f95-c94c-46db-bcad-db86527ea7ac', creditor_veto_constitutive_of_monetary_legitimacy, instrumental).
narrative_ontology:cs_axiom('cfe26f95-c94c-46db-bcad-db86527ea7ac', foundational, reserve_issuer_exemption_distorts_adjustment).
narrative_ontology:cs_axiom_status(reserve_issuer_exemption_distorts_adjustment, holdable).
narrative_ontology:cs_axiom_grounding('cfe26f95-c94c-46db-bcad-db86527ea7ac', reserve_issuer_exemption_distorts_adjustment, empirically_contingent).
narrative_ontology:cs_reference_frame('cfe26f95-c94c-46db-bcad-db86527ea7ac', gold_standard_discipline_equilibrium).
narrative_ontology:cs_drift_state('cfe26f95-c94c-46db-bcad-db86527ea7ac', contemporary_dollar_hegemony, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cfe26f95-c94c-46db-bcad-db86527ea7ac', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_issuer).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_debtor_nations).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, dollar_hegemony_stability).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, exorbitant_privilege_necessary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the global reserve currency and sets the marginal cost of international liquidity through Federal Reserve policy. Exempt from balance-of-payments discipline because its liabilities are accepted as final settlement worldwide. Finances persistent fiscal deficits without external hard-constraint. Collects seigniorage from global dollar demand and exercises sanctions leverage through control of correspondent banking and settlement infrastructure.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_issuer, beneficiary).

% Sovereign debtors that gained macroeconomic policy autonomy relative to the gold standard: they can devalue, inflate, and run counter-cyclical deficits without facing immediate gold redemption. However, non-reserve members of this group experience the benefit unevenly; their fiscal flexibility is bounded by dollar-denominated debt markets and IMF conditionality.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    organized, biographical, constrained, global).

% Surplus nations and reserve accumulators that lost the right to demand gold redemption against dollar claims. Forced to hold fiat reserves whose real value depends on US inflation policy and geopolitical risk. Diversification into non-dollar assets is possible at the margin but structurally costly due to market depth, network effects, and alliance considerations.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, biographical, constrained, global).

% Peripheral economies that must obtain dollars for trade, debt service, and reserve buffers. Subject to IMF structural adjustment when reserves deplete. Cannot issue liabilities accepted as global settlement. Their domestic monetary policy is heavily constrained by dollar credit cycles, capital flight risk, and commodity price volatility denominated in dollars.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_debtor_nations, payer,
    powerless, biographical, trapped, national).

% Economists, historians, and policy advocates who argue that hard-settlement discipline prevents sovereign moral hazard and systemic imbalance. Their arguments are structurally excluded from mainstream central banking discourse, treated as archaic or politically infeasible despite periodic academic resurgence.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_standard_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified global settlement medium and lender-of-last-resort liquidity pool, eliminating the deflationary rigidity and gold-scarcity bottlenecks of the metal-based system. Enables trade finance and macroeconomic stabilization across borders without relying on physical reserve movements.
% TRANSFER_FUNCTION: Moves purchasing power and policy autonomy from creditor nations (via loss of redemption leverage and inflation exposure) and non-reserve debtor nations (via dollar dependency and conditional access) to the reserve-currency issuer (via seigniorage, deficit freedom, and sanctions leverage).
% ABSENT_VOICES: Gold-standard advocates who would demand restored hard-settlement discipline; non-reserve debtor populations who bear inflation and austerity costs but are not represented in IMF governance; rival reserve-currency blocs structurally delayed by network effects and geopolitical pressure.
% DISAPPEARANCE_RATIONALE: If the fiat reserve standard and its enforcement architecture vanished overnight, global trade finance would seize, US Treasury borrowing costs would spike as external hard constraint returned, creditor nations would demand tangible settlement, and peripheral debtors would face immediate currency crises. The world would rearrange around multiple bilateral settlement systems or a new commodity anchor.
% FOUNDING_PROBLEM: The interwar gold standard's pro-cyclical deflation and the Bretton Woods Triffin dilemma, in which global liquidity demand outran US gold reserves, producing periodic confidence crises and restricting macroeconomic policy autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians Eichengreen and Bordo corroborate the deflationary rigidity of the gold standard from outside the US Treasury/Fed beneficiary circle; heterodox economists (Hudson, Minsky) corroborate the Triffin dilemma but argue the fiat solution created new pathologies. Creditor nations and gold-standard advocates dispute that the elimination of their veto was necessary to solve the founding problem.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.79 because the reserve issuer gains substantial seigniorage and deficit freedom while creditor nations absorb the inflation tax and redemption risk. Suppression (0.72) reflects active enforcement: IMF conditionalities enforce the fiat standard on periphery debtors, legal tender laws entrench domestic currency monopolies, and geopolitical pressure (sanctions, SWIFT exclusion) suppress alternatives to dollar settlement. Theater ratio (0.56) is elevated because official discourse frames the arrangement as global public good and necessary liquidity provision, masking the asymmetric extraction. Accessibility collapse (0.71) is high because the network effects of dollar invoicing, military-backed petrodollar recycling, and institutional lock-in make exit prohibitively costly for most agents. Resistance (0.46) is moderate: dedollarization initiatives (BRICS, bilateral yuan settlement, central bank gold accumulation) constitute real but still fragmented pushback.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (US reserve issuer) experiences the constraint as a necessary coordination mechanism that solves global liquidity and trade settlement problems; from this seat, the elimination of gold redemption was a functional response to the Triffin dilemma. The payer seats (creditor nations and non-reserve debtors) experience the same arrangement as the removal of their hard-exit option and the imposition of a soft-currency dependency. The engine will compute divergent classifications: the reserve issuer likely computes toward rope or low-extraction tangled rope, while creditor and peripheral debtor seats compute toward snare or high-extraction tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the US reserve issuer and debtor nations broadly; the reserve issuer is the concentrated beneficiary with arbitrage-grade exit (it writes the rules). Debtor nations are diffuse beneficiaries of increased fiscal space. Victim declarations map to creditor nations (who lost the gold redemption veto) and non-reserve debtor nations (who face tightened dollar dependency). Directionality is derived structurally: the reserve issuer sits near dâ0.0 (full beneficiary), creditor nations near dâ0.9 (full target due to constrained exit from the dollar system despite their power), and non-reserve debtors near dâ1.0 (trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâgold-standard deflationary rigidity and the Triffin dilemmaâwas arguably solved by the fiat transition, but the arrangement persisted and deepened beyond the resolution of that problem. The R5 genealogy interview flags a potential mandatrophy: the problem (insufficient liquidity/gold scarcity) is contested as still live, while the arrangement has accumulated extraction (seigniorage, sanctions leverage) far beyond its original coordination justification. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals capture rather than obsolescenceâindicating tangled rope rather than scaffold or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the elimination of creditor veto power the primary structural effect of the gold-to-fiat transition, or an epiphenomenon of broader technological and institutional changes?',
    'Comparative predictive testing across the three readings: does the creditor-discipline model predict post-1971 balance-of-payments and geopolitical outcomes better than the automatic-constraint or composite-overdetermination models?',
    'If the creditor-discipline reading is primary, the constraint is fundamentally a power relation with high extraction; if composite or automatic, extraction is lower and more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity between power-shift and technocratic readings of the transition').

omega_variable(
    reserve_issuer_exceptionalism,
    'Does the extraction inherent in the fiat reserve system require US-specific geopolitical dominance, or would any reserve currency issuer exhibit the same structural asymmetry?',
    'Historical comparison with pre-1914 sterling standard and potential post-dollar multipolar reserve systems; network-effect analysis of reserve currencies.',
    'If specific to US hegemony, the constraint may erode with multipolarity; if generic to reserve currency status, the extraction is a feature of the international monetary architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_issuer_exceptionalism, empirical, 'Whether reserve-currency extraction is universal or US-specific').

omega_variable(
    non_reserve_beneficiary_status,
    'Do non-reserve debtor nations experience the post-gold fiat system as a net benefit (fiscal flexibility) or net cost (dollar dependency and inflation exposure)?',
    'Cross-national panel analysis comparing macroeconomic policy autonomy under gold-standard, Bretton Woods, and post-1971 fiat regimes for non-reserve issuers.',
    'If net cost, non-reserve debtor nations should be classified as victims alongside creditor nations, raising epsilon further; if net benefit, the beneficiary class is broader and extraction more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_reserve_beneficiary_status, empirical, 'Ambiguity in non-reserve debtor nation structural position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 0, 53).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gold_tr_t8, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(gold_tr_t16, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(gold_tr_t24, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(gold_tr_t32, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement(gold_tr_t40, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(gold_tr_t48, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 48, 0.54).
narrative_ontology:measurement(gold_tr_t53, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 53, 0.56).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gold_be_t8, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(gold_be_t16, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(gold_be_t24, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(gold_be_t32, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 32, 0.73).
narrative_ontology:measurement(gold_be_t40, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(gold_be_t48, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 48, 0.78).
narrative_ontology:measurement(gold_be_t53, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 53, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gold_su_t8, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(gold_su_t16, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(gold_su_t24, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(gold_su_t32, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(gold_su_t40, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(gold_su_t48, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 48, 0.75).
narrative_ontology:measurement(gold_su_t53, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 53, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, global_infrastructure).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gold_fiat_transition_mechanism kernel, isolating the creditor-discipline dimension. The automatic-constraint and composite-overdetermination readings are structurally distinct siblings that assign different epsilon values and stakeholder directionalities to the same historical transition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
