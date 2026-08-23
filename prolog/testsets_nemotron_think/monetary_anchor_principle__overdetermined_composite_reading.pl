% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Bretton Woods Gold-Exchange Standard Collapse (Overdetermined Composite Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story represents the overdetermined composite reading of
 *   the monetary_anchor_principle kernel: the collapse of the Bretton Woods
 *   gold-exchange standard was not a single event (Nixon's 1971 suspension)
 *   nor a single structural inevitability (Triffin dilemma alone), but an
 *   overdetermined composite of four convergent pressures — the Triffin
 *   dilemma (reserve currency deficit requirement vs. gold backing), Vietnam
 *   War fiscal deficits (guns and butter without taxation), Keynesian policy
 *   consensus (treating gold convertibility as a constraint on
 *   countercyclical policy), and technological capital mobility (Eurodollar
 *   markets, communications technology enabling capital flight). Each
 *   pressure alone might have been manageable; their convergence by the late
 *   1960s made the gold anchor's collapse structurally inevitable. The
 *   constraint type is tangled_rope because the system performed a genuine
 *   coordination function (stable exchange rates for trade) while
 *   simultaneously extracting from currency holders and fixed-income
 *   investors to expand state fiscal capacity. Active enforcement (Gold Pool,
 *   capital controls, swap lines, pressure on allies) was required to
 *   maintain the peg as pressures mounted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.78).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.72).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Bretton Woods Gold-Exchange Standard Collapse (Overdetermined Composite Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, 'ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd').
narrative_ontology:cs_kernel_codification('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', fixed_text).
narrative_ontology:cs_authority_grounding('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', lineage).
narrative_ontology:cs_interpretation_layer_present('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd').
narrative_ontology:cs_reading_relation('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', monetary_anchor_principle__punctuated_swap_reading, influences).
narrative_ontology:cs_axiom('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', foundational, transition_was_overdetermined_by_multiple_pressures).
narrative_ontology:cs_axiom_status(transition_was_overdetermined_by_multiple_pressures, holdable).
narrative_ontology:cs_axiom_grounding('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', transition_was_overdetermined_by_multiple_pressures, empirically_contingent).
narrative_ontology:cs_axiom('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', foundational, state_fiscal_capacity_benefited_from_anchor_removal).
narrative_ontology:cs_axiom_status(state_fiscal_capacity_benefited_from_anchor_removal, holdable).
narrative_ontology:cs_axiom_grounding('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', state_fiscal_capacity_benefited_from_anchor_removal, empirically_contingent).
narrative_ontology:cs_reference_frame('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', nixon_shock_1971, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ae336fe4-a3ed-47d0-ad5e-9bf89ab3fdcd', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_authorities).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, currency_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_investors).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, central_bank_governors).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, global_trade_participants).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, global_trade_participants).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, fiscal_policy_autonomy_doctrine).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, managed_currency_stability_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control fiscal and monetary policy; gained freedom from gold convertibility constraint after 1971, enabling deficit financing for Vietnam War and Great Society programs. The gold standard's discipline was the primary obstacle to autonomous fiscal expansion.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_authorities, beneficiary).

% Academic and institutional economists who dominated policy advisory roles; their theoretical framework treated gold convertibility as a barbarous relic constraining countercyclical policy. The collapse vindicated their intellectual authority and expanded their institutional influence.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment, beneficiary,
    organized, generational, analytical, global).

% Domestic and foreign holders of dollar-denominated assets; bore the inflation tax after gold convertibility ended. Exit options limited: could shift to other currencies (constrained by dollar's reserve role), real assets (gold, commodities), or accept depreciation.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, currency_holders, payer,
    moderate, biographical, constrained, global).

% Pension funds, insurance companies, sovereign wealth holders locked into long-duration dollar bonds; suffered real returns erosion as inflation accelerated post-1971. Contractual obligations prevented easy exit; regulatory frameworks often mandated dollar-bond holdings.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_investors, payer,
    organized, biographical, constrained, global).

% Monetarists, Austrian-school economists, and hard-money politicians who warned that abandoning gold would unleash inflation; structurally excluded from policy decisions by the Keynesian consensus. Their exit was intellectual — they could not leave the monetary system, only dissent from within it.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_advocates, excluded,
    moderate, biographical, trapped, global).

% Administered the Bretton Woods system through the Gold Pool and swap lines; gained discretionary policy autonomy after 1971 but also inherited the burden of managing a pure fiat system. Their institutional power expanded but so did accountability for inflation outcomes.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, central_bank_governors, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, central_bank_governors, beneficiary).

% Multinational corporations and trading nations; benefited from stable exchange rates under Bretton Woods for trade planning, but paid increasing costs of capital controls and exchange rate uncertainty as the system strained. Post-1971 gained flexibility but faced volatility.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, global_trade_participants, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, global_trade_participants, payer).

% Monitored the adjustable-peg system and later the floating-rate regime; their analytical frameworks shifted from policing parities to surveillance of exchange rate policies. Neither collected rents nor bore direct costs, but their institutional mandate was reshaped by the transition.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, imf_surveillance_apparatus, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable nominal anchor for international trade and investment under fixed exchange rates, solving the coordination problem of cross-border price stability without a world central bank. The gold-exchange standard economized on gold reserves by using dollars as reserve assets.
% TRANSFER_FUNCTION: Transferred the discipline of gold convertibility from the monetary system to the fiscal authorities: the inflation constraint was removed from currency holders and fixed-income investors, and the fiscal capacity of the reserve-currency issuer (US) was expanded. The seigniorage gain accrued to the US Treasury; the inflation cost was distributed globally across dollar holders.
% ABSENT_VOICES: Gold standard advocates and monetarist dissenters were structurally excluded from the policy consensus by the mid-1960s; their objections were treated as ideological rather than analytical. Developing nations without reserve currency status had no voice in the G-10 / IMF governance that managed the transition.
% DISAPPEARANCE_RATIONALE: If the gold-exchange standard had been maintained (counterfactual), the US would have faced a gold run forcing either drastic deflation or formal devaluation; the fiscal-monetary policy mix of the 1960s-70s would have been impossible. The world rearranged around fiat money, floating rates, and the dollar's exorbitant privilege — a structural reconfiguration of the international monetary order.
% FOUNDING_PROBLEM: Post-WWII reconstruction required a stable international monetary system that avoided both the deflationary bias of the classical gold standard and the competitive devaluations of the 1930s. The Bretton Woods compromise — dollar-gold convertibility at $35/oz with adjustable pegs — was designed to provide liquidity for trade expansion while constraining inflation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (postwar reconstruction + trade expansion under stable rates) was largely solved by the late 1950s; European currencies became convertible in 1958. The Triffin dilemma (identified 1960) demonstrated that the system's own success — supplying global dollar liquidity — undermined its gold backing. This diagnosis comes from Triffin himself (outside the US fiscal beneficiary set), the G-10 central bankers who managed the Gold Pool, and the IMF's own historical analyses — not from the Keynesian establishment that benefited from the system's collapse.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the system's terminal phase transferred massive purchasing power from global dollar holders to the US fiscal authority via the inflation tax. Suppression (0.72) reflects the active enforcement machinery: the London Gold Pool (1961-68), capital controls (Interest Equalization Tax, Voluntary Foreign Credit Restraint), swap lines to defend parities, and political pressure on allies not to convert dollars. Theater ratio (0.48) captures the growing gap between the system's stated purpose (stable rates for trade) and its actual operation (financing US deficits). Accessibility collapse (0.68) is moderate-high: alternatives (floating rates, SDRs, gold revaluation) were discussed but blocked by US veto power and the coordination advantage of the incumbent system. Resistance (0.55) is moderate: European central banks resisted through the Gold Pool and later the Smithsonian Agreement, but lacked coordinated exit power.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (currency holders, fixed-income investors) and the agenda-setter seats (state fiscal authorities, central banks) should compute differently: from the fiscal authority's position, the system's collapse was a necessary adaptation to structural pressures; from the currency holder's position, the same transition was a default on the convertibility promise. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State fiscal authorities (US Treasury, Congress) are primary beneficiaries: they gained fiscal autonomy (d ≈ 0.1). Keynesian establishment benefits intellectually and institutionally (d ≈ 0.2). Central bank governors are dual: gained policy discretion but inherited inflation accountability (d ≈ 0.5). Currency holders and fixed-income investors are primary payers: bore inflation tax with constrained exit (d ≈ 0.85-0.9). Gold standard advocates are excluded: their structural position was marginalization, not extraction (d ≈ 0.7 but exit trapped). Global trade participants are near-symmetric: coordination benefit roughly balanced by volatility cost post-1971 (d ≈ 0.5). IMF apparatus is analytical observer (d ≈ 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The Bretton Woods system's founding mandate (postwar reconstruction under stable rates) was achieved by 1958; the system persisted 13+ years beyond mandate fulfillment. The mandatrophy is resolved in the sense that the arrangement's original justification was dead, but the system did not disappear — it mutated into a fiat dollar standard. This is not pure extraction (snare) because the coordination function (trade stability) was real and valued by participants; it is not pure coordination (rope) because the extraction from dollar holders was structural and asymmetric. Tangled rope captures the hybrid: a genuine coordination mechanism that became an extraction vehicle when its founding problem died but its enforcement machinery was repurposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is the overdetermined composite reading a distinct constraint from its sibling readings, or a meta-framing that subsumes them?',
    'Test ε-invariance: if the composite reading''s ε (0.78) differs materially from the triffin_inevitability_reading''s ε (expected ~0.65) and the punctuated_swap_reading''s ε (expected ~0.45), they are distinct constraints. The composite reading''s ε is higher because it includes Vietnam deficits and Keynesian consensus as independent extraction amplifiers.',
    'If distinct, three separate constraint stories are warranted (ε-invariance principle). If the composite reading is merely a meta-framing, it should not have its own ε but should be an analytical overlay. The current authoring treats it as a distinct constraint with its own stakeholder structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the overdetermined composite reading instantiates a separate constraint from its sibling readings of the same kernel.').

omega_variable(
    triffin_sufficiency_vs_overdetermination,
    'Was the Triffin dilemma alone sufficient to force collapse, or did it require the Vietnam deficit and Keynesian consensus pressures to make collapse inevitable by the late 1960s?',
    'Counterfactual historical analysis: simulate a timeline where Triffin pressure exists but US runs balanced budgets (no Vietnam War) and maintains a hard-money consensus. Would the gold pool have held? The Eurodollar market''s independent growth (technological capital mobility) is a third independent variable.',
    'If Triffin alone was sufficient, the triffin_inevitability_reading and overdetermined_composite_reading converge on the same constraint (ε-invariance violation). If Triffin was necessary but not sufficient, they are distinct constraints with different ε values and different victim/beneficiary structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_sufficiency_vs_overdetermination, empirical, 'Whether the Triffin dilemma was a sufficient cause or one necessary component of an overdetermined composite.').

omega_variable(
    keynesian_consensus_as_extraction_mechanism,
    'Was the Keynesian policy consensus a genuine intellectual conviction that gold convertibility constrained welfare-enhancing policy, or a rationalizing ideology for fiscal expansion?',
    'Analyze the internal debates of the Council of Economic Advisers, Fed Board, and academic economists 1961-1968: did they acknowledge the inflationary consequences of their policy mix and proceed anyway, or did they genuinely believe the Phillips curve tradeoff was stable?',
    'If genuine conviction, the Keynesian establishment is a beneficiary in good faith (coordination function). If rationalizing ideology, they are complicit in extraction (snare component). This affects the tangled_rope vs. snare boundary for the Keynesian establishment seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(keynesian_consensus_as_extraction_mechanism, conceptual, 'Whether the intellectual framework enabling the transition was a coordination-enabling belief or an extraction-rationalizing ideology.').

omega_variable(
    eurodollar_market_exogeneity,
    'Was the growth of the Eurodollar market (technological capital mobility) an exogenous shock to the Bretton Woods system, or an endogenous response to the system''s own interest rate differentials and regulatory arbitrage opportunities?',
    'Trace the causality: did Eurodollar growth precede and cause the system''s strain, or did the system''s strain (rising US rates, Regulation Q) create the arbitrage opportunity that grew the Eurodollar market?',
    'If exogenous, it is an independent mountain feeding the composite. If endogenous, it is a feedback loop within the system, not an independent pressure. This changes the number of upstream mountains from four to three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eurodollar_market_exogeneity, empirical, 'Whether capital mobility was an independent structural pressure or an endogenous consequence of the system''s own contradictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monetary_anchor_overdetermined_tr_t1944, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(monetary_anchor_overdetermined_tr_t1950, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(monetary_anchor_overdetermined_tr_t1958, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(monetary_anchor_overdetermined_tr_t1960, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(monetary_anchor_overdetermined_tr_t1965, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1965, 0.38).
narrative_ontology:measurement(monetary_anchor_overdetermined_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.45).
narrative_ontology:measurement(monetary_anchor_overdetermined_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.48).
narrative_ontology:measurement(monetary_anchor_overdetermined_tr_t1973, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1973, 0.52).

% Extraction over time
narrative_ontology:measurement(monetary_anchor_overdetermined_be_t1944, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(monetary_anchor_overdetermined_be_t1950, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(monetary_anchor_overdetermined_be_t1958, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(monetary_anchor_overdetermined_be_t1960, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(monetary_anchor_overdetermined_be_t1965, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(monetary_anchor_overdetermined_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.68).
narrative_ontology:measurement(monetary_anchor_overdetermined_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.78).
narrative_ontology:measurement(monetary_anchor_overdetermined_be_t1973, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1973, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(monetary_anchor_overdetermined_su_t1944, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(monetary_anchor_overdetermined_su_t1950, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(monetary_anchor_overdetermined_su_t1958, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement(monetary_anchor_overdetermined_su_t1960, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement(monetary_anchor_overdetermined_su_t1965, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement(monetary_anchor_overdetermined_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.72).
narrative_ontology:measurement(monetary_anchor_overdetermined_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.78).
narrative_ontology:measurement(monetary_anchor_overdetermined_su_t1973, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1973, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, fiat_dollar_standard).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, petrodollar_recycling).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, volcker_disinflation).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel decomposes into three constraint stories: (1) overdetermined_composite_reading (this story) — ε=0.78, tangled_rope, four upstream mountains; (2) triffin_inevitability_reading — ε≈0.65, tangled_rope or snare, single upstream mountain (Triffin); (3) punctuated_swap_reading — ε≈0.45, scaffold or rope, discrete choice with coordination function. The ε values differ because the referent (standing arrangement under contest) is evaluated differently: the composite reading sees extraction from all four pressures; the Triffin reading sees extraction from reserve-currency structure alone; the swap reading sees a coordination mechanism with modest extraction. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, institutional, 0.15).
constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, organized, 0.35).
constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
