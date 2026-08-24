% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-Fiat Transition as Convergent Structural Changes
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   The gold-to-fiat transition is conventionally dated to Nixon's 1971
 *   suspension of dollar-gold convertibility. This reading argues that 'the
 *   transition' is a retrospective construct imposed on a convergence of
 *   structurally independent changes: (1) telecommunications advances (SWIFT
 *   1973, Reuters Monitor 1973, computerized trading) that made instant
 *   capital flows operationally possible for the first time; (2) the
 *   cumulative collapse of the Bretton Woods peg system (1968 two-tier gold
 *   market, 1971 Smithsonian Agreement, 1973 generalized floating) driven by
 *   US deficits and European resistance to revaluation; (3) a secular shift
 *   in labor bargaining power (productivity-wage decoupling from ~1973, union
 *   density decline, globalization of labor markets) that changed the
 *   domestic political economy of adjustment; (4) the maturation of legal
 *   tender enforcement (central bank lender-of-last-resort credibility,
 *   deposit insurance, payment system oversight) that made fiat money
 *   operationally viable without metallic backing. Nixon Shock was a symbolic
 *   marker — the US simply acknowledged what the Eurodollar market and
 *   capital flight had already made inevitable. The constraint story treats
 *   this convergence as a single tangled_rope constraint: genuine
 *   coordination functions (payment finality, exchange rate discovery,
 *   liquidity provision) coexist with asymmetric extraction (reserve currency
 *   privilege, financial sector rents, labor discipline).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.35).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-Fiat Transition as Convergent Structural Changes").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'bf0d6428-abf6-4fde-92c3-ca008db4186f').
narrative_ontology:cs_kernel_codification('bf0d6428-abf6-4fde-92c3-ca008db4186f', distributed).
narrative_ontology:cs_authority_grounding('bf0d6428-abf6-4fde-92c3-ca008db4186f', distributed).
narrative_ontology:cs_reading_relation('bf0d6428-abf6-4fde-92c3-ca008db4186f', gold_fiat_transition_mechanism__automatic_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('bf0d6428-abf6-4fde-92c3-ca008db4186f', gold_fiat_transition_mechanism__creditor_discipline_reading, forecloses).
narrative_ontology:cs_axiom('bf0d6428-abf6-4fde-92c3-ca008db4186f', foundational, transition_was_convergent_not_singular).
narrative_ontology:cs_axiom_status(transition_was_convergent_not_singular, holdable).
narrative_ontology:cs_axiom_grounding('bf0d6428-abf6-4fde-92c3-ca008db4186f', transition_was_convergent_not_singular, empirically_contingent).
narrative_ontology:cs_axiom('bf0d6428-abf6-4fde-92c3-ca008db4186f', foundational, nixon_shock_was_symbolic_not_causal).
narrative_ontology:cs_axiom_status(nixon_shock_was_symbolic_not_causal, holdable).
narrative_ontology:cs_axiom_grounding('bf0d6428-abf6-4fde-92c3-ca008db4186f', nixon_shock_was_symbolic_not_causal, empirically_contingent).
narrative_ontology:cs_reference_frame('bf0d6428-abf6-4fde-92c3-ca008db4186f', multi_causal_convergence).
narrative_ontology:cs_drift_state('bf0d6428-abf6-4fde-92c3-ca008db4186f', contemporary_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bf0d6428-abf6-4fde-92c3-ca008db4186f', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_institutions).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, capital_mobile_actors).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_fixed_exchange_regimes).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_nations_under_gold).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, developing_nations_debt).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, chartalism).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, endogenous_money_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States as issuer of the global reserve currency gained exorbitant privilege: ability to run persistent deficits, set global monetary conditions, and avoid gold convertibility discipline after 1971. It administered the transition through unilateral suspension of gold convertibility and subsequent management of the floating rate system. Exit from the system it dominates is structurally near-impossible for others but trivial for itself.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer, beneficiary).

% Commercial and investment banks gained expanded credit creation capacity, new derivative markets for currency and interest rate risk, and privileged access to central bank liquidity. The end of gold discipline and fixed exchange rates created vast new profit pools in foreign exchange trading, interest rate arbitrage, and sovereign debt markets. Their exit options are high — they can relocate activities across jurisdictions.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_institutions, beneficiary,
    powerful, biographical, mobile, global).

% Multinational corporations, hedge funds, and wealthy individuals gained ability to move capital instantly across borders, arbitrage interest rate differentials, and escape domestic monetary repression. Telecommunications advances (SWIFT, Reuters, later electronic trading) gave them operational capacity that did not exist under Bretton Woods. They are the most mobile agents in the system.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, capital_mobile_actors, beneficiary,
    organized, biographical, arbitrage, global).

% Workers in countries maintaining fixed exchange rates (European EMS members, Latin American peggers) bore the adjustment costs of defending parities: wage suppression, unemployment, and loss of bargaining power when capital mobility made devaluation costly. Their exit is constrained by national borders, language, and skill specificity — they cannot easily follow capital.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_fixed_exchange_regimes, payer,
    moderate, biographical, constrained, national).

% Surplus countries (Germany, Japan, Switzerland, oil exporters) lost the gold redemption threat that disciplined deficit nations. They accumulated low-yield dollar reserves they could not convert to gold, effectively subsidizing US deficits. Their exit is constrained by the need to maintain export competitiveness and the lack of an alternative reserve asset — they are locked into recycling surpluses into US Treasuries.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_nations_under_gold, payer,
    powerful, generational, constrained, global).

% Low-income countries faced the triple bind of petrodollar recycling, volatile interest rates, and commodity price shocks — all enabled by the fiat system's elasticity. The 1980s debt crisis was a direct consequence of the credit expansion the transition unleashed. They have no meaningful exit: capital controls are punished by markets, default cuts off financing, and IMF conditionality enforces the creditor-friendly framework.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, developing_nations_debt, payer,
    powerless, generational, trapped, global).

% Gained full discretion over monetary policy — lender of last resort, interest rate targeting, quantitative easing — but also inherited the burden of managing exchange rates, financial stability, and inflation without an external anchor. Their 'exit' is analytical: they debate regime choices (inflation targeting, price level targeting, nominal GDP targeting) but cannot exit the fiat constraint itself.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, agenda_setter,
    institutional, generational, analytical, national).

% Produce the competing readings of the transition: automatic constraint, creditor discipline, composite overdetermination. Their 'situation' is the epistemic contest over whether the transition was a singular regime change or a convergent emergence. They bear no material cost from the constraint but their professional standing depends on which reading prevails in the literature.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The convergence solved multiple distinct coordination problems simultaneously: (1) telecommunications (SWIFT, Reuters, electronic trading) enabled instant cross-border settlement coordination without physical gold movement; (2) legal tender enforcement maturation coordinated domestic payment finality without metallic backing; (3) the Bretton Woods collapse forced a new exchange rate coordination regime (floating rates, then managed floats); (4) the Eurodollar market coordinated offshore dollar liquidity outside any national jurisdiction. No single authority designed this convergence — it emerged from independent technological, legal, and geopolitical shifts.
% TRANSFER_FUNCTION: The convergence transferred: (a) monetary policy discretion from an automatic gold anchor to central banks (seigniorage and crisis management authority); (b) seigniorage benefits and deficit financing capacity to the reserve currency issuer (US); (c) adjustment burdens from symmetric (gold flows) to asymmetric (deficit nations adjust via austerity, surplus nations accumulate reserves); (d) credit allocation power from gold-constrained banking to financially innovative institutions; (e) exchange rate risk from governments (fixed parities) to private actors (corporates, banks, households).
% ABSENT_VOICES: Developing nations were excluded from the 1971 Smithsonian Agreement and subsequent G10/G7 governance of the new system — they would have objected to the asymmetric adjustment burden and the recycling of petrodollars through commercial banks that precipitated the 1980s debt crisis. Labor unions in fixed-exchange regimes (European social partners) were not consulted on the financial liberalization that eroded their bargaining leverage. Future generations who inherit the debt dynamics and climate-externality subsidies of the fiat system have no voice in the original transition.
% DISAPPEARANCE_RATIONALE: If the specific convergence of telecom-enabled capital mobility, Bretton Woods collapse, labor power shifts, and legal tender maturation had not occurred, the world would not have arrived at the current fiat system. The gold standard might have persisted in modified form (e.g., SDR-based), or a different fiat transition might have occurred with different distributional outcomes. The modern monetary architecture — floating rates, independent central banks, global dollar funding markets, financialized accumulation — is contingent on this specific convergence. Removing it rearranges the monetary world.
% FOUNDING_PROBLEM: The Bretton Woods gold-exchange standard faced a trilemma: fixed exchange rates, capital mobility, and domestic monetary autonomy could not coexist. The US balance of payments deficit (Triffin dilemma) made gold convertibility unsustainable. European surplus nations resisted revaluation. Domestic full-employment commitments (especially in the US and UK) clashed with external discipline. The system needed to break somewhere — but the composite reading insists it broke in multiple independent places at once, not at a single causal node.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Eichengreen 'Globalizing Capital', Kindleberger 'Manias, Panics, and Crashes', Obstfeld 'The International Monetary System') document the trilemma and the gold standard's incompatibility with domestic policy autonomy — corroborating the founding problem's existence. However, the composite reading's specific claim that the transition was a convergence of independent changes with no unified causal node is NOT corroborated outside its proponents (e.g., Helleiner 'States and the Reemergence of Global Finance', Burn 'The Global Financial System'). The automatic and creditor readings remain dominant in textbook accounts. No external authority has endorsed the composite reading's causal decomposition as the definitive account.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).
:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the convergence created both genuine coordination gains (lower transaction costs, crisis management capacity) and extractive rents (exorbitant privilege, financial sector profits, asymmetric adjustment). Suppression (0.35) reflects that alternatives (gold standard, fixed rates, capital controls) were not violently suppressed but became operationally obsolete — the constraint persists because the convergent changes are mutually reinforcing, not because of active coercion. Theater ratio is low (0.15) because the coordination functions are real and the extraction is structural, not performative. Accessibility collapse (0.60) is moderate: returning to a gold standard is technically possible but would require unwinding the entire convergent stack (telecom, legal tender, labor markets, geopolitical order). Resistance (0.40) reflects ongoing political contestation (gold standard advocates, MMT, crypto alternatives) but no effective counter-coalition. The time series shows extractiveness rising through the 1960s (Eurodollar growth, gold pool strains), spiking at the Nixon Shock (1971) and float (1973), then settling as the new institutional architecture consolidated. Theater peaks at 1971 (the performative 'temporary' suspension) then declines as the system normalizes. Suppression requirement peaks at 1971 (capital controls, wage-price controls) then falls as the fiat system gains legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the reserve currency issuer's seat, the convergence looks like a successful adaptation: the US managed the transition, retained privilege, and gained policy space. From labor's seat in fixed regimes, it looks like a coordinated attack on bargaining power via capital mobility. From developing nations' seat, it looks like a debt trap engineered by the same financial institutions that benefited. The engine's per-seat classification will capture this divergence: the same tangled_rope constraint computes as rope-like for beneficiaries (coordination gains exceed extraction) and snare-like for trapped payers (extraction dominates, no exit). The composite reading's analytical seat sees the full structure — the convergence itself, not any single strand.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve currency issuer (US) is the primary structural beneficiary: it collects seigniorage, sets the global monetary template, and faces no exit constraint. Financial institutions and capital-mobile actors are secondary beneficiaries with high mobility — they extract rents from the system's elasticity. Labor in fixed regimes, creditor nations, and developing nations are payers: they bear adjustment costs, hold depreciating reserves, or face debt traps. Their exit options range from constrained to trapped. Central banks are dual-positioned agenda-setters: they gained discretion (benefit) but inherited systemic instability management (cost). Economic historians are pure observers. The engine will compute directionality from these structural positions: beneficiaries near d=0.0, payers near d=1.0, central banks near d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Bretton Woods trilemma) is contested as live/dead: some argue the trilemma persists in new form (impossible trinity), others say fiat money resolved it. The composite reading prevents mislabeling by refusing the singular 'transition' narrative that would classify the outcome as either pure coordination (automatic constraint reading) or pure extraction (creditor discipline reading). By naming the convergence, it shows that coordination and extraction are inseparably entangled — telecom enabled both efficient payments AND capital flight; legal tender enabled both domestic stability AND seigniorage extraction. This is the tangled_rope signature: the coordination function is real and necessary, but it carries asymmetric extraction that cannot be removed without losing the coordination. Mandatrophy is resolved insofar as the founding problem (gold standard incompatibility with domestic autonomy) is dead, but the constraint persists because the convergent stack has no sunset clause and no single agent can unwind it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convergence_independence,
    'Were the four structural changes (telecom, Bretton Woods collapse, labor shifts, legal tender maturation) truly independent, or did they share a common cause (e.g., US hegemony decline, Cold War imperatives, technological determinism)?',
    'Counterfactual historical analysis: if US hegemony had not declined, would telecom advances still have enabled capital mobility? If Bretton Woods had survived, would labor bargaining power have shifted similarly? Compare with other monetary transitions (e.g., classical gold standard adoption) for structural parallels.',
    'If changes share a common cause, the composite reading reduces to a single-cause reading with multiple transmission channels — the ''overdetermination'' claim collapses. If truly independent, the kernel singularity is genuinely false and the sibling readings commit a category error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_independence, empirical, 'Whether the convergent changes are causally independent or epiphenomena of a deeper driver.').

omega_variable(
    periodization_category_error,
    'Is the composite reading''s denial of a ''unified transition'' a substantive historical claim or a category error — do historians necessarily periodize complex processes into named transitions (e.g., ''the Industrial Revolution'') without claiming causal unity?',
    'Philosophy of history analysis: examine whether ''the transition'' in sibling readings functions as a causal claim or a periodization label. Survey economic history methodology on periodization vs. causation.',
    'If ''the transition'' is merely a periodization label, the composite reading forecloses a straw man — the sibling readings may not claim causal unity at all. If it is a causal claim, the foreclosure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(periodization_category_error, conceptual, 'Whether the dispute is about historical causation or historiographic convention.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of monetary alternatives (gold standard, fixed rates, capital controls) structural (institutional lock-in, path dependence) or internalized (ideological commitment to ''modern'' central banking, epistemic capture of the economics profession)?',
    'Post-exit suppression trajectory: examine countries that attempted alternatives (Swiss gold referendum 2014, Zimbabwe dollarization, Ecuador dollarization, crypto adoption in El Salvador). If suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the economics profession and policy elites carry the suppression with them, making alternatives cognitively inaccessible even when structurally feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the monetary regime transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftm_cor_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(gftm_cor_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(gftm_cor_tr_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(gftm_cor_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.18).
narrative_ontology:measurement(gftm_cor_tr_t1973, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1973, 0.15).
narrative_ontology:measurement(gftm_cor_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(gftm_cor_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.15).

% Extraction over time
narrative_ontology:measurement(gftm_cor_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(gftm_cor_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(gftm_cor_be_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(gftm_cor_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.42).
narrative_ontology:measurement(gftm_cor_be_t1973, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1973, 0.48).
narrative_ontology:measurement(gftm_cor_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.44).
narrative_ontology:measurement(gftm_cor_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gftm_cor_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(gftm_cor_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(gftm_cor_su_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(gftm_cor_su_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1971, 0.4).
narrative_ontology:measurement(gftm_cor_su_t1973, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1973, 0.38).
narrative_ontology:measurement(gftm_cor_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.33).
narrative_ontology:measurement(gftm_cor_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form a constraint family decomposing the 'gold-fiat transition' label. The automatic reading claims a constraint type change (material → institutional) with low extraction. The creditor reading claims a geopolitical power shift with moderate extraction. This composite reading claims the label conflates multiple independent changes with heterogeneous extraction profiles. All three share the same historical referent (1960-1980 monetary regime change) but disagree on causal structure, beneficiary/victim sets, and ε. The ε-invariance principle requires separate stories because measuring 'the transition' via automatic constraint metrics vs. creditor discipline metrics vs. composite convergence metrics yields different ε values — they are different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, institutional, 0.15).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, powerful, 0.65).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
