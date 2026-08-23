% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Bretton Woods Gold Convertibility Obligation (Triffin Structural Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   The Bretton Woods gold convertibility obligation (Article IV, Section 4
 *   of the IMF Articles of Agreement) required the United States to convert
 *   foreign-held dollars into gold at $35 per ounce. The Triffin structural
 *   reading argues this design contains an irreparable contradiction: to
 *   supply the world with dollar liquidity for growing trade, the US must run
 *   persistent balance-of-payments deficits, which inevitably drain its gold
 *   reserves and destroy the credibility of the conversion promise. Both the
 *   US (gold loss, policy constraint) and creditor nations (dollar
 *   accumulation, conversion risk) are structural victims of this trilemma.
 *   The genuine coordination beneficiaries are international traders,
 *   corporations, and financial institutions. The system requires active
 *   enforcement (Gold Pool, IMF surveillance, central bank swap lines) which
 *   intensifies over time. The post-1971 floating regime is the structural
 *   beneficiary of the collapse — it resolves the contradiction by
 *   eliminating the convertibility constraint. This reading does not describe
 *   the legal obligation alone (strict reading) nor the policy flexibility
 *   view; it identifies the structural impossibility as the constraint's
 *   defining feature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.65).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Bretton Woods Gold Convertibility Obligation (Triffin Structural Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, 'db595007-1afc-4cf0-9a17-121b882aae2e').
narrative_ontology:cs_kernel_codification('db595007-1afc-4cf0-9a17-121b882aae2e', formalized).
narrative_ontology:cs_authority_grounding('db595007-1afc-4cf0-9a17-121b882aae2e', lineage).
narrative_ontology:cs_interpretation_layer_present('db595007-1afc-4cf0-9a17-121b882aae2e').
narrative_ontology:cs_reading_relation('db595007-1afc-4cf0-9a17-121b882aae2e', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('db595007-1afc-4cf0-9a17-121b882aae2e', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_axiom('db595007-1afc-4cf0-9a17-121b882aae2e', foundational, triffin_dilemma_structural).
narrative_ontology:cs_axiom_status(triffin_dilemma_structural, holdable).
narrative_ontology:cs_axiom_grounding('db595007-1afc-4cf0-9a17-121b882aae2e', triffin_dilemma_structural, empirically_contingent).
narrative_ontology:cs_axiom('db595007-1afc-4cf0-9a17-121b882aae2e', foundational, systemic_revision_necessary).
narrative_ontology:cs_axiom_status(systemic_revision_necessary, holdable).
narrative_ontology:cs_axiom_grounding('db595007-1afc-4cf0-9a17-121b882aae2e', systemic_revision_necessary, instrumental).
narrative_ontology:cs_reference_frame('db595007-1afc-4cf0-9a17-121b882aae2e', bretton_woods_adjustable_peg_design).
narrative_ontology:cs_drift_state('db595007-1afc-4cf0-9a17-121b882aae2e', pre_nixon_shock_1971, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db595007-1afc-4cf0-9a17-121b882aae2e', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, international_traders).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, global_financial_institutions).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury_fed).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_central_banks).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, triffin_dilemma).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, impossible_trinity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the reserve currency and bears the legal obligation to convert dollars to gold at $35/oz. Must run balance-of-payments deficits to supply global liquidity, draining gold reserves and constraining domestic monetary policy. The more dollars supplied for world trade, the less credible convertibility becomes. Exit means abandoning the reserve currency role — a constrained choice with massive systemic consequences.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury_fed, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury_fed, agenda_setter).

% Accumulate dollar reserves from trade surpluses. Face a choice: hold dollars (earning low return, bearing conversion risk) or convert to gold (draining US reserves, triggering systemic crisis). Their export-led growth models depend on the system, but the system makes their reserve holdings structurally unsafe. Exit means revaluing currencies or imposing capital controls — constrained by trade dependencies.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_central_banks, payer,
    organized, biographical, constrained, global).

% Benefit from stable exchange rates, predictable pricing, and reliable dollar settlement for cross-border trade. The convertibility anchor reduces transaction costs and exchange risk. They do not administer the system and bear no direct cost of its maintenance. Exit is mobile — they can shift invoicing currencies or use forward markets, though at higher cost.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_traders, beneficiary,
    organized, biographical, mobile, global).

% Operate globally with stable dollar-denominated costs and revenues. The fixed-rate system enables long-term investment planning and intra-firm transfer pricing. They collect the coordination benefit without bearing the gold-convertibility cost. Exit is mobile — they can hedge or relocate production, but the system's stability is a significant operational subsidy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, multinational_corporations, beneficiary,
    powerful, biographical, mobile, global).

% IMF, BIS, and major commercial banks administer and profit from the dollar-centered system. The IMF surveils exchange rates and provides standby facilities; the BIS coordinates central bank gold operations (London Gold Pool); commercial banks intermediate Eurodollar markets. They collect fees, seigniorage-like returns, and institutional authority. Their exit is arbitrage-grade — they adapt to any monetary regime.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, global_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, global_financial_institutions, agenda_setter).

% The post-1971 flexible exchange rate regime that emerges from the collapse. It is not a participant in the Bretton Woods period but is the structural beneficiary of the system's failure — the Triffin reading identifies the floating regime as the arrangement that resolves the structural contradiction by removing the convertibility constraint. Listed as non-agent to mark its counterfactual/future status in this reading.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bw_floating_regime, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, post_bw_floating_regime).

% Group of central banks (US, UK, Germany, France, Italy, Belgium, Netherlands, Switzerland) that cooperated 1961-1968 to defend the $35/oz gold price by pooling gold reserves. They actively enforce the convertibility constraint through market intervention. The Pool's collapse in 1968 marks the failure of active enforcement. Exit is constrained — leaving the Pool accelerates the crisis.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, gold_pool_participants, agenda_setter,
    institutional, biographical, constrained, global).

% Hold minimal reserves, depend on dollar-denominated trade and aid, have no voice in G-10 / IMF governance of the system. Bear the adjustment costs of US monetary policy transmitted through the dollar standard. Would object to the asymmetric burden but are structurally excluded from the negotiation. Exit is trapped — no alternative monetary framework accessible.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, developing_nations, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, universally accepted anchor for international payments: fixed exchange rates against the dollar, dollar convertible to gold, enabling predictable cross-border trade and investment without bilateral clearing arrangements.
% TRANSFER_FUNCTION: Moves real resources and policy autonomy from both the reserve currency issuer (US gold reserves, monetary sovereignty) and surplus nations (safe asset accumulation, export competitiveness) to the global trading and financial system (stable prices, low transaction costs). The transfer is symmetric extraction — both center and periphery pay for the coordination.
% ABSENT_VOICES: Developing nations and non-G10 creditors are excluded from the governance of the gold pool and IMF surveillance decisions. They would object to bearing adjustment costs without representation. The post-BW floating regime is absent as a future state — its 'voice' is the structural resolution that the current participants resist.
% DISAPPEARANCE_RATIONALE: If the convertibility obligation vanished overnight (as it effectively did in August 1971), the fixed exchange rate system would collapse, the dollar would float, gold would demonetize, and the international monetary system would reorganize around flexible rates and fiat money. The world rearranged — the Nixon shock was the discrete event.
% FOUNDING_PROBLEM: Post-war reconstruction required a stable international monetary system that avoided the competitive devaluations and trade collapse of the 1930s. The Bretton Woods design aimed to combine fixed exchange rates (for trade stability) with capital controls (for policy autonomy) and a gold anchor (for confidence).
% FOUNDING_PROBLEM_CORROBORATION: Triffin (1960) and subsequent monetary historians (Eichengreen, Bordo, James) attest the founding problem was real but the design contained a structural contradiction: the gold anchor required US deficits for liquidity, which undermined the anchor. The IMF's own official history acknowledges the system's 'inherent instability.' The US Treasury and Federal Reserve at the time contested the structural reading, insisting the problem was manageable through policy coordination (Gold Pool, swap lines, SDRs).
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.25 to 0.78 over the interval as the structural contradiction matures: early years the system works as coordination; by the 1960s the gold drain and dollar overhang make the constraint extractive from both center and periphery. Suppression requirement rises as enforcement machinery (Gold Pool, swap lines, capital controls) must expand to maintain the peg. Theater ratio rises as official rhetoric emphasizes 'confidence' and 'cooperation' while the structural gap widens — the 1968 two-tier gold market is a theatrical compromise. Accessibility collapse is moderate (0.62): alternatives (floating rates, SDRs, capital controls) exist but are politically blocked until collapse. Resistance is moderate (0.58): creditor nations resist conversion pressure (France 1965), US resists adjustment, but the constraint holds until the Nixon shock.
 *
 * PERSPECTIVAL GAP:
 *   From the US seat, the constraint appears as a voluntary leadership burden (Triffin's 'benevolent hegemony' framing) — but the structural reading shows the US is a victim of its own reserve currency role. From creditor nations' seats, the constraint appears as dollar privilege for the US — but they are equally victims of the symmetric extraction. From traders/MNCs' seats, it is pure coordination benefit. The engine computes per-seat types from these structural asymmetries: the same constraint is snare-like for US and creditors, rope-like for traders, piton-like for financial institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   US Treasury/Fed is a dual-positioned actor: agenda setter (sets dollar policy, leads Gold Pool) AND payer (bears gold drain, policy constraint). Directionality derives high d from victim role (extraction target) despite institutional power. Creditor nations are payers with organized power but constrained exit — their surplus strategy traps them in dollar accumulation. International traders and MNCs are beneficiaries with mobile exit — they capture coordination gains without bearing convertibility costs. Global financial institutions are beneficiaries with arbitrage exit — they administer and profit, adapting to any regime. The post-BW floating regime is a non-agent future beneficiary. Developing nations are excluded and trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war monetary stability) was real but the design contained a structural time bomb. The mandate did not atrophy — it became impossible. The system persisted 27 years through active enforcement (Gold Pool, swap lines, capital controls) that masked the structural contradiction. The mandatrophy is not 'mission accomplished' but 'mission impossible' — the constraint extracts from its administrators and participants alike until collapse. The floating regime that follows is not a planned successor but the structural resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Triffin structural reading a distinct constraint from the strict legal and policy-flexible readings of the same kernel, or a different analytical lens on the same constraint?',
    'Compare epsilon values and beneficiary/victim structures across the three readings. If epsilon differs materially (Triffin: high extractivity from both US and creditors; Strict: low extractivity, legal compliance; Flexible: variable extractivity), they are distinct constraints per epsilon-invariance.',
    'If distinct, each reading gets its own constraint story with independent classification. If same constraint, the epsilon variance signals measurement error or observable-dependence (violating epsilon-invariance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three kernel readings instantiate three structurally distinct constraints or one constraint with three analytical perspectives.').

omega_variable(
    post_bw_regime_as_beneficiary,
    'Can a future regime (post-1971 floating rates) be a structural beneficiary of a constraint that operated 1944-1971, when it did not exist as an actor during the constraint''s operation?',
    'Test whether the constraint''s extraction dynamics structurally produce the successor regime as a functional outcome. If the collapse predictably generates the floating regime, the successor is a structural beneficiary in the Triffin reading''s causal model.',
    'If yes, the beneficiary set includes a non-contemporaneous entity — unusual but structurally coherent for a ''design flaw requiring revision'' reading. If no, the beneficiary set shrinks to contemporaneous actors only, changing the extraction calculus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_bw_regime_as_beneficiary, conceptual, 'Whether the post-Bretton Woods floating regime qualifies as a beneficiary in the structural reading''s causal model.').

omega_variable(
    symmetric_victim_structure,
    'Are the US and creditor nations symmetrically victimized by the trilemma, or does the asymmetry of reserve currency privilege make the US a net beneficiary despite gold losses?',
    'Quantify seigniorage gains vs. gold losses for US; quantify reserve accumulation benefits vs. conversion losses for creditors. Compare net positions.',
    'If US is net beneficiary, the victim set shrinks to creditors only, changing the constraint toward snare (asymmetric extraction). If symmetric, tangled_rope holds with dual victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetric_victim_structure, empirical, 'Whether the Triffin dilemma''s extraction falls symmetrically on both center and periphery or asymmetrically on creditors only.').

omega_variable(
    coordination_function_genuineness,
    'Was the Bretton Woods coordination function (stable rates for trade) genuine and valuable, or was it always a cover for dollar hegemony?',
    'Measure trade growth, investment stability, and welfare gains during the fixed-rate period vs. counterfactual (floating rates with 1940s-50s capital controls). Compare to the Gold Standard era.',
    'If genuine, tangled_rope (coordination + extraction) holds. If cover, snare (pure extraction) — the coordination story was always pretext.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuineness, empirical, 'Whether the coordination function was real (making this tangled_rope) or pretextual (making this snare).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_triffin_tr_t1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1944, 0.15).
narrative_ontology:measurement(dgc_triffin_tr_t1950, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(dgc_triffin_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.25).
narrative_ontology:measurement(dgc_triffin_tr_t1960, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1960, 0.32).
narrative_ontology:measurement(dgc_triffin_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.38).
narrative_ontology:measurement(dgc_triffin_tr_t1968, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1968, 0.41).
narrative_ontology:measurement(dgc_triffin_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.42).

% Extraction over time
narrative_ontology:measurement(dgc_triffin_be_t1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1944, 0.25).
narrative_ontology:measurement(dgc_triffin_be_t1950, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(dgc_triffin_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.48).
narrative_ontology:measurement(dgc_triffin_be_t1960, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(dgc_triffin_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.71).
narrative_ontology:measurement(dgc_triffin_be_t1968, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1968, 0.76).
narrative_ontology:measurement(dgc_triffin_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dgc_triffin_su_t1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1944, 0.3).
narrative_ontology:measurement(dgc_triffin_su_t1950, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(dgc_triffin_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.45).
narrative_ontology:measurement(dgc_triffin_su_t1960, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement(dgc_triffin_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement(dgc_triffin_su_t1968, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1968, 0.65).
narrative_ontology:measurement(dgc_triffin_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__triffin_structural_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, eurodollar_market_emergence).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, special_drawing_rights_creation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, nixon_shock_1971).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, jamaica_agreement_1976).

% DUAL FORMULATION NOTE:
% This is the Triffin structural reading of the dollar_gold_convertibility kernel. It decomposes the kernel into a constraint with high symmetric extractiveness (both US and creditors as victims) and a post-collapse beneficiary (floating regime). The strict reading sees low extractiveness (legal obligation); the flexible reading sees variable extractiveness (policy choice). The three readings form a constraint family linked by affects_constraints. The epsilon values differ: Triffin ~0.78 (structural impossibility), Strict ~0.15 (legal compliance cost), Flexible ~0.45 (policy management cost).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, institutional, 0.75).
constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
