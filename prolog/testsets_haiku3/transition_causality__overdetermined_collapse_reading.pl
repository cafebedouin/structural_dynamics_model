% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Fixed-Rate Regime Collapse (Overdetermined Causality Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the OVERDETERMINED COLLAPSE READING of
 *   the Bretton Woods transition kernel: the fixed-rate regime's collapse was
 *   not a policy choice among alternatives, but the inevitable outcome of
 *   mathematically insurmountable contradictions. The Triffin Dilemma
 *   (reserve-currency issuer cannot simultaneously maintain fixed conversion
 *   rates, provide unlimited reserves, and control domestic inflation) is
 *   authored as a mountain constraint—a natural law of monetary accounting.
 *   The reading models multiple converging causal pathways: gold reserve
 *   depletion, imported inflation in follower economies, speculative runs on
 *   dollars, and the mathematical impossibility of satisfying all constraints
 *   simultaneously. Counterfactual viability of indefinite fixed-rate
 *   maintenance is near-zero. The constraint benefits the reserve-currency
 *   issuer (through seigniorage and autonomy) and the financial orthodoxy
 *   doctrine (which is vindicated by the collapse); it victimizes all
 *   fixed-rate regime participants, especially inflation-importing developing
 *   economies.
 *
 * KEY AGENTS:
 *   - Reserve-currency issuer (US): institutional beneficiary, maintainer of the regime, but structurally trapped by the Triffin contradiction
 *   - Gold-standard creditors (allies): organized payers, holding depreciating reserves with no escape except a run that collapses the system
 *   - Fixed-rate regime participants (other developed/developing economies): identity-locked targets, forced to absorb imported inflation and surrender monetary autonomy
 *   - Speculative capital: powerful arbitrageurs profiting from the gap between official and shadow rates, accelerating the run
 *   - Financial orthodoxy establishment: beneficiary (the doctrine is vindicated by the collapse), but a non-agent entity
 *   - Excluded heterodox economists: voices warning of Triffin Dilemma, suppressed by consensus but validated by events
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.92).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.88).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Fixed-Rate Regime Collapse (Overdetermined Causality Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '08427f70-07a2-4c15-b7da-7669d1d2fcab').
narrative_ontology:cs_kernel_codification('08427f70-07a2-4c15-b7da-7669d1d2fcab', formalized).
narrative_ontology:cs_authority_grounding('08427f70-07a2-4c15-b7da-7669d1d2fcab', expertise).
narrative_ontology:cs_interpretation_layer_present('08427f70-07a2-4c15-b7da-7669d1d2fcab').
narrative_ontology:cs_reading_relation('08427f70-07a2-4c15-b7da-7669d1d2fcab', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('08427f70-07a2-4c15-b7da-7669d1d2fcab', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('08427f70-07a2-4c15-b7da-7669d1d2fcab', foundational, triffin_dilemma_mathematically_binding).
narrative_ontology:cs_axiom_status(triffin_dilemma_mathematically_binding, holdable).
narrative_ontology:cs_axiom_grounding('08427f70-07a2-4c15-b7da-7669d1d2fcab', triffin_dilemma_mathematically_binding, empirically_contingent).
narrative_ontology:cs_axiom('08427f70-07a2-4c15-b7da-7669d1d2fcab', foundational, regime_collapse_overdetermined_by_contradiction).
narrative_ontology:cs_axiom_status(regime_collapse_overdetermined_by_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('08427f70-07a2-4c15-b7da-7669d1d2fcab', regime_collapse_overdetermined_by_contradiction, empirically_contingent).
narrative_ontology:cs_reference_frame('08427f70-07a2-4c15-b7da-7669d1d2fcab', bretton_woods_mathematical_necessity).
narrative_ontology:cs_drift_state('08427f70-07a2-4c15-b7da-7669d1d2fcab', august_1971_suspension, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('08427f70-07a2-4c15-b7da-7669d1d2fcab', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, financial_orthodoxy_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, speculative_capital_markets).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, gold_standard_creditors).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_participants).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, speculative_capital_markets).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, inflation_importing_nations).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, triffin_dilemma).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, fundamental_exchange_rate_instability).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, impossible_trinity_constraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, as issuer of the reserve currency and defender of the fixed gold peg, faces a structural contradiction: maintaining the peg requires domestic financial discipline (no inflation, budget deficits constrained), but global reserve currency demand and financing of global military commitments require unlimited monetary expansion. The two demands cannot be simultaneously satisfied. By the late 1960s, US gold reserves are depleting visibly, forcing a choice between honor the peg or finance global position.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).

% Allied nations and trading partners who hold dollars as reserves are nominally protected by the US commitment to convert dollars to gold at $35 per ounce. As the dollar's true value erodes (US inflation visible, gold reserve adequacy questioned), these actors face a dilemma: hold depreciating reserves or attempt redemption and trigger a run on US gold. Collective action is impossible—each actor's optimal move is to redeem first, but collective redemption is the collapse scenario.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, gold_standard_creditors, payer,
    organized, biographical, trapped, global).

% Other developed and developing economies commit to pegging their currencies to the dollar (which is pegged to gold). They surrender monetary policy autonomy to maintain the peg. As US inflation exports to their economies through fixed exchange rates, they face imported inflation without the ability to devalue or tighten unilaterally. Their institutional identities are tied to Bretton Woods participation; exit is ideologically and institutionally costly.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_participants, payer,
    moderate, biographical, identity_locked, global).

% Financial actors profit from arbitrage opportunities as the gap between official and shadow market exchange rates widens. They also carry the risk that the peg's collapse triggers massive revaluation. Their behavior accelerates the run on gold by moving capital in anticipation of devaluation.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, speculative_capital_markets, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, speculative_capital_markets, payer).

% Developing economies and smaller trading partners are forced to accept US-exported inflation while absorbing the real resource costs of maintaining fixed-rate commitments. They have no unilateral exit; collective action to renegotiate the peg fails because the hegemon dictates terms.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, inflation_importing_nations, payer,
    powerless, biographical, trapped, global).

% The doctrine of sound money, fixed exchange rates, and balanced budgets is vindicated by the regime's structure. The collapse vindicates the doctrine's warnings about unsustainable contradictions, not its prescriptions for prevention.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, financial_orthodoxy_establishment, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(transition_causality__overdetermined_collapse_reading, financial_orthodoxy_establishment).

% Economists questioning the fixed-rate framework (post-Keynesians, institutionalists) are excluded from policy conversations dominated by orthodox consensus. Their warnings about the Triffin Dilemma and impossible trinity are treated as fringe concerns until the collapse itself validates them.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, academic_heterodox_economists, excluded,
    moderate, biographical, constrained, national).

% US policymakers (Treasury, Federal Reserve) observe the structural bind they occupy: the system is mathematically unsustainable, but the political commitment to the peg prevents orderly unwinding. The eventual choice to suspend convertibility is not a choice between alternatives, but capitulation to a mathematically determined outcome.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, hegemon_policymakers, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__overdetermined_collapse_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(transition_causality__overdetermined_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a single numeraire (dollar-gold peg at $35/oz) enabling stable exchange rates, predictable trade, and multilateral commerce after wartime chaos.
% TRANSFER_FUNCTION: Transfers monetary policy autonomy from all participants to the reserve-currency issuer; transfers purchasing power from inflation-importing economies and gold-standard creditors to the issuer through seigniorage and imported inflation.
% ABSENT_VOICES: Alternative monetary frameworks (managed floats, plural reserve currencies, commodity-basket standards) are excluded from serious policy consideration. Developing economies forced into fixed-rate pegs have no voice in rate-setting. Heterodox economists (Triffin, Minsky, Friedman) warning of the dilemma are suppressed by consensus.
% DISAPPEARANCE_RATIONALE: The regime's collapse was followed by floating exchange rates, independent monetary policies, and a multi-currency reserve system. The regime was not natural equilibrium but constructed institutional arrangement; its disappearance forced fundamental reorganization of international monetary coordination.
% FOUNDING_PROBLEM: Post-war currency chaos: devaluations, trade collapse, lack of convertibility. Bretton Woods designed to restore stable exchange rates and multilateral trade.
% FOUNDING_PROBLEM_CORROBORATION: By late 1960s, the founding problem was substantially solved by the regime itself—two decades of stable trade growth, currency convertibility restored. Regime persisted because beneficiary captured asymmetric gains, not because problem remained live. Economists (Triffin, 1960; Minsky, 1966; Friedman, 1968) identified unsustainability empirically; collapse vindicated structural analysis.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.65 to 0.92 over the interval because the contradiction's mathematical severity becomes undeniable: US inflation (visible by mid-1960s) erodes the real value of foreign-held reserves; gold depletion (from 21,000 tonnes in 1949 to 8,133 tonnes by 1971) collapses the credibility of the peg; the foreign-exchange deficit (cumulative $30+ billion by 1971) makes redemption inevitable for any creditor attempting it. By endpoint (t=30, modeling the months before August 1971), extractiveness saturates at 0.92 because the constraint is mathematically binding: there is no policy adjustment that preserves all three objectives (fixed rate, unlimited reserves, domestic price stability). Suppression rises similarly (0.58 to 0.88) because maintaining the regime increasingly requires active defense against speculation, capital controls, and discipline imposed on follower economies. Theater ratio remains low (0.08 to 0.12) because the regime's coordination function (stable exchange rates, trade facilitation) is real; the theater is minimal—the constraint operates as structural mathematics, not as performance or rhetoric. Accessibility collapse is very high (0.91) because once economists and creditors understand the Triffin Dilemma, alternatives are structurally foreclosed: the math does not permit indefinite fixed rates with unlimited reserves. Resistance is high (0.73) because follower economies, creditors, and heterodox economists all push against the regime, but the hegemon's power (institutional, financial, military) suppresses exit and renegotiation until the collapse becomes inevitable.
 *
 * PERSPECTIVAL GAP:
 *   The reserve-currency issuer and orthodoxy establishment author this as inevitable structural breakdown (exonerating policymakers: 'the math was impossible'). Follower economies and creditors experience it as imposed collapse by a hegemon that chose seigniorage over honor. Heterodox economists experience vindication of long-suppressed warnings. The engine computes divergence from the structural data: the regime's high extractiveness and suppression should yield different type-classifications for the payer and beneficiary seats. From the beneficiary seat (reserve issuer), the constraint is a mountain—natural law of monetary mathematics. From the payer seats (follower economies, creditors), it is a snare—extractive, suppressed, with victims clearly identified.
 *
 * DIRECTIONALITY LOGIC:
 *   Reserve-currency issuer (d ≈ 0.0, full beneficiary): collects seigniorage, maintains monetary autonomy, exerts structural power through the peg rate and reserve issuance. Gold-standard creditors (d ≈ 0.95, near-target): hold depreciating reserves, have no unilateral exit, are trapped in a prisoner's-dilemma run scenario. Fixed-rate regime participants (d ≈ 0.90, target): forced to accept imported inflation, surrender monetary policy, identity-locked to Bretton Woods participation. Speculative capital (d ≈ 0.40, symmetric with upside): profit from arbitrage but carry collapse risk. Heterodox economists (d ≈ 0.75, analytical near-target): structurally excluded from policy voice, vindicated but not compensated by the collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (currency chaos) is genuinely dead by the late 1960s: the regime itself had solved it—trade is stable, currencies are convertible, commerce flows predictably. The regime persists and intensifies (extractiveness rising 0.65→0.92) precisely because the problem is solved and the beneficiary captures all surplus. This is mandatrophy: the legitimating justification has outlived its function. The regime's collapse is classified as mountain (inevitable contradiction) under this reading, but mandatrophy resolution is exactly the mechanism: legitimacy exhausted, beneficiary unwilling to release the arrangement, mathematical contradiction becomes binding. The constraint does not persist through choice or consensus—it persists through structural math until the math breaks the institutional capacity to maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_regime_extension,
    'Could the Bretton Woods fixed-rate regime have been extended indefinitely with different policy choices (larger US fiscal discipline, earlier revaluation of gold, capital controls on speculative outflows)?',
    'Historical modeling: simulate the US balance-of-payments constraint with different assumptions about inflation, gold sterilization, and capital account restrictions. Compare to actual trajectory.',
    'If counterfactual extension is mathematically viable with achievable policies, the regime is Tangled Rope (coordination with asymmetric extraction, but not mathematically inevitable collapse) under hybrid_trigger reading. If counterfactual extension is non-viable even with extreme policies, overdetermined_collapse reading (this one) stands as a mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_regime_extension, empirical, 'Whether the fixed-rate regime''s collapse was mathematically determined or contingent on policy choices.').

omega_variable(
    natural_law_vs_institutional_design,
    'Is the Triffin Dilemma a natural law of monetary arithmetic (no reserve-currency standard can simultaneously maintain fixed conversion rates, unlimited reserve supply, and price stability), or an artifact of the specific Bretton Woods institutional design?',
    'Comparison to other historical reserve systems (sterling standard, 19th-century gold standard under different central bank coordination rules). If the dilemma recurs across different institutional designs, it is a natural law. If it is specific to Bretton Woods structure, it is institutional contradiction, not mountain.',
    'If natural law: mountain classification stands. If institutional-design artifact: the constraint is Tangled Rope or Snare depending on whether reform was possible with cooperation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_design, conceptual, 'The structural origin of the Triffin Dilemma: natural law or institutional contingency.').

omega_variable(
    beneficiary_false_summit_candidate,
    'Is the reserve-currency issuer genuinely benefiting from the fixed-rate regime in the long run, or does the eventual collapse vindicate the regime''s critics, making the issuer a false summit that falsely claims natural-law status to justify extraction?',
    'Cost-benefit analysis of the US position 1945–1971: seigniorage benefits, military-support financing, and trade advantages vs. gold depletion, imported inflation during Vietnam War, and loss of monetary autonomy to gold-convertibility obligation. Compare to counterfactual floating-rate scenario.',
    'If long-run benefits are positive, beneficiary classification stands and the regime is extractive but beneficial to the hegemon. If long-run costs exceed benefits (credibility loss, gold depletion, eventual forced exit), the beneficiary is a false summit: the regime extracts from followers and the issuer, vindicated by its collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_false_summit_candidate, empirical, 'Whether the reserve-currency issuer is genuinely a beneficiary or a trapped actor misclassified as beneficiary.').

omega_variable(
    suppression_mechanism_structural_vs_hegemonic,
    'Is the regime''s suppression mechanism structural (mathematical: the fixed rate makes devaluation impossible for follower nations) or hegemonic (political: the US uses its power to prevent exit and renegotiation)?',
    'Examine instances of attempted exit or renegotiation (France''s gold buying, Britain''s devaluation attempts, developing-nation currency crises): were they blocked by math or by US pressure?',
    'If structural suppression dominates, the constraint is a mountain of arithmetic. If hegemonic suppression dominates, the constraint is a snare maintained by coercion, and classification would shift toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_hegemonic, empirical, 'The nature of suppression: mathematical inevitability or political coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transition_overdetermined_tr_t0, transition_causality__overdetermined_collapse_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(transition_overdetermined_tr_t0, observed).
narrative_ontology:measurement(transition_overdetermined_tr_t5, transition_causality__overdetermined_collapse_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(transition_overdetermined_tr_t5, observed).
narrative_ontology:measurement(transition_overdetermined_tr_t10, transition_causality__overdetermined_collapse_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(transition_overdetermined_tr_t10, observed).
narrative_ontology:measurement(transition_overdetermined_tr_t15, transition_causality__overdetermined_collapse_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(transition_overdetermined_tr_t15, observed).
narrative_ontology:measurement(transition_overdetermined_tr_t20, transition_causality__overdetermined_collapse_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(transition_overdetermined_tr_t20, observed).
narrative_ontology:measurement(transition_overdetermined_tr_t25, transition_causality__overdetermined_collapse_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(transition_overdetermined_tr_t25, observed).
narrative_ontology:measurement(transition_overdetermined_tr_t30, transition_causality__overdetermined_collapse_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(transition_overdetermined_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(transition_overdetermined_be_t0, transition_causality__overdetermined_collapse_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(transition_overdetermined_be_t0, observed).
narrative_ontology:measurement(transition_overdetermined_be_t5, transition_causality__overdetermined_collapse_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(transition_overdetermined_be_t5, observed).
narrative_ontology:measurement(transition_overdetermined_be_t10, transition_causality__overdetermined_collapse_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement_basis(transition_overdetermined_be_t10, observed).
narrative_ontology:measurement(transition_overdetermined_be_t15, transition_causality__overdetermined_collapse_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement_basis(transition_overdetermined_be_t15, observed).
narrative_ontology:measurement(transition_overdetermined_be_t20, transition_causality__overdetermined_collapse_reading, base_extractiveness, 20, 0.89).
narrative_ontology:measurement_basis(transition_overdetermined_be_t20, observed).
narrative_ontology:measurement(transition_overdetermined_be_t25, transition_causality__overdetermined_collapse_reading, base_extractiveness, 25, 0.92).
narrative_ontology:measurement_basis(transition_overdetermined_be_t25, observed).
narrative_ontology:measurement(transition_overdetermined_be_t30, transition_causality__overdetermined_collapse_reading, base_extractiveness, 30, 0.92).
narrative_ontology:measurement_basis(transition_overdetermined_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(transition_overdetermined_su_t0, transition_causality__overdetermined_collapse_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(transition_overdetermined_su_t0, observed).
narrative_ontology:measurement(transition_overdetermined_su_t5, transition_causality__overdetermined_collapse_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(transition_overdetermined_su_t5, observed).
narrative_ontology:measurement(transition_overdetermined_su_t10, transition_causality__overdetermined_collapse_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(transition_overdetermined_su_t10, observed).
narrative_ontology:measurement(transition_overdetermined_su_t15, transition_causality__overdetermined_collapse_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement_basis(transition_overdetermined_su_t15, observed).
narrative_ontology:measurement(transition_overdetermined_su_t20, transition_causality__overdetermined_collapse_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement_basis(transition_overdetermined_su_t20, observed).
narrative_ontology:measurement(transition_overdetermined_su_t25, transition_causality__overdetermined_collapse_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement_basis(transition_overdetermined_su_t25, observed).
narrative_ontology:measurement(transition_overdetermined_su_t30, transition_causality__overdetermined_collapse_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement_basis(transition_overdetermined_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__overdetermined_collapse_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, triffin_dilemma_mountain).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, impossible_trinity_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bretton Woods transition kernel, modeling overdetermined collapse causality. Sibling readings (contingent_choice_reading and hybrid_trigger_reading) model the same transition but emphasize policy contingency and trigger-event dependence respectively. All three share the empirical referent (August 1971 suspension of convertibility) but diverge on whether the collapse was inevitable, chosen, or trigger-contingent. The Triffin Dilemma constraint (triffin_dilemma_mountain) is the mathematical foundation underlying this reading's inevitability claim; the Impossible Trinity (impossible_trinity_constraint) is a complementary structural contradiction. This reading forecloses the contingent_choice_reading if the mathematical case is tight (one framework cannot hold both 'inevitable' and 'chosen'); coexists_with hybrid_trigger_reading because triggers and overdetermination are compatible (multiple causes + contingent actualization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__overdetermined_collapse_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
