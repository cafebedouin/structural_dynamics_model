% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Gold-Dollar Anchor as Sovereignty Defense Against External Discipline
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the 'sovereignty_defense' reading of the Bretton
 *   Woods kernel: the system is read as constraining EXTERNAL monetary
 *   discipline (speculative capital flight, gold-standard-style automatic
 *   adjustment) in order to preserve member states' national monetary
 *   sovereignty. Under this reading's own lights, the arrangement began as
 *   genuine coordination — it let states pursue domestic stabilization goals
 *   the interwar gold standard had punished — but the same architecture
 *   structurally routed the benefit of that latitude disproportionately to
 *   the reserve-currency issuer. The U.S. enters the beneficiary set via what
 *   came to be called exorbitant privilege: it alone could run deficits
 *   settled in its own currency, while non-reserve states' 'preserved
 *   sovereignty' was bounded by peg defense obligations they did not set. The
 *   gold anchor, read this way, functions less as a stabilizer of last resort
 *   and more as a lever periphery states had to defend on the center's
 *   behalf. This is a DIFFERENT constraint from the sibling readings, not
 *   another view of the same one: the keynesian_embedded_liberalism reading
 *   centers the constraint on international CAPITAL (protecting policy space
 *   from capital flows) with different beneficiary/victim framing (domestic
 *   labor and industry as beneficiaries, footloose capital as the constrained
 *   party); the neoliberal_convertibility reading centers the constraint on
 *   GOVERNMENT INTERVENTION (enabling free capital markets), with government
 *   discretion itself as the thing constrained and convertibility advocates
 *   as beneficiaries. All three share the treaty substrate but diverge in
 *   what is claimed to be constrained and who is claimed to benefit — hence
 *   three separate ε values and three separate stories, linked by network
 *   edges rather than folded into one.
 *
 * KEY AGENTS:
 *   - united_states_treasury: primary beneficiary (institutional/arbitrage) — exorbitant privilege from reserve-currency status
 *   - united_states_multinational_firms: secondary beneficiary (powerful/mobile) — dollar-denominated global operations without adjustment burden
 *   - non_reserve_currency_states: primary target (moderate/constrained) — peg defense as the price of formal sovereignty
 *   - developing_world_import_sectors: diffuse victim (powerless/trapped) — bears devaluation and import-compression shocks
 *   - peripheral_central_banks: administering victim (organized/constrained) — runs the peg defense mechanism at direct cost
 *   - monetary_historians: analytical observer — traces the asymmetry across the system's full life
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.6).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Gold-Dollar Anchor as Sovereignty Defense Against External Discipline").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '9ece48b0-cafc-4f8b-9876-e2d5f0c9104b').
narrative_ontology:cs_kernel_codification('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', formalized).
narrative_ontology:cs_authority_grounding('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', extraction).
narrative_ontology:cs_interpretation_layer_present('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b').
narrative_ontology:cs_reading_relation('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', foundational, national_monetary_policy_autonomy_is_the_protected_good).
narrative_ontology:cs_axiom_status(national_monetary_policy_autonomy_is_the_protected_good, holdable).
narrative_ontology:cs_axiom_grounding('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', national_monetary_policy_autonomy_is_the_protected_good, conventional).
narrative_ontology:cs_axiom('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', secondary, reserve_currency_symmetry_is_achievable_within_the_peg_system).
narrative_ontology:cs_axiom_status(reserve_currency_symmetry_is_achievable_within_the_peg_system, overridden).
narrative_ontology:cs_axiom_grounding('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', reserve_currency_symmetry_is_achievable_within_the_peg_system, empirically_contingent).
narrative_ontology:cs_reference_frame('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', adjustable_peg_sovereignty_compact).
narrative_ontology:cs_drift_state('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', post_triffin_dilemma_recognition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9ece48b0-cafc-4f8b-9876-e2d5f0c9104b', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_multinational_firms).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_world_import_sectors).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_central_banks).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, national_monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, adjustable_peg_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the reserve currency other states must hold and settle in; can run balance-of-payments deficits financed by foreign central banks accumulating dollars rather than gold, and can adjust domestic monetary policy for domestic ends with limited external discipline. The dollar-gold link is nominally a constraint on the U.S. but in practice the U.S. sets the terms others must adapt to.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury, agenda_setter).

% Borrow, invest, and price internationally in a currency their home government issues, absorbing none of the adjustment costs that fall on firms operating in non-reserve currencies. Benefit from dollar stability without bearing the sovereignty-preserving adjustment burden other states must accept.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_multinational_firms, beneficiary,
    powerful, generational, mobile, global).

% Peg their currencies to the dollar and must defend the peg through capital controls, interest rate adjustments, or IMF-monitored devaluation. Their claimed 'sovereignty' over domestic monetary policy is real relative to unrestricted capital flows, but their actual room to run independent policy is bounded by the need to maintain dollar reserves and avoid a destabilizing devaluation.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, biographical, constrained, national).

% Depend on foreign exchange availability set by the peg and reserve position of their national central bank. When reserves run short, import compression and devaluation-driven price shocks fall directly on them, with no seat in the negotiations that set the peg's terms.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_world_import_sectors, payer,
    powerless, biographical, trapped, national).

% Administer the peg domestically, accumulate dollar reserves as the mechanism for defending it, and bear the direct cost of intervention when confidence in the peg wavers. They set domestic policy day to day but within limits set by the dollar-gold arrangement they did not design.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_central_banks, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_central_banks, agenda_setter).

% Administers adjustable pegs, extends conditional financing, and monitors compliance. Frames the system as protecting members' monetary sovereignty against unrestricted capital flows and speculative attack, while in practice its surveillance function disciplines peripheral policy choices more than it disciplines the reserve issuer.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_gold_convertibility_regime, agenda_setter,
    institutional, civilizational, analytical, global).

% European central banks that pooled gold reserves to defend the dollar price of gold in the 1960s absorbed the cost of maintaining U.S. credibility without a formal seat in setting U.S. domestic monetary or fiscal policy, the actual driver of the strain on the gold price.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, gold_pool_central_banks, excluded,
    organized, generational, trapped, continental).

% Assess the system's operation across its full life (1944–1971), tracing how the sovereignty-preserving justification coexisted with an asymmetric structure that privileged the reserve issuer.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides non-reserve-currency states an adjustable peg and capital-control latitude so they can pursue domestic full-employment and reconstruction policy without immediate destabilizing capital flight — the sovereignty the Bretton Woods architects (especially White and, more ambivalently, Keynes) wanted member states to retain against 1920s-style gold-standard discipline.
% TRANSFER_FUNCTION: Moves seigniorage-like benefit and policy latitude toward the reserve-currency issuer (the United States), and moves adjustment costs (reserve depletion, import compression, IMF conditionality) toward non-reserve-currency states and their weakest-positioned residents.
% ABSENT_VOICES: Colonial and newly-independent developing states had minimal voice at the 1944 Bretton Woods negotiations themselves; their populations bearing later devaluation and import-compression costs were not represented in the design of the peg mechanics that would govern them.
% DISAPPEARANCE_RATIONALE: If the dollar-gold anchor and its adjustable-peg architecture vanished, non-reserve states would face immediate exchange-rate uncertainty without the peg's coordination function, but would also be freed from defending a peg calibrated to U.S. domestic conditions; the U.S. would lose the reserve-currency latitude that let it run deficits financed by foreign accumulation rather than domestic adjustment. Both the coordination benefit and the asymmetric extraction would end together — which is exactly why the sovereignty-defense reading and the exorbitant-privilege critique describe the same event from different seats.
% FOUNDING_PROBLEM: The interwar collapse of the gold standard produced competitive devaluations, capital flight, and beggar-thy-neighbor policies that destroyed trade and deepened the Depression; architects sought a system letting states pursue domestic stabilization without triggering the destabilizing capital flows that had punished such choices under the classical gold standard.
% FOUNDING_PROBLEM_CORROBORATION: IMF Article IV historical retrospectives and post-1971 economic historians (e.g., work following Triffin's dilemma analysis) attest the coordination problem the peg solved was real for a period but that reserve-currency asymmetry was present from the founding rather than emerging later — a reading corroborated by contemporary European finance ministries' 1960s complaints about 'exorbitant privilege,' a phrase attributed to French officials outside the U.S. Treasury that benefited from the arrangement.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.42 to 0.68) as Triffin-dilemma dynamics matured: early Bretton Woods (1944-1950s) delivered genuine coordination value relative to interwar chaos, but as U.S. deficits accumulated and dollar overhang grew relative to gold reserves, the system's function shifted from mutual stabilization toward one-sided seigniorage extraction, culminating in the pressures that produced the 1971 Nixon shock. Suppression (0.6) reflects the real coercive weight of IMF conditionality and peg-defense obligations on non-reserve states, but is not maximal because exit via devaluation, however costly, remained a formally available (if punished) option — this is not a trapped snare, it is a tangled rope with real coordination function underneath the extraction. Theater ratio rises moderately (0.2 to 0.4) as gold-pool defense operations in the 1960s became increasingly performative relative to their capacity to actually hold the dollar price of gold.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States sits nearest the beneficiary end: its 'constraint' (dollar convertibility to gold at $35/oz) was nominal while its actual latitude to run deficits financed by foreign reserve accumulation was real and growing. Non-reserve states sit nearer the target end: their formal sovereignty (freedom from destabilizing hot-money flows) came bundled with an obligation to defend a peg calibrated to conditions set in Washington. Peripheral central banks occupy a dual position — they administer the defense (agenda_setter-adjacent) but bear its costs (payer) — captured via the secondary_role field rather than an override, since the derivation from beneficiary/victim + exit options already produces a directionality near the target end appropriately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar beggar-thy-neighbor devaluation and capital flight — was substantially resolved by the mid-1950s; the system's continuation through the 1960s under conditions of mounting dollar overhang increasingly served the reserve-issuer's ongoing convenience rather than the original coordination function. The founding_problem_status is authored as contested rather than flatly dead because reasonable observers (including some IMF historical retrospectives) argue the coordination function remained partially live until the 1971 collapse, while others (Triffin and successors) argued the asymmetry was present from 1944 and simply grew visible over time. This is precisely the kind of case the tangled_rope classification and the R5 genealogy interview exist to surface: neither 'pure extraction from the start' nor 'genuine coordination undermined only by exogenous shock' captures it cleanly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_rhetoric_vs_structural_privilege,
    'Was the ''preserve national monetary sovereignty'' framing a genuine design goal that was later structurally undermined by dollar overhang, or was reserve-currency asymmetry embedded in the design from 1944 and the sovereignty rhetoric always partially cover for it?',
    'Close reading of the 1944 negotiation record (White plan vs. Keynes plan drafts) for whether asymmetric reserve-currency privilege was foreseen and accepted by non-U.S. delegations, versus emerging later as an unanticipated consequence of U.S. deficit behavior.',
    'If asymmetry was foreseen and accepted, this reading''s coordination function claim weakens toward snare; if it emerged later as drift, the tangled_rope classification (genuine coordination degrading into extraction) is more strongly supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_rhetoric_vs_structural_privilege, conceptual, 'Whether reserve-currency asymmetry was a founding feature or an emergent drift.').

omega_variable(
    kernel_reading_boundary_capital_vs_sovereignty,
    'Is the sovereignty_defense reading (constraint on external discipline) structurally distinct from the keynesian_embedded_liberalism reading (constraint on international capital), or are they the same claim under different labels?',
    'Compare the two readings'' victim sets and beneficiary sets directly: sovereignty_defense names non-reserve states as victims and the U.S. as beneficiary; keynesian_embedded_liberalism names footloose capital as the constrained party and domestic labor/industry as beneficiary. Where these sets diverge is where the readings are genuinely distinct constraints, not merely relabeled framings of one.',
    'If the readings converge on the same victim/beneficiary structure under analysis, they should be merged into one story per the ε-invariance principle; if they diverge (as authored here), they remain properly separate constraints linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_capital_vs_sovereignty, conceptual, 'Whether this reading and its sibling keynesian reading are structurally distinct or the same constraint mislabeled twice.').

omega_variable(
    gold_anchor_stabilizer_or_snare,
    'Is the gold-dollar anchor, from the sovereignty_defense reading''s own lights, better characterized as a genuine (if imperfect) stabilizing mechanism through most of its life, or as a snare on peripheral states from a much earlier point?',
    'Trace the ratio of U.S. gold reserves to foreign dollar liabilities year-by-year (the Triffin ratio) against instances of peripheral states being forced into austerity or devaluation attributable to defending the peg rather than to domestic conditions.',
    'An early and sustained snare reading would push extractiveness higher across the whole interval rather than rising gradually as currently authored; a later-onset reading supports the tangled_rope-then-decay trajectory as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_stabilizer_or_snare, empirical, 'Timing of the transition from coordination-dominant to extraction-dominant operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bret_tr_t5, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 5, 0.24).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 10, 0.3).
narrative_ontology:measurement(bret_tr_t15, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 15, 0.35).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 20, 0.38).
narrative_ontology:measurement(bret_tr_t27, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 27, 0.4).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bret_be_t5, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(bret_be_t15, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(bret_be_t27, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 27, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bret_su_t5, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(bret_su_t15, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(bret_su_t27, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 27, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__sovereignty_defense, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the bretton_woods_treaty_substrate kernel, each instantiating a distinct constraint from the same 1944 treaty text: sovereignty_defense (this story — external discipline constrained, non-reserve states victimized, U.S. privileged), keynesian_embedded_liberalism (international capital constrained, domestic labor/industry beneficiary, footloose capital constrained party), and neoliberal_convertibility (government intervention constrained, convertibility beneficiaries, discretionary-policy states constrained). Each carries its own ε, its own beneficiary/victim structure, and its own classification per the ε-invariance principle; they are linked here rather than merged because measuring 'what Bretton Woods constrains' yields materially different answers depending on which structural claim is being evaluated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
