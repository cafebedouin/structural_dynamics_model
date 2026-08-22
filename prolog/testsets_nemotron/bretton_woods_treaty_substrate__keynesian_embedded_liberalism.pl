% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls as Domestic Policy Space Protection
 *   domain: international_political_economy
 *
 * SUMMARY:
 *   The Bretton Woods treaty substrate (1944 Articles of Agreement) is read
 *   here as the Keynesian embedded liberalism constraint: a system of
 *   adjustable peg exchange rates backed by capital controls that protects
 *   national policy autonomy — full employment, industrial policy,
 *   developmental statecraft — from the disciplining pressure of free capital
 *   mobility. International finance (banks, speculators, mobile capital) is
 *   the structural victim: it bears the cost of constrained exit and
 *   below-world-market returns. National governments, labor coalitions, and
 *   developmental bureaucracies are beneficiaries: they gain policy space.
 *   The constraint requires active enforcement (IMF Article VIII/VI, national
 *   capital control laws) and solves a genuine coordination problem
 *   (preventing competitive devaluation, stabilizing postwar trade
 *   reconstruction). Over time, as capital markets deepen and Eurodollar
 *   markets emerge, the extractiveness rises and enforcement hardens — the
 *   1958 European convertibility milestone and 1971 Nixon shock mark the
 *   constraint's structural breakdown. This reading instantiates one
 *   constraint in the bretton_woods_treaty_substrate kernel family.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.22).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.35).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.22).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls as Domestic Policy Space Protection").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'ee58d051-fb16-4695-bf5c-53f2c32be5ed').
narrative_ontology:cs_kernel_codification('ee58d051-fb16-4695-bf5c-53f2c32be5ed', formalized).
narrative_ontology:cs_authority_grounding('ee58d051-fb16-4695-bf5c-53f2c32be5ed', lineage).
narrative_ontology:cs_interpretation_layer_present('ee58d051-fb16-4695-bf5c-53f2c32be5ed').
narrative_ontology:cs_reading_relation('ee58d051-fb16-4695-bf5c-53f2c32be5ed', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('ee58d051-fb16-4695-bf5c-53f2c32be5ed', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('ee58d051-fb16-4695-bf5c-53f2c32be5ed', foundational, capital_controls_legitimate_policy_tool).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('ee58d051-fb16-4695-bf5c-53f2c32be5ed', capital_controls_legitimate_policy_tool, conventional).
narrative_ontology:cs_axiom('ee58d051-fb16-4695-bf5c-53f2c32be5ed', foundational, policy_autonomy_primacy_over_capital_mobility).
narrative_ontology:cs_axiom_status(policy_autonomy_primacy_over_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('ee58d051-fb16-4695-bf5c-53f2c32be5ed', policy_autonomy_primacy_over_capital_mobility, instrumental).
narrative_ontology:cs_axiom('ee58d051-fb16-4695-bf5c-53f2c32be5ed', secondary, adjustable_peg_as_coordination_mechanism).
narrative_ontology:cs_axiom_status(adjustable_peg_as_coordination_mechanism, overridden).
narrative_ontology:cs_axiom_grounding('ee58d051-fb16-4695-bf5c-53f2c32be5ed', adjustable_peg_as_coordination_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('ee58d051-fb16-4695-bf5c-53f2c32be5ed', keynes_white_embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('ee58d051-fb16-4695-bf5c-53f2c32be5ed', post_1958_european_convertibility, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ee58d051-fb16-4695-bf5c-53f2c32be5ed', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_coalitions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, developmental_state_bureaucracies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, global_bank_syndicates).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, currency_speculators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_hegemon).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use capital controls to maintain adjustable pegs while pursuing full employment, industrial policy, and developmental strategies. Bound by IMF Articles but retain Article VI capital control rights. Exit means abandoning the fixed-rate system — costly but possible (Canada 1950, 1962).
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary,
    institutional, biographical, constrained, national).

% Gain macroeconomic policy space for full employment targeting and wage bargaining backed by managed exchange rates. Their exit is political — they can pressure governments but cannot directly escape the international monetary framework.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_coalitions, beneficiary,
    organized, biographical, constrained, national).

% Direct credit, protect infant industries, manage import substitution behind capital controls. Japan MITI, French Planning Commission, Italian IRI. Exit means dismantling the developmental model — structurally difficult once instituted.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, developmental_state_bureaucracies, beneficiary,
    institutional, generational, constrained, national).

% Bears constrained returns and restricted cross-border deployment. But gains stable trading environment, reduced currency risk, and guaranteed convertibility for current account. Partial exit via Eurodollar market innovation (offshore dollar lending outside national controls).
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital, payer,
    powerful, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital, beneficiary).

% Constrained in cross-border lending by national capital controls. Develop Eurodollar markets as structural workaround. Benefit from stable trade finance flows under the peg system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, global_bank_syndicates, payer,
    powerful, immediate, mobile, global).

% Capital controls directly target speculative flows. No legitimate exit within the system — their activity is what the constraint suppresses. Exit means leaving the currency market entirely or moving to uncontrolled offshore venues.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, currency_speculators, payer,
    moderate, immediate, trapped, global).

% Administers the Articles of Agreement: polices current-account convertibility (Article VIII) while tolerating capital controls (Article VI). Adjudicates parity changes. Its authority derives from the treaty substrate itself.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_surveillance, agenda_setter,
    institutional, generational, analytical, global).

% Anchor currency issuer; bears the system's balance-of-payments burden (Triffin dilemma). Sets the terms of adjustment. Can exit by closing the gold window (1971) — the ultimate arbitrage exit that ends the constraint.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_hegemon, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_hegemon, payer).

% Analyze the regime's distributional effects, coordination success, and breakdown. No material stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents competitive devaluation and beggar-thy-neighbor policies; stabilizes postwar trade reconstruction; provides a predictable monetary framework for national reconstruction and development planning.
% TRANSFER_FUNCTION: Moves seigniorage and policy autonomy from international finance (which would discipline governments via capital flight) to national governments (which gain space for full employment and industrial policy). The transfer is the constrained return on mobile capital.
% ABSENT_VOICES: Colonial and postcolonial economies not at Bretton Woods (India attended but many African/Asian territories did not) — their policy space was constrained by metropolitan currencies and sterling balances, not protected. Future generations who inherit the Triffin dilemma's breakdown. Global South voices in the 1970s NIEO debates who argued the system entrenched Northern policy autonomy at Southern expense.
% DISAPPEARANCE_RATIONALE: If capital controls and adjustable pegs vanished overnight in 1958, governments would lose policy autonomy, finance would gain disciplining power, trade would face exchange rate volatility, and the developmental state model would be immediately threatened. The world rearranged in 1971 when the constraint did disappear — floating rates, financialization, and the end of the embedded liberalism compromise followed.
% FOUNDING_PROBLEM: Postwar reconstruction required both international trade stability (fixed rates) and domestic policy autonomy (full employment, industrial policy) — the prewar gold standard sacrificed the latter for the former, and the 1930s chaos showed both could not be had without constraints on capital.
% FOUNDING_PROBLEM_CORROBORATION: Keynes and White's negotiating records (horse's mouth). Ruggie's 'embedded liberalism' thesis (independent IR scholar). Eichengreen's historical analysis (independent economist). The operator (US Treasury) and beneficiaries (European governments) attested the problem was live through the 1960s; the 1971 breakdown confirms the problem's conditions (US surplus, capital immobility) were gone.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).
:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.22 at midpoint, rising to 0.35 at breakdown) because capital controls were not primarily rent-extraction devices — they were the price of the adjustable peg system. The coordination function (trade stability, preventing beggar-thy-neighbor) is genuine and documented. Suppression is moderate (0.35) because controls were legally enforced but with wide current-account freedom; the victim set (international finance) had partial exit via Eurodollar innovation. Theater ratio is low (0.18) because the system delivered its stated function for 25 years. Accessibility collapse is moderate (0.42): alternatives (floating rates, gold standard) existed but were politically disfavored. Resistance (0.38) came from finance interests and later from US balance-of-payments pressure. The claimed type is rope because the coordination function dominates and beneficiaries are broad coalitions, not narrow rentiers — but the rising extractiveness trajectory and victim set make it a contested rope.
 *
 * PERSPECTIVAL GAP:
 *   From the national government seat, this is a rope (coordination with modest cost). From the international finance seat, it is a snare (extraction with suppressed exit). The engine computes this divergence from the structural data: different power/exit combinations yield different effective extraction. The neoliberal_convertibility reading would invert the beneficiary/victim assignment; the sovereignty_defense reading would shift victims to IMF/supervisory bodies. These are distinct constraints in the kernel family, not perspectives on one constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments (institutional power, biographical horizon, constrained exit via treaty obligations) are primary beneficiaries: they gain policy space. Domestic labor and developmental bureaucracies (organized/moderate power, biographical horizon, constrained exit) are secondary beneficiaries. International finance (powerful/moderate power, immediate horizon, mobile-to-arbitrage exit) is the structural victim: it bears the transfer. The asymmetry is real but not extreme — finance accepted the regime for the trading stability it provided, making this a conditional victimhood rather than pure coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (postwar reconstruction with policy autonomy) was live in 1944 and remained live through the 1960s. The constraint did not outlive its mandate — it broke down when the founding problem's conditions changed (US hegemony erosion, capital mobility explosion). This is not mandatrophy; it is structural obsolescence. The mandatrophy_resolved flag is false because the constraint disappeared when its function became unsustainable, not when it persisted without function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct structural reading of the Bretton Woods treaty substrate kernel, or a rhetorical framing of the same constraint the neoliberal and sovereignty readings describe?',
    'Compare beneficiary/victim sets across the three readings: if each reading assigns different agents to beneficiary and victim roles with different structural exit options, they are distinct constraints linked by kernel_id; if they differ only in evaluation of the same structural arrangement, they are perspectives on one constraint.',
    'If distinct constraints, each gets its own ε and classification; if perspectives, they share a single constraint story with multiple observer seats. The current authoring treats them as distinct constraints in a kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s declared readings instantiate separate constraints or observer perspectives').

omega_variable(
    capital_controls_coordination_vs_extraction,
    'Do capital controls under Bretton Woods solve a genuine coordination problem (preventing competitive devaluation, stabilizing trade) or primarily extract from international finance to subsidize domestic policy?',
    'Historical counterfactual: compare trade volume and exchange rate stability in periods with binding vs. relaxed controls (1945-1958 transition, 1971 breakdown). If controls correlate with stability without suppressing trade below autarky levels, coordination function is genuine.',
    'If coordination is genuine and extraction asymmetric, classification is tangled_rope; if coordination is pretext, snare; if both weak, piton. Current metrics author genuine coordination with modest extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_controls_coordination_vs_extraction, empirical, 'Whether capital controls have independent coordination value beyond extraction').

omega_variable(
    domestic_policy_space_operationalization,
    'What specific domestic policies did the capital control regime actually enable that would have been impossible under free capital mobility?',
    'Documented policy episodes: full employment targeting (UK 1945-51, Sweden), industrial policy (France, Italy, Japan), agricultural price supports, directed credit. Trace whether each was capital-mobility-constrained.',
    'If the enabled policy set is large and welfare-significant, beneficiary declaration is structural; if thin or symbolic, beneficiary declaration is performative and the constraint trends toward snare for finance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_policy_space_operationalization, empirical, 'Concrete policy space the constraint protected').

omega_variable(
    finance_victimhood_degree,
    'Was international finance a net victim (bearing costs without offsetting benefits) or a conditional participant (accepting controls for stable trading environment)?',
    'Profitability and volume data for international banking 1945-1971. If returns were adequate and exit was voluntary (no coercive retention), victim status is partial; if controls forced below-market returns with no exit, victim status is structural.',
    'If finance was a conditional participant, the constraint is closer to rope; if structural victim, tangled_rope or snare depending on coordination genuineness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(finance_victimhood_degree, empirical, 'Whether international finance was coerced or accommodated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_kel_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(bw_kel_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(bw_kel_tr_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(bw_kel_tr_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(bw_kel_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.25).

% Extraction over time
narrative_ontology:measurement(bw_kel_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.12).
narrative_ontology:measurement(bw_kel_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(bw_kel_be_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(bw_kel_be_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(bw_kel_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(bw_kel_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.25).
narrative_ontology:measurement(bw_kel_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(bw_kel_su_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement(bw_kel_su_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement(bw_kel_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_conditionality_evolution).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, eurodollar_market_emergence).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, triffin_dilemma_constraint).

% DUAL FORMULATION NOTE:
% Part of the bretton_woods_treaty_substrate kernel family. This reading (keynesian_embedded_liberalism) assigns beneficiaries={national_governments, domestic_labor, developmental_bureaucracies}, victims={international_finance, global_banks, speculators}. The neoliberal_convertibility reading inverts this assignment. The sovereignty_defense reading assigns beneficiaries={central_banks}, victims={IMF_supervision, external_discipline}. All three share the treaty substrate but instantiate different constraints with different ε and structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, institutional, 0.15).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, powerful, 0.75).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, moderate, 0.65).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
