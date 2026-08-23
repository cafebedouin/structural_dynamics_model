% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [RESOLVED MANDATROPHY]
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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls as Embedded Liberalism
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   The Bretton Woods system (1944-1971) established a global monetary order
 *   centered on the dollar-gold standard with adjustable pegs and explicit
 *   permission for capital controls (Article VI). This reading — Keynesian
 *   embedded liberalism — sees the constraint as a deliberate design: capital
 *   controls are not a bug but the feature that protects domestic policy
 *   space for full employment and welfare states. International finance is
 *   structurally constrained (victim/payer) so that national governments can
 *   be beneficiaries. The system worked for ~25 years until the Triffin
 *   dilemma, eurodollar market innovation, and US fiscal-military overstretch
 *   broke the dollar-gold link. The mandate (protect policy space for full
 *   employment) was resolved by the system's success — the golden age of
 *   capitalism — but the constraint persisted into its own obsolescence,
 *   becoming a piton-like shell before Nixon ended convertibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.42).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.58).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.42).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls as Embedded Liberalism").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:has_sunset_clause(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '300812c9-1f99-4f63-b289-857807c22da7').
narrative_ontology:cs_kernel_codification('300812c9-1f99-4f63-b289-857807c22da7', formalized).
narrative_ontology:cs_authority_grounding('300812c9-1f99-4f63-b289-857807c22da7', lineage).
narrative_ontology:cs_interpretation_layer_present('300812c9-1f99-4f63-b289-857807c22da7').
narrative_ontology:cs_reading_relation('300812c9-1f99-4f63-b289-857807c22da7', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('300812c9-1f99-4f63-b289-857807c22da7', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('300812c9-1f99-4f63-b289-857807c22da7', foundational, capital_controls_are_legitimate_sovereign_tools).
narrative_ontology:cs_axiom_status(capital_controls_are_legitimate_sovereign_tools, holdable).
narrative_ontology:cs_axiom_grounding('300812c9-1f99-4f63-b289-857807c22da7', capital_controls_are_legitimate_sovereign_tools, conventional).
narrative_ontology:cs_axiom('300812c9-1f99-4f63-b289-857807c22da7', foundational, domestic_full_employment_primacy_over_capital_mobility).
narrative_ontology:cs_axiom_status(domestic_full_employment_primacy_over_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('300812c9-1f99-4f63-b289-857807c22da7', domestic_full_employment_primacy_over_capital_mobility, deontological).
narrative_ontology:cs_axiom('300812c9-1f99-4f63-b289-857807c22da7', secondary, adjustable_peg_transitional_to_more_flexible_system).
narrative_ontology:cs_axiom_status(adjustable_peg_transitional_to_more_flexible_system, overridden).
narrative_ontology:cs_axiom_grounding('300812c9-1f99-4f63-b289-857807c22da7', adjustable_peg_transitional_to_more_flexible_system, instrumental).
narrative_ontology:cs_reference_frame('300812c9-1f99-4f63-b289-857807c22da7', embedded_liberalism_compromise_1944).
narrative_ontology:cs_drift_state('300812c9-1f99-4f63-b289-857807c22da7', collapse_1971, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('300812c9-1f99-4f63-b289-857807c22da7', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, keynesian_welfare_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_hegemon).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, capital_controls_legitimate_policy_tool).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_full_employment_priority).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, embedded_liberalism_compromise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain policy autonomy to pursue full employment, industrial policy, and welfare states through capital controls and adjustable pegs. They administer the system through the IMF but are also its primary beneficiaries. Exit means abandoning the fixed-rate system and facing currency volatility.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary,
    institutional, generational, constrained, national).

% Benefit from government commitment to full employment and wage growth enabled by policy autonomy. Their bargaining power is strengthened when capital cannot easily exit. Exit is nearly impossible — they are territorially bound.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor, beneficiary,
    organized, biographical, constrained, national).

% The subset of national governments that actively use policy space for redistributive programs. They are the most committed defenders of the capital control regime. Their exit would mean dismantling the welfare compact.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, keynesian_welfare_states, beneficiary,
    institutional, generational, constrained, national).

% Face capital controls, regulated interest rates, and restricted cross-border lending. Their profits are compressed by the policy space granted to governments. They seek arbitrage through eurodollar markets and offshore banking. Exit means moving operations offshore — which they progressively do.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance, payer,
    powerful, biographical, constrained, global).

% Short-term hot money flows are directly targeted by capital controls. They are the most constrained by the system but also the most mobile — they invent new instruments (eurodollars, currency swaps) to circumvent controls. Their exit is the system's failure mode.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_capital, payer,
    moderate, immediate, mobile, global).

% Pay through restricted cross-border lending but benefit from stable exchange rates for trade finance. They develop the eurodollar market as an exit strategy while still using the Bretton Woods framework for legitimate trade. Dual position: constrained by controls, stabilized by pegs.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_banks, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_banks, beneficiary).

% Administer the adjustable peg system, police capital controls, and provide balance-of-payments financing. They embody the compromise — their Articles of Agreement enshrine capital controls (Article VI) while promoting convertibility (Article VIII). They are the institutional hinge.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_world_bank, agenda_setter,
    institutional, generational, analytical, global).

% Provides the reserve currency (dollar-gold convertibility) and military-security umbrella. Gains seigniorage and structural power but bears the Triffin dilemma — supplying global liquidity undermines its own gold backing. Ultimately chooses exit (1971 Nixon shock) rather than sustain the constraint.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_hegemon, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_hegemon, payer).

% Formally members but structurally marginalized — the system was designed by and for industrial economies. They lack the institutional capacity to use policy space effectively and face asymmetric adjustment burdens. Their voices are absent from the core governance.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, developing_nations, excluded,
    moderate, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Post-war monetary stability: fixed but adjustable exchange rates prevent competitive devaluations and beggar-thy-neighbor policies; capital controls give governments space to manage domestic employment without capital flight; IMF provides liquidity for temporary imbalances.
% TRANSFER_FUNCTION: Moves policy autonomy from international finance to national governments — specifically, the freedom to set interest rates, direct credit, and run counter-cyclical fiscal policy without immediate capital flight discipline. The transfer is from mobile capital's exit option to states' policy space.
% ABSENT_VOICES: Developing nations (structurally excluded from governance), colonial territories (not yet independent), and future generations (who would inherit the system's contradictions). The eurodollar market pioneers — who would become the system's undoing — were not yet visible in 1944.
% DISAPPEARANCE_RATIONALE: When the constraint collapsed in 1971, the world rearranged: floating rates replaced the peg, capital controls were dismantled globally, the eurodollar market exploded, and the neoliberal policy regime replaced embedded liberalism. The constraint's disappearance restructured the global political economy.
% FOUNDING_PROBLEM: The interwar disaster: competitive devaluations, capital flight destroying domestic recovery, gold standard rigidity preventing counter-cyclical policy, and the resulting political extremism. Bretton Woods was built to give governments the policy space to maintain full employment without triggering capital flight.
% FOUNDING_PROBLEM_CORROBORATION: Keynesian architects (Keynes, White) and post-war social democratic governments attest the problem was live and the solution worked for 25 years. Neoliberal economists (Friedman, Hayek) and international finance attest the problem was solved by 1960s and the constraint became rent-seeking. The Triffin dilemma — identified by a Belgian economist outside the beneficiary set — corroborates the structural contradiction.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) is moderate — capital controls extract from finance's mobility but the system also provides genuine coordination (stable trade rates, crisis lending). Suppression (0.58) is significant — capital controls require active enforcement (exchange controls, interest rate ceilings, regulatory barriers) and the system suppresses the eurodollar market's emergence until it breaks through. Theater ratio (0.18) is low initially — the coordination function is real and dominant — but rises slightly as the system's contradictions accumulate and the adjustable peg becomes a ritual. Accessibility collapse (0.35) is moderate — alternatives (floating rates, free capital mobility) exist conceptually but are politically illegitimate within the embedded liberalism consensus. Resistance (0.45) is moderate — finance resists through innovation (eurodollars) and lobbying, but the political coalition sustaining the system is strong until the late 1960s.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types from this structural data. From national governments' seat: the constraint is a rope/scaffold — genuine coordination with transitional justification. From international finance's seat: it is a snare/tangled_rope — extraction suppressing their mobility. From the US hegemon's seat: it shifts from rope to piton to snare as the Triffin dilemma intensifies. The claimed_type (tangled_rope) reflects the system-level structural hybridity — the engine's seat-divergence will reveal the political economy of the embedded liberalism compromise.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and domestic labor are structural beneficiaries (d near 0.0) — the constraint subsidizes their policy autonomy. International finance and speculative capital are structural targets (d near 1.0) — the constraint extracts their exit option. The IMF/World Bank sits near symmetric (d ~ 0.5) — they administer and benefit from the system's legitimacy but are constrained by its rules. The US hegemon is the most complex: agenda_setter (sets rules) but also payer (bears Triffin costs) — its directionality shifts over the interval from beneficiary to payer, culminating in exit. Developing nations are excluded — they would be payers if they had voice, but the system's design marginalizes them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interwar monetary chaos preventing full employment) was substantially solved by the 1960s — the 'golden age' of growth and employment. But the constraint persisted without its sunset clause being triggered (the adjustable peg was meant to evolve, not freeze). The mandate atrophied: by 1968-1971, the system constrained governments (via dollar-gold pressure) more than it empowered them. The Nixon shock was the mandatrophy resolution — the constraint's function had inverted. This reading captures the system at its functional peak (1944-1968) before the inversion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the keynesian_embedded_liberalism reading a distinct constraint from neoliberal_convertibility and sovereignty_defense, or are they observations of the same constraint from different angles?',
    'Apply the ε-invariance test: if measuring extraction via capital control restrictiveness gives low ε (finance constrained) but measuring via IMF conditionality gives high ε (governments constrained), they are different constraints. The three readings identify different beneficiary/victim structures and different coordination functions — they are structurally distinct constraints sharing a treaty text.',
    'If they are one constraint, ε is ambiguous and classification is observer-dependent. If three, each has stable ε and the kernel is a family. This reading authors ε=0.42 for the capital-control-as-protection constraint; neoliberal_convertibility would author a different ε for the conditionality-as-discipline constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the Bretton Woods treaty substrate is one constraint or a constraint family.').

omega_variable(
    mandatrophy_timing,
    'At what point did the Bretton Woods system''s mandate (protect policy space for full employment) invert into a constraint on that same policy space?',
    'Trace the Triffin dilemma''s quantitative pressure: when US gold coverage ratio fell below the threshold where dollar-gold convertibility required contractionary US policy that transmitted globally. The 1960 gold pool formation and 1968 two-tier market are markers. The mandate inverted when the system''s anchor (US) became its constraint.',
    'If mandatrophy occurred earlier (e.g., 1958 European convertibility), the system was a piton for longer. If later (1971 only), it was functional until collapse. This affects the theater_ratio trajectory and the claimed_type''s temporal validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_timing, empirical, 'When the embedded liberalism compromise inverted into its opposite.').

omega_variable(
    capital_control_effectiveness,
    'How effective were capital controls actually at protecting policy space versus how much was rhetorical?',
    'Compare onshore/offshore interest rate differentials, eurodollar market growth rates, and capital flight episodes across the interval. The controls were porous but not meaningless — they raised the cost of exit enough to sustain the policy coalition for 25 years.',
    'If controls were highly effective, extractiveness on finance was higher and the coordination function more genuine. If largely rhetorical, the system was theater from earlier, and the claimed_type shifts toward snare/piiton. The current metrics assume partial effectiveness — real friction but growing arbitrage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_effectiveness, empirical, 'The material reality of capital controls versus their legal form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(bret_tr_t1954, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1954, 0.14).
narrative_ontology:measurement(bret_tr_t1959, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1959, 0.15).
narrative_ontology:measurement(bret_tr_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1964, 0.16).
narrative_ontology:measurement(bret_tr_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1968, 0.17).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.18).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.25).
narrative_ontology:measurement(bret_be_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement(bret_be_t1954, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1954, 0.35).
narrative_ontology:measurement(bret_be_t1959, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1959, 0.38).
narrative_ontology:measurement(bret_be_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1964, 0.4).
narrative_ontology:measurement(bret_be_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.45).
narrative_ontology:measurement(bret_su_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(bret_su_t1954, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1954, 0.52).
narrative_ontology:measurement(bret_su_t1959, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1959, 0.55).
narrative_ontology:measurement(bret_su_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1964, 0.57).
narrative_ontology:measurement(bret_su_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1968, 0.58).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, eurodollar_market_emergence).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, nixon_shock_1971).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_policy_regime_consolidation).

% DUAL FORMULATION NOTE:
% This is the keynesian_embedded_liberalism reading of the bretton_woods_treaty_substrate kernel. The neoliberal_convertibility reading inverts the beneficiary/victim structure (finance becomes beneficiary, governments become constrained). The sovereignty_defense reading centers national monetary autonomy against IMF discipline. All three share the treaty text but instantiate different constraints with different ε values and different structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, institutional, 0.15).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
