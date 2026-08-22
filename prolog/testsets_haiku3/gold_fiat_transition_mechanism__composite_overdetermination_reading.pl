% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_composite, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Fiat Money Transition via Structural Convergence (Composite Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   The transition from the Bretton Woods gold-anchored system (1944–1971) to
 *   floating fiat regimes did not result from a single causal node—neither an
 *   inevitable physical constraint (the automatic_constraint_reading) nor a
 *   geopolitical power grab (the creditor_discipline_reading). Instead, four
 *   independent structural changes converged: (1) Telecommunications
 *   technology (satellites, digital networks) enabled instant capital flows
 *   and arbitrage that fixed-exchange regimes could not sustain. (2) The
 *   Bretton Woods peg itself became unsustainable as capital mobility
 *   increased, independent of gold reserves (the Triffin dilemma was a
 *   symptom, not the cause). (3) Organized labor in core industrial economies
 *   achieved peak bargaining power through union mobilization and tight labor
 *   markets, enabling wage growth that Bretton Woods' deflationary discipline
 *   would have suppressed. (4) Legal tender enforcement matured—central banks
 *   developed capacity to enforce fiat currency without gold backing, which
 *   Bretton Woods framers viewed as impossible. Nixon's 1971 suspension of
 *   gold convertibility was a symbolic marker of these convergent forces, not
 *   their origin. The composite reading challenges the kernel itself: it
 *   denies that there was a single 'transition mechanism' to analyze. If
 *   correct, the automatic and creditor readings misattribute causality to
 *   unified forces that did not exist.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer (US Fed/Treasury): gained discretionary monetary authority as gold-backed anchor collapsed; benefits from seigniorage and freedom from redemption discipline.
 *   - fixed_exchange_rate_creditors (foreign central banks): absorbed losses on reserve holdings; lost disciplinary veto power over US policy.
 *   - telecommunications_arbitrageurs (financial traders/institutions): exploited instant capital flows and currency volatility enabled by technology; beneficiaries of the shift to floating rates.
 *   - commodity_exporters (OPEC, agricultural nations): faced unexpected terms-of-trade volatility and inflation externalities; lost nominal stability of commodity contracts.
 *   - labor_mobilized_constituencies (organized labor, core industrial unions): gained employment flexibility and wage growth from fiat discretion, though labor mobilization was an independent force.
 *   - savers_in_fixed_nominal_terms (households, pensioners): bore inflation tax from fiat discretion; no exit available.
 *   - Bretton Woods architects (analytical seat): designed the system on assumptions that telecommunications, labor power, and capital mobility would not render fixed rates unsustainable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.42).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Fiat Money Transition via Structural Convergence (Composite Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '89c460d1-d1fe-4ba1-943e-8e8f35028121').
narrative_ontology:cs_kernel_codification('89c460d1-d1fe-4ba1-943e-8e8f35028121', fixed_text).
narrative_ontology:cs_authority_grounding('89c460d1-d1fe-4ba1-943e-8e8f35028121', extraction).
narrative_ontology:cs_interpretation_layer_present('89c460d1-d1fe-4ba1-943e-8e8f35028121').
narrative_ontology:cs_reading_relation('89c460d1-d1fe-4ba1-943e-8e8f35028121', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('89c460d1-d1fe-4ba1-943e-8e8f35028121', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('89c460d1-d1fe-4ba1-943e-8e8f35028121', foundational, transition_mechanism_overdetermined).
narrative_ontology:cs_axiom_status(transition_mechanism_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('89c460d1-d1fe-4ba1-943e-8e8f35028121', transition_mechanism_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('89c460d1-d1fe-4ba1-943e-8e8f35028121', foundational, multiple_independent_structural_forces).
narrative_ontology:cs_axiom_status(multiple_independent_structural_forces, holdable).
narrative_ontology:cs_axiom_grounding('89c460d1-d1fe-4ba1-943e-8e8f35028121', multiple_independent_structural_forces, empirically_contingent).
narrative_ontology:cs_reference_frame('89c460d1-d1fe-4ba1-943e-8e8f35028121', coordinated_capital_flows_gold_backed).
narrative_ontology:cs_drift_state('89c460d1-d1fe-4ba1-943e-8e8f35028121', post_1971_floating_rates, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('89c460d1-d1fe-4ba1-943e-8e8f35028121', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_arbitrageurs).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_mobilized_constituencies).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_exchange_rate_creditors).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, commodity_exporters).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, savers_in_fixed_nominal_terms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks_periphery).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, money_supply_discretion_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, institutional_authority_replaces_material_constraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States Federal Reserve and Treasury coordinate monetary and fiscal policy under fiat authority. Freed from gold redemption discipline, they gain discretionary control over money supply and inflation outcomes. They set the enforcement mechanism (legal tender laws, capital controls, exchange intervention) and collect the seigniorage benefit of fiat creation. Exit is not available; the issuer IS the system.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer, beneficiary).

% Central banks and treasuries holding reserves under Bretton Woods agreement bore the cost of the transition: the gold redemption guarantee they held as monetary anchor was eliminated, exposing them to currency devaluation risk. Their disciplinary power (ability to call for gold) was stripped. They absorbed losses on their existing USD-denominated reserves through inflation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_exchange_rate_creditors, payer,
    institutional, generational, constrained, global).

% Financial traders and institutions exploiting instant capital flows made possible by satellite and digital telecommunications. The technology enabled carry trades, currency speculation, and interest-rate arbitrage that were impossible under fixed-exchange regimes. The transition to floating rates and fiat discretion opened these profitable channels. They benefit from volatility and from the speed advantages their technology provides.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_arbitrageurs, beneficiary,
    powerful, biographical, mobile, global).

% Oil and commodity-producing nations (OPEC, agricultural exporters) faced unexpected terms-of-trade shifts and inflation volatility as USD floated freely. The Bretton Woods anchor had provided nominal stability in commodity contracts; fiat discretion removed that anchor. Their revenues, priced in dollars, became subject to US monetary policy externalities they could not control or discipline via gold redemption threats.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, commodity_exporters, payer,
    moderate, generational, constrained, global).

% Organized labor in core industrial nations (US, Western Europe) achieved peak bargaining power in the 1960s–early 1970s independent of the gold standard's collapse. The shift to fiat enabled wage growth and fuller employment without the deflationary discipline the gold standard imposed. Labor's exit options were rising (migration, skill formation, union organization) due to labor-market tightness, not directly because of monetary regime change, but the timing and fiat discretion enabled accommodation of wage growth that Bretton Woods would have suppressed.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_mobilized_constituencies, beneficiary,
    organized, generational, constrained, national).

% Individuals holding cash, bonds, and long-term fixed-rate instruments saw their purchasing power eroded by inflation that followed fiat transition. They could not redeem currency for gold; they bore the inflation tax created by the regime's newfound discretionary capacity. Their exit options (currency diversification, real assets) existed in principle but were costly and unevenly available across populations.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, savers_in_fixed_nominal_terms, payer,
    powerless, biographical, trapped, national).

% The 1944 framers (Keynes, White, and the allied economists) designed a hybrid system: gold-backed dollar at fixed parities. Their intent was to provide both monetary discretion and discipline. The composite reading suggests their design was undermined not by a single causal failure but by the convergence of multiple structural forces their framework did not anticipate: instant capital flows, independent labor mobilization, and the technical impossibility of maintaining fixed rates under those pressures.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_architects, observer,
    analytical, generational, analytical, global).

% Smaller, non-reserve-issuing central banks lost the stable nominal anchor Bretton Woods provided. They faced either fixed-rate pegs (which became unsustainable without gold backing) or floating rates (exposing them to external monetary policy shocks). They bore the cost of transition volatility and had no disciplinary mechanism available; exit was not available.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks_periphery, payer,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods was designed to coordinate international capital flows and trade settlement under a gold-backed nominal anchor. The coordination function was real and successful for approximately 25 years. However, multiple independent structural changes (telecommunications enabling instant flows, labor mobilization enabling wage pressure, capital mobility outpacing gold reserves, institutional capacity for fiat enforcement) undermined the fixed-rate mechanism itself. The transition fragmented coordination: different nations adopted different floating and managed-peg regimes, and no unified coordination mechanism was restored.
% TRANSFER_FUNCTION: Moves monetary authority from gold-backed constraint (benefiting creditors with disciplinary veto) to discretionary fiat (benefiting reserve issuers and arbitrageurs). Also redistributes across savers (who lose to inflation) and wage earners (who gain flexibility). The transfer is the aggregate effect of multiple independent structural changes, not the intention of a single agent.
% ABSENT_VOICES: Bretton Woods was negotiated by a small set of institutional actors (US, UK, allied nations' treasuries and central banks). The telecommunications arbitrageurs (who would become major beneficiaries) had no representation—the technology did not exist. Savers in fixed terms, commodity exporters, and peripheral central banks were not represented in the design process and had no seat at the transition decision.
% DISAPPEARANCE_RATIONALE: If the composite transition never occurred and Bretton Woods remained in force, the world would have faced cumulative pressure from all four structural forces (telecommunications arbitrage would have created unsustainable capital flows; labor mobilization would have pressured wages; capital mobility would have exceeded reserve backing; institutional fiat capacity would have been unutilized). Either the fixed-rate system would have collapsed, or it would have required increasingly severe capital controls and deflationary discipline. The transition enabled floating rates and fiat accommodation that no single causal node created, but the cumulative effect of the four forces made the transition inevitable.
% FOUNDING_PROBLEM: The Bretton Woods system (1944) was designed to prevent a return to 1930s-style competitive devaluations, beggar-thy-neighbor policies, and deflationary crises. The gold standard at fixed parities, with an 'adjustable peg' mechanism, was intended to provide both discipline (gold redemption threats) and flexibility (the ability to adjust the peg under pressure).
% FOUNDING_PROBLEM_CORROBORATION: The problem the system was designed to solve—preventing competitive devaluations and deflationary spirals—was indeed avoided under Bretton Woods (1944–1971). However, the problem ceased to be the operative constraint by the late 1960s. Instead, the system itself became the constraint: the fixed-rate mechanism was incompatible with the capital flows and labor pressures that developed. The architects (Keynes, White) did not anticipate telecommunications, labor mobilization, or the cumulative effect of capital mobility on reserve adequacy. Post-1971 economic historians and monetary economists (Eichengreen, Obstfeld, Temin, Kindleberger) document that the founding problem was solved, but new constraints emerged that the architecture could not accommodate. No one attesting outside the Bretton Woods beneficiary set (which included central banks) advocated for the system's preservation by 1971—even creditor nations recognized it was unsustainable.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58 reflects that the transition redistributed significant purchasing power and monetary authority but without a single concentrated beneficiary. The reserve issuer gained authority but faced new constraints (capital flight threats, inflation expectations). Telecommunications arbitrageurs gained most directly but represent a narrow constituency. Labor beneficiaries gained flexibility but at the cost of inflation volatility they could not fully control. Measured at the endpoint (1976), extractiveness is moderate because the fiat regime had stabilized somewhat after the initial shock of 1971–73, and the multiple distributional effects no longer accumulated monotonically. Suppression at 0.42 reflects that the transition required active enforcement (legal tender laws, capital controls in some regimes, central bank intervention to manage floating rates) but was not a high-suppression regime in the Bretton Woods sense—there was no single enforcer preventing alternatives. Theater ratio at 0.28 and rising through the interval reflects that increasingly, central banks engaged in rhetoric about managing the system (exchange intervention, coordinated policy) whose effect on outcomes was often theatrical—the underlying structural forces were moving independently. Accessibility collapse at 0.65 because, once the system began to float and fiat took hold, alternatives (return to gold standard, return to fixed rates without gold backing) became increasingly costly to reimpose; the technological and labor changes were not reversible. Resistance at 0.72 reflects that the creditor nations, commodity exporters, savers, and peripheral central banks mounted real resistance: capital flight, hoarding, speculation, and ultimately legislative pressure (the Smithsonian Agreement attempt in 1971, repeated IMF negotiations). The resistance did not prevent the transition but was substantial enough that enforcement machinery was required.
 *
 * PERSPECTIVAL GAP:
 *   From the reserve issuer: liberation and rational management. From creditors: theft of discipline. From labor: freedom to bargain. From commodity exporters: instability imposed. From savers: inflation confiscation. From telecommunications arbitrageurs: market opportunity. The composite reading does not resolve these perspectives but denies they trace to a single causal node, which undermines both the automatic and creditor readings' structural claims.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve issuer is a clear beneficiary (d toward 0.0) because it gained discretionary authority and seigniorage. However, its gains came from adaptation, not from seizing control—this distinguishes the composite reading from the creditor_discipline_reading (which claims intentional power-taking). Creditors are clear targets (d toward 1.0) because they bore the losses on reserve devaluation and lost their veto. Telecommunications arbitrageurs are beneficiaries (d toward 0.0) because the floating-rate, high-volume environment was their primary profit source. Labor constituencies are complex: they are beneficiaries (d toward 0.0) in the sense that fiat discretion enabled accommodative policy, but labor mobilization was an independent force—labor would have sought wage growth under Bretton Woods too. Commodity exporters are targets (d toward 1.0) due to volatility and terms-of-trade risk. Savers are targets (d toward 1.0) due to inflation tax. The directional asymmetry is real, but the absence of a single agenda-setter differentiates this from a snare: the transition happened because of structural forces, not because a single agent coordinated it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of Bretton Woods (1944) was to coordinate international capital flows and prevent competitive devaluations while allowing monetary flexibility. The system did coordinate for roughly 25 years, during which the founding problem remained live. By 1968–1971, however, the founding problem status transitioned from live to contested and then dead: the problem it was designed to solve (the 1930s chaos) was no longer the operative constraint; instead, the system itself became the constraint. The composite reading suggests mandatrophy was overdetermined: no single cause killed the founding problem, so no single remedy could resurrect it. Attempts to resurrect Bretton Woods-like coordination (the Smithsonian Agreement, the European Monetary System, later the euro) all relied on either reimposing capital controls, accepting deflationary discipline, or (in the EMU case) delegating monetary authority to a supranational institution. The transition itself represents mandatrophy: the fiat regime persists because legal tender enforcement matured and because reversing any one of the four structural changes (telecom, capital mobility, labor power, institutional capacity) would not restore Bretton Woods—all four would have to be reversed simultaneously, which is infeasible. This is structurally different from a snare (which persists because enforcement suppresses alternatives) or a piton (which persists through inertia). The fiat regime is actively maintained, but the reason is not that the reserve issuer seized power; it is that the alternatives are structurally impossible given the convergent forces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    single_versus_multiple_causality,
    'Did the Bretton Woods transition result from a single causal force (gold constraint, creditor veto, or policy choice) or from convergent independent structural changes?',
    'Counterfactual historical analysis: if one structural force had been absent (say, telecommunications had not advanced), could Bretton Woods have persisted? Empirical economic historians (Eichengreen, Obstfeld, Temin) provide data on the relative timing and independence of each force. Cross-national evidence on whether nations with different labor dynamics, capital mobility, or technological infrastructure experienced the transition differently.',
    'If single causality: one of the sibling readings (automatic or creditor) is correct, and this reading is wrong. If convergent: this reading is correct, and both siblings misattribute causality; the kernel itself lacks a single mechanism. Classification consequence: a single-cause reading would produce a more determinate type (snare or tangled rope with clear agenda-setter); a convergent-causes reading produces a tangled rope with diffuse beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_versus_multiple_causality, empirical, 'Whether the transition was overdetermined or had a single dominant causal mechanism.').

omega_variable(
    labor_power_independence,
    'Was the rise in labor bargaining power in the 1960s–70s an independent structural shift or a consequence of the Bretton Woods system''s monetary accommodation?',
    'Comparative labor history: did labor mobilization and wage pressure emerge similarly across different currency regimes? Did peripheral nations with capital controls and fixed rates experience similar labor dynamics? Cross-sector analysis of wage growth relative to productivity in different monetary policy regimes.',
    'If independent: labor is a distinct beneficiary of the transition, confirming the composite reading''s claim of multiple independent forces. If dependent: labor''s wage growth was enabled by Bretton Woods'' accommodative phase, and the transition to fiat was primarily about managing that accommodation''s inflationary consequences—a different causal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_power_independence, empirical, 'Whether labor mobilization was an independent or derivative force in the transition.').

omega_variable(
    telecommunications_counterfactual,
    'If telecommunications had not advanced beyond telephone/telegraph speeds, could fixed exchange rates have persisted even as US deficits accumulated?',
    'Simulation of Bretton Woods under capital control assumptions and slower information diffusion; comparison with actual managed-peg regimes (China, Vietnam, some Middle East sovereigns) that limited capital flows technologically and legally. Historical analysis of whether the 1960s-era gold runs and speculative attacks required real-time trading or could have occurred under slower information.',
    'If affirmative: telecommunications was a necessary condition for the transition, and the composite reading''s claim of multiple overdetermined causes is correct. If negative (fixed rates could have failed anyway): telecommunications enabled the transition but was not determinative; policy choices or external shocks (Vietnam War spending, the 1973 oil crisis) were primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(telecommunications_counterfactual, conceptual, 'Whether telecommunications advancement was a necessary condition for the transition or merely accelerated an inevitable shift.').

omega_variable(
    reading_singularity,
    'Is the ''transition from gold to fiat'' a coherent singular constraint, or does the composite reading''s emphasis on convergence imply it is actually a constraint family that should be decomposed?',
    'Structural analysis per the ε-invariance principle: if measuring the transition by the elimination of gold constraint (automatic reading) yields a different ε than measuring by the loss of creditor discipline (creditor reading), which in turn differs from measuring by the enablement of telecommunications arbitrage (telecommunications component of composite), then the three readings are actually three constraints, not three readings of one. The composite reading itself implies this possibility: if the multiple forces are truly independent, they are independently sufficient as constraints.',
    'If the readings are actually separate constraints: decompose into a constraint family with affects_constraints links. If the readings are truly alternative framings of a single constraint: accept the composite reading''s claim that the transition''s mechanism was overdetermined, and classify accordingly. This is the deepest uncertainty: whether the kernel is singular or plural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_singularity, conceptual, 'Whether the gold-fiat transition is one constraint with multiple readings or a constraint family wrongly unified under a single label.').

omega_variable(
    legal_tender_enforcement_maturation,
    'Did central banks develop the institutional capacity to enforce fiat currency independently of Bretton Woods, or did fiat enforcement capacity emerge *as a consequence* of Bretton Woods collapse, not as a prior enabling condition?',
    'Institutional history of central banking: when did central banks adopt legal tender enforcement mechanisms, inflation-targeting frameworks, and monetary policy independence? Were these developed under Bretton Woods (suggesting prior capacity) or after 1971 (suggesting post-hoc adaptation)? Comparative study of which nations had fiat capacity before 1944 and whether they were able to sustain it.',
    'If prior capacity: legal tender enforcement was a standing alternative to gold backing, and the composite reading''s claim of convergent structural changes is supported. If post-hoc adaptation: the transition to fiat was not enabled by pre-existing enforcement maturity but rather forced central banks to develop it, which suggests the transition was more contingent than the composite reading implies—a different causal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_tender_enforcement_maturation, empirical, 'Whether fiat enforcement capacity existed before 1971 or was developed in response to the transition.').

omega_variable(
    kernel_contest_foreclosure,
    'Do the three readings (automatic, creditor, composite) logically foreclose each other, or can they coexist as competing interpretations of a singular event?',
    'Logical analysis of each reading''s core claim: the automatic reading claims gold-constraint elimination was the necessary and sufficient cause. The creditor reading claims geopolitical power transfer was the mechanism. The composite reading claims multiple independent forces converged. These claims are logically distinct but not logically contradictory—each could be true of different aspects of the transition (gold constraint was a bottleneck; power transfer did occur; multiple forces were at work). The question is whether the framings are compatible within a single explanatory framework or whether accepting one forecloses the others.',
    'If foreclosure: one reading is structurally true and the others false; the kernel has a unique solution. If coexistence: the readings are genuinely alternative framings and all are live options; the kernel is indeterminate, and this omega itself becomes part of the constraint''s content.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether the three kernel readings are logically compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1944, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1944, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement_basis(gold_tr_t1944, projected).
narrative_ontology:measurement(gold_tr_t1952, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1952, 0.12).
narrative_ontology:measurement_basis(gold_tr_t1952, observed).
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement_basis(gold_tr_t1960, observed).
narrative_ontology:measurement(gold_tr_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement_basis(gold_tr_t1968, observed).
narrative_ontology:measurement(gold_tr_t1972, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1972, 0.31).
narrative_ontology:measurement_basis(gold_tr_t1972, observed).
narrative_ontology:measurement(gold_tr_t1976, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1976, 0.28).
narrative_ontology:measurement_basis(gold_tr_t1976, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1944, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement_basis(gold_be_t1944, projected).
narrative_ontology:measurement(gold_be_t1952, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1952, 0.28).
narrative_ontology:measurement_basis(gold_be_t1952, observed).
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement_basis(gold_be_t1960, observed).
narrative_ontology:measurement(gold_be_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1968, 0.54).
narrative_ontology:measurement_basis(gold_be_t1968, observed).
narrative_ontology:measurement(gold_be_t1972, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1972, 0.62).
narrative_ontology:measurement_basis(gold_be_t1972, observed).
narrative_ontology:measurement(gold_be_t1976, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1976, 0.58).
narrative_ontology:measurement_basis(gold_be_t1976, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1944, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1944, 0.25).
narrative_ontology:measurement_basis(gold_su_t1944, projected).
narrative_ontology:measurement(gold_su_t1952, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1952, 0.35).
narrative_ontology:measurement_basis(gold_su_t1952, observed).
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement_basis(gold_su_t1960, observed).
narrative_ontology:measurement(gold_su_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement_basis(gold_su_t1968, observed).
narrative_ontology:measurement(gold_su_t1972, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1972, 0.44).
narrative_ontology:measurement_basis(gold_su_t1972, observed).
narrative_ontology:measurement(gold_su_t1976, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1976, 0.42).
narrative_ontology:measurement_basis(gold_su_t1976, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.18).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gold-fiat transition kernel. The composite_overdetermination_reading denies the kernel's premise of a single mechanism—it claims the transition resulted from convergent independent structural changes (telecommunications, labor mobilization, capital mobility, legal tender enforcement maturation) rather than from a unified causal force. If correct, the automatic_constraint_reading (which attributes the transition to the elimination of a physical gold constraint) and the creditor_discipline_reading (which attributes it to a geopolitical power transfer) both misattribute causality to non-existent singular mechanisms. The three readings are linked via network.affects_constraints to enable cross-reading structural comparison and detection of foreclosure or compatibility. The composite reading influences both siblings by challenging their shared assumption that the transition had a single causal node.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
