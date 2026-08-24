% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Structural Adjustment Conditionalities — Hybrid Selectivity Reading
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   Structural adjustment conditionalities are the policy requirements
 *   attached to IMF and World Bank lending to crisis-hit developing
 *   countries. The hybrid selectivity reading holds that the system performs
 *   a genuine coordination function — solving the sovereign lending
 *   monitoring problem — but its enforcement is structurally asymmetric:
 *   geopolitically strategic debtors receive systematic waivers and
 *   forbearance, while non-strategic debtors face full enforcement. This
 *   selectivity is not noise; it is the mechanism by which the hegemon (US
 *   and allies) converts financial leverage into geopolitical alignment. The
 *   constraint is a Tangled Rope: coordination for some, extraction for
 *   others, held together by active enforcement that masks the selectivity
 *   behind technical language.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.76).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities — Hybrid Selectivity Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'a699e5eb-46af-4a2c-8c6d-b56f51628c61').
narrative_ontology:cs_kernel_codification('a699e5eb-46af-4a2c-8c6d-b56f51628c61', formalized).
narrative_ontology:cs_authority_grounding('a699e5eb-46af-4a2c-8c6d-b56f51628c61', lineage).
narrative_ontology:cs_interpretation_layer_present('a699e5eb-46af-4a2c-8c6d-b56f51628c61').
narrative_ontology:cs_reading_relation('a699e5eb-46af-4a2c-8c6d-b56f51628c61', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('a699e5eb-46af-4a2c-8c6d-b56f51628c61', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('a699e5eb-46af-4a2c-8c6d-b56f51628c61', foundational, conditionalities_selectively_enforced_by_geopolitics).
narrative_ontology:cs_axiom_status(conditionalities_selectively_enforced_by_geopolitics, holdable).
narrative_ontology:cs_axiom_grounding('a699e5eb-46af-4a2c-8c6d-b56f51628c61', conditionalities_selectively_enforced_by_geopolitics, empirically_contingent).
narrative_ontology:cs_axiom('a699e5eb-46af-4a2c-8c6d-b56f51628c61', secondary, strategic_waivers_undermine_coordination_legitimacy).
narrative_ontology:cs_axiom_status(strategic_waivers_undermine_coordination_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a699e5eb-46af-4a2c-8c6d-b56f51628c61', strategic_waivers_undermine_coordination_legitimacy, deontological).
narrative_ontology:cs_reference_frame('a699e5eb-46af-4a2c-8c6d-b56f51628c61', post_1980s_debt_crisis_coordination_mandate).
narrative_ontology:cs_drift_state('a699e5eb-46af-4a2c-8c6d-b56f51628c61', contemporary_geopolitical_competition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a699e5eb-46af-4a2c-8c6d-b56f51628c61', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, strategic_debtor_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_nations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_civil_societies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_nations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, strategic_debtor_governments).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__hybrid_selectivity_reading, fiscal_discipline_enhances_creditworthiness).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__hybrid_selectivity_reading, market_confidence_requires_policy_anchors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, negotiate, and enforce conditionality packages attached to sovereign lending. Staff missions monitor compliance; non-compliance triggers program suspension. The institutions justify conditionality as necessary for macroeconomic stability and debt sustainability, but exercise discretion in waiver decisions that correlates with major shareholder geopolitical priorities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, imf_world_bank, agenda_setter,
    institutional, generational, analytical, global).

% Major shareholders (US, EU members, Japan) control IMF/World Bank governance via weighted voting. They benefit when conditionalities open debtor markets, protect creditor repayment capacity, and align debtor policies with creditor commercial and strategic interests. Their exit is arbitrage-grade: they set the rules and face no enforcement.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Geopolitically strategic debtors (e.g., Egypt 1990s, Pakistan 2000s, Ukraine 2014+, Argentina at moments) receive repeated waivers, program extensions, and augmented access despite missing targets. Their alignment with hegemon security interests buys conditionality relief. Exit is mobile: they have alternative financing (bilateral, regional banks, strategic aid).
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states, beneficiary,
    powerful, biographical, mobile, regional).

% Governments of strategically important debtor nations negotiate conditionality terms from a position of leverage: their geopolitical value to the hegemon translates into softer enforcement. They still pay some adjustment costs (subsidy cuts, tariff reductions) but avoid the deepest structural reforms. Their exit is constrained — they need IMF liquidity but have fallback patrons.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, strategic_debtor_governments, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, strategic_debtor_governments, payer).

% Low-income and lower-middle-income countries without geopolitical leverage face full enforcement: prior actions, quarterly reviews, structural benchmarks. Non-compliance means loss of IMF seal of approval, triggering creditor runs and market exclusion. They have no alternative financing at scale; exit is trapped. The adjustment burden falls on public sector wages, subsidies, health/education spending.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_nations, payer,
    powerless, biographical, trapped, national).

% Trade unions, peasant organizations, urban poor movements, and opposition parties in debtor nations bear the lived costs of enforced austerity (user fees, privatization, labor flexibilization) without voice in program design. They are structurally excluded from negotiations; their resistance (protests, strikes) is met with repression or ignored. Exit is trapped — they cannot leave the polity.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_civil_societies, payer,
    powerless, biographical, trapped, local).

% Scholars of development finance, political economy, and international relations document the selectivity pattern: econometric studies show waiver probability correlates with UN voting alignment with G7, US military aid receipts, and strategic location. They have no stake in enforcement but shape the interpretive frame.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, academic_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly solves the coordination problem of sovereign lending under asymmetric information: conditionalities signal borrower commitment to reform, reduce moral hazard, and coordinate dispersed creditors around a common monitoring framework, lowering the cost of capital for compliant borrowers.
% TRANSFER_FUNCTION: Moves policy autonomy and fiscal resources from non-strategic debtor nations (via enforced austerity, privatization, liberalization) to core creditors (via assured repayment, market opening) and to the hegemon (via geopolitical alignment of strategic debtors). Strategic debtors transfer minimal policy autonomy; they receive liquidity without full adjustment.
% ABSENT_VOICES: Debtor-country legislatures, trade unions, peasant movements, indigenous organizations, opposition parties, and local government associations are excluded from conditionality design and review. They are present in the streets (protest) but absent from the table. Their exclusion is structural: IMF Article IV consultations and program negotiations are executive-to-executive.
% DISAPPEARANCE_RATIONALE: If conditionality enforcement vanished overnight, non-strategic debtors would immediately expand public spending, reverse privatizations, and impose capital controls — reshaping domestic political economies. Strategic debtors would lose their privileged access but retain bilateral backstops. Core creditors would face higher default risk and lost market-access leverage. The IMF would lose its primary policy lever.
% FOUNDING_PROBLEM: The 1980s Latin American debt crisis and subsequent African debt crises created a coordination failure: commercial banks would not lend without IMF certification, but the IMF had no enforcement mechanism beyond conditionality. The arrangement was built to restore market access for insolvent sovereigns by substituting official monitoring for market discipline.
% FOUNDING_PROBLEM_CORROBORATION: IMF staff and core shareholder governments attest the founding problem remains live: sovereign debt markets still need a coordination anchor, and conditionalities are the only proven mechanism (IMF 2023 Review of Conditionality). Critics from the Global South (G24 communiqués, UNCTAD reports) and independent economists (e.g., Stiglitz, Gallagher, Kozul-Wright) attest the problem is substantially solved — market access now depends on global liquidity cycles, not IMF programs — and the arrangement persists as a tool of policy coercion. The corroboration is split along the creditor/debtor divide.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the net transfer from non-strategic debtors: they bear the full adjustment burden (fiscal contraction, structural reform) while strategic peers do not. Suppression (0.76) is high because the enforcement machinery (program conditionality, prior actions, quarterly reviews) is actively deployed against weak states and its credibility depends on maintaining the threat. Theater ratio (0.42) is moderate: the coordination function (monitoring, technical assistance, crisis lending) is real and valued by some borrowers, but a growing share of enforcement activity serves the selectivity mechanism rather than the coordination function. Accessibility collapse (0.62) and resistance (0.71) reflect that alternatives (capital controls, default, regional financing) exist but are systematically discouraged or punished.
 *
 * PERSPECTIVAL GAP:
 *   From the IMF staff seat, the constraint is a Rope: a necessary coordination mechanism they administer impartially. From the non-strategic debtor seat, it is a Snare: extraction enforced by an unaccountable external power. From the strategic debtor seat, it is a Rope with a discount: coordination they can afford. The engine computes this per-seat divergence from the structural data; the claim (tangled_rope) captures the system-level hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   IMF/World Bank (agenda_setter, institutional/analytical) sits near the beneficiary end of directionality — they administer the system and face no extraction. Core creditor nations (beneficiary, institutional/arbitrage) are full beneficiaries: they gain market access and repayment assurance at zero enforcement cost. Hegemon-aligned and strategic debtor states (beneficiary/payer, powerful-moderate/mobile-constrained) are net beneficiaries: they receive liquidity with diluted conditionality. Non-strategic debtor nations and their civil societies (payer, powerless/trapped) are full targets: they bear extraction with no exit. The engine derives this gradient from the beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1980s debt crisis coordination) is contested: creditors say it persists; debtors and critics say global liquidity conditions have made IMF certification incidental. The arrangement persists because it serves a new function (geopolitical leverage) that the original mandate did not envision. This is not pure mandatrophy — the coordination function is not dead — but the selectivity mechanism has become the dominant operational logic, converting a Scaffold-like transitional tool into a permanent Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_mechanism_causal,
    'Is the observed waiver pattern for strategic debtors caused by explicit political interference (shareholder directives) or by institutional culture and bureaucratic incentives within the IMF that align with shareholder preferences without direct orders?',
    'Internal IMF staff surveys, leaked communications, board minutes, and comparative case studies of waiver decisions with and without US/EU strategic interest at stake.',
    'If explicit interference, the selectivity is a designed feature of hegemon control; if bureaucratic alignment, it is an emergent property of institutional capture. The former implies reform requires governance change; the latter implies reform requires cultural/incentive change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_mechanism_causal, empirical, 'Whether geopolitical selectivity is designed or emergent.').

omega_variable(
    coordination_function_counterfactual,
    'Would sovereign lending markets coordinate at comparable scale and cost without IMF conditionality, or has the IMF crowded out alternative monitoring mechanisms (regional funds, credit rating agencies, bond covenants)?',
    'Counterfactual analysis of pre-IMF lending eras, regional financing arrangements (Chiang Mai Initiative, FLAR, AMF), and private-sector monitoring substitutes.',
    'If coordination would exist without IMF, the coordination function is not unique to this constraint and the extraction is less justified. If IMF is indispensable, the Tangled Rope classification is strengthened — the coordination function is real and irreplaceable, making the extraction harder to disentangle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_counterfactual, conceptual, 'Whether the coordination function is uniquely performed by IMF conditionality.').

omega_variable(
    kernel_reading_framing,
    'Does the hybrid_selectivity_reading foreclose the creditor_coordination_reading, or do they coexist as descriptions of different operational layers (formal rules vs. enforcement practice)?',
    'Analyze whether IMF official discourse (formal rule layer) can be maintained while the hybrid_selectivity_reading describes the enforcement layer — i.e., whether the readings occupy different levels of abstraction or contradict at the same level.',
    'If they coexist at different layers, the kernel supports multiple simultaneous readings. If they contradict at the same layer, the kernel is irreducibly contested and no single reading can claim descriptive completeness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Structural relationship between this reading and the creditor_coordination_reading sibling.').

omega_variable(
    internalized_suppression_ambiguity,
    'Is the suppression experienced by non-strategic debtor nations primarily structural (market exclusion, aid cutoff) or partially internalized (domestic elites adopt conditionality as their own policy preference, technocratic capture of finance ministries)?',
    'Post-program policy persistence studies: do austerity measures remain after IMF programs end? Elite interviews on policy ownership vs. imposition. Comparison of policy trajectories in countries with similar conditionality exposure but different domestic political economies.',
    'If significantly internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression after formal exit. This would amplify the Snare-like character for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-strategic debtor nations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_hybrid_tr_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(sac_hybrid_tr_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(sac_hybrid_tr_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(sac_hybrid_tr_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(sac_hybrid_tr_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(sac_hybrid_be_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(sac_hybrid_be_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(sac_hybrid_be_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(sac_hybrid_be_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(sac_hybrid_be_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sac_hybrid_su_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(sac_hybrid_su_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(sac_hybrid_su_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(sac_hybrid_su_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(sac_hybrid_su_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2020, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.12).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, sovereign_debt_restructuring_framework).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, multilateral_development_bank_lending_conditionalities).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, bilateral_aid_conditionality_alignment).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, capital_account_management_norm).

% DUAL FORMULATION NOTE:
% Part of the structural_adjustment_conditionalities constraint family (kernel_id: structural_adjustment_conditionalities). This reading (hybrid_selectivity_reading) centers geopolitical enforcement asymmetry. The creditor_coordination_reading centers the monitoring function; the debtor_extraction_reading centers the neo-colonial transfer. All three share the same formal architecture (IMF Article IV, conditionality guidelines) but differ in ε referent: coordination reading assesses ε against the monitoring function (low); extraction reading assesses ε against the transfer to creditors (high); this reading assesses ε against the selective enforcement gap (intermediate, heterogeneous).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, powerful, 0.25).
constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
