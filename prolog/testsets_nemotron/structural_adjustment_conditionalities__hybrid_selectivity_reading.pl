% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: IMF/World Bank Structural Adjustment Conditionalities — Hybrid Selectivity Reading
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint story captures the hybrid selectivity reading of
 *   structural adjustment conditionalities: the IMF and World Bank attach
 *   policy conditions to lending that are ostensibly designed to restore
 *   fiscal sustainability and market confidence (coordination function), but
 *   in practice these conditions are enforced with high severity on
 *   geopolitically non-strategic debtor states while being waived, diluted,
 *   or delayed for states deemed strategically important by the hegemon
 *   (primarily the United States and its allies). The selectivity creates a
 *   Tangled Rope structure: genuine coordination (crisis prevention, creditor
 *   confidence) coexists with asymmetric extraction (non-strategic debtors
 *   bear the full disciplinary burden; strategic debtors receive de facto
 *   subsidies). The arrangement persists because it serves both a
 *   coordination function for the global financial system and an extraction
 *   function for core creditors and the hegemon. Active enforcement is
 *   required — conditionality compliance is monitored through program
 *   reviews, and non-compliance triggers suspension of disbursements, but
 *   this enforcement machinery is selectively deployed.
 *
 * KEY AGENTS:
 *   - hegemon_aligned_states: Primary beneficiaries (institutional/arbitrage) — shape conditionality design and enforce selectivity to serve geopolitical interests
 *   - core_creditor_institutions: Primary beneficiaries (institutional/arbitrage) — IMF, World Bank, major commercial banks; collect debt service, maintain systemic stability
 *   - strategic_debtor_elites: Beneficiaries (powerful/constrained) — receive waivers/dilution; extract domestic rents while retaining external finance access
 *   - non_strategic_debtor_states: Primary victims (moderate/trapped) — face full conditionality enforcement; limited exit from dollar-denominated debt system
 *   - vulnerable_populations_in_program_countries: Primary victims (powerless/trapped) — bear austerity costs (subsidy cuts, public sector retrenchment, user fees) with no voice in program design
 *   - civil_society_resistance_movements: Excluded (organized/constrained) — contest conditionalities domestically and internationally; structurally excluded from negotiation tables
 *   - critical_scholars_analysts: Observers (analytical/analytical) — document selectivity patterns; no formal role in governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.75).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "IMF/World Bank Structural Adjustment Conditionalities — Hybrid Selectivity Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '13298af1-1a7c-4136-960e-2c24de7b7b6d').
narrative_ontology:cs_kernel_codification('13298af1-1a7c-4136-960e-2c24de7b7b6d', formalized).
narrative_ontology:cs_authority_grounding('13298af1-1a7c-4136-960e-2c24de7b7b6d', extraction).
narrative_ontology:cs_interpretation_layer_present('13298af1-1a7c-4136-960e-2c24de7b7b6d').
narrative_ontology:cs_reading_relation('13298af1-1a7c-4136-960e-2c24de7b7b6d', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('13298af1-1a7c-4136-960e-2c24de7b7b6d', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('13298af1-1a7c-4136-960e-2c24de7b7b6d', foundational, conditionalities_selectively_enforced_by_geopolitical_position).
narrative_ontology:cs_axiom_status(conditionalities_selectively_enforced_by_geopolitical_position, holdable).
narrative_ontology:cs_axiom_grounding('13298af1-1a7c-4136-960e-2c24de7b7b6d', conditionalities_selectively_enforced_by_geopolitical_position, empirically_contingent).
narrative_ontology:cs_axiom('13298af1-1a7c-4136-960e-2c24de7b7b6d', foundational, coordination_and_extraction_fused_through_selectivity).
narrative_ontology:cs_axiom_status(coordination_and_extraction_fused_through_selectivity, holdable).
narrative_ontology:cs_axiom_grounding('13298af1-1a7c-4136-960e-2c24de7b7b6d', coordination_and_extraction_fused_through_selectivity, empirically_contingent).
narrative_ontology:cs_reference_frame('13298af1-1a7c-4136-960e-2c24de7b7b6d', post_1980s_debt_crisis_conditional_lending_framework).
narrative_ontology:cs_drift_state('13298af1-1a7c-4136-960e-2c24de7b7b6d', post_cold_war_unipolar_hegemony, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('13298af1-1a7c-4136-960e-2c24de7b7b6d', '2026-08-14T12:00:00Z').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, strategic_debtor_elites).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_program_countries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, strategic_debtor_elites).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__hybrid_selectivity_reading, conditionalities_as_selective_discipline).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitical_exceptionalism_in_financial_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape conditionality design through veto power in IMF/World Bank governance; use selectivity to advance geopolitical objectives (alliance maintenance, resource access, strategic positioning) while core creditors enforce the financial discipline. They do not bear conditionality costs — they set the rules others follow.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states, beneficiary,
    institutional, generational, arbitrage, global).

% IMF, World Bank, major commercial banks, and Paris Club creditors. They administer conditionality programs, monitor compliance, and control disbursement. They collect debt service and maintain systemic stability. Their enforcement discretion enables selectivity — they can waive, delay, or dilute conditions for strategic debtors while enforcing strictly on others. They benefit from both the coordination function (systemic stability) and the extraction function (debt service flows).
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Governments of geopolitically strategic states (e.g., Egypt, Pakistan, Turkey at various periods; Ukraine post-2014). They receive conditionality waivers, extended timelines, or program cancellations without penalty. They capture domestic rents from the fiscal space created by waivers (military spending, patronage, elite consumption) while retaining access to external finance. Their exit is constrained — they need the dollar system but have leverage to negotiate terms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, strategic_debtor_elites, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, strategic_debtor_elites, payer).

% Governments of non-strategic debtor states (e.g., Sub-Saharan African countries, Latin American countries in 1980s-90s, Jamaica, Ghana, Zambia). They face full conditionality enforcement: fiscal austerity, privatization, liberalization, subsidy removal. Non-compliance triggers disbursement suspension and market exclusion. Their exit options are severely limited: no alternative lender of last resort, dollar-denominated debt obligations, trade dependence on creditor-country markets. They bear the disciplinary costs while strategic peers do not.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states, payer,
    moderate, biographical, trapped, national).

% Poor and working-class populations in program countries. They experience conditionalities as collapsed public services (health, education, water), user fees, public sector job losses, food subsidy removal, and wage suppression. They have no voice in program design, no exit from citizenship/poverty, and internalize the suppression — the constraint reorganizes their survival conditions. Their identity is fused with the compromised public institutions they depend on.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_program_countries, payer,
    powerless, biographical, identity_locked, local).

% Trade unions, student movements, peasant organizations, NGOs, and political opposition in program countries. They contest conditionalities through protests, strikes, policy advocacy, and international solidarity networks. They are structurally excluded from negotiation tables (IMF Article IV consultations, program design meetings). Their resistance occasionally forces modifications but rarely alters the structural selectivity. They experience the constraint as suppression of democratic policy space.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, civil_society_resistance_movements, excluded,
    organized, biographical, constrained, national).

% Academic researchers (development economics, political economy, international relations), investigative journalists, and independent policy analysts. They document the selectivity pattern, model its effects, and publish critiques. They have no formal role in governance but shape the discursive field. Their exit is analytical — they can leave the field but the constraint's structural reality persists regardless of their analysis.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, critical_scholars_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a lender-of-last-resort function for sovereigns in crisis, coordinates creditor expectations to prevent runs, and imposes fiscal/monetary discipline that (in principle) restores market access and debt sustainability.
% TRANSFER_FUNCTION: Moves fiscal resources and policy autonomy from non-strategic debtor states (and their populations) to core creditors (debt service) and the hegemon (geopolitical compliance), while strategic debtor elites capture the differential (waived conditions, retained fiscal space for domestic priorities).
% ABSENT_VOICES: Vulnerable populations in program countries (no vote in IMF/World Bank, no seat at program negotiation); future generations who inherit depleted public assets and ecological damage from forced liberalization; debtor-country legislatures bypassed by executive agreements with IFIs.
% DISAPPEARANCE_RATIONALE: If conditionalities and their selective enforcement vanished overnight, non-strategic debtors would immediately regain policy space (capital controls, industrial policy, social spending); strategic debtors would lose their privileged access terms; core creditors would face higher default risk and need new coordination mechanisms; the hegemon would lose a primary instrument of financial statecraft. The global debt architecture would reorganize around either unconditional financing, regional alternatives, or systemic crisis.
% FOUNDING_PROBLEM: The 1980s sovereign debt crisis threatened global financial stability: commercial banks faced massive defaults, debtor states lost market access, and no mechanism existed to coordinate restructuring. Conditionalities were designed to restore creditor confidence and debtor solvency simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Core creditors and hegemon-aligned states attest the problem remains live (recurring crises, fiscal indiscipline, moral hazard). Critical scholars, debtor-country governments, and UN development agencies attest the problem is substantially solved for strategic debtors (who access finance without conditionality) but weaponized against non-strategic debtors — the arrangement now serves extraction and geopolitical discipline. No single corroborating source outside the beneficiary set validates the original mandate as currently operative.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness (0.68) reflects that non-strategic debtors transfer substantial resources (fiscal surpluses, policy autonomy, social spending) to creditors under conditions that strategic debtors avoid. Suppression (0.75) is high because the constraint's persistence depends on active enforcement — program conditionality monitoring, disbursement conditionality, and the threat of financial isolation for non-compliance — and alternatives (capital controls, default, regional financing) are structurally suppressed. Theater ratio (0.42) is significant: the coordination rhetoric (fiscal sustainability, market confidence) is real but increasingly performs a legitimating function for selective enforcement. Accessibility collapse (0.62) is moderate-high: once a country enters the conditional lending framework, alternatives (sovereign default, regional alternatives, capital controls) are severely constrained by the architecture of the dollar system. Resistance (0.58) is substantial: debtor governments, social movements, and some creditor-country legislatures contest conditionalities, but resistance is fragmented and rarely alters the structural selectivity.
 *
 * PERSPECTIVAL GAP:
 *   From the hegemon/creditor seat, the constraint appears as a Rope with necessary selectivity — coordination with pragmatic exceptions. From the non-strategic debtor state seat, it appears as a Snare — extraction enforced by a hegemon that exempts its allies. From the vulnerable population seat, it is a Snare with identity-locked suppression — the constraint reorganizes their survival conditions while denying them voice. The engine computes these per-seat classifications from the declared structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The hegemon and core creditors are structural beneficiaries (d near 0.0): they design the rules, control enforcement discretion, and collect the financial and geopolitical returns. Strategic debtor elites are secondary beneficiaries (d ~ 0.2-0.3): they receive waivers but remain dependent on the system. Non-strategic debtor states are primary targets (d ~ 0.8-0.9): they face full enforcement with constrained exit (trapped in dollar-denominated debt, no alternative lender of last resort). Vulnerable populations are identity-locked targets (d ~ 0.95): their exit is blocked by citizenship, poverty, and the collapse of public services — they carry the suppression internally. Civil society resistance is excluded from the coordination table (role=excluded) but experiences the constraint as suppression. Critical scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1980s debt crisis resolution and market access restoration) is contested: creditors argue it remains live (recurring crises, fiscal indiscipline); critics argue it is dead (conditionalities now serve creditor extraction and geopolitical discipline, not crisis resolution). The arrangement persists because the hegemon and core creditors benefit from both the coordination function (systemic stability) and the extraction function (resource transfers, geopolitical leverage). No single party bears enough cost to dismantle it, and no party benefits enough from the coordination alone to maintain it without the extraction — classic mandatrophy where the mandate has outlived its function but the structure persists through selective enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the structural_adjustment_conditionalities kernel, or does it describe the kernel itself?',
    'Compare the beneficiary/victim structure and enforcement pattern across the three declared readings (creditor_coordination_reading, debtor_extraction_reading, hybrid_selectivity_reading). If each reading instantiates a different victim set and different extraction profile, they are distinct constraints sharing a kernel.',
    'If confirmed as a reading, the ε-invariance principle holds: this story''s ε refers to the standing arrangement as the hybrid selectivity reading sees it — selective enforcement that extracts from non-strategic debtors while subsidizing strategic ones. The sibling readings would author different ε values over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether hybrid_selectivity_reading is a distinct constraint reading of the kernel').

omega_variable(
    selectivity_mechanism_ambiguity,
    'Is the selective application of conditionalities an explicit policy choice by creditor institutions, or an emergent property of bargaining power asymmetries?',
    'Analyze IMF/World Bank board minutes, conditionality waiver records, and voting patterns on program approvals for strategic vs. non-strategic debtors. Look for explicit geopolitical criteria in program design documents.',
    'If explicit policy, the constraint is a designed instrument of geopolitical discipline (stronger tangled_rope/snare characterization). If emergent, the selectivity is a structural outcome of power asymmetries that could shift with coalition changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_mechanism_ambiguity, empirical, 'Whether selectivity is designed or emergent').

omega_variable(
    strategic_debtor_benefit_capture,
    'Do strategic debtor elites capture the gains from conditionalities waivers, or do waivers primarily benefit the hegemon''s geopolitical position?',
    'Track resource flows in waived programs: do freed fiscal resources go to elite consumption, security apparatus, or genuine development? Compare with non-waived program countries.',
    'If elites capture gains, strategic_debtor_elites are beneficiaries in the extraction sense. If gains flow to geopolitical alignment, the hegemon is the primary beneficiary and strategic debtors are instrumentally tolerated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_debtor_benefit_capture, empirical, 'Who captures the rents from selective enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_cond_sel_tr_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(sa_cond_sel_tr_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(sa_cond_sel_tr_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(sa_cond_sel_tr_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(sa_cond_sel_tr_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(sa_cond_sel_be_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(sa_cond_sel_be_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(sa_cond_sel_be_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(sa_cond_sel_be_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(sa_cond_sel_be_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sa_cond_sel_su_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(sa_cond_sel_su_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(sa_cond_sel_su_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(sa_cond_sel_su_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(sa_cond_sel_su_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2020, 0.75).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2020
narrative_ontology:measurement(sa_cond_sel_grid_01, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 1980, 0.5).
narrative_ontology:measurement(sa_cond_sel_grid_02, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 2020, 0.7).
narrative_ontology:measurement(sa_cond_sel_grid_03, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 1980, 0.4).
narrative_ontology:measurement(sa_cond_sel_grid_04, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 2020, 0.65).
narrative_ontology:measurement(sa_cond_sel_grid_05, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 1980, 0.35).
narrative_ontology:measurement(sa_cond_sel_grid_06, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 2020, 0.55).
narrative_ontology:measurement(sa_cond_sel_grid_07, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 1980, 0.6).
narrative_ontology:measurement(sa_cond_sel_grid_08, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 2020, 0.75).
narrative_ontology:measurement(sa_cond_sel_grid_09, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 1980, 0.5).
narrative_ontology:measurement(sa_cond_sel_grid_10, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 2020, 0.65).
narrative_ontology:measurement(sa_cond_sel_grid_11, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 1980, 0.35).
narrative_ontology:measurement(sa_cond_sel_grid_12, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 2020, 0.5).
narrative_ontology:measurement(sa_cond_sel_grid_13, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 1980, 0.4).
narrative_ontology:measurement(sa_cond_sel_grid_14, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 2020, 0.55).
narrative_ontology:measurement(sa_cond_sel_grid_15, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 1980, 0.45).
narrative_ontology:measurement(sa_cond_sel_grid_16, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 2020, 0.6).
narrative_ontology:measurement(sa_cond_sel_grid_17, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 1980, 0.45).
narrative_ontology:measurement(sa_cond_sel_grid_18, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 2020, 0.68).
narrative_ontology:measurement(sa_cond_sel_grid_19, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 1980, 0.3).
narrative_ontology:measurement(sa_cond_sel_grid_20, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 2020, 0.55).
narrative_ontology:measurement(sa_cond_sel_grid_21, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 1980, 0.4).
narrative_ontology:measurement(sa_cond_sel_grid_22, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 2020, 0.6).
narrative_ontology:measurement(sa_cond_sel_grid_23, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 1980, 0.5).
narrative_ontology:measurement(sa_cond_sel_grid_24, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 2020, 0.72).
narrative_ontology:measurement(sa_cond_sel_grid_25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 1980, 0.55).
narrative_ontology:measurement(sa_cond_sel_grid_26, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 2020, 0.78).
narrative_ontology:measurement(sa_cond_sel_grid_27, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 1980, 0.45).
narrative_ontology:measurement(sa_cond_sel_grid_28, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 2020, 0.7).
narrative_ontology:measurement(sa_cond_sel_grid_29, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 1980, 0.5).
narrative_ontology:measurement(sa_cond_sel_grid_30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 2020, 0.72).
narrative_ontology:measurement(sa_cond_sel_grid_31, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 1980, 0.6).
narrative_ontology:measurement(sa_cond_sel_grid_32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.18).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, sovereign_debt_architecture).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, global_financial_safety_net).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_finance_conditionality).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the structural_adjustment_conditionalities kernel. The creditor_coordination_reading (ε ≈ 0.25, claimed rope) emphasizes the coordination function. The debtor_extraction_reading (ε ≈ 0.85, claimed snare) emphasizes the extraction function. This hybrid_selectivity_reading (ε = 0.68, claimed tangled_rope) captures the structural fusion: genuine coordination that operates through geopolitically selective enforcement, extracting from non-strategic debtors while subsidizing strategic ones. All three stories link via network.affects_constraints to form the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, powerful, 0.25).
constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, moderate, 0.85).
constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
