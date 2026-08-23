% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities â Creditor Coordination Reading
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the creditor_coordination_reading of
 *   the contested kernel structural_adjustment_conditionalities. Under this
 *   reading, conditionality attached to sovereign lending by multilateral
 *   institutions is a Rope: a coordination mechanism solving the
 *   time-inconsistency problem in debtor-creditor relations and the
 *   collective-action problem among creditors. The arrangement is read as
 *   generating net benefits through credible commitment to fiscal
 *   sustainability, with costs falling narrowly on inefficient state sectors
 *   that absorb rents. The sibling debtor_extraction_reading reads the same
 *   kernel as a Snare of neo-colonial extraction, while the
 *   hybrid_selectivity_reading sees it as a Tangled Rope of asymmetric
 *   enforcement. The authored metrics are indexed to this reading's
 *   assessment of the standing arrangement: low extractiveness and
 *   suppression, reflecting the view that enforcement is proportionate to a
 *   genuine coordination failure and that alternatives (unconditional lending
 *   to distressed sovereigns) are inferior rather than suppressed.
 *
 * KEY AGENTS:
 *   - multilateral_creditor_institutions: Agenda-setter (institutional/arbitrage/global) â sets and monitors conditionality
 *   - future_taxpayers: Primary beneficiary (powerless/trapped/national) â gains from debt sustainability
 *   - international_capital: Primary beneficiary (powerful/arbitrage/global) â gains from standardized risk reduction
 *   - inefficient_state_sectors: Target/payer (moderate/constrained/national) â bears direct costs of liberalization and consolidation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.3).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.25).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities â Creditor Coordination Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'd2aad72c-3970-4a25-832c-848be44c5321').
narrative_ontology:cs_kernel_codification('d2aad72c-3970-4a25-832c-848be44c5321', formalized).
narrative_ontology:cs_authority_grounding('d2aad72c-3970-4a25-832c-848be44c5321', expertise).
narrative_ontology:cs_interpretation_layer_present('d2aad72c-3970-4a25-832c-848be44c5321').
narrative_ontology:cs_reading_relation('d2aad72c-3970-4a25-832c-848be44c5321', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2aad72c-3970-4a25-832c-848be44c5321', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('d2aad72c-3970-4a25-832c-848be44c5321', foundational, external_conditionality_solves_time_inconsistency).
narrative_ontology:cs_axiom_status(external_conditionality_solves_time_inconsistency, holdable).
narrative_ontology:cs_axiom_grounding('d2aad72c-3970-4a25-832c-848be44c5321', external_conditionality_solves_time_inconsistency, empirically_contingent).
narrative_ontology:cs_axiom('d2aad72c-3970-4a25-832c-848be44c5321', foundational, creditor_coordination_prevents_free_riding).
narrative_ontology:cs_axiom_status(creditor_coordination_prevents_free_riding, holdable).
narrative_ontology:cs_axiom_grounding('d2aad72c-3970-4a25-832c-848be44c5321', creditor_coordination_prevents_free_riding, empirically_contingent).
narrative_ontology:cs_reference_frame('d2aad72c-3970-4a25-832c-848be44c5321', sustainable_sovereign_finance_equilibrium).
narrative_ontology:cs_drift_state('d2aad72c-3970-4a25-832c-848be44c5321', contemporary_multipolar_finance_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('d2aad72c-3970-4a25-832c-848be44c5321', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, time_inconsistency_theory).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, macroeconomic_stabilization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets macroeconomic conditionality terms in sovereign loan agreements, monitors compliance through periodic Article IV reviews and tranched disbursements, and coordinates creditor clubs to prevent free-riding; adjusts frameworks as economic doctrine evolves.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, multilateral_creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Are the diffuse, unorganized beneficiaries of reduced sovereign debt burdens and sustainable public finance; they do not participate in negotiations but are invoked as the ultimate beneficiaries of fiscal discipline and intergenerational equity.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers, beneficiary,
    powerless, generational, trapped, national).

% Provides portfolio and direct investment to debtor countries; benefits from standardized macroeconomic rules and surveillance that reduce default risk and information asymmetry; can reallocate globally if conditions deteriorate.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital, beneficiary,
    powerful, biographical, arbitrage, global).

% Comprises public enterprises, protected industries, and public-sector workers who lose subsidies, employment guarantees, and regulatory protection under privatization, fiscal consolidation, and market liberalization mandates; they bear the direct adjustment costs but are framed by the arrangement as obstacles to efficiency.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors, payer,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sovereign debtors face a time-inconsistency problem: governments promise fiscal reform to secure financing but have domestic political incentives to renege after disbursement. Multilateral creditors face a collective-action problem in monitoring debtor behavior and coordinating rescue packages. Conditionality solves both by making disbursement contingent on externally verified reform steps, creating a credible commitment device that aligns short-term political incentives with long-term solvency.
% TRANSFER_FUNCTION: Moves policy credibility and reduced default risk from multilateral institutions to debtor governments; moves fiscal space and protective regulation away from inefficient state sectors toward future taxpayers and international investors via consolidation and market liberalization.
% ABSENT_VOICES: Informal-sector workers and rural poor who lose social services during adjustment but have no seat at Paris Club or IFI negotiations; heterodox development economists advocating non-conditional financing or capital-account management; debtor-country civil society organizations systematically excluded from program design.
% DISAPPEARANCE_RATIONALE: If conditionality vanished overnight, the credibility mechanism for sovereign reform would dissolve; debtor governments would face sharply higher risk premia, creditor coordination would fragment into inefficient bilateral bargaining, and macroeconomic stabilization paths would unravel as domestic incumbents reversed consolidation measures.
% FOUNDING_PROBLEM: Sovereign debt markets suffer from time-inconsistency and creditor coordination failures that produce repeated default crises, stop-go reform cycles, and inefficient bilateral bailouts.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by institutional economists studying sovereign commitment (e.g., Drazen, Easterly) and by historical debt-crisis episodes (1980s Latin America, 1990s East Asia), but contested by critical development scholars (UNCTAD, heterodox economists) and civil society actors outside the creditor-beneficiary complex who argue the problem is misdiagnosed.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).
:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.30 (low-moderate) because this reading treats conditionality as a service-cost overhead of credible commitment rather than rent extraction. Suppression is low (0.25) because the reading views alternatives (unconditional crisis lending) as genuinely inferior due to moral hazard, not as actively suppressed. Theater ratio is low (0.20) because surveillance and tranched disbursement are read as functional enforcement of a real contract, not performance. Accessibility collapse is moderate-high (0.60) because once the time-inconsistency logic is accepted, unconditional alternatives collapse intellectually; resistance (0.35) comes from domestic incumbents who lose rents but are framed as opposing efficiency rather than resisting exploitation.
 *
 * PERSPECTIVAL GAP:
 *   The creditor institutions and international capital compute low directionality (near-beneficiaries) because the constraint subsidizes their risk management and coordination. Inefficient state sectors compute high directionality (near-targets) because the constraint directly removes their protections and transfers resources away from them. Future taxpayers are diffuse beneficiaries with low power and no exit, so their effective extraction is damped into subsidy despite their structural immobility.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to agents who gain from reduced default risk and credible reform: future_taxpayers receive fiscal sustainability, international capital receives standardized risk metrics. The payer declaration maps to inefficient state sectors who bear the direct costs of liberalization and consolidation. The creditor institutions set the agenda but, in this reading, do not capture rents â they provide a coordination service.
 *
 * MANDATROPHY ANALYSIS:
 *   The Rope classification prevents misreading the creditor institutions' enforcement activity as extraction: the reading holds that the same enforcement (surveillance, conditionality lists) would be necessary even if no institution profited, because the problem is the sovereign's inability to commit. If the founding problem (time-inconsistency) were dead, the persistence of the arrangement would signal Piton or Snare; authored as live, the classification remains Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creditor_reading_kernel_position,
    'This constraint is one reading of kernel structural_adjustment_conditionalities; would adopting the sibling debtor_extraction_reading reclassify the same arrangement as a snare with high extraction and broad victimization?',
    'Comparative case analysis of conditional vs unconditional lending outcomes, and examination of net transfer direction (debtor to creditor vs creditor to debtor).',
    'If net transfers flow to creditors and conditionalities systematically overrule domestic policy autonomy, the creditor reading''s rope classification fails; if reforms persist and default rates fall, the coordination reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_reading_kernel_position, conceptual, 'Kernel reading contestation between coordination and extraction framings').

omega_variable(
    sovereign_time_inconsistency_genuine,
    'Is sovereign time-inconsistency in fiscal policy a genuine structural problem that requires external enforcement, or can domestic institutions solve it without conditionality?',
    'Panel analysis comparing reform persistence under IMF programs vs domestically initiated reforms in middle-income countries with strong institutions.',
    'If domestic institutions can credibly commit without external conditionality, the coordination function collapses and the constraint reclassifies toward extraction or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_time_inconsistency_genuine, empirical, 'Whether the coordination problem is endogenous or constructed').

omega_variable(
    inefficient_sector_vs_broad_population,
    'Do the costs of structural adjustment fall only on inefficient state sectors, or do they diffuse to poor and vulnerable populations through reduced social spending and devaluation?',
    'Distributional incidence studies of IMF program episodes, tracking consumption and health outcomes across income quintiles.',
    'If costs fall broadly on the poor, the victim set expands beyond inefficient sectors and the reading''s rope claim becomes unsustainable; if costs are absorbed by rent-seeking incumbents, the rope claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inefficient_sector_vs_broad_population, empirical, 'Scope of adjustment costs beyond state-sector incumbents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 32, 0.17).
narrative_ontology:measurement(stru_tr_t44, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 44, 0.2).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(stru_be_t44, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 44, 0.3).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(structural_adjustment_conditionalities__creditor_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
