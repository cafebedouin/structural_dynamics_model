% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Selectively Enforced IMF/Creditor Conditionality Regime
 *   domain: international political economy / development finance
 *
 * SUMMARY:
 *   This story instantiates the hybrid_selectivity_reading of the
 *   structural_adjustment_conditionalities kernel: the claim that
 *   conditionality's coordination function is real but its enforcement is
 *   systematically non-uniform, tracking geopolitical alignment rather than
 *   uniform fiscal risk criteria. Non-strategic debtor states face rigorous,
 *   front-loaded discipline; hegemon-aligned debtors receive waivers and
 *   softened targets for equivalent or worse fiscal indicators. The
 *   coordination story (solving sovereign lending's commitment problem) is
 *   genuine, but its application is captured by core creditor governments'
 *   foreign policy interests, producing asymmetric extraction concentrated on
 *   the powerless. This is a distinct constraint from the
 *   creditor_coordination_reading (which holds enforcement is uniformly
 *   necessary and largely consistent) and the debtor_extraction_reading
 *   (which holds the entire coordination story is cover for extraction with
 *   no genuine coordination function at all) — this reading occupies the
 *   structural middle: real coordination function, captured and selectively
 *   weaponized application.
 *
 * KEY AGENTS:
 *   - multilateral_lending_institutions: agenda_setter/institutional — administers conditionality design and waiver discretion
 *   - core_creditor_governments: beneficiary/agenda_setter/institutional — uses selective enforcement as foreign policy leverage
 *   - hegemon_aligned_debtor_states: beneficiary/powerful — receives de facto leniency
 *   - peripheral_non_strategic_debtor_states: payer/powerless/trapped — bears rigorous enforcement
 *   - public_sector_workers_in_adjusting_states: payer/powerless/trapped — transmission channel of enforced discipline
 *   - domestic_populations_facing_austerity: payer/powerless/trapped — downstream social cost bearer
 *   - bondholders_and_private_creditors: beneficiary/organized/mobile — protected by enforced fiscal priority on debt service
 *   - independent_debt_sustainability_analysts: observer/analytical — documents the alignment-enforcement correlation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.71).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Selectively Enforced IMF/Creditor Conditionality Regime").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international political economy / development finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '07e7db7e-8366-4ea9-a58a-a0cbab10c77a').
narrative_ontology:cs_kernel_codification('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', formalized).
narrative_ontology:cs_authority_grounding('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', extraction).
narrative_ontology:cs_interpretation_layer_present('07e7db7e-8366-4ea9-a58a-a0cbab10c77a').
narrative_ontology:cs_reading_relation('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', structural_adjustment_conditionalities__debtor_extraction_reading, influences).
narrative_ontology:cs_axiom('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', foundational, enforcement_selectivity_is_the_operative_variable).
narrative_ontology:cs_axiom_status(enforcement_selectivity_is_the_operative_variable, holdable).
narrative_ontology:cs_axiom_grounding('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', enforcement_selectivity_is_the_operative_variable, empirically_contingent).
narrative_ontology:cs_axiom('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', foundational, coordination_function_and_capture_coexist_in_one_mechanism).
narrative_ontology:cs_axiom_status(coordination_function_and_capture_coexist_in_one_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', coordination_function_and_capture_coexist_in_one_mechanism, conventional).
narrative_ontology:cs_reference_frame('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', uniform_criteria_based_conditionality_design).
narrative_ontology:cs_drift_state('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', post_cold_war_geopolitical_realignment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('07e7db7e-8366-4ea9-a58a-a0cbab10c77a', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_debtor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_governments).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, multilateral_lending_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, peripheral_non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, public_sector_workers_in_adjusting_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, domestic_populations_facing_austerity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, bondholders_and_private_creditors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the conditionality frameworks — fiscal targets, privatization schedules, subsidy removals — attached to lending programs. Retains discretion over waivers, program design flexibility, and enforcement intensity, and its institutional legitimacy and staff mandate depend on the conditionality apparatus continuing to exist and appear technocratically neutral.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, multilateral_lending_institutions, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, multilateral_lending_institutions, beneficiary).

% Hold effective voting weight and informal veto power inside the lending institutions' governance structures. Use conditionality enforcement selectively to protect their own banks' exposure, open markets for their firms, and reward or punish debtor states based on alignment with their foreign policy objectives, not on uniform fiscal criteria.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_governments, agenda_setter).

% Receive waivers, delayed reviews, softened targets, or outright program restructuring when conditionality compliance would be politically destabilizing, because their continued alignment (military basing, votes in international fora, resource access) is worth more to core creditors than strict enforcement. Experience the same nominal rules as their peripheral counterparts but face negligible real consequences for noncompliance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_debtor_states, beneficiary,
    powerful, biographical, arbitrage, national).

% Face rigorous, often front-loaded conditionality enforcement — currency devaluation, subsidy elimination, public payroll cuts — with little room for negotiation because they lack the geopolitical leverage to make deviation costly for creditors. Default or exit from the lending relationship threatens sovereign borrowing capacity entirely, so exit is nominal, not real.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, peripheral_non_strategic_debtor_states, payer,
    powerless, biographical, trapped, national).

% Absorb layoffs, wage freezes, and pension restructuring mandated by conditionality programs in non-strategic states. Have no voice in program design and cannot exit their national economy without personal migration costs; their labor conditions are the direct transmission channel of the enforced discipline.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, public_sector_workers_in_adjusting_states, payer,
    powerless, biographical, trapped, national).

% Experience reduced subsidies, healthcare and education cuts, and currency devaluation effects on real wages as conditionality's downstream social cost. Bear the consequences of a bargain struck between their government and external creditors in which they had no seat.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, domestic_populations_facing_austerity, payer,
    powerless, biographical, trapped, national).

% Benefit from conditionality-enforced fiscal discipline that prioritizes debt service over domestic spending in non-strategic states, protecting their claims. Can exit individual sovereign exposures relatively freely by trading in secondary bond markets, unlike the debtor populations bearing the adjustment.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, bondholders_and_private_creditors, beneficiary,
    organized, biographical, mobile, global).

% Study cross-country conditionality enforcement records and document the correlation between geopolitical alignment and program leniency. Their findings are cited in academic and policy debate but do not themselves alter program design.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, independent_debt_sustainability_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_governments).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Conditionality frameworks genuinely solve a real coordination problem: without some enforceable discipline mechanism, sovereign borrowers have weak commitment devices, and lending institutions and creditor syndicates need assurance that borrowed funds will be used in ways compatible with eventual repayment, preventing a collective-action collapse in sovereign credit markets.
% TRANSFER_FUNCTION: Moves fiscal adjustment burden and enforcement intensity asymmetrically: rigorous discipline and austerity costs flow onto non-strategic debtor populations and their public sectors, while geopolitically strategic debtors and core creditor-aligned bondholders retain access to lending, protected debt service, and market confidence without proportional adjustment cost.
% ABSENT_VOICES: Populations in non-strategic debtor states — the workers, pensioners, and public-service users who bear the transmission costs of enforced conditionality — have no seat in program negotiation, which occurs between technocratic institution staff and national finance ministries under creditor governance oversight. Strategic debtors' populations are similarly absent, but the leniency they receive means the absence has less practical bite.
% DISAPPEARANCE_RATIONALE: If selective conditionality enforcement disappeared overnight — either by universal strict enforcement or by universal leniency — the leverage core creditors currently exercise over non-strategic debtor policy would collapse, sovereign lending terms would need to be renegotiated on more uniform criteria, and the geopolitical utility of lending institutions as instruments of alignment-reward would be lost. Debt markets, foreign policy bargaining, and domestic fiscal policy in dozens of states would all reorganize.
% FOUNDING_PROBLEM: Sovereign lending faced a genuine commitment problem: without conditions attached to loans, debtor governments could borrow, spend on politically expedient priorities, and default, leaving creditors with no recourse and future lending markets undermined for everyone.
% FOUNDING_PROBLEM_CORROBORATION: Multilateral institution staff and core creditor governments attest the founding problem (fiscal indiscipline, moral hazard) remains live and justifies conditionality broadly. Independent debt sustainability analysts and comparative political economy researchers — outside the benefiting institutional and creditor-state parties — attest that enforcement intensity tracks geopolitical alignment far more strongly than it tracks measured fiscal risk, indicating the founding coordination problem has been substantially supplanted by a selective-discipline function serving creditor foreign policy interests.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.68 at interval end) reflects that a real coordination function exists (sovereign commitment problems are genuine) but is substantially captured — the gap between nominal criteria and actual enforcement outcomes is the extractive residue. Suppression (0.71) is high because non-strategic debtors face near-total loss of negotiating leverage once a program begins; exit from the lending relationship threatens sovereign credit access entirely. Theater ratio (0.52) is elevated because technocratic, criteria-based language increasingly masks what is, on the documented record, a political sorting mechanism — this is the diagnostic signature this reading is built to surface. All temporal series share one grid (T=0 to 40, six points) so no metric is back-filled from an end-state value.
 *
 * DIRECTIONALITY LOGIC:
 *   Core creditor governments and hegemon-aligned debtors sit near the full-beneficiary end: creditors set and selectively waive terms; aligned debtors receive real benefit (continued access, softened terms) without proportional adjustment cost. Peripheral debtor states and their populations sit near the full-target end: trapped exit options (sovereign credit dependency), no negotiating leverage, and the full transmission of adjustment costs. Bondholders benefit indirectly through protected debt service priority even though they hold no formal enforcement role — their mobility (secondary market exit) differentiates them structurally from the state-level payers who cannot exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabeling errors: treating the whole apparatus as pure Rope (which would erase the documented selectivity and its victims) or as pure Snare (which would deny the genuine commitment-problem function that justifies conditionality's existence in cases where it is applied evenly). The founding problem is contested-status, not simply dead: fiscal indiscipline risk is real, but the mechanism built to address it now operates as a selective-discipline instrument whose primary variable is geopolitical alignment, not fiscal risk — exactly the mismatch the R5 corroboration surface is designed to expose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_measurement_ambiguity,
    'Is the observed correlation between geopolitical alignment and enforcement leniency a causal capture mechanism, or does it reflect confounded fiscal fundamentals (aligned states may also have stronger institutions)?',
    'Matched-pair comparative case analysis controlling for fiscal indicators (debt-to-GDP, reserve adequacy, growth trajectory) across strategic and non-strategic debtors with similar fundamentals but different alignment status, examining program design and waiver-grant timing.',
    'If the correlation survives fundamentals controls, it strongly corroborates the hybrid_selectivity_reading''s core claim over the creditor_coordination_reading''s uniform-application premise. If it does not survive controls, this reading''s distinguishing claim weakens substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_measurement_ambiguity, empirical, 'Whether alignment-correlated leniency is causal capture or confounded by fiscal fundamentals.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (creditor_coordination, debtor_extraction, hybrid_selectivity) disagree — is it about the EXISTENCE of a genuine coordination function, or only about its UNIFORMITY of application?',
    'This constraint''s own structural commitment: the coordination function exists (contra debtor_extraction_reading) but its application is non-uniform and alignment-driven (contra creditor_coordination_reading''s uniform-application premise). The disagreement locus is the application-uniformity axis, not the existence-of-coordination axis.',
    'Fixes which structural claim differentiates this reading from each sibling and prevents this story from silently drifting toward either sibling''s position during future updates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Committer structure: locates the exact axis of disagreement among sibling kernel readings.').

omega_variable(
    waiver_discretion_institutional_capture,
    'Is the multilateral lending institution''s waiver discretion itself captured by core creditor governance weight, or does it retain independent technocratic judgment that is merely correlated with, but not driven by, creditor preferences?',
    'Internal governance vote-record analysis on waiver decisions, cross-referenced against creditor government public statements and diplomatic pressure documented in leaked cables or FOIA-released communications.',
    'If waiver discretion is shown to be substantively independent, the agenda_setter seat''s classification shifts toward genuine coordination intermediary; if captured, it reinforces the beneficiary/agenda_setter dual role authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waiver_discretion_institutional_capture, empirical, 'Whether institutional waiver discretion is independent or captured by dominant creditor governments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(stru_grid_01, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement(stru_grid_02, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 40, 0.65).
narrative_ontology:measurement(stru_grid_03, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(stru_grid_04, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 40, 0.7).
narrative_ontology:measurement(stru_grid_05, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 0, 0.35).
narrative_ontology:measurement(stru_grid_06, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 40, 0.5).
narrative_ontology:measurement(stru_grid_07, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 0, 0.4).
narrative_ontology:measurement(stru_grid_08, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 40, 0.58).
narrative_ontology:measurement(stru_grid_09, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 0, 0.35).
narrative_ontology:measurement(stru_grid_10, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 40, 0.58).
narrative_ontology:measurement(stru_grid_11, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 0, 0.2).
narrative_ontology:measurement(stru_grid_12, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 40, 0.32).
narrative_ontology:measurement(stru_grid_13, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 0, 0.25).
narrative_ontology:measurement(stru_grid_14, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 40, 0.4).
narrative_ontology:measurement(stru_grid_15, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 0, 0.3).
narrative_ontology:measurement(stru_grid_16, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 40, 0.45).
narrative_ontology:measurement(stru_grid_17, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 0, 0.45).
narrative_ontology:measurement(stru_grid_18, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 40, 0.63).
narrative_ontology:measurement(stru_grid_19, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 0, 0.5).
narrative_ontology:measurement(stru_grid_20, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 40, 0.68).
narrative_ontology:measurement(stru_grid_21, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 0, 0.3).
narrative_ontology:measurement(stru_grid_22, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 40, 0.45).
narrative_ontology:measurement(stru_grid_23, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 0, 0.4).
narrative_ontology:measurement(stru_grid_24, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 40, 0.6).
narrative_ontology:measurement(stru_grid_25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 0, 0.55).
narrative_ontology:measurement(stru_grid_26, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 40, 0.72).
narrative_ontology:measurement(stru_grid_27, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(stru_grid_28, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 40, 0.6).
narrative_ontology:measurement(stru_grid_29, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 0, 0.4).
narrative_ontology:measurement(stru_grid_30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 40, 0.55).
narrative_ontology:measurement(stru_grid_31, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 0, 0.5).
narrative_ontology:measurement(stru_grid_32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the structural_adjustment_conditionalities kernel. creditor_coordination_reading authors a low-extraction, largely uniform-enforcement account (rope-leaning); debtor_extraction_reading authors a high-extraction, no-genuine-coordination account (snare-leaning); this hybrid_selectivity_reading occupies the tangled_rope middle, asserting both a real coordination function AND captured, alignment-driven selective extraction. All three share the same kernel (the conditionality apparatus itself) but are structurally distinct constraints with different ε, different victim/beneficiary sets, and different classifications — per the ε-invariance principle, they are authored as separate files linked via network edges, not as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
