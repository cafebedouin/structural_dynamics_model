% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Selectively Enforced IFI Loan Conditionalities
 *   domain: international political economy / development finance
 *
 * SUMMARY:
 *   This story instantiates the hybrid-selectivity reading of the structural
 *   adjustment conditionalities kernel: conditionalities are treated neither
 *   as pure coordination devices (the creditor_coordination_reading) nor as
 *   uniformly extractive neo-colonial instruments applied to all debtors
 *   alike (the debtor_extraction_reading), but as a mechanism whose actual
 *   severity is a function of the debtor's geopolitical position. The same
 *   technical apparatus — fiscal targets, structural benchmarks, program
 *   reviews — is enforced with markedly different rigor depending on whether
 *   the debtor state is strategically valuable to core creditor governments.
 *   This produces a genuine Tangled Rope structure: real coordination
 *   function coexists with asymmetric, selectively applied extraction running
 *   through the identical institutional machinery. Non-strategic debtor
 *   states and the populations within them bear a materially harsher version
 *   of the same instrument that hegemon-aligned states experience as
 *   comparatively soft external validation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.71).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Selectively Enforced IFI Loan Conditionalities").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international political economy / development finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'dc7629df-f30e-49af-b731-cf230216cf3d').
narrative_ontology:cs_kernel_codification('dc7629df-f30e-49af-b731-cf230216cf3d', formalized).
narrative_ontology:cs_authority_grounding('dc7629df-f30e-49af-b731-cf230216cf3d', extraction).
narrative_ontology:cs_interpretation_layer_present('dc7629df-f30e-49af-b731-cf230216cf3d').
narrative_ontology:cs_reading_relation('dc7629df-f30e-49af-b731-cf230216cf3d', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('dc7629df-f30e-49af-b731-cf230216cf3d', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('dc7629df-f30e-49af-b731-cf230216cf3d', foundational, enforcement_severity_tracks_geopolitical_alignment_not_fiscal_need).
narrative_ontology:cs_axiom_status(enforcement_severity_tracks_geopolitical_alignment_not_fiscal_need, holdable).
narrative_ontology:cs_axiom_grounding('dc7629df-f30e-49af-b731-cf230216cf3d', enforcement_severity_tracks_geopolitical_alignment_not_fiscal_need, empirically_contingent).
narrative_ontology:cs_axiom('dc7629df-f30e-49af-b731-cf230216cf3d', secondary, coordination_function_is_genuine_but_unevenly_realized).
narrative_ontology:cs_axiom_status(coordination_function_is_genuine_but_unevenly_realized, holdable).
narrative_ontology:cs_axiom_grounding('dc7629df-f30e-49af-b731-cf230216cf3d', coordination_function_is_genuine_but_unevenly_realized, empirically_contingent).
narrative_ontology:cs_reference_frame('dc7629df-f30e-49af-b731-cf230216cf3d', uniform_technocratic_conditionality_doctrine).
narrative_ontology:cs_drift_state('dc7629df-f30e-49af-b731-cf230216cf3d', post_cold_war_geopolitical_realignment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc7629df-f30e-49af-b731-cf230216cf3d', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_debtor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_governments).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, ifi_technical_staff).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, systemic_bondholders).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, public_sector_workers_in_program_countries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, subsistence_populations_facing_subsidy_removal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold controlling voting shares and informal veto power over IFI program design and waiver decisions. Direct conditionality severity through board influence, deciding which debtors face full program discipline and which receive quiet waivers tied to alliance value, basing access, or vote alignment in multilateral forums.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Design and administer the conditionality packages, producing technical justifications for fiscal targets and structural benchmarks. Their institutional legitimacy and career trajectories depend on the coordination narrative holding; they apply the same technical toolkit with visibly different rigor depending on which capital's political weight is behind the loan.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, ifi_technical_staff, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, ifi_technical_staff, beneficiary).

% Receive loans with softened, delayed, or unenforced conditionality despite comparable or worse fiscal indicators than non-strategic peers, because their geopolitical alignment, basing rights, or resource access matters to core creditor governments. Retain far more domestic policy discretion than the coordination story would predict.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_debtor_states, beneficiary,
    moderate, biographical, mobile, national).

% Face the full, rigidly enforced conditionality package — austerity targets, privatization mandates, subsidy removal timelines — with little room for renegotiation. Lack the strategic leverage that would earn a waiver; default or exit from the program means loss of market access and further isolation, so compliance is effectively compelled.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states, payer,
    powerless, biographical, trapped, national).

% Absorb wage freezes, layoffs, and pension restructuring mandated by conditionality benchmarks in non-strategic states. Have no seat in program negotiations and no meaningful exit from labor markets already narrowed by the same austerity.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, public_sector_workers_in_program_countries, payer,
    powerless, immediate, trapped, national).

% Bear the direct welfare cost when food and fuel subsidies are cut to meet fiscal targets in states without geopolitical leverage. Their consumption patterns and survival margins are shaped entirely by decisions made in creditor capitals and IFI boardrooms they have no access to.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, subsistence_populations_facing_subsidy_removal, payer,
    powerless, immediate, trapped, local).

% Hold sovereign debt whose value is protected by the conditionality regime's function of restoring debtor states to debt service capacity via austerity extraction. Diversified across many program countries, exposed to program failure only diffusely, and can exit any single credit exposure without bearing conditionality's costs themselves.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, systemic_bondholders, beneficiary,
    organized, biographical, arbitrage, global).

% Would object to both the substance of conditionality and its selective enforcement if given standing in program negotiations, but have no formal role in IFI board decisions or bilateral waiver discussions; their objections surface only as post-hoc unrest or academic critique.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, civil_society_and_labor_movements_in_program_states, excluded,
    moderate, biographical, trapped, national).

% Study program design and enforcement patterns across debtor states, documenting the correlation between geopolitical alignment and conditionality leniency independent of underlying fiscal indicators.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, comparative_political_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, diffuse).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Conditionality frameworks genuinely solve a real coordination problem: sovereign lenders and multilateral institutions need some mechanism to verify that borrowed funds will be used toward debt-service capacity rather than dissipated, and debtor governments benefit from an external commitment device that lets them impose otherwise politically costly reforms.
% TRANSFER_FUNCTION: Moves fiscal adjustment burden (wage suppression, subsidy removal, privatization proceeds, debt service capacity) from creditor balance sheets onto the domestic populations of debtor states — but the size of that transfer is not a function of fiscal need alone; it is scaled down or waived for states whose alignment matters to core creditor governments, and applied at full force to states without that leverage.
% ABSENT_VOICES: Civil society and labor movements inside program countries, and non-strategic debtor governments themselves in the moments waivers are negotiated for peer states, have no seat at the table where selectivity is actually decided; their exclusion means the selectivity itself is rarely part of the official program record.
% DISAPPEARANCE_RATIONALE: If conditionality enforcement disappeared overnight, non-strategic debtor states would retain far more domestic fiscal discretion immediately, austerity-driven subsidy and wage cuts in those states would likely reverse or stall, and the informal leverage core creditor governments currently exercise through selective waiver would lose its primary instrument — the world would rearrange substantially for the powerless payer seats, even though hegemon-aligned states would notice little change since they already receive de facto leniency.
% FOUNDING_PROBLEM: Sovereign debt crises in the late 20th century created a genuine problem: lenders needed assurance that emergency financing would restore debt-service capacity rather than be captured by patronage spending, and crisis-hit governments needed external cover to enact reforms they could not sell domestically on their own authority.
% FOUNDING_PROBLEM_CORROBORATION: IFI staff and core creditor governments attest the founding problem remains live — fiscal discipline verification is still needed wherever debt sustainability is genuinely at risk. Independent economists and UN human-rights rapporteurs studying program outcomes across decades attest that the enforcement pattern no longer tracks fiscal need alone but tracks geopolitical alignment, meaning the founding problem has been substantially superseded by a selective-discipline function for a subset of debtors — this latter attestation comes from academic panel studies and rapporteur reports outside the benefiting creditor and hegemon-aligned state seats.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) and suppression (0.71) reflect the experience of the modal program country without geopolitical leverage — the population this reading centers as its victim class. Theater ratio (0.58) is elevated because a substantial share of the technical justification apparatus (fiscal target derivations, benchmark rationales) functions to launder selective severity as neutral, uniform, technocratic discipline; the same staff producing rigorous debt-sustainability analysis for one country produce comparatively permissive analysis for another with similar fundamentals, and the technical language obscures rather than reveals this. Accessibility collapse (0.5) and resistance (0.62) sit at moderate-high levels because alternatives to program compliance exist in principle (default, alternative financing, regional lending facilities) but are foreclosed in practice for non-strategic states lacking market access alternatives, while resistance from affected populations and civil society is real but structurally unable to reach the boardroom where selectivity decisions are actually made.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of ifi_technical_staff and core creditor governments, the arrangement looks like consistent technical discipline uniformly applied — the selectivity is invisible from inside the institution because each waiver or leniency decision is justified on its own technical merits. From the seat of non_strategic_debtor_states and the populations bearing subsidy removal, the same apparatus is experienced as arbitrarily harsher than what comparable peers face, with no technical rationale sufficient to explain the gap. The engine computing divergent per-seat classifications from the same structural data is exactly what this reading is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Core creditor governments and ifi_technical_staff are agenda-setters who administer the selective machinery; hegemon-aligned debtor states are structural beneficiaries who receive the coordination function's benefits (external cover, financing access) with attenuated costs. Non-strategic debtor states, their public-sector workers, and subsistence populations facing subsidy removal are the targets — they bear the full transfer with no leverage to negotiate its terms and no meaningful exit given constrained market access. Systemic bondholders benefit diffusely and can exit any single exposure, distinguishing their directionality from the trapped populations who cannot exit the program's domestic consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (verifying that emergency lending restores debt-service capacity rather than being captured by patronage) remains genuinely live for some debtors, which is precisely why this reading resists collapsing into pure extraction: the coordination function is real and defensible in the abstract. What has drifted is enforcement discretion — the same instrument now also performs an unstated selective-discipline function correlated with alignment rather than fiscal need alone. Classifying this as tangled_rope rather than snare prevents mislabeling the entire conditionality apparatus as pure extraction (which would erase the genuine cases where the coordination function operates roughly as designed for strategically neutral debtors with real fiscal support needs) while still naming the asymmetric extraction that the coordination story cannot account for on its own.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_evidentiary_threshold,
    'How much of the observed variance in conditionality severity across debtor states is explained by geopolitical alignment versus by genuine differences in underlying fiscal fundamentals?',
    'Cross-national panel regression controlling for debt-to-GDP, reserve coverage, and macro indicators against program severity indices, with geopolitical alignment (UN voting coincidence, basing agreements, strategic resource access) as the key explanatory variable; a strong, robust alignment coefficient after controls corroborates the hybrid-selectivity reading over the pure-coordination reading.',
    'If alignment explains little variance after controlling for fundamentals, this reading collapses toward creditor_coordination_reading; if fundamentals explain little variance after controlling for alignment, this reading collapses toward debtor_extraction_reading''s uniformity claim being wrong in the other direction (extraction correlated with weakness, not selectively distributed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_evidentiary_threshold, empirical, 'Whether selectivity is genuinely geopolitical or a fundamentals proxy.').

omega_variable(
    which_reading_is_the_kernel_default,
    'Is the hybrid-selectivity account the correct decomposition of the conditionality kernel, or is selectivity itself better modeled as a second-order distortion of a genuinely uniform coordination mechanism (i.e., closer to creditor_coordination_reading with corruption) or as the true face of an extraction mechanism that merely varies in visibility (i.e., closer to debtor_extraction_reading with camouflage)?',
    'This is a conceptual framing question about which reading carries the burden of default — it cannot be settled by the panel regression alone because the same coefficient pattern is compatible with multiple framings; it would require normative and institutional-design argument about what counts as the ''baseline'' function of conditionality.',
    'Adopting a different default framing would relocate the constraint''s claimed_type without changing any single empirical fact — this is exactly the committer-structure disagreement the kernel decomposition is designed to hold outside any single constraint''s ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_kernel_default, conceptual, 'Which sibling reading should be treated as the interpretive default for the kernel.').

omega_variable(
    waiver_data_opacity,
    'Are informal waiver decisions (as opposed to formally documented program modifications) systematically undercounted because they occur through back-channel diplomatic pressure that leaves no institutional paper trail?',
    'Comparison of formally documented conditionality modifications against leaked diplomatic cables, investigative journalism, and former-staff testimony to estimate the gap between recorded and actual selective leniency.',
    'If informal waivers substantially outnumber documented ones, the true magnitude of selectivity — and thus the extraction borne by non-strategic states relative to a genuinely uniform baseline — is understated by any analysis relying only on official program records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waiver_data_opacity, empirical, 'Whether official records understate the true extent of selective enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 32, 0.55).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 24, 0.62).
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
narrative_ontology:measurement(stru_grid_01, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 0, 0.4).
narrative_ontology:measurement(stru_grid_02, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 40, 0.6).
narrative_ontology:measurement(stru_grid_03, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(stru_grid_04, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 40, 0.55).
narrative_ontology:measurement(stru_grid_05, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 0, 0.3).
narrative_ontology:measurement(stru_grid_06, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 40, 0.48).
narrative_ontology:measurement(stru_grid_07, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(stru_grid_08, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 40, 0.5).
narrative_ontology:measurement(stru_grid_09, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 0, 0.4).
narrative_ontology:measurement(stru_grid_10, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 40, 0.62).
narrative_ontology:measurement(stru_grid_11, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 0, 0.2).
narrative_ontology:measurement(stru_grid_12, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 40, 0.3).
narrative_ontology:measurement(stru_grid_13, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(stru_grid_14, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 40, 0.5).
narrative_ontology:measurement(stru_grid_15, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 0, 0.25).
narrative_ontology:measurement(stru_grid_16, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 40, 0.35).
narrative_ontology:measurement(stru_grid_17, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 0, 0.45).
narrative_ontology:measurement(stru_grid_18, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 40, 0.65).
narrative_ontology:measurement(stru_grid_19, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 0, 0.4).
narrative_ontology:measurement(stru_grid_20, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 40, 0.62).
narrative_ontology:measurement(stru_grid_21, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(stru_grid_22, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 40, 0.55).
narrative_ontology:measurement(stru_grid_23, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 0, 0.3).
narrative_ontology:measurement(stru_grid_24, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 40, 0.4).
narrative_ontology:measurement(stru_grid_25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 0, 0.55).
narrative_ontology:measurement(stru_grid_26, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 40, 0.72).
narrative_ontology:measurement(stru_grid_27, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 0, 0.5).
narrative_ontology:measurement(stru_grid_28, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 40, 0.68).
narrative_ontology:measurement(stru_grid_29, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 0, 0.45).
narrative_ontology:measurement(stru_grid_30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 40, 0.6).
narrative_ontology:measurement(stru_grid_31, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 0, 0.6).
narrative_ontology:measurement(stru_grid_32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the structural_adjustment_conditionalities kernel. creditor_coordination_reading authors low ε under the premise that conditionality is uniform, necessary fiscal discipline; debtor_extraction_reading authors high ε under the premise that conditionality is uniformly extractive across all debtor states; this hybrid_selectivity_reading authors moderate-to-high ε (0.68) under the premise that extraction is real but concentrated on non-strategic debtors while attenuated or waived for hegemon-aligned states. All three share the same underlying institutional kernel (IFI lending conditionality) but instantiate structurally distinct constraints with different victim sets, different beneficiary sets, and different ε values — per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
