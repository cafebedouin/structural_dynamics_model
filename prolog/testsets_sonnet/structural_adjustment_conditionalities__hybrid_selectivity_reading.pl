% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: IMF/Creditor Conditionality — Selective Enforcement by Geopolitical Strategic Value
 *   domain: International Political Economy / Development Finance / Institutional Economics
 *
 * SUMMARY:
 *   International lending conditionality — fiscal targets, structural
 *   benchmarks, and policy reforms attached to IMF/creditor financing — is
 *   formally uniform across borrowing states but empirically enforced with
 *   sharply different rigor depending on the debtor's strategic alignment
 *   with dominant creditor-state interests. Geopolitically strategic debtors
 *   (frontline states, key military basing partners, states whose default
 *   would trigger contagion the hegemon cannot tolerate) routinely receive
 *   waivers, extended timelines, and quiet non-enforcement of missed
 *   benchmarks. Non-strategic debtors face the full weight of the same formal
 *   instrument, with tranche suspension and program cancellation for
 *   identical shortfalls. The coordination function (solving the sovereign
 *   commitment problem) is genuine; the extraction is not visible in the text
 *   of any single program but in the pattern of who the text is actually
 *   enforced against.
 *
 * KEY AGENTS:
 *   - core_creditor_institutions: agenda_setter (institutional/arbitrage) — designs and administers conditionality, controls waiver discretion
 *   - hegemon_treasury_departments: agenda_setter/beneficiary (institutional/arbitrage) — exercises informal steering power that determines de facto enforcement
 *   - hegemon_aligned_debtor_states: beneficiary (powerful/mobile) — experience conditionality as negotiable
 *   - non_strategic_debtor_states: payer (moderate/trapped) — experience conditionality as binding
 *   - public_sector_workers_in_adjusting_states: payer (powerless/trapped) — absorb enforced fiscal contraction
 *   - subsistence_populations_in_adjusting_states: payer (powerless/trapped) — absorb subsidy removal with no exit
 *   - comparative_political_economy_scholars: observer (analytical) — see the cross-case selectivity pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.61).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "IMF/Creditor Conditionality — Selective Enforcement by Geopolitical Strategic Value").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "International Political Economy / Development Finance / Institutional Economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '85e2de8d-cffe-431b-a4cf-6d0b17a66397').
narrative_ontology:cs_kernel_codification('85e2de8d-cffe-431b-a4cf-6d0b17a66397', formalized).
narrative_ontology:cs_authority_grounding('85e2de8d-cffe-431b-a4cf-6d0b17a66397', extraction).
narrative_ontology:cs_interpretation_layer_present('85e2de8d-cffe-431b-a4cf-6d0b17a66397').
narrative_ontology:cs_reading_relation('85e2de8d-cffe-431b-a4cf-6d0b17a66397', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('85e2de8d-cffe-431b-a4cf-6d0b17a66397', structural_adjustment_conditionalities__debtor_extraction_reading, influences).
narrative_ontology:cs_axiom('85e2de8d-cffe-431b-a4cf-6d0b17a66397', foundational, enforcement_discretion_is_geopolitically_conditioned).
narrative_ontology:cs_axiom_status(enforcement_discretion_is_geopolitically_conditioned, holdable).
narrative_ontology:cs_axiom_grounding('85e2de8d-cffe-431b-a4cf-6d0b17a66397', enforcement_discretion_is_geopolitically_conditioned, empirically_contingent).
narrative_ontology:cs_axiom('85e2de8d-cffe-431b-a4cf-6d0b17a66397', foundational, formal_uniformity_does_not_imply_applied_uniformity).
narrative_ontology:cs_axiom_status(formal_uniformity_does_not_imply_applied_uniformity, holdable).
narrative_ontology:cs_axiom_grounding('85e2de8d-cffe-431b-a4cf-6d0b17a66397', formal_uniformity_does_not_imply_applied_uniformity, empirically_contingent).
narrative_ontology:cs_axiom('85e2de8d-cffe-431b-a4cf-6d0b17a66397', secondary, coordination_function_survives_alongside_selective_extraction).
narrative_ontology:cs_axiom_status(coordination_function_survives_alongside_selective_extraction, holdable).
narrative_ontology:cs_axiom_grounding('85e2de8d-cffe-431b-a4cf-6d0b17a66397', coordination_function_survives_alongside_selective_extraction, conventional).
narrative_ontology:cs_reference_frame('85e2de8d-cffe-431b-a4cf-6d0b17a66397', postwar_multilateral_crisis_lending_architecture).
narrative_ontology:cs_drift_state('85e2de8d-cffe-431b-a4cf-6d0b17a66397', post_cold_war_unipolar_and_multipolar_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('85e2de8d-cffe-431b-a4cf-6d0b17a66397', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_debtor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_treasury_departments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, public_sector_workers_in_adjusting_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, subsistence_populations_in_adjusting_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, approves, and enforces conditionality packages attached to lending programs. Sets the technical criteria (fiscal targets, privatization schedules, subsidy removal) that determine compliance, and controls the discretionary waiver process that determines who is actually held to those criteria. Retains institutional distance from the political calculus that governs waivers, allowing enforcement variance to appear technical rather than political.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Exercises effective veto and steering power over lending decisions through board representation and informal influence. Uses conditionality leniency as a foreign-policy instrument — rewarding geopolitically aligned or strategically important debtors with relaxed terms, waived benchmarks, or program restructuring, while allowing strict enforcement to fall on states without comparable leverage.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_treasury_departments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_treasury_departments, beneficiary).

% Receive the same nominal loan programs as other debtors but experience conditionality as negotiable: benchmarks are missed without consequence, timelines are extended, and politically painful reforms (subsidy cuts, currency floats) are quietly waived when domestic instability would threaten a strategically valuable government. Their exit option is real because non-compliance carries no enforcement cost.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_debtor_states, beneficiary,
    powerful, biographical, mobile, national).

% Face the identical formal conditionality language but full enforcement: missed benchmarks trigger tranche suspension, program cancellation, or credit-rating cascades. Lack the geopolitical leverage to negotiate waivers and cannot exit the lending relationship without triggering a sovereign default or losing access to capital markets entirely.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states, payer,
    moderate, biographical, trapped, national).

% Absorb wage freezes, layoffs, and pension restructuring mandated by conditionality packages actually enforced against their government. Have no seat in program negotiation and no exit short of emigration; the adjustment is designed to fall on this group because public payroll is a visible, immediately reducible fiscal line item.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, public_sector_workers_in_adjusting_states, payer,
    powerless, immediate, trapped, national).

% Bear the removal of fuel, food, and utility subsidies mandated under fully-enforced programs, with no compensating social protection floor comparable to what strategically favored debtors are permitted to preserve for their populations. Cannot relocate or substitute; local markets are the only markets they have access to.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, subsistence_populations_in_adjusting_states, payer,
    powerless, immediate, trapped, local).

% Publish evidence that identical conditionality benchmarks are enforced with markedly different rigor depending on debtor geopolitical alignment, but have no formal role in program design or waiver adjudication. Their findings circulate in academic and NGO channels without altering the closed-door discretion exercised by creditor institutions and hegemon treasuries.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_governmental_development_economists, excluded,
    moderate, generational, analytical, global).

% Study cross-national variation in program compliance enforcement and waiver frequency, correlating enforcement severity with UN voting alignment, basing agreements, and trade relationships with the hegemon. Positioned to see the selective-application pattern across the full case set that any single debtor or institution cannot see from inside a single program.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, comparative_political_economy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, diffuse).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Conditionality frameworks genuinely solve a real coordination problem: without some binding commitment mechanism, sovereign borrowers facing balance-of-payments crises would have no credible way to signal future fiscal discipline to creditors, and creditors would have no basis for extending further credit. The formal architecture of benchmarks, tranche releases, and program reviews is a legitimate response to sovereign lending's commitment problem.
% TRANSFER_FUNCTION: Moves fiscal adjustment burden asymmetrically: identical formal obligations translate into full-force austerity for non-strategic debtors (transferred onto public workers and subsistence populations) while translating into negotiable, frequently-waived obligations for geopolitically strategic debtors — effectively transferring the reputational and financial stability benefits of the lending architecture to creditor institutions and hegemon interests without proportionate adjustment cost.
% ABSENT_VOICES: Populations bearing the enforced adjustment in non-strategic states have no representation in program design; academic economists documenting the selectivity pattern are cited in postmortems but not consulted during active negotiations; the debtor governments themselves negotiate program terms but not the informal political calculus that determines whether the terms will actually be enforced.
% DISAPPEARANCE_RATIONALE: Creditor institutions and hegemon-aligned states would argue the world rearranges catastrophically — lending discipline collapses, moral hazard proliferates, capital markets seize for weak sovereigns. Non-strategic debtor populations and comparative economists would argue that removing the SELECTIVE application (while retaining uniform enforcement) would rearrange the world by removing the current asymmetric burden, while removing conditionality altogether would primarily affect who bears adjustment costs rather than whether adjustment happens.
% FOUNDING_PROBLEM: Recurrent sovereign balance-of-payments crises in the postwar and post-Bretton-Woods era created a need for a credible mechanism to extend emergency financing while protecting the resource pool of a shared lending facility from serial non-repayment or moral hazard.
% FOUNDING_PROBLEM_CORROBORATION: Creditor institutions attest the problem remains live and that conditionality is applied on technical, program-specific grounds. Independent comparative political economy research (e.g., studies correlating IMF program compliance leniency with UN General Assembly voting alignment and US Treasury influence) attests from outside the creditor institutions that enforcement variance tracks geopolitical alignment rather than program-specific fiscal risk, corroborating the selectivity reading from a source with no stake in either debtor or creditor outcomes.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) and theater_ratio (0.58) are both elevated and rising because the formal justification (fiscal sustainability, market confidence) increasingly diverges from the empirical enforcement pattern documented across program comparisons — the theater is the maintained fiction that identical benchmark language implies identical treatment. Suppression (0.61) is substantial but lower than extraction because non-strategic debtors are not physically coerced; their trap is structural (no alternative capital access, sovereign default cascades) rather than actively policed. accessibility_collapse (0.52) is moderate — alternative financing (regional facilities, bilateral lending, BRICS-adjacent institutions) has expanded, offering non-strategic debtors partial exit that did not exist decades ago, which is why this figure sits below what a pure snare would show. resistance (0.47) reflects genuine but limited debtor pushback — program renegotiation demands, IMF quota reform advocacy — that has not yet altered the selectivity pattern.
 *
 * PERSPECTIVAL GAP:
 *   From the core_creditor_institutions seat, the arrangement is uniform technical coordination applied consistently to program design; the institution's own compliance monitoring documents formal parity. From the non_strategic_debtor_states seat, the same instrument is experienced as arbitrarily severe relative to peer states with comparable fiscal indicators — the gap between these seats is not a difference of interpretation but a difference of which cases each seat is comparing itself against; only the comparative_political_economy_scholars seat has visibility across the full case set to see the pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   hegemon_treasury_departments and core_creditor_institutions sit at the low-d, beneficiary end: they set the rules and control discretionary application, extracting stability and geopolitical leverage from the arrangement without bearing its costs. hegemon_aligned_debtor_states also sit low-d despite formally being 'borrowers' because the constraint functionally subsidizes them via non-enforcement. non_strategic_debtor_states, public_sector_workers, and subsistence_populations sit at high-d: trapped exit, full enforcement, no negotiating leverage. This is the structural essence of the hybrid reading — directionality is not determined by loan-recipient status but by geopolitical position, which is exactly the variable the formal instrument does not name.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible commitment mechanism for crisis lending) remains partially live — sovereign debt crises continue to occur and require some resolution mechanism. This is what prevents a pure debtor_extraction reading: the coordination function has not gone extinct. But the founding problem's SELECTIVE non-enforcement for strategic debtors has no defensible genealogy in the original commitment-mechanism logic — it is a geopolitical overlay that the formal architecture was never designed to justify and does not acknowledge. The mandatrophy here is partial and located specifically in the enforcement-discretion layer, not in the conditionality architecture itself; classifying the whole instrument as either pure coordination or pure extraction would mislabel this asymmetric-application structure in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_measurement_confound,
    'Is documented enforcement variance driven by genuine differences in underlying fiscal/macroeconomic risk between strategic and non-strategic debtors, or by geopolitical alignment independent of risk fundamentals?',
    'Matched-pair comparative analysis controlling for fiscal indicators (debt-to-GDP, reserve coverage, inflation) across strategic and non-strategic debtors with similar risk profiles at time of program negotiation; residual enforcement-severity gap after controls isolates the geopolitical component.',
    'If risk fundamentals fully explain the variance, this reading collapses toward creditor_coordination_reading (technical, not selective). If a substantial residual persists after controls, it corroborates the hybrid reading''s core claim that geopolitical alignment is doing independent explanatory work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_measurement_confound, empirical, 'Whether enforcement variance is explained by fiscal risk or geopolitical alignment.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does this reading''s boundary with debtor_extraction_reading sit — is the coordination function real for ALL debtors, or only genuinely operative for strategic debtors (making non-strategic enforcement pure extraction dressed in coordination language)?',
    'Assess whether non-strategic debtors who fully comply with conditionality receive the promised coordination benefit (capital market access restoration, crisis resolution) at rates comparable to strategic debtors, or whether compliance itself fails to deliver the coordination benefit for non-strategic states.',
    'If compliant non-strategic debtors reliably receive the coordination benefit, the hybrid reading holds as a genuine middle position. If compliance does not reliably deliver the benefit even for non-strategic debtors, the hybrid reading is unstable and collapses toward debtor_extraction_reading for that subset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the hybrid reading is a stable middle position or an unstable compromise between the sibling readings.').

omega_variable(
    waiver_discretion_provenance,
    'Is the waiver-discretion mechanism itself a deliberate design feature of the lending architecture (built-in flexibility) or an emergent capture of a mechanism intended for genuine case-by-case technical judgment?',
    'Institutional history review of waiver-clause drafting intent versus documented pattern of waiver grants correlated with hegemon-state diplomatic priorities over multiple decades.',
    'If waiver discretion was designed with technical flexibility in mind and has been captured, this supports treating enforcement discretion as a corrigible design flaw. If the discretion was designed from inception to accommodate geopolitical override, the selective-application pattern is original to the constraint, not a later drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waiver_discretion_provenance, conceptual, 'Whether waiver discretion is captured design or original design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 24, 0.49).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 32, 0.54).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is the hybrid_selectivity_reading member of a three-story kernel family on structural_adjustment_conditionalities. creditor_coordination_reading models the same formal instrument assuming enforcement uniformity (ε low, Rope/Tangled Rope at the coordination pole). debtor_extraction_reading models it assuming enforcement is uniformly extractive across all debtors regardless of geopolitical position (ε high, Snare at the extraction pole). This story's distinguishing structural claim is that ε is NOT uniform across debtors — it varies systematically with geopolitical alignment, which is precisely the variable neither sibling reading's ε can represent as a single scalar. All three stories share the same underlying formal architecture (conditionality benchmarks, tranche release mechanics) but diverge on the empirical claim about how that architecture is actually applied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
