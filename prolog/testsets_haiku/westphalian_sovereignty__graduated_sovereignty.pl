% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty: External Discretionary Reclassification of State Capacity
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The graduated-sovereignty reading operationalizes the claim that
 *   sovereignty exists on a spectrum determined by state capacity and
 *   governance legitimacy, as measured by powerful-state institutions and
 *   international bodies. This reading authorizes external
 *   interveners—wealthy nations, the UN Security Council, the World Bank, the
 *   IMF, regional military alliances—to classify states as insufficiently
 *   capable or governed, and to condition aid, investment, and recognition on
 *   institutional reforms, transparency measures, and policy alignment with
 *   intervener preferences. Weak states that are reclassified lose de facto
 *   control over policy; populations within reclassified states experience
 *   military intervention, structural adjustment programs, and loss of
 *   economic sovereignty. The reading is presented as humanitarian
 *   capacity-building; the structural effect is extraction—transfer of policy
 *   autonomy and resource access from weak states to powerful interveners,
 *   justified by the language of measurement and improvement. This is one
 *   reading of the contested westphalian_sovereignty kernel; the competing
 *   absolute_sovereignty and conditional_sovereignty readings produce
 *   structurally different constraints with different victim sets and
 *   extraction patterns.
 *
 * KEY AGENTS:
 *   - Powerful external interveners (wealthy nations, permanent UN Security Council members, NATO) — set capacity standards, deploy forces, extract concessions, face no reciprocal reclassification
 *   - Weak state governments — reclassified and constrained, lose autonomy, bear conditionality costs
 *   - Populations in reclassified states — experience military/economic intervention, resource extraction, externally-imposed governance
 *   - International authority institutions (World Bank, IMF, UN, regional development banks) — operationalize reclassification, administer conditionality, extract resource access and policy leverage
 *   - State capacity measurement bodies (governance indices, rating agencies, think tanks) — author the measurement standards and conduct the evaluations that trigger reclassification
 *   - Resistance movements in weak states — resist external intervention and measurement-based reclassification; mobilize at high cost against vastly superior institutional resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.71).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty: External Discretionary Reclassification of State Capacity").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '94c14a2f-e5f6-4d30-a5b9-914fea2a2e63').
narrative_ontology:cs_kernel_codification('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', distributed).
narrative_ontology:cs_authority_grounding('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', extraction).
narrative_ontology:cs_interpretation_layer_present('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63').
narrative_ontology:cs_reading_relation('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', foundational, sovereignty_is_continuous_not_binary).
narrative_ontology:cs_axiom_status(sovereignty_is_continuous_not_binary, holdable).
narrative_ontology:cs_axiom_grounding('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', sovereignty_is_continuous_not_binary, empirically_contingent).
narrative_ontology:cs_axiom('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', foundational, capacity_measurement_determines_legitimacy).
narrative_ontology:cs_axiom_status(capacity_measurement_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', capacity_measurement_determines_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', measured_capacity_hierarchy).
narrative_ontology:cs_drift_state('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('94c14a2f-e5f6-4d30-a5b9-914fea2a2e63', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, powerful_external_interveners).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_authority_institutions).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_state_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, populations_in_reclassified_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.62 at interval end) because the constraint transfers policy autonomy, resource access, and territorial control from weak states to powerful interveners without commensurate benefit to the reclassified populations—the extraction is asymmetric and persistent. Suppression is higher (0.71) because the constraint's maintenance depends on actively excluding alternative frameworks (absolute_sovereignty, conditional_sovereignty readings) and suppressing resistance from reclassified states that dispute the legitimacy of external measurement and intervention authority. Suppression requirement has risen sharply from 0.48 in 1995 to 0.71 in 2025 because as reclassification has become routine and resistance has grown, the institutional machinery required to maintain the graduated-sovereignty frame has intensified (more intrusive measurement, more conditional aid, more military intervention, more data collection on 'governance'). Theater ratio is moderate-high (0.48) because a substantial share of the framing and activity is performance: the 'capacity-building' narrative obscures resource extraction; the 'good governance' standards encode powerful-state preferences; the 'humanitarian intervention' justification masks geopolitical ambition. The measurement series shows extractiveness rising steeply 1995-2008 (post-Cold War era, post-9/11 security anxieties) then plateauing 2008-2025 (retrenchment of intervention, greater state resistance, China/Russia institutional alternatives), while suppression requirement continues rising (maintaining the framework requires more institutional effort as legitimacy is contested). The temporal pattern suggests the constraint has matured into its extractive equilibrium: extractiveness has settled at the level where compliance costs roughly balance intervener benefits, while suppression has hardened because resistance is persistent and institutional alternatives are emerging.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (powerful interveners, international institutions) and the payer seats (weak states, reclassified populations) should compute radically different constraint types. From the agenda-setter seat, graduated sovereignty is rope or tangled_rope—a coordination mechanism solving genuine problems (weak states' governance failures, humanitarian crises, security threats) while bearing asymmetric costs. From the payer seat, it is a snare—the coordination story is cover; the actual function is extraction; the measurement standards are weaponized; exit is impossible and resistance is futile. The engine computes this divergence from the structural data: the payer seats have trapped exit, high d directionality toward the target end, and no beneficiary position; the agenda-setter seats have arbitrage exit, low d toward the beneficiary end, and control over the rules. This perspectival gap is the central feature of the reading—it explains why the constraint persists despite producing no improvement in outcomes for reclassified states.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful external interveners hold d near 0.0 (beneficiary end): they define the standards, collect the extraction (policy concessions, resource access, military bases, geopolitical alignment), and face no reciprocal reclassification. International institutions hold d near 0.1-0.2 (beneficiary with some cost): they administer the framework, benefit from expanded mandates and staff growth, but bear some reputational and operational costs when interventions fail. Weak state governments hold d near 0.8-0.9 (target end): they pay autonomy, bear conditionality costs, and cannot exit. Populations in reclassified states hold d near 0.85-0.95 (full target): they bear military intervention, economic extraction, and loss of sovereignty without any seat in the process. Resistance movements hold d near 0.90 (full target): they mobilize against structural odds and are systematically suppressed. Measurement bodies hold d near 0.1 (beneficiary end): they author the standards and conduct the evaluations; they face no accountability for bias or error. This directionality distribution is what makes the constraint a snare: it extracts from the powerless and trapped, concentrating extraction on those with no exit and no voice in the measurement process.
 *
 * MANDATROPHY ANALYSIS:
 *   The graduated-sovereignty reading does not resolve mandatrophy; it exemplifies it. The founding problem (weak states create humanitarian crises and security threats) is contested as to whether it is live or dead. Powerful interveners claim it is live and cite ongoing state fragility as justification for expanded intervention. Reclassified states and resistance scholars claim the founding problem is dead—that state fragility is often a product of past intervention, resource extraction, and imposed structural adjustment. The evidence is mixed: some reclassified states (Rwanda post-1994, Liberia) show some governance improvement; many others (Afghanistan, Iraq, Libya, Mali) show systematic degradation of state capacity and increased violence after intervention. The constraint persists despite mixed or negative outcomes because the extraction mechanism is not dependent on achieving the founding goal—the mechanism persists because powerful interveners benefit from the discretion to intervene and the international institutions benefit from expanded mandates. This is the characteristic signature of mandatrophy: the constraint's stated function (capacity-building, humanitarian relief) has outlived its credibility, but the constraint persists because the institutional infrastructure that administers it collects rents from its operation, and because exit is impossible for the reclassified populations. The graduated-sovereignty reading is a perfect case study in how a legitimate coordinating constraint (if it worked) can degrade into a pure extraction mechanism once the founding problem is solved or revealed as unsolvable by the stated mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_framework_bias,
    'Who authors the capacity and governance standards by which states are reclassified, and do those standards encode the preferences and institutional interests of the powerful states that define them?',
    'Comparative analysis of capacity metrics authored by powerful-state institutions versus those authored by weak-state analysts; measurement bias audits; longitudinal tracking of reclassification consistency across states with similar objective characteristics but different geopolitical alignments.',
    'If measurement standards are biased in favor of powerful-state preferences (e.g., favoring market liberalization, Western-style governance, NATO alignment), the graduated-sovereignty reading becomes a cover story for neo-colonial extraction. If standards are objective and consistently applied across state types regardless of geopolitical alignment, the reading retains legitimacy as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_framework_bias, empirical, 'Whether capacity measurement standards are objective or biased toward powerful states'' interests.').

omega_variable(
    intervention_outcome_paradox,
    'Do external interventions under graduated-sovereignty framework actually improve state capacity and governance outcomes in reclassified states, or do they systematically leave states more dependent and less capable than before?',
    'Longitudinal data on governance indicators, economic indicators, and state institutional capacity before and after intervention; comparison with counterfactual case studies of similar states that were not intervened upon; tracking of conditionality compliance and autonomy loss.',
    'If interventions improve outcomes, the constraint operates as coordinate improvement with asymmetric cost-bearing—a tangled_rope with real coordination value. If interventions systematically degrade outcomes while extracting resources and autonomy, the constraint is a snare: the stated function (capacity-building) is theater; the actual function is extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_outcome_paradox, empirical, 'Whether intervention outcomes vindicate the capacity-building narrative or refute it.').

omega_variable(
    reciprocal_reclassification_absence,
    'Why are powerful states never reclassified as having insufficient capacity or governance legitimacy, even when they engage in systematic human rights abuses, institutional capture, or state failure in specific domains?',
    'Analysis of reclassification criteria and application across state power levels; documentation of powerful states'' immunity from the same standards applied to weak states; investigation of how criteria would be applied if reciprocal measurement were authorized.',
    'Absence of reciprocal reclassification suggests the framework is structurally designed to extract from weak states only—it is a pure snare, not a coordination mechanism with asymmetric cost. Presence of reciprocal reclassification would support a tangled-rope reading (coordination with extraction costs borne unequally but by all).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocal_reclassification_absence, conceptual, 'Whether graduated-sovereignty standards are universally applied or weaponized against weak states only.').

omega_variable(
    kernel_reading_contest,
    'Is sovereignty a spectrum determined by capacity (graduated_sovereignty reading) or an unconditional right of statehood (absolute_sovereignty reading) or a conditional responsibility framework (conditional_sovereignty reading)?',
    'Comparison of extractive outcomes and legitimacy claims across the three readings; analysis of which reading produces the most extractive pattern and which produces coordinated improvement. The kernel contest cannot be resolved by measuring the world — it is resolved by choosing which reading the international system will operationalize.',
    'This omega routes the committer frame: if absolute_sovereignty is operationalized, external intervention loses normative legitimacy and extraction must be naked. If conditional_sovereignty is operationalized, intervention requires demonstrable human rights violations, not capacity measurement. If graduated_sovereignty persists, the measurement discretion remains with powerful states, enabling continued extraction. The three readings produce different constraint types (absolute → piton/scaffold of absolute-sovereignty doctrine; conditional → tangled_rope with real coordination; graduated → snare enabling neo-colonial extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the westphalian_sovereignty kernel will the international system operationalize, and what extraction pattern follows from each?').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural—imposed externally through military, economic, and institutional force—or internalized—does the reclassified state''s own leadership internalize the capacity-deficit narrative and self-impose the reforms external interveners demand?',
    'Post-exit trajectory analysis: when external pressure is removed or negotiations shift power balance, do reclassified states maintain the adopted reforms (internalized suppression) or revert to prior institutional forms (structural suppression)? Analysis of leadership''s private vs. public statements about the legitimacy of reclassification. Comparison of reform adoption rates when conditionality is hard-enforced versus when it is voluntary.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than the measured 0.71—the target carries the suppression with them even if external pressure decreases. If suppression is structural, it is reversible if the external machinery is dismantled. Internalization suggests the constraint operates at the identity and cognition level; structural suppression operates at the institutional level only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of state autonomy is structurally imposed or internalized by reclassified states'' own leadership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1995, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(west_tr_t2001, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(west_tr_t2008, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2008, 0.41).
narrative_ontology:measurement(west_tr_t2015, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2015, 0.46).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(west_be_t1995, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(west_be_t2001, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(west_be_t2008, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement(west_be_t2015, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1995, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(west_su_t2001, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(west_su_t2008, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2008, 0.64).
narrative_ontology:measurement(west_su_t2015, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, global_infrastructure).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__graduated_sovereignty, 0.25).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, structural_adjustment_programs).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, conditional_development_lending).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the westphalian_sovereignty kernel. The absolute_sovereignty and conditional_sovereignty readings are sibling constraints in the same kernel family. Each reading operationalizes a different interpretation of what the westphalian commitment to sovereign statehood permits. Graduated-sovereignty differs structurally by transferring legitimacy from formal statehood (absolute) or documented violations (conditional) to capacity measurement—which enables discretionary external intervention justified by measurement standards authored by the interveners themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
