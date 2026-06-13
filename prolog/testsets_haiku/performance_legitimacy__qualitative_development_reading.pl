% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy via Qualitative Development (High-Tech Innovation and Structural Transformation)
 *   domain: political_economy/state_capitalism/development_planning
 *
 * SUMMARY:
 *   The state adopts a performance-legitimacy frame centered on innovation,
 *   technological leadership, and structural transformation toward
 *   'high-quality development'—efficiency, sustainability, and global
 *   competitiveness in advanced sectors—rather than maximizing raw GDP growth
 *   or delivering tangible improvements in everyday livelihoods. This reading
 *   justifies redirecting state capacity and capital toward high-tech sectors
 *   and away from traditional manufacturing, agriculture, and property
 *   development. The beneficiaries are the high-tech sectors, the
 *   state-backed innovation institutions that train workers and produce IP,
 *   and the multinational and venture capital firms integrated into this
 *   ecosystem. The victims are workers in traditional sectors who face
 *   unemployment and wage loss, local governments whose fiscal bases erode,
 *   and rural communities whose development prospects depend on sectors now
 *   marked as transitional and inefficient. The core extraction is
 *   justifiable as necessary investment in the state's technological future;
 *   the core suppression is narrative—the framing of resistance to sector
 *   decline as opposition to progress and modernization. This reading is one
 *   of four sibling readings of the performance_legitimacy kernel, each
 *   grounding legitimacy in a different foundational claim (quantitative
 *   growth, livelihood security, technological nationalism, or qualitative
 *   development).
 *
 * KEY AGENTS:
 *   - state_planning_apparatus: institutional agenda-setter directing capital and narrative; controls what counts as 'development'
 *   - high_tech_sectors: institutional beneficiary; growth decoupled from overall GDP constraints; receives sustained state funding and preferential policy
 *   - venture_capital_ecosystem: powerful beneficiary; profits from state-directed capital flows and M&A consolidation in high-tech space
 *   - traditional_manufacturing_workers: powerless victim; face sector decline, wage loss, constrained exit; suppressed by modernization narrative
 *   - property_dependent_local_governments: moderate-power victim; budgets erode as property and manufacturing bases shrink; caught between central directives and local populations
 *   - rural_agricultural_communities: powerless victim; identity-locked to land; development narrative devalues their livelihood as pre-modern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy via Qualitative Development (High-Tech Innovation and Structural Transformation)").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/state_capitalism/development_planning").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '392226d4-570a-4787-9a37-e46b9e023bd0').
narrative_ontology:cs_kernel_codification('392226d4-570a-4787-9a37-e46b9e023bd0', distributed).
narrative_ontology:cs_authority_grounding('392226d4-570a-4787-9a37-e46b9e023bd0', extraction).
narrative_ontology:cs_interpretation_layer_present('392226d4-570a-4787-9a37-e46b9e023bd0').
narrative_ontology:cs_reading_relation('392226d4-570a-4787-9a37-e46b9e023bd0', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('392226d4-570a-4787-9a37-e46b9e023bd0', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('392226d4-570a-4787-9a37-e46b9e023bd0', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('392226d4-570a-4787-9a37-e46b9e023bd0', foundational, innovation_structural_transformation_necessary).
narrative_ontology:cs_axiom_status(innovation_structural_transformation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('392226d4-570a-4787-9a37-e46b9e023bd0', innovation_structural_transformation_necessary, empirically_contingent).
narrative_ontology:cs_axiom('392226d4-570a-4787-9a37-e46b9e023bd0', foundational, traditional_sector_decline_acceptable_cost).
narrative_ontology:cs_axiom_status(traditional_sector_decline_acceptable_cost, holdable).
narrative_ontology:cs_axiom_grounding('392226d4-570a-4787-9a37-e46b9e023bd0', traditional_sector_decline_acceptable_cost, deontological).
narrative_ontology:cs_reference_frame('392226d4-570a-4787-9a37-e46b9e023bd0', pre_innovation_transition_state).
narrative_ontology:cs_drift_state('392226d4-570a-4787-9a37-e46b9e023bd0', contemporary_state_capacity_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('392226d4-570a-4787-9a37-e46b9e023bd0', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, venture_capital_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_institutions).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_workers).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, rural_agricultural_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.52 to 0.68 over the interval as high-tech sectors consolidate and traditional sectors decline, raising the cost (wage loss, unemployment, service degradation) borne by payers. Suppression holds steady at 0.62 because the core suppression mechanism—the narrative that sector decline is 'necessary modernization' that resistance appears backward—is stable; enforcement intensifies only at the margin (labor disputes, local government pushback) as displacement accelerates, but the narrative frame itself does not need strengthening. Theater (0.41, climbing from 0.28) reflects rising gap between the state's stated rationale (innovation infrastructure, efficiency gains, sustainability) and actual function (directing rents to high-tech beneficiaries). The measurement series track on a single shared time grid so every metric is authored at every point examined. The rising theater suggests the state is spending increasing effort on performance of innovation legitimacy to justify costs borne by payers—more press releases, patent statistics, startup events, research institute celebrations—while the underlying transformation function (shift capital from traditional to high-tech) proceeds regardless of rhetorical success. This is characteristic tangled_rope drift toward piton-grade theater as the original coordination problem (portfolio selection) becomes subordinate to the extraction function (rents to high-tech sectors).
 *
 * PERSPECTIVAL GAP:
 *   The state planning apparatus and high-tech sectors should compute as beneficiary seats (low directionality, low extracted cost, high coordination benefit). Traditional manufacturing workers and rural communities compute as target seats (high directionality, high extracted cost, suppressed exit, high forced participation). Property-dependent local governments sit near symmetric or slightly toward target (genuine coordination problem they benefit from—state directs investment somewhere—but they lose access to the sectors generating their fiscal base). The multinational firms that are simultaneously beneficiary and structurally constrained by technology transfer and local partnership requirements compute near symmetric in directionality (benefits + constraints offset). The engine computes per-seat types from power, exit, beneficiary/victim declarations, and directionality derivation; seats will diverge where power atoms and exit conditions differ sharply. The state apparatus and high-tech sectors see tangled_rope (real coordination problem + asymmetric distribution). Traditional workers and rural communities may compute as snare from their seats (high suppression, high extraction, no real coordination benefit, forced participation). The gap is the point—the constraint's structure is different depending which seat measures it.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning apparatus: institutional power, arbitrage exit (can shift policy direction), agenda-setter role = beneficiary directionality (d near 0.0, low chi). High-tech sectors and venture capital: powerful power atoms, mobile exit, beneficiary roles = low directionality (d ~0.1–0.2). State innovation institutions: institutional power, analytical exit, beneficiary role = low directionality. Traditional manufacturing workers: powerless power atom, constrained exit (sector decline removes options), payer role, high extraction burden = target directionality (d near 1.0, high chi). Property-dependent local governments: moderate power, constrained exit (fiscal dependency, cannot exit local jurisdiction), payer role but moderate burden relative to state actors = symmetric to slight-target directionality (d ~0.55–0.65). Rural communities: powerless power atom, identity-locked exit (land-bound, identity-constituted), payer role, high suppression = target directionality (d ~0.85–0.95). The directionality difference between institutional beneficiaries (d near 0.0) and powerless payers (d near 0.9) is extreme and drives the extraction asymmetry: the constraint extracts from those with no exit and suppresses their ability to organize resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimacy crisis when raw growth became untenable) is LIVE in the state's framing; the transformation narrative remains politically necessary. However, the mechanisms for solving it have shifted: early in the interval, the state genuinely attempted to manage transformation with retraining, reskilling, and gradual sector shift. By the interval's end, transformation becomes self-justifying—sector decline is celebrated as 'success' (fewer workers in 'inefficient' sectors) even though living standards for those workers have fallen. The theater measurement (rising from 0.28 to 0.41) suggests the founding problem's solution function is degrading: more energy goes into narrative maintenance (celebrating patents, startup events, innovation rankings) rather than managing the actual transition costs. This is classic mandatrophy: the constraint persists because it solved the state's legitimacy crisis, not because the original problem (managing structural transformation without destroying worker livelihoods) is being solved. The constraint could be reclassified to piton if theater continues to rise and becomes the primary function—at some point, innovation theater and transformation narrative sustain the constraint more than any real coordination or transformation outcome. The six_questions mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) flags this: the founding problem's solution is contested, yet the constraint's disappearance would rearrange the world, suggesting the constraint persists not because the founding problem is solved but because it benefits the state's legitimacy and high-tech sectors' resource access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_extraction_boundary,
    'Is the measured extraction from traditional sectors structurally necessary to fund innovation infrastructure, or does the state use innovation narratives to justify extraction that funds elite consumption and rent-seeking in high-tech sectors?',
    'Comparative analysis of state R&D budgets, patent productivity per dollar invested, and ratio of actual innovation output to value of extracted capital from traditional sectors. Counterfactual: what innovation could be funded at lower extraction rates?',
    'If extraction is proportional to genuine innovation costs, the constraint is tangled_rope (coordination + asymmetric transfer). If extraction far exceeds innovation costs and funds rent-seeking or elite consumption, reclassify to snare. The boundary is empirically locatable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_extraction_boundary, empirical, 'Whether innovation funding justifies the measured extraction from traditional sectors.').

omega_variable(
    displaced_worker_coalition_potential,
    'Can workers and communities in traditional sectors organize collective resistance that forces renegotiation of the transformation pace and beneficiary distribution?',
    'Historical observation: when displaced populations coordinate (union action, local government coalitions, electoral pressure), what concessions does the state make? Do they alter sector policy or merely increase retraining spending without changing direction?',
    'If collective action forces genuine policy shifts (slowing sector decline, protecting worker wages, restoring regional investment), the constraint''s suppression is lower than authored and its type shifts toward rope. If suppression holds despite organized resistance, the constraint is snare-grade or high-extraction tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_worker_coalition_potential, empirical, 'Whether suppression of worker resistance is structural or merely overwhelming current organization.').

omega_variable(
    reading_substitution_ambiguity,
    'Is this reading a genuine alternative performance-legitimacy frame, or is it the state''s adopted cover story for the same underlying extraction problem that livelihood_security_reading and quantitative_growth_reading also describe?',
    'Structural comparison across readings: do the four readings produce different beneficiary sets and extraction mechanisms, or do they all funnel gains to the same elites while offering different narrative justifications? If all readings show state institutions, high-tech firms, and multinational capital as net beneficiaries regardless of reading, the distinction is rhetorical, not structural.',
    'If readings are structurally distinct (different actual beneficiaries, different extraction targets), each is a true alternative. If all readings describe the same extraction with different narratives, the constraint family reduces from four to one, and the kernel contest is a legitimacy theater (piton-grade: performative choice among identical outcomes).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_substitution_ambiguity, conceptual, 'Whether this reading is structurally distinct from sibling readings or a rhetorical variant of a single underlying extraction.').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For rural agricultural communities marked as ''identity_locked,'' is the lock structural (geographic isolation, family land ties, lack of transferable skills) or internalized (cultural identity and self-concept constituted through agricultural practice)?',
    'Post-exit trajectory analysis: when rural residents do migrate to urban centers or high-tech regions, what share experience ongoing self-devaluation and identity disruption versus successful economic integration? If disruption persists after geographic move, lock is internalized; if integration proceeds, lock was primarily structural.',
    'If internalized, the constraint''s effective suppression on rural communities is higher than the structural measure (the community carries the suppression with them after exit). Policy remedies would need to address identity restoration, not just economic opportunity. This affects classification only if internalized lock raises total suppression above tangled_rope threshold into snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Whether identity-lock suppression on rural communities is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(perf_tr_t3, performance_legitimacy__qualitative_development_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(perf_tr_t6, performance_legitimacy__qualitative_development_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__qualitative_development_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(perf_tr_t18, performance_legitimacy__qualitative_development_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__qualitative_development_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(perf_be_t3, performance_legitimacy__qualitative_development_reading, base_extractiveness, 3, 0.56).
narrative_ontology:measurement(perf_be_t6, performance_legitimacy__qualitative_development_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__qualitative_development_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(perf_be_t18, performance_legitimacy__qualitative_development_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__qualitative_development_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(perf_su_t3, performance_legitimacy__qualitative_development_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(perf_su_t6, performance_legitimacy__qualitative_development_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__qualitative_development_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(perf_su_t18, performance_legitimacy__qualitative_development_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__qualitative_development_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel is instantiated by four distinct constraint stories, each grounding state legitimacy in a different foundational claim. This story (qualitative_development_reading) emphasizes innovation and structural transformation. It is linked to its siblings because the state's choice among these readings reshapes capital allocation, sector priorities, and victim/beneficiary distributions. The readings coexist as positions held by different state factions and international advisory constituencies; they influence each other through shifts in policy emphasis and institutional resource allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
