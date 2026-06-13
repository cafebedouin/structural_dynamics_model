% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Typology (Hybrid Pragmatic Reading)
 *   domain: epistemology/institutional_design
 *
 * SUMMARY:
 *   The constraint typology of Deferential Realism (mountains, ropes,
 *   tangled_ropes, snares, scaffolds, pitons) is itself a constraint on how
 *   constraint classification discourse proceeds. This story analyzes ONE
 *   reading of that constraint: the hybrid pragmatic reading, which asserts
 *   that the typology has a fixed observational core (mountains and ropes,
 *   grounded in physical and coordination facts) but an irreducibly normative
 *   periphery (tangled_rope and snare classification depends on normative
 *   judgments about legitimate beneficiaries and institutional design). This
 *   reading competes with two sibling readings—immutabilist (all
 *   classification is observational) and rhetorical (all classification is
 *   normative)—and its persistence depends on managing the suppression of
 *   explicit contradiction between them. The extraction lies not in
 *   measurement or authority over conclusions, but in institutional
 *   gatekeeping: the pragmatist frame licenses both empirical investigation
 *   AND normative advocacy without requiring resolution of their tensions,
 *   and this license is withheld from traditions that reject the boundary.
 *
 * KEY AGENTS:
 *   - pragmatist_interpretive_community: Scholars and designers who benefit from the hybrid frame's flexibility—it permits both objectivity claims (core) and policy argument (periphery)
 *   - immutabilist_epistemic_tradition: Researchers committed to observational/diagnostic classification across all categories; constrained exit because pragmatism dominates institutional discourse
 *   - rhetorical_normative_tradition: Critical theorists and policy advocates who read the framework as insufficiently committal to normativity; constrained exit mirrors immutabilism
 *   - boundary_constrained_researchers: Career-dependent (identity-locked) investigators of cases where pragmatist core/periphery boundary is itself contested; their coherence depends on the reading's maintenance
 *   - observational_science_community: Logically excluded from legitimacy discussions by the pragmatist definition that normativity corrupts classification
 *   - institutional_constraint_designers: Policy makers who benefit from the dual move—some constraints transcend politics, some require collective choice
 *   - philosophy_of_science_community: Analytical observers examining structural coherence of all three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.62).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Typology (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, 'a2879b55-45e2-441f-8a51-7b02f89c40ff').
narrative_ontology:cs_kernel_codification('a2879b55-45e2-441f-8a51-7b02f89c40ff', distributed).
narrative_ontology:cs_authority_grounding('a2879b55-45e2-441f-8a51-7b02f89c40ff', distributed).
narrative_ontology:cs_reading_relation('a2879b55-45e2-441f-8a51-7b02f89c40ff', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2879b55-45e2-441f-8a51-7b02f89c40ff', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('a2879b55-45e2-441f-8a51-7b02f89c40ff', foundational, core_observational_periphery_normative).
narrative_ontology:cs_axiom_status(core_observational_periphery_normative, holdable).
narrative_ontology:cs_axiom_grounding('a2879b55-45e2-441f-8a51-7b02f89c40ff', core_observational_periphery_normative, conventional).
narrative_ontology:cs_axiom('a2879b55-45e2-441f-8a51-7b02f89c40ff', foundational, boundary_institutional_maintenance_legitimate).
narrative_ontology:cs_axiom_status(boundary_institutional_maintenance_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('a2879b55-45e2-441f-8a51-7b02f89c40ff', boundary_institutional_maintenance_legitimate, instrumental).
narrative_ontology:cs_reference_frame('a2879b55-45e2-441f-8a51-7b02f89c40ff', constraint_typology_core_periphery_split).
narrative_ontology:cs_drift_state('a2879b55-45e2-441f-8a51-7b02f89c40ff', contemporary_institutional_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2879b55-45e2-441f-8a51-7b02f89c40ff', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, pragmatist_interpretive_community).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, non_pragmatist_epistemic_traditions).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, boundary_constrained_researchers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks at 0.58 (moderate-high) and stabilizes by midpoint because the constraint's primary extraction is institutional authority over discourse legitimacy, not rent collection. The initial rise (0.38→0.58 over 30 units) reflects pragmatism's increasing dominance in academic and policy institutions; the plateau (0.58 stable from t=30 onward) indicates the reading has institutionalized sufficiently that further extraction does not require intensification. Theater ratio climbs to 0.48 and plateaus, indicating rising performative maintenance: pragmatism's institutional success requires increasingly elaborate defenses of the core/periphery distinction against critique from both sides (immutabilists claiming it is incoherent, rhetoricians claiming it is insufficiently normative). Suppression requirement reaches 0.62 and holds because maintaining the reading requires actively excluding or marginalizing observational science communities (by definition) and periodically reasserting the distinction against internal pressure. The measurement series samples the trajectory of institutional adoption: early uncertainty (higher theater, lower suppression) gives way to consolidated gatekeeping (stable theater and suppression). Resistance is consistently high (0.62→0.71) because both sibling traditions actively contest the pragmatist frame.
 *
 * PERSPECTIVAL GAP:
 *   From the pragmatist seat, the constraint is a genuine coordination solution—it resolves a real institutional problem (the binary pressure between immutabilism and rhetorical normativity) by creating space for both empirical and normative work. From immutabilist and rhetorical seats, the same structure is experienced as suppressive gatekeeping: pragmatism extracts from them by insisting their tradition's core claim is wrong or incomplete. The pragmatist frame does not resolve this gap; it institutionalizes it by making pragmatism the default and marginalizing the alternatives. The engine computes these divergent classifications from the structural data—immutabilists and rhetoricians experience effective extraction (high d toward target) while pragmatists experience beneficial coordination (low d toward beneficiary). The measurement trajectory shows this divergence intensifying as pragmatism consolidates institutional position: early-interval suppression is lower (both sides still contest at equal strength); late-interval suppression is higher (pragmatism has gatekeeping power).
 *
 * DIRECTIONALITY LOGIC:
 *   The pragmatist community benefits from authority over classification legitimacy and institutional resources for boundary maintenance—d near 0.1 (beneficiary, powerful institutional actor, mobile exit via arbitrage of rhetorical positioning). Immutabilist and rhetorical traditions are targets: they bear the cost of marginalization and operate with constrained exit (they cannot step outside constraint-theoretic frames institutionally without career risk). Boundary-constrained researchers are identity-locked targets (their work only makes sense within the pragmatist frame; if it disappeared, their research program collapses). Institutional designers are secondary beneficiaries (mobile, powerful, benefit from the dual move without maintaining it). The measurement series reveals institutional-level consolidation—suppression rises because pragmatism gains gatekeeping power; resistance rises because both alternatives mount critique. Accessibility collapse is high (0.64) because once the pragmatist frame dominates, alternatives are difficult to articulate from within institutional spaces (academia, policy, foundations) that have adopted the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—binary pressure between immutabilism and rhetorical normativity in constraint classification—has NOT been resolved by the pragmatist reading; it has been deferred through institutional gatekeeping. The reading asserts a fixed core/contested periphery distinction, but this distinction is itself normative (the boundary is drawn by choice, not discovered). This is the core mandatrophy: the pragmatist frame solves the founding problem only if one accepts the normative choice that core/periphery is a defensible split, which is exactly the choice the rhetorical tradition rejects and the immutabilist tradition views as confused. The constraint's persistence depends on institutional actors (pragmatist scholars, designers) not fully acknowledging this circularity. Mandatrophy is resolved (not eliminated) by making the distinction performatively mandatory through gatekeeping: pragmatism extracts from all sides by insisting it is the only coherent position, while its coherence depends on suppression of the fact that it is itself a normative stance. The measurement data show mandatrophy's institutional work: theater_ratio climbs because the core/periphery distinction requires increasing elaborate defense against internal pressure; suppression climbs because maintaining the reading requires excluding competing definitions of what 'coherence' means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_periphery_boundary_normativity,
    'Is the pragmatist core/periphery distinction (observational core, normative periphery) itself objectively grounded, or is it a normative choice?',
    'Foundational analysis of whether the boundary between mountains/ropes and tangled_rope/snare can be drawn without normative premises. If the distinction requires choosing what counts as ''legitimate beneficiary'' even to identify the boundary, the distinction is itself normative and pragmatism collapses into the rhetorical reading.',
    'If the boundary is normative, pragmatism is incoherent—it claims to fix the core observationally while that fixation depends on normative choice. This would support the immutabilist claim (all classification is observational, pragmatism is confusion) or the rhetorical claim (all classification is normative, pragmatism is half-measure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(core_periphery_boundary_normativity, conceptual, 'Whether pragmatism''s core/periphery distinction is self-defeating on its own epistemic premises.').

omega_variable(
    institutional_capture_vs_coordination,
    'Does pragmatism benefit constraint-theoretic inquiry by providing institutional space for empirical and normative work, or does pragmatism extract by monopolizing legitimacy over constraint classification discourse?',
    'Longitudinal study of research productivity and theoretical innovation within each reading tradition. If pragmatism''s dominance correlates with suppression of immutabilist and rhetorical research, the reading is primarily extractive; if all traditions advance work under pragmatism''s institutional frame, it is genuinely coordinating.',
    'If extractive, pragmatism should be reclassified from tangled_rope (hybrid coordination/extraction requiring enforcement) to snare (pure extraction with coordination as cover story). If coordinating, the reading''s tangled_rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_coordination, empirical, 'Whether pragmatism''s institutional dominance reflects coordination success or institutional capture.').

omega_variable(
    sibling_foreclosure_possibility,
    'Do the pragmatist and rhetorical readings actually COEXIST, or does pragmatism''s insistence on a fixed observational core logically foreclose rhetorical normativity?',
    'Formal analysis of whether accepting pragmatism''s core/periphery framework commits one to rejecting the rhetorical claim that all classification is policy-dependent. If pragmatism''s fixed core is incompatible with rhetorical thoroughgoing normativity, the relation is foreclosure, not coexistence.',
    'If pragmatism forecloses rhetorical normativity, the reading_relations should be updated from coexists_with to forecloses. This would elevate pragmatism''s claim from hybrid gatekeeping to logical dominance—a different structural position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_foreclosure_possibility, conceptual, 'Whether pragmatism''s logical structure permits genuine coexistence with the rhetorical reading.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.62) structural (institutional barriers to rhetorical and immutabilist voices) or internalized (researchers self-censor because they accept pragmatism as the only coherent position)?',
    'Post-suppression trajectory analysis: if immutabilist and rhetorical researchers active in non-pragmatist institutional spaces (alternative academies, policy-adjacent think tanks) show reduced self-censorship, suppression is structural; if they remain self-censoring even outside pragmatist-dominant institutions, it is internalized.',
    'If structural, pragmatism''s suppression depends on continued institutional gatekeeping; if internalized, pragmatism has captured the epistemic identity of alternative traditions themselves (deeper extraction). Both are consistent with tangled_rope, but internalized suppression would indicate stronger institutional hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of alternative readings is external institutional barrier or internalized epistemic stance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 5, 0.36).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(defe_tr_t35, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 35, 0.48).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(defe_be_t35, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(defe_su_t35, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__hybrid_pragmatic_reading, 0.18).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_classification_boundary_detection).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, normative_judgment_in_empirical_analysis).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_gatekeeping_dynamics).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel has three constraint stories: immutable_diagnostic_reading (all classification observational, extraction is measurement error); hybrid_pragmatic_reading (fixed core observational, contested periphery normative, this story); rhetorical_scaffold_reading (all classification normative, framework is policy vocabulary). Each story is ε-invariant: immutabilism has low epsilon (contradiction is correction target); pragmatism has medium epsilon (contradiction is gatekeeping object); rhetorical has high epsilon (contradiction is policy space). The three readings form a constraint family linked by network.affects_constraints. Pragmatism INFLUENCES both siblings: it extracts from immutabilism by claiming the core/periphery distinction renders pure observationalism incomplete; it influences rhetorical normativity by insisting core observationality limits the scope of policy discretion. Neither reading logically forecloses pragmatism within its own framework (coexistence), but pragmatism's institutional dominance creates structural pressure on both alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
