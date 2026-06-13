% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Path Dependency Reading)
 *   domain: technology_history/institutional_analysis
 *
 * SUMMARY:
 *   QWERTY keyboard layout has persisted for 150+ years despite demonstrated
 *   superiority of alternative layouts (Dvorak, etc.). This story
 *   instantiates the PATH DEPENDENCY READING: QWERTY persists not because
 *   manufacturers strategically defend it or extract rents from it, but
 *   because it is a coordination equilibrium stable under network effects and
 *   training investment. The founding mechanical problem (typewriter
 *   escapement jams) is extinct; the constraint's persistence is entirely
 *   driven by the mathematical structure of the lock-in game, not by
 *   strategic action. No concentrated beneficiary extracts from
 *   QWERTY—manufacturers would switch if coordination were possible, users
 *   would retrain if the barrier were removed, and designers remain excluded
 *   only by the impossibility of moving the equilibrium unilaterally. This
 *   reading contests the STRATEGIC LOCK-IN READING, which claims
 *   manufacturers deliberately sustained QWERTY through training partnerships
 *   and cartel activity. This story generates one of two constraint stories
 *   for the same kernel; the sibling (strategic_lock_in_reading) will claim
 *   intentional beneficiaries and higher extractiveness.
 *
 * KEY AGENTS:
 *   - typewriter_users: trained population trapped by learning investment (not by coercion)
 *   - typewriter_manufacturers: responsive to demand, no extractive strategy possible
 *   - keyboard_layout_designers: excluded by coordination threshold, not by suppression
 *   - market_dynamics_analyst: sees the lock-in as mathematical equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Keyboard Layout Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'c0e6d304-b206-4927-8fff-7d98fe405af4').
narrative_ontology:cs_kernel_codification('c0e6d304-b206-4927-8fff-7d98fe405af4', distributed).
narrative_ontology:cs_authority_grounding('c0e6d304-b206-4927-8fff-7d98fe405af4', practice).
narrative_ontology:cs_reading_relation('c0e6d304-b206-4927-8fff-7d98fe405af4', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('c0e6d304-b206-4927-8fff-7d98fe405af4', foundational, coordination_equilibrium_primacy).
narrative_ontology:cs_axiom_status(coordination_equilibrium_primacy, holdable).
narrative_ontology:cs_axiom_grounding('c0e6d304-b206-4927-8fff-7d98fe405af4', coordination_equilibrium_primacy, empirically_contingent).
narrative_ontology:cs_axiom('c0e6d304-b206-4927-8fff-7d98fe405af4', foundational, accident_path_dependency).
narrative_ontology:cs_axiom_status(accident_path_dependency, holdable).
narrative_ontology:cs_axiom_grounding('c0e6d304-b206-4927-8fff-7d98fe405af4', accident_path_dependency, empirically_contingent).
narrative_ontology:cs_reference_frame('c0e6d304-b206-4927-8fff-7d98fe405af4', accident_driven_coordination).
narrative_ontology:cs_drift_state('c0e6d304-b206-4927-8fff-7d98fe405af4', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c0e6d304-b206-4927-8fff-7d98fe405af4', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because there is no concentrated beneficiary and manufacturers price competitively—the constraint is not a rent-collection mechanism. Suppression is minimal (0.12) because users are trapped by their own training investment and the coordination game structure, not by active enforcement. Theater is negligible (0.05) because there is no performative pretense—everyone openly acknowledges QWERTY is suboptimal, but the coordination problem is genuine and immovable by individual action. Accessibility collapse is extremely high (0.88) because once trained in QWERTY, users face near-infinite relearning costs for alternatives; this is not externally imposed but mathematically inherent to network goods. Resistance is minimal (0.08) because no agent has the power to move the equilibrium unilaterally—alternative-keyboard advocates have zero leverage because their designs cannot achieve critical mass. The measurement series shows slight early increase in extractiveness and theater as QWERTY became globally standardized (1873–1960), then plateauing as the constraint reached full saturation. The suppression_requirement rises early (mechanical enforcement via escapement design) then stabilizes (network-effects enforcement is automatic, requires no institutional machinery).
 *
 * PERSPECTIVAL GAP:
 *   This reading predicts that all seats (manufacturers, users, designers, analysts) should compute the constraint similarly—as a coordination equilibrium—because no seat holds extractive power over the others. The strategic_lock_in_reading predicts a perspectival gap: manufacturers benefit strategically from QWERTY training cartel activity, users and designers experience extraction. The engine's per-seat classification will show the gap if and only if the strategic reading is correct; if all seats compute as mountain (or all as low-extractiveness rope), the path-dependency reading is vindicated. The divergence between claimed type (mountain) and authored metrics (low but non-zero extractiveness) is intentional: the measured extractiveness reflects real but diffuse costs (training investment, suboptimal efficiency) that arise from the coordination equilibrium itself, not from anyone's strategy—a mountain with measurable friction.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares NO beneficiaries and NO victims because the constraint operates as a coordination equilibrium with distributed costs (training investment shared equally across users) and distributed benefits (everyone gains from standardization). There is no directional extraction from a target to a capturer. Manufacturers are in a symmetric position: they benefit from a standard (reduced design variation) and bear the cost of limited design freedom equally with everyone else. Users benefit from standardization (universal typing) and bear the cost of learning (which they would bear for any standard). Designers are excluded by the coordination threshold, not by anyone's deliberate choice. The directionality is near-symmetric across all seats because the constraint is a technical equilibrium, not an institutional extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jam prevention) is unambiguously dead in the electronic era. The constraint persists despite the problem's extinction, which might trigger a mandatrophy flag. However, the path-dependency reading resolves this: the constraint's mandate shifted from 'prevent mechanical jams' to 'maintain typing universality'—the original problem died, but the solution locked in a new problem (efficiency loss from suboptimal layout) that cannot be solved without coordination. Mandatrophy is NOT present because the mandate evolved, not because it is zombie. A mandatrophy verdict would follow only if the constraint persisted with no function at all—but coordination function is its current function, even if that function is not the original founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_intent_counterfactual,
    'Did typewriter and computer manufacturers deliberately perpetuate QWERTY through training partnerships, cartel coordination, or standards-setting bodies, or did the constraint persist organically via market response to installed base?',
    'Archival evidence from manufacturer board minutes, trade association records, and antitrust discovery documents (Bell Labs, IBM, AT&T). Historical interviews with engineers, standardization committee members, and business strategists. Comparative historical analysis of keyboard layout transitions in parallel industries (telephone switchboards, musical instruments) to isolate strategic vs. accidental drivers.',
    'If manufacturers strategically defended QWERTY (high confidence evidence of coordination, price-fixing, training cartels), the constraint reclassifies from path_dependency_reading (mountain) to strategic_lock_in_reading (snare) with beneficiary set {typewriter_manufacturers, computer_manufacturers} and victim set {keyboard_layout_designers, efficiency-seeking users}. If no evidence of strategic coordination exists, the path-dependency reading stands as the primary account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_intent_counterfactual, empirical, 'Whether QWERTY persistence is accident-driven path dependency or manufacturer-engineered strategic lock-in.').

omega_variable(
    mandate_shift_vs_mandatrophy,
    'Is QWERTY a constraint whose original mandate (prevent typewriter jams) has died but whose function has shifted to coordination (maintain typing universality), or is it a zombie constraint whose mandate is extinct and whose current persistence is pure inertia?',
    'Analyze the logical necessity of the current function (typing universality): (a) Does an optimal alternative layout exist that could be reached if coordination were solved? (b) If yes, is the inability to coordinate a temporary market failure (path-dependency trap) or a permanent feature of the coordination game? (c) If temporary, what would it take to move the equilibrium (regulatory mandate, subsidized retraining)? If permanent, the function is new and non-negotiable.',
    'If the current coordination function is genuinely non-negotiable and optimal given path-dependency constraints, mandatrophy is not present—the constraint serves a real purpose. If the constraint persists only due to inertia and switching would be efficient if coordination were solved, mandatrophy applies: the original mandate is dead and the new mandate (prevent switching) is derived from the solution, not from a real problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_shift_vs_mandatrophy, conceptual, 'Whether QWERTY has resolved mandatrophy via mandate shift or remains a zombie constraint.').

omega_variable(
    efficiency_loss_victim_identity,
    'Who bears the efficiency cost of QWERTY suboptimality? Is it a diffuse externality (everyone equally), a concentrated burden (power users, data-entry workers, accessibility-needs populations), or an uncompensated transfer to a specific beneficiary?',
    'Ergonomic and efficiency studies measuring typing speed, error rate, and repetitive-strain injury rates across populations segmented by usage intensity, occupational category, age, and physical ability. Economic quantification of cumulative efficiency loss attributable to QWERTY vs. optimal layouts. If efficiency loss concentrates on powerless or immobile populations, reclassify that segment as a victim group.',
    'If efficiency loss is diffuse (all users equally harmed), the path-dependency reading holds: no victim set, no beneficiary extraction. If loss concentrates on powerless populations (e.g., data-entry workers, disabled users for whom retraining is prohibitive), the constraint acquires a victim set and the reading shifts toward snare-like classification on the victimized seats. The overall constraint type may shift to tangled_rope if coordination benefits (for general users) coexist with concentrated extraction (for high-intensity users).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_loss_victim_identity, empirical, 'Whether QWERTY''s efficiency costs are diffuse or concentrated on identifiable victim groups.').

omega_variable(
    reading_boundary_precision,
    'Does the path-dependency reading''s core claim (QWERTY persists due to coordination equilibrium without strategic beneficiaries) remain logically distinct from the strategic-lock-in reading if the two readings both acknowledge that manufacturers RESPOND to QWERTY demand without needing to actively defend it?',
    'Clarify the boundary between (a) manufacturers responding to market demand for QWERTY (passive coordination response, no strategy required) and (b) manufacturers actively sustaining QWERTY through deliberate institutional action (training partnerships, cartel pricing, standards manipulation). The path-dependency reading permits (a) and denies (b); the strategic-lock-in reading requires (b). If manufacturers are found to respond passively to demand while also engaging in minor defensive coordination (e.g., standardization committee participation), does that falsify one reading or blend both?',
    'If manufacturers engage in passive-market response only, the path-dependency reading is vindicated and the constraint is mountain-like. If manufacturers engage in active institutional defense, the strategic-lock-in reading is vindicated and the constraint is snare-like. A mixed finding (passive response + minor defensive coordination) may require a third reading or a hybrid classification, or it may be interpreted as path-dependency reading with minor enforcement overhead (low-suppression mountain rather than snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_precision, conceptual, 'The precise boundary between passive market response (path-dependency) and active institutional defense (strategic lock-in).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1873, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1920, 0.03).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(qwer_tr_t2025, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1873, 0.05).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1920, 0.08).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(qwer_be_t2025, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1873, 0.02).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1920, 0.05).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(qwer_su_t2025, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2025, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__path_dependency_reading, 0.03).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence kernel family contains two structurally distinct constraint readings: (1) path_dependency_reading (this story): QWERTY persists as accident-driven coordination equilibrium, no strategic beneficiaries, mountain-type constraint. (2) strategic_lock_in_reading (sibling story): QWERTY persists as manufacturer-engineered lock-in via training partnerships and cartel standardization, snare-type constraint with beneficiary {manufacturers} and victim set {designers, efficiency-focused users}. The kernel itself (qwerty_persistence_inevitability) is the contested claim that QWERTY's 150-year persistence is explicable. The ε-invariance principle requires separate constraint stories because the causal mechanism differs structurally: if QWERTY persists due to path-dependency, ε is low and no beneficiary exists; if it persists due to strategic lock-in, ε is high and manufacturers are structural beneficiaries. These are different constraints with different structural properties. Both stories link to each other via network.affects_constraints; the committer-axis frame distinguishes them via cs_structure.reading_relations and cs_structure.axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
