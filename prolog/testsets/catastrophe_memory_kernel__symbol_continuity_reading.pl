% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual Symbolic Continuity and Collective Identity Preservation
 *   domain: religious/cultural/anthropological
 *
 * SUMMARY:
 *   Among surviving groups that have endured catastrophe (genocide,
 *   persecution, exile), ritual crystallizes as a mechanism for preserving
 *   collective identity and symbolic continuity across generations. This
 *   reading frames ritual as a SYMBOLIC TRANSMISSION CONSTRAINT, not
 *   primarily a survival-encoding device or boundary-enforcement mechanism
 *   (those are sibling readings). The constraint coordinates on maintaining a
 *   fixed ritual form whose symbolic meaning — 'we are still this people,
 *   descended from the catastrophe-survivors' — is the point. Extractiveness
 *   is LOW because the constraint's primary function is meaning-preservation,
 *   not resource transfer; but a real cost accrues to those whose lived
 *   reality diverges from the ritual's embedded premises and who cannot
 *   modify it without identity expulsion.
 *
 * KEY AGENTS:
 *   - Tradition-continuity keepers: ritual specialists and authorities who maintain exact ritual forms, identity-locked to the practice
 *   - Collective identity keepers: community members for whom the ritual anchors group identity, also identity-locked
 *   - Adaptive modification agents: those who see ritual as functionally outdated but constrained from modifying it
 *   - Pragmatic rationalists: those who recognize the ritual's original survival function but argue its contemporary symbolic rigidity costs adaptive capacity
 *   - Younger generation: experiencing the ritual as both identity-anchor and lived-reality mismatch
 *   - External observers: anthropologists and scholars measuring symbolic function without participating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual Symbolic Continuity and Collective Identity Preservation").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious/cultural/anthropological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '69e8df5d-27b1-4987-89b7-36c2e3f3a06b').
narrative_ontology:cs_kernel_codification('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', fixed_text).
narrative_ontology:cs_authority_grounding('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', lineage).
narrative_ontology:cs_interpretation_layer_present('69e8df5d-27b1-4987-89b7-36c2e3f3a06b').
narrative_ontology:cs_reading_relation('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', foundational, symbolic_continuity_is_identity).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', symbolic_continuity_is_identity, deontological).
narrative_ontology:cs_axiom('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', secondary, ritual_form_stability_as_sacred_duty).
narrative_ontology:cs_axiom_status(ritual_form_stability_as_sacred_duty, holdable).
narrative_ontology:cs_axiom_grounding('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', ritual_form_stability_as_sacred_duty, deontological).
narrative_ontology:cs_reference_frame('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', post_catastrophe_identity_crystallization).
narrative_ontology:cs_drift_state('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', contemporary_post_collective_trauma, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69e8df5d-27b1-4987-89b7-36c2e3f3a06b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_keepers).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_agents).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, pragmatic_rationalists).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.28 endpoint) because the constraint's primary function is symbolic coordination, not wealth transfer or operational extraction. The low suppression (0.15) reflects that identity-fusion is the binding mechanism, not overt coercion — members are willing participants in most cases, even when experiencing cognitive dissonance. Theater ratio is high and rising (0.48→0.62 over the interval) because as the founding problem (post-catastrophe identity-dissolution) becomes historically distant, the ritual's function shifts from adaptive necessity to performance of continuity. The measurement series shows a classic pattern of a constraint whose original problem has substantially solved: extractiveness plateaus after generation-30 (roughly one lifetime), theater rises as the symbolic meaning increasingly becomes performative rather than operationally necessary, and suppression requirement gradually increases as younger members experience more mismatch between the ritual form and their lived reality. The rise and partial plateau of theater around time-40 reflects the point at which the constraint's original functional crisis is fully historical, and the ritual becomes primarily about meaning-transmission rather than group-survival. The slight decline in theater after time-50 reflects some community adaptation and ritualized revision (maintaining the form while reinterpreting embedded meanings), showing that the constraint is neither perfectly rigid nor completely dissolved.
 *
 * PERSPECTIVAL GAP:
 *   The tradition-continuity keepers experience this as ROPE — genuine coordination solving a real collective-action problem (meaning preservation requires shared form). The adaptive-modification agents experience it as TANGLED ROPE or low-grade SNARE — they recognize the coordination function but perceive the cost of rigidity as exceeding the benefit of continuity, and they lack exit options or modification authority. The younger generation sits between: they experience the symbolic benefit (identity anchor) and the cost (lived-reality mismatch) simultaneously, and the constraint's structure prevents them from resolving the tension through adaptation. The engine computes per-seat classification from the structural data: powerful organized tradition-keepers with arbitrage-grade exit (can move to stricter communities if reform threatens) compute as beneficiary-directed; moderate adaptive-modification agents with identity-locked exit compute as target-directed. The divergence is the core analytical finding — the same constraint reads entirely differently from different seated positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition-continuity keepers: power=organized, exit=identity_locked but with arbitrage-grade escape (join stricter communities, become conservative leaders). They are agenda-setters and beneficiaries. Directionality should derive near beneficiary-end (0.1-0.3 range) — they control the constraint, they benefit from its symbolic function, and they have exit if it becomes too heterodox. Adaptive-modification agents: power=moderate, exit=identity_locked (no arbitrage — leaving the community means identity expulsion). They are payers. Directionality should derive near target-end (0.6-0.8 range) — they bear the cost of cognitive dissonance, they lack modification authority, and their only exit is abandonment. Collective identity keepers: power=organized but diffuse (many, not concentrated), exit=identity_locked (genuine, not arbitrage). They experience genuine benefit from the symbolic coordination (the ritual DOES anchor their identity). Directionality near symmetric (0.4-0.6) — they are neither targets nor pure beneficiaries; they are coordinated members who also pay the cost of rigidity. No overrides needed; the structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-catastrophe identity-dissolution) is DEAD or substantially solved. Seventy years on, the group's identity is stable, reproduced through family and education and diaspora networks, not dependent on the ritual's symbolic rigidity. The ritual persists because it is now PERFORMANCE of continuity rather than NECESSITY for survival. The constraint does not show full mandatrophy characteristics (it is not completely inert, theater ratio is high but not dominant, some functional residue remains) but it shows PARTIAL MANDATE OBSOLESCENCE — the problem it was built for is historical, and the cost of maintaining it (suppression of adaptive modification, identity costs for those whose reality diverges) is increasingly borne by those without power to revise it. The theater-ratio rise over time is the key signal: when performance becomes more of the constraint's activity than actual coordination, the mandate is atrophying. The slight plateau and minor decline in theater after time-50 suggests some degree of adaptive recalibration within the tradition-keeping community — reinterpretation of the ritual's meaning without changing its form, a kind of invisible modification that allows younger members to find contemporary resonance in the ancient practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_necessity_vs_inertia,
    'Is the ritual''s symbolic continuity function genuinely necessary for collective identity preservation, or is the identity now self-sustaining through other mechanisms and the ritual''s symbolic claim is inertia?',
    'Ethnographic study of communities that have substantially modified or abandoned the ritual: does their collective identity fragment or adapt? Do they develop new symbolic forms? Do younger members report weaker sense of group continuity or do they construct new anchors?',
    'If symbolic continuity is genuinely necessary, the constraint is ROPE with real coordination function. If identity is self-sustaining and the ritual is theatrical residue, the constraint is PITON with performative maintenance covering mandate obsolescence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_necessity_vs_inertia, empirical, 'Whether the ritual''s symbolic function is operationally necessary or primarily performative maintenance.').

omega_variable(
    identity_lock_internalization_vs_structural,
    'Is the identity-lock binding agents to the ritual internalized (they have come to believe they SHOULD maintain it even when it costs them) or structurally coercive (they are afraid of expulsion if they don''t)?',
    'Longitudinal study of those who leave the community: do they maintain ritual practice in private? Do they express relief at abandonment or guilty conflict? Interview younger members about how they experience the permission/prohibition structure.',
    'If internalized, suppression is higher than the raw structural measure suggests — the constraint has colonized its targets'' own judgment. If structural, the constraint might be loosened by removing expulsion threats. The distinction determines whether de-enforcement of the constraint would feel like liberation or identity-loss to participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_vs_structural, empirical, 'Whether identity-lock suppression is internalized or structurally coercive.').

omega_variable(
    ritual_decomposition_possibility,
    'Can the ritual be decomposed into the symbolic-transmission function (which operates via fixed form) and the survival-encoding function (which might operate via adaptive form), such that each can be carried independently?',
    'Comparative study of related traditions: do all catastrophe-surviving communities maintain ritual rigidity, or do some maintain meaning-transmission through flexible forms while encoding survival competence through separate mechanisms?',
    'If decomposable, the constraint is unnecessary — symbolic continuity could be carried by adapted ritual and survival-encoding by explicit teaching. If inseparable, the fixed form is genuinely necessary. This determines whether the constraint is rent-seeking or coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_decomposition_possibility, conceptual, 'Whether symbolic-transmission rigidity is structurally necessary or merely traditional.').

omega_variable(
    sibling_reading_empirical_status,
    'Which of the sibling readings (survival_competence, trauma_encoding, boundary_maintenance) actually operationalizes in this community''s lived practice? Are they co-present in the ritual or genuinely distinct?',
    'Ethnographic interviews asking practitioners: what does the ritual DO for you? Why do you maintain it? What happens if you modify it? Code responses by the reading categories (identity-transmission vs. survival-encoding vs. trauma-warning vs. boundary-enforcement). Measure whether practitioners mention one dominant function or multiple co-present functions.',
    'If multiple readings co-operationalize in the same ritual, the four stories represent one constraint''s multiple functions, not a genuine kernel decomposition. If practitioners clearly identify one primary function (symbol-transmission for some, survival-encoding for others, trauma-warning for a third subset), the kernel genuinely decomposes and the four readings are separable constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_status, empirical, 'Whether the sibling readings represent genuinely distinct constraint-functions or co-present operations of one multi-function constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.63).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.65).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.63).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 70, 0.62).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.27).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 70, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.16).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.17).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 70, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_memory_kernel — a persisting institutional commitment ('ritual serves catastrophe-surviving communities') that different communities interpret structurally differently. Symbol_continuity_reading foregrounds identity-transmission and has low extractiveness; survival_competence_reading foregrounds operational encoding and has moderate extractiveness; trauma_encoding_reading foregrounds intergenerational warning and has different victim structure; boundary_maintenance_reading foregrounds enforcement and has high suppression. Each reading has its own ε, beneficiary/victim structure, and classification. All four affect one another because they share the same ritual practice — modifying the ritual on survival-competence grounds would alter its symbolic-continuity function; enforcing it for boundary maintenance would constrain its trauma-encoding potential. The four stories are linked by network.affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
