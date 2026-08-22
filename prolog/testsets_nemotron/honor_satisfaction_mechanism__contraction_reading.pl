% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Cognitive Unthinkability of Honor Dueling (Contraction Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the contraction_reading of the
 *   honor_satisfaction_mechanism kernel. The reading holds that dueling did
 *   not decline, get suppressed, or get replaced — it became cognitively
 *   unthinkable, a category-level impossibility. The constraint is the
 *   structural evacuation of the 'duelling subject' from the space of
 *   possible modern persons. This is not a prohibition (which requires
 *   enforcement) but a cognitive-ontological boundary: the modern subject is
 *   *constituted* by the unavailability of honor-violence as a live option.
 *   The constraint has near-zero extractiveness and suppression in the
 *   present because it requires no enforcement — it is a Mountain of the
 *   cognitive order. The historical suppression_requirement measurements
 *   trace the *transition period* when active enforcement (legal penalties,
 *   social sanctions, military codes) was still required to police the
 *   boundary; by the mid-20th century, the boundary had been internalized as
 *   cognitive infrastructure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.02).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.01).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Cognitive Unthinkability of Honor Dueling (Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '2ac31c41-f230-4970-b5d8-c3dea6b3a063').
narrative_ontology:cs_kernel_codification('2ac31c41-f230-4970-b5d8-c3dea6b3a063', implicit).
narrative_ontology:cs_authority_grounding('2ac31c41-f230-4970-b5d8-c3dea6b3a063', diffuse_epistemic).
narrative_ontology:cs_reading_relation('2ac31c41-f230-4970-b5d8-c3dea6b3a063', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ac31c41-f230-4970-b5d8-c3dea6b3a063', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('2ac31c41-f230-4970-b5d8-c3dea6b3a063', foundational, dueling_subject_position_evacuated).
narrative_ontology:cs_axiom_status(dueling_subject_position_evacuated, holdable).
narrative_ontology:cs_axiom_grounding('2ac31c41-f230-4970-b5d8-c3dea6b3a063', dueling_subject_position_evacuated, empirically_contingent).
narrative_ontology:cs_axiom('2ac31c41-f230-4970-b5d8-c3dea6b3a063', foundational, modern_personhood_requires_honor_violence_unthinkability).
narrative_ontology:cs_axiom_status(modern_personhood_requires_honor_violence_unthinkability, holdable).
narrative_ontology:cs_axiom_grounding('2ac31c41-f230-4970-b5d8-c3dea6b3a063', modern_personhood_requires_honor_violence_unthinkability, deontological).
narrative_ontology:cs_reference_frame('2ac31c41-f230-4970-b5d8-c3dea6b3a063', honor_society_subjectivity).
narrative_ontology:cs_drift_state('2ac31c41-f230-4970-b5d8-c3dea6b3a063', contemporary_modernity, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2ac31c41-f230-4970-b5d8-c3dea6b3a063', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, modern_personhood_boundary_excludes_violent_honor).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, state_monopoly_on_violence_is_cognitive_not_just_coercive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Observes the historical transformation from a society where elite men could challenge each other to lethal combat over insult, to a society where the very concept of resolving honor through violence has become cognitively unavailable — not prohibited, but unthinkable. No personal stake in the constraint; tracks the structural shift in possibility space.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary of legitimate personhood in modernity: the category of 'person who resolves honor through lethal violence' has been evacuated, making the modern subject structurally incapable of occupying that position. Solves the coordination problem of defining the post-dueling subject without requiring continuous enforcement.
% TRANSFER_FUNCTION: Transfers the capacity for honor-violence from the domain of live options to the domain of category errors. No material flows; the transfer is cognitive-ontological — the option is removed from the possibility space of what a 'civilized person' can do.
% ABSENT_VOICES: The historical duellists themselves — the aristocratic and military elites for whom the duel was a live practice — are absent because the constraint operates by making their subject-position historically extinct. They cannot object because the kind of person who would object no longer exists as a recognized social type.
% DISAPPEARANCE_RATIONALE: If the cognitive unthinkability vanished overnight, dueling would not return — the material, institutional, and normative conditions that made it a live practice (aristocratic honor culture, military officer culture, absence of state monopoly on interpersonal violence) are historically gone. The constraint is a category-level fossil: the world arranged itself *around* the evacuation, and the evacuation persists without the world depending on it.
% FOUNDING_PROBLEM: The problem was not 'how to stop dueling' but 'what kind of subject survives the transition from honor society to rights-bearing citizenry.' The evacuation of the dueling subject-position is the solution: a person who *cannot* duel is a person who can be a rights-holder.
% FOUNDING_PROBLEM_CORROBORATION: No beneficiaries exist to self-assert this genealogy. The reading is corroborated by historical sociology (Elias, The Civilizing Process; Frevert, Men of Honour) showing the disappearance of dueling as a structural transformation of subjectivity, not a policy outcome. The founding problem (creating the modern subject) is dead — the subject exists — but the constraint (cognitive unthinkability) persists as a Mountain.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is mountain because the constraint operates as a natural law of the modern cognitive order: the category 'person who duels for honor' has the same structural status as 'married bachelor' — a category error, not a prohibited act. Extractiveness is 0.02 (residual: occasional performative revivals in subcultures that prove the rule). Suppression is 0.01 (no active enforcement needed). Theater is 0.0 (no performance of maintaining the constraint). Accessibility_collapse is 0.95 (alternatives are not just blocked but cognitively unavailable). Resistance is 0.02 (marginal subcultural revivals that confirm the boundary). The metrics and claim are independent: the metrics describe the *current* structural operation; the claim describes the *type*. The engine computes per-seat classification from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The contraction_reading and decline_reading would compute different effective extractions for historical agents (the decline_reading sees ongoing suppression of a live practice; the contraction_reading sees a practice that has already been evacuated from possibility space). The engine computes this divergence from the structural data authored in each reading's story.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims exist in the present — the constraint extracts from no one and suppresses no one. The single stakeholder is the analytical_observer. Directionality derivation yields d ≈ 0.5 (symmetric) for all power atoms because the constraint is a structural feature of the shared cognitive environment. The historical suppression_requirement series (declining from 0.85 to 0.01) reflects the *transition* from enforced prohibition to cognitive evacuation, not the current constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has no mandate — it is not an arrangement that outlived its function. It is a cognitive boundary that *became* the function (defining the modern subject). Mandatrophy is inapplicable; the constraint is not a zombie institution but a Mountain of the normative order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_evacuation_vs_suppression,
    'Is the unthinkability of dueling a genuine cognitive-ontological boundary (Mountain) or a deeply internalized suppression that *feels* like unthinkability (Snare/Tangled Rope with internalized suppression)?',
    'Cross-cultural and cross-temporal comparison: if societies without the Western ''civilizing process'' history show the same cognitive unavailability of honor-dueling among modernized elites, the boundary is structural to modernity. If the unavailability tracks exactly the historical enforcement trajectory, it is internalized suppression.',
    'If internalized suppression, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression cognitively. The classification would shift from mountain to piton or snare depending on whether any beneficiary captures the internalized suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_evacuation_vs_suppression, conceptual, 'Whether cognitive unthinkability is a natural law of modern subjectivity or an internalized historical suppression.').

omega_variable(
    reading_relations_kernel_contestation,
    'Does the contraction_reading foreclose the decline_reading and composite_reading, or do they coexist as live historical interpretations?',
    'Historiographical analysis: if the decline_reading''s claim (dueling persisted as a live practice until fringe status) is empirically true for some contexts (e.g., German Mensur, Southern US dueling post-1865), then the readings coexist as describing different historical trajectories. If the contraction_reading''s claim (cognitive evacuation) is a structural generalization that *subsumes* the decline_reading''s empirical persistence as ''residual performances after cognitive evacuation,'' then the contraction_reading influences but does not foreclose the others.',
    'If forecloses: the kernel has a single true reading (contraction). If coexists_with: the kernel is genuinely contested across historiographical frameworks. If influences: the contraction_reading provides the structural frame within which the others'' empirical claims are situated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_kernel_contestation, conceptual, 'Structural relationship between this reading and its sibling readings of the honor_satisfaction_mechanism kernel.').

omega_variable(
    founding_problem_corroboration_gap,
    'Is the ''modern subject constitution'' founding problem a genuine historical genealogy or a retrospective rationalization of the cognitive evacuation?',
    'Intellectual history of the ''civilizing process'' thesis: trace whether the evacuation of honor-violence was *intended* to produce the rights-bearing subject (Elias: no, it was an unintended structural drift) or whether the founding problem is imputed by later theory.',
    'If retrospective rationalization, the founding_problem_status ''dead'' is misleading — there was no founding problem, only an emergent structural outcome. The constraint would be a genuine Mountain with no teleological genealogy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_corroboration_gap, empirical, 'Whether the genealogical account of the constraint''s origin is historically grounded or theoretically imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1750, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1750, 0.0).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(hono_tr_t2000, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(hono_tr_t2025, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 2025, 0.0).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1750, 0.0).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1800, 0.0).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1850, 0.0).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1900, 0.0).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1950, 0.0).
narrative_ontology:measurement(hono_be_t2000, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement(hono_be_t2025, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 2025, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1750, 0.85).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(hono_su_t2000, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 2000, 0.01).
narrative_ontology:measurement(hono_su_t2025, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 2025, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__contraction_reading, 0.05).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This story and its siblings form a constraint family decomposing the 'honor satisfaction mechanism' label. The contraction_reading claims the mechanism is a cognitive evacuation (Mountain, ε≈0). The decline_reading claims it is a practice that faded under suppression (Snare/Piton, higher ε). The composite_reading claims multiple mechanisms operated (constraint family with internal network edges). All three share the kernel_id but instantiate distinct constraints with different ε, stakeholders, and types — per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
