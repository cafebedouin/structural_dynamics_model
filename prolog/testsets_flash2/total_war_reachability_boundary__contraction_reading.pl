% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint represents the 'contraction reading' of the total war
 *   reachability boundary kernel. It posits that the advent of nuclear
 *   weapons fundamentally and irreversibly contracted the strategic space,
 *   making winnable total war a physical impossibility due to the condition
 *   of Mutual Assured Destruction (MAD). This is treated as a Mountain, an
 *   unchangeable physical/logical limit, with universal victims (human
 *   species, global ecosystems) and no identifiable beneficiaries, as no
 *   actor can 'win' under MAD. The metrics reflect this: negligible
 *   extractiveness (it doesn't extract, it limits), high suppression (it
 *   suppresses the option of total war), and zero theater (it's a physical
 *   reality, not a performance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, 'a412436d-8bad-4983-b822-98bebe16cb45').
narrative_ontology:cs_kernel_codification('a412436d-8bad-4983-b822-98bebe16cb45', implicit).
narrative_ontology:cs_authority_grounding('a412436d-8bad-4983-b822-98bebe16cb45', self_enforcing).
narrative_ontology:cs_reading_relation('a412436d-8bad-4983-b822-98bebe16cb45', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_reading_relation('a412436d-8bad-4983-b822-98bebe16cb45', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('a412436d-8bad-4983-b822-98bebe16cb45', foundational, mutual_assured_destruction_is_permanent).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_permanent, holdable).
narrative_ontology:cs_axiom_grounding('a412436d-8bad-4983-b822-98bebe16cb45', mutual_assured_destruction_is_permanent, empirically_contingent).
narrative_ontology:cs_axiom('a412436d-8bad-4983-b822-98bebe16cb45', foundational, winnable_total_war_is_a_physical_impossibility).
narrative_ontology:cs_axiom_status(winnable_total_war_is_a_physical_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('a412436d-8bad-4983-b822-98bebe16cb45', winnable_total_war_is_a_physical_impossibility, empirically_contingent).
narrative_ontology:cs_reference_frame('a412436d-8bad-4983-b822-98bebe16cb45', post_hiroshima_strategic_reality).
narrative_ontology:cs_drift_state('a412436d-8bad-4983-b822-98bebe16cb45', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a412436d-8bad-4983-b822-98bebe16cb45', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, human_species).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, global_ecosystems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces existential threat from any large-scale nuclear exchange, regardless of intent or outcome. Bears the ultimate cost of the constraint's existence, with no agency to alter it.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, human_species, payer,
    powerless, civilizational, trapped, universal).

% Would suffer catastrophic, irreversible damage from nuclear winter and widespread radiation, leading to mass extinctions and collapse of planetary support systems. Bears the environmental cost.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, global_ecosystems, payer,
    powerless, civilizational, trapped, universal).

% Possess the means to initiate total war, but are also the primary agents whose strategic calculus is constrained by the threat of mutual assured destruction. They administer the nuclear arsenals and deterrence doctrines, but cannot escape the physical reality of the constraint.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Study the implications of nuclear weapons for international security, developing theories of deterrence and arms control. They observe the constraint's effects on state behavior and the global strategic landscape.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint itself does not coordinate; rather, it imposes a physical limit that forces coordination (deterrence) among nuclear powers by making total war unwinnable.
% TRANSFER_FUNCTION: Transfers the concept of 'winnable total war' from the realm of feasible strategic options to the realm of physical impossibility, effectively transferring existential risk to the entire species.
% ABSENT_VOICES: Future generations and non-human species, who bear the ultimate, irreversible costs of nuclear war, have no voice in the strategic decisions that maintain this boundary.
% DISAPPEARANCE_RATIONALE: If the physical reality of nuclear weapons' destructive power (and thus the MAD condition) were to disappear overnight, the strategic calculus of great powers would fundamentally revert to pre-nuclear norms, making large-scale conventional total war once again a 'winnable' option, and global security arrangements would be completely re-written.
% FOUNDING_PROBLEM: The problem of preventing existential catastrophe from great power conflict in an era of unprecedented destructive capability.
% FOUNDING_PROBLEM_CORROBORATION: International scientific bodies, UN resolutions, and the consistent strategic doctrines of nuclear powers (outside of any single benefiting party) corroborate that the problem of preventing nuclear war remains live and central to global security.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that the constraint does not actively extract resources or rents from any party; rather, it imposes a universal, species-level cost of existence under the threat of nuclear annihilation, which is an inherent property of the constraint itself, not an extraction. The high suppression (0.95) signifies the near-complete collapse of the option of 'winnable total war' from the strategic feasible set. The zero theater ratio (0.0) indicates that the constraint is a brute physical reality, not maintained by performance or institutional inertia. Accessibility collapse is near total (0.98) because the option of winnable total war is almost entirely foreclosed. Resistance is negligible (0.02) because the physical reality of MAD is not something that can be 'resisted' in a meaningful way by any actor.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap on the core physical reality of this constraint. While different actors may interpret its implications or the likelihood of its breach differently, the fundamental contraction of the strategic space is universally acknowledged by those who accept the MAD premise. The divergence arises in how other readings interpret the permanence or contingency of this boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   All actors, including nuclear powers, are structurally targets of this constraint, as none can escape the consequences of total war. The human species and global ecosystems are universal victims, bearing the ultimate, existential cost. Nuclear powers, while 'agenda-setters' in terms of maintaining arsenals, are also fundamentally constrained by the MAD condition, making them targets of the constraint's physical limits.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a Mountain, is not subject to mandatrophy in the conventional sense, as its function is a physical limit, not a human mandate. The question of its persistence is tied to the physical reality of nuclear weapons, not institutional inertia. The classification prevents mislabeling a physical limit as a human-constructed constraint that could be 'fixed' or 'reformed' by policy choices alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, permanent contraction of the strategic space, or is it a contingent state that could be altered by technological or political shifts?',
    'Empirical observation of future technological developments (e.g., effective missile defense, ''clean'' nuclear weapons) or shifts in international political order that fundamentally alter the MAD condition. Conceptual analysis of the definition of ''winnable total war''.',
    'If contingent, the constraint might reclassify as a Piton (atrophied capability) or a Rope (coordination equilibrium), depending on the nature of the contingency. If permanent, its Mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading (''contraction_reading'') of the ''total_war_reachability_boundary'' kernel. Sibling readings (''dropping_reading'', ''contingent_reachability_reading'') dispute the permanence and nature of this boundary. This omega captures the core ambiguity of the kernel itself.').

omega_variable(
    mad_stability_contingency,
    'Is the condition of Mutual Assured Destruction (MAD) a stable, permanent feature of the nuclear age, or is it vulnerable to technological breakthroughs or shifts in doctrine that could restore a ''first-strike advantage''?',
    'Ongoing analysis of strategic stability, arms race dynamics, and technological advancements in offensive and defensive nuclear capabilities. Historical analysis of ''windows of vulnerability'' debates.',
    'If MAD is unstable, the ''contraction_reading'' would be undermined, potentially shifting the constraint towards a ''contingent_reachability_reading'' (Piton) where total war is merely temporarily unreachable. If MAD is robust, the Mountain classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mad_stability_contingency, empirical, 'Assesses the robustness of the MAD condition, which is foundational to the ''contraction_reading''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(tota_tr_t1960, total_war_reachability_boundary__contraction_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__contraction_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(tota_be_t1960, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(tota_su_t1960, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1960, 0.95).
narrative_ontology:measurement(tota_su_t1980, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1980, 0.95).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_reachability_boundary' kernel. This 'contraction_reading' asserts total war is a Mountain (physically impossible). The 'dropping_reading' asserts it's a Rope (deterrence equilibrium). The 'contingent_reachability_reading' asserts it's a Piton (atrophied capability, could return). Each reading is a distinct constraint with its own metrics and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
