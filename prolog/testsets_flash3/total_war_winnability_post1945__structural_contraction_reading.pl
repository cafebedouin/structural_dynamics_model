% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Total War Winnability (Structural Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'structural contraction' reading of the
 *   post-1945 strategic environment, where nuclear weapons are understood to
 *   have physically removed the possibility of winning a total war. It is a
 *   Mountain-class constraint because its persistence is due to the immutable
 *   physical reality of nuclear destruction, not social agreement or
 *   enforcement. There are no beneficiaries in the extractive sense, as no
 *   party 'collects' from this physical impossibility; the 'victims' are
 *   hypothetical populations in a counterfactual nuclear exchange. The
 *   constraint is not 'socially abandoned' but 'structurally impossible'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.01).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.99).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.01).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.99).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Total War Winnability (Structural Contraction Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, 'd9b0bee2-8fe8-492e-88da-57695c916dec').
narrative_ontology:cs_kernel_codification('d9b0bee2-8fe8-492e-88da-57695c916dec', implicit).
narrative_ontology:cs_authority_grounding('d9b0bee2-8fe8-492e-88da-57695c916dec', self_enforcing).
narrative_ontology:cs_reading_relation('d9b0bee2-8fe8-492e-88da-57695c916dec', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('d9b0bee2-8fe8-492e-88da-57695c916dec', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('d9b0bee2-8fe8-492e-88da-57695c916dec', foundational, total_war_is_physically_unwinnable).
narrative_ontology:cs_axiom_status(total_war_is_physically_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('d9b0bee2-8fe8-492e-88da-57695c916dec', total_war_is_physically_unwinnable, empirically_contingent).
narrative_ontology:cs_axiom('d9b0bee2-8fe8-492e-88da-57695c916dec', foundational, nuclear_weapons_alter_strategic_space).
narrative_ontology:cs_axiom_status(nuclear_weapons_alter_strategic_space, holdable).
narrative_ontology:cs_axiom_grounding('d9b0bee2-8fe8-492e-88da-57695c916dec', nuclear_weapons_alter_strategic_space, empirically_contingent).
narrative_ontology:cs_reference_frame('d9b0bee2-8fe8-492e-88da-57695c916dec', pre_nuclear_strategic_calculus).
narrative_ontology:cs_drift_state('d9b0bee2-8fe8-492e-88da-57695c916dec', post_nuclear_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d9b0bee2-8fe8-492e-88da-57695c916dec', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states possess the means to initiate a nuclear exchange, but their strategic calculus is constrained by the physical impossibility of winning a total war. They observe the constraint as an immutable fact of the nuclear age.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_armed_states, observer,
    institutional, generational, analytical, global).

% These states are subject to the global strategic environment shaped by nuclear weapons. They observe the constraint as a fundamental alteration of international security, even if they do not directly wield the weapons.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states, observer,
    moderate, generational, analytical, global).

% The ultimate hypothetical victim of a nuclear exchange, this population exists under the shadow of the structural contraction. While not actively paying, their existence is fundamentally altered by the physical limits on total war.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, global_population, observer,
    powerless, immediate, trapped, universal).

% These analysts study the implications of nuclear weapons for international conflict. They interpret the constraint as a fundamental, physical alteration of the strategic landscape, rather than a social or normative one.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint itself does not coordinate; rather, it imposes a physical limit that forces states to coordinate their strategic behavior to avoid mutual annihilation. It creates a 'negative coordination' by foreclosing a prior option.
% TRANSFER_FUNCTION: No direct transfer of resources. Instead, it transfers the concept of 'winnable total war' from the realm of possibility to impossibility, altering the fundamental calculus of state power and conflict.
% ABSENT_VOICES: Pre-nuclear strategic thinkers who believed in the utility and winnability of total war would find their foundational assumptions foreclosed by this structural reality. Their voices are absent because their strategic universe no longer exists.
% DISAPPEARANCE_RATIONALE: If the physical impossibility of winning total war vanished (e.g., through a technological breakthrough rendering nuclear weapons obsolete or harmless), the entire global strategic landscape would rearrange. States would revert to pre-nuclear strategic thinking, potentially leading to a resurgence of conventional total war as a viable option.
% FOUNDING_PROBLEM: The problem of how to prevent catastrophic, civilization-ending conflict in an era of unprecedented destructive power.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals and the doctrine of mutually assured destruction (MAD) by nuclear-armed states, along with ongoing international efforts for non-proliferation, corroborate that the problem of preventing catastrophic conflict remains live. Independent strategic analyses and historical records from outside the benefiting parties (e.g., non-nuclear states, academic institutions) consistently affirm this.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.01, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero because no agent benefits from its operation; it simply describes a physical limit. Suppression is near maximal because the physical reality of nuclear weapons fundamentally suppresses the option of total war. Theater ratio is zero as there is no performative aspect to a physical impossibility. Accessibility collapse is high because the option of 'winnable total war' is almost entirely foreclosed. Resistance is negligible because one cannot 'resist' a physical law.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap in the experience of this constraint, as its nature is physical and universal. All rational actors, regardless of their power or position, must contend with the same physical impossibility. Divergence arises in how this physical reality is interpreted (e.g., as a normative shift vs. a structural one), which is handled by the kernel's sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents, from nuclear-armed states to global populations, are structurally 'observers' of this constraint. Their relationship is one of being subject to a physical reality, not of being beneficiaries or targets in an extractive sense. The constraint subsidizes no one and extracts from no one; it simply defines the boundaries of the possible.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_normative_causation,
    'Is the absence of total war primarily due to the physical impossibility (structural contraction) or to the development of international norms against it (normative drop)?',
    'Counterfactual historical analysis: if nuclear weapons had been developed but international norms had not evolved, would total war still be absent? Or, if norms had evolved without nuclear weapons, would total war be absent?',
    'If primarily normative, this constraint would be reclassified from Mountain to a social construct (e.g., Rope or Tangled Rope), with identifiable beneficiaries (e.g., international legal institutions) and active enforcement. If primarily structural, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_normative_causation, conceptual, 'Distinguishing physical impossibility from normative prohibition as the primary cause for the absence of total war.').

omega_variable(
    structural_vs_ideational_causation,
    'Is the absence of total war primarily due to the physical impossibility (structural contraction) or to a shift in strategic culture and elite discourse (ideational drift)?',
    'Analysis of strategic decision-making processes: do decision-makers primarily cite physical limits or evolving strategic thought when discussing total war? Examination of historical archives and declassified documents.',
    'If primarily ideational, this constraint would be reclassified from Mountain to a social construct (e.g., Rope or Tangled Rope), with identifiable agenda-setters (e.g., strategic elites) and a different beneficiary structure. If primarily structural, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_ideational_causation, empirical, 'Distinguishing physical impossibility from ideational shifts in strategic culture as the primary cause for the absence of total war.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(tota_tr_t1980, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(tota_tr_t2000, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1960, 0.01).
narrative_ontology:measurement(tota_be_t1980, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1980, 0.01).
narrative_ontology:measurement(tota_be_t2000, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2000, 0.01).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2024, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1945, 0.99).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1960, 0.99).
narrative_ontology:measurement(tota_su_t1980, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1980, 0.99).
narrative_ontology:measurement(tota_su_t2000, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2000, 0.99).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2024, 0.99).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
