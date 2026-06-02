% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor Satisfaction Mechanism — Contraction Reading (Cognitive Evacuation)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism — the use of ritualized violence
 *   (dueling) to restore or defend personal honor in cases of insult —
 *   functioned for centuries as a live structural constraint within European
 *   aristocratic and educated classes. Across the 17th-18th centuries, it was
 *   a coherent, publicly defended, and widely practiced solution to the
 *   problem of how injured honor could be satisfied outside legal channels.
 *   By the early 19th century, across most of Western Europe and North
 *   America, the mechanism had become categorically unthinkable. Not merely
 *   illegal (it had been illegal in many jurisdictions for a century or
 *   more), but unthinkable — actors could no longer conceive of dueling as a
 *   legitimate satisfaction mechanism for honor, even as a matter of abstract
 *   principle. The contraction reading models this shift as a genuine
 *   cognitive category evacuation: the constraint does not disappear through
 *   suppression, replacement, or enforcement, but through the collapse of the
 *   conceptual framework that made it thinkable. What was once a
 *   category-level necessity (honor requires satisfaction; dueling satisfies
 *   honor) became a category-level impossibility (honor is no longer a
 *   concept through which violence can be justified). This is distinct from
 *   the decline reading (dueling suppressed and rejected but cognitively
 *   thinkable as a live alternative) and the composite reading (dueling
 *   persists in residual pockets while being suppressed at the center). The
 *   contraction reading asserts that the category itself has been evacuated
 *   from epistemic possibility — modern actors cannot think their way into
 *   the honor-satisfaction-by-dueling framework even as a historical
 *   curiosity. They can understand it intellectually, but they cannot inhabit
 *   its logic.
 *
 * KEY AGENTS:
 *   - The Aristocratic Honor Framework (institutional/arbitrage): Primary beneficiary of the dueling mechanism; used it to resolve disputes and maintain status hierarchies. Benefits from the clarity and finality of violent resolution.
 *   - The Injured Party (powerless/trapped): Forced to defend honor through dueling or face social death. Bears extraction through the mechanism itself.
 *   - The Cognitive Authority Structure (institutional/analytical): The network of law, education, philosophy, and social norm-setting that gradually evacuated the category. Not active agents but structural forces that shift what counts as thinkable.
 *   - The Modern Observer (analytical/analytical): Cannot inhabit the logic of honor-satisfaction-by-violence; can only observe the historical framework from outside it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.08).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.02).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor Satisfaction Mechanism — Contraction Reading (Cognitive Evacuation)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '70eb8dbc-c637-41fc-9889-148f0b32ed9d').
narrative_ontology:cs_kernel_codification('70eb8dbc-c637-41fc-9889-148f0b32ed9d', distributed).
narrative_ontology:cs_authority_grounding('70eb8dbc-c637-41fc-9889-148f0b32ed9d', lineage).
narrative_ontology:cs_interpretation_layer_present('70eb8dbc-c637-41fc-9889-148f0b32ed9d').
narrative_ontology:cs_reading_relation('70eb8dbc-c637-41fc-9889-148f0b32ed9d', honor_satisfaction_mechanism__decline_reading, forecloses).
narrative_ontology:cs_reading_relation('70eb8dbc-c637-41fc-9889-148f0b32ed9d', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('70eb8dbc-c637-41fc-9889-148f0b32ed9d', foundational, honor_satisfaction_category_evacuated).
narrative_ontology:cs_axiom_status(honor_satisfaction_category_evacuated, holdable).
narrative_ontology:cs_axiom_grounding('70eb8dbc-c637-41fc-9889-148f0b32ed9d', honor_satisfaction_category_evacuated, empirically_contingent).
narrative_ontology:cs_axiom('70eb8dbc-c637-41fc-9889-148f0b32ed9d', secondary, cognitive_categories_are_epistemic_boundaries).
narrative_ontology:cs_axiom_status(cognitive_categories_are_epistemic_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('70eb8dbc-c637-41fc-9889-148f0b32ed9d', cognitive_categories_are_epistemic_boundaries, deontological).
narrative_ontology:cs_reference_frame('70eb8dbc-c637-41fc-9889-148f0b32ed9d', honor_satisfaction_through_ritual_violence).
narrative_ontology:cs_drift_state('70eb8dbc-c637-41fc-9889-148f0b32ed9d', enlightenment_transition_to_legalism, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('70eb8dbc-c637-41fc-9889-148f0b32ed9d', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POST-SHIFT ACTOR (MOUNTAIN) — Once the cognitive category of 'honor' as a satisfaction mechanism has been evacuated from possibility space, the actor cannot even conceive of the dueling solution as thinkable. The constraint is not suppressed — it has been erased from the epistemic horizon. What was once a live option is now literally unthinkable. The agent faces a category-level impossibility, not a choice with high cost.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__contraction_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From the standpoint of cognitive archaeology, the shift in what counts as thinkable is a structural fact about epistemic possibility, not a policy choice or enforcement mechanism. The evacuation of 'honor satisfaction by violence' from the category-space of permissible conflict resolution is as immutable as a logical or mathematical limit — once the conceptual framework has shifted, the previous option is categorically unavailable. This is a genuine natural law of cultural cognition, not a suppressed alternative.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__contraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(honor_satisfaction_mechanism__contraction_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_satisfaction_mechanism__contraction_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

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
 *   Extractiveness (0.08): Very low. In the contraction reading, the constraint is nearly pure natural law — the evacuation of a cognitive category from possibility space. There is no residual extraction because there are no actors left for whom the mechanism remains a live option. The small non-zero value (0.08 rather than 0.00) reflects minor historical ambiguity: some actors in the 18th century may have experienced dueling as an extractive constraint (forced to participate or lose honor), but by the 19th century, the cognitive evacuation is nearly complete. Suppression (0.02): Minimal. Once a cognitive category has been evacuated, no suppression is needed. The constraint persists in history texts and legal statutes as a prohibition, but the prohibition enforces something that is already categorically impossible. Theater ratio (0.15): Low. The remaining 'theater' is the continued legal prohibition against dueling, which persists partly as residual enforcement and partly as symbolic confirmation that the category has been evacuated. There is little performative activity left because the category itself is gone.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints, this one exhibits zero perspectival gap across the two perspectives presented. Both the post-shift actor and the analytical observer see the same structural reality: a cognitive category has been evacuated from possibility space. This uniformity is diagnostic of a genuine mountain constraint. There is no beneficiary perspective showing rope or tangled_rope — there is no living beneficiary position available. The constraint does not show up differently from different power positions because it is no longer a structural feature of the world; it is a historical artifact. The absence of perspectival gap is itself the signal that the mountain classification is correct: the constraint has ceased to exist as a live structural problem.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared because in the contraction reading, there are no living agents who benefit from the constraint. The honor-satisfaction mechanism benefited aristocratic actors in the 17th-18th centuries, but those actors and their framework are now historical artifacts. Modern observers cannot occupy the beneficiary position because they cannot inhabit the cognitive category that made the mechanism coherent. This absence of a living beneficiary is a diagnostic marker of the contraction reading: the constraint has been not suppressed but structurally evacuated.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_shift_mechanism_ambiguity,
    'Is the evacuation of honor-satisfaction-by-dueling from cognitive possibility driven by logical inconsistency within the honor framework itself, by external replacement of the honor framework with an incompatible framework (e.g., legalism, mediated justice), or by both?',
    'Detailed textual analysis of contemporaneous sources: do 18th-19th century writers attempting to defend dueling do so by defending honor as a value, or by accepting honor but arguing dueling is not a valid satisfaction mechanism? Do they argue dueling is logically incoherent within honor logic, or that honor itself has become obsolete?',
    'If logical inconsistency: the mountain classification is stable — the contraction reading is correct. If framework replacement: the classification may degrade to rope or piton at regional scale — the contraction reading treats replacement as cognitive shift, but it could be enforcement + persistence of the old framework among holdouts. If both: the contraction reading captures the dominant mechanism but omegas must record residual populations still holding the old framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_shift_mechanism_ambiguity, empirical, 'Mechanism of cognitive shift: internal inconsistency vs. framework replacement').

omega_variable(
    universal_vs_regional_cognitive_evacuation,
    'Is the evacuation of dueling from cognitive possibility universal across the educated European population, or is it regionally and class-stratified, with pockets of the old framework persisting among non-cosmopolitan actors?',
    'Longitudinal historical records: tracking of dueling incidents and the explicit justifications offered for them across regions, social classes, and time periods. Mapping of where the cognitive category persists vs. where it has been evacuated.',
    'If universal: mountain classification holds globally. If stratified: the constraint is a mountain only at the cosmopolitan/educated level; it remains rope or tangled_rope at regional/local scales where the old framework has not been fully evacuated. The scope modifier σ(S) then becomes critical — at universal scope, mountain holds; at regional/local scope, classification degrades.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_vs_regional_cognitive_evacuation, empirical, 'Scope and stratification of cognitive evacuation').

omega_variable(
    contraction_vs_decline_reading_boundary,
    'How do we distinguish between the contraction reading (honor-satisfaction-by-dueling becomes categorically unthinkable) and the decline reading (dueling becomes suppressed but cognitively thinkable as a live alternative that actors reject for prudential or legal reasons)?',
    'Textual analysis of attempted justifications for continued dueling: if actors explicitly argue ''dueling IS a valid honor satisfaction mechanism and should be legal,'' the decline reading holds (the category remains thinkable). If actors argue ''dueling was never a valid satisfaction mechanism'' or ''honor no longer works that way,'' the contraction reading holds. The distinction is whether actors defend the mechanism or defend the honor logic while abandoning the mechanism.',
    'This is the critical reading-discrimination test. Contraction reading requires that contemporary actors cannot meaningfully defend dueling even as a matter of principle — the category has been genuinely evacuated. If significant populations of 19th-century lawyers and philosophers continued to defend honor-by-dueling as a principle (while pragmatically accepting legal prohibition), the decline reading should replace contraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contraction_vs_decline_reading_boundary, empirical, 'Boundary between contraction and decline readings: principled defense vs. categorical evacuation').

omega_variable(
    natural_law_false_summit_check,
    'Is this a genuine natural law of cognitive possibility (the contraction reading), or is it a constructed institutional arrangement (alternative reading that would trigger false summit detection)?',
    'Test whether the evacuation of dueling from cognitive possibility could have been different given different institutional and legal arrangements. If dueling would still be unthinkable even if legal sanctions were removed, the evacuation is cognitive/cultural (mountain). If dueling would immediately resurge as a thinkable option once legal and social sanctions were lifted, the evacuation is institutional suppression (snare or tangled_rope under alternative reading).',
    'If genuine cognitive shift: mountain classification is defensible. If institutional suppression masquerading as cognitive shift: the contraction reading is a false summit that naturalizes contingent arrangements. The test is not whether dueling is currently illegal, but whether the category remains alive in actors'' epistemic possibility space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_check, conceptual, 'Whether cognitive evacuation is genuine or institutional suppression misframed as cognitive shift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1650, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_theater_1650, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1650, 0.05).
narrative_ontology:measurement(honor_theater_1750, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(honor_theater_1850, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1850, 0.15).

% Extraction over time
narrative_ontology:measurement(honor_extract_1650, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1650, 0.08).
narrative_ontology:measurement(honor_extract_1750, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1750, 0.07).
narrative_ontology:measurement(honor_extract_1850, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1850, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, legal_prohibition_escalation).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, violence_legitimacy_category_collapse).

% DUAL FORMULATION NOTE:
% The honor satisfaction mechanism kernel admits three structurally distinct constraint stories: (1) contraction_reading — cognitive category evacuated, constraint ceases to exist; (2) decline_reading — constraint suppressed but cognitively thinkable as live alternative; (3) composite_reading — partial evacuation, constraint persists in residual populations. Each reading has different ε, different measurement profiles, and different classification. The network links show how they affect each other: contraction reading's cognitive evacuation influences (but does not foreclose) the decline and composite readings at different scopes and populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
