% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Possibility Space Contraction (Strategic Cognitive Foreclosure)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   Nuclear weapons altered strategic rationality by making total war
 *   logically incoherent as a strategic objective: any strategy aiming at
 *   complete victory against a nuclear-armed opponent is self-defeating
 *   because victory and mutual annihilation become indistinguishable. Under
 *   the SPACE-CONTRACTION reading, this incoherence removed total war not
 *   merely from what is preferable but from what is STRATEGICALLY THINKABLE.
 *   Military institutions institutionalized this cognitive foreclosure:
 *   war-gaming shifted from great-power total conflict to limited regional
 *   scenarios; mobilization doctrine atrophied; strategic planning space
 *   contracted around the assumption that great-power conflict would be
 *   non-nuclear or would never reach the existential phase. The constraint
 *   operates as a categorical impossibility, not as a high-cost option. This
 *   reading claims the constraint is a MOUNTAIN — an irreducible feature of
 *   rationality under nuclear conditions — while acknowledging that the
 *   existence of the constraint benefits identifiable institutional actors
 *   (general staffs freed from incoherent planning mandates) in ways that
 *   could trigger false-summit detection.
 *
 * KEY AGENTS:
 *   - Great-power general staffs and defense ministries (institutional agenda-setters, beneficiaries of simplified planning space)
 *   - Strategic studies and deterrence theory communities (observers, contesters of the reading)
 *   - Non-nuclear-armed states (structural victims of the constraint's asymmetry)
 *   - Mobilization doctrine practitioners and military historians (victims of institutional atrophy)
 *   - Civilian populations of nuclear powers (passive beneficiaries, identity-locked to constraint)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.0).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Possibility Space Contraction (Strategic Cognitive Foreclosure)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '78d8059e-ea08-4e71-9627-7e38284c3d07').
narrative_ontology:cs_kernel_codification('78d8059e-ea08-4e71-9627-7e38284c3d07', formalized).
narrative_ontology:cs_authority_grounding('78d8059e-ea08-4e71-9627-7e38284c3d07', expertise).
narrative_ontology:cs_interpretation_layer_present('78d8059e-ea08-4e71-9627-7e38284c3d07').
narrative_ontology:cs_reading_relation('78d8059e-ea08-4e71-9627-7e38284c3d07', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('78d8059e-ea08-4e71-9627-7e38284c3d07', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('78d8059e-ea08-4e71-9627-7e38284c3d07', foundational, total_war_cognitive_impossibility).
narrative_ontology:cs_axiom_status(total_war_cognitive_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('78d8059e-ea08-4e71-9627-7e38284c3d07', total_war_cognitive_impossibility, empirically_contingent).
narrative_ontology:cs_axiom('78d8059e-ea08-4e71-9627-7e38284c3d07', secondary, institutional_atrophy_of_total_war_planning).
narrative_ontology:cs_axiom_status(institutional_atrophy_of_total_war_planning, holdable).
narrative_ontology:cs_axiom_grounding('78d8059e-ea08-4e71-9627-7e38284c3d07', institutional_atrophy_of_total_war_planning, empirically_contingent).
narrative_ontology:cs_reference_frame('78d8059e-ea08-4e71-9627-7e38284c3d07', rational_total_war_incoherence).
narrative_ontology:cs_drift_state('78d8059e-ea08-4e71-9627-7e38284c3d07', contemporary_great_power_competition, gap(stable, minor, true)).
narrative_ontology:cs_created_at('78d8059e-ea08-4e71-9627-7e38284c3d07', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, great_power_strategic_planning_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, civilian_population_of_nuclear_armed_states).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, non_nuclear_armed_states).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, mobilization_doctrine_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% General staffs, defense ministries, and strategic planning bodies in nuclear-armed states. They benefit from the cognitive impossibility of total war planning: it removes an incoherent planning objective from their mandate. They set the institutional frame by determining which scenarios are 'strategically coherent' enough to plan for. Their discretion over the planning space itself is the form their benefit takes.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_strategic_planning_apparatus, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__space_contraction_reading, great_power_strategic_planning_apparatus, agenda_setter).

% Academic strategists, think-tank analysts, and policy intellectuals who study deterrence. They observe the constraint's operation and contest its characterization: some argue total war remains strategically thinkable but deterred; others argue it is normatively taboo rather than cognitively impossible. Their contestation is internal to the strategic studies discipline.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, deterrence_theory_community, observer,
    organized, biographical, analytical, global).

% States without nuclear weapons remain strategically thinkable targets for total war (or fear they are). They bear the asymmetry created by the constraint: total war is removed from great-power possibility space but not from the space of great-power-versus-non-nuclear-state conflict. Their vulnerability is the shadow cast by the constraint.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, non_nuclear_armed_states, payer,
    organized, generational, constrained, global).

% Military planners, historians, and logisticians trained in total-war mobilization. Their professional identity and expertise become obsolete or marginalized as total-war planning atrophies institutionally. They experience the constraint as institutional drift that devalues their knowledge and career paths.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, mobilization_doctrine_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Citizens of nuclear-armed states experience the constraint as protection: total war against their state is removed from strategic possibility. But they have no voice in the constraint's operation or in the strategic doctrines that govern their safety. Their benefit is passive and identity-locked to their national citizenship.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, civilian_population_of_nuclear_armed_states, beneficiary,
    powerless, biographical, trapped, national).

% Scholars and policy actors who argue total war became impossible through CONSTRUCTED NORM rather than through material/cognitive constraint. They are excluded from this reading's causal frame: under the space-contraction reading, the taboo is an EFFECT (expressed through atrophying institutions) rather than the CAUSE. They would argue for normative rather than cognitive mechanism.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_taboo_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This is a cognitive/institutional impossibility, not a coordination mechanism. It does not solve a collective-action problem; it forecloses an option from the planning space of strategic actors.
% TRANSFER_FUNCTION: None. No extraction or transfer occurs. The constraint operates by removing a plan-type from what is strategically thinkable, not by moving resources or imposing costs.
% ABSENT_VOICES: Non-nuclear-armed states and their strategists are excluded from the reading's beneficiary space: they remain vulnerable to total war in ways great powers are not. Nuclear-armed smaller states have ambiguous standing — they benefit from the impossibility of great-power total war but remain uncertain whether the constraint protects them. They would argue for a reading that makes the protection explicit rather than dependent on institutional interpretation.
% DISAPPEARANCE_RATIONALE: If the cognitive impossibility evaporated — if total war re-entered strategic planning space — militaries and defense ministries would reorganize their planning apparatus. Mobilization doctrine would be dusted off, general staffs would resume war-gaming for existential conflict, strategic studies would shift resources back to total-war scenarios, and the entire institutional landscape of strategic planning would shift. The constraint's disappearance would be deeply observable in military institutional change.
% FOUNDING_PROBLEM: How do strategically rational states behave when both sides possess weapons capable of destroying civilization? The founding problem is the paradox of mutual vulnerability: any strategy that aims at total victory is self-defeating because victory is indistinguishable from mutual annihilation.
% FOUNDING_PROBLEM_CORROBORATION: Game theorists, deterrence scholars (Schelling, Jervis, Waltz), and defense intellectuals from outside the strategic planning apparatus itself attest that the mutual vulnerability paradox remains live: it continues to structure strategic reasoning. However, the strategic planning apparatus itself has largely STOPPED treating the founding problem as a live planning question — it has institutionally resolved the paradox by removing total war from what is thinkable as a strategy, even though the underlying physics and vulnerability dynamics remain unchanged.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero (0.0) because the constraint operates through cognitive foreclosure, not through resource extraction or asymmetric distribution. Suppression is zero (0.0) because the constraint does not require active enforcement — no party is defending a preference against resistance. The constraint is self-sustaining because the incoherence is structural: once a strategist recognizes that total victory is impossible, total war ceases to be a rational planning objective. Accessibility collapse is very high (0.95) because no coherent strategic alternative exists — once the incoherence is recognized, no strategist can re-enter the planning space of total war without internal contradiction. Resistance is minimal (0.05) because military institutions have internalized the constraint: they are not resisting it, but institutionalizing it. The measurement series across the 1945-2026 interval holds extractiveness and theater_ratio constant at zero because the constraint's structural properties do not change — it is a mountain, stable across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of great-power strategic planners, the constraint is a natural law that simplifies their mandate: total war is not an option they must plan for or deter. From the perspective of non-nuclear states, the constraint is an asymmetric vulnerability: total war is removed from the strategic space of conflicts between nuclear powers, but not from the space of nuclear-power-versus-non-nuclear-state conflict. From the perspective of deterrence theorists, the constraint is either real (space-contraction reading) or false (deterrence-equilibrium reading: total war is still thinkable but deterred). The engine computes per-seat perceptions from the structural data; the divergence is central to the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Great-power strategic planning apparatus is the beneficiary: they are freed from the incoherent planning objective of total war. Their benefit is not resource-based but cognitive-structural: the removal of an impossible planning target simplifies their strategic mandate. Non-nuclear states and mobilization practitioners are secondary victims: they experience the constraint's asymmetry or obsolescence. The directionality is weak (near zero) because the constraint operates through natural impossibility, not through coerced distribution. The beneficiary identification (great-power apparatus) could trigger false-summit detection if the constraint is later found to be socially constructed rather than cognitively inevitable.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint could be classified as snare with mandatrophy if the beneficiary declaration is true: military institutions benefit from the removal of total-war planning, and the constraint persists because it serves their interest (freeing them from an impossible mandate). However, under the space-contraction reading, the constraint is a MOUNTAIN because the impossibility is logical/material, not institutional. The beneficiary declaration is included precisely to test this ambiguity: if the constraint is found to be a false summit (socially constructed rather than naturally necessary), the beneficiary identification will activate false-summit detection. The commentary documents this ambiguity; the omega variables carry the test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_norm,
    'Is total war truly removed from the strategically THINKABLE (a cognitive/material impossibility grounded in the physics of nuclear weapons) or is it removed from the strategically ACCEPTABLE (a constructed norm that could be overridden by a shift in values or beliefs)?',
    'Examine institutional response to shock: if a power shift (e.g., one side loses nuclear capability, or a new ideology explicitly rejects the taboo) causes strategic planning to resume total-war scenarios despite unchanged nuclear arsenals, the impossibility is normative not cognitive. If total-war planning remains absent even when political will to wage war rises, it is cognitive.',
    'If normative (constructed), reclassify from mountain to tangled_rope or snare (the norm is maintained through institutional and social enforcement, not through material constraint). If cognitive (natural law), the mountain classification holds: the impossibility is as irreducible as gravity within the domain of nuclear-armed strategic planning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, empirical, 'Whether the total-war foreclosure is cognitive/material or normative/constructed.').

omega_variable(
    reading_vs_reading_contradiction,
    'Does the space-contraction reading (total war is COGNITIVELY FORECLOSED) logically exclude the deterrence-equilibrium reading (total war remains thinkable but is DETERRED), or are they describing different mechanisms at different institutional levels?',
    'At the game-theoretic level: if game theory still admits total war as a strategy and only adds a high cost to it, deterrence holds and space-contraction does not. At the institutional level: if strategic planning apparatus genuinely does not entertain total war as an option (absent from war-games, doctrine, mobilization planning), space-contraction holds regardless of game-theoretic admissibility. The question is whether these operate in the same reference frame.',
    'If they operate in the same frame and contradict (game theory says it is thinkable, institutions say it is not), the constraint is neither a pure mountain nor a pure deterrent system, but a hybrid: a cognitive impossibility maintained by institutional practice even though game-theoretic possibility persists. If they operate at different levels (one describes theory, one describes practice), both readings coexist without contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_reading_contradiction, conceptual, 'Whether space-contraction forecloses deterrence-equilibrium or describes a different analytical level.').

omega_variable(
    beneficiary_power_asymmetry,
    'Who benefits from the removal of total war from strategic planning space? Great powers that no longer have to plan total war, or non-nuclear states that are protected from it?',
    'Examine post-Cold War strategic documents: do nuclear-armed states express relief that total war is removed from their planning mandates, or do they express concern about their vulnerability to non-nuclear threats and regional conflicts? Do non-nuclear states treat the constraint as protection or as a structural asymmetry that leaves them unprotected?',
    'If great powers are the beneficiaries (freed from incoherent planning), the constraint is a mountain that serves institutional simplification. If non-nuclear states are the beneficiaries (protected by the constraint), it is a protective mountain with an asymmetric distribution of protection. If there is genuine contestation about who benefits, the constraint may be a false summit masquerading as natural law while actually serving great-power interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_power_asymmetry, empirical, 'Who actually benefits from total-war removal: great powers or non-nuclear states?').

omega_variable(
    kernel_reading_relationship,
    'This constraint is ONE READING of the contested kernel ''total_war_possibility_space''. Do the sibling readings (deterrence_equilibrium_reading, nuclear_taboo_reading) articulate genuinely different mechanisms, or are they different interpretations of the same underlying constraint?',
    'Check whether each reading makes empirically falsifiable predictions that diverge: space-contraction predicts institutional atrophy of total-war planning even if deterrent capability remains unchanged; deterrence predicts total war becomes unthinkable ONLY when both sides maintain nuclear capability (vulnerability-dependent); taboo predicts removal becomes independent of capability (norm-dependent). Test these predictions against institutional change in states that lose or gain nuclear capability.',
    'If the readings make divergent predictions that are empirically testable, they are genuinely different constraints and should be authored separately with distinct ε values and causal mechanisms. If they are interpretive frames layered on the same underlying phenomenon, they are one constraint with multiple framings, and the committer frame (Rule 1–4) is the correct authoring structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship, empirical, 'Whether the kernel readings are different constraints or interpretive framings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__space_contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__space_contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(tota_tr_t2026, total_war_possibility_space__space_contraction_reading, theater_ratio, 2026, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.0).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1980, 0.0).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement(tota_be_t2026, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2026, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.0).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% The total_war_possibility_space kernel decomposes into three constraint stories, one per reading. Each reading instantiates a different causal mechanism (cognitive foreclosure, game-theoretic deterrence, or normative taboo) explaining why total war is absent from strategic planning. The three stories have different ε values, different beneficiary/victim structures, and different classifications. They are linked as competing readings of a single kernel, not as three independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
