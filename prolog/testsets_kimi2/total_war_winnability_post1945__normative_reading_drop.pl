% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Normative Prohibition of Total War via UN Charter Article 2(4) and Humanitarian Law
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint is the normative_reading_drop of the
 *   total_war_winnability_post1945 kernel. It treats the post-1945
 *   abandonment of total war as a coordination achievement: UN Charter
 *   Article 2(4) and the development of international humanitarian law
 *   collectively solve the coordination problem of preventing wars of
 *   annihilation. The constraint's beneficiaries are global civilian
 *   populations protected by the norm; its payers are revisionist powers
 *   whose strategic options are constrained. Sibling readings include
 *   structural_contraction_reading (nuclear weapons made total war physically
 *   impossible) and strategic_culture_drift (ideational shift in elite
 *   discourse). This reading is distinguished by its grounding in formal
 *   legal obligation rather than material capability or informal culture.
 *
 * KEY AGENTS:
 *   - global_civilian_populations: Primary beneficiary (powerless/trapped/universal) â receive protection from the legal prohibition of total war and civilian targeting norms
 *   - revisionist_powers: Primary payer (powerful/constrained/national) â bear the opportunity cost of foregone aggressive war options and face institutional penalties for defection
 *   - un_charter_system: Agenda setter (institutional/constrained/global) â maintains the treaty framework and collective security mechanisms
 *   - international_humanitarian_law_regime: Agenda setter (institutional/constrained/global) â codifies and updates limits on methods and means of warfare
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.22).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.35).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.22).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Prohibition of Total War via UN Charter Article 2(4) and Humanitarian Law").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '4cf79485-6d24-444b-873f-42ad355f1f6b').
narrative_ontology:cs_kernel_codification('4cf79485-6d24-444b-873f-42ad355f1f6b', formalized).
narrative_ontology:cs_authority_grounding('4cf79485-6d24-444b-873f-42ad355f1f6b', lineage).
narrative_ontology:cs_interpretation_layer_present('4cf79485-6d24-444b-873f-42ad355f1f6b').
narrative_ontology:cs_reading_relation('4cf79485-6d24-444b-873f-42ad355f1f6b', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cf79485-6d24-444b-873f-42ad355f1f6b', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('4cf79485-6d24-444b-873f-42ad355f1f6b', foundational, aggressive_war_categorically_prohibited).
narrative_ontology:cs_axiom_status(aggressive_war_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('4cf79485-6d24-444b-873f-42ad355f1f6b', aggressive_war_categorically_prohibited, conventional).
narrative_ontology:cs_axiom('4cf79485-6d24-444b-873f-42ad355f1f6b', foundational, civilian_immunity_non_derogable).
narrative_ontology:cs_axiom_status(civilian_immunity_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('4cf79485-6d24-444b-873f-42ad355f1f6b', civilian_immunity_non_derogable, conventional).
narrative_ontology:cs_reference_frame('4cf79485-6d24-444b-873f-42ad355f1f6b', un_charter_legal_order).
narrative_ontology:cs_drift_state('4cf79485-6d24-444b-873f-42ad355f1f6b', multipolar_resurgence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cf79485-6d24-444b-873f-42ad355f1f6b', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal prohibition of total war and the humanitarian-law protections against direct targeting. They cannot exit the state system or the prospect of interstate violence; their protection depends entirely on state compliance with Article 2(4) and the Geneva Conventions.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, civilizational, trapped, universal).

% States or elites that would pursue territorial revision or regime change through unlimited warfare if unconstrained. They bear the opportunity cost of foregone military options and face sanctions, diplomatic isolation, and ICC referral when they violate the norms. Their exit is constrained by the collective response of the UN system and allied economic networks.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, national).

% Maintains the treaty framework that renders aggressive war illegitimate. Administers collective security mechanisms, peacekeeping mandates, and Chapter VII enforcement through the Security Council and General Assembly. Its institutional identity is fused with the post-1945 legal order; exit by member states is technically possible but systemically destabilizing.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, un_charter_system, agenda_setter,
    institutional, generational, constrained, global).

% Codifies and updates the laws of war to limit methods and means of hostilities, distinguishing combatants from civilians. Operates through treaty conferences, ICRC customary-law formation, and ad hoc tribunals. It is structurally dependent on state consent but exercises autonomous normative pressure through judicial interpretation.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_law_regime, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating state conduct to prevent wars of annihilation and unlimited civilian targeting by establishing a collective, treaty-based renunciation of aggressive war and legally binding limitations on the means and methods of hostilities.
% TRANSFER_FUNCTION: Moves the legitimate option to wage total war from the unilateral discretion of states into a collectively prohibited category, and moves protective legal status to civilian populations and non-combatants.
% ABSENT_VOICES: Non-state actors capable of catastrophic violence are not party to the UN Charter framework; populations in revisionist states who might support total war for national unification are underrepresented in treaty forums; future generations bear the consequences of normative collapse but have no participatory voice in treaty review or Security Council deliberation.
% DISAPPEARANCE_RATIONALE: If Article 2(4) and the humanitarian law framework vanished overnight, the institutional architecture preventing total war would collapse. States would revert to unilateral determinations of just cause and unlimited methods, the collective security system would lose its constitutive purpose, and civilian protection would become contingent on state mercy rather than legal obligation.
% FOUNDING_PROBLEM: The devastation of the Second World War, including the industrial-scale bombing of cities and civilian death, demonstrated that unregulated great-power war threatened civilization itself; the League of Nations had failed to prevent aggressive war.
% FOUNDING_PROBLEM_CORROBORATION: The post-1945 diplomatic record and the Nuremberg Tribunal judgments corroborate the founding problem from outside the UN beneficiary apparatus. Realist scholars and some military historians contest that the norm solved the problem, attributing the long peace to nuclear deterrence and bipolarity rather than to legal prohibition; this dissent comes from analytical seats outside the benefiting parties.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint functions primarily as coordination: it solves a collective-action problem that no state could solve unilaterally. Suppression is moderate (0.35) because the norm is widely internalized but still requires institutional enforcement (sanctions, ICC referrals) against defectors. Theater ratio is low (0.15) because the legal framework retains genuine protective function, though some performative legalism arises in great-power exceptions. Accessibility collapse is high (0.75): once the norm is understood, total war ceases to be a legitimate strategic option. Resistance is moderate (0.40) because revisionist powers actively contest the norm while most states comply.
 *
 * PERSPECTIVAL GAP:
 *   Global civilians experience the constraint as protective architecture; revisionist powers experience it as an imposed limitation on sovereignty and strategic freedom. The UN system experiences it as constitutive of its own legitimacy. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations are structural beneficiaries (d near 0.0) because the constraint subsidizes their security without extracting from them. Revisionist powers are structural targets (d near 1.0) because the constraint directly removes strategic options from their choice set. The UN Charter system and humanitarian law regime sit near symmetric (d ~0.5): they administer the constraint and are simultaneously bound by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as a snare because no concentrated beneficiary captures the gains; civilian protection is diffuse and non-excludable. It prevents mislabeling as a mountain because the constraint requires active institutional maintenance (treaty conferences, Security Council action, ICRC customary-law development) and would atrophy without that maintenance. The rope classification captures the genuine coordination function while acknowledging the asymmetric cost to revisionist powers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_structural_causation,
    'Is the absence of total war since 1945 caused by normative prohibition under Article 2(4) and humanitarian law, or by the structural impossibility of total war in a nuclear-armed system?',
    'Comparative case analysis of pre-nuclear eras with strong normative prohibitions versus post-1945 nuclear eras; counterfactual analysis of whether revisionist powers would pursue total war if nuclear weapons did not exist but norms remained.',
    'If structural factors dominate, this constraint''s epsilon is overstated as a coordination mechanism and should be reclassified toward mountain or piton; if normative factors dominate, the rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_vs_structural_causation, conceptual, 'Ambiguity between normative and structural causation for the absence of total war').

omega_variable(
    revisionist_constraint_as_coordination_cost,
    'Does the constraint on revisionist powers constitute a symmetric coordination cost borne by all parties to the UN Charter, or asymmetric extraction from a targeted subgroup?',
    'Examine whether compliant states bear equivalent opportunity costs by renouncing aggressive war, or whether the constraint falls disproportionately on states that would otherwise exercise that option.',
    'If symmetric, the payer seat is merely bearing the coordination cost and the rope classification holds; if asymmetric, the constraint may compute as tangled_rope or snare depending on enforcement structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisionist_constraint_as_coordination_cost, conceptual, 'Whether revisionist constraint is coordination cost or asymmetric extraction').

omega_variable(
    kernel_reading_scope,
    'This constraint is one reading of the total_war_winnability_post1945 kernel. Do the sibling readings describe the same constraint measured differently or structurally distinct constraints?',
    'Epsilon-invariance test: if the three readings produce different epsilon values and different stakeholder directionalities, they are distinct constraints; if metrics converge, they are observational variants.',
    'If the readings are distinct constraints, the kernel decomposition is validated; if not, the stories should be merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Validation of kernel decomposition into distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tota_tr_t13, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 13, 0.06).
narrative_ontology:measurement(tota_tr_t26, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 26, 0.08).
narrative_ontology:measurement(tota_tr_t39, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 39, 0.09).
narrative_ontology:measurement(tota_tr_t52, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 52, 0.11).
narrative_ontology:measurement(tota_tr_t65, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 65, 0.13).
narrative_ontology:measurement(tota_tr_t79, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 79, 0.15).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tota_be_t13, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 13, 0.15).
narrative_ontology:measurement(tota_be_t26, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 26, 0.14).
narrative_ontology:measurement(tota_be_t39, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 39, 0.16).
narrative_ontology:measurement(tota_be_t52, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 52, 0.17).
narrative_ontology:measurement(tota_be_t65, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 65, 0.2).
narrative_ontology:measurement(tota_be_t79, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 79, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__normative_reading_drop, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, strategic_culture_drift).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, structural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the total_war_winnability_post1945 kernel, which decomposes into three structurally distinct claims: normative illegitimacy (this file), strategic culture drift, and structural impossibility via nuclear weapons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
