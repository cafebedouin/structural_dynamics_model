% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Deterrence Unthinkability: Great-Power Total War as Categorically Unwinnable
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   The advent of nuclear weapons fundamentally altered the strategic
 *   geometry of great-power relations. This constraint story captures the
 *   'deterrence_unthinkable' reading: the proposition that total war between
 *   nuclear-armed great powers is categorically unwinnable, rendering
 *   operational planning for victory incoherent. The constraint is treated as
 *   a fixed feature of the post-1945 strategic environment â a
 *   technological-strategic ceiling that persists without enforcement. It is
 *   authored as a Mountain with declared beneficiaries (civilian populations)
 *   and victims (military establishments) to trigger False Summit evaluation,
 *   reflecting the contested nature of the kernel.
 *
 * KEY AGENTS:
 *   - Civilian populations (powerless/trapped) â structural beneficiaries spared annihilation
 *   - Military establishments (institutional/constrained) â structural payers bearing mission incoherence
 *   - Strategic studies community (analytical) â interpretive observer translating technology into doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.15).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.1).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.15).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Deterrence Unthinkability: Great-Power Total War as Categorically Unwinnable").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'cb86666d-d336-41bc-8b0f-6df0f4922417').
narrative_ontology:cs_kernel_codification('cb86666d-d336-41bc-8b0f-6df0f4922417', distributed).
narrative_ontology:cs_authority_grounding('cb86666d-d336-41bc-8b0f-6df0f4922417', self_enforcing).
narrative_ontology:cs_reading_relation('cb86666d-d336-41bc-8b0f-6df0f4922417', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('cb86666d-d336-41bc-8b0f-6df0f4922417', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('cb86666d-d336-41bc-8b0f-6df0f4922417', foundational, total_war_categorically_unwinnable).
narrative_ontology:cs_axiom_status(total_war_categorically_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('cb86666d-d336-41bc-8b0f-6df0f4922417', total_war_categorically_unwinnable, empirically_contingent).
narrative_ontology:cs_axiom('cb86666d-d336-41bc-8b0f-6df0f4922417', secondary, victory_planning_strategically_incoherent).
narrative_ontology:cs_axiom_status(victory_planning_strategically_incoherent, holdable).
narrative_ontology:cs_axiom_grounding('cb86666d-d336-41bc-8b0f-6df0f4922417', victory_planning_strategically_incoherent, instrumental).
narrative_ontology:cs_reference_frame('cb86666d-d336-41bc-8b0f-6df0f4922417', deterrence_stability_framework).
narrative_ontology:cs_drift_state('cb86666d-d336-41bc-8b0f-6df0f4922417', contemporary_strategic_competition, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('cb86666d-d336-41bc-8b0f-6df0f4922417', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inhabit the cities and societies that would be annihilated in a great-power total war. They benefit from the constraint because the physical impossibility of victory prevents powers from initiating such war. They have no exit from the nuclear shadow and no institutional voice in deterrence policy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, civilizational, trapped, global).

% Bear the institutional cost of mission incoherence: the traditional purpose of achieving decisive victory in great-power war becomes strategically meaningless under conditions of mutual assured destruction. Must reorient doctrine toward deterrence, damage limitation, and war prevention, with corresponding budgetary and cultural displacement.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, constrained, national).

% Produces the analytical frameworks interpreting nuclear strategy. While the constraint is technologically fixed, the community administers the doctrinal superstructure that translates physical capability into strategic meaning.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_studies_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power total war by making the costs of any conceivable victory exceed the value of the object being fought over, thereby coordinating mutual restraint without requiring trust or continuous negotiation.
% TRANSFER_FUNCTION: Moves the burden of strategic adaptation from civilian populations (who are spared annihilation) to military establishments (whose institutional purpose of achieving victory becomes incoherent and who must absorb doctrinal and budgetary displacement).
% ABSENT_VOICES: Conventional military strategists committed to decisive victory doctrines; non-nuclear states whose security depends on extended nuclear deterrence but who have no voice in nuclear targeting policy; future generations who bear the latent risk of accidental escalation or doctrinal decay.
% DISAPPEARANCE_RATIONALE: If the constraint vanished â if technological or doctrinal change made great-power total war winnable again â military establishments would revert to victory-oriented planning, arms races would intensify, extended deterrence commitments would unravel, and the institutional architecture of strategic stability would collapse.
% FOUNDING_PROBLEM: The industrialization of warfare in the first half of the twentieth century produced weapons capable of annihilating entire societies, culminating in nuclear weapons, which made traditional great-power war self-defeating and created the need for a strategic framework oriented toward prevention rather than victory.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of strategic bombing and nuclear devastation document the qualitative shift in destructive capacity; game theorists and strategic analysts outside the military establishment corroborate the logic of mutual restraint from analytical rather than institutional seats.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, ExtMetricName, E),
    domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint is a structural limit rather than a rent-extraction mechanism; it imposes costs (mission incoherence) without transferring resources. Suppression is minimal (0.10) because no active enforcement is required â the constraint is self-enforcing through physics and game theory. Theater ratio is negligible (0.05) as maintenance is not performative. Accessibility collapse is near-total (0.95): once the destructive capacity is understood, victory planning collapses as a coherent alternative. Resistance is low (0.10) because actors cannot resist the physical reality, though institutional resistance to mission displacement persists.
 *
 * PERSPECTIVAL GAP:
 *   Civilian populations and military establishments occupy opposite ends of the directionality spectrum. Civilians experience the constraint as protective: a ceiling that prevents annihilation. Military establishments experience it as displacing: a floor that removes their institutional purpose. The strategic studies community sits at an analytical remove, interpreting both experiences without sharing either's existential exposure.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are structural beneficiaries (d near 0.0) because the constraint removes the threat of total war. Military establishments are structural targets (d near 1.0) because the constraint extracts mission coherence and institutional identity from them. No directionality overrides are required: the physical asymmetry of destruction maps cleanly onto beneficiary and victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by distinguishing the physical reality of nuclear destruction from the doctrinal apparatus built atop it. If missile defense, counterforce, or emerging technologies were to restore winnability, the doctrine of unwinnability would become mandatrophic â persisting as ideology after its physical basis eroded. The temporal measurements are flat to reflect stability, with a slight downward drift in extractiveness as the constraint became naturalized over the Cold War.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the categorical unwinnability of great-power nuclear war a physical-strategic fact emergent from technology, or a constructed deterrence doctrine that stabilizes a particular international order benefiting identifiable coalitions?',
    'Forensic analysis of strategic planning documents across nuclear powers; if operational plans for decisive victory continue to be updated and exercised, the unwinnability is rhetorical rather than operational.',
    'If operational planning for victory persists, the constraint is a False Summit and reclassifies to tangled_rope or snare; if absent, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether deterrence unthinkability is technological fact or constructed doctrine').

omega_variable(
    civilian_benefit_as_negative_relief,
    'Does the benefit to civilian populations â the absence of total war â constitute beneficiary status in the extractive sense, or merely the absence of a negative externality?',
    'Identify whether any party captures concentrated rents from the maintenance of the unwinnability doctrine (e.g., institutional budgets, professional status, strategic autonomy) that would not exist absent the doctrine.',
    'If no concentrated rent extraction exists, the FSM beneficiary flag may not override mountain classification despite beneficiary presence; if rents exist, false summit is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_benefit_as_negative_relief, conceptual, 'Whether civilian benefit triggers false summit or is negative relief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0, 0.1).
narrative_ontology:measurement(war__tr_t15, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 15, 0.08).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 30, 0.06).
narrative_ontology:measurement(war__tr_t45, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 45, 0.05).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 60, 0.05).
narrative_ontology:measurement(war__tr_t75, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 75, 0.05).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(war__be_t15, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(war__be_t45, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 45, 0.13).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(war__be_t75, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 75, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(war_winnability_post_1945__deterrence_unthinkable, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, rhetorical_contraction).

% DUAL FORMULATION NOTE:
% The kernel 'war_winnability_post_1945' decomposes into three structurally distinct constraints. This reading ('deterrence_unthinkable') asserts operational unwinnability; 'countervailing_thinkable' asserts limited winnability; 'rhetorical_contraction' asserts discursive suppression without operational change. Each carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
