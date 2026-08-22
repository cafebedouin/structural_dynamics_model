% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'dropping reading' of the total war
 *   reachability kernel. The reading asserts that total war remains reachable
 *   despite the decline in its empirical probability—that is, the capability
 *   exists, strategic actors maintain posture as though they would use it,
 *   and the probability boundary has been pushed down but not eliminated.
 *   Under this reading, deterrence functions as a tangled rope: genuine
 *   coordination (mutual constraint keeps conflicts bounded) coupled with
 *   asymmetric extraction (nuclear powers benefit from maintaining deterrence
 *   credibility while non-nuclear states and civilian populations bear the
 *   cost of living under perpetual threat). The constraint is CLAIMED as
 *   tangled_rope and the metrics describe coordination-plus-extraction
 *   operation; the claim and metrics are authored independently and the
 *   engine measures their consonance.
 *
 * KEY AGENTS:
 *   - nuclear_armed_powers: institutional power holders who set deterrence doctrine and maintain the reachability narrative; trapped exit (unilateral disarmament invites conventional aggression); beneficiaries of the deterrence equilibrium
 *   - civilian_populations_under_threat: powerless, trapped, bearing the psychological and strategic cost of reachability; victims who cannot exit
 *   - non_nuclear_states: moderate power, constrained exit (cannot pursue certain strategic objectives without breaching the boundary), payers in the deterrence system
 *   - deterrence_credibility_maintainers: institutional identity-locked actors whose professional authority depends on maintaining the reachability narrative
 *   - strategic_escalation_analysts: analytical observers who measure whether the constraint's core claim holds
 *   - disarmament_advocates: excluded parties who directly contest the reachability premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.68).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.72).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, 'c5cb1489-6a22-4aa4-9765-7ae59fe33453').
narrative_ontology:cs_kernel_codification('c5cb1489-6a22-4aa4-9765-7ae59fe33453', formalized).
narrative_ontology:cs_authority_grounding('c5cb1489-6a22-4aa4-9765-7ae59fe33453', extraction).
narrative_ontology:cs_interpretation_layer_present('c5cb1489-6a22-4aa4-9765-7ae59fe33453').
narrative_ontology:cs_reading_relation('c5cb1489-6a22-4aa4-9765-7ae59fe33453', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('c5cb1489-6a22-4aa4-9765-7ae59fe33453', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('c5cb1489-6a22-4aa4-9765-7ae59fe33453', foundational, reachability_maintained_by_doctrine).
narrative_ontology:cs_axiom_status(reachability_maintained_by_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c5cb1489-6a22-4aa4-9765-7ae59fe33453', reachability_maintained_by_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('c5cb1489-6a22-4aa4-9765-7ae59fe33453', foundational, deterrence_as_coordination_equilibrium).
narrative_ontology:cs_axiom_status(deterrence_as_coordination_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('c5cb1489-6a22-4aa4-9765-7ae59fe33453', deterrence_as_coordination_equilibrium, instrumental).
narrative_ontology:cs_reference_frame('c5cb1489-6a22-4aa4-9765-7ae59fe33453', nuclear_deterrence_equilibrium).
narrative_ontology:cs_drift_state('c5cb1489-6a22-4aa4-9765-7ae59fe33453', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5cb1489-6a22-4aa4-9765-7ae59fe33453', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, deterrence_credibility_maintainers).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_armed_powers).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, civilian_populations_under_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, rival_military_powers).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, rival_military_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and deterrence doctrine; set the boundaries of acceptable strategic behavior by credibly threatening total war in extremis. They benefit from the deterrence equilibrium that keeps conflicts below the nuclear threshold, and they maintain the constraint through doctrine, exercises, and strategic signaling. Their exit is structurally impossible: unilateral disarmament reverses the deterrent force and invites conventional aggression.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_armed_powers, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_armed_powers, beneficiary).

% Bear the cost of living under the shadow of total war reachability: the possibility that escalation dynamics could breach the current probabilistic boundary. They pay in psychological burden, constrained geopolitical autonomy, and the fact that nuclear states' deterrence maintenance relies partly on the credibility of their willingness to inflict total war. Exit is impossible—they cannot opt out of nuclear deterrence even if they reject its logic.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, civilian_populations_under_threat, payer,
    powerless, biographical, trapped, global).

% Constrained in their strategic choices by the deterrence boundary. They cannot pursue certain conventional military objectives, expand territory, or challenge nuclear powers directly because total war remains reachable—a constraint imposed by others' arsenals. Their exit options are limited: develop nuclear weapons (high cost), form alliances with nuclear powers (loss of autonomy), or accept strategic subordination.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Strategic theorists, military planners, and policy elites whose professional identity and institutional authority depend on the coherence of deterrence doctrine. They maintain the constraint by authoring and transmitting the narrative that total war remains reachable despite probability decline, that credibility requires acting as though it is always possible, and that erosion of this belief destabilizes the equilibrium. Their identity is constituted through deterrence logic; rejecting it means professional dissolution.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, deterrence_credibility_maintainers, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Competing powers who benefit from the deterrence boundary insofar as it also constrains their rivals (mutual constraint is the coordination function), but bear the cost of military modernization, alert posture, and the perpetual strategic anxiety the constraint requires. They are locked into the identity of 'nuclear peer' and cannot credibly back down from deterrence posture without inviting challenge.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, rival_military_powers, payer,
    powerful, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, rival_military_powers, beneficiary).

% Measure and model the probability boundary, track whether reachability persists despite declining probability, and examine whether the coordination equilibrium (deterrence) can survive if belief in reachability erodes. They have no stake in maintaining the constraint but serve as the epistemic arbiters of whether the constraint's core claim—total war remains reachable—holds under scrutiny.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_escalation_analysts, observer,
    analytical, generational, analytical, global).

% Argue that total war is neither reachable (nuclear arsenals are unusable) nor a legitimate deterrence mechanism, and that deterrence itself is a false coordination function hiding pure extraction (threat-based control). They are structurally excluded from setting doctrine because their position directly contradicts the maintained narrative that reachability must be credibly preserved.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, disarmament_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_armed_powers).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deters direct military conflict between nuclear powers by creating a shared understanding that escalation beyond conventional limits risks total war—a mutual constraint that keeps conflicts in a bounded strategic space where neither party can win by crossing into nuclear use. The coordination problem solved: absent this boundary, rational military expansion under uncertainty would push powers toward preemptive nuclear strikes; the boundary is the equilibrium that prevents that spiral.
% TRANSFER_FUNCTION: Transfers strategic vulnerability from nuclear powers to non-nuclear states and civilian populations globally. Nuclear-armed powers retain the right to threaten total war and collect the security benefit of that threat; non-nuclear states and civilians bear the cost of living under permanent reachability of that threat and constrain their own strategic choices accordingly.
% ABSENT_VOICES: Disarmament advocates and populations of non-nuclear states are excluded from the strategic conversation that maintains the constraint. They would argue that reachability is decreasing, not stable; that deterrence is a false coordination masking coercive control; and that the constraint's persistence depends on suppressing evidence that total war is becoming unreachable. They cannot credibly participate because accepting their premises would dissolve the deterrence equilibrium.
% DISAPPEARANCE_RATIONALE: Nuclear-armed powers and strategic analysts argue that if the constraint—the maintained belief that total war remains reachable—disappeared, deterrence would collapse and conventional military escalation would resume unconstrained, leading to conflicts that might breach the nuclear threshold anyway. Disarmament advocates argue the opposite: if actors believed total war was truly unreachable, they would stop configuring their strategies around deterrence and might actually de-escalate. The verdicts diverge because the dispute is about whether the constraint creates stability or illusion.
% FOUNDING_PROBLEM: The development of thermonuclear weapons created a strategic dilemma: both the capability for total destruction and the mutual vulnerability to it. Early deterrence theory posed that credible mutual threat of annihilation could substitute for military victory as a stabilizer—a coordination mechanism to prevent nuclear use. The constraint was built to solve the problem of how to keep wars limited when the capacity to wage unlimited war existed.
% FOUNDING_PROBLEM_CORROBORATION: Strategic historians and analysts of nuclear policy document that the founding problem—preventing escalation in conflicts involving nuclear-armed powers—has been substantially solved: no nuclear escalation has occurred in major-power conflicts since 1945, and the probability of escalation has measurably declined. Nuclear-armed powers formally acknowledge that escalation did not occur during the Cold War and has not occurred post-Cold War, yet continue to maintain deterrence doctrine as though the problem remains live. Independent analysis from the Stockholm International Peace Research Institute and the Union of Concerned Scientists corroborates that the founding problem is dead or dying, and deterrence maintenance persists as institutional inertia rather than as response to an active threat.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, contested).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because deterrence delivers genuine coordination benefit (prevents nuclear escalation) but requires non-nuclear states and civilian populations to absorb strategic vulnerability as the price of that coordination. Suppression is high (0.72) because the constraint's persistence depends on active maintenance of the reachability narrative—doctrine, exercises, strategic posturing, and most critically, the suppression of evidence or credible argument that total war is becoming unreachable. Theater ratio climbs from 0.28 to 0.41 over the interval, indicating increasing proportion of deterrence activity is performative (exercises, statements, doctrine refinement) relative to functional (actual escalation prevention), suggesting the constraint is drifting toward piton characteristics even while maintaining the claim of live deterrence. The measurement series on one shared time grid show extractiveness rising then plateauing (evidence accumulating that probability has stabilized at low level), theater increasing then stabilizing (activity becoming increasingly theatrical), and suppression requirement constant at high level (maintaining the narrative requires unabating effort). This trajectory is consistent with a tangled rope that is beginning to convert to a piton—the coordination function persists, but maintaining it increasingly depends on suppression of countervailing evidence rather than on structural enforcement of the coordination itself.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (nuclear powers and deterrence maintainers) perceives the constraint as rope or mountain: a natural equilibrium that coordinates mutual safety. The payer seats (non-nuclear states, civilian populations) perceive the same structure as tangled rope or snare: coercive control masquerading as coordination. The engine should compute different types for these seats because they sit in different structural positions relative to the constraint: one seat benefits from the deterrence equilibrium and collects rents on maintaining it; the other seat bears the cost. The perspectival gap is the measurement the corpus takes of whether deterrence is coordination or extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed powers: d approaches 0.0 (full beneficiary) because they benefit from deterrence equilibrium, control doctrine, and exit is identity-fused (unilateral disarmament means loss of power status). Non-nuclear states: d approaches 1.0 (full target) because they bear strategic constraint, cannot exit without accepting subordination, and do not control the boundary. Civilian populations: d approaches 1.0 (full target) because they pay in psychological burden and vulnerability, have zero exit options, and receive no direct benefit from the deterrence equilibrium. Deterrence maintainers: d near 0.5 (symmetric to slightly beneficiary) because they benefit professionally from maintaining the constraint but also bear the cost of perpetual alert posture and cognitive dissonance between probability decline and reachability maintenance. Strategic analysts: d approaches 0.5 (symmetric) because their analytical distance gives them neither primary benefit nor cost, though their role in measuring the constraint creates a secondary interest in its continuation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly blocks the classification as pure rope by asserting that extraction is present (non-nuclear states constrained, civilians bear costs) alongside coordination. It also blocks mountain classification by asserting reachability is maintained through active doctrine, posturing, and suppression of counterargument—not through natural law. The tangled rope classification requires: (1) genuine coordination (deterrence prevents escalation—affirmed), (2) identifiable beneficiary set (nuclear-armed powers—affirmed), (3) identifiable victim set (non-nuclear states and civilian populations—affirmed), (4) active enforcement (maintenance of deterrence narrative and suppression of reachability-erosion evidence—affirmed). The mandatrophy analysis here is that deterrence doctrine underwent a silent conversion: it was founded to solve the problem of preventing escalation given the existence of total-war capability. As that problem receded (escalation did not occur; probability dropped), the constraint persisted through narrative maintenance rather than through structural necessity. The founding problem (prevent nuclear escalation) is now dead or dying, yet the constraint (maintain deterrence doctrine) persists—classic mandatrophy. The reading maintains that this does NOT make the constraint a snare: there is a real coordination function, and part of the suppression and extraction is the cost of maintaining that function. But as the ratio tilts (theater rising, extractiveness plateauing), the constraint approaches piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_epistemology,
    'What constitutes evidence that total war remains reachable? Is reachability a fact about capability (missiles exist, doctrine authorizes use) or a fact about probability (would the state actually use them)? Can capability exist without credible probability?',
    'Epistemological analysis of how deterrence theorists measure reachability; examination of whether the constraint''s persistence depends on conflating capability with probable use.',
    'If reachability is identified with capability alone, the constraint is more clearly tangled rope (real coordination with extracted cost). If reachability requires credible probability, and probability has fallen below the threshold of credibility, the constraint converts to piton (performatively maintained). This omega addresses the core contested axiom of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reachability_epistemology, conceptual, 'Whether reachability is defined by capability or by credible probability of use.').

omega_variable(
    theater_accumulation_foreclosure,
    'Can the constraint remain a tangled rope if theater_ratio continues rising? At what point does increasing theatrical activity foreclose the rope classification?',
    'Longitudinal analysis of theater ratio across decades of deterrence history; comparison with institutional constraints known to be pitons to establish a foreclosure threshold.',
    'If theater rises above 0.6, the constraint should reclassify from tangled rope to piton (coordination function overwhelmed by performative maintenance). The Boltzmann coupling computation would shift the effective extraction profile. This omega documents the time-dependent risk that this reading converts to contingent_reachability or contraction readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_accumulation_foreclosure, empirical, 'Whether rising theater ratio will foreclose the rope classification.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of reachability-erosion evidence structural (gatekeeping by institutions, classification of analyses) or internalized (deterrence maintainers have adopted the reachability axiom as professional identity and cognitively filter contradictory evidence)?',
    'Post-exit suppression trajectory: if analysts who leave the deterrence establishment continue suppressing reachability-erosion evidence, the suppression is internalized; if suppression stops post-exit, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the authored 0.72 suggests, and the constraint is more extractive (targets absorb the identity-fusion cost even if they exit the structural position). This affects the piton-conversion forecast.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of contrary evidence is structural or identity-internalized.').

omega_variable(
    sibling_reading_foreclosure_contingency,
    'Does the dropping reading logically foreclose the contraction reading, or do they remain live alternatives?',
    'Test whether adopting the dropping reading''s core premise (reachability is maintained, probability has dropped but not to zero) logically requires rejecting the contraction reading''s core premise (reachability has left the feasible set entirely). They appear to directly contradict.',
    'If they logically foreclose each other, the reading relation to contraction_reading should be ''forecloses'', not ''influences'' or ''coexists_with''. This affects the CS structure classification and the constraint family architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_contingency, conceptual, 'Whether the dropping and contraction readings logically foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_reachability_boundary__dropping_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t5, total_war_reachability_boundary__dropping_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(tota_tr_t5, observed).
narrative_ontology:measurement(tota_tr_t10, total_war_reachability_boundary__dropping_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(tota_tr_t10, observed).
narrative_ontology:measurement(tota_tr_t15, total_war_reachability_boundary__dropping_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(tota_tr_t15, observed).
narrative_ontology:measurement(tota_tr_t20, total_war_reachability_boundary__dropping_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(tota_tr_t20, observed).
narrative_ontology:measurement(tota_tr_t25, total_war_reachability_boundary__dropping_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(tota_tr_t25, observed).
narrative_ontology:measurement(tota_tr_t30, total_war_reachability_boundary__dropping_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(tota_tr_t30, observed).
narrative_ontology:measurement(tota_tr_t35, total_war_reachability_boundary__dropping_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(tota_tr_t35, observed).
narrative_ontology:measurement(tota_tr_t40, total_war_reachability_boundary__dropping_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(tota_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_reachability_boundary__dropping_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t5, total_war_reachability_boundary__dropping_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(tota_be_t5, observed).
narrative_ontology:measurement(tota_be_t10, total_war_reachability_boundary__dropping_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(tota_be_t10, observed).
narrative_ontology:measurement(tota_be_t15, total_war_reachability_boundary__dropping_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(tota_be_t15, observed).
narrative_ontology:measurement(tota_be_t20, total_war_reachability_boundary__dropping_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(tota_be_t20, observed).
narrative_ontology:measurement(tota_be_t25, total_war_reachability_boundary__dropping_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tota_be_t25, observed).
narrative_ontology:measurement(tota_be_t30, total_war_reachability_boundary__dropping_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(tota_be_t30, observed).
narrative_ontology:measurement(tota_be_t35, total_war_reachability_boundary__dropping_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(tota_be_t35, observed).
narrative_ontology:measurement(tota_be_t40, total_war_reachability_boundary__dropping_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(tota_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_reachability_boundary__dropping_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(tota_su_t0, observed).
narrative_ontology:measurement(tota_su_t5, total_war_reachability_boundary__dropping_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(tota_su_t5, observed).
narrative_ontology:measurement(tota_su_t10, total_war_reachability_boundary__dropping_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(tota_su_t10, observed).
narrative_ontology:measurement(tota_su_t15, total_war_reachability_boundary__dropping_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(tota_su_t15, observed).
narrative_ontology:measurement(tota_su_t20, total_war_reachability_boundary__dropping_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(tota_su_t20, observed).
narrative_ontology:measurement(tota_su_t25, total_war_reachability_boundary__dropping_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(tota_su_t25, observed).
narrative_ontology:measurement(tota_su_t30, total_war_reachability_boundary__dropping_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(tota_su_t30, observed).
narrative_ontology:measurement(tota_su_t35, total_war_reachability_boundary__dropping_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(tota_su_t35, observed).
narrative_ontology:measurement(tota_su_t40, total_war_reachability_boundary__dropping_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(tota_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.18).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three constraint stories, each a distinct reading with its own ε and beneficiary/victim structure. The dropping reading (this constraint) asserts: (1) reachability remains positive but probability has dropped; (2) deterrence is tangled rope, not mountain; (3) the constraint persists despite its founding problem atrophying. The contraction reading asserts reachability has left the feasible set (mountain classification). The contingent reading asserts reachability is technology-dependent and current drop is piton (could reverse). These readings coexist as live positions held by different strategic communities and forecast different futures. The dropping reading influences both siblings by establishing that technological contingency is not the only possible source of future change (dropping could continue or stabilize) and by asserting that reachability has not yet fully contracted (maintaining the contested middle position).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, institutional, 0.15).
constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, powerless, 0.95).
constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
